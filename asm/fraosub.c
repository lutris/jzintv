/*
HEADER:     ;
TITLE:      Frankenstein Cross Assemblers;
VERSION:    2.0;
DESCRIPTION: "  Reconfigurable Cross-assembler producing Intel (TM)
        Hex format object records.  ";
SYSTEM:     UNIX, MS-Dos ;
FILENAME:   fraosub.c;
WARNINGS:   "This software is in the public domain.  
        Any prior copyright claims are relinquished.  

        This software is distributed with no warranty whatever.  
        The author takes no responsibility for the consequences 
        of its use."  ;
SEE-ALSO:   frasmain.c;
AUTHORS:    Mark Zenier;
*/

/*
    description output pass utility routines
    history     September 27, 1987
            March 15, 1988   release 1.1 WIDTH
            September 14, 1990  Dosify, 6 char unique names
*/

#include "config.h"
#include "file/file.h"
#include <stdio.h>
#include "frasmdat.h"
#include "fragcon.h"
#include "types.h"
#include "protos.h"
#include "intermed.h"
#include "icart/icartrom.h"
#include "icart/icartbin.h"

#define OUTRESULTLEN 256
#define NUMHEXPERL 16
#define SOURCEOFFSET 24
#define NUMHEXSOURCE 6

#define INTELLEN 32

extern FILE *smapf;
extern char *srcmapfn;

static const char *currentfnm = NULL;
static int   linenumber = 0;
       int   listlineno = 0;
static char *lineLbuff  = NULL;
static int   lineLflag  = FALSE;
static int   show_noncode_lines = TRUE;
static int   show_coded_lines   = TRUE;

#define LISTMODESTK (256)
static int listmodestk[LISTMODESTK];

static unsigned char    outresult[OUTRESULTLEN];
static unsigned short   outrs16  [OUTRESULTLEN / 2];
static int  nextresult;
static int  genlocctr, resultloc;

static unsigned mode_set = 0, mode_clr = 0, type_flag = 0;
static icartrom_t icart_rom;
extern ictype_t   icart_type;

static const char *oeptr;

#define MAXIMPWID   24

static int widthmask[MAXIMPWID+1] =
{
/*  0 */    1L,
/*  1 */    1L,
/*  2 */    (1L <<  2 ) - 1,
/*  3 */    (1L <<  3 ) - 1,
/*  4 */    (1L <<  4 ) - 1,
/*  5 */    (1L <<  5 ) - 1,
/*  6 */    (1L <<  6 ) - 1,
/*  7 */    (1L <<  7 ) - 1,
/*  8 */    (1L <<  8 ) - 1,
/*  9 */    (1L <<  9 ) - 1,
/* 10 */    (1L << 10 ) - 1,
/* 11 */    (1L << 11 ) - 1,
/* 12 */    (1L << 12 ) - 1,
/* 13 */    (1L << 13 ) - 1,
/* 14 */    (1L << 14 ) - 1,
/* 15 */    (1L << 15 ) - 1,
/* 16 */    (1L << 16 ) - 1,
/* 17 */    (1L << 17 ) - 1,
/* 18 */    (1L << 18 ) - 1,
/* 19 */    (1L << 19 ) - 1,
/* 20 */    (1L << 20 ) - 1,
/* 21 */    (1L << 21 ) - 1,
/* 22 */    (1L << 22 ) - 1,
/* 23 */    (1L << 23 ) - 1,
/* 24 */    (1L << 24 ) - 1
};

static int count_nl(const char *s)
{
    const char *nl = s;
    int cnt = 0;

    while ((nl = strchr(nl, '\n')) != NULL)
    {
        cnt++;
        nl++;
    }

    return cnt;
}


static void parsemode(const char *modestr)
{
    int action = 0;
    unsigned set = 0, clr = 0, bit;

    while (*modestr)
    {
        while (*modestr && *modestr == ',') modestr++;
        bit = action = 0;

        do {
            while (*modestr && strchr(" \t\n\r", *modestr))
                modestr++;

            if (!*modestr) break;

            if (!action) { action = *modestr++; continue; }
            switch (*modestr++)
            {
                case 'R': case 'r': bit |= ICARTROM_READ;    break;
                case 'W': case 'w': bit |= ICARTROM_WRITE;   break;
                case 'N': case 'n': bit |= ICARTROM_NARROW;  break;
                case 'B': case 'b': bit |= ICARTROM_BANKSW;  break;
                case '-': case '+': case '=':
                    frp2error("Mode syntax: Action char where mode "
                             "char expected"); break;
                default:
                    frp2error("Mode syntax: Unknown mode character"); break;
            }
        } while (*modestr && *modestr != ',');

        if (!action) break;

        switch (action)
        {
            case '+': { set |=  bit; clr &= ~bit; break; } 
            case '-': { set &= ~bit; clr |=  bit; break; } 
            case '=': { set  =  bit; clr  = ~bit; break; } 
            case 'R' : case 'W' : case 'B' : case 'N':
            case 'r' : case 'w' : case 'b' : case 'n':
                frp2error("Mode syntax: Missing action character"); break;
            default:
                frp2error("Mode syntax: Unknown action character"); break;
        }
    }

    mode_set = set & 0xF;
    mode_clr = clr & 0xF;
}

static void markrange(int startaddr, int endaddr)
{
    icartrom_addseg(&icart_rom, NULL, startaddr, endaddr - startaddr, 
                    mode_set, mode_clr);
}
        
void outphase(void)
/*
    description process all the lines in the intermediate file
    globals the output expression pointer
            line number
            file name
            the binary output array and counts
*/
{
    irec_union *rec;

    if (srcmapfn)
        sm_outpath();

    while ((rec = pass2_next_rec()) != NULL)
    {
        linenumber = rec->irec.line;

        if (rec->irec.type == REC_LIST_LINE)
        {
            if (listflag)  flushlisthex();
            if (lineLbuff) free(lineLbuff);

            lineLbuff = strdup(rec->string.string);
            lineLflag = show_coded_lines;
        }

        switch (rec->irec.type)
        {
            case REC_LIST_MODE: //'N':
            {
                int i, lm;
                lm = rec->list_mode.mode;

                if (lm == LIST_PREV)
                {
                    for (i = LISTMODESTK - 1; i > 0; i--)
                        listmodestk[i] = listmodestk[i - 1];
                    lm = listmodestk[LISTMODESTK-1];
                } else
                {
                    for (i = 1; i < LISTMODESTK - 1; i++)
                        listmodestk[i] = listmodestk[i + 1];
                    listmodestk[0]             = 0;
                    listmodestk[LISTMODESTK-1] = lm;
                }

                switch (lm)
                {
                    case LIST_ON: 
                        lineLflag          = show_noncode_lines;
                        show_coded_lines   = TRUE;
                        show_noncode_lines = TRUE;  break;
                    case LIST_CODE: 
                        lineLflag          = FALSE;
                        show_coded_lines   = TRUE;
                        show_noncode_lines = FALSE; break;
                    case LIST_OFF: 
                        lineLflag          = FALSE;
                        show_coded_lines   = FALSE;
                        show_noncode_lines = FALSE; break;
                }
                break;
            }

            case REC_ERROR:   //'E': /* error */
            {
                if(listflag)
                    flushsourceline();

                fputs(rec->string.string, loutf);
                fputc('\n', loutf);
                listlineno += 1 + count_nl(rec->string.string);

                break;
            }

            case REC_LIST_LINE:  //'L': /* listing */
            {
                break;
            }

            case REC_SET_EQU:
            {
                if (listflag && show_noncode_lines)
                {
                    char hbuf[12];
                    sprintf(hbuf,  "0x%X",   rec->set_equ.value);
                    fprintf(loutf, "%-*.*s", SOURCEOFFSET, SOURCEOFFSET, hbuf);
                    if(lineLflag)
                    {
                        fputs(lineLbuff, loutf);
                        lineLflag = FALSE;
                        listlineno += count_nl(lineLbuff);
                    } else
                    {
                        fputc('\n', loutf);
                        listlineno++;
                    }
                } else
                    lineLflag = FALSE;
                break;
            }

            case REC_COMMENT:  //'C': /* comment / uncounted listing */
            {
                if (listflag && show_noncode_lines)
                {
                    fprintf(loutf,"%-*.*s", 
                            SOURCEOFFSET, SOURCEOFFSET, rec->string.string);
                    if(lineLflag)
                    {
                        fputs(lineLbuff, loutf); /* already has newline */
                        lineLflag = FALSE;
                        listlineno += count_nl(lineLbuff);
                    } else
                    {
                        fputc('\n', loutf);
                        listlineno++;
                    }
                } else
                    lineLflag = FALSE;
                break;
            }

            case REC_USER_COMMENT: //'S': /* user comment */
            {
                /* User comments always get printed in the listing, but the
                   code that generates the comment never does. */
                if(listflag)
                {
                    fprintf(loutf,"%s\n", rec->string.string);
                    listlineno += 1 + count_nl(rec->string.string);
                    lineLflag = FALSE;
                } else
                    lineLflag = FALSE;
                break;
            }

            case REC_LOC_SET: //'P': /* location set */
            {
                currseg   = rec->loc_set.seg;
                locctr    = rec->loc_set.loc;
                genlocctr = rec->loc_set.loc;
                type_flag = rec->loc_set.type;
                parsemode  (rec->loc_set.mode);
                break;
            }

            case REC_RESERVE_RANGE:  //'R': /* reserve range */
            {
                markrange(genlocctr, rec->reserve_range.len);
                break;
            }

            case REC_MARK_RANGE: //'M': /* mark range with mode */
            {
                unsigned old_set = mode_set, old_clr = mode_clr;
                parsemode(rec->mark_range.mode);
                markrange(rec->mark_range.lo,
                          rec->mark_range.hi);
                mode_set = old_set;
                mode_clr = old_clr;
                break;
            }
        
            case REC_DATA_BLOCK: //'D': /* data */
            {
                oeptr      = rec->string.string;
                nextresult = 0;
                resultloc  = genlocctr;
                outeval();
                if (listflag && (show_coded_lines || show_noncode_lines))
                {
                    listhex();
                    if (hexflag && srcmapfn)
                        flushlisthex();
                }
                if (hexflag)
                    outhexblock();
                break;
            }

            case REC_FILE_START:
            case REC_FILE_EXIT:
            {
                currentfnm = rec->string.string;
                break;
            }

            default:
            {
                frp2error("unknown intermediate file command");
                break;
            }
        }

        pass2_release_rec(rec);
    }

    if(hexflag)
        flushhex();

    if(listflag)
        flushlisthex();

    if (srcmapfn)
        sm_flush();
}

void outeval(void)
/*
    description convert the polish form character string in the 
            intermediate file 'D' line to binary values in the
            output result array.
    globals     the output expression pointer
            the output result array
*/
{
    register int etop = 0;
    int offset = 0;

    estkm1p = &estk[0];

    while( *oeptr != '\0')
    {
        switch(*oeptr)
        {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            etop = (etop << 4) + ((*oeptr) - '0');
            break;

        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
            etop = (etop << 4) + ((*oeptr) - 'a' + 10);
            break;
        
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
            etop = (etop << 4) + ((*oeptr) - 'A' + 10);
            break;

#define FRAERR frp2error

#include "fraeuni.h"
#include "fraebin.h"
        case IFC_SYMB:
            {
                struct symel *tsy;

                tsy = symbindex[etop];
                if(tsy->seg <= 0)
                {
                    frp2undef(tsy);
                    etop = 0;
                }
                else
                {
                    if(tsy->seg == SSG_EQU ||
                       tsy->seg == SSG_SET)
                    {
                        frp2warn( "forward reference to SET/EQU symbol");
                    }
                    etop = tsy->value;
                }
            }
            break;

        case IFC_CURRLOC: 
            etop = genlocctr + (offset / 2);
            break;

        case IFC_PROGCTR:
            etop = genlocctr;
            break;

        case IFC_DUP:
            if(estkm1p >= &estk[PESTKDEPTH-1])
            {
                frp2error("expression stack overflow");
            }
            else
            {
                (++estkm1p)->v = etop;
            }
            break;

        case IFC_LOAD:
            if(estkm1p >= &estk[PESTKDEPTH-1])
            {
                frp2error("expression stack overflow");
            }
            else
            {
                (++estkm1p)->v = etop;
            }
            etop = 0;
            break;

        case IFC_CLR:
            etop = 0;
            break;

        case IFC_CLRALL:
            etop = 0;
            estkm1p = &estk[0];
            break;

        case IFC_POP:
            etop = (estkm1p--)->v;
            break;

        case IFC_TESTERR:
            if(etop)
            {
                frp2error("expression fails validity test");
            }
            break;

        case IFC_SWIDTH:
            if( etop > 0 && etop <= MAXIMPWID)
            {
                if( estkm1p->v < -(widthmask[etop-1]+1) ||
                    estkm1p->v > widthmask[etop-1] )
                {
                    frp2error("expression exceeds available field width");
                }
                etop = ((estkm1p--)->v)  & widthmask[etop];
            }
            else
            {
                frp2error("unimplemented width");
            }
            break;

        case IFC_WIDTH:
            if( etop > 0 && etop <= MAXIMPWID)
            {
                if( estkm1p->v < -(widthmask[etop-1]+1) ||
                    estkm1p->v > widthmask[etop] )
                {
                    frp2error("expression exceeds available field width");
                }
                etop = ((estkm1p--)->v)  & widthmask[etop];
            }
            else
            {
                frp2error("unimplemented width");
            }
            break;

        case IFC_IWIDTH:
            if( etop > 0 && etop <= MAXIMPWID)
            {
                unsigned sign_check = estkm1p->v & 0xFFFF8000;

                if (!(etop == 16 && 
                      (sign_check == 0xFFFF8000 || !sign_check)) &&
                     (estkm1p->v < 0 ||
                      estkm1p->v > widthmask[etop]))
                {
                    frp2error("expression exceeds available field width");
                }
                etop = ((estkm1p--)->v)  & widthmask[etop];
            }
            else
            {
                frp2error("unimplemented width");
            }
            break;

        case IFC_EMU8:
            if( etop >= -128 && etop <= 255)
            {
                outresult[nextresult++] = etop & 0xff;
            }
            else
            {
                outresult[nextresult++] = 0;
                frp2error("expression exceeds available field width");
            }
            offset++;
            etop = 0;
            break;

        case IFC_EMS7:
            if(etop >= -128 && etop <= 127)
            {
                outresult[nextresult++] = etop & 0xff;
            }
            else
            {
                outresult[nextresult++] = 0;
                frp2error("expression exceeds available field width");
            }
            offset++;
            etop = 0;
            break;

        case IFC_EM16:
            if(etop >= -32768L && etop <= 65535L)
            {
                outresult[nextresult++] = (etop >> 8) & 0xff;
                outresult[nextresult++] = etop & 0xff;
            }
            else
            {
                outresult[nextresult++] = 0;
                outresult[nextresult++] = 0;
                frp2error("expression exceeds available field width");
            }
            offset += 2;
            etop = 0;
            break;

        case IFC_EMBR16:
            if(etop >= -32768L && etop <= 65535L)
            {
                outresult[nextresult++] = etop & 0xff;
                outresult[nextresult++] = (etop >> 8) & 0xff;
            }
            else
            {
                outresult[nextresult++] = 0;
                outresult[nextresult++] = 0;
                frp2error("expression exceeds available field width");
            }
            offset += 2;
            etop = 0;
            break;

        default:
            break;
        }
        oeptr++;
    }

    genlocctr += offset / 2;
}

static int lhaddr, lhnextaddr;
static int lhnew, lhnext = 0;
static unsigned char listbuffhex[NUMHEXPERL];

void flushlisthex(void)
/*
    description output the residue of the hexidecimal values for
            the previous assembler statement.
    globals     the new hex list flag
*/
{
    listouthex();
    lhnew = TRUE;
}

void listhex(void)
/*
    description buffer the output result to block the hexidecimal 
            listing on the output file to NUMHEXPERL bytes per
            listing line.
    globals     The output result array and count
            the hex line buffer and counts
*/
{
    register int cht;
    register int inhaddr = resultloc;

    if(lhnew)
    {
        lhaddr = lhnextaddr = resultloc;
        lhnew = FALSE;
    }

    for(cht = 0; cht < nextresult; cht += 2)
    {
        if(lhnextaddr != inhaddr 
         || lhnext >= (lineLflag ? NUMHEXSOURCE : NUMHEXPERL ) )
        {
            listouthex();
            lhaddr = lhnextaddr = inhaddr;
        }
        listbuffhex[lhnext++] = outresult[cht];
        listbuffhex[lhnext++] = outresult[cht+1];
        lhnextaddr++;
        inhaddr++;
    }
}

void listouthex(void)
/*
    description print a line of hexidecimal on the listing
    globals     the hex listing buffer
*/
{
    register int cn;

    if (lhnext > 0)
    {
        fprintf(loutf, "%.4X ", 0xFFFF & (int)lhaddr);

        for(cn = 0; cn < lhnext; cn += 2)
        {
            fprintf(loutf, "%.2X%.2X ",
                    0xFF & (int) listbuffhex[cn    ],
                    0xFF & (int) listbuffhex[cn + 1]);
        }

        if (!lineLflag)
        {
            listlineno++;
            fputc('\n', loutf);
        }
    } else
    {
        if (!show_noncode_lines)
            lineLflag = FALSE;
    }

    if(lineLflag)
    {
        if(lineLbuff[0] != '\n')
        {
            switch(lhnext)
            {
            case 0:
            case 1:
                fputs("\t\t\t",loutf);
                break;
            case 2:
            case 3:
            case 4:
                fputs("\t\t",loutf);
                break;
            case 5:
            case 6:
                fputs("\t",loutf);
                break;
            default:
                break;
            }

            fputs(lineLbuff, loutf);
            listlineno += count_nl(lineLbuff);
            lineLflag = FALSE;
        }
        else
        {
            fputc('\n', loutf);
            listlineno++;
        }
    }
        
    lhnext = 0;
}

static const char *sm_last_file = NULL;
static int sm_start = -2, sm_end = -2, sm_line = -2, sm_list = -2; 
static unsigned int sm_type = ~2;

void sm_outpath(void)
{
    path_t *path = as1600_search_path;

#ifndef NO_GETCWD
    char *cwdbuf = (char *)malloc(65536);
    if (cwdbuf)
    {
        char *cwd = getcwd(cwdbuf, 65536);
        if (cwd)
            fprintf(smapf, "CWD %s\n", cwd);

        free(cwdbuf);
    }
#endif

    while (path)
    {
        fprintf(smapf, "PATH %s\n", path->name);
        path = path->next;
    }

    if (listflag)
        fprintf(smapf, "LISTING %s\n", loutfn);
}

void sm_flush()
{
    if (sm_start > 0)
        fprintf(smapf, "%4X %4X %2X %d %d\n", 
                sm_start, sm_end, sm_type, sm_line, sm_list);

    if (currentfnm != sm_last_file)
    {
        fprintf(smapf, "FILE %s\n", currentfnm);
        sm_last_file = currentfnm;
    }

    sm_start = sm_end = sm_line = -2;
}

void sm_outrange(int lo, int hi)
{
    int list = show_coded_lines ? listlineno : 0;

    if (currentfnm != sm_last_file || 
       lo          != sm_end + 1   || 
       linenumber  != sm_line      ||
       list        != sm_list      ||
       type_flag   != sm_type)
    {
        sm_flush();
        sm_line  = linenumber;
        sm_start = lo;
        sm_end   = hi;
        sm_type  = type_flag;
        sm_list  = list;
        return;
    }

    sm_end = hi;
    return;
}


static int  nextoutaddr, blockaddr;
extern unsigned int memory_bitmap[65536 >> 5];

void outhexblock(void)
/*
    description buffer the output result to group adjacent output
                data into longer lines.
    globals     the output result array
                the intel hex line buffer
*/
{
    int i;

    blockaddr = resultloc;
    nextoutaddr = blockaddr + currseg;

    for (i = 0; i < nextresult; i += 2)
        outrs16[i>>1] = (outresult[i] << 8) | (0xFF & outresult[i + 1]);
        
    icartrom_addseg(&icart_rom, outrs16, nextoutaddr, nextresult >> 1, 
                    mode_set, mode_clr);

    for (i = nextoutaddr; i < nextoutaddr + (nextresult>>1); i++)
        memory_bitmap[i >> 5] |= 1 << (i & 31);

    if (srcmapfn)
        sm_outrange(nextoutaddr, nextoutaddr + (nextresult >> 1) - 1);
}


void flushhex(void)
/*
    description flush the intel hex line buffer at the end of
            the second pass
    globals     the intel hex line buffer
*/
{
    uint_32 size;
    uint_8  *rom_img;

#if 0
    if(hnextsub > 0)
        intelout(0, blockaddr, hnextsub, hlinebuff);
    if(endsymbol != SYMNULL && endsymbol->seg > 0)
        intelout(1, endsymbol->value, 0, "");
    else
        intelout(1, 0L, 0, "");
#endif
        
    if (binoutf && cfgoutf)
    {
        icb_write_bincfg(binoutf, cfgoutf, &icart_rom, 0);
    } 

    if (romoutf)
    {
        rom_img = icartrom_genrom(&icart_rom, &size, icart_type);
        if (rom_img)
        {
            fwrite(rom_img, 1, size, romoutf);
            fflush(romoutf);
        }
        free(rom_img);
    }
}



#define UBUFSZ (32)

static struct 
{
    int        line;
    const char *file;
    const char *symb;
} undef_filt[UBUFSZ];

static unsigned undef_filt_idx = 0;

void frp2undef(struct symel *symp)
/*
    description second pass - print undefined symbol error message on
            the output listing device.  If the the listing flag
            is false, the output device is the standard output, and
            the message format is different.
    parameters  a pointer to a symbol table element
    globals     the count of errors
*/
{
    int i;

    if(listflag)
        flushsourceline();

    /* filter out redundant errors that can happen due to multiple refs
       within the same polish expression, or multiple references on the
       same line */
    for (i = 0; i < UBUFSZ; i++)
    {
        if (!undef_filt[i].file)
            continue;

        if (undef_filt[i].line == linenumber &&
            undef_filt[i].file == currentfnm &&
            undef_filt[i].symb == symp->symstr)
            return;
    }
    
    /* insert this error in the filter */
    undef_filt[undef_filt_idx].file = currentfnm;
    undef_filt[undef_filt_idx].line = linenumber;
    undef_filt[undef_filt_idx].symb = symp->symstr;
    undef_filt_idx = (undef_filt_idx + 1) % UBUFSZ;
    
    /* Now print the error. */
    if ((symp->flags & SFLAG_ARRAY) == 0)
    {
        fprintf(loutf, "%s:%d: ERROR - undefined symbol  %s\n", 
                currentfnm, linenumber, symp->symstr);
        listlineno++;
    } else
    {
        const char *symstr = symp->symstr;
        int         first  = 1;

        do
        {
            const char *aidx_str = strchr(symstr, 0x01);
            unsigned    aidx_val = 0;

            if (aidx_str && 
                (aidx_str[1] & 0x80) == 0x80  &&
                (aidx_str[2] & 0x80) == 0x80  &&
                (aidx_str[3] & 0x80) == 0x80  &&
                (aidx_str[4] & 0x80) == 0x80  &&
                (aidx_str[5] & 0x80) == 0x80)
            {
                aidx_val = ((aidx_str[1] & 0x7F) <<  0)
                         | ((aidx_str[2] & 0x7F) <<  7)
                         | ((aidx_str[3] & 0x7F) << 14)
                         | ((aidx_str[4] & 0x7F) << 21)
                         | ((aidx_str[5] & 0x0F) << 28);
                symstr  = aidx_str + 6;
            } else
            {
                if (!first)
                {
                    fprintf(loutf, "\n");
                    listlineno++;
                }
                fprintf(loutf, 
                        "%s:%d: INTERNAL ERROR IN ARRAY ENCODING",
                        currentfnm, linenumber);
                break;
            }
            if (first)
                fprintf(loutf, "%s:%d: ERROR - undefined array index %.*s",
                        currentfnm, linenumber, 
                        (int)(aidx_str - symp->symstr), 
                        symp->symstr);
            first = 0;
            fprintf(loutf, "[%d]", aidx_val);
        } while (*symstr);
        fprintf(loutf, "\n");
        listlineno++;
    }

    errorcnt++;
}

void frp2warn(char *str)
/*
    description second pass - print a warning message on the listing
            file, varying the format for console messages.
    parameters  the message
    globals     the count of warnings
*/
{
    if(listflag)
        flushsourceline();

    fprintf(loutf, "%s:%d: WARNING - %s\n", currentfnm, linenumber, str);
    listlineno += 1 + count_nl(str);
    warncnt++;
}


void frp2error(char *str)
/*
    description second pass - print a message on the listing file
    parameters  message
    globals     count of errors
*/
{
    flushsourceline();

    fprintf(loutf, "%s:%d: ERROR - %s\n", currentfnm, linenumber, str);
    listlineno += 1 + count_nl(str);
    errorcnt++;
}

void flushsourceline(void)
/*
    description flush listing line buffer before an error for
            that line is printed
*/
{
    if(listflag && lineLflag && show_noncode_lines)
    {
        fputs("\t\t\t", loutf);
        fputs(lineLbuff, loutf);
        listlineno += count_nl(lineLbuff);
    }
    lineLflag = FALSE;
}
