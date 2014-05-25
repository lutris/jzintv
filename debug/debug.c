/*
 * ============================================================================
 *  Title:    Debugger
 *  Author:   J. Zbiciak
 * ============================================================================
 *  This module will implement the debugger as a combination of interactive
 *  interface and a peripheral which maps into the entire address space
 *  which allows capturing memory events.
 * ============================================================================
 */

#define STK_TRC 1

#include "config.h"
#include "periph/periph.h"
#include "cp1600/cp1600.h"
#include "cp1600/op_decode.h"
#include "gfx/gfx.h"
#include "demo/demo.h"
#include "stic/stic.h"
#include "speed/speed.h"
#include "util/symtab.h"
#include "debug_.h"
#include "debug_tag.h"
#include "debug_dasm1600.h"
#include "file/file.h"
#include "debug/source.h"

#define HISTSIZE (0x10000)
#define HISTMASK (HISTSIZE-1)
#define RH_RECSIZE (16)

LOCAL void debug_write_memattr(const char *fname);
LOCAL uint_32 debug_peek(periph_t *p, periph_t *r, uint_32 a, uint_32 d);
LOCAL void debug_poke(periph_t *p, periph_t *r, uint_32 a, uint_32 d);
LOCAL void debug_disasm_cache_inval(void);
LOCAL void debug_print_reghist(int count, int offset);

LOCAL int dc_hits = 0, dc_miss = 0, dc_nocache = 0; 
LOCAL int dc_unhook_ok = 0, dc_unhook_odd = 0;

LOCAL uint_16 *debug_reghist = NULL;
LOCAL uint_32 *debug_profile = NULL;
LOCAL uint_8  *debug_memattr = NULL;
LOCAL uint_16 *debug_mempc   = NULL;
LOCAL int      debug_rh_ptr = -1;
LOCAL int      debug_histinit = 0;
LOCAL void debug_write_reghist(const char *, periph_p, cp1600_t *);
LOCAL symtab_t *debug_symtab;
LOCAL int disasm_mode = 0;  /* -1 is disasm only, 0 is mixed, 1 is src only */

LOCAL uint_32 debug_watch_w[0x10000 >> 5];
LOCAL uint_32 debug_watch_r[0x10000 >> 5];
#define WATCHING(x,y) ((int)((debug_watch_##y[(x) >> 5] >> ((x) & 31)) & 1))
#define WATCHTOG(x,y) ((debug_watch_##y[(x) >> 5] ^= 1 << ((x) & 31)))

/* JSR table:  The first address is the address of the JSR and the second is 
 * the return address.  JSR_RET_WINDOW is the lookahead window for watching
 * reads past the address of a JSR instruction to detect data-after-JSR.
 */
#define MAX_JSRS (128)
#define JSR_RET_WINDOW (3)  
LOCAL uint_16 debug_jsrs[MAX_JSRS][2];
LOCAL int debug_num_jsrs = 0;

int debug_fault_detected = 0;
const char *debug_halt_reason = NULL;

LOCAL char str_null[] = "(null)";

#ifdef STK_TRC

#define MAX_STK (256)

LOCAL uint_16 stk_pc[MAX_STK];
LOCAL uint_32 stk_deep = 0;
LOCAL uint_32 stk_base = 0;
LOCAL uint_32 stk_wr   = 0;

LOCAL FILE *stk_trc = NULL;


#endif

/* ======================================================================== */
/*  DEBUG_PRINT_USAGE                                                       */
/* ======================================================================== */
LOCAL void debug_print_usage(void)
{
    jzp_printf(
"jzIntv Debugger Commands\n"
"\n"
"Running the game:\n"
"   r<#>        'R'un <#> cycles.  <#> defaults to \"infinity\"\n"
"   s<#>        'S'tep <#> cycles showing disassembly as it runs.  \n"
"               <#> defaults to 1 cycle. -1 means \"forever.\"\n"
"   t<#>        'T'race-over a JSR.  Similar to \"step\" except it attempts\n"
"               to step over functions called by JSR.\n"
"   [enter]     [enter] with no command is the same as \"s1\" or \"t1\",\n"
"               depending on whether \"s\" or \"t\" was used most recently\n"
"   f<#>        'F'ast forward to <#>.  When running in fast-forward mode,\n"
"               the debugger skips all breakpoints and will only stop at the\n"
"               target address or when the user pushes the BREAK key.  <#>\n"
"               defaults to the current program counter\n"
"   z           Toggle showing timestamps during 'step'\n"
"   x           Toggle showing CPU reads and writes during 'step'\n"
"   b<#>        Set a 'B'reakpoint at <#>.  <#> defaults to the current PC.\n"
"   n<#>        u'N'set a breakpoint at <#>.  <#> defaults to the current PC \n"
"\n"
">> Note:  Pressing the BREAK key while running or stepping will drop jzIntv\n"
">>        back to the debugger prompt.  BREAK is usually bound to F4.\n"
"\n"
"Viewing / changing state:\n"
"   u<#1> <#2>  'U'nassemble <#2> instructions starting at <#1>.  <#2>\n"
"               defaults to 10.  <#1> defaults to the current PC after 'r'un\n"
"               or 's'tep, or to the next address after 'u'nassemble.\n"
"   m<#1> <#2>  Show <#2> 'm'emory locations starting at <#1>.  <#2> defaults\n"
"               to 40 (hex).  <#1> defaults to the first undisplayed location\n"
"               after the last 'm' command.\n"
"   g<#1> <#2>  Write the value <#2> to re'G'ister <#1>.\n"
"   e<#1> <#2>  'E'nter the value <#2> to memory location <#1>.\n"
"   p<#1> <#2>  'P'oke the value <#2> to memory location <#1>.  'P'oke can\n"
"               always write to GRAM and STIC registers whereas 'E'nter\n"
"               has the same restrictions as the CPU.\n"
"   w<#1> <#2>  Toggle write 'w'atching on range from <#1> through <#2>,\n"
"               inclusive.  If <#2> is omitted, it toggles the watch flag for\n"
"               a single location\n"
"   @<#1> <#2>  Toggle read watching on range from <#1> through <#2>,\n"
"               inclusive.  If <#2> is omitted, it toggles the watch flag for\n"
"               a single location\n"
"\n"
"STIC Specific\n"
"   ^           Toggle displaying dropped writes to STIC registers or GRAM\n"
"   %%           Toggle displaying dropped reads of STIC registers or GRAM\n"
"   $           Toggle displaying STIC video FIFO loads\n"
"   #           Toggle halting when display blanks\n"
/*
"   $<path>     Enable STIC video FIFO checks with bitmap in <path>\n"
"   $           Disable STIC video FIFO checks\n"
*/
"\n"
"Statistics / History tracking:\n"
"   d           Dump CPU and memory state to a series of files.  Requires\n"
"               history or attribute logging to be enabled.\n"
"   h           Toggle history logging.  Use \"d\" to dump to \"dump.hst\"\n"
"               and \"dump.cpu\"\n"
"   a           Toggle memory attribute logging.  Use \"d\" to dump to \n"
"               \"dump.atr\"\n"
"   !<#1> <#2>  Print the last <#1> instructions that ran, ending <#2>\n"
"               cycles back. <#1> defaults to 40, <#2> defaults to 0.\n"
"\n"
"Miscellaneous commands:\n"
"   l<path>     Load symbol table from <path>.  Format must be same as that\n"
"               output by as1600's -s flag\n"
"   /<string>   Look for symbols containing <string>.  If <string> is also a\n"
"               valid hex number, look for symbols at that address.  To only\n"
"               look for symbols at an address use /$<addr>\n"
"   o<#>        Set label/address 'o'utput format.  # is one of:\n"
"                   0 => LABEL ($1234)       <--- default\n"
"                   1 => LABEL\n"
"                   2 => $1234 (LABEL)\n"
"                   3 => $1234\n"
"   < <path>    Execute a script from <path>. Scripts are just collections of\n"
"               debugger commands, one per line.\n"
#ifdef WIN32
"   > <#>       Change the command window width to <#>\n"
#endif
"   q           Quit jzIntv\n"
"   ?           Print this usage information\n"
"\n"
">> Most commands that take an address can also take a symbol defined by a\n"
">> symbol table file.  These are output by as1600's -s flag.  You can load a\n"
">> symbol table into jzIntv with the --sym-file=<path> command line flag or\n"
">> with the 'L'oad command shown above\n"
"\n"
);
}

/* ======================================================================== */
/*  DEBUG_TAG_RANGE                                                         */
/* ======================================================================== */
void debug_tag_range(uint_32 lo, uint_32 hi, uint_32 flags)
{
    if (!debug_memattr)
    {
        debug_memattr = CALLOC(uint_8 ,  0x10000);
        debug_mempc   = CALLOC(uint_16,  0x10000);
        if (!debug_memattr || !debug_mempc)
        {
            CONDFREE(debug_memattr);
            CONDFREE(debug_mempc  );
            return;
        }
    }
    if (debug_memattr)
    {
        while (lo <= hi)
            debug_memattr[lo++] |= flags;
    }
}

/* ======================================================================== */
/*  DEBUG_READ_SYMTBL                                                       */
/* ======================================================================== */
LOCAL void debug_read_symtbl(const char *fname)
{
    FILE *f;
    char buf[512], symb[512];

    if ((f = fopen(fname, "r")) == NULL)
    {
        jzp_printf("debug: Could not open symbol table file '%s' for reading\n",
                    fname);
        return;
    }

    if (!debug_symtab)
        debug_symtab = symtab_create();

    jzp_printf("debug: Reading symbols from %s\n", fname);

    /* -------------------------------------------------------------------- */
    /*  Read in lines from .sym file from as1600.  Format is as follows:    */
    /*                                                                      */
    /*      ???????? DOIDLEHST                                              */
    /*      ???????? IDLEHST                                                */
    /*      0000a000 _GFX                                                   */
    /*      0000a000 TOPMOUNT                                               */
    /*                                                                      */
    /*  Ignore undefined symbols and symbols outside the range 0000 - FFFF. */
    /* -------------------------------------------------------------------- */
    while (fgets(buf, 512, f) != NULL)
    {
        uint_32 addr = 0, old_addr;

        if (buf[0] == '?')
            continue;

        addr = 0xFFFFFFFF;
        symb[0] = 0;
        if (sscanf(buf, "%x %s", &addr, symb) != 2)
            continue;

        if (addr > 0xFFFF || symb[0] == 0)
            continue;

        if (symtab_getaddr(debug_symtab, symb, &old_addr) == 0)
        {
            if (old_addr != addr)
            {
                jzp_printf("debug: Warning, '%s' already has value $%.4X.\n"
                           "       Ignoring new value $%.4X\n",
                           symb, old_addr, addr);
            }
        } else
        {
            symtab_defsym(debug_symtab, symb, addr);
        }
    }

    fclose(f);
}

/* ======================================================================== */
/*  DEBUG_DECODE_VAL -- Decide if a string is a label or a hex constant     */
/*                      and return the value.                               */
/* ======================================================================== */
LOCAL int debug_decode_val(const char *str, uint_32 maxval)
{
    uint_32 val;
    const char *x;
    const char *s = str;
    int must_hex = 0;
    
    if (s[0] == '$') { s++; must_hex = 1; }
    else if (debug_symtab && (symtab_getaddr(debug_symtab, s, &val) == 0))
        goto got_it;

    for (x = s; *x; x++)
        if (!(isdigit(*x) || (toupper(*x) >= 'A' && toupper(*x) <= 'F')))
        {
            jzp_printf(must_hex ? "Error parsing: %s\n" 
                                : "Could not find symbol named '%s'\n", str);
            return -1;
        }

    if (sscanf(s, "%x", &val) != 1)
    {
        jzp_printf("Error parsing: %s\n", s);
        return -1;
    }

got_it:

    if (val > maxval)
    {
        jzp_printf("Value %.8X is out of range.\n", val);
        return -1;
    }

    return (int)val;
}

/* ======================================================================== */
/*  DEBUG_SYMB_FOR_ADDR  -- Return a parenthesized string associated with   */
/*                          an address, if any, else an empty string.       */
/* ======================================================================== */
LOCAL char *debug_symb_for_addr(debug_t *debug, uint_32 addr, char *s4a_buf)
{
    const char *symb;
    const char *temp;
    int which;
    int fmt = debug->symb_addr_format;
    int len;

    if (!debug_symtab || !(symb = symtab_getsym(debug_symtab, addr, 0, 0)))
    {
        fmt = 3;
        len = 0;
    } else
    {
        len = strlen(symb);
    }

    if (fmt < 3)
    {
        /* Skip symbol names that start with "." if there are alternatives. */
        which = 1;
        while (symb[0] == '.' && 
               (temp = symtab_getsym(debug_symtab, addr, 0, which)) != NULL)
        {
            symb = temp;
            which++;
        }
    }
    
    switch (fmt)
    {
        case 0:
            if (len > 27)
                len = 27;

            sprintf(s4a_buf, "%*s ($%.4X)", len, symb, addr);
            break;

        case 1:
            if (len > 35)
                len = 35;

            sprintf(s4a_buf, "%*s", len, symb);
            break;

        case 2:
            if (len > 27)
                len = 27;

            sprintf(s4a_buf, "$%.4X (%*s)", addr, len, symb);
            break;

        case 3:
            sprintf(s4a_buf, "$%.4X", addr);
            break;
    }

    return s4a_buf;
}

/* ======================================================================== */
/*  DEBUG_SEARCH_SYMBOL  -- Search for symbols matching a string.           */
/* ======================================================================== */
LOCAL void print_symbol(const char *symbol, uint_32 addr, int which)
{
    UNUSED(which);
    jzp_printf(" $%.4X %s\n", addr, symbol);
}

LOCAL void debug_search_symbol(const char *search_string)
{
    char *copy;
    char *s, *start;
    uint_32 addr;

    if (!debug_symtab)
        return;

    s = copy = strdup(search_string);

    while (*s && isspace(*s))
        s++;

    start = s;

    if (*start == '$')
    {
        start++;
        goto must_hex;
    }

    while (*s && !isspace(*s) && *s != '\r' && *s != '\n')
        s++;

    *s = 0;

    if (*start)
        symtab_grep_for_symbol(debug_symtab, print_symbol, start);

    for (s = start; *s; s++)
        if (!(isdigit(*s) || (toupper(*s) >= 'A' && toupper(*s) <= 'F')))
            goto not_hex;

must_hex:
    if (sscanf(start, "%x", &addr) == 1)
    {
        int i = 0;
        const char *symb;

        while ((symb = symtab_getsym(debug_symtab, addr, 0, i)) != NULL)
        {
            print_symbol(symb, addr, i);
            i++;
        }
    }

not_hex:
    free(copy);
}

/* ======================================================================== */
/*  DEBUG_HIT_JSR_RET -- Get JSR address for apparent "step over" PC addr,  */
/*                       and remove address from list of active tracepoints */
/* ======================================================================== */
LOCAL uint_32 debug_get_jsr_addr(uint_32 ret_addr)
{
    int i, j;
    uint_32 jsr_addr = 0;

    for (i = 0; i < debug_num_jsrs; i++)
    {
        if (debug_jsrs[i][1] == ret_addr)
        {
            jsr_addr = debug_jsrs[i][0];
            for (j = i + 1; j < debug_num_jsrs; i++, j++)
            {
                debug_jsrs[i][0] = debug_jsrs[j][0];
                debug_jsrs[i][1] = debug_jsrs[j][1];
            }
            debug_num_jsrs--;
        }
    }

    return jsr_addr;
}

/* ======================================================================== */
/*  DEBUG_SET_JSR_RET -- Set JSR address and return address when stepping   */
/*                       over a JSR instruction.                            */
/* ======================================================================== */
LOCAL void debug_set_jsr_ret(uint_32 jsr_addr, uint_32 ret_addr)
{
    int i;

    /* Replace redundant tracepoints */
    for (i = 0; i < debug_num_jsrs; i++)
        if (debug_jsrs[i][0] == jsr_addr)
        {
            debug_jsrs[i][0] = jsr_addr;
            debug_jsrs[i][1] = ret_addr;
            return;
        }

    /* Don't track more than MAX_JSRS outstanding JSRs. */
    if (debug_num_jsrs >= MAX_JSRS)
        return;

    debug_jsrs[debug_num_jsrs][0] = jsr_addr;
    debug_jsrs[debug_num_jsrs][1] = ret_addr;
    debug_num_jsrs++;
}

/* ======================================================================== */
/*  DEBUG_CHK_JSR_RET -- Watch for reads to JSR return locations, and move  */
/*                       them forward if they're seen.  This is intended to */
/*                       catch functions that have arguments after the JSR  */
/* ======================================================================== */
LOCAL void debug_chk_jsr_ret(cp1600_t *cp, uint_32 read_addr)
{
    int i;

    /* -------------------------------------------------------------------- */
    /*  Ignore reads if we have no tracepoints set or the read is an        */
    /*  instruction fetch (PC == read address).                             */
    /* -------------------------------------------------------------------- */
    if (!debug_num_jsrs)
        return;

    if (cp->r[7] == read_addr)
        return;

    /* -------------------------------------------------------------------- */
    /*  Adjust the return address if we see a read to an address in the     */
    /*  range "ret_addr" to "ret_addr + JSR_RET_WINDOW", inclusive.  Set    */
    /*  the return address to 1 after the address of the read.              */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < debug_num_jsrs; i++)
    {
        if (read_addr >=           debug_jsrs[i][1] && 
            read_addr <= (uint_16)(debug_jsrs[i][1] + JSR_RET_WINDOW))
        {
            cp1600_clr_breakpt(cp, debug_jsrs[i][1], CP1600_BKPT_ONCE);
            cp1600_set_breakpt(cp, read_addr + 1,    CP1600_BKPT_ONCE);
            debug_jsrs[i][1] = read_addr + 1;
            return;
        }
    }

    return;
}

/* ======================================================================== */
/*  DEBUG_FIND_JSRS  -- If we're stepping over JSRs, handle any JSR at the  */
/*                      current PC.                                         */
/* ======================================================================== */
LOCAL int debug_find_jsrs(periph_t *p, cp1600_t *cp, uint_32 pc)
{
    uint_16 n1, n2;

    /* Is this a JUMP family instruction? */
    n1 = periph_peek((periph_t*)p->bus, p, (pc + 0) & 0xFFFF, ~0) & 0x3FF;
    n2 = periph_peek((periph_t*)p->bus, p, (pc + 1) & 0xFFFF, ~0) & 0x3FF;
    if (n1 == 0x0004 && (n2 & 0x300) != 0x300)
    {
        cp1600_set_breakpt(cp, pc + 3, CP1600_BKPT_ONCE);
        debug_set_jsr_ret(pc, pc + 3);
        return 1;
    }

    return 0;
}

/* ======================================================================== */
/*  DEBUG_OPEN_SCRIPT   Open up a new debugger script file                  */
/* ======================================================================== */
LOCAL void debug_open_script(debug_t *debug, const char *script)
{
    if (debug->filestk_depth < DEBUG_MAX_FILESTK - 1)
    {
        FILE *f = fopen(script, "r");
        if (!f)
            jzp_printf("debug: Could not open script file '%s'\n", script);
        else
            debug->filestk[++debug->filestk_depth] = f;
    } else
        jzp_printf("debug: Script files nested too deeply\n");
}
                                    
/* ======================================================================== */
/*  DEBUG_SHOW_VICINITY  -- Show the source code in the vicinity of an      */
/*                          address, if available.                          */
/* ======================================================================== */
LOCAL void debug_show_vicinity(uint_32 addr, int range)
{
    int file, line;
    int i;

    file = file_line_for_addr(addr, &line);

    if (file < 0)
    {
        jzp_printf("Unable to find source for address $%.4X\n", addr);
        return;
    }

    for (i = -range; i <= range; i++)
    {
        const char *src = source_for_file_line(file, line + i);

        if (src)
            jzp_printf("%c%s\n", i ? ' ' : '>', src);
    }
}

#ifdef STK_TRC
/* ======================================================================== */
/*  DEBUG_INFER_STACK    -- Try to figure out a reasonable base for the     */
/*                          stack.                                          */
/* ======================================================================== */
LOCAL uint_16 debug_infer_stack(cp1600_t *cp)
{
    uint_32 val;

    /* First look for the symbol _m.stk */
    if (debug_symtab && (symtab_getaddr(debug_symtab, "_m.stk", &val) == 0))
        return val;

    /* Next look for the symbol STACK */
    if (debug_symtab && (symtab_getaddr(debug_symtab, "STACK", &val) == 0))
        return val;

    /* Finally, assume R6 is at bottom of stack. */
    return cp->r[6];
}
#endif

/*
 * ============================================================================
 *  DEBUG_RD         -- Capture/print a read event.
 *  DEBUG_WR         -- Capture/print a write event.
 *  DEBUG_TK         -- Debugger 'tick' function.  This where the user command
 *                      line called from.
 * ============================================================================
 */
uint_32 debug_rd(periph_t *p, periph_t *r, uint_32 a, uint_32 d)
{
    debug_t *debug = (debug_t*)p;
    cp1600_t *cp = debug->cp1600;
    char addrbuf[36], pcbuf[36];

    if (p == r->req)
        return ~0;

    debug_chk_jsr_ret(cp, a);

    if (debug->show_rd || WATCHING(a,r))
    {
        jzp_printf(" RD a=%s d=%.4X %-16s (PC = %s) t=%llu\n", 
                debug_symb_for_addr(debug, a, addrbuf),
                d, r==(periph_p)p->bus ? r->req->name:r->name, 
                debug_symb_for_addr(debug, cp->oldpc, pcbuf),
                cp->periph.now);
    }

    if (debug_memattr)
    {
        if (cp->r[7] == a) 
            debug_memattr[a] |= DEBUG_MA_CODE;
        else if (cp->D)
            debug_memattr[a] |= DEBUG_MA_SDBD | DEBUG_MA_DATA;
        else
            debug_memattr[a] |= DEBUG_MA_DATA;

        debug_mempc[a]  = cp->r[7];
    }

#ifdef STK_TRC
    if (stk_trc && a <= stk_wr && a >= stk_base && (a == cp->r[6] - 1u))
    {
        if (stk_wr - stk_base >= stk_deep)
        {
            int i;

            stk_deep = stk_wr - stk_base + 1;

            fprintf(stk_trc, "Deep: %d\n", stk_deep);
            for (i = 0; i < (int)stk_deep; i++)
                fprintf(stk_trc, " %.4X", stk_pc[i]);

            fprintf(stk_trc, "\n");
            fflush(stk_trc);
        }
    }
#endif

    return ~0U;
}

void    debug_wr(periph_t *p, periph_t *r, uint_32 a, uint_32 d)
{
    debug_t *debug = (debug_t*)p;
    cp1600_t *cp = debug->cp1600;
    char addrbuf[36], pcbuf[36];

    if (p == r->req)
        return;

    if (debug->show_wr || WATCHING(a,w))
    {
        jzp_printf(" WR a=%s d=%.4X %-16s (PC = %s) t=%llu\n", 
                debug_symb_for_addr(debug, a, addrbuf),
                d, r==(periph_p)p->bus ? r->req->name:r->name, 
                debug_symb_for_addr(debug, cp->oldpc, pcbuf),
                cp->periph.now);
    }

    if (debug_memattr)
    {
        debug_memattr[a] |= DEBUG_MA_DATA | DEBUG_MA_WRITE;
        debug_mempc  [a]  = cp->r[7];
    }

#ifdef STK_TRC
    if (stk_trc && a >= stk_base && a < stk_base + MAX_STK && a == cp->r[6])
    {
        stk_wr = a;
        stk_pc[a - stk_base] = cp->oldpc;
    }
#endif
}

LOCAL uint_32 debug_peek(periph_t *p, periph_t *r, uint_32 a, uint_32 d)
{
    UNUSED(p);
    UNUSED(r);
    UNUSED(a);
    UNUSED(d);
    return ~0U;
}

LOCAL void debug_poke(periph_t *p, periph_t *r, uint_32 a, uint_32 d)
{
    UNUSED(p);
    UNUSED(r);
    UNUSED(a);
    UNUSED(d);
}


uint_32 debug_tk(periph_t *p, uint_32 len)
{
    debug_t *debug = (debug_t*)p;
    cp1600_t *cp = debug->cp1600;
    uint_32 pc = cp->r[7];
    char    buf[1024], *s;
    static  uint_16 next_hex_dump = 0x0000;
    static  int     prev_pc = ~0U;
    static  int     non_int = 0;
    static  int     show_time = 1;
    static  int     show_rdwr = 0;
    static  int     last_over = 0;
    static  uint_32 fast_fwd  = 0, ff_bkpt = 0; 
    static  uint_64 prev_rh_now   = 0;
    static  int     prev_rh_intrq = 0;
    sint_32 slen = (sint_32)len;
    uint_64 instrs = cp->tot_instr - debug->tot_instr;
    uint_64 now = cp->periph.now;
    uint_16 next_disassem = pc;
    int     intrq, busrq, wind = 0, show_once = 0;
    /*static req_bus_t old_req = { 0, 0, 0, 0, 0, 0, 0 };*/
    /*static uint_64 old_now = 0;*/

    debug->tot_instr = cp->tot_instr;

    /* get current INTRM, BUSRQ */
    intrq = (cp->req_bus.intrq & 1);
    busrq = (cp->req_bus.intrq & 2) >> 1;

    /* See if we've edged outside the INTRQ window or into the next */
    intrq &= now <  cp->req_bus.intrq_until;
    intrq |= now >= cp->req_bus.next_intrq;

    /* See if we've edged outside the BUSRQ window or into the next */
    busrq &= now <  cp->req_bus.busrq_until;
    busrq |= now >= cp->req_bus.next_busrq;

    /* Merge intrq/busrq back into single 2-bit field */
    intrq |= busrq << 1;

    /* check to see if we're INTAK or BUSAK */
    if (now == cp->req_bus.intak)       intrq = 4;
    if (now >  cp->req_bus.busak &&
        now == cp->req_bus.busrq_until) intrq = 5;
#if 0
    if (intrq > 3)
    {
        jzp_printf("prev:  n=%-8llu ni=%-8llu nb=%-8llu\n"
             "                  iu=%-8llu bu=%-8llu\n"
             "                   i=%-8llu  b=%-8llu\n", 
               old_now, 
               old_req.next_intrq,
               old_req.next_busrq,
               old_req.intrq_until,
               old_req.busrq_until,
               old_req.intak,
               old_req.busak);
    }
    old_req = cp->req_bus;
    old_now = now;
#endif


/*jzp_printf("%15llu %15llu %15llu %d\n", now, cp->req_bus.intak, cp->req_bus.busak, intrq);*/
    /* -------------------------------------------------------------------- */
    /*  If we're keeping a trace history, update it now.                    */
    /* -------------------------------------------------------------------- */
    if (debug_rh_ptr >= 0 && (now != prev_rh_now || intrq != prev_rh_intrq))
    {
        int i;

        prev_rh_now   = now;
        prev_rh_intrq = intrq;

        if (slen > 0 && prev_pc > 0 && cp->intr == 0) 
        { 
            non_int += len; 
        } else 
        { 
            if (*debug->vid_enable && non_int >= 50)
            {  
                char pcbuf[36];
                jzp_printf("NON_INT = %d at PC = %s\n", non_int, 
                        debug_symb_for_addr(debug, pc, pcbuf));
            }
            
            non_int = 0;
        }

        memcpy(debug_reghist + debug_rh_ptr * RH_RECSIZE, cp->r, 16);


        debug_reghist[debug_rh_ptr * RH_RECSIZE + 8] = 1 +
                                              ((!!cp->S    ) << 1) + 
                                              ((!!cp->C    ) << 2) +
                                              ((!!cp->O    ) << 3) +
                                              ((!!cp->Z    ) << 4) +
                                              ((!!cp->I    ) << 5) +
                                              ((!!cp->D    ) << 6) +
                                              ((!!cp->intr ) << 7) +
                                              ((intrq      ) << 8);

        for (i = 0; i < 3; i++)
            debug_reghist[debug_rh_ptr * RH_RECSIZE + 9 + i] = 
                periph_peek((periph_t*)p->bus, p, pc + i, ~0);

        debug_reghist[debug_rh_ptr * RH_RECSIZE + 12] = (now    ) & 0xFFFF;
        debug_reghist[debug_rh_ptr * RH_RECSIZE + 13] = (now>>16) & 0xFFFF;
        debug_reghist[debug_rh_ptr * RH_RECSIZE + 14] = (now>>32) & 0xFFFF;
        debug_reghist[debug_rh_ptr * RH_RECSIZE + 15] = (now>>48) & 0xFFFF;

        debug_rh_ptr = (debug_rh_ptr + 1) & HISTMASK;

        if (slen > 0 && prev_pc > 0)
        {
            debug_profile[prev_pc] += len * 2;
            debug_profile[pc]      |= !!cp->D;
        }
    }
    prev_pc = pc;

    /* -------------------------------------------------------------------- */
    /*  If slen == -CYC_MAX, we're crashing.                                */
    /* -------------------------------------------------------------------- */
    if (slen == -CYC_MAX || debug_fault_detected)
    {   
        if (debug_fault_detected > 0)
        {
            if (debug_rh_ptr >= 0)  debug_write_reghist("dump.hst", p, cp);
            if (debug_memattr)      debug_write_memattr("dump.atr");
            jzp_printf("CPU crashed!\n"); 
        } else
        {
            jzp_printf("Asynchronous halt requested.\n");
        }
        if (debug_halt_reason)
        {
            jzp_printf("Reason: '%s'\n", debug_halt_reason);
        }
        jzp_flush();
        debug_fault_detected = 0;
        debug_halt_reason    = NULL;
        slen = -CYC_MAX;
    }

    /* -------------------------------------------------------------------- */
    /*  Short-circuit the debugger if we're not doing anything interesting  */
    /* -------------------------------------------------------------------- */
    if (debug->step_count < 0 && slen > 0 && !debug->show_ins)
        return len;

    /* -------------------------------------------------------------------- */
    /*  If we're "fast-forwarding", only stop if our PC == the ffwd addr    */
    /*  or someone stops us with the "break" key.  This ignores all other   */
    /*  breakpoints, tracepoints, etc.  We barrel through everything.  We   */
    /*  cancel the fast forward on "break" key, or we reach our target.     */
    /* -------------------------------------------------------------------- */
    if (fast_fwd && debug->step_count != 0 && pc != fast_fwd)
    {
        /* ---------------------------------------------------------------- */
        /*  Pop off "return from JSRs" that we pass while fast-forwarding.  */
        /*  We shouldn't keep them since we've popped the return stack.     */
        /* ---------------------------------------------------------------- */
        if (cp->hit_bkpt == 2)
            debug_get_jsr_addr(pc);

        return len;
    }


    /* -------------------------------------------------------------------- */
    /*  Decrement our CPU step count.                                       */
    /* -------------------------------------------------------------------- */
    if (debug->step_count > 0)
    {
        debug->step_count -= instrs;
        if (debug->step_count < 0)
            debug->step_count = 0;

    }

    /* -------------------------------------------------------------------- */
    /*  If this was a breakpoint or a "tracepoint" (ie. from stepping over  */
    /*  a JSR instruction), the cycle count will be negative.               */
    /* -------------------------------------------------------------------- */
    if (slen < 0)
    {
        uint_32 jsr_addr = 0;


        /* ---------------------------------------------------------------- */
        /*  If we hit a tracepoint (ie. the return from a JSR), then don't  */
        /*  drop into the debugger.  Rather, go back into "step" mode with  */
        /*  our remaining step count.                                       */
        /* ---------------------------------------------------------------- */
        if (cp->hit_bkpt == 2 && (jsr_addr = debug_get_jsr_addr(pc)) != 0)
        {
            jzp_printf("Returned from JSR at $%.4X.\n", jsr_addr);
            debug->step_count = debug->step_over >> 1;
            debug->step_over &= 1;
            debug->show_ins   = 1;
            debug->show_rd    = show_rdwr;
            debug->show_wr    = show_rdwr;

            if (debug->step_count > 0)
                debug->step_count--;
        }

        /* ---------------------------------------------------------------- */
        /*  If we hit a breakpoint or anything other than a tracepoint,     */
        /*  drop into the debugger.  In the case of a breakpoint, let the   */
        /*  user know that we hit a breakpoint.                             */
        /* ---------------------------------------------------------------- */
        if (cp->hit_bkpt == 1)
            jzp_printf(fast_fwd ? "Fast forwarded to $%.4X\n" :
                                  "Hit breakpoint at $%.4X\n", pc);
        if (cp->hit_bkpt != 2)
            debug->step_count = 0;
    }

    /* -------------------------------------------------------------------- */
    /*  Clear fast-fwd here so we can modify breakpoint message.            */
    /* -------------------------------------------------------------------- */
    if (fast_fwd)
    {
        if (!ff_bkpt)
            cp1600_clr_breakpt(cp, fast_fwd, CP1600_BKPT);
        fast_fwd = 0;
    }

    /* -------------------------------------------------------------------- */
    /*  If we're stepping over JSRs, go into "run" mode if the next         */
    /*  instruction is a JSR.   We'll return to step mode once we hit the   */
    /*  return statement from the JSR.                                      */
    /* -------------------------------------------------------------------- */
    if (debug->step_count > 0 && debug->step_over)
    {
        if (debug_find_jsrs(p, cp, pc))
        {
            debug->step_over  = 1 | (debug->step_count << 1);
            debug->step_count = debug->step_count ? -1 : 0;
            debug->show_ins   = 0;
            debug->show_rd    = 0;
            debug->show_wr    = 0;
            show_once         = 1;
        }
    }

    /* -------------------------------------------------------------------- */
    /*  If we're in sync with the CPU, then grab the current PC and words   */
    /*  at that location, disassemble and display.                          */
    /* -------------------------------------------------------------------- */
    if (debug->show_ins || !debug->step_count || show_once /*|| !len*/)
    {
        const char *dis, *symb;
        int disasm_width;

        show_once = 0;

show_disassem:
        disasm_width = get_disp_width() - (show_time ? 60 : 51);
        if (disasm_width < 20)
            disasm_width = -1;

        pc = cp->r[7];

        /* ---------------------------------------------------------------- */
        /*  Where possible, cache disassembled instructions for speed.      */
        /* ---------------------------------------------------------------- */
        dis = debug_disasm_src(p, cp, pc, NULL, cp->D, disasm_width);
        dis = dis ? dis + 22 : str_null;

        /* ---------------------------------------------------------------- */
        /*  Print the state of the machine, along w/ disassembly.           */
        /* ---------------------------------------------------------------- */
        if (debug_symtab && (symb = symtab_getsym(debug_symtab, pc, 0, 0)))
            jzp_printf("%s:\n", symb);

        jzp_printf(
            show_time ?
               " %.4X %.4X %.4X %.4X %.4X %.4X %.4X %.4X %c%c%c%c%c%c%c%c"
               "%-*.*s %8llu\n"
            :
               " %.4X %.4X %.4X %.4X %.4X %.4X %.4X %.4X %c%c%c%c%c%c%c%c"
               "%-*.*s\n"
            ,
               cp->r[0], cp->r[1], cp->r[2], cp->r[3], 
               cp->r[4], cp->r[5], cp->r[6], cp->r[7],
               cp->S ? 'S' : '-',
               cp->C ? 'C' : '-',
               cp->O ? 'O' : '-',
               cp->Z ? 'Z' : '-',
               cp->I ? 'I' : '-',
               cp->D ? 'D' : '-',
               cp->intr ? 'i' : '-',
               intrq == 1 ? 'q' : 
               intrq == 2 ? 'b' : 
               intrq == 3 ? '?' :
               intrq == 4 ? 'Q' : 
               intrq == 5 ? 'B' : '-', 
               disasm_width, disasm_width, dis,
               now);

        if (debug->speed) speed_resync(debug->speed);
    }

    /* -------------------------------------------------------------------- */
    /*  If we've exhausted our CPU step count, drop into command prompt.    */
    /* -------------------------------------------------------------------- */
    if (debug->step_count == 0 /*|| !len*/)
    {
        int cmd = 0, c, arg = -1, arg2 = -1;
        char argstr[100], argstr2[100];
        static int over = 0;

        wind = gfx_force_windowed(debug->gfx, 1);

next_cmd:
        do
        {
            jzp_printf("> ");
            jzp_flush();

            while (!fgets(buf, 1023, debug->filestk[debug->filestk_depth]))
            {
                if (debug->filestk_depth == 0)
                {
                    strcpy(buf, "q");
                    break;
                } else
                    fclose(debug->filestk[debug->filestk_depth--]);
            }

            if (debug->filestk_depth > 0)
                fputs(buf, stdout);

            /* ignore anything after a semicolon */
            if ((s = strchr(buf, ';')) != NULL)
                *s = 0;

            s = buf;

            cmd = -1;

            /* ignore leading whitespace */
            while (*s && isspace(*s)) s++;

            /* chop trailing newline, whitespace */
            {
                char *ss, *nws;
                for (ss = nws = s; *ss; ss++)
                    if (!isspace(*ss))
                        nws = ss;
                nws[1] = 0;
                if ((ss = strchr(s, '\n')) != NULL) *ss = 0;
                if ((ss = strchr(s, '\r')) != NULL) *ss = 0;
            }

            /* step over / trace over if empty line, but only if interactive */
            if (!*s)
            {
                if (debug->filestk_depth == 0)
                {
                    strcpy(buf, last_over ? "t1" : "s1");
                    s = buf;
                } else
                    goto next_cmd;
            }

            c = toupper(*s++);

            /* skip whitespace after command character but before first arg */
            while (isspace(*s))
                s++;

            /* decode the command */
            if (c == 'S') cmd = 1, over = 0;  /* step into */
            if (c == 'T') cmd = 1, over = 1;  /* trace over */
            if (c == 'R') cmd = 2, over = 0;  /* run  */
            if (c == 'F') cmd = 23;           /* fast forward to */
            if (c == 'D') cmd = 3;  /* dump/abort */
            if (c == 'Q') cmd = 4;  /* quit */
            if (c == 'B') cmd = 5;  /* breakpoint */
            if (c == 'M') cmd = 6;  /* mem dump */
            if (c == 'U') cmd = 7;  /* 'Un'assemble.            */
            if (c == 'C') cmd = 8;  /* Disassembly Cache stats. */
            if (c == 'G') cmd = 9;  /* Change re'G'ister value. */
            if (c == 'N') cmd = 10; /* uNset breakpoint */
            if (c == 'H') cmd = 11; /* toggle History */
            if (c == 'W') cmd = 12; /* toggle watch write */
            if (c == '@') cmd = 17; /* toggle watch read  */
            if (c == 'A') cmd = 13; /* enable memory Attribute discovery */
            if (c == 'P') cmd = 14; /* poke into memory. (no side effect)*/
            if (c == 'E') cmd = 15; /* write to memory. (side effects)   */
#ifdef STK_TRC
            if (c == 'K') cmd = 16; /* stack tracing.  */
#endif
            if (c == 'Z') cmd = 18; /* toggle showing timestamps */
            if (c == 'X') cmd = 19; /* toggle showing read/write during step */
            if (c == '?') cmd = 20; /* print usage information */
            if (c == 'L') cmd = 21; /* load a symbol table file. */
            if (c == '/') cmd = 22; /* search for symbol in symtab */
            if (c == '$') cmd = 24; /* toggle STIC video FIFO messages */
            if (c == '%') cmd = 25; /* toggle STIC dropped reads */
            if (c == '^') cmd = 26; /* toggle STIC droped writes */
            if (c == 'O') cmd = 27; /* change label/addr output format */
            if (c == '<') cmd = 28; /* read script from file */
            if (c == '!') cmd = 29; /* print last N instructions from hist */
            if (c == '>') cmd = 30; /* Set window width */
            if (c == 'V') cmd = 31; /* Print source for vicinity */
            if (c == '#') cmd = 32; /* Breakpoint on screen blank */

            if (cmd == -1)
            {
                jzp_printf("Did not understand command '%s'.  "
                            "Type '?' for help\n", buf);
                goto next_cmd;
            }

            if (cmd == 1)
            {
                if (sscanf(s, "%d", &arg) != 1) 
                    arg = 1;
            } else if (cmd == 2)
            {
                if (sscanf(s, "%d", &arg) != 1) 
                    arg = -1;
            } else if (cmd == 5 || cmd == 10 || cmd == 23) {
                if (sscanf(s, "%100s", argstr) != 1) 
                    arg = pc;
                else
                {
                    arg = debug_decode_val(argstr, 0xFFFF);
                    if (arg == -1)
                        goto next_cmd;
                }
#ifdef STK_TRC
            } else if (cmd == 16) {
                if (sscanf(s, "%100s", argstr) != 1) 
                    arg = debug_infer_stack(cp);
                else
                {
                    arg = debug_decode_val(argstr, 0xFFFF);
                    if (arg == -1)
                        goto next_cmd;
                }
#endif
            } else if (cmd == 6 || cmd == 7 || cmd == 31) {
                int args = sscanf(s, "%100s %100s", argstr, argstr2);

                if (args >= 1) arg  = debug_decode_val(argstr,  0xFFFF);
                if (args >= 2) arg2 = debug_decode_val(argstr2, 0xFFFF);

                if ((args >= 1 && arg  == -1) || 
                    (args >= 2 && arg2 == -1))
                    goto next_cmd;

                if (args < 1) arg  = cmd == 6 ? next_hex_dump : next_disassem;
                if (args < 2) arg2 = cmd == 6 ? 0x40          : 0x10;

                if (cmd == 7)
                    next_disassem = arg;

                if (cmd == 6)
                    next_hex_dump = arg + arg2;
            } else if (cmd == 9) {
                int args = sscanf(s, "%100s %100s", argstr, argstr2);

                if (args >= 1) arg  = debug_decode_val(argstr, 7);
                if (args >= 2) arg2 = debug_decode_val(argstr2, 0xFFFF);

                if ((args >= 1 && arg  == -1) || 
                    (args >= 2 && arg2 == -1))
                    goto next_cmd;

                if (args != 2 || arg < 0 || arg > 7) arg = -2;
            } else if (cmd == 12 || cmd == 17) {
                int args = sscanf(s, "%100s %100s", argstr, argstr2);

                if (args >= 1) arg  = debug_decode_val(argstr,  0xFFFF);
                if (args >= 2) arg2 = debug_decode_val(argstr2, 0xFFFF);

                if ((args >= 1 && arg  == -1) || 
                    (args >= 2 && arg2 == -1))
                    goto next_cmd;

                if (args == 1) arg2 = arg;
                if (arg2 < arg) { int tmp = arg2; arg2 = arg; arg = tmp; }
            } else if (cmd == 14 || cmd == 15) {
                int args = sscanf(s, "%100s %100s", argstr, argstr2);

                if (args >= 1) arg  = debug_decode_val(argstr,  0xFFFF);
                if (args >= 2) arg2 = debug_decode_val(argstr2, 0xFFFF);

                if ((args >= 1 && arg  == -1) || 
                    (args >= 2 && arg2 == -1))
                    arg = -2;

                if (args != 2 || arg < 0x0000 || arg > 0xFFFF) arg = -2;
            } else if (cmd == 27 || cmd == 30)
            {
                if (sscanf(s, "%d", &arg) != 1) 
                    arg = 0;
            } else if (cmd == 29)
            {
                int args = sscanf(s, "%d %d", &arg, &arg2);
                if (args != 2)              arg2 = 0;
                if (args != 1 && args != 2) arg = 40;
            } else 
                arg=0;
        } while (!cmd || arg < -1);

        switch (cmd)
        {   
            case 1: /* step/trace */
            case 2: /* run */
            {
                if (cmd == 1)
                    last_over = over;
                else
                    over = 0;

                if (over)
                {
                    debug->step_over = 1 | (arg << 1);
                    if (debug_find_jsrs(p, cp, pc))
                    {
                        cmd = 2;
                        arg = -1;
                    }
                } else
                    debug->step_over = 0;

                debug->step_count = arg;
                debug->show_ins   = cmd == 1;
                debug->show_rd = debug->show_wr = cmd == 1 ? show_rdwr : 0;
                if (debug_rh_ptr < 0)
                    cp->instr_tick_per = cmd == 1 ? 1 : arg > 0 ? arg : 0;
                    
                break;
            }
            case 23:    /* fast forward */
            {
                if (arg > 0 && arg <= 0xFFFF)
                {
                    fast_fwd           = arg;
                    debug->step_over   = 0;
                    debug->step_count  = -1;
                    debug->show_ins    = 0;
                    debug->show_rd     = 0;
                    debug->show_wr     = 0;
                    if (debug_rh_ptr < 0)
                        cp->instr_tick_per = 0;
                    ff_bkpt = cp1600_set_breakpt(cp, arg, CP1600_BKPT);
                }
                break;
            }
            case 3: 
                dump_state();
                if (debug_rh_ptr >= 0)
                    debug_write_reghist("dump.hst", p, cp);
                if (debug_memattr)
                    debug_write_memattr("dump.atr");
                goto next_cmd;
                break;
            case 4:
                exit(0);
                break;
            case 5:
            case 10:
            {
                int set = cmd == 5;
                if (set)
                    cp1600_set_breakpt(cp, arg, CP1600_BKPT);
                else
                    cp1600_clr_breakpt(cp, arg, CP1600_BKPT);
                jzp_printf("%s breakpoint at $%.4X\n", set ? "Set" : "Unset", arg);
                goto next_cmd;
                break;
            }
            case 6:
                debug_dispmem(p, arg, arg2);
                goto next_cmd;
                break;
            case 7:
                debug_disasm_mem(p, cp, &next_disassem, arg2);
                goto next_cmd;
                break;
            case 8:
                jzp_printf("dc hits: %6d  misses: %6d  nocache: %6d  "
                       "unhook: %6d vs %6d\n", dc_hits, dc_miss, dc_nocache,
                       dc_unhook_ok, dc_unhook_odd);
                goto next_cmd;
                break;
            case 9:
                cp->r[arg] = arg2;
                goto show_disassem;
                break;
            case 11:
                if (!debug_histinit)
                {
                    debug_reghist = CALLOC(uint_16, (HISTSIZE+1)* RH_RECSIZE);
                    debug_profile = CALLOC(uint_32, 0x10000);
                    if (!debug_reghist || !debug_profile)
                    {
                        CONDFREE(debug_reghist);
                        CONDFREE(debug_profile);
                        jzp_printf("Couldn't allocate register history\n");
                    } else
                    {
                        debug_histinit = 1;
                    }
                }
                if (debug_histinit)
                {
                    if (debug_rh_ptr < 0) debug_rh_ptr = 0;
                    else                  debug_rh_ptr = -1;
                    jzp_printf("Register History is %s\n", 
                            debug_rh_ptr ? "Off" : "On");
                    if (!debug_rh_ptr)
                    {
                        memset(debug_reghist, 0, 
                               RH_RECSIZE*sizeof(uint_16)*HISTSIZE);
                        memset(debug_profile, 0, 0x10000 * sizeof(uint_32));
                    }
                    cp->instr_tick_per = debug_rh_ptr ? 0 : 1;
                }
                jzp_flush();
                goto next_cmd;
                break;
            case 12:
                {
                    int i, watch;

                    for (i = arg; i <= arg2; i++)
                    {
                        WATCHTOG(i,w);
                    }

                    for (i = arg, watch=-1; i <= arg2; i++)
                    {
                        if (WATCHING(i,w) != watch)
                        {
                            if (watch != -1) 
                                jzp_printf(" through $%.4X\n", i - 1);
                            watch = WATCHING(i,w);
                            jzp_printf("%s watching writes to $%.4X", 
                                   watch ? "Now" : "No longer", i);
                        }
                    }
                    if (arg2 >= arg)
                    {
                        jzp_printf(arg == arg2?"\n":" through $%.4X\n", arg2);
                    }
                }
                goto next_cmd;
                break;
            case 17:
                {
                    int i, watch;

                    for (i = arg; i <= arg2; i++)
                    {
                        WATCHTOG(i,r);
                    }

                    for (i = arg, watch=-1; i <= arg2; i++)
                    {
                        if (WATCHING(i,r) != watch)
                        {
                            if (watch != -1) 
                                jzp_printf(" through $%.4X\n", i - 1);
                            watch = WATCHING(i,r);
                            jzp_printf("%s watching reads of $%.4X", 
                                   watch ? "Now" : "No longer", i);
                        }
                    }
                    if (arg2 >= arg)
                    {
                        jzp_printf(arg == arg2?"\n":" through $%.4X\n", arg2);
                    }
                }
                goto next_cmd;
                break;
            case 13:
                if (!debug_memattr)
                {
                    debug_memattr = CALLOC(uint_8 ,  0x10000);
                    debug_mempc   = CALLOC(uint_16,  0x10000);
                    if (!debug_memattr || !debug_mempc)
                    {
                        CONDFREE(debug_memattr);
                        CONDFREE(debug_mempc  );
                        jzp_printf("Couldn't allocate memory attribute map\n");
                    } else
                    {
                        jzp_printf("Memory attribute map enabled.\n");
                    }
                } else
                {
                    memset(debug_memattr, 0, 0x10000 * sizeof(uint_8));
                    memset(debug_mempc  , 0, 0x10000 * sizeof(uint_16));
                    jzp_printf("Reset memory attribute map.\n");
                }
                goto next_cmd;
            case 14: /* poke */
            {
                if (arg >= 0)
                {
                    periph_poke((periph_p)cp->periph.bus, 
                                (periph_p)cp, arg, arg2);
                    cp1600_invalidate(cp, arg, arg);
                }
                else
                    jzp_printf("invalid args.  P addr data\n");

                goto next_cmd;
            }
            case 15: /* enter */
            {
                if (arg >= 0)
                {
                    periph_write((periph_p)cp->periph.bus, 
                                 (periph_p)cp, arg, arg2);
                    cp1600_invalidate(cp, arg, arg);
                }
                else
                    jzp_printf("invalid args.  E addr data\n");

                goto next_cmd;
            }
#ifdef STK_TRC
            case 16:
            {
                if (!stk_trc)
                {
                    jzp_printf("Opening stack.trc\n");
                    jzp_printf("Stack base set to $%.4X\n", arg);
                    stk_base = stk_wr = arg;
                    stk_trc = fopen("stack.trc", "a");
                    if (stk_trc)
                    {
                        fprintf(stk_trc, "--- start of log\n");
                    }
                } else
                {
                    jzp_printf("Closing stack.trc\n");
                    fprintf(stk_trc, "--- end of log\n");
                    fclose(stk_trc);
                    stk_trc = NULL;
                }
                goto next_cmd;
            }
#endif
            case 18:
            {
                show_time = !show_time;
                jzp_printf(show_time 
                        ? "Now showing timestamps during step\n"
                        : "No longer showing timestamps during step\n");
                goto next_cmd;
            }
            case 19:
            {
                show_rdwr = !show_rdwr;
                jzp_printf(show_rdwr 
                        ? "Now showing CPU reads/writes during step\n"
                        : "No longer showing CPU reads/writes during step\n");
                goto next_cmd;
            }
            case 20: 
            {
                debug_print_usage(); goto next_cmd;
            }
            case 21:
            {
                debug_read_symtbl(s);
                goto next_cmd;
            }
            case 22:
            {
                debug_search_symbol(s);
                goto next_cmd;
            }

            case 24: 
            {
                *debug->stic_dbg_flags ^= STIC_SHOW_FIFO_LOAD; 
                jzp_printf(*debug->stic_dbg_flags & STIC_SHOW_FIFO_LOAD
                        ? "Now showing STIC video FIFO loads\n"
                        : "No longer showing STIC video FIFO loads\n");
                goto next_cmd;
            }
            case 25: 
            {
                *debug->stic_dbg_flags ^= STIC_SHOW_RD_DROP;
                jzp_printf(*debug->stic_dbg_flags & STIC_SHOW_RD_DROP
                        ? "Now showing dropped STIC CTRL/GMEM reads\n"
                        : "No longer showing dropped STIC CTRL/GMEM reads\n");
                goto next_cmd;
            }
            case 26: 
            {
                *debug->stic_dbg_flags ^= STIC_SHOW_WR_DROP;
                jzp_printf(*debug->stic_dbg_flags & STIC_SHOW_WR_DROP
                        ? "Now showing dropped STIC CTRL/GMEM writes\n"
                        : "No longer showing dropped STIC CTRL/GMEM writes\n");
                goto next_cmd;
            }
            case 32: 
            {
                *debug->stic_dbg_flags ^= STIC_HALT_ON_BLANK;
                jzp_printf(*debug->stic_dbg_flags & STIC_HALT_ON_BLANK
                        ? "Now halting when display blanks\n"
                        : "No longer halting when display blanks\n");
                goto next_cmd;
            }
            case 27:
            {
                if (set_symb_addr_format(arg))
                {
                    jzp_printf("debug: Changed address format to \"%s\"\n", 
                                arg == 0 ? "LABEL ($1234)"
                              : arg == 1 ? "LABEL"
                              : arg == 2 ? "$1234 (LABEL)"
                              :            "$1234");

                    debug->symb_addr_format = arg;
                    debug_disasm_cache_inval();
                }
                goto next_cmd;
            }
            case 28:
            {
                debug_open_script(debug, s);
                goto next_cmd;
            }
            case 29:
            {
                if (debug_reghist)
                    debug_print_reghist(arg, arg2);
                goto next_cmd;
            }
            case 30:
            {
                if (arg >= 70)
                    set_disp_width(arg);
                goto next_cmd;
            }
            case 31:
            {
                debug_show_vicinity(arg, arg2);
                goto next_cmd;
            }
        }

        if (debug->speed) speed_resync(debug->speed);
        if (wind)         gfx_toggle_windowed(debug->gfx, 1);
    }

    return len;
}



/*
 * ============================================================================
 *  Instruction Disassembly Cache
 *
 *  This holds a cache of disassembled instructions.  The cache is maintained
 *  as an LRU list of entries.  We place an upper bound on the number of 
 *  entries just so we don't totally thrash through memory.
 * ============================================================================
 */
#define DISASM_CACHE (64)      /* Cache size. */

typedef struct disasm_cache_t
{
    char                    disasm[64];
    uint_32                 len;
    struct disasm_cache_t   *next,*prev;
    char                    **hook;
} disasm_cache_t;

disasm_cache_t *disasm_cache = NULL;

/*
 * ============================================================================
 *  DEBUG_DISASM_CACHE_INVAL -- invalidate the entire disassembly cache
 * ============================================================================
 */
LOCAL void debug_disasm_cache_inval(void)
{
    disasm_cache_t *dc = disasm_cache;

    /* -------------------------------------------------------------------- */
    /*  Go through the entire disassembly cache and unhook all cached       */
    /*  disassemblies.                                                      */
    /* -------------------------------------------------------------------- */
    do
    {
        if (dc->hook && *dc->hook == (char*)dc) /* Unhook it if we're hooked */
            *dc->hook = NULL;

        dc = dc->next;
    } while (dc && dc != disasm_cache);
}

/*
 * ============================================================================
 *  DEBUG_DISASM      -- Disassembles one instruction, returning a pointer
 *                       to the disassembled text.  Uses the disassembly
 *                       cache if possible.
 * ============================================================================
 */
char * debug_disasm(periph_t *p, cp1600_t *cp, uint_16 addr, 
                    uint_32 *len, int dbd)
{
    static char buf[1024];
    uint_16 w1, w2, w3, pc = addr;
    disasm_cache_t *disasm = NULL;
    int instr_len;

    /* -------------------------------------------------------------------- */
    /*  If memory attribute tracking is enabled and this looks like data,   */
    /*  just return a DECLE directive.  We never cache these.               */
    /* -------------------------------------------------------------------- */
    if (debug_memattr && 
        (debug_memattr[addr] & (DEBUG_MA_CODE|DEBUG_MA_DATA)) == DEBUG_MA_DATA)
    {
        char c, d;
        w1 = periph_read((periph_t*)p->bus, p, pc    , ~0);
    
        c = w1 >> 8;
        d = w1;
        
        if ( !isalnum(c) ) c = '.';
        if ( !isalnum(d) ) d = '.';

        sprintf(buf, " %.4X                   DECLE $%.4X ; '%c%c'", 
                     w1, w1, c, d);

        if (len)
            *len = 1;

        return buf;
    }

#if 1
    /* -------------------------------------------------------------------- */
    /*  If we can't cache this disassembly, or if it's not disassembled     */
    /*  yet, go do the disassembly.  Also force disassembly if DBD is set   */
    /*  since we might have seen this instr before w/out DBD set.           */
    /* -------------------------------------------------------------------- */
    if (!cp->disasm[pc] || dbd)
    {
        w1 = periph_read((periph_t*)p->bus, p, pc    , ~0);
        w2 = periph_read((periph_t*)p->bus, p, pc + 1, ~0);
        w3 = periph_read((periph_t*)p->bus, p, pc + 2, ~0);

        instr_len = dasm1600(buf, pc, dbd, w1, w2, w3, debug_symtab);

        dc_miss++;
        dc_hits--;            /* compensate for dc_hits++ in LRU update */

        /* ------------------------------------------------------------ */
        /*  If DBD is set, or if this instruction hasn't been decoded   */
        /*  yet, don't cache this instruction.                          */
        /* ------------------------------------------------------------ */
        if (dbd || !cp->instr[pc])
        {
            if (len) *len = instr_len;
            return buf + 17;
        }

        /* ------------------------------------------------------------ */
        /*  Allocate a new disassembly record by evicting the LRU.      */
        /* ------------------------------------------------------------ */
        disasm = disasm_cache->prev;        /* Tail of LRU list.        */

        if (disasm->hook &&                 /* Is this already hooked?  */
            *disasm->hook == (char*)disasm) /* Unhook it.               */
        {
            dc_unhook_ok++;
            *disasm->hook = NULL;
        } else
            dc_unhook_odd++;

        /* ------------------------------------------------------------ */
        /*  Hook this disassembly to the instruction record.            */
        /* ------------------------------------------------------------ */
        disasm->hook   = &cp->disasm[pc];
        cp->disasm[pc] = (char*) disasm;

        strncpy(disasm->disasm, buf + 17, sizeof(disasm->disasm));
        disasm->disasm[sizeof(disasm->disasm) - 1] = 0;
        disasm->len = instr_len;
    
    }

    /* -------------------------------------------------------------------- */
    /*  Grab the cached disassembly and update the LRU.                     */
    /* -------------------------------------------------------------------- */
    dc_hits++;
    disasm = (disasm_cache_t *)cp->disasm[pc];

    /* -------------------------------------------------------------------- */
    /*  Move this disasm record to head of the LRU.  Do this by first       */
    /*  unhooking it from the doubly-linked list, and then reinserting      */
    /*  it at the head of the doubly-linked list.                           */
    /* -------------------------------------------------------------------- */
    disasm->next->prev = disasm->prev;      /* Unhook 'next'.           */
    disasm->prev->next = disasm->next;      /* Unhook 'prev'.           */

    disasm->next = disasm_cache->next;      /* Hook us to new 'next'.   */
    disasm->prev = disasm_cache;            /* Hook us to new 'prev'.   */

    disasm->next->prev = disasm;            /* Hook new 'next' to us.   */
    disasm_cache->next = disasm;            /* Hook new 'prev' to us.   */


    /* -------------------------------------------------------------------- */
    /*  Returned cached disassembly.                                        */
    /* -------------------------------------------------------------------- */
    if (len) *len = disasm ? disasm->len : 0;
    return disasm ? disasm->disasm : NULL;
#else
    w1 = periph_read((periph_t*)p->bus, p, pc    , ~0);
    w2 = periph_read((periph_t*)p->bus, p, pc + 1, ~0);
    w3 = periph_read((periph_t*)p->bus, p, pc + 2, ~0);

    instr_len = dasm1600(buf, pc, cp->D, w1, w2, w3, debug_symtab);

    if (len) *len = instr_len;

    return buf + 17;
#endif
}

/*
 * ============================================================================
 *  DEBUG_DISASM_SRC  -- Attempt to append source to the disassembly
 * ============================================================================
 */
const char * debug_disasm_src(periph_t *p, cp1600_t *cp, uint_16 addr, 
                              uint_32 *len, int dbd, int disasm_width)
{
    static char buf[1024];
    const char *dis, *src;

    if (disasm_mode <  0)   // disasm only
    {
        return debug_disasm(p, cp, addr, len, dbd);
    }

    if (disasm_mode == 0)   // mixed disasm / source (default)
    {
        dis = debug_disasm(p, cp, addr, len, dbd);

        if (disasm_width < 40)
            return dis;

        src = source_for_addr(addr);

        if (src)
        {
            snprintf(buf, 1007, "%-61.61s | %s", dis, src);
            return buf;
        }

        return dis;
    }

    // source-only when source available
    src = source_for_addr(addr);

    if (!src)
        return debug_disasm(p, cp, addr, len, dbd);

    return src;
}

/*
 * ============================================================================
 *  DEBUG_DISASM_MEM  -- Disassembles a range of memory locations.
 * ============================================================================
 */
void debug_disasm_mem(periph_t *p, cp1600_t *cp, uint_16 *paddr, uint_32 cnt)
{
    const char *dis;
    const char *symb;
    uint_32 tot = 0, len = ~0, w0 = 0;
    uint_16 addr = *paddr;

    while (tot <= cnt && len > 0)
    {
        int disp_width   = get_disp_width();
        int disasm_width = disp_width - 42;
        if (len == 1)
            w0 = periph_read((periph_t*)p->bus, p, addr-1, ~0);

        dis = debug_disasm_src(p, cp, addr, &len, len == 1 && w0 == 0x0001,
                               disasm_width);

        if (debug_symtab && (symb = symtab_getsym(debug_symtab, addr, 0, 0)))
            jzp_printf("                   %s:\n", symb);
        jzp_printf(" $%.4X:   %-*.*s\n", addr, 
                   disp_width - 12, disp_width - 12, dis);

        addr += len;
        tot++;
    }

    *paddr = addr;
}

/*
 * ============================================================================
 *  DEBUG_DISPMEM     -- Displays ten lines of "hex dump" memory information.
 *                       The first arg is the address to start dumping at.
 *                       The second arg is the number of addresses to dump.
 * ============================================================================
 */
void debug_dispmem(periph_t *p, uint_16 addr, uint_16 len)
{
    int         i, j, k, l;
    uint_32     w[8];
    
    /* -------------------------------------------------------------------- */
    /*  Round our address down to the next lower multiple of 8.  Add this   */
    /*  difference to the length, and then round the length up to the       */
    /*  next multiple of 8.                                                 */
    /* -------------------------------------------------------------------- */
    k = addr & ~7;

    len = (len + 7 + addr - k) & ~7;
    
    /* -------------------------------------------------------------------- */
    /*  Iterate until we've dumped the entire block of memory.              */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < len; i += 8)
    {
        l = k;
        
        /* ---------------------------------------------------------------- */
        /*  Read one row of 8 memory locations.                             */
        /* ---------------------------------------------------------------- */
        for (j = 0; j < 8; j++)
        {
            w[j] = periph_peek((periph_t*)p->bus, p, k++, ~0);
        }
        
        /* ---------------------------------------------------------------- */
        /*  Display them.                                                   */
        /* ---------------------------------------------------------------- */
        jzp_printf( "%.4X:  %.4X%c %.4X%c %.4X%c %.4X%c "
                    " %.4X%c %.4X%c %.4X%c %.4X%c   # ",
                    l, w[0], l + 0 == addr ? '*' : ' ',
                       w[1], l + 1 == addr ? '*' : ' ',
                       w[2], l + 2 == addr ? '*' : ' ',
                       w[3], l + 3 == addr ? '*' : ' ',
                       w[4], l + 4 == addr ? '*' : ' ',
                       w[5], l + 5 == addr ? '*' : ' ',
                       w[6], l + 6 == addr ? '*' : ' ',
                       w[7], l + 7 == addr ? '*' : ' ');
        
        /* ---------------------------------------------------------------- */
        /*  Display ASCII equivalents.                                      */
        /* ---------------------------------------------------------------- */
        for (j = 0; j < 8; j++)
        {
            char c, d;
        
            c = w[j] >> 8;
            d = w[j];
            
            if ( !isalnum(c) )
                c = '.';
            if ( !isalnum(d) )
                d = '.';
                
            jzp_printf( "%c%c", c, d );
        }
        
        jzp_printf( "\n" );
    }
}

/*
 * ============================================================================
 *  DEBUG_RENDER_REGHIST  -- stringify a record in the register history
 * ============================================================================
 */
LOCAL const char *debug_render_reghist(int ofs, int indent, int disp_width)
{
    int idx, S, C, O, Z, I, D, intr, irq, nirq, disasm_width;
    uint_16 pc, flags, r0, r1, r2, r3, r4, r5, r6, w1, w2, w3;
    char *dis, *rslt, *out;
    const char *symb, *src = NULL;
    static char buf[1024];
    uint_64 now, n0, n1, n2, n3;

    disasm_width = disp_width - 60;
    if (disasm_width > 500) disasm_width = 500;

    idx   = ((ofs + debug_rh_ptr) & HISTMASK) * RH_RECSIZE;
    n3    = debug_reghist[idx + 15];
    n2    = debug_reghist[idx + 14];
    n1    = debug_reghist[idx + 13];
    n0    = debug_reghist[idx + 12];
    now   = (n3 << 48) | (n2 << 32) | (n1 << 16) | n0;
    w3    = debug_reghist[idx + 11];
    w2    = debug_reghist[idx + 10];
    w1    = debug_reghist[idx +  9];
    flags = debug_reghist[idx +  8];
    pc    = debug_reghist[idx +  7];
    r6    = debug_reghist[idx +  6];
    r5    = debug_reghist[idx +  5];
    r4    = debug_reghist[idx +  4];
    r3    = debug_reghist[idx +  3];
    r2    = debug_reghist[idx +  2];
    r1    = debug_reghist[idx +  1];
    r0    = debug_reghist[idx +  0];

    if ((flags & 1) == 0) 
    {
        buf[0] = 0;
        return buf;
    }

    S    = (flags >> 1) & 1;
    C    = (flags >> 2) & 1;
    O    = (flags >> 3) & 1;
    Z    = (flags >> 4) & 1;
    I    = (flags >> 5) & 1;
    D    = (flags >> 6) & 1;
    intr = (flags >> 7) & 1;
    irq  = (flags >> 8) & 7;

    nirq = (debug_reghist[idx + 8 + RH_RECSIZE] >> 8) & 7;

    /* -------------------------------------------------------------------- */
    /*  Directly decode exactly what is in memory now, rather than rely     */
    /*  on the disassembly cache.                                           */
    /* -------------------------------------------------------------------- */
    dasm1600(buf, pc, D, w1, w2, w3, debug_symtab);
    dis = buf + 39;

    if (nirq == 4 || nirq == 5) /* did this one get stomped? */
    {
        dis = buf + 37;
        dis[0] = '>';
        dis[1] = '>';
    }

    /* -------------------------------------------------------------------- */
    /*  See if we can get a source line for this address.                   */
    /* -------------------------------------------------------------------- */
    if (disasm_width > 40)
        src = source_for_addr(pc);

    if (src)
    {
        dis[40] = 0;
        strcat(dis, "                                        ");
        dis[39] = ' ';
        dis[40] = '|';
        dis[41] = ' ';
        strncpy(dis + 42, src, disasm_width - 42);
    }

    /* -------------------------------------------------------------------- */
    /*  Move the interesting portion of the disassembly to the beginning    */
    /*  of the buffer.                                                      */
    /* -------------------------------------------------------------------- */
    memmove(buf, dis, disasm_width);
    dis = buf;
    dis[disasm_width] = 0;

    /* -------------------------------------------------------------------- */
    /*  Print the state of the machine, along w/ disassembly.               */
    /* -------------------------------------------------------------------- */
    rslt = out = buf + disasm_width + 1;

    if (debug_symtab && (symb = symtab_getsym(debug_symtab, pc, 0, 0)))
    {
        int len = strlen(symb);
        if (len > disp_width - 2)
            len = disp_width - 2;

        strncpy(out, symb, len + 1);
        out[len    ] = ':';
        out[len + 1] = '\n';

        out += len + 2;
    }

    sprintf(out, "%*s%.4X %.4X %.4X %.4X %.4X %.4X %.4X %.4X %c%c%c%c%c%c%c%c"
           "%-*.*s %8llu\n", 
           indent, "",
           r0, r1, r2, r3, r4, r5, r6, pc,
           S ? 'S' : '-', C ? 'C' : '-', O ? 'O' : '-', Z ? 'Z' : '-',
           I ? 'I' : '-', D ? 'D' : '-', 
           intr ? 'i' : '-', 
           irq == 1 ? 'q' : 
           irq == 2 ? 'b' : 
           irq == 3 ? '?' :
           irq == 4 ? 'Q' :
           irq == 5 ? 'B' : '-', 
           disasm_width, disasm_width, dis, now);

    return rslt;
}

/*
 * ============================================================================
 *  DEBUG_PRINT_REGHIST -- Print recent register history
 * ============================================================================
 */
LOCAL void debug_print_reghist(int count, int offset)
{
    int i, w = get_disp_width();

    for (i = -offset - count; i < -offset; i++)
    {
        const char *hist =  debug_render_reghist(i, 1, w);
        if (hist[0])
            jzp_printf("%s", hist);
    }
}

/*
 * ============================================================================
 *  DEBUG_WRITE_REGHIST  -- Write CPU register history trace to file.
 * ============================================================================
 */
LOCAL void debug_write_reghist(const char *fname, periph_p p, cp1600_t *cp)
{
    FILE *f;
    int i;
    double total = 0.0;
    char *dis;
    uint_16 pc;

    f = fopen(fname, "w");
    if (!f)
    {
        fprintf(stderr, "Could not open register history file '%s'\n", fname);
        return;
    }

    jzp_printf("Dumping register history to '%s'...  ", fname);
    jzp_flush();

    for (i = 0; i < HISTSIZE; i++)
    {
        fputs(debug_render_reghist(i, 0, 80), f);
    }
    jzp_printf("Done.\n");

    jzp_printf("Dumping profile data to '%s'...  ", fname);
    jzp_flush();

    fprintf(f,"\n\nProfile\n");

    /* -------------------------------------------------------------------- */
    /*  First total up the cycles profiled.                                 */
    /* -------------------------------------------------------------------- */
    for (i = total = 0; i <= 0xFFFF; i++)
        total += (debug_profile[i] >> 1);

    /* -------------------------------------------------------------------- */
    /*  Then output the blow-by-blow.                                       */
    /* -------------------------------------------------------------------- */
    for (i = pc = 0; i <= 0xFFFF; i++, pc++)
    {
        double cycles;

        /* ---------------------------------------------------------------- */
        /*  Skip addresses that were never executed.                        */
        /* ---------------------------------------------------------------- */
        if (!debug_profile[pc]) continue;

        /* ---------------------------------------------------------------- */
        /*  Disassemble those that were.                                    */
        /* ---------------------------------------------------------------- */
        dis = debug_disasm(p, cp, pc, NULL, debug_profile[pc] & 1);
        dis = dis ? dis : str_null;

        /* ---------------------------------------------------------------- */
        /*  Write the disassembly to the file.                              */
        /* ---------------------------------------------------------------- */
        cycles = debug_profile[pc] >> 1;
        fprintf(f, "%10d (%7.3f%%) | $%.4X:%s\n", 
                (uint_32)cycles, 100.0 * cycles / total, pc, dis);
    }
    jzp_printf("Done.\n");

    jzp_flush();
    fclose(f);
}


/*
 * ============================================================================
 *  DEBUG_WRITE_MEMATTR  -- Write memory attribute map for disassembler
 * ============================================================================
 */
LOCAL void debug_write_memattr(const char *fname)
{
    FILE *f;
    int i, span_s, span_e;
    const char *dir = NULL;

    f = fopen(fname, "w");
    if (!f)
    {
        fprintf(stderr, "Could not open memory attribute file '%s'\n", fname);
        return;
    }

    jzp_printf("Dumping memory attribute map to '%s'...  ", fname);
    jzp_flush();

    for (i = 0; i < 0x10000; i = span_e + 1)
    {
        span_s = i;
        for (span_e = span_s; span_e < 0x10000 && 
             debug_memattr[span_e] == debug_memattr[span_s]/* &&
             debug_mempc  [span_e] == debug_mempc  [span_s]*/;
             span_e++)
             ;
        span_e--;

        if (debug_memattr[span_s] == 0)
            continue;

        dir = NULL;
        if        (debug_memattr[span_s] & DEBUG_MA_CODE)
        {
            dir = "code";
        } else if (debug_memattr[span_s] & DEBUG_MA_SDBD)
        {
            dir = "dbdata";
        } else if (debug_memattr[span_s] & DEBUG_MA_DATA)
        {
            dir = "data";
        }

        if (dir)
        {
            /*
            fprintf(f, "%-6s %.4X %.4X ; %.4X\n", dir, span_s, span_e, 
                    debug_mempc[span_s]);
            */
            fprintf(f, "%-6s %.4X %.4X\n", dir, span_s, span_e);
        }
    }

    jzp_printf("Done.\n");
    jzp_flush();

    fclose(f);
}


/*
 * ============================================================================
 *  DEBUG_DTOR       -- Tears down all the debugger state. 
 * ============================================================================
 */
LOCAL void debug_dtor(periph_p p)
{
    debug_t *debug = (debug_t*)p;

    UNUSED(debug);

    dc_hits = dc_miss = dc_nocache = dc_unhook_ok = dc_unhook_odd = 0;

    CONDFREE(disasm_cache );
    CONDFREE(debug_memattr);
    CONDFREE(debug_mempc  );
    CONDFREE(debug_reghist);
    CONDFREE(debug_profile);

    if (debug_symtab)
        symtab_destroy(debug_symtab);

    debug_symtab         = NULL;
    debug_histinit       = 0;
    debug_rh_ptr         = -1;
    debug_fault_detected = 0;
    debug_halt_reason    = NULL;
    debug_num_jsrs       = 0;

#ifdef STK_TRC
    if (stk_trc)
        fclose(stk_trc);

    stk_trc  = NULL;
    stk_deep = 0;
    stk_base = 0;
    stk_wr   = 0;
#endif
}
    

/*
 * ============================================================================
 *  DEBUG_INIT       -- Initializes a debugger object and registers a CPU
 *                      pointer.
 * ============================================================================
 */
int debug_init(debug_t *debug, cp1600_t *cp1600, speed_t *speed, gfx_t *gfx,
               const char *symtbl, uint_8 *vid_enable, uint_32 *stic_dbg_flags,
               const char *script)
{
    static uint_8   dummy  = 0;
    static uint_32  dummy2 = 0;

    /* -------------------------------------------------------------------- */
    /*  Set up the debugger's state.                                        */
    /* -------------------------------------------------------------------- */
    debug->periph.read     = debug_rd;
    debug->periph.write    = debug_wr;
    debug->periph.peek     = debug_peek;
    debug->periph.poke     = debug_poke;
    debug->periph.tick     = NULL;
    debug->periph.min_tick = ~0U;
    debug->periph.max_tick = ~0U;
    debug->periph.dtor     = debug_dtor;

    debug->periph.addr_base = 0;
    debug->periph.addr_mask = ~0U;

    debug->show_rd = 1;
    debug->show_wr = 1;
    debug->cp1600  = cp1600;
    debug->speed   = speed;
    debug->gfx     = gfx;

    debug->vid_enable     = vid_enable     ? vid_enable     : &dummy;
    debug->stic_dbg_flags = stic_dbg_flags ? stic_dbg_flags : &dummy2;

    debug->filestk[0] = stdin;

    /* -------------------------------------------------------------------- */
    /*  Register the debugger's tick function with the CPU.  We're ticked   */
    /*  by the CPU, not by the peripheral bus.                              */
    /* -------------------------------------------------------------------- */
    cp1600_instr_tick(cp1600, debug_tk, (periph_p)debug);

    /* -------------------------------------------------------------------- */
    /*  Set up the instruction disassembly cache.                           */
    /* -------------------------------------------------------------------- */
    if (!disasm_cache)
    {
        int i;

        disasm_cache = CALLOC(disasm_cache_t, DISASM_CACHE);
        
        for (i = 0; i < DISASM_CACHE-1; i++)
        {
            disasm_cache[i  ].next = &disasm_cache[i+1];
            disasm_cache[i+1].prev = &disasm_cache[i  ];
        }
        disasm_cache[0].prev = &disasm_cache[DISASM_CACHE-1];
        disasm_cache[DISASM_CACHE-1].next = &disasm_cache[0];
    }

    /* -------------------------------------------------------------------- */
    /*  Clear watch array                                                   */
    /* -------------------------------------------------------------------- */
    memset(debug_watch_r, 0, sizeof(debug_watch_r));
    memset(debug_watch_w, 0, sizeof(debug_watch_w));

    /* -------------------------------------------------------------------- */
    /*  Read symbol table if requested to do so.                            */
    /* -------------------------------------------------------------------- */
    if (symtbl)
        debug_read_symtbl(symtbl);

    /* -------------------------------------------------------------------- */
    /*  Set up the debugger script if requested.                            */
    /* -------------------------------------------------------------------- */
    if (script)
        debug_open_script(debug, script);

    return 0;
}

/* ======================================================================== */
/*  This program is free software; you can redistribute it and/or modify    */
/*  it under the terms of the GNU General Public License as published by    */
/*  the Free Software Foundation; either version 2 of the License, or       */
/*  (at your option) any later version.                                     */
/*                                                                          */
/*  This program is distributed in the hope that it will be useful,         */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of          */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       */
/*  General Public License for more details.                                */
/*                                                                          */
/*  You should have received a copy of the GNU General Public License       */
/*  along with this program; if not, write to the Free Software             */
/*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               */
/* ======================================================================== */
/*                 Copyright (c) 1998-1999, Joseph Zbiciak                  */
/* ======================================================================== */
