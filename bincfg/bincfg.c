/* ======================================================================== */
/*  BINCFG   -- Routines for reading a configuration file for the BIN+CFG   */
/*              file format.  Includes debug functions which can generate   */
/*              a new configuration file from the parsed config file.       */
/*                                                                          */
/*  Parser interface functions, intended to be called for reading a .CFG:   */
/*                                                                          */
/*  BC_PARSE_CFG  -- Reads a configuration file using the lexer/grammar.    */
/*  BC_READ_DATA  -- Reads ROM segments and attaches them to bc_cfgfile_t.  */
/*  BC_DO_MACROS  -- Applies macros that can be safely applied statically.  */
/*  BC_FREE_CFG   -- Frees all memory associated with a bc_cfgfile_t.       */
/*                                                                          */
/*  Structure printing functions for generating a .CFG from a parsed CFG.   */
/*  The following functions are compiled out if BC_NOPRINT is defined.      */
/*                                                                          */
/*  BC_PRINT_CFG  -- Chase through a bc_cfgfile_t structure and print out   */
/*                   what we find therein.  It calls the following helpers: */
/*                                                                          */
/*  BC_PRINT_DIAG    -- Print all the collected diagnostics attached to cfg */
/*  BC_PRINT_MACRO   -- Print the [macro] section                           */
/*  BC_PRINT_MACRO_T -- Print a single bc_macro_t                           */
/*  BC_PRINT_VARLIKE -- Print [var],[keys],[joystick] sections              */
/*  BC_PRINT_VAR_T   -- Print a single <name>,<value> tuple                 */
/*  BC_PRINT_MEMSPAN -- Print out all the memory span information.          */
/* ======================================================================== */

#include "config.h"
#include "bincfg/bincfg.h"
#include "bincfg/bincfg_lex.h"
#include "bincfg/bincfg_grmr.tab.h"
#include "file/file.h"

bc_cfgfile_t *bc_parsed_cfg = NULL;

extern int bc_parse(void);  /* grrr... bison doesn't do this for us?!       */

/* ======================================================================== */
/*  BC_PARSE_CFG  -- Invokes the lexer and grammar to parse the config.     */
/* ======================================================================== */
bc_cfgfile_t *bc_parse_cfg
(
    FILE *f, 
    const char *const binfile,
    const char *const cfgfile  
)
{
    bc_memspan_t *span, *prev;

    bc_parsed_cfg = NULL;

    /* -------------------------------------------------------------------- */
    /*  Scan in the file, ignoring errors returned directly from bc_parse.  */
    /*  All the diagnostics will be attached to whatever cfg we generate.   */
    /* -------------------------------------------------------------------- */
    if (f)
    {
        bc_restart(f);      /* register the file with the lexer.            */
        bc_parse();         /* run the grammar.  It calls the bc_lex().     */
    } 

    /* -------------------------------------------------------------------- */
    /*  If not parsing a configuration file, or the file was empty, just    */
    /*  make an empty config as a starting point.                           */
    /* -------------------------------------------------------------------- */
    if (!bc_parsed_cfg)
    {
        bc_parsed_cfg = CALLOC(bc_cfgfile_t, 1);
    }

    /* -------------------------------------------------------------------- */
    /*  Leave if we get here without a valid config.                        */
    /* -------------------------------------------------------------------- */
    if (!bc_parsed_cfg)
        return NULL;

    if (binfile) bc_parsed_cfg->binfile = strdup(binfile);
    if (cfgfile) bc_parsed_cfg->cfgfile = strdup(cfgfile);

    /* -------------------------------------------------------------------- */
    /*  Scan the memory spans looking for preload sections.  If none are    */
    /*  found, add the default configuration's memory spans.                */
    /* -------------------------------------------------------------------- */
    prev = NULL;
    span = bc_parsed_cfg->span;
    while (span && (span->flags & BC_SPAN_PL) == 0)
    {
        prev = span;
        span = (bc_memspan_t*)(span->l.next);
    }

    /* -------------------------------------------------------------------- */
    /*  If span's NULL, we didn't find a section with 'preload'.            */
    /*  This means that there were no [mapping] or [preload] sections.      */
    /* -------------------------------------------------------------------- */
    if (!span)
    {
        bc_memspan_t *s0, *s1, *s2;

        if (!(s0 = CALLOC(bc_memspan_t, 1)) ||
            !(s1 = CALLOC(bc_memspan_t, 1)) ||
            !(s2 = CALLOC(bc_memspan_t, 1)))
        {
            fprintf(stderr, "out of memory\n");
            exit(1);
        }

        s0->s_fofs = 0x0000;
        s0->e_fofs = 0x1FFF;
        s0->s_addr = 0x5000;
        s0->e_addr = 0x6FFF;
        s0->flags  = BC_SPAN_R | BC_SPAN_PL;
        s0->width  = 16;
        s0->epage  = BC_SPAN_NOPAGE;
        s0->f_name = NULL;
        s0->l.next = (ll_t *)s1;

        s1->s_fofs = 0x2000;
        s1->e_fofs = 0x2FFF;
        s1->s_addr = 0xD000;
        s1->e_addr = 0xDFFF;
        s1->flags  = BC_SPAN_R | BC_SPAN_PL;
        s1->width  = 16;
        s1->epage  = BC_SPAN_NOPAGE;
        s1->f_name = NULL;
        s1->l.next = (ll_t *)s2;

        s2->s_fofs = 0x3000;
        s2->e_fofs = 0x3FFF;
        s2->s_addr = 0xF000;
        s2->e_addr = 0xFFFF;
        s2->flags  = BC_SPAN_R | BC_SPAN_PL;
        s2->width  = 16;
        s2->epage  = BC_SPAN_NOPAGE;
        s2->f_name = NULL;
        s2->l.next = NULL;

        if (!prev)
            bc_parsed_cfg->span = s0;
        else
            prev->l.next = (ll_t*)s0;
    }

    return bc_parsed_cfg;
}

/* ======================================================================== */
/*  BC_READ_HELPER -- helper function for bc_read_data.                     */
/* ======================================================================== */
LOCAL int bc_read_helper(char *f_name, uint_16 *buf, int width)
{        
    FILE *f;
    int len;

    /* -------------------------------------------------------------------- */
    /*  Open and read up to 64K words from a file.  Return how much we      */
    /*  actually read from the file to the caller.                          */
    /* -------------------------------------------------------------------- */
    if ((f = fopen(f_name, "rb")) == NULL)
    {

        perror("fopen()");
        fprintf(stderr, "Could not open binary file '%s' for reading.\n",
                f_name);
        return -1;
    }

    len = width > 8 ? file_read_rom16(f, BC_MAXBIN, buf) 
                    : file_read_rom8 (f, BC_MAXBIN, buf);

    if (len < 0)
    {
        fprintf(stderr, "Unable to read binary file '%s'\n", f_name);
        return -1;
    }

    fclose(f);

    return len;
}

/* ======================================================================== */
/*  BC_READ_DATA  -- Reads ROM segments and attaches them to bc_cfgfile_t.  */
/*                                                                          */
/*  This pass will adjust, or remove and free memspans that are partially   */
/*  or completely outside the bounds of the associated binfile.             */
/* ======================================================================== */
int bc_read_data(bc_cfgfile_t *bc)
{
    bc_memspan_t *span, *prev;
    uint_16 *pbuf, *lbuf, *buf;
    size_t plen, llen, len;
    int slen, i;

    /* -------------------------------------------------------------------- */
    /*  Allocate a temporary buffer for the primary BIN file.  Note that    */
    /*  we can't just parcel our BIN segments out of this buffer with       */
    /*  pointer manipulation unless there is only 1 such segment.  This     */
    /*  is due to the 'free()' semantics for the ->data field in each       */
    /*  memspan.                                                            */
    /* -------------------------------------------------------------------- */
    if ((pbuf = CALLOC(uint_16, BC_MAXBIN*2)) == NULL)
    {
        fprintf(stderr, "out of memory\n");
        exit(1);
    }
    lbuf = pbuf + BC_MAXBIN;

    for (i = 0; i < BC_MAXBIN; i += 2)
    {
        pbuf[i + 0] = 0xDEAD;
        pbuf[i + 1] = 0xBEEF;
        lbuf[i + 0] = 0xDEAD;
        lbuf[i + 1] = 0xBEEF;
    }

    /* -------------------------------------------------------------------- */
    /*  Load the primary file up front.                                     */
    /* -------------------------------------------------------------------- */
    if (!bc->binfile || !file_exists(bc->binfile))
    {
        plen = 0;
    } else
    {
        int tmp;

        /* Assume primary binfile is width >= 9. */
        tmp = bc_read_helper(bc->binfile, pbuf, 16);

        if (tmp < 0) { free(pbuf); return -1; }

        plen = (size_t)tmp;
    }

    /* -------------------------------------------------------------------- */
    /*  Chase down the spans, attaching ->data to each span.                */
    /* -------------------------------------------------------------------- */
    for (prev = NULL, span = bc->span; span; 
         prev = span, span = (bc_memspan_t *)(span->l.next))
    {
        bc_memspan_t dummy;

        /* ---------------------------------------------------------------- */
        /*  Skip spans that already have data attached -- assume they are   */
        /*  correct.  These are usually POKE spans.                         */
        /* ---------------------------------------------------------------- */
        if (span->data)
            continue;

        assert((span->flags & BC_SPAN_PK) == 0);

        /* ---------------------------------------------------------------- */
        /*  Skip spans that dopn't preload a data segment.  These would be  */
        /*  banksw/memattr spans.                                           */
        /* ---------------------------------------------------------------- */
        if ((span->flags & BC_SPAN_PL) == 0)
            continue;

        /* ---------------------------------------------------------------- */
        /*  Load any file that might be associated with this span.          */
        /* ---------------------------------------------------------------- */
        if (span->f_name != NULL) 
        {
            int tmp;

            tmp = bc_read_helper(span->f_name, lbuf, 16);

            if (tmp < 0) { free(pbuf); return -1; }

            llen = (size_t)tmp;
            len = llen;
            buf = lbuf;
        } else if (span->f_name == NULL) /* true if primary-file span. */
        {
            len = plen;
            buf = pbuf;
        }

        /* ---------------------------------------------------------------- */
        /*  Now attach data from the file to the memspan.  If the span      */
        /*  started after EOF, delete the span from the span list.  If      */
        /*  the span starts in the file, but ends after EOF, trim it.       */
        /* ---------------------------------------------------------------- */
        if (span->s_fofs >= len)
        {
            ll_t *dead = (ll_t *)span;

            if (!prev) 
            {
                bc->span     = (bc_memspan_t *)span->l.next;
                dummy.l.next = span->l.next;
                span         = &dummy;
            } else       
            {
                prev->l.next = span->l.next;
                span         = prev;
            }
#ifndef BC_NOFREE
            bc_free_memspan_t(dead, NULL);
#endif
            continue;
        }

        if (span->e_fofs >= len)
            span->e_fofs = len - 1;

        slen         = span->e_fofs - span->s_fofs + 1;
        span->e_addr = span->s_addr + slen - 1;
        span->data   = CALLOC(uint_16, slen);

        if (!span->data) { fprintf(stderr, "out of memory\n"); exit(1); }

        memcpy(span->data, pbuf + span->s_fofs, slen * sizeof(uint_16));
    }

    free(pbuf);
    return 0;
}



#ifndef BC_NODOMACRO /* BC_NODOMACRO if you don't want to interpret macros */
/* ======================================================================== */
/*  BC_DO_MACROS  -- Applies macros that can be safely applied statically.  */
/*                                                                          */
/*  I offer two modes here:                                                 */
/*                                                                          */
/*      1.  Execute up until the first macro we can't do statically, and    */
/*                                                                          */
/*      2.  Scan the macros, and execute them only if they're all safe      */
/*          to execute or ignore.  Special case:  We'll go ahead and        */
/*          execute a macro set that ends in a "run" command as long as     */
/*          it's the last macro in the list.                                */
/*                                                                          */
/*  Macros that are safe to execute statically:                             */
/*                                                                          */
/*      L <file> <width> <addr> 'L'oad <file> of <width> at <addr>.         */
/*      P <addr> <data>         'P'oke <data> at <addr>.                    */
/*                                                                          */
/*  Macros that are safe to treat as NOPs during this pass:                 */
/*                                                                          */
/*    <n/a>                     Null macro lines                            */
/*      A                       Shows the instructions 'A'head.             */
/*      I <addr>                'I'nspect data/code at <addr>.              */
/*      W <name> <list>         'W'atch a set of values with label <name>.  */
/*      V                       'V'iew emulation display                    */
/*                                                                          */
/*  Macros that are NOT safe to execute statically:                         */
/*                                                                          */
/*    [0-7] <value>             Set register <n> to <value>                 */
/*      B                       Run until next vertical 'B'lank.            */
/*      R                       'R'un emulation.                            */
/*      T <addr>                'T'race to <addr>.                          */
/*      O <addr>                Run t'O' <addr>.                            */
/*    <unk>                     Any unknown macros that are in the list.    */
/*                                                                          */
/*  We implement L and P by tacking additional ROM segments to the          */
/*  memspan list.  Load and Poke macros implicitly set "Preload" and        */
/*  "Read" attributes.  Load may also set 'Narrow' if the ROM is <= 8 bits  */
/*  wide.  Poke will set the "POKE" attribute.  No others are set.          */
/* ======================================================================== */
int  bc_do_macros(bc_cfgfile_t *cfg, int partial_ok)
{
    bc_macro_t *macro;
    bc_memspan_t *newspan = NULL;

    /* -------------------------------------------------------------------- */
    /*  Trivial case:  Zero macros.                                         */
    /* -------------------------------------------------------------------- */
    if (!cfg->macro)
        return 0;

    /* -------------------------------------------------------------------- */
    /*  If given an all-or-nothing directive, prescan macro list to make    */
    /*  sure it is acceptable.                                              */
    /* -------------------------------------------------------------------- */
    if (!partial_ok)
    {
        for (macro = bc_parsed_cfg->macro; macro; 
             macro = (bc_macro_t*)macro->l.next)
        {
            if (macro->cmd != BC_MAC_LOAD    &&
                macro->cmd != BC_MAC_POKE    &&
                macro->cmd != BC_MAC_NONE    &&
                macro->cmd != BC_MAC_AHEAD   &&
                macro->cmd != BC_MAC_INSPECT &&
                macro->cmd != BC_MAC_WATCH   &&
                macro->cmd != BC_MAC_VIEW)
                break;
        }

        /* ---------------------------------------------------------------- */
        /*  If we exited before end of list, we're at an unsupported macro. */
        /*  If it's not a "run" macro at the end of the list, refuse it.    */
        /* ---------------------------------------------------------------- */
        if (macro != NULL && 
            (macro->l.next != NULL || macro->cmd != BC_MAC_RUN))
            return -1;
    }

    /* -------------------------------------------------------------------- */
    /*  Chase down the macro list looking for LOAD/POKE commands.           */
    /* -------------------------------------------------------------------- */
    for (macro = bc_parsed_cfg->macro; macro; 
         macro = (bc_macro_t*)macro->l.next)
    {
        if (macro->cmd == BC_MAC_LOAD || macro->cmd == BC_MAC_POKE)
        {
            newspan = CALLOC(bc_memspan_t, 1);
            if (!newspan)
            {
                fprintf(stderr, "like, out of memory or something\n");
                exit(1);
            }

            newspan->l.next = NULL;

            /* Inefficient, but called very rarely. */
            bc_parsed_cfg->span = LL_CONCAT(bc_parsed_cfg->span, newspan,
                                            bc_memspan_t);
        }

        switch (macro->cmd)
        {
            case BC_MAC_LOAD :
            {
                newspan->s_fofs = 0;
                newspan->e_fofs = 0xFFFF;
                newspan->s_addr = macro->arg.load.addr;
                newspan->e_addr = 0; /* calculated on the fly. */
                newspan->width  = macro->arg.load.width;
                newspan->flags  = BC_SPAN_PL | BC_SPAN_R;
                newspan->epage  = BC_SPAN_NOPAGE;
                newspan->f_name = strdup(macro->arg.load.name);

                if (newspan->width <= 8)
                    newspan->flags |= BC_SPAN_N;

                break;
            }

            case BC_MAC_POKE :
            {
                if (!(newspan->data = CALLOC(uint_16, 1)))
                {
                    fprintf(stderr, "out of memory\n");
                    exit(1);
                }

                newspan->s_fofs  = 0;
                newspan->e_fofs  = 0; 
                newspan->s_addr  = macro->arg.poke.addr;
                newspan->e_addr  = macro->arg.poke.addr;
                newspan->width   = 16;
                newspan->flags   = BC_SPAN_PL | BC_SPAN_R | BC_SPAN_PK;
                newspan->epage   = BC_SPAN_NOPAGE;
                newspan->f_name  = NULL;
                newspan->data[0] = macro->arg.poke.value;

                break;
            }

            case BC_MAC_NONE:
            case BC_MAC_AHEAD:
            case BC_MAC_WATCH:
            case BC_MAC_VIEW:
            {
                /* ignore */
                break;
            }

            default:
            {
                macro = NULL; /* force loop to terminate. */
                break;
            }
        }
        newspan = NULL;
    }


    return 0;
}
#endif


#ifndef BC_NOFREE /* BC_NOFREE if you have no intention of freeing a cfg */
/* ======================================================================== */
/*  BC_FREE_CFG   -- Release storage held by bc_cfgfile_t.                  */
/*                                                                          */
/*  Helper functions for FREE_CFG:                                          */
/*                                                                          */
/*  BC_FREE_MEMSPAN_T    -- Releases storage associated with bc_memspan_t.  */
/*  BC_FREE_MACRO_T      -- Releases storage associated with bc_macro_t.    */
/*  BC_FREE_VAR_T        -- Releases storage associated with bc_var_t.      */
/*  BC_FREE_VARLIKE_T    -- Releases storage associated with bc_varlike_t.  */
/*  BC_FREE_DIAG_T       -- Releases storage associated with bc_diag_t      */
/* ======================================================================== */
#ifndef CONDFREE
# define CONDFREE(x) do { if (x) free(x); } while (0)
#endif

/* ======================================================================== */
/*  BC_FREE_MEMSPAN_T    -- Releases storage associated with bc_memspan_t.  */
/* ======================================================================== */
void bc_free_memspan_t(ll_t *l_mem, void *unused)
{
    bc_memspan_t *mem = (bc_memspan_t*)l_mem;
    UNUSED(unused);

    CONDFREE(mem->f_name);
    CONDFREE(mem->data);
    free(mem);
}

/* ======================================================================== */
/*  BC_FREE_MACRO_T      -- Releases storage associated with bc_macro_t.    */
/* ======================================================================== */
void bc_free_macro_t(ll_t *l_mac, void *unused)
{
    bc_macro_t *mac = (bc_macro_t *)l_mac;

    UNUSED(unused);

    switch (mac->cmd)
    {
        case BC_MAC_LOAD:   CONDFREE(mac->arg.load.name);   break;

        case BC_MAC_WATCH:  CONDFREE(mac->arg.watch.name); 
                            CONDFREE(mac->arg.watch.addr);  break;

        default: /* nothing */ ;
    }
    free(mac);
}

/* ======================================================================== */
/*  BC_FREE_VAR_T        -- Releases storage associated with bc_var_t.      */
/* ======================================================================== */
void bc_free_var_t(ll_t *l_var, void *unused)
{
    bc_var_t *var = (bc_var_t *)l_var;

    UNUSED(unused);

    CONDFREE(var->name);
    CONDFREE(var->val.str_val);
    free(var);
}

/* ======================================================================== */
/*  BC_FREE_DIAG_T       -- Releases storage associated with bc_diag_t      */
/* ======================================================================== */
void bc_free_diag_t(ll_t *l_diag, void *unused)
{
    bc_diag_t *diag = (bc_diag_t *)l_diag;

    UNUSED(unused);

    CONDFREE(diag->sect);
    CONDFREE(diag->msg);
}


/* ======================================================================== */
/*  BC_FREE_CFG   -- Release storage held by bc_cfgfile_t.                  */
/* ======================================================================== */
void bc_free_cfg(bc_cfgfile_t *cfg)
{
    CONDFREE(cfg->cfgfile);
    CONDFREE(cfg->binfile);

    if (cfg->span    ) ll_acton(&cfg->span->l,     bc_free_memspan_t, NULL);
    if (cfg->macro   ) ll_acton(&cfg->macro->l,    bc_free_macro_t,   NULL);
    if (cfg->vars    ) ll_acton(&cfg->vars->l,     bc_free_var_t,     NULL);
    if (cfg->keys[0] ) ll_acton(&cfg->keys[0]->l,  bc_free_var_t,     NULL);
    if (cfg->keys[1] ) ll_acton(&cfg->keys[1]->l,  bc_free_var_t,     NULL);
    if (cfg->keys[2] ) ll_acton(&cfg->keys[2]->l,  bc_free_var_t,     NULL);
    if (cfg->keys[3] ) ll_acton(&cfg->keys[3]->l,  bc_free_var_t,     NULL);
    if (cfg->joystick) ll_acton(&cfg->joystick->l, bc_free_var_t,     NULL);
    if (cfg->diags   ) ll_acton(&cfg->diags->l,    bc_free_diag_t,    NULL);

    free(cfg);
}
#endif


#ifndef BC_NOPRINT
/* ======================================================================== */
/*  BC_PRINT_CFG  -- Chase through a bc_cfgfile_t structure and print out   */
/*                   what we find therein.                                  */
/*                                                                          */
/*  Helper functions for PRINT_CFG:                                         */
/*                                                                          */
/*  BC_PRINT_DIAG    -- Print all the collected diagnostics attached to cfg */
/*  BC_PRINT_MACRO   -- Print the [macro] section                           */
/*  BC_PRINT_VARLIKE -- Print [var],[keys],[joystick] sections              */
/*  BC_PRINT_MEMSPAN -- Print out all the memory span information.          */
/* ======================================================================== */


/* ======================================================================== */
/*  BC_PRINT_DIAG    -- Print all the collected diagnostics attached to cfg */
/* ======================================================================== */
void bc_print_diag
(
    FILE                *RESTRICT const f, 
    const char          *RESTRICT const fname,
    const bc_diag_t     *RESTRICT const diag,
    int                                 cmt 
)
{
    const bc_diag_t *RESTRICT d;
    int tot_warn = 0, tot_err = 0;

    /* -------------------------------------------------------------------- */
    /*  Step through the list and dump these out.  If cmt != 0, put a       */
    /*  semicolon before each line.                                         */
    /* -------------------------------------------------------------------- */
    for (d = diag; d; d = (const bc_diag_t *)d->l.next)
    {
        fprintf(f, "%s%s:%d:%s %s: %s\n", 
                cmt ? "; " : "", fname, d->line, 
                d->sect ? d->sect : "<toplevel?>", 
                d->type == BC_DIAG_WARNING ? "warning" : "error",
                d->msg  ? d->msg  : "out of memory?");
        if (d->type == BC_DIAG_WARNING) tot_warn++; else tot_err++;
    }

    /* -------------------------------------------------------------------- */
    /*  If we had any warnings or errors, print summary information.        */
    /* -------------------------------------------------------------------- */
    if (tot_warn || tot_err)
    {
        fprintf(f, "%s%d warnings, %d errors\n", 
                cmt ? "; " : "", tot_warn, tot_err);
    }
}

/* ======================================================================== */
/*  BC_PRINT_MACRO_T -- Print a single macro_t structure.                   */
/* ======================================================================== */
void bc_print_macro_t(ll_t *l_mac, void *v_f)
{
    bc_macro_t *RESTRICT const mac = (bc_macro_t*)l_mac;
    FILE       *RESTRICT const f   = (FILE*)v_f;

    if (mac->quiet) 
        fputc('@', f);

    switch (mac->cmd)
    {
        case BC_MAC_NONE:       { fputc(';', f); break; }
        case BC_MAC_AHEAD:      { fputc('A', f); break; }
        case BC_MAC_BLANK:      { fputc('B', f); break; }
        case BC_MAC_RUN:        { fputc('R', f); break; }
        case BC_MAC_VIEW:       { fputc('V', f); break; }

        case BC_MAC_REG:
        {
            fprintf(f, "%d $%.4X", mac->arg.reg.reg, mac->arg.reg.value);
            break;
        }
                                
        case BC_MAC_INSPECT:    
        {
            fprintf(f, "I $%.4X", mac->arg.inspect.addr);
            break;
        }

        case BC_MAC_POKE:
        {
            fprintf(f, "P $%.4X $%.4X", 
                    mac->arg.poke.addr, mac->arg.poke.value);
            break;
        }

        case BC_MAC_LOAD:
        {
            fprintf(f, "L %s %d $%.4X",
                    mac->arg.load.name,
                    mac->arg.load.width,
                    mac->arg.load.addr);
            break;
        }

        case BC_MAC_RUNTO:
        {
            fprintf(f, "O $%.4X", mac->arg.runto.addr);
            break;
        }

        case BC_MAC_TRACE:
        {
            fprintf(f, "T $%.4X", mac->arg.runto.addr);
            break;
        }

        case BC_MAC_WATCH:
        {
            int i, lo, hi;

            fprintf(f, "W %s ", mac->arg.watch.name);
            for (i = 0; i < mac->arg.watch.spans; i++)
            {
                if (i) 
                    fputc(',', f);

                lo = mac->arg.watch.addr[2*i + 0];
                hi = mac->arg.watch.addr[2*i + 1];

                if (lo == hi) fprintf(f, "$%.4X", lo);
                else          fprintf(f, "$%.4X-$%.4X", lo, hi);
            }
            break;
        }

        case BC_MAC_ERROR:
        default:
        {
            fprintf(f, "; unknown macro\n");
            break;
        }
    }

    fputc('\n', f);
}

/* ======================================================================== */
/*  BC_PRINT_MACRO   -- Print the [macro] section                           */
/* ======================================================================== */
void bc_print_macro(FILE *RESTRICT f, bc_macro_t *RESTRICT mac)
{
    /* -------------------------------------------------------------------- */
    /*  Step through the macro list, regenerating each macro.               */
    /* -------------------------------------------------------------------- */
    fprintf(f, "\n[macro]\n");
    ll_acton(&(mac->l), bc_print_macro_t, (void *)f);
}

/* ======================================================================== */
/*  BC_PRINT_VAR_T   -- Print <name> = <value> tuple.                       */
/* ======================================================================== */
void bc_print_var_t
(
    ll_t *RESTRICT l_var,
    void *RESTRICT v_f  
)
{
    bc_var_t *var = (bc_var_t *)l_var;
    FILE *f       = (FILE *)v_f;

    if (var->val.flag & (BC_VAR_DECNUM))
    {
        fprintf(f, "%s = %d\n", var->name, var->val.dec_val);
    } else if (var->val.flag & (BC_VAR_HEXNUM))
    {
        fprintf(f, "%s = $%.4X\n", var->name, var->val.hex_val);
    } else
    {
        fprintf(f, "%s = %s\n", var->name, var->val.str_val);
    }
}

/* ======================================================================== */
/*  BC_PRINT_VARLIKE -- Print [var],[keys],[joystick] sections              */
/* ======================================================================== */
void bc_print_varlike
(
    FILE       *RESTRICT f, 
    bc_var_t   *RESTRICT varlike, 
    const char *RESTRICT sectname
)
{
    /* -------------------------------------------------------------------- */
    /*  Real easy:  Just step thru the list and print all the tuples.       */
    /* -------------------------------------------------------------------- */
    fprintf(f, "\n%s\n", sectname);
    ll_acton(&(varlike->l), bc_print_var_t, f);
}

/* ======================================================================== */
/*  BC_PRINT_MEMSPAN -- Print out all the memory span information.          */
/* ======================================================================== */
void bc_print_memspan
(
    FILE                *RESTRICT const f, 
    const bc_memspan_t  *RESTRICT const mem
)
{
    const bc_memspan_t *RESTRICT m;
    int need_hdr;

    /* -------------------------------------------------------------------- */
    /*  First print out a 'raw' list as a comment.                          */
    /* -------------------------------------------------------------------- */
    m = mem;
    while (m)
    {
        fprintf(f, "; $%.4X - $%.4X => $%.4X - $%.4X PAGE %X "
                   "FLAGS %c%c%c%c%c%c%c from \"%s\"\n",
                   m->s_fofs, m->e_fofs, m->s_addr, m->e_addr, m->epage,
                   m->flags & BC_SPAN_R  ? 'R' : '-',
                   m->flags & BC_SPAN_W  ? 'W' : '-',
                   m->flags & BC_SPAN_N  ? 'N' : '-',
                   m->flags & BC_SPAN_B  ? 'B' : '-',
                   m->flags & BC_SPAN_PL ? 'L' : '-',
                   m->flags & BC_SPAN_PK ? 'K' : '-',
                   m->flags & BC_SPAN_EP ? 'E' : '-',
                   m->f_name ? m->f_name : "(primary)"
                   );

        m = (bc_memspan_t *)m->l.next;
    }

    /* -------------------------------------------------------------------- */
    /*  Next, try to print it out as CFG sections.  The CFG format is       */
    /*  kinda odd, but since we just parsed the config, it should be easy.  */
    /*                                                                      */
    /*  The following truth table indicates how the attributes map back     */
    /*  to different sections.                                              */
    /*                                                                      */
    /*      R W N B PL PK EP   Section       Format                         */
    /*      R x x x PL -  -    [mapping]     $xxxx - $xxxx = $xxxx          */
    /*      R x x x PL -  EP   [mapping]     $xxxx - $xxxx = $xxxx PAGE d   */
    /*      - x x x PL -  -    [preload]     $xxxx - $xxxx = $xxxx          */
    /*      - x x x PL -  EP   [preload]     $xxxx - $xxxx = $xxxx PAGE d   */
    /*      x x x B x  -  x    [bankswitch]  $xxxx - $xxxx                  */
    /*      R W x x x  -  x    [memattr]     $xxxx - $xxxx = RAM d          */
    /*      - W x x x  -  x    [memattr]     $xxxx - $xxxx = WOM d          */
    /*      R - x x -  -  x    [memattr]     $xxxx - $xxxx = ROM d          */
    /* -------------------------------------------------------------------- */


    /* -------------------------------------------------------------------- */
    /*  Get all the [mapping] sections.                                     */
    /* -------------------------------------------------------------------- */
    need_hdr = 1;
    for (m = mem; m; m = (bc_memspan_t *)m->l.next)
    {
        if ((m->flags & (BC_SPAN_R|BC_SPAN_PL)) != (BC_SPAN_R|BC_SPAN_PL) ||
            (m->flags & BC_SPAN_PK) ||
            (m->f_name != NULL))
            continue;

        if (need_hdr)
        {
            fprintf(f, "\n[mapping]\n");
            need_hdr = 0;
        }

        if (m->flags & BC_SPAN_EP)
            fprintf(f, "$%.4X - $%.4X = $%.4X PAGE %X\n",
                    m->s_fofs, m->e_fofs, m->s_addr, m->epage);
        else
            fprintf(f, "$%.4X - $%.4X = $%.4X\n",
                    m->s_fofs, m->e_fofs, m->s_addr);
    }

    /* -------------------------------------------------------------------- */
    /*  Get all the [preload] sections.                                     */
    /* -------------------------------------------------------------------- */
    need_hdr = 1;
    for (m = mem; m; m = (bc_memspan_t *)m->l.next)
    {
        if ((m->flags & (BC_SPAN_R|BC_SPAN_PL)) != (BC_SPAN_PL) ||
            (m->flags & BC_SPAN_PK) ||
            (m->f_name != NULL))
            continue;

        if (need_hdr)
        {
            fprintf(f, "\n[preload]\n");
            need_hdr = 0;
        }

        if (m->flags & BC_SPAN_EP)
            fprintf(f, "$%.4X - $%.4X = $%.4X PAGE %X\n",
                    m->s_fofs, m->e_fofs, m->s_addr, m->epage);
        else
            fprintf(f, "$%.4X - $%.4X = $%.4X\n",
                    m->s_fofs, m->e_fofs, m->s_addr);
    }

    /* -------------------------------------------------------------------- */
    /*  Get all the [bankswitch] sections.                                  */
    /* -------------------------------------------------------------------- */
    need_hdr = 1;
    for (m = mem; m; m = (bc_memspan_t *)m->l.next)
    {
        if ((m->flags & (BC_SPAN_B)) != (BC_SPAN_B) ||
            (m->flags & BC_SPAN_PK))
            continue;

        if (need_hdr)
        {
            fprintf(f, "\n[bankswitch]\n");
            need_hdr = 0;
        }

        fprintf(f, "$%.4X - $%.4X\n", m->s_addr, m->e_addr);
    }


    /* -------------------------------------------------------------------- */
    /*  Get all the [memattr] RAM sections.                                 */
    /* -------------------------------------------------------------------- */
    need_hdr = 1; /* reuse for next 3 loops */
    for (m = mem; m; m = (bc_memspan_t *)m->l.next)
    {
        if ((m->flags & (BC_SPAN_R | BC_SPAN_W)) != (BC_SPAN_R | BC_SPAN_W) ||
            (m->flags & BC_SPAN_PK))
            continue;

        if (need_hdr)
        {
            fprintf(f, "\n[memattr]\n");
            need_hdr = 0;
        }

        fprintf(f, "$%.4X - $%.4X = RAM %d\n", m->s_addr, m->e_addr, m->width);
    }


    /* -------------------------------------------------------------------- */
    /*  Get all the [memattr] WOM sections.                                 */
    /* -------------------------------------------------------------------- */
    /* carry need_hdr from previous loop */
    for (m = mem; m; m = (bc_memspan_t *)m->l.next)
    {
        if ((m->flags & (BC_SPAN_R | BC_SPAN_W)) != (BC_SPAN_W) ||
            (m->flags & BC_SPAN_PK))
            continue;

        if (need_hdr)
        {
            fprintf(f, "\n[memattr]\n");
            need_hdr = 0;
        }

        fprintf(f, "$%.4X - $%.4X = RAM %d\n", m->s_addr, m->e_addr, m->width);
    }

    /* -------------------------------------------------------------------- */
    /*  Get all the [memattr] ROM sections.                                 */
    /* -------------------------------------------------------------------- */
    /* carry need_hdr from previous loop */
    for (m = mem; m; m = (bc_memspan_t *)m->l.next)
    {
        if ((m->flags & (BC_SPAN_R | BC_SPAN_W | BC_SPAN_PL)) != (BC_SPAN_R) ||
            (m->flags & BC_SPAN_PK))
            continue;

        if (need_hdr)
        {
            fprintf(f, "\n[memattr]\n");
            need_hdr = 0;
        }

        fprintf(f, "$%.4X - $%.4X = RAM %d\n", m->s_addr, m->e_addr, m->width);
    }
}

/* ======================================================================== */
/*  BC_PRINT_CFG -- Chase through a bc_cfgfile_t structure and print out    */
/*                  what we find therein.                                   */
/* ======================================================================== */
void bc_print_cfg
(
    FILE                *RESTRICT const f, 
    const bc_cfgfile_t  *RESTRICT const bc
)
{
    if (bc->diags)      bc_print_diag   (f, bc->cfgfile,  bc->diags, 1   );
    if (bc->span)       bc_print_memspan(f, bc->span                     );
    if (bc->macro)      bc_print_macro  (f, bc->macro                    );
    if (bc->vars)       bc_print_varlike(f, bc->vars,     "[vars]"       );
    if (bc->keys[0])    bc_print_varlike(f, bc->keys[0],  "[keys]"       );
    if (bc->keys[1])    bc_print_varlike(f, bc->keys[1],  "[capslock]"   );
    if (bc->keys[2])    bc_print_varlike(f, bc->keys[2],  "[numlock]"    );
    if (bc->keys[3])    bc_print_varlike(f, bc->keys[3],  "[scrolllock]" );
    if (bc->joystick)   bc_print_varlike(f, bc->joystick, "[joystick]"   );
}

#endif /* BC_NOPRINT */

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
/*                 Copyright (c) 2003-+Inf, Joseph Zbiciak                  */
/* ======================================================================== */


