/* ======================================================================== */
/*  Title:    Legacy INTVPC BIN+CFG support                                 */
/*  Author:   J. Zbiciak, J. Tanner                                         */
/* ------------------------------------------------------------------------ */
/*  This module implements a memory peripheral with the semantics of        */
/*  INTVPC's BIN+CFG file, at least for the most part.  ECS paged ROM is    */
/*  supported indirectly by instantiating Paged ROMs from mem/mem.c.        */
/*                                                                          */
/*  The routines for reading BIN+CFG files are in bincfg/bincfg.h, not      */
/*  this file.                                                              */
/* ======================================================================== */


#include "config.h"
#include "periph/periph.h"
#include "mem/mem.h"
#include "file/file.h"
#include "cp1600/cp1600.h"
#include "bincfg.h"
#include "legacy.h"

/* ======================================================================== */
/*  LEGACY_READ -- read from a legacy BIN+CFG.                              */
/* ======================================================================== */
uint_32 legacy_read (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    legacy_t *l = (legacy_t*)per;
    uint_32 mask;

    UNUSED(ign);
    UNUSED(data);

    if ((l->loc[addr].flags & BC_SPAN_R) != 0)
    {
        mask = ~(~0 << l->loc[addr].width);
        return l->loc[addr].data & mask;
    }

    return ~0U;
}

/* ======================================================================== */
/*  LEGACY_WRITE -- write to a legacy BIN+CFG.                              */
/* ======================================================================== */
void  legacy_write(periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    legacy_t *l = (legacy_t*)per;
    uint_32 mask;

    UNUSED(ign);
    UNUSED(data);

    if ((l->loc[addr].flags & BC_SPAN_W) != 0)
    {
        mask = ~(~0 << l->loc[addr].width);
        l->loc[addr].data = data & mask;
    }
}

/* ======================================================================== */
/*  LEGACY_POKE  -- write to a legacy BIN+CFG, ignoring read-only status.   */
/* ======================================================================== */
void legacy_poke (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    legacy_t *l = (legacy_t*)per;
    uint_32 mask;

    UNUSED(ign);
    UNUSED(data);

    mask = ~(~0 << l->loc[addr].width);
    l->loc[addr].data = data & mask;
}


/* ======================================================================== */
/*  LEGACY_APPLY_BINCFG -- Populates a legacy_t from a BIN+CFG.             */
/* ======================================================================== */
LOCAL int legacy_apply_bincfg(bc_cfgfile_t *bc, legacy_t *l, void *cpu)
{
    int addr, ofs;
    bc_memspan_t *span;
    int num_ecs = 0;

    l->pg_rom = NULL;
    l->bc     = bc;
    l->loc    = CALLOC(struct legacy_loc_t, 65536);
    if (!l->loc)
        return -1;

    /* -------------------------------------------------------------------- */
    /*  Traverse the memspan list, registering memory segments in legacy_t  */
    /* -------------------------------------------------------------------- */
    for (span = bc->span; span; span = (bc_memspan_t *)span->l.next)
    {
        int slen;

        slen = span->e_addr - span->s_addr + 1;

        /* ---------------------------------------------------------------- */
        /*  If this span has an ECS page, skip it for now.  We'll come      */
        /*  back to it in a minute.                                         */
        /* ---------------------------------------------------------------- */
        if ( span->epage != BC_SPAN_NOPAGE ||
            (span->flags & BC_SPAN_EP) != 0)
        {
            num_ecs++;
            continue;
        }

        /* ---------------------------------------------------------------- */
        /*  Assertion:  If PRELOAD, then span->data.                        */
        /* ---------------------------------------------------------------- */
        assert(((span->flags & BC_SPAN_PL) == 0) || (span->data != 0));

        /* ---------------------------------------------------------------- */
        /*  Add the segment to the legacy_t.                                */
        /* ---------------------------------------------------------------- */
        for (addr = span->s_addr, ofs = 0; addr <= span->e_addr; addr++, ofs++)
        {
            l->loc[addr].flags |= span->flags;

            if (span->data)
                l->loc[addr].data  = span->data[ofs];
            if ((span->flags & BC_SPAN_PK) == 0)
                l->loc[addr].width = span->width;
        }
    }

    /* -------------------------------------------------------------------- */
    /*  Now allocate ECS Paged ROMs as needed.                              */
    /* -------------------------------------------------------------------- */
    l->npg_rom = num_ecs;
    if (num_ecs > 0)
    {
        uint_32 slen, i, cur_ecs;

        l->pg_rom = CALLOC(mem_t, num_ecs);

        for (span = bc->span, cur_ecs = 0; span; 
             span = (bc_memspan_t *)span->l.next)
        {
            /* ------------------------------------------------------------ */
            /*  Skip spans that are not ECS pages.                          */
            /* ------------------------------------------------------------ */
            if ( span->epage == BC_SPAN_NOPAGE &&
                (span->flags & BC_SPAN_EP) == 0)
            {
                continue;
            }

            /* ------------------------------------------------------------ */
            /*  Assertion:  If ECS Paged, then span->data && bank >= 0      */
            /* ------------------------------------------------------------ */
            assert(span->data && span->epage != BC_SPAN_NOPAGE);

            /* ------------------------------------------------------------ */
            /*  Get the page size and round up to next power of 2.          */
            /* ------------------------------------------------------------ */
            slen = span->e_addr - span->s_addr; /* size - 1 */
            for (i = 0; slen >> i == 0; i++)
                slen |= slen >> i;
            slen++;

            if (slen != (uint_32)span->e_addr - span->s_addr + 1)
            {
                span->data   = (uint_16 *)realloc(span->data, slen);
                span->e_addr = span->s_addr + slen - 1;
                if (!span->data)
                    goto fail;
            }

            /* ------------------------------------------------------------ */
            /*  Make a paged ROM out of this.                               */
            /* ------------------------------------------------------------ */
            if (mem_make_prom(l->pg_rom + cur_ecs, span->width, span->s_addr, 
                             slen, span->epage, span->data, cpu))
                goto fail;

            /* ------------------------------------------------------------ */
            /*  Since we claimed span->data, null it out.                   */
            /* ------------------------------------------------------------ */
            span->data = NULL;

            cur_ecs++;
        }
    }

    return 0;

fail:
    {
        int i;

        if (l->pg_rom)
            for (i = 0; i < num_ecs; i++)
                CONDFREE(l->pg_rom[num_ecs].image);

        CONDFREE(l->loc);
        CONDFREE(l->pg_rom);
    }
    return -1;
}

/* ======================================================================== */
/*  LEGACY_READ_BINCFG -- Reads a .BIN and optional .CFG file.              */
/* ======================================================================== */
LOCAL int legacy_read_bincfg(const char *bin_fn, const char *cfg_fn, 
                             legacy_t *l, void *cpu)
{
    FILE *fc;
    bc_cfgfile_t *bc;

    /* -------------------------------------------------------------------- */
    /*  Read the .CFG file.  This process  open it, then parse it.          */
    /*  Otherwise, we skip it -- lack of .CFG file is non-fatal.            */
    /* -------------------------------------------------------------------- */
    if (cfg_fn && (fc = fopen(cfg_fn, "r")) != NULL)
    {
        bc = bc_parse_cfg(fc, bin_fn, cfg_fn);
        fclose(fc);
    } else
    {
        bc = bc_parse_cfg(NULL, bin_fn, NULL);
    }
    if (!bc || !bc->span)
    {
        if (cfg_fn)
            fprintf(stderr, 
                   "Could not generate configuration from this BIN+CFG:\n"
                   "  \"%s\"\n  \"%s\"\n", bin_fn, cfg_fn);
        else
            fprintf(stderr, 
                   "Could not generate configuration from this BIN:\n"
                   "  \"%s\"\n", bin_fn);

        return -1;
    }

#ifndef BC_NODOMACRO
    /* -------------------------------------------------------------------- */
    /*  Apply any statically safe macros.  Ignore errors.                   */
    /* -------------------------------------------------------------------- */
    bc_do_macros(bc, 0);
#endif

    /* -------------------------------------------------------------------- */
    /*  Populate the config with corresponding BIN data.                    */
    /* -------------------------------------------------------------------- */
    if (bc_read_data(bc))
    {
        fprintf(stderr, "Error reading data for CFG file.\n");
        goto err;
    }

    /* -------------------------------------------------------------------- */
    /*  Apply the configuration.  This generates the icartrom.              */
    /* -------------------------------------------------------------------- */
    if (legacy_apply_bincfg(bc, l, cpu))
    {
        fprintf(stderr, "Error applying CFG file\n");
        goto err;
    }

    return 0;

err:
#ifndef BC_NOFREE
    /* -------------------------------------------------------------------- */
    /*  Discard the parsed config.                                          */
    /* -------------------------------------------------------------------- */
    bc_free_cfg(bc);
    l->bc = NULL;
#endif
    return -1;

}

/* ======================================================================== */
/*  LEGACY_PRINT_LOADING -- Print what files it's trying to load.           */
/* ======================================================================== */
LOCAL void legacy_print_loading(char *f1, char *f2)
{
    jzp_printf("Loading:\n");
    if (f1 && file_exists(f1)) jzp_printf("  %s\n", f1);
    if (f2 && file_exists(f2)) jzp_printf("  %s\n", f2);
}

/* ======================================================================== */
/*  LEGACY_DTOR   -- Tear down a legacy_t, freeing its resources.           */
/* ======================================================================== */
LOCAL void legacy_dtor(periph_p p)
{
    legacy_t *l = (legacy_t *)p;

    int i;

    for (i = 0; i < l->npg_rom; i++)
        CONDFREE(l->pg_rom[i].image);

    CONDFREE(l->pg_rom);
    CONDFREE(l->loc);

    if (l->bc)
        bc_free_cfg(l->bc);
}

/* ======================================================================== */
/*  LEGACY_BINCFG -- Try to determine if a file is BIN+CFG or ROM, and      */
/*                   read it in if it is BIN+CFG.                           */
/*                                                                          */
/*  The return value from this function requires explanation.  If we        */
/*  figure out a .ROM file associated with this fname, we will a distinct   */
/*  char * that points to its filename.  If we determine the file is a      */
/*  BIN+CFG file pair, we will try to load it.  On success, we will return  */
/*  an exact copy of fname directly.  Otherwise, we will return NULL.       */
/* ======================================================================== */
char *legacy_bincfg
(
    legacy_t        *l,         /*  Legacy BIN+CFG structure            */
    path_t          *path,      /*  Search path for games.              */
    const char      *fname,     /*  Basename to use for CFG/BIN         */
    int             *legacy_rom,
    void            *cpu
)
{
    char *rom1_fn = NULL, *bin1_fn = NULL, *cfg1_fn = NULL;
    char *rom2_fn = NULL, *bin2_fn = NULL, *cfg2_fn = NULL, *int2_fn = NULL;
    char *rom3_fn = NULL;
    char *itv2_fn = NULL;
    char *p_rom1_fn = NULL, *p_bin1_fn = NULL, *p_cfg1_fn = NULL;
    char *p_rom2_fn = NULL, *p_bin2_fn = NULL, *p_cfg2_fn = NULL;
    char *p_rom3_fn = NULL;
    char *p_itv2_fn = NULL, *p_int2_fn = NULL;
    int name_len = strlen(fname);
    char *ext = strrchr(fname, '.');  /* look for an extension */
    path_t dummy, *tmp;
    char *fn;
    int max_p_len = 1, abs_path = 0;
    char sep[2] = { PATH_SEP, 0 };

    *legacy_rom = 0;

    /* -------------------------------------------------------------------- */
    /*  Silly case:  If the filename ends in a '.', strip it off.           */
    /* -------------------------------------------------------------------- */
    if (ext && ext[1] == '\0')
    {
        *ext = '\0';
        ext = NULL;
    }
 
    /* -------------------------------------------------------------------- */
    /*  Is it .ROM or .CC3?                                                 */
    /* -------------------------------------------------------------------- */
    if (ext && (stricmp(ext, ".rom")==0 ||
                stricmp(ext, ".cc3")==0))
    {
        rom1_fn = strdup(fname);
    }
    /* -------------------------------------------------------------------- */
    /*  Is it .BIN or .INT or .ITV?                                         */
    /* -------------------------------------------------------------------- */
    else if (ext && (stricmp(ext, ".bin")==0 || 
                     stricmp(ext, ".int")==0 || 
                     stricmp(ext, ".itv")==0) )
    {
        char *s;

        bin1_fn = strdup(fname);
        cfg1_fn = strdup(fname);
        if (!cfg1_fn) 
        { 
            fprintf(stderr, "legacy_bincfg: Out of memory\n");
            exit(1);
        }

        s = cfg1_fn + name_len - 4;
        strcpy(s, ".cfg");
    }
    /* -------------------------------------------------------------------- */
    /*  In case those fail, have a backup plan.                             */
    /*                                                                      */
    /*  For instance, on the Mac version of "Intellivision Lives!", the     */
    /*  ROM images are equivalent to a .BIN, but they have no extension.    */
    /*  They do, occasionally, have a corresponding .CFG file, with ".cfg"  */
    /*  as the extension.  (Pointed out by JJT.)                            */
    /* -------------------------------------------------------------------- */
    else 
    {
        /* ---------------------------------------------------------------- */
        /*  Handle the no-extension-but-matching-cfg case.                  */
        /* ---------------------------------------------------------------- */
        if (!ext)
        {
            bin1_fn = strdup(fname);
            cfg1_fn = CALLOC(char, name_len+5);
            snprintf(cfg1_fn, name_len + 5, "%s.cfg", fname);
        }
        
        /* ---------------------------------------------------------------- */
        /*  Also search for all known extensions appended to the file name. */
        /*  This lets users type "jzintv foo" and it'll find "foo.rom" or   */
        /*  "foo.bin" or whatever.                                          */
        /* ---------------------------------------------------------------- */
        bin2_fn = CALLOC(char, 6*(name_len + 5));
        if (!bin2_fn)
        { 
            fprintf(stderr, "legacy_bincfg: Out of memory\n");
            exit(1);
        }
        cfg2_fn = bin2_fn + name_len + 5;
        rom2_fn = cfg2_fn + name_len + 5;
        int2_fn = rom2_fn + name_len + 5;
        itv2_fn = int2_fn + name_len + 5;
        rom3_fn = itv2_fn + name_len + 5;
        
        snprintf(bin2_fn, name_len + 5, "%s.bin", fname);    
        snprintf(cfg2_fn, name_len + 5, "%s.cfg", fname);    
        snprintf(rom2_fn, name_len + 5, "%s.rom", fname);    
        snprintf(int2_fn, name_len + 5, "%s.int", fname);    
        snprintf(itv2_fn, name_len + 5, "%s.itv", fname);
        snprintf(rom3_fn, name_len + 5, "%s.cc3", fname);
    }

    /* -------------------------------------------------------------------- */
    /*  Now try out all of our options in each directory of the path.       */
    /*  If the filename is an absolute path, short out the path search.     */
    /* -------------------------------------------------------------------- */
    if (!path)
    {
        path        = &dummy;
        dummy.p_len = 1;
        dummy.name  = ".";
        dummy.next  = NULL;
    }

    if (is_absolute_path(fname))
    {
        path        = &dummy;
        dummy.p_len = 0;
        dummy.name  = "";
        dummy.next  = NULL;
        abs_path    = 1;
        sep[0]      = 0;
    }

    for (tmp = path; tmp; tmp = tmp->next)
        if (max_p_len < tmp->p_len)
            max_p_len = tmp->p_len;

    p_rom1_fn = CALLOC(char, 9*(max_p_len + name_len + 7));
    p_bin1_fn = p_rom1_fn + max_p_len + name_len + 7;
    p_cfg1_fn = p_bin1_fn + max_p_len + name_len + 7;
    p_rom2_fn = p_cfg1_fn + max_p_len + name_len + 7;
    p_bin2_fn = p_rom2_fn + max_p_len + name_len + 7;
    p_cfg2_fn = p_bin2_fn + max_p_len + name_len + 7;
    p_int2_fn = p_cfg2_fn + max_p_len + name_len + 7;
    p_itv2_fn = p_int2_fn + max_p_len + name_len + 7;
    p_rom3_fn = p_itv2_fn + max_p_len + name_len + 7;

    tmp = path;

    while (path)
    {
        /* ---------------------------------------------------------------- */
        /*  Generate path-qualified versions of the filenames.              */
        /* ---------------------------------------------------------------- */
#define N (max_p_len+name_len+7)         
        if (rom1_fn) snprintf(p_rom1_fn, N,"%s%s%s", path->name, sep, rom1_fn);
        if (bin1_fn) snprintf(p_bin1_fn, N,"%s%s%s", path->name, sep, bin1_fn);
        if (cfg1_fn) snprintf(p_cfg1_fn, N,"%s%s%s", path->name, sep, cfg1_fn);
        if (rom2_fn) snprintf(p_rom2_fn, N,"%s%s%s", path->name, sep, rom2_fn);
        if (bin2_fn) snprintf(p_bin2_fn, N,"%s%s%s", path->name, sep, bin2_fn);
        if (cfg2_fn) snprintf(p_cfg2_fn, N,"%s%s%s", path->name, sep, cfg2_fn);
        if (int2_fn) snprintf(p_int2_fn, N,"%s%s%s", path->name, sep, int2_fn);
        if (itv2_fn) snprintf(p_itv2_fn, N,"%s%s%s", path->name, sep, itv2_fn);
        if (rom3_fn) snprintf(p_rom3_fn, N,"%s%s%s", path->name, sep, rom3_fn);

        /* ---------------------------------------------------------------- */
        /*  See if path-qualified instances of the files exist.             */
        /* ---------------------------------------------------------------- */
        if (rom1_fn && file_exists(p_rom1_fn))
        {
            legacy_print_loading(p_rom1_fn, NULL);
            fn = strdup(p_rom1_fn);
            
            free(p_rom1_fn);
            if (rom1_fn) free(rom1_fn);
            if (bin1_fn) free(bin1_fn);
            if (cfg1_fn) free(cfg1_fn);
            if (bin2_fn) free(bin2_fn);  /* also frees cfg2/int2/itv2/rom2 */

            return fn;
        }

        if (bin1_fn && file_exists(p_bin1_fn))
        {
            legacy_print_loading(p_bin1_fn, p_cfg1_fn);
            fn = strdup(p_bin1_fn);

            if (legacy_read_bincfg(p_bin1_fn, p_cfg1_fn, l, cpu))
            {
                free(p_rom1_fn);
                if (rom1_fn) free(rom1_fn);
                if (bin1_fn) free(bin1_fn);
                if (cfg1_fn) free(cfg1_fn);
                if (bin2_fn) free(bin2_fn); /*also frees cfg2/int2/itv2/rom2*/
                return NULL;
            }

            goto finish;
        }

        if (rom2_fn && file_exists(p_rom2_fn))
        {
            legacy_print_loading(p_rom2_fn, NULL);
            fn = strdup(p_rom2_fn);

            free(p_rom1_fn);
            if (rom1_fn) free(rom1_fn);
            if (bin1_fn) free(bin1_fn);
            if (cfg1_fn) free(cfg1_fn);
            if (bin2_fn) free(bin2_fn); /*also frees cfg2/int2/itv2/rom2*/
            return fn;
        }

        if (rom3_fn && file_exists(p_rom3_fn))
        {
            legacy_print_loading(p_rom3_fn, NULL);
            fn = strdup(p_rom3_fn);

            free(p_rom1_fn);
            if (rom1_fn) free(rom1_fn);
            if (bin1_fn) free(bin1_fn);
            if (cfg1_fn) free(cfg1_fn);
            if (bin2_fn) free(bin2_fn); /*also frees cfg2/int2/itv2/rom2*/
            return fn;
        }

        if (bin2_fn && file_exists(p_bin2_fn))
        {
            legacy_print_loading(p_bin2_fn, p_cfg2_fn);
            fn = strdup(p_bin2_fn);

            if (legacy_read_bincfg(p_bin2_fn, p_cfg2_fn, l, cpu))
            {
                free(p_rom1_fn);
                if (rom1_fn) free(rom1_fn);
                if (bin1_fn) free(bin1_fn);
                if (cfg1_fn) free(cfg1_fn);
                if (bin2_fn) free(bin2_fn); /*also frees cfg2/int2/itv2/rom2*/
                return NULL;
            }

            goto finish;
        }

        if (int2_fn && file_exists(p_int2_fn))
        {
            legacy_print_loading(p_int2_fn, p_cfg2_fn);
            fn = strdup(p_int2_fn);

            if (legacy_read_bincfg(p_int2_fn, p_cfg2_fn, l, cpu))
            {
                free(p_rom1_fn);
                if (rom1_fn) free(rom1_fn);
                if (bin1_fn) free(bin1_fn);
                if (cfg1_fn) free(cfg1_fn);
                if (bin2_fn) free(bin2_fn); /*also frees cfg2/int2/itv2/rom2*/
                return NULL;
            }
            goto finish;
        }
        
        if (itv2_fn && file_exists(p_itv2_fn))
        {
            legacy_print_loading(p_itv2_fn, p_cfg2_fn);
            fn = strdup(p_itv2_fn);

            if (legacy_read_bincfg(p_itv2_fn, p_cfg2_fn, l, cpu))
            {
                free(p_rom1_fn);
                if (rom1_fn) free(rom1_fn);
                if (bin1_fn) free(bin1_fn);
                if (cfg1_fn) free(cfg1_fn);
                if (bin2_fn) free(bin2_fn); /*also frees cfg2/int2/itv2/rom2*/
                return NULL;
            }

            goto finish;
        }

        path = path->next;
    }

    fprintf(stderr, "\nCould not find any of these candidate files:\n");
    if (rom1_fn) fprintf(stderr, "  %s\n", rom1_fn);
    if (bin1_fn) fprintf(stderr, "  %s\n", bin1_fn);
    if (rom2_fn) fprintf(stderr, "  %s\n", rom2_fn);
    if (rom3_fn) fprintf(stderr, "  %s\n", rom3_fn);
    if (bin2_fn) fprintf(stderr, "  %s\n", bin2_fn);
    if (int2_fn) fprintf(stderr, "  %s\n", int2_fn);
    if (itv2_fn) fprintf(stderr, "  %s\n", itv2_fn);

    free(p_rom1_fn);
    if (rom1_fn) free(rom1_fn);
    if (bin1_fn) free(bin1_fn);
    if (cfg1_fn) free(cfg1_fn);
    if (bin2_fn) free(bin2_fn); /*also frees cfg2/int2/itv2/rom2*/

    dump_search_path(tmp);

    return NULL;

finish:
    *legacy_rom = 1;
    free(p_rom1_fn);
    if (rom1_fn) free(rom1_fn);
    if (bin1_fn) free(bin1_fn);
    if (cfg1_fn) free(cfg1_fn);
    if (bin2_fn) free(bin2_fn); /*also frees cfg2/int2/itv2/rom2*/

    /* -------------------------------------------------------------------- */
    /*  Set up peripheral function pointers to support reads of the right   */
    /*  width.  Ignore writes and explicitly disallow ticks.                */
    /* -------------------------------------------------------------------- */
    l->periph.read        = legacy_read;
    l->periph.write       = legacy_write;
    l->periph.peek        = legacy_read;
    l->periph.poke        = legacy_poke;
    l->periph.dtor        = legacy_dtor;

    l->periph.tick        = NULL;
    l->periph.min_tick    = ~0U;
    l->periph.max_tick    = ~0U;
    l->periph.addr_base   = 0;
    l->periph.addr_mask   = 0xFFFF;

    return fn;
}

/* ======================================================================== */
/*  LEGACY_REGISTER -- Actually registers the legacy ROMs.  Also frees      */
/*                     the saved bc_cfgfile_t.                              */
/* ======================================================================== */
int legacy_register
(
    legacy_t    *l,
    periph_bus_p bus,
    cp1600_t    *cp
)
{
    bc_cfgfile_t *bc = l->bc;
    bc_memspan_t *span;
    int num_ecs = 0;
    int addr, need_register;

    /* -------------------------------------------------------------------- */
    /*  Register the legacy ROM.  Eventually, we might 'optimize' this as   */
    /*  I did for the Intellicart, where I have multiple periph_t's based   */
    /*  on what flags are set for each range.                               */
    /* -------------------------------------------------------------------- */
    for (span = bc->span; span; span = (bc_memspan_t *)span->l.next)
    {
        int slen;

        slen = span->e_addr - span->s_addr + 1;

        /* ---------------------------------------------------------------- */
        /*  Handle ECS-style pages differently.                             */
        /* ---------------------------------------------------------------- */
        if ( span->epage != BC_SPAN_NOPAGE ||
            (span->flags & BC_SPAN_EP) != 0)
        {
            char name[17];

            snprintf(name, 17, "Paged ROM %d", num_ecs);
            periph_register(bus, &(l->pg_rom[num_ecs].periph), 
                            span->s_addr, span->e_addr, name);

            cp1600_cacheable(cp, span->s_addr, span->e_addr,
                             (span->flags & BC_SPAN_W) != 0);
            num_ecs++;
            continue;
        }

        /* ---------------------------------------------------------------- */
        /*  Handle other segments directly.  If all the addresses in a      */
        /*  span are already mapped, skip this span.  We could/should       */
        /*  optimize this at some point to only register areas not yet      */
        /*  mapped, but I'm lazy right now.  The optimization would rarely  */
        /*  if ever kick in.                                                */
        /* ---------------------------------------------------------------- */
        need_register = 0;
        for (addr = span->s_addr; addr <= span->e_addr; addr++)
        {
            if (!(l->loc[addr].flags & LOC_MAPPED))
                need_register = 1;

            l->loc[addr].flags |= LOC_MAPPED;
        }

        if (need_register)
            periph_register(bus, &l->periph, span->s_addr, span->e_addr, 
                            span->flags & BC_SPAN_PK ? "[macro poke]"
                                                     : "Legacy BIN+CFG");
        
        /* ---------------------------------------------------------------- */
        /*  Mark the span cacheable, with snoop if necessary.               */
        /* ---------------------------------------------------------------- */
        cp1600_cacheable(cp, span->s_addr, span->e_addr,
                         (span->flags & BC_SPAN_W) != 0);
    }

#ifndef BC_NOFREE
    /* -------------------------------------------------------------------- */
    /*  Discard the parsed config.                                          */
    /* -------------------------------------------------------------------- */
    bc_free_cfg(l->bc);
    l->bc = NULL;
#endif

    assert(num_ecs == l->npg_rom);

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
/* ------------------------------------------------------------------------ */
/*           Copyright (c) 2003-+Inf, Joseph Zbiciak, John Tanner           */
/* ======================================================================== */
