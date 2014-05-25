/* ======================================================================== */
/*  Routines for reading/writing a .BIN+.CFG to/from an icartrom_t.         */
/* ------------------------------------------------------------------------ */
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
/*                 Copyright (c) 1998-2001, Joseph Zbiciak                  */
/* ======================================================================== */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "config.h"
#include "icart/icartrom.h"
#include "icart/icartbin.h"
#include "bincfg/bincfg.h"


/* ======================================================================== */
/*  ICB_SHOW_RANGES                                                         */
/*  Shows a list of ranges of addresses represented by a bit-vector.        */
/* ======================================================================== */
void icb_show_ranges(uint_32 *bv)
{
    int lo, hi, i;

    /* -------------------------------------------------------------------- */
    /*  Iterate over all 256 256-decle pages, with a little slop at both    */
    /*  ends of the spectrum.  Look for spans of set bits.                  */
    /* -------------------------------------------------------------------- */
    for (i = 0, lo = hi = -1; i <= 256; i++)
    {
        int idx, shf;

        idx = i >> 5;
        shf = i & 31;
        if (i < 256 && (1 & (bv[idx] >> shf)))
        {
            hi = i;
            if (lo == -1) { lo = i; }
        } else
        {
            if (lo != -1)
            {
                printf("    $%.4X - $%.4X (%d pages)\n",
                        lo << 8, (hi << 8) + 0xFF, (hi - lo + 1));
            }
            hi = lo = -1;
        }
    }
}

/* ======================================================================== */
/*  ICB_WRITE_MAPPINGS                                                      */
/*  Writes the [mappings] section of a .CFG file, based on the icartrom.    */
/* ======================================================================== */
int icb_write_mappings(FILE *fb, FILE *fc, icartrom_t *icart, int ofs)
{
    int lo, hi, i, j;

    /* -------------------------------------------------------------------- */
    /*  Make sure at least one page is both 'preload' and 'readable'.       */
    /*  If there are none, then don't output a [mapping] section.           */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 8; i++)
        if (icart->preload[i] & icart->readable[i])
            break;
    if (i == 8)
        return 0;

    fprintf(fc, "[mapping]\r\n");

    /* -------------------------------------------------------------------- */
    /*  Iterate over all 256 256-decle pages, with a little slop at both    */
    /*  ends of the spectrum.  Look for spans of pages that are both        */
    /*  readable and preloaded.                                             */
    /* -------------------------------------------------------------------- */
    for (i = 0, lo = hi = -1; i <= 256; i++)
    {
        int idx, shf;

        idx = i >> 5;
        shf = i & 31;
        if (i < 256 && 
            (1 & ((icart->preload [idx] & 
                   icart->readable[idx]) >> shf)))
        {
            hi = i;
            if (lo == -1) { lo = i; }
        } else
        {
            if (lo != -1)
            {
                lo <<= 8;
                hi = (hi << 8) + 0x100;
                fprintf(fc, "$%.4X - $%.4X = $%.4X\r\n", 
                        ofs, ofs + hi - lo - 1, lo);
                for (j = lo; j < hi; j++)
                {
                    fputc(icart->image[j] >> 8,   fb);
                    fputc(icart->image[j] & 0xFF, fb);
                }
                ofs += hi - lo;
            }
            hi = lo = -1;
        }
    }

    return ofs;
}

/* ======================================================================== */
/*  ICB_WRITE_PRELOADS                                                      */
/*  Writes the [preload] section of a .CFG, which addresses ranges of       */
/*  address that are preloaded, but not readable.                           */
/* ======================================================================== */
int icb_write_preloads(FILE *fb, FILE *fc, icartrom_t *icart, int ofs)
{
    int lo, hi, i, j;

    /* -------------------------------------------------------------------- */
    /*  Make sure at least one page is both 'preload' and 'not readable'.   */
    /*  If there are none, then don't output a [preload] section.           */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 8; i++)
        if (icart->preload[i] & ~icart->readable[i])
            break;
    if (i == 8)
        return 0;

    fprintf(fc, "[preload]\r\n");

    /* -------------------------------------------------------------------- */
    /*  Iterate over all 256 256-decle pages, with a little slop at both    */
    /*  ends of the spectrum.  Look for spans of pages that are both        */
    /*  preloaded and not-readable.                                         */
    /* -------------------------------------------------------------------- */
    for (i = 0, lo = hi = -1; i <= 256; i++)
    {
        int idx, shf;

        idx = i >> 5;
        shf = i & 31;
        if (i < 256 && 
            (1 & (( icart->preload [idx] & 
                   ~icart->readable[idx]) >> shf)))
        {
            hi = i;
            if (lo == -1) { lo = i; }
        } else
        {
            if (lo != -1)
            {
                lo <<= 8;
                hi = (hi << 8) + 0x100;
                fprintf(fc, "$%.4X - $%.4X = $%.4X\r\n", 
                        ofs, ofs + hi - lo - 1, lo);
                for (j = lo; j < hi; j++)
                {
                    fputc(icart->image[j] >> 8,   fb);
                    fputc(icart->image[j] & 0xFF, fb);
                }
                ofs += hi - lo;
            }
            hi = lo = -1;
        }
    }
    return ofs;
}

/* ======================================================================== */
/*  ICB_WRITE_BANKSW                                                        */
/*  Writes the [bankswitch] section.  These are sections marked for         */
/*  Intellicart-style bankswitching.                                        */
/* ======================================================================== */
void icb_write_banksw(FILE *fc, icartrom_t *icart)
{
    int lo, hi, i;

    /* -------------------------------------------------------------------- */
    /*  Make sure at least one page is 'bankswitched'.                      */
    /*  If there are none, then don't output a [bankswitch] section.        */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 8; i++)
        if (icart->dobanksw[i])
            break;
    if (i == 8)
        return;

    /* -------------------------------------------------------------------- */
    /*  Iterate over all 256 256-decle pages, with a little slop at both    */
    /*  ends of the spectrum.  Look for spans of pages that are banksw.     */
    /* -------------------------------------------------------------------- */
    fprintf(fc, "[bankswitch]\r\n");
    for (i = 0, lo = hi = -1; i <= 256; i++)
    {
        int idx, shf;

        idx = i >> 5;
        shf = i & 31;
        if (i < 256 && (1 & (icart->dobanksw[idx] >> shf)))
        {
            hi = i;
            if (lo == -1) { lo = i; }
        } else
        {
            if (lo != -1)
            {
                fprintf(fc, "$%.4X - $%.4X\r\n",
                        lo << 8, (hi << 8) + 0xFF);
            }
            hi = lo = -1;
        }
    }
}

/* ======================================================================== */
/*  ICB_WRITE_MEMATTR                                                       */
/*  Writes the [memattr] section.  These are sections marked as RAM.        */
/* ======================================================================== */
void icb_write_memattr(FILE *fc, icartrom_t *icart)
{
    int lo, hi, i;

    /* -------------------------------------------------------------------- */
    /*  Make sure at least one page is 'writable.'                          */
    /*  If there are none, then don't output a [memattr] section.           */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 8; i++)
        if (icart->writable[i])
            break;
    if (i == 8)
        return;

    fprintf(fc, "[memattr]\r\n");
    /* -------------------------------------------------------------------- */
    /*  Iterate over all 256 256-decle pages, with a little slop at both    */
    /*  ends of the spectrum.  Look for spans of pages that are writable    */
    /*  but not narrow.  These are RAM 16 spans.                            */
    /* -------------------------------------------------------------------- */
    for (i = 0, lo = hi = -1; i <= 256; i++)
    {
        int idx, shf;

        idx = i >> 5;
        shf = i & 31;
        if (i < 256 && 
            (1 & (( icart->writable [idx] & 
                   ~icart->narrow   [idx]) >> shf)))
        {
            hi = i;
            if (lo == -1) { lo = i; }
        } else
        {
            if (lo != -1)
            {
                fprintf(fc, "$%.4X - $%.4X = RAM 16\r\n",
                        lo << 8, (hi << 8) + 0xFF);
            }
            hi = lo = -1;
        }
    }
    /* -------------------------------------------------------------------- */
    /*  Iterate over all 256 256-decle pages, with a little slop at both    */
    /*  ends of the spectrum.  Look for spans of pages that are writable    */
    /*  and also narrow.  These are RAM 8 spans.                            */
    /* -------------------------------------------------------------------- */
    for (i = 0, lo = hi = -1; i <= 256; i++)
    {
        int idx, shf;

        idx = i >> 5;
        shf = i & 31;
        if (i < 256 && 
            (1 & (( icart->writable [idx] & 
                    icart->narrow   [idx]) >> shf)))
        {
            hi = i;
            if (lo == -1) { lo = i; }
        } else
        {
            if (lo != -1)
            {
                fprintf(fc, "$%.4X - $%.4X = RAM 8\r\n",
                        lo << 8, (hi << 8) + 0xFF);
            }
            hi = lo = -1;
        }
    }
}

/* ======================================================================== */
/*  ICB_WRITE_BINCFG                                                        */
/*  Write out an entire BIN+CFG.                                            */
/* ======================================================================== */
int icb_write_bincfg(FILE *fb, FILE *fc, icartrom_t *icart, int ofs)
{
    ofs = icb_write_mappings(fb, fc, icart, ofs);
    ofs = icb_write_preloads(fb, fc, icart, ofs);
    icb_write_memattr(fc, icart);
    icb_write_banksw (fc, icart);

    return ofs;
}
int icb_writefile(char *bin_fn, char *cfg_fn, icartrom_t *icart)
{
    FILE *fb, *fc;
    int ofs;

    if (!(fb = fopen(bin_fn, "wb")))
    {
        perror("fopen():");
        fprintf(stderr, "ERROR:  Cannot open '%s' for writing\n", bin_fn);
        exit(1);
    }
    if (!(fc = fopen(cfg_fn, "wb")))
    {
        perror("fopen():");
        fprintf(stderr, "ERROR:  Cannot open '%s' for writing\n", cfg_fn);
        exit(1);
    }
    ofs = icb_write_bincfg(fb, fc, icart, 0);

    fclose(fc);
    fclose(fb);

    return ofs;
}

/* ======================================================================== */
/*  ICB_SHOW_SUMMARY                                                        */
/*  Show a bunch of human-readable info about an icartrom.                  */
/* ======================================================================== */
void icb_show_summary(icartrom_t *icart)
{
    printf("Preloaded memory ranges:\n");
    icb_show_ranges(icart->preload);

    printf("Readable memory ranges:\n");
    icb_show_ranges(icart->readable);

    printf("Writeable memory ranges:\n");
    icb_show_ranges(icart->writable);

    printf("Narrow (8-bit wide) memory ranges:\n");
    icb_show_ranges(icart->narrow);

    printf("Bank-switched memory ranges:\n");
    icb_show_ranges(icart->dobanksw);
}


/* ======================================================================== */
/*  APPLY_CFG -- Reads through a parsed config and generates an icartrom_t  */
/*               from it.   Expects all macros and data segments are        */
/*               already attached to it.                                    */
/* ======================================================================== */
static void icb_apply_cfg(bc_cfgfile_t *bc, icartrom_t *icart, int loud)
{
    uint_32 ic_flags;
    bc_memspan_t *span;


    /* -------------------------------------------------------------------- */
    /*  Traverse the memspan list, calling icartrom_addseg on each.         */
    /* -------------------------------------------------------------------- */
    for (span = bc->span; span; span = (bc_memspan_t *)span->l.next)
    {
        int slen;

        slen = span->e_addr - span->s_addr + 1;

        /* ---------------------------------------------------------------- */
        /*  If this span has an ECS page associated with it, warn that we   */
        /*  ignore it.                                                      */
        /* ---------------------------------------------------------------- */
        if ( span->epage != BC_SPAN_NOPAGE ||
            (span->flags & BC_SPAN_EP) != 0)
        {
            if (loud)
            {
                printf("IGNORING ECS PAGE %d for span at ofs %.4X, len %.4X\n",
                        span->epage, span->s_fofs,  slen);
            }
            continue;
        }

        /* ---------------------------------------------------------------- */
        /*  Translate the BC flags to ICART_ROM flags.  They're closely     */
        /*  related, but not 100% identical.                                */
        /* ---------------------------------------------------------------- */
        ic_flags = ((span->flags & BC_SPAN_R) ? ICARTROM_READ    : 0) |
                   ((span->flags & BC_SPAN_W) ? ICARTROM_WRITE   : 0) |
                   ((span->flags & BC_SPAN_B) ? ICARTROM_BANKSW  : 0) |
                   ((span->flags & BC_SPAN_N) ? ICARTROM_NARROW  : 0) |
   ((span->flags & (BC_SPAN_PL | BC_SPAN_PK)) ? ICARTROM_PRELOAD : 0);
        

        if (loud)
        {
            printf("SEGMENT ofs %.4X  len %.4X  addr %.4X  "
                    "FLAGS: %c%c%c%c%c\n", 
                   span->s_fofs, slen, span->s_addr,
                   ic_flags & ICARTROM_READ    ? 'R' : '-',
                   ic_flags & ICARTROM_WRITE   ? 'W' : '-',
                   ic_flags & ICARTROM_BANKSW  ? 'B' : '-',
                   ic_flags & ICARTROM_NARROW  ? 'N' : '-',
                   ic_flags & ICARTROM_PRELOAD ? 'P' : '-');
            fflush(stdout);
        }

        /* ---------------------------------------------------------------- */
        /*  Assertion:  If PRELOAD, then span->data.                        */
        /* ---------------------------------------------------------------- */
        assert(((ic_flags & ICARTROM_PRELOAD) == 0) || (span->data != 0));

        /* ---------------------------------------------------------------- */
        /*  Add the segment to the icartrom.                                */
        /* ---------------------------------------------------------------- */
        icartrom_addseg(icart, span->data, span->s_addr, slen, ic_flags, 0);
    }
}

/* ======================================================================== */
/*  ICB_READ_BINCFG -- Reads a .BIN and optional .CFG file.                 */
/* ======================================================================== */
void icb_read_bincfg(char *bin_fn, char *cfg_fn, icartrom_t *the_icart)
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

#ifndef BC_NODOMACRO
    /* -------------------------------------------------------------------- */
    /*  Apply any statically safe macros.                                   */
    /* -------------------------------------------------------------------- */
    bc_do_macros(bc, 0);
#endif

    /* -------------------------------------------------------------------- */
    /*  Populate the config with corresponding BIN data.                    */
    /* -------------------------------------------------------------------- */
    if (bc_read_data(bc))
    {
        fprintf(stderr, "Error reading data for CFG file.\n");
        exit(1);
    }

    /* -------------------------------------------------------------------- */
    /*  Apply the configuration.  This generates the icartrom.              */
    /* -------------------------------------------------------------------- */
    icb_apply_cfg(bc, the_icart, 1);

#ifndef BC_NOFREE
    /* -------------------------------------------------------------------- */
    /*  Discard the parsed config.                                          */
    /* -------------------------------------------------------------------- */
    bc_free_cfg(bc);
#endif
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
/*                 Copyright (c) 1998-2001, Joseph Zbiciak                  */
/* ======================================================================== */
