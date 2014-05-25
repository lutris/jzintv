/* ======================================================================== */
/*  Title:    Intellicart Emulation                                         */
/*  Author:   J. Zbiciak                                                    */
/*  $Id: icart.c,v 1.2 2001/02/02 04:16:49 im14u2c Exp $  */
/* ------------------------------------------------------------------------ */
/*  This module implements Intellicart Emulation.  We use this emulation    */
/*  to emulate ALL Intellivision cartridges because it's much more          */
/*  convenient that way.                                                    */
/* ======================================================================== */

#include <assert.h>
#include "../config.h"
#include "periph/periph.h"
#include "cp1600/cp1600.h"
#include "icart/icart.h"

/* ======================================================================== */
/*  ICART_CALC_BS    -- Calculate an address for a bank-switched address.   */
/* ======================================================================== */
LOCAL INLINE uint_32 icart_calc_bs(icart_t *ic, uint_32 addr)
{
    return 0xFFFF & (addr + ic->bs_tbl[addr >> 11]);
}

/* ======================================================================== */
/*  ICART_RD_F       -- Read flat 16-bit Intellicart memory.                */
/*  ICART_RD_FN      -- Read flat 8-bit Intellicart memory.                 */
/*  ICART_RD_B       -- Read bank-switched 16-bit Intellicart memory.       */
/*  ICART_RD_BN      -- Read bank-switched 8-bit Intellicart memory.        */
/* ======================================================================== */

uint_32 icart_rd_NULL(periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    UNUSED(per); UNUSED(ign); UNUSED(addr); UNUSED(data);
    return ~0U;
}
uint_32 icart_rd_f   (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    icart_t *ic = (icart_t *)(per->parent);
    UNUSED(ign); UNUSED(data);
    return ic->rom.image[addr];
}
uint_32 icart_rd_fn  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    icart_t *ic = (icart_t *)(per->parent);
    UNUSED(ign); UNUSED(data);
    return ic->rom.image[addr] & 0xFF;
}
uint_32 icart_rd_b   (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    icart_t *ic = (icart_t *)(per->parent);
    UNUSED(ign); UNUSED(data);
    return ic->rom.image[icart_calc_bs(ic, addr)];
}
uint_32 icart_rd_bn  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    icart_t *ic = (icart_t *)(per->parent);
    UNUSED(ign); UNUSED(data);
    return ic->rom.image[icart_calc_bs(ic, addr)] & 0xFF;
}

/* ======================================================================== */
/*  ICART_WR_F       -- Write flat 16-bit Intellicart memory.               */
/*  ICART_WR_FN      -- Write flat 8-bit Intellicart memory.                */
/*  ICART_WR_B       -- Write bank-switched 16-bit Intellicart memory.      */
/*  ICART_WR_BN      -- Write bank-switched 8-bit Intellicart memory.       */
/*  ICART_WR_BI      -- Write bank-switch 16-bit I-cart mem, w/ invalidate  */
/*  ICART_WR_BNI     -- Write bank-switch 8-bit I-cart mem, w/ invalidate   */
/* ======================================================================== */

void    icart_wr_NULL(periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    UNUSED(per); UNUSED(ign); UNUSED(addr); UNUSED(data);
}
void    icart_wr_f   (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    icart_t *ic = (icart_t *)(per->parent);
    UNUSED(ign);
    ic->rom.image[addr] = data;
}
void    icart_wr_fn  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    icart_t *ic = (icart_t *)(per->parent);
    UNUSED(ign);
    ic->rom.image[addr] = data & 0xFF;
}
void    icart_wr_b   (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    icart_t *ic = (icart_t *)(per->parent);
    UNUSED(ign);
    ic->rom.image[icart_calc_bs(ic, addr)] = data;
}
void    icart_wr_bn  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    icart_t *ic = (icart_t *)(per->parent);
    UNUSED(ign);
    ic->rom.image[addr] = data & 0xFF;
}

/* ======================================================================== */
/*  ICART_RD_BS      -- Read from bankswitch registers.                     */
/*  ICART_WR_BS      -- Write to bankswitch registers.                      */
/* ======================================================================== */
uint_32 icart_rd_bs  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    UNUSED(per); UNUSED(ign); UNUSED(addr); UNUSED(data);
    return ~0U;
}

void    icart_wr_bs  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    icart_t *ic = (icart_t *)(per->parent);
    int b_idx, b_shf, t_idx;

    UNUSED(ign);

    addr = ((addr & 0xF) << 12) | ((addr & 0x10) << 7);

    b_idx =  addr >> 13;
    b_shf = (addr >> 8) & 31;
    if (((ic->rom.dobanksw[b_idx] >> b_shf) & 1) == 0) 
        return;

    t_idx = addr >> 11;
    data  = ((data << 8) - (addr & ~0x0700)) & 0xFFFF;

    ic->bs_tbl[t_idx] = data;

    if (ic->cache_bs && ((ic->rom.readable[b_idx] >> b_shf) & 1) == 1) 
    {
        cp1600_invalidate(ic->cpu, addr, addr + 0x00FF);
    }
}

/* ======================================================================== */
/*  ROM_SIZE     -- Returns size of the *body* of a .ROM file, ignoring     */
/*                  any tags that might be appended to it.                  */
/* ======================================================================== */
LOCAL long rom_size(FILE *rom_image)
{
    int  num_segments, i, seg_lo, seg_hi;
    long end_of_file, tag_offset;
    uint_8 hdr;

    /* -------------------------------------------------------------------- */
    /*  First, find the filesize by seeking to end-of-file.                 */
    /* -------------------------------------------------------------------- */
    fseek(rom_image, 0, SEEK_END);
    end_of_file = ftell(rom_image);
    rewind(rom_image);

    /* -------------------------------------------------------------------- */
    /*  Now start parsing the .ROM file.                                    */
    /* -------------------------------------------------------------------- */
    hdr = fgetc(rom_image);
    if (hdr != 0xA8 && hdr != 0x61 && hdr != 0x41) 
        return -1;  /* not a .ROM file. */

    /* -------------------------------------------------------------------- */
    /*  Get the number of ROM segments and sanity-check it.                 */
    /* -------------------------------------------------------------------- */
    num_segments = fgetc(rom_image);

    if (num_segments < 1 || num_segments > 32)
         return -1;  /* Invalid # of ROM segments. */

    if (num_segments != (0xFF ^ fgetc(rom_image)))
         return -1;  /* Header consistency check failed. */

    /* -------------------------------------------------------------------- */
    /*  Skip over the the ROM segments.                                     */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < num_segments; i++)
    {
        /* ---------------------------------------------------------------- */
        /*  Read the memory range of this ROM segment and apply a           */
        /*  simple sanity check to it.                                      */
        /* ---------------------------------------------------------------- */
        seg_lo = fgetc(rom_image);
        seg_hi = fgetc(rom_image) + 1;

        if (seg_lo >= seg_hi)
             return -1;  /* Address range is backwards! */

        /* ---------------------------------------------------------------- */
        /*  The segment range is in terms of 256-word pages.  Each          */
        /*  256-word page is 512 bytes.  Skip over the segment and          */
        /*  CRC-16.                                                         */
        /* ---------------------------------------------------------------- */
        if (fseek(rom_image, (seg_hi - seg_lo) * 512 + 2, SEEK_CUR) < 0 ||
            ftell(rom_image) > end_of_file - 50)
            return -1;   /* Bad ROM segment or truncated file. */
    }

    /* -------------------------------------------------------------------- */
    /*  Now skip over the enable tables.  These have a well-defined         */
    /*  and fixed size.                                                     */
    /* -------------------------------------------------------------------- */
    fseek(rom_image, 50, SEEK_CUR);

    /* -------------------------------------------------------------------- */
    /* Let's see where we ended up in the file.                             */
    /* -------------------------------------------------------------------- */
    tag_offset = ftell(rom_image);

    /* -------------------------------------------------------------------- */
    /*  Return the location of end-of-file if no tags are found, or the     */
    /*  offset where tags begin if there are some.                          */
    /* -------------------------------------------------------------------- */
    return tag_offset < end_of_file ? tag_offset : end_of_file;
}


/* ======================================================================== */
/*  These handy tables simplify initializing the Intellicart.               */
/* ======================================================================== */
typedef struct 
{
    periph_rd_t read;
    periph_wr_t write;
    periph_rd_t peek;
    periph_wr_t poke;
} ic_init_t;

LOCAL ic_init_t ic_init[] =
{
    {   icart_rd_f,     icart_wr_NULL,  icart_rd_f,     icart_wr_f      },
    {   icart_rd_NULL,  icart_wr_f,     icart_rd_f,     icart_wr_f      },
    {   icart_rd_f,     icart_wr_f,     icart_rd_f,     icart_wr_f      },

    {   icart_rd_fn,    icart_wr_NULL,  icart_rd_fn,    icart_wr_fn     },
    {   icart_rd_NULL,  icart_wr_fn,    icart_rd_fn,    icart_wr_fn     },
    {   icart_rd_fn,    icart_wr_fn,    icart_rd_fn,    icart_wr_fn     },

    {   icart_rd_b,     icart_wr_NULL,  icart_rd_b,     icart_wr_b      },
    {   icart_rd_NULL,  icart_wr_b,     icart_rd_b,     icart_wr_b      },
    {   icart_rd_b,     icart_wr_b,     icart_rd_b,     icart_wr_b      },

    {   icart_rd_bn,    icart_wr_NULL,  icart_rd_bn,    icart_wr_bn     },
    {   icart_rd_NULL,  icart_wr_bn,    icart_rd_bn,    icart_wr_bn     },
    {   icart_rd_bn,    icart_wr_bn,    icart_rd_bn,    icart_wr_bn     },
};

/* Legend: R == Readable, W == Writable, N == Narrow, B == Bankswitchable */

LOCAL int ic_attr_map[16] =
{
/*       R       R         */
/*           W   W         */
    -1,  0,  1,  2, /*     */
    -1,  3,  4,  5, /* N   */
    -1,  6,  7,  8, /*   B */
    -1,  9, 10, 11, /* N B */
};


/* ======================================================================== */
/*  ICART_INIT       -- Initialize the Intellicart w/ a ROM image.          */
/* ======================================================================== */
int icart_init
(
    icart_t     *ic,
    FILE        *rom,
    long        *tag_ofs
)
{
    uint_8      *rom_img;
    periph_p    p[12];
    long        size;
    int         err;
    int         i;

    /* -------------------------------------------------------------------- */
    /*  Sanity checks.                                                      */
    /* -------------------------------------------------------------------- */
    if (!ic || !rom)
        return -1;

    /* -------------------------------------------------------------------- */
    /*  Clean things up.                                                    */
    /* -------------------------------------------------------------------- */
    memset(ic, 0, sizeof(icart_t));

    /* -------------------------------------------------------------------- */
    /*  The Intellicart offers multiple "periph" interfaces, each tuned     */
    /*  for a different type of memory.  This is done for performance.      */
    /*                                                                      */
    /*                  Read        Write       8-bit       Bankswitch      */
    /*      ic->r       Yes         No          No          No              */
    /*      ic->w       No          Yes         No          No              */
    /*      ic->rw      Yes         Yes         No          No              */
    /*      ic->rn      Yes         No          Yes         No              */
    /*      ic->wn      No          Yes         Yes         No              */
    /*      ic->rwn     Yes         Yes         Yes         No              */
    /*      ic->rb      Yes         No          No          Yes             */
    /*      ic->wb      No          Yes         No          Yes             */
    /*      ic->rwb     Yes         Yes         No          Yes             */
    /*      ic->rnb     Yes         No          Yes         Yes             */
    /*      ic->wnb     No          Yes         Yes         Yes             */
    /*      ic->rwnb    Yes         Yes         Yes         Yes             */
    /* -------------------------------------------------------------------- */
    p[ 0] = &(ic->r);               p[ 6] = &(ic->rb);  
    p[ 1] = &(ic->w);               p[ 7] = &(ic->wb);  
    p[ 2] = &(ic->rw);              p[ 8] = &(ic->rwb); 
    p[ 3] = &(ic->rn);              p[ 9] = &(ic->rnb); 
    p[ 4] = &(ic->wn);              p[10] = &(ic->wnb); 
    p[ 5] = &(ic->rwn);             p[11] = &(ic->rwnb);

    for (i = 0; i < 12; i++)
    {
        p[i]->read      = ic_init[i].read;
        p[i]->write     = ic_init[i].write;
        p[i]->peek      = ic_init[i].peek;
        p[i]->poke      = ic_init[i].poke;
        p[i]->tick      = NULL;
        p[i]->min_tick  = ~0U;
        p[i]->max_tick  = ~0U;
        p[i]->addr_base = 0x000;
        p[i]->addr_mask = 0xFFFF;
        p[i]->parent    = (void*)ic;
    }

    /* -------------------------------------------------------------------- */
    /*  Now, read in the .ROM file.                                         */
    /* -------------------------------------------------------------------- */
    size = rom_size(rom);
    if (size <= 52)
    {
        fprintf(stderr, "icart:  Short file?\n");
        return -1;      /* Short file, or seek failed */
    }

    if ((rom_img = CALLOC(uint_8, size)) == NULL)
    {
        fprintf(stderr, "icart:  Out of memory.\n");
        return -1;
    }

    rewind(rom);
    if ((long)fread(rom_img, 1, size, rom) != size)
    {
        fprintf(stderr, "icart:  Short read while reading ROM.\n");
        return -1;
    }

    if ((err = icartrom_decode(&(ic->rom), rom_img, 0)) < 0)
    {
        fprintf(stderr, "icart:  Error %d while decoding ROM.\n", -err);
        return -1;
    }
    free(rom_img);

    if (tag_ofs) *tag_ofs = size;

    return 0;
}

/* ======================================================================== */
/*  ICART_REGISTER   -- The Intellicart is unique in that it will register  */
/*                      itself on the peripheral bus.                       */
/* ======================================================================== */
int icart_register
(
    icart_t     *ic,
    periph_bus_p bus,
    cp1600_t    *cpu,
    uint_32     cache_flags
)
{
    int         attr, p_attr;
    int         i, idx, shf;
    char        buf[17];
    int         has_banksw = 0;
    uint_32     addr_lo, addr_hi;
    periph_p    p[12];

    /* -------------------------------------------------------------------- */
    /*  Sanity checks.                                                      */
    /* -------------------------------------------------------------------- */
    if (!ic || !bus)
        return -1;

    /* -------------------------------------------------------------------- */
    /*  See table above in icart_init for explanation.                      */
    /* -------------------------------------------------------------------- */
    p[ 0] = &(ic->r);               p[ 6] = &(ic->rb);  
    p[ 1] = &(ic->w);               p[ 7] = &(ic->wb);  
    p[ 2] = &(ic->rw);              p[ 8] = &(ic->rwb); 
    p[ 3] = &(ic->rn);              p[ 9] = &(ic->rnb); 
    p[ 4] = &(ic->wn);              p[10] = &(ic->wnb); 
    p[ 5] = &(ic->rwn);             p[11] = &(ic->rwnb);

    /* -------------------------------------------------------------------- */
    /*  Register ourself on the peripheral bus.  Also, set up the CPU       */
    /*  cacheability of each range we register.                             */
    /* -------------------------------------------------------------------- */
    has_banksw = 0;
    ic->cpu = cpu;
    p_attr  = -1;
    addr_lo = addr_hi = -1;
    for (i = 0; i <= 256; i++)
    {
        idx  = i >> 5;
        shf  = i & 31;

        attr = i >= 256 ? -1 
                        : (int)((1 & ((ic->rom.readable[idx] >> shf) << 0)) |
                                (2 & ((ic->rom.writable[idx] >> shf) << 1)) |
                                (4 & ((ic->rom.narrow  [idx] >> shf) << 2)) |
                                (8 & ((ic->rom.dobanksw[idx] >> shf) << 3)));
        if (attr != p_attr)
        {
            if (p_attr > 0 && ic_attr_map[p_attr] >= 0)
            {
                snprintf(buf, sizeof(buf),
                        "ICart   [%c%c%c%c]",
                        p_attr & 1 ? 'R' : ' ',
                        p_attr & 2 ? 'W' : ' ',
                        p_attr & 4 ? 'N' : ' ',
                        p_attr & 8 ? 'B' : ' ');

                periph_register(bus, p[ic_attr_map[p_attr]], 
                                addr_lo, addr_hi, buf);
                if (p_attr & ICARTROM_BANKSW)
                    has_banksw = 1;

                if (cache_flags & (1 << p_attr))
                {
                    int snoop;

                    snoop = (cache_flags >> (p_attr + 16)) & 1;
                    cp1600_cacheable(cpu, addr_lo, addr_hi, snoop);
                }
            }
            addr_lo = i << 8;
            addr_hi = addr_lo + 0xFF;
            p_attr  = attr;
        } else
        {
            addr_hi = (i << 8) + 0xFF;
        }
    }   

    /* -------------------------------------------------------------------- */
    /*  If this cartridge image uses bankswitching, register the control    */
    /*  registers that handle bankswitching.                                */
    /* -------------------------------------------------------------------- */
    if (has_banksw)
    {
        if (cache_flags & (IC_C_B__R | IC_C_B_WR | IC_C_BN_R | IC_C_BNWR))
            ic->cache_bs = 1;

        ic->bs.read      = icart_rd_bs;
        ic->bs.write     = icart_wr_bs;
        ic->bs.peek      = icart_rd_bs;
        ic->bs.poke      = icart_wr_bs;
        ic->bs.tick      = 0;
        ic->bs.max_tick  = ~0U;
        ic->bs.min_tick  = ~0U;
        ic->bs.addr_base = 0x40;
        ic->bs.addr_mask = 0x1F;
        ic->bs.parent    = ic;
        ic->bs.dtor      = NULL;

        periph_register(bus, &(ic->bs), 0x40, 0x5F, "ICart BankSw");
    }

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
/*                 Copyright (c) 2001-+Inf, Joseph Zbiciak                  */
/* ======================================================================== */
