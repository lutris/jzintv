/* ======================================================================== */
/*  INTELLICART ROM manipulation routines.              J. Zbiciak, 2001    */
/*                                                                          */
/*  These routines are intended for reading and writing Intellicart ROM     */
/*  images.  Portions of this code are based on Chad Schell's own routines  */
/*  for generating Intellicart ROM images.                                  */
/* ======================================================================== */

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

#include <stdio.h>
#include <assert.h>
#include "config.h"
#include "icart/icartrom.h"
#include "misc/crc16.h"


#define SET_BIT(bv,idx) do {                                    \
                            int _ = (idx);                      \
                            (bv)[_ >> 5] |= 1 << (_ & 31);      \
                        } while(0)

#define CLR_BIT(bv,idx) do {                                    \
                            int _ = (idx);                      \
                            (bv)[_ >> 5] &= ~(1 << (_ & 31));   \
                        } while(0)

#define GET_BIT(bv,i,b) do {                                    \
                            int _ = (i);                        \
                            b = ((bv)[_ >> 5] >> (_ & 31)) & 1; \
                        } while(0)


/* ======================================================================== */
/*  ICARTROM_INIT   -- Initialize an Intellicart image to defaults.         */
/* ======================================================================== */
void icartrom_init(icartrom_t *rom)
{
    if (rom) memset(rom, 0, sizeof(icartrom_t));
}

/* ======================================================================== */
/*  ICARTROM_ADDSEG -- Adds a segment to an Intellcart ROM structure.       */
/* ======================================================================== */
int icartrom_addseg
(   
    icartrom_t  *rom,       /* Intellicart ROM structure.                   */
    uint_16     *data,      /* Data to insert into the ROM.  May be NULL.   */
    uint_32     addr,       /* Address to insert the data at.               */
    uint_32     len,        /* Length of the data to insert.                */
    uint_8      set_attr,   /* Attributes to set (read, write, banksw).     */
    uint_8      clr_attr    /* Attributes to clear                          */
)
{
    uint_32 p, a;
/*  jzp_printf("ADDSEG: addr %.4X len %.4X set %.2X clr %.2X\n", addr, len, set_attr, clr_attr);*/

    /* -------------------------------------------------------------------- */
    /*  Sanity checks:                                                      */
    /* -------------------------------------------------------------------- */
    if (!rom)                   return -1;  /* Valid ROM pointer?           */
    if (addr + len > 0x10000)   return -1;  /* addr + len doesn't wrap?     */
    
    /* -------------------------------------------------------------------- */
    /*  Next, if any actual data was provided, memcpy() it into the ICart   */
    /* -------------------------------------------------------------------- */
    if (data) 
    {
        set_attr |=  ICARTROM_PRELOAD; /* Force these pages to be preloaded */
        clr_attr &= ~ICARTROM_PRELOAD; /* Force these pages to be preloaded */
        memcpy(rom->image + addr, data, len * sizeof(uint_16));
    }

    /* -------------------------------------------------------------------- */
    /*  Now, on the range specified, update the various attributes.         */
    /* -------------------------------------------------------------------- */
    for (a = addr; a < addr + len; a += 256)
    {
        p = a >> 8;
        if (set_attr & ICARTROM_PRELOAD) SET_BIT(rom->preload , p);
        if (set_attr & ICARTROM_READ   ) SET_BIT(rom->readable, p);
        if (set_attr & ICARTROM_WRITE  ) SET_BIT(rom->writable, p);
        if (set_attr & ICARTROM_NARROW ) SET_BIT(rom->narrow  , p);
        if (set_attr & ICARTROM_BANKSW ) SET_BIT(rom->dobanksw, p);

        if (clr_attr & ICARTROM_PRELOAD) CLR_BIT(rom->preload , p);
        if (clr_attr & ICARTROM_READ   ) CLR_BIT(rom->readable, p);
        if (clr_attr & ICARTROM_WRITE  ) CLR_BIT(rom->writable, p);
        if (clr_attr & ICARTROM_NARROW ) CLR_BIT(rom->narrow  , p);
        if (clr_attr & ICARTROM_BANKSW ) CLR_BIT(rom->dobanksw, p);
/*      jzp_printf("ATTR: set %.2X clr %.2X on %.2X00\n", set_attr,clr_attr,p);*/

        if (a + 255 > addr + len) p = (addr + len - 1) >> 8;
        else                      p = (a    + 255 - 1) >> 8;
        if (set_attr & ICARTROM_PRELOAD) SET_BIT(rom->preload , p);
        if (set_attr & ICARTROM_READ   ) SET_BIT(rom->readable, p);
        if (set_attr & ICARTROM_WRITE  ) SET_BIT(rom->writable, p);
        if (set_attr & ICARTROM_NARROW ) SET_BIT(rom->narrow  , p);
        if (set_attr & ICARTROM_BANKSW ) SET_BIT(rom->dobanksw, p);

        if (clr_attr & ICARTROM_PRELOAD) CLR_BIT(rom->preload , p);
        if (clr_attr & ICARTROM_READ   ) CLR_BIT(rom->readable, p);
        if (clr_attr & ICARTROM_WRITE  ) CLR_BIT(rom->writable, p);
        if (clr_attr & ICARTROM_NARROW ) CLR_BIT(rom->narrow  , p);
        if (clr_attr & ICARTROM_BANKSW ) CLR_BIT(rom->dobanksw, p);
/*      jzp_printf("ATTR: set %.2X clr %.2X on %.2X00\n", set_attr,clr_attr,p);*/
    }

    return 0; /*SUCCESS*/
}

/* ======================================================================== */
/*  ICARTROM_GENROM -- Generate a .ROM image from an icartrom_t.            */
/* ======================================================================== */
uint_8 *icartrom_genrom(icartrom_t *rom, uint_32 *rom_size, ictype_t type)
{
    uint_32 size;
    uint_8  bank_attr[32];
    uint_8  fine_addr[32];
    uint_8  seg_lo[128], seg_hi[128];
    int     num_seg;
    int     i, j, lo, hi, b, tmp, attr, ofs;
    uint_8  *rom_img;
    uint_16 crc;

    /* -------------------------------------------------------------------- */
    /*  Sanity checks.                                                      */
    /* -------------------------------------------------------------------- */
    if (!rom || !rom_size) return NULL;
    *rom_size = 0;

    /* -------------------------------------------------------------------- */
    /*  Scan the memory map looking for segments of memory image to dl.     */
    /* -------------------------------------------------------------------- */
    for (i = num_seg = 0, lo = hi = -1; i <= 256; i++)
    {
        if (i != 256) { GET_BIT(rom->preload, i, b); } else { b = 0; }
        if (b)
        {   
            hi = i;                     /* extend upper end of segment  */
            if (lo == -1) { lo = i; }   /* detect start of a segment    */
        } else
        {
            if (lo >= 0)
            {
                seg_lo[num_seg] = lo;
                seg_hi[num_seg] = hi;
                num_seg++;
            }
            hi = lo = -1;
        }
    }

    /* -------------------------------------------------------------------- */
    /*  Now, build up the attribute flags and fine-address ranges.          */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 256; i += 8)
    {
        uint_8 page[8];     /* attrs for pages within 2K bank */

        /* ---------------------------------------------------------------- */
        /*  First, generate the union of all attributes in this 2K bank.    */
        /* ---------------------------------------------------------------- */
        for (j = attr = 0; j < 8; j++)
        {
            tmp = 0;
            GET_BIT(rom->readable, (i+j), b); if (b) tmp |= ICARTROM_READ;
            GET_BIT(rom->writable, (i+j), b); if (b) tmp |= ICARTROM_WRITE;
            GET_BIT(rom->narrow,   (i+j), b); if (b) tmp |= ICARTROM_NARROW;
            GET_BIT(rom->dobanksw, (i+j), b); if (b) tmp |= ICARTROM_BANKSW;
            attr |= tmp;
            page[j] = tmp;
        }

        /* ---------------------------------------------------------------- */
        /*  No attributes?  Go to next 2K bank.                             */
        /* ---------------------------------------------------------------- */
        if (!attr)
        {
            fine_addr[i >> 3] = 0x07;
            bank_attr[i >> 3] = 0x00;
            continue;
        }

        /* ---------------------------------------------------------------- */
        /*  Next, determine the range of addresses that have any bits set.  */
        /*  Warn if some pages don't have all of the attributes that the    */
        /*  bank will end up having.                                        */
        /* ---------------------------------------------------------------- */
        for (j = 0, lo = hi = -1; j < 8; j++)
        {
            if (page[j])
            {
                if (lo == -1) lo = j;
                hi = j;

                if ((page[j] & attr) != attr)
                {
                    /* put warning here */
                }
            }
        }

        /* ---------------------------------------------------------------- */
        /*  Store out the final fine-address range and bank attributes.     */
        /* ---------------------------------------------------------------- */
        assert (lo != -1 && hi != -1);
        fine_addr[i >> 3] = (lo << 4) | hi;
        bank_attr[i >> 3] = attr;
    }

    /* -------------------------------------------------------------------- */
    /*  Next, calculate the size of the .ROM image.  The .ROM format has    */
    /*  the following fixed and variable costs:                             */
    /*                                                                      */
    /*   -- Header:              3 bytes (fixed)                            */
    /*   -- Attribute Table:    16 bytes (fixed)                            */
    /*   -- Fine Addr Table:    32 bytes (fixed)                            */
    /*   -- Table Checksum:      2 bytes (fixed)                            */
    /*                         ------------------                           */
    /*                          53 bytes total fixed overhead               */
    /*                                                                      */
    /*   -- ROM segments:       4 + 2*num_words per segment.                */
    /* -------------------------------------------------------------------- */
    size = 53;

    for (i = 0; i < num_seg; i++)
        size += 512 * (seg_hi[i] - seg_lo[i] + 1) + 4;

    *rom_size = size;   /* Report the size to the caller.                   */

    /* -------------------------------------------------------------------- */
    /*  Allocate a hunk of memory for the .ROM file.                        */
    /* -------------------------------------------------------------------- */
    if ((rom_img = CALLOC(uint_8, size)) == NULL)
        return NULL;  /* fail if out-of-memory. */

    /* -------------------------------------------------------------------- */
    /*  Now, construct the .ROM image.                                      */
    /*  First:  The header.                                                 */
    /* -------------------------------------------------------------------- */
    switch (type)
    {
        case ICART:
            rom_img[0] = 0xA8;      /*  Autobaud Rate-Detection Byte        */
            break;
        case CC3_STD:
            rom_img[0] = 0x41;      /*  Autobaud Rate-Detection Byte        */
            break;
        case CC3_ADV:
            rom_img[0] = 0x61;      /*  Autobaud Rate-Detection Byte        */
            break;
        default:
            free(rom_img);          /*  Fail on unknown type.               */
            return NULL;
    }
    rom_img[1] = num_seg;           /*  Number of ROM segments in image.    */
    rom_img[2] = num_seg ^ 0xFF;    /*  1s Complement of # of segments.     */

    /* -------------------------------------------------------------------- */
    /*  Next:   The ROM segments.                                           */
    /* -------------------------------------------------------------------- */
    for (i = 0, ofs = 3; i < num_seg; i++)
    {
        int start_ofs, len;

        start_ofs = ofs;

        /* ---------------------------------------------------------------- */
        /*  Inclusive segment range.  (upper 8 of addresses)                */
        /* ---------------------------------------------------------------- */
        rom_img[ofs++] = seg_lo[i];
        rom_img[ofs++] = seg_hi[i];

        lo  =  seg_lo[i] << 8;
        hi  = (seg_hi[i] << 8) + 0x100;
        len = 2*(hi - lo) + 2;            /* +2 to include header */

        /* ---------------------------------------------------------------- */
        /*  Actual ROM image data in big-endian format.                     */
        /* ---------------------------------------------------------------- */
        for (j = lo; j < hi; j++)
        {
            rom_img[ofs++] = rom->image[j] >> 8;
            rom_img[ofs++] = rom->image[j] & 0xFF;
        }

        /* ---------------------------------------------------------------- */
        /*  ROM segment checksum (CRC-16), also big-endian.                 */
        /* ---------------------------------------------------------------- */
        crc = crc16_block(0xFFFF, rom_img + start_ofs, len);

        rom_img[ofs++] = crc >> 8;
        rom_img[ofs++] = crc & 0xFF;
    }

    /* -------------------------------------------------------------------- */
    /*  Last:   The attribute and fine-address tables.                      */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 16; i++)
        rom_img[ofs++] = bank_attr[i*2 + 0] | (bank_attr[i*2 + 1] << 4);

    for (j = 0; j < 2; j++)
        for (i = 0; i < 16; i++)
            rom_img[ofs++] = fine_addr[i*2 + j];

    crc = crc16_block(0xFFFF, rom_img + ofs - 48, 48);
    rom_img[ofs++] = crc >> 8;
    rom_img[ofs++] = crc & 0xFF;

    /* -------------------------------------------------------------------- */
    /*  Return the completed .ROM image.                                    */
    /* -------------------------------------------------------------------- */
    return rom_img;
}

/* ======================================================================== */
/*  ICARTROM_DECODE -- Decode a .ROM image into an icartrom_t.              */
/* ======================================================================== */
int icartrom_decode
(
    icartrom_t *rom,
    uint_8     *rom_img,
    int        ignore_crc
)
{
    int num_seg, lo, hi, i, j, ofs, p, attr;
    uint_16 crc_expect, crc_actual;

    /* -------------------------------------------------------------------- */
    /*  Sanity checks.                                                      */
    /* -------------------------------------------------------------------- */
    if (!rom_img)
        return -1;

    /* -------------------------------------------------------------------- */
    /*  First, check to see if the header passes initial muster.            */
    /* -------------------------------------------------------------------- */
    if ((rom_img[0] != 0xA8 && (rom_img[0] & ~0x20) != 0x41) ||
        (rom_img[1] ^ rom_img[2]) != 0xFF)
        return -2;      /* Bad header */

    /* -------------------------------------------------------------------- */
    /*  Step through the ROM segments.                                      */
    /* -------------------------------------------------------------------- */
    num_seg = rom_img[1];
    ofs = 3;
    for (i = 0; i < num_seg; i++)
    {
        int start;

        /* ---------------------------------------------------------------- */
        /*  Get the range of addresses for the ROM segment.                 */
        /* ---------------------------------------------------------------- */
        start = ofs;
        lo =  rom_img[ofs++] << 8;
        hi = (rom_img[ofs++] << 8) + 0x100;

        if (hi < lo)
            return -4;  /* Bad ROM segment address range. */
        
        if (!rom)
            ofs += 2 * (hi - lo);
        else
            for (j = lo; j < hi; j++)
            {
                rom->image[j] = (rom_img[ofs] << 8) | (rom_img[ofs + 1] & 0xFF);
                ofs += 2;
            }

        /* ---------------------------------------------------------------- */
        /*  Mark the "preload" bits for this ROM segment.                   */
        /* ---------------------------------------------------------------- */
        if (rom)
            for (j = lo >> 8; j < (hi >> 8); j++)
                SET_BIT(rom->preload, j);

        /* ---------------------------------------------------------------- */
        /*  Check the CRC-16, unless instructed not to.  (We might do this  */
        /*  to try to rescue a corrupt or tweaked ROM or something.)        */
        /* ---------------------------------------------------------------- */
        if (!ignore_crc)
        {
            crc_expect = (rom_img[ofs] << 8) | (rom_img[ofs + 1] & 0xFF);
            crc_actual = crc16_block(0xFFFF, rom_img + start, 2 * (hi-lo) + 2);
            if (crc_expect != crc_actual)
                return -3;  /* CRC error in ROM segments. */
        }
        ofs += 2; /* step over CRC */
    }

    /* -------------------------------------------------------------------- */
    /*  Unpack the attributes into our bitmaps.                             */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 32; i++)
    {
        /* ---------------------------------------------------------------- */
        /*  Get our attribute and fine-address bits from the right nibbles. */
        /* ---------------------------------------------------------------- */
        attr = 0xF & (rom_img[ofs + (i >> 1)] >> ((i & 1) * 4));
        hi   = rom_img[ofs + 16 + ((i >> 1) | ((i & 1) << 4))];
        lo   = (hi >> 4) & 0x7;
        hi   = (hi & 0x7) + 1;

        /* ---------------------------------------------------------------- */
        /*  Sanity checks.                                                  */
        /* ---------------------------------------------------------------- */
        if (hi < lo)
            return -5; /* Bad fine-address range */

        if (!attr || !rom)
            continue;

        /* ---------------------------------------------------------------- */
        /*  Apply attributes to our fine-address range.                     */
        /* ---------------------------------------------------------------- */
        for (j = lo; j < hi; j++)
        {
            p = (i << 3) + j;

            if (attr & ICARTROM_READ  ) SET_BIT(rom->readable, p);
            if (attr & ICARTROM_WRITE ) SET_BIT(rom->writable, p);
            if (attr & ICARTROM_NARROW) SET_BIT(rom->narrow  , p);
            if (attr & ICARTROM_BANKSW) SET_BIT(rom->dobanksw, p);
        }
    }

    /* -------------------------------------------------------------------- */
    /*  Check the CRC on our attributes.                                    */
    /* -------------------------------------------------------------------- */
    if (!ignore_crc)
    {
        crc_expect = (rom_img[ofs + 48] << 8) | (rom_img[ofs + 49] & 0xFF);
        crc_actual = crc16_block(0xFFFF, rom_img + ofs, 48);
        if (crc_expect != crc_actual)
            return -6;  /* CRC error in enable tables */
    }

    ofs += 50;

    /* -------------------------------------------------------------------- */
    /*  Return the total number of bytes decoded.                           */
    /* -------------------------------------------------------------------- */
    return ofs;
}

/* ======================================================================== */
/*  ICARTROM_READFILE -- Reads a file into an icartrom_t.                   */
/* ======================================================================== */
int icartrom_readfile(char *fname, icartrom_t *the_icart) 
{ 
    int len, decoded;
    uint_8 *rom_img;
    FILE *f;

    f = fopen(fname, "rb");

    if (!f)
    {   
        perror("fopen()");
        fprintf(stderr, "ERROR: Couldn't open '%s' for reading\n", fname);
        exit(1);
    }

    fseek(f, 0, SEEK_END);
    if ((len = ftell(f)) < 0)
    {   
        perror("fseek()");
        fprintf(stderr, "ERROR:  Error seeking while reading '%s'\n", fname);
        exit(1);
    }
    rewind(f);

    if ((rom_img = CALLOC(uint_8, len)) == NULL)
    {   
        perror("malloc()");
        fprintf(stderr, "ERROR:  Out of memory decoding '%s'\n", fname);
        exit(1);
    }

    fread(rom_img, 1, len, f);
    fclose(f);

    decoded = icartrom_decode(the_icart, rom_img, 0);
    free(rom_img);

    return decoded;
}


/* ======================================================================== */
/*  ICARTROM_WRITEFILE -- Writes a file from an icartrom_t.                 */
/* ======================================================================== */
uint_32 icartrom_writefile(char *fname, icartrom_t *the_icart, ictype_t type) 
{
    uint_32 size = 0;
    uint_8  *rom_img = NULL;
    FILE    *fr;

    fr = fopen(fname, "wb");
    if (!fr)
    {
        fprintf(stderr, "ERROR:  Could not open '%s' for writing\n", fname);
        exit(1);
    }

    rom_img = icartrom_genrom(the_icart, &size, type);

    if (!rom_img)
    {
        fprintf(stderr, "ERROR:  No ROM image generated?\n");
        exit(1);
    }

    fwrite(rom_img, 1, size, fr);
    fclose(fr);

    return size;
}

/* ======================================================================== */
/*                 Copyright (c) 2001-+Inf, Joseph Zbiciak                  */
/* ======================================================================== */
