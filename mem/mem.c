/*
 * ============================================================================
 *  Title:    Memory Subsystem
 *  Author:   J. Zbiciak
 *  $Id: mem.c,v 1.15 2001/02/03 02:34:21 im14u2c Exp $
 * ============================================================================
 *  This module implements RAMs and ROMs of various sizes and widths.
 *  Memories are peripherals that extend periph_t.
 *
 *  Currently, bank-switched ROMs aren't supported, but they will be
 *  eventually as I need to.  
 * ============================================================================
 * ============================================================================
 */


#include "config.h"
#include "periph/periph.h"
#include "mem.h"
#include "cp1600/cp1600.h"
#include "serializer/serializer.h"


/*
 * ============================================================================
 *  MEM_RD_8     -- Reads from an 8-bit memory.
 *  MEM_RD_10    -- Reads from a 10-bit memory.
 *  MEM_RD_16    -- Reads from a 16-bit memory.
 *  MEM_RD_G16   -- Reads from a 16-bit glitchy memory.
 *  MEM_RD_IC16  -- Reads from an Intellicart 16-bit memory
 *  MEM_RD_NULL  -- Ignored read.
 *  MEM_WR_8     -- Writes to an 8-bit memory.
 *  MEM_WR_10    -- Writes to a 10-bit memory.
 *  MEM_WR_16    -- Writes to a 16-bit memory.
 *  MEM_WR_G16   -- Writes to a 16-bit glitchy memory.
 *  MEM_WR_IC16  -- Writes to an Intellicart 16-bit memory
 *  MEM_WR_NULL  -- Ignored write.
 * ============================================================================
 */

uint_32 mem_rd_8    (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    UNUSED(ign);
    UNUSED(data);
    return mem->image[addr] & 0xFF;
}

uint_32 mem_rd_10   (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    UNUSED(ign);
    UNUSED(data);
    return mem->image[addr] & 0x3FF;
}

uint_32 mem_rd_16   (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    UNUSED(ign);
    UNUSED(data);
    return mem->image[addr];
}

uint_32 mem_rd_ic16 (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    int   bank = (addr >> 11) & 0x1F;
    UNUSED(ign);
    UNUSED(data);
    return mem->image[addr ^ (mem->banksw[bank] << 8)];
}

uint_32 mem_rd_g16  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    uint_16 glitch = 0;
    UNUSED(ign);
    UNUSED(data);
    /*if ((rand() & 131071) == 3) glitch = 1 << (0xF & rand());*/
    if ((rand() & 65535) == 3) glitch = 1 << (0xF & rand());
    return glitch ^ mem->image[addr];
}

uint_32 mem_rd_gen  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    UNUSED(ign);
    UNUSED(data);
    return mem->image[addr] & mem->data_mask;
}

uint_32 mem_rd_null (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    UNUSED(per);
    UNUSED(ign);
    UNUSED(addr);
    UNUSED(data);
    return ~0U;
}

void    mem_wr_8    (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    UNUSED(ign);
    mem->image[addr] = data & 0xFF;
}

void    mem_wr_10   (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    UNUSED(ign);
    mem->image[addr] = data & 0x3FF;
}

void    mem_wr_16   (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    UNUSED(ign);
    mem->image[addr] = data;
}

void    mem_wr_g16  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    UNUSED(ign);
    if ((rand() & 131071) == 3) data ^= 1 << (0xF & rand());
    mem->image[addr] = data;
}

void    mem_wr_ic16 (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    int   bank = (addr >> 11) & 0x1F;

    UNUSED(ign);
    if (bank == 0)
    {
        bank = ((data >> 4) & 0xF) | ((data << 1) & 0x10);
        mem->banksw[bank] = data & 0xFF;
    } else
    {
        mem->image[addr ^ mem->banksw[bank]] = data;
    }
}


void    mem_wr_gen  (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    UNUSED(ign);
    mem->image[addr] = data & mem->data_mask;
}

void    mem_wr_null (periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    UNUSED(per);
    UNUSED(ign);
    UNUSED(addr);
    UNUSED(data);
}

/*
 * ============================================================================
 *  MEM_RD_P16   -- Read a paged ROM
 *  MEM_WR_P16   -- Write a paged ROM
 *  MEM_PK_P16   -- Poke a paged ROM
 *  MEM_RS_P16   -- Reset a paged ROM
 * ============================================================================
 */
LOCAL uint_32 mem_rd_p16(periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    int range;

    UNUSED(ign);
    UNUSED(data);

    range = (addr >> 12) & 0xF;

    if (mem->banksw[range*2] == mem->banksw[range*2 + 1])
        return mem->image[addr - mem->img_base] & mem->data_mask;

    return ~0U;
}

LOCAL void mem_wr_p16(periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    int range, page;

    UNUSED(ign);

    if ((addr & 0x0FFF) != 0x0FFF) return;
    if ((addr & 0xFA50) != (data & 0xFFF0)) return;

    range = (data >> 12) & 0xF;
    page  = (data      ) & 0xF;

    if (mem->cpu)
        cp1600_invalidate((cp1600_t*)mem->cpu, 
                          range << 12, (range << 12) | 0xFFF);

    mem->banksw[range*2] = page;
}

LOCAL void mem_pk_p16(periph_t *per, periph_t *ign, uint_32 addr, uint_32 data)
{
    mem_t *mem = (mem_t*)per;
    int range;

    UNUSED(ign);
    UNUSED(data);

    range = (addr >> 12) & 0xF;

    if (mem->banksw[range*2] == mem->banksw[range*2 + 1])
        mem->image[addr - mem->img_base] = data & mem->data_mask;
}

LOCAL void mem_rs_p16(periph_t *per)
{
    mem_t *mem = (mem_t*)per;
    int range;

    for (range = 0; range < 16; range++)
        mem->banksw[range*2] = 0;
}

LOCAL void mem_ram_dtor(periph_p p)
{
    mem_t *mem = (mem_t *)p;

    CONDFREE(mem->image);
}

/*
 * ============================================================================
 *  MEM_MAKE_ROM -- Initializes a mem_t to be a ROM of a specified width
 *  MEM_MAKE_RAM -- Initializes a mem_t to be a RAM of a specified width
 *  MEM_MAKE_IC  -- Initializes a mem_t to be an Intellicart
 *  MEM_MAKE_PROM-- Initializes a mem_t to be a Paged ROM of specified width
 *  MEM_MAKE_9600A  Initializes a mem_t behave like RO-3-9600A at $360-$3FF
 * ============================================================================
 */

int mem_make_rom
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    int             width,      /*  width of ROM in bits.           */
    uint_32         addr,       /*  Base address of ROM.            */
    uint_32         size,       /*  Pwr of 2 size of ROM in words.  */
    uint_16         *image      /*  Memory image to use for ROM     */
)
{
    /* -------------------------------------------------------------------- */
    /*  Set up peripheral function pointers to support reads of the right   */
    /*  width.  Ignore writes and explicitly disallow ticks.                */
    /* -------------------------------------------------------------------- */
    mem->periph.write   = NULL;
    mem->periph.read    = width ==  8 ? mem_rd_8  :
                          width == 10 ? mem_rd_10 :
                          width == 16 ? mem_rd_16 : mem_rd_gen;

    mem->periph.peek    = mem->periph.read;
    mem->periph.poke    = width ==  8 ? mem_wr_8  :
                          width == 10 ? mem_wr_10 :
                          width == 16 ? mem_wr_16 : mem_wr_gen;

    mem->periph.tick        = NULL;
    mem->periph.min_tick    = ~0U;
    mem->periph.max_tick    = ~0U;
    mem->periph.addr_base   = addr;
    mem->periph.addr_mask   = ~((~0U) << size);
    mem->periph.ser_init    = NULL; /* don't serialize ROMs */
    mem->periph.dtor        = NULL; /* don't destruct ROMs */

    /* -------------------------------------------------------------------- */
    /*  Set up the mem-specific fields.                                     */
    /* -------------------------------------------------------------------- */
    mem->image      = image;
    mem->img_length = 1 << size;
    mem->data_mask  = ~((~0U) << width);

    return 0;
}


int mem_make_ram
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    int             width,      /*  Width of RAM in bits.           */
    uint_32         addr,       /*  Base address of RAM.            */
    uint_32         size,       /*  Pwr of 2 size of RAM in words.  */
    int             randomize   /*  randomize memory on powerup?    */
)
{
    
    /* -------------------------------------------------------------------- */
    /*  Set up peripheral function pointers to support reads and writes of  */
    /*  the right width.  Explicitly disallow ticks.                        */
    /* -------------------------------------------------------------------- */
    mem->periph.write   = width ==  8 ? mem_wr_8  :
                          width == 10 ? mem_wr_10 :
                          width == 16 ? mem_wr_16 : mem_wr_gen;
    mem->periph.read    = width ==  8 ? mem_rd_8  :
                          width == 10 ? mem_rd_10 :
                          width == 16 ? mem_rd_16 : mem_rd_gen;

    mem->periph.peek    = mem->periph.read;
    mem->periph.poke    = mem->periph.write;

    mem->periph.tick        = NULL;
    mem->periph.min_tick    = ~0U;
    mem->periph.max_tick    = ~0U;
    mem->periph.addr_base   = addr;
    mem->periph.addr_mask   = ~((~0U) << size);
    mem->periph.ser_init    = mem_ser_init;
    mem->periph.dtor        = mem_ram_dtor;

    /* -------------------------------------------------------------------- */
    /*  Set up the mem-specific fields.                                     */
    /* -------------------------------------------------------------------- */
    mem->image      = CALLOC(uint_16, 1 << size);
    mem->img_length = 1 << size;
    mem->data_mask  = ~((~0U) << width);

    /* -------------------------------------------------------------------- */
    /*  If set to randomize the memory, do so.                              */
    /* -------------------------------------------------------------------- */
    if (randomize && mem->image)
    {
        unsigned i; 
        for (i = 0; i < mem->img_length; i++)
            mem->image[i] = rand() & mem->data_mask;
    }

    return mem->image == NULL ? -1 : 0;
}

#if 0
int mem_make_ic
(
    mem_t           *mem        /*  mem_t structure to initialize   */
)
{
    
    /* -------------------------------------------------------------------- */
    /*  Set up peripheral function pointers to support reads and writes of  */
    /*  the right width.  Explicitly disallow ticks.                        */
    /* -------------------------------------------------------------------- */
    mem->periph.write   = mem_wr_ic16;
    mem->periph.read    = mem_rd_ic16;

    mem->periph.peek    = mem->periph.read;
    mem->periph.poke    = mem->periph.write;

    mem->periph.tick        = NULL;
    mem->periph.min_tick    = ~0U;
    mem->periph.max_tick    = ~0U;
    mem->periph.addr_base   = 0;
    mem->periph.addr_mask   = 0xFFFF;
    mem->periph.ser_init    = mem_ser_init;


    /* -------------------------------------------------------------------- */
    /*  Set up the mem-specific fields.                                     */
    /* -------------------------------------------------------------------- */
    mem->image      = CALLOC(uint_16, 65536);
    mem->img_length = 0x10000;
    mem->data_mask  = 0xFFFF;

    /* -------------------------------------------------------------------- */
    /*  Set up the Intellicart-specific fields.                             */
    /* -------------------------------------------------------------------- */
    memset(mem->banksw, 0, sizeof(mem->banksw));

    return mem->image == NULL ? -1 : 0;
}
#endif

int mem_make_prom
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    int             width,      /*  width of ROM in bits.           */
    uint_32         addr,       /*  Base address of ROM.            */
    uint_32         size,       /*  Pwr of 2 size of ROM in words.  */
    uint_32         page,       /*  ECS Page number to map to.      */
    uint_16         *image,     /*  Memory image to use for ROM     */
    void            *cpu        /*  CPU pointer for handling cache  */
)
{
    int a_lo, a_hi, a;

    /* -------------------------------------------------------------------- */
    /*  Set up peripheral function pointers to support reads of the right   */
    /*  width.  Ignore writes and explicitly disallow ticks.                */
    /* -------------------------------------------------------------------- */
    mem->periph.read    = mem_rd_p16;
    mem->periph.write   = mem_wr_p16;
    mem->periph.peek    = mem->periph.read;
    mem->periph.poke    = mem_pk_p16;
    mem->periph.reset   = mem_rs_p16;

    mem->periph.tick        = NULL;
    mem->periph.min_tick    = ~0U;
    mem->periph.max_tick    = ~0U;
    mem->periph.addr_base   = 0;        /* bankswitch code needs full addr. */
    mem->periph.addr_mask   = 0xFFFF;
    mem->periph.ser_init    = mem_ser_init;
    mem->periph.dtor        = NULL;

    /* -------------------------------------------------------------------- */
    /*  Set up the mem-specific fields.                                     */
    /* -------------------------------------------------------------------- */
    mem->image      = image;
    mem->img_length = 1 << size;
    mem->img_base   = addr;
    mem->data_mask  = ~((~0U) << width);
    mem->cpu        = cpu;

    /* -------------------------------------------------------------------- */
    /*  Set up the page-switching specific fields.                          */
    /* -------------------------------------------------------------------- */
    for (a = 0; a < 16; a++)
    {
        mem->banksw[2*a + 0] = 0;
        mem->banksw[2*a + 1] = 255;
    }

    a_lo = addr >> 12;
    a_hi = a_lo + ((size - 1) >> 12);

    for (a = a_lo; a <= a_hi; a++)
        mem->banksw[2*a + 1] = page;

    return 0;
}

LOCAL uint_16 ra3_9600a_pat[8] = 
{
    0x0204, 0x0255, 0x4104, 0x0020, 
    0xF460, 0x0080, 0x0120, 0x0404
};

int mem_make_9600a
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    uint_32         addr,       /*  Base address of ROM.            */
    uint_32         size        /*  Pwr of 2 size of ROM in words.  */
)
{
    int i;

    /* -------------------------------------------------------------------- */
    /*  Set up peripheral function pointers to support reads of the right   */
    /*  width.  Ignore writes and explicitly disallow ticks.                */
    /* -------------------------------------------------------------------- */
    mem->periph.write   = NULL;
    mem->periph.read    = mem_rd_g16;
    mem->periph.peek    = mem_rd_g16;
    mem->periph.poke    = mem_wr_16;

    mem->periph.tick        = NULL;
    mem->periph.min_tick    = ~0U;
    mem->periph.max_tick    = ~0U;
    mem->periph.addr_base   = addr;
    mem->periph.addr_mask   = ~((~0U) << size);
    mem->periph.ser_init    = mem_ser_init;
    mem->periph.dtor        = mem_ram_dtor;

    /* -------------------------------------------------------------------- */
    /*  Set up the mem-specific fields.                                     */
    /* -------------------------------------------------------------------- */
    mem->image      = CALLOC(uint_16, 1 << size);
    mem->img_length = 1 << size;
    mem->data_mask  = 0xFFFF;

    /* -------------------------------------------------------------------- */
    /*  Write out a pattern similar to what I've observed on RA-3-9600A.    */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < (1 << size); i++)
        mem->image[i] = ra3_9600a_pat[i & 7];

    return 0;
}

int mem_make_glitch_ram
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    uint_32         addr,       /*  Base address of ROM.            */
    uint_32         size,       /*  Pwr of 2 size of ROM in words.  */
    int             randomize   /*  Randomize on init?              */
)
{
    /* -------------------------------------------------------------------- */
    /*  Set up peripheral function pointers to support reads of the right   */
    /*  width.  Ignore writes and explicitly disallow ticks.                */
    /* -------------------------------------------------------------------- */
    mem->periph.write   = mem_wr_g16;
    mem->periph.read    = mem_rd_g16;
    mem->periph.peek    = mem_rd_g16;
    mem->periph.poke    = mem_wr_g16;

    mem->periph.tick        = NULL;
    mem->periph.min_tick    = ~0U;
    mem->periph.max_tick    = ~0U;
    mem->periph.addr_base   = addr;
    mem->periph.addr_mask   = ~((~0U) << size);
    mem->periph.ser_init    = mem_ser_init;
    mem->periph.dtor        = mem_ram_dtor;

    /* -------------------------------------------------------------------- */
    /*  Set up the mem-specific fields.                                     */
    /* -------------------------------------------------------------------- */
    mem->image      = CALLOC(uint_16, 1 << size);
    mem->img_length = 1 << size;
    mem->data_mask  = 0xFFFF;

    /* -------------------------------------------------------------------- */
    /*  If set to randomize the memory, do so.                              */
    /* -------------------------------------------------------------------- */
    if (randomize && mem->image)
    {
        unsigned i; 
        for (i = 0; i < mem->img_length; i++)
            mem->image[i] = rand() & mem->data_mask;
    }

    return 0;
}


/*
 * ============================================================================
 *  MEM_SER_INIT
 * ============================================================================
 */
void mem_ser_init(periph_p p)
{
#ifdef NO_SERIALIZER
    UNUSED(p);
#else
    ser_hier_t *hier, *phier;
    mem_t *mem = (mem_t *)p;


    hier  = ser_new_hierarchy(NULL, p->name);
    phier = ser_new_hierarchy(hier, "periph");

    periph_ser_register(p, phier);

    ser_register(hier, "data_mask", &mem->data_mask, ser_u32, 1, 
                 SER_MAND|SER_HEX);

    if (p->read == mem_rd_p16)
    {        
        ser_register(hier, "banksw", &mem->banksw, ser_u8, 32, 
                     SER_MAND|SER_HEX);
    } else
    {
        ser_register(hier, "image", mem->image, ser_u16, mem->img_length,
                     SER_MAND|SER_HEX);
    }
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
/*                 Copyright (c) 1998-2000, Joseph Zbiciak                  */
/* ======================================================================== */
