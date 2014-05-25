/*
 * ============================================================================
 *  Title:    Memory Subsystem
 *  Author:   J. Zbiciak
 *  $Id: mem.h,v 1.9 2001/02/03 02:34:21 im14u2c Exp $
 * ============================================================================
 *  This module implements RAMs and ROMs of various sizes and widths.
 *  Memories are peripherals that extend periph_t.
 *
 *  Currently, bank-switched ROMs aren't supported, but they will be
 *  eventually as I need to.  
 * ============================================================================
 * ============================================================================
 */

#ifndef _MEM_H
#define _MEM_H

/*
 * ============================================================================
 *  MEM_T        -- Memory Peripheral Structure
 * ============================================================================
 */

typedef struct mem_t
{
    periph_t    periph;     /*  Peripheral structure.                       */
    uint_16     *image;     /*  Memory image.                               */
    uint_32     img_base;   /*  address base of memory image.               */
    uint_32     data_mask;  /*  Data mask for odd-sized memories.           */
    uint_8      banksw[32]; /*  Bankswitch/Paging info (Intellicart/ECS)    */
    uint_32     img_length; /*  Actual length of memory image.              */
    void        *cpu;       /*  CPU pointer for handling caching.           */
} mem_t;


/*
 * ============================================================================
 *  MEM_RD_8     -- Reads from an 8-bit memory.
 *  MEM_RD_10    -- Reads from a 10-bit memory.
 *  MEM_RD_16    -- Reads from a 16-bit memory.
 *  MEM_RD_NULL  -- Ignored read.
 *  MEM_WR_8     -- Writes to an 8-bit memory.
 *  MEM_WR_10    -- Writes to a 10-bit memory.
 *  MEM_WR_16    -- Writes to a 16-bit memory.
 *  MEM_WR_NULL  -- Ignored write.
 * ============================================================================
 */

uint_32 mem_rd_8    (periph_t *, periph_t *, uint_32, uint_32);
uint_32 mem_rd_10   (periph_t *, periph_t *, uint_32, uint_32);
uint_32 mem_rd_16   (periph_t *, periph_t *, uint_32, uint_32);
uint_32 mem_rd_g16  (periph_t *, periph_t *, uint_32, uint_32);
uint_32 mem_rd_ic16 (periph_t *, periph_t *, uint_32, uint_32);
uint_32 mem_rd_gen  (periph_t *, periph_t *, uint_32, uint_32);
uint_32 mem_rd_null (periph_t *, periph_t *, uint_32, uint_32);

void    mem_wr_8    (periph_t *, periph_t *, uint_32, uint_32);
void    mem_wr_10   (periph_t *, periph_t *, uint_32, uint_32);
void    mem_wr_16   (periph_t *, periph_t *, uint_32, uint_32);
void    mem_wr_g16  (periph_t *, periph_t *, uint_32, uint_32);
void    mem_wr_ic16 (periph_t *, periph_t *, uint_32, uint_32);
void    mem_wr_gen  (periph_t *, periph_t *, uint_32, uint_32);
void    mem_wr_null (periph_t *, periph_t *, uint_32, uint_32);


/*
 * ============================================================================
 *  MEM_MAKE_ROM -- Initializes a mem_t to be a ROM of a specified width
 *  MEM_MAKE_RAM -- Initializes a mem_t to be a RAM of a specified width
 *  MEM_MAKE_IC  -- Initializes a mem_t to emulate an Intellicart.
 * ============================================================================
 */

int mem_make_rom
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    int             width,      /*  width of ROM in bits.           */
    uint_32         addr,       /*  Base address of ROM.            */
    uint_32         size,       /*  Size of ROM in words.           */
    uint_16         *image      /*  Memory image to use for ROM     */
);

int mem_make_prom
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    int             width,      /*  width of ROM in bits.           */
    uint_32         addr,       /*  Base address of ROM.            */
    uint_32         size,       /*  Size of ROM in words.           */
    uint_32         page,       /*  ECS page number of ROM (0..15)  */
    uint_16         *image,     /*  Memory image to use for ROM     */
    void            *cpu        /*  CPU struct, for managing cache  */
);

int mem_make_ram
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    int             width,      /*  Width of RAM in bits.           */
    uint_32         addr,       /*  Base address of RAM.            */
    uint_32         size,       /*  Size of RAM in words.           */
    int             randomize   /*  Flag: Randomize on init         */
);

int mem_make_ic
(
    mem_t           *mem        /*  mem_t structure to initialize   */
);

int mem_make_9600a
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    uint_32         addr,       /*  Base address                    */
    uint_32         size        /*  Power-of-2 size                 */
);

int mem_make_glitch_ram
(
    mem_t           *mem,       /*  mem_t structure to initialize   */
    uint_32         addr,       /*  Base address                    */
    uint_32         size,       /*  Power-of-2 size                 */
    int             randomize   /*  Flag: Randomize on init         */
);

void mem_ser_init(periph_p);

#endif

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
