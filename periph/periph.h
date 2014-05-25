/*
 * ============================================================================
 *  Title:    Peripheral Subsystem
 *  Author:   J. Zbiciak
 *  $Id: periph.h,v 1.16 2001/02/03 02:34:21 im14u2c Exp $
 * ============================================================================
 *  PERIPH_NEW       -- Creates a new peripheral bus
 *  PERIPH_DELETE    -- Disposes a peripheral bus
 *  PERIPH_REGISTER  -- Registers a peripheral on the bus
 *  PERIPH_READ      -- Perform a read on a peripheral bus
 *  PERIPH_WRITE     -- Perform a write on a peripheral bus
 *  PERIPH_TICK      -- Perform a tick on a peripheral bus
 * ============================================================================
 *  PERIPH_BUS_T     -- Peripheral bus information
 *  PERIPH_T         -- Per-peripheral information
 *  PERIPH_RD_T      -- Peripheral Read function pointer type
 *  PERIPH_WR_T      -- Peripheral Write function pointer type
 *  PERIPH_TICK_T    -- Peripheral Clock-Tick function pointer type
 * ============================================================================
 *  Peripheral bus information is divided into two sets of information:
 *
 *   -- Bus-wide information stored in a periph_bus_t
 *   -- Per-device information stored in a periph_t
 * 
 *  Peripherals wishing to use the peripheral subsystem need to 'extend'
 *  periph_t by declaring a structure which has a periph_t as its first
 *  element.  This extended structure can then be passed into the periph_*
 *  routines as if it were a 'periph_t', and can be passed to each of the
 *  peripherals as a 'this' pointer.
 * ============================================================================
 */

#ifndef _PERIPH_H
#define _PERIPH_H

#include "serializer/serializer.h"
#define MAX_PERIPH_BIN (32)

/*
 * ============================================================================
 *  PERIPH_RD_T      -- Peripheral Read function pointer type
 *  PERIPH_WR_T      -- Peripheral Write function pointer type
 *  PERIPH_TICK_T    -- Peripheral Clock-Tick function pointer type
 * ============================================================================
 */
typedef struct periph_t * periph_p;

typedef uint_32 (*periph_rd_t)  (periph_p periph, periph_p req, 
                                 uint_32 addr, uint_32 data);
typedef void    (*periph_wr_t)  (periph_p periph, periph_p req, 
                                 uint_32 addr, uint_32 data);
typedef uint_32 (*periph_tick_t)(periph_p periph, uint_32 len);
typedef void    (*periph_rst_t) (periph_p periph);
typedef void    (*periph_ser_t) (periph_p periph);
typedef void    (*periph_dtor_t)(periph_p periph);

/*
 * ============================================================================
 *  PERIPH_T         -- Per-peripheral information
 * ============================================================================
 */
typedef struct periph_t
{
    char            name[16];   /*  Name of device                          */
    periph_rd_t     read;       /*  Called for every read in addr space.    */
    periph_wr_t     write;      /*  Called for every write in addr space.   */
    periph_rd_t     peek;       /*  Reads memory without side-effects.      */
    periph_wr_t     poke;       /*  Writes memory (including ROM.)          */
    periph_tick_t   tick;       /*  Called every 'tick_per' ticks.          */
    periph_rst_t    reset;      /*  Called when resetting the machine       */
    periph_ser_t    ser_init;   /*  Called at reg time to init serializer   */
    periph_dtor_t   dtor;       /*  Destructor; called when shutting down   */

    uint_32         addr_base;  /*  Address base -- SUB'd from addrs on     */
                                /*  each read or write.                     */

    uint_32         addr_mask;  /*  Address mask -- AND'd with addr after   */
                                /*  subtracting the address base.           */

    uint_64         now;        /*  Peripheral's concept of 'now'.          */
    uint_32         min_tick;   /*  Minimum number of cycles between ticks. */
    uint_32         max_tick;   /*  Maximum number of cycles between ticks. */
    uint_32         next_tick;  /*  Number of ticks until next tick call.   */

    struct periph_bus_t *bus;   /*  Peripheral bus registered on.           */
    periph_p        next;       /*  Next peripheral on peripheral bus.      */
    periph_p        tickable;   /*  Next periph on tickable list.           */

    int             busy;       /*  Busy flag to prevent infinite loops.    */
    periph_p        req;        /*  Requestor busying this peripheral.      */
    void            *parent;    /*  Optional pointer to parent structure.   */
} periph_t;

/*
 * ============================================================================
 *  PERIPH_BUS_T     -- Peripheral bus information
 * ============================================================================
 */
typedef struct periph_bus_t
{
    periph_t    periph;         /*  Peripheral busses are also peripherals  */

    uint_32     addr_mask;      /*  Address mask (up to 32 bits).           */
    uint_32     data_mask;      /*  Data mask (up to 32 bits).              */

    uint_32     decode_shift;   /*  Controls granularity of addr. decode.   */
    
    periph_p    *rd[MAX_PERIPH_BIN];    /* Pointers to readable peripherals */
    periph_p    *wr[MAX_PERIPH_BIN];    /* Pointers to writable peripherals */

    periph_p    list;           /*  Linked list of peripherals on this bus  */
    periph_p    tickable;       /*  Linked list of periph. w/ tick fxns.    */
} periph_bus_t, *periph_bus_p;


/*
 * ============================================================================
 *  PERIPH_NEW       -- Creates a new peripheral bus
 * ============================================================================
 */
periph_bus_p periph_new
(
    int addr_size,              /*  Address size (in bits)              */
    int data_size,              /*  Data size (in bits)                 */
    int decode_shift            /*  Decode granularity control          */
);


/*
 * ============================================================================
 *  PERIPH_DELETE    -- Disposes a peripheral bus
 * ============================================================================
 */
void periph_delete
(
    periph_bus_p bus            /*  Peripheral bus to dispose           */
);

/*
 * ============================================================================
 *  PERIPH_REGISTER  -- Registers a peripheral on the bus
 * ============================================================================
 */
void periph_register
(
    periph_bus_p    bus,        /*  Peripheral bus being registered on. */
    periph_p        periph,     /*  Peripheral being (re)registered.    */
    uint_32         addr_lo,    /*  Low end of address range.           */
    uint_32         addr_hi,    /*  High end of address range.          */
    const char      *name       /*  Name to give device.                */
);

/*
 * ============================================================================
 *  PERIPH_READ      -- Perform a read on a peripheral bus
 * ============================================================================
 */
unsigned periph_read
(
    periph_p        bus,        /*  Peripheral bus being read.          */
    periph_p        req,        /*  Peripheral requesting read.         */
    uint_32         addr,       /*  Address being read.                 */
    uint_32         data        /*  Current state of data being read.   */
);

unsigned periph_peek
(
    periph_p        bus,        /*  Peripheral bus being read.          */
    periph_p        req,        /*  Peripheral requesting read.         */
    uint_32         addr,       /*  Address being read.                 */
    uint_32         data        /*  Current state of data being read.   */
);


/*
 * ============================================================================
 *  PERIPH_WRITE     -- Perform a write on a peripheral bus
 * ============================================================================
 */
void periph_write
(
    periph_p        bus,        /*  Peripheral bus being written.       */
    periph_p        req,        /*  Peripheral requesting write.        */
    uint_32         addr,       /*  Address being written.              */
    uint_32         data        /*  Data being written.                 */
);

void periph_poke 
(
    periph_p        bus,        /*  Peripheral bus being written.       */
    periph_p        req,        /*  Peripheral requesting write.        */
    uint_32         addr,       /*  Address being written.              */
    uint_32         data        /*  Data being written.                 */
);

/*
 * ============================================================================
 *  PERIPH_TICK      -- Perform a tick on a peripheral bus
 * ============================================================================
 */
uint_32 periph_tick
(
    periph_p        bus,        /*  Peripheral bus being ticked.        */
    uint_32         len
);

/*
 * ============================================================================
 *  PERIPH_RESET     -- Resets all of the peripherals on the bus
 * ============================================================================
 */
void periph_reset
(
    periph_bus_p    bus
);

/*
 * ============================================================================
 *  PERIPH_SER_REGISTER -- registers a peripheral for serialization
 * ============================================================================
 */
void periph_ser_register
(
    periph_p    per,
    ser_hier_t  *hier
);


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
/*                 Copyright (c) 1998-1999, Joseph Zbiciak                  */
/* ======================================================================== */
