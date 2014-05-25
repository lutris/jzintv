/*
 * ============================================================================
 *  Title:    INTV2PC Interface for controller pads.
 *  Author:   J. Zbiciak
 *  $Id$
 * ============================================================================
 *  This module implements the controller pads.
 *  Pads are peripherals that extend periph_t.
 * ============================================================================
 */

#ifndef PAD_INTV2PC_H_
#define PAD_INTV2PC_H_

/*
 * ============================================================================
 *  PAD_INTV2PC_T -- INTV2PC interface structure
 * ============================================================================
 */
typedef struct pad_intv2pc_t
{
#ifdef DIRECT_INTV2PC
    periph_t    periph;     /*  Peripheral structure.                       */
    uint_32     io_base;    /*  I/O port that INTV2PC is on.                */
    uint_32     rd_state;   /*  State-machine counter.                      */
    uint_32     rd_val;     /*  Value being read from INTV2PC.              */
    uint_8      side[2];    /*  Last read values from each controller.      */
    uint_8      io  [2];    /*  Flag bits:  Is this side set for output?    */
#else
    char        unused;     /*  nearly empty struct if compiled out.        */
#endif
} pad_intv2pc_t;

#ifdef DIRECT_INTV2PC
extern int pads_intv2pc_ports_ok;

/*
 * ============================================================================
 *  PAD_INTV2PC_READ -- Get the current state of the INTV2PC controller.
 * ============================================================================
 */
uint_32 pad_intv2pc_read(periph_t *, periph_t *, uint_32, uint_32);

/*
 * ============================================================================
 *  PAD_INTV2PC_WRITE -- We need to monitor the I/O state for the pads ports.
 * ============================================================================
 */
void    pad_intv2pc_write(periph_t *, periph_t *, uint_32, uint_32);

/*
 * ============================================================================
 *  PAD_INTV2PC_TICK  -- Update the INTV2PC reading state machine.
 * ============================================================================
 */
uint_32 pad_intv2pc_tick(periph_t *p, uint_32 len);
#endif /*DIRECT_INTV2PC*/

/*
 * ============================================================================
 *  PAD_INTV2PC_INIT  -- Initializes an INTV2PC device.
 * ============================================================================
 */
int pad_intv2pc_init
(
    pad_intv2pc_t   *pad,       /*  INTV2PC structure to initialize     */
    uint_32         addr,       /*  Base address of pad.                */
    uint_32         io_base     /*  Hand contr interface IO base.       */
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
/*                 Copyright (c) 1998-2004, Joseph Zbiciak                  */
/* ======================================================================== */
