/*
 * ============================================================================
 *  Title:    Controller pads
 *  Author:   J. Zbiciak
 *  $Id: pads.h,v 1.10 1999/10/10 08:44:29 im14u2c Exp $
 * ============================================================================
 *  This module implements the controller pads.
 *  Pads are peripherals that extend periph_t.
 * ============================================================================
 */

#ifndef PAD_H_
#define PAD_H_

typedef enum { PAD_HAND = 0, PAD_KEYBOARD = 1, PAD_PIANO = 2 } pad_mode_t;

/*
 * ============================================================================
 *  PAD_T        -- Controller Pad structure
 * ============================================================================
 */
typedef struct pad_t
{
    periph_t    periph;     /*  Peripheral structure.                       */
    uint_32     last_evt;   /*  Last event number processed.                */
    pad_mode_t  mode;       /*  Current pad mode -- controller or keyboard  */
    uint_8      side[2];    /*  Last read values from each controller.      */
    uint_8      io  [2];    /*  Flag bits:  Is this side set for output?    */

    uint_32     l[17]; 
    uint_32     r[17];      /*  Event inputs to left and right controls.    */
    uint_32     k[8];       /*  Keyboard inputs for all 8 scanning rows.    */
    uint_32     fake_shift; /*  We fake pressing the shift key sometimes.   */
} pad_t;

/*
 * ============================================================================
 *  PAD_READ     -- Reads the hand controller pad (read or emulated).
 * ============================================================================
 */
uint_32 pad_read        (periph_t *, periph_t *, uint_32, uint_32);

/*
 * ============================================================================
 *  PAD_WRITE    -- Looks for changes in I/O mode on PSG I/O ports.
 * ============================================================================
 */
void pad_write(periph_t *p, periph_t *r, uint_32 a, uint_32 d);

/*
 * ============================================================================
 *  PAD_TK_EVENT -- Updates that pad's states according to event inputs.
 * ============================================================================
 */
uint_32 pad_tk_event(periph_t *p, uint_32 len);

/*
 * ============================================================================
 *  PAD_INIT     -- Makes an input pad device
 * ============================================================================
 */
int pad_init
(
    pad_t           *pad,       /*  pad_t structure to initialize       */
    uint_32         addr,       /*  Base address of pad.                */
    pad_mode_t      initial     /*  Initial controller/keyboard mode.   */
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
