/*
 * ============================================================================
 *  Title:    Controller pads and Keyboard.
 *  Author:   J. Zbiciak
 *  $Id$
 * ============================================================================
 *  This module implements the ECS keyboard, which hooks to the controller 
 *  pads.
 *  Pads are peripherals that extend periph_t.
 * ============================================================================
 */


#include "config.h"
#include "periph/periph.h"
#include "pads/pads.h"
#include "event/event.h"


/*
 * ============================================================================
 *  PAD_TK_KEYBOARD  -- Updates that pad's states according to event inputs.
 * ============================================================================
 */
uint_32 pad_tk_keyboard(periph_t *p, uint_32 len)
{
    pad_t   *pad = (pad_t*)p;
    uint_32 value, need_shift = 0;
    int i;

    /* -------------------------------------------------------------------- */
    /*  If I/O directions aren't set up for keyboard scanning, float.       */
    /* -------------------------------------------------------------------- */
    if (pad->io[0] == 0)
    {
        pad->side[0] = 0xFF;
        if (pad->io[1] == 0) 
            pad->side[1] = 0xFF;

        return len;
    }
    if (pad->io[1])
        return len;

    /* -------------------------------------------------------------------- */
    /*  Ok, merge together the data from the rows selected by side 0.       */
    /* -------------------------------------------------------------------- */
    value = need_shift = 0;
    for (i = 0; i < 8; i++)
    {
//      jzp_printf("%.2X ", pad->k[i]);
        need_shift |= pad->k[i];

        if ((pad->side[0] & (1 << i)) == 0)
            value |= pad->k[i];

        if ((pad->side[0] & (1 << i)) == 0 && (pad->fake_shift & 1) != 0)
            value |= pad->k[i] >> 8;
    }

    /* -------------------------------------------------------------------- */
    /*  Some of our keybindings press a key AND shift.  We handle that by   */
    /*  looking at the upper bits of the value table and seeing if we need  */
    /*  to push shift in addition to the key.                               */
    /* -------------------------------------------------------------------- */
    if ((need_shift & 0xFF00) != 0)
    {
        pad->fake_shift |= 2;
    } else
    {
        pad->fake_shift  = 0;
    }

    if ((pad->side[0] & (1 << 6)) == 0 && (pad->fake_shift & 2) != 0)
    {
        value |= 128;
        pad->fake_shift = 1;
    } 

    pad->side[1] = 0xFF & ~value;

//jzp_printf("\n(a) side[0]=%.2X side[1]=%.2X\n", pad->side[0], pad->side[1]);

    return len;
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
/*                 Copyright (c) 2003-+Inf, Joseph Zbiciak                  */
/* ======================================================================== */
