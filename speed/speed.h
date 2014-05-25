/*
 * ============================================================================
 *  Title:    
 *  Author:   J. Zbiciak
 *  $Id: speed.h,v 1.7 1999/10/10 08:44:30 im14u2c Exp $
 * ============================================================================
 *
 * ============================================================================
 *
 * ============================================================================
 */

#ifndef _SPEED_H
#define _SPEED_H

typedef struct speed_t
{
    periph_t        periph;
    double          last_time;
    double          threshold;
    double          target_rate;
    uint_64         tick;
    uint_32         warmup;
    uint_8          busywaits_ok;
    uint_8          pal;
    gfx_t           *gfx;
    stic_t          *stic;
} speed_t;

/*
 * ============================================================================
 *  SPEED_TK         -- Main throttling agent.
 *  SPEED_INIT       -- Initializes a speed-control object.
 *  SPEED_RESYNC     -- Slips time to resync speed-control
 * ============================================================================
 */
uint_32 speed_tk    (periph_t *p, uint_32 len);
int     speed_init  (speed_t *speed, gfx_t *gfx, stic_t *stic,
                     int busywaits, double target, int pal_mode);
void    speed_resync(speed_t *speed);

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


