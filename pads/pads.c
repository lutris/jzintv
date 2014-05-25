/*
 * ============================================================================
 *  Title:    Controller pads
 *  Author:   J. Zbiciak
 *  $Id: pads.c,v 1.18 1999/10/10 08:44:29 im14u2c Exp $
 * ============================================================================
 *  This module implements the controller pads.
 *  Pads are peripherals that extend periph_t.
 * ============================================================================
 */


#include "config.h"
#include "periph/periph.h"
#include "pads/pads.h"
#include "event/event.h"

uint_32 pad_tk_keyboard(periph_t *p, uint_32 len);

/*
 * ============================================================================
 *  PAD_TK_EVENT -- Updates that pad's states according to event inputs.
 * ============================================================================
 */
uint_32 pad_tk_event(periph_t *p, uint_32 len)
{
    pad_t   *pad = (pad_t*)p;
    int     i, s;
    uint_32 value = 0, dflag;
    uint_32 *ptr;

    /* -------------------------------------------------------------------- */
    /*  If this pad interface is set to Keyboard or Piano, get outta here.  */
    /* -------------------------------------------------------------------- */
    if (pad->mode != PAD_HAND) return pad_tk_keyboard(p, len);

    /* -------------------------------------------------------------------- */
    /*  If no input events have occurred, don't reevaluate the pads.        */
    /* -------------------------------------------------------------------- */
    if (pad->last_evt == event_count)
        return len;

    pad->last_evt = event_count;

    /* -------------------------------------------------------------------- */
    /*  Iterate over both controllers.                                      */
    /* -------------------------------------------------------------------- */
    for (s = 0; s < 2; s++)
    {
        /* ---------------------------------------------------------------- */
        /*  Skip ports that are marked as 'output.'                         */
        /* ---------------------------------------------------------------- */
        if (pad->io[s]) continue;

        /* ---------------------------------------------------------------- */
        /*  Start out with "nothing pressed."                               */
        /* ---------------------------------------------------------------- */
        value = 0;

        /* ---------------------------------------------------------------- */
        /*  Merge all of the keypad / action key inputs to the controller.  */
        /* ---------------------------------------------------------------- */
        ptr = (s) ? pad->l : pad->r;

        for (i = 0; i < 15; i++)
            value |= ptr[i];

        /* ---------------------------------------------------------------- */
        /*  Now, generate a disc dir # from E/NE/N/NW/W/SW/S/SE flags.      */
        /*                                                                  */
        /*  Input bits 0, 2, 4, and 6 give us pure E, N, W, and S.          */
        /*  Input bits 1, 3, 5, and 7 give us NE, NW, SE, SW.               */
        /*                                                                  */
        /*  Pad bit 0 is set for WSW through SE.                            */
        /*  Pad bit 1 is set for SSE through NE.                            */
        /*  Pad bit 2 is set for ENE through NW.                            */
        /*  Pad bit 3 is set for NNW through SW.                            */
        /*  Pad bit 4 is set for NE, NNE, NW, WNW, SW, SSW, SE, ESE.        */
        /*                                                                  */
        /*  Input bit to compass headings:                                  */
        /*                                                                  */
        /*      Compass         Pattern 1           Pattern 2               */
        /*         E            00000001                                    */
        /*         ENE          00000011                                    */
        /*         NE           00000010            00000101                */
        /*         NNE          00000110                                    */
        /*         N            00000100                                    */
        /*         NNW          00001100                                    */
        /*         NW           00001000            00010100                */
        /*         WNW          00011000                                    */
        /*         W            00010000                                    */
        /*         WSW          00110000                                    */
        /*         SW           00100000            01010000                */
        /*         SSW          01100000                                    */
        /*         S            01000000                                    */
        /*         SSE          11000000                                    */
        /*         SE           10000000            01000001                */
        /*         ESE          10000001                                    */
        /*                                                                  */
        /* ---------------------------------------------------------------- */
        dflag = (0xFF & (ptr[15]|ptr[16]));
        dflag = (dflag << 4) | (dflag >> 4);

        /* ---------------------------------------------------------------- */
        /*  Step through the four major compass dirs and set bits 0..3      */
        /*  according to each range.  Also process bit 4 along the way.     */
        /*  We begin our analysis with 'south' as that is bit 0.            */
        /* ---------------------------------------------------------------- */
        for (i = 0; i < 4; i++, dflag >>= 2)
        {
            /* ------------------------------------------------------------ */
            /*  Handle major-direction bit.  We set the bit for any dir     */
            /*  that is mostly in the direction of the major-direction, as  */
            /*  well as for one lop-sided case on the side.                 */
            /* ------------------------------------------------------------ */
            if ((dflag&0x14) != 0x10 && (dflag&0x0F) > 0x01) value |= 1 << i;

            /* ------------------------------------------------------------ */
            /*  Check for diagonal bit (bit 4) also.                        */
            /* ------------------------------------------------------------ */
            if ((0x64 >> (dflag & 0x07)) & 1)                value |= 0x10;
        }

        /* ---------------------------------------------------------------- */
        /*  Remember our output value.                                      */
        /* ---------------------------------------------------------------- */
        pad->side[s] = 0xFF & ~value;
    }

    return len;
}


/*
 * ============================================================================
 *  PAD_TK_KEYBOARD  -- Updates that pad's states according to event inputs.
 * ============================================================================
 */
uint_32 pad_tk_keyboard(periph_t *p, uint_32 len)
{
    pad_t   *pad = (pad_t*)p;
    uint_32 value, need_shift = 0, synth_keys = 0;
    int i, j;
    uint_32 kk[8], tk[8];

    /* -------------------------------------------------------------------- */
    /*  The I/O ports can have one of four settings.  The following truth   */
    /*  table indicates the meaning of these four modes.                    */
    /*                                                                      */
    /*      io[1]   io[0]       Meaning                                     */
    /*        0       0         Both sides read -- just float inputs        */
    /*        0       1         Normal scanning mode.  0 drives, 1 reads    */
    /*        1       0         Transposed scanning.   1 drives, 0 reads    */
    /*        1       1         Illegal:  Both sides driving.               */
    /*                                                                      */
    /*  It's worth noting that ECS BASIC does not employ transposed scan.   */
    /*  Future software might, because it gives better opportunities for    */
    /*  disambiguating key aliases, esp. wrt to the SHIFT key.              */
    /*                                                                      */
    /*  The QWERTY keyboard has no diodes and therefore can be scanned      */
    /*  both normally and in a transposed manner.  It is also subject to    */
    /*  buffer fights due to ghost paths, limiting what keys the Inty can   */
    /*  resolve simultaneously.                                             */
    /*                                                                      */
    /*  The synthesizer keyboard, on the other hand, has a full set of      */
    /*  diodes.  As a result, it can only be scanned in the normal          */
    /*  direction, but there will never be any issues with ghosting or      */
    /*  buffer fights.  The Inty can resolve any key combination.           */
    /*                                                                      */
    /*  We handle the undefined "both driven" state by ignoring it.  If     */
    /*  we want to, we can use that state as a magic handshake so that      */
    /*  apps can request higher-quality keyboard input from an emulator.    */
    /* -------------------------------------------------------------------- */
    if (pad->io[0] == pad->io[1])
    {
        pad->side[0] = pad->side[1] = 0xFF;
        return len;
    }

    /* -------------------------------------------------------------------- */
    /*  Fold the 'fake-shift' data down into the real data if we need it.   */
    /* -------------------------------------------------------------------- */
    if (pad->fake_shift & 1)
        for (i = 0; i < 8; i++)
            kk[i] = (pad->k[i] | (pad->k[i] >> 8)) & 0xFF;
    else
        for (i = 0; i < 8; i++)
            kk[i] = (pad->k[i] & 0xFF);


    /* -------------------------------------------------------------------- */
    /*  Determine if we'll need fake-shift next scan. Some of our           */
    /*  keybindings press a key AND shift.  We handle that by looking at    */
    /*  the upper bits of the value table and seeing if we need to push     */
    /*  shift in addition to the key.                                       */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 8; i++)
        need_shift |= pad->k[i];

    synth_keys = (need_shift & 0xFF0000) != 0;
    need_shift = (need_shift & 0x00FF00) != 0;

    if (need_shift) { pad->fake_shift |= 2; } 
    else            { pad->fake_shift  = 0; }

    /* -------------------------------------------------------------------- */
    /*  Compute the transpose of the keyboard.  We need this to identify    */
    /*  "buffer fight paths", so that we correctly mask away keys in the    */
    /*  same way a real ECS keyboard will.                                  */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 8; i++)
    {
        uint_32 tmp = 0;

        for (j = 0; j < 8; j++)
            tmp |= ((kk[j] >> i) & 1) << j;

        tk[i] = tmp;
    }

    /* -------------------------------------------------------------------- */
    /*  See if any synth keys are pressed.  If so, merge them into the      */
    /*  keyboard image, but not the transpose.                              */
    /* -------------------------------------------------------------------- */
    if (synth_keys)
        for (i = 0; i < 8; i++)
            kk[i] |= (pad->k[i] >> 16) & 0xFF;

    /* -------------------------------------------------------------------- */
    /*  Handle normal scanning:                                             */
    /* -------------------------------------------------------------------- */
    if (pad->io[0] == 1)
    {
        /* ---------------------------------------------------------------- */
        /*  Ok, merge together the data from the rows selected by side 0.   */
        /* ---------------------------------------------------------------- */
        value = 0;
        for (i = 0; i < 8; i++)
        {
            if ((pad->side[0] & (1 << i)) == 0)
                value |= kk[i];
        }

        /* ---------------------------------------------------------------- */
        /*  Handle pushing the fake-shift.  We look for row 6 requested in  */
        /*  the scan, and the fake-shift flag being set.  If both are met,  */
        /*  assert the shift in col 7.                                      */
        /* ---------------------------------------------------------------- */
        if ((pad->side[0] & (1 << 6)) == 0 && (pad->fake_shift & 2) != 0)
        {
            value |= 128;
            pad->fake_shift = 1;
        } 

        /* ---------------------------------------------------------------- */
        /*  Go back and clear out bits that would get zapped by ghost       */
        /*  paths in the scanning matrix.  These happen wherever there's a  */
        /*  1 in the transpose that isn't matched up to a 0 in the scan val */
        /* ---------------------------------------------------------------- */
        for (i = 0; i < 8; i++)
        {
            uint_32 tmp = tk[i] & pad->side[0];
            if (tmp)
                value &= ~(1 << i);
        }

        pad->side[1] = 0xFF & ~value;
    } 
    /* -------------------------------------------------------------------- */
    /*  Handle transposed scanning:                                         */
    /* -------------------------------------------------------------------- */
    else if (pad->io[1] == 1)
    {
        /* ---------------------------------------------------------------- */
        /*  Ok, merge together the data from the rows selected by side 0.   */
        /* ---------------------------------------------------------------- */
        value = 0;
        for (i = 0; i < 8; i++)
        {
            if ((pad->side[1] & (1 << i)) == 0)
                value |= tk[i];
        }

        /* ---------------------------------------------------------------- */
        /*  Handle pushing the fake-shift.  We look for col 7 requested in  */
        /*  the scan, and the fake-shift flag being set.  If both are met,  */
        /*  assert the shift in row 6.                                      */
        /* ---------------------------------------------------------------- */
        if ((pad->side[1] & (1 << 7)) == 0 && (pad->fake_shift & 2) != 0)
        {
            value |= 64;
            pad->fake_shift = 1;
        } 

        /* ---------------------------------------------------------------- */
        /*  Go back and clear out bits that would get zapped by ghost       */
        /*  paths in the scanning matrix.  These happen wherever there's a  */
        /*  1 in the transpose that isn't matched up to a 0 in the scan val */
        /* ---------------------------------------------------------------- */
        for (i = 0; i < 8; i++)
        {
            uint_32 tmp = kk[i] & pad->side[1];
            if (tmp)
                value &= ~(1 << i);
        }

        pad->side[0] = 0xFF & ~value;
    } 

    else
    {
        fprintf(stderr, "pad_keyboard_tk: INTERNAL ERROR\n");
        exit(1);
    }

/*jzp_printf("\n(a) side[0]=%.2X side[1]=%.2X\n", pad->side[0], pad->side[1]);*/

    return len;
}

/*
 * ============================================================================
 *  PAD_READ     -- Returns the current state of the pads.
 * ============================================================================
 */
uint_32 pad_read        (periph_t *p, periph_t *r, uint_32 a, uint_32 d)
{
    pad_t *pad = (pad_t*)p;

    UNUSED(r);    
    UNUSED(d);    

    /* -------------------------------------------------------------------- */
    /*  Ignore accesses that are outside our address space.                 */
    /* -------------------------------------------------------------------- */
    if (a < 14) return ~0U;

    if (pad->mode == PAD_KEYBOARD)
        pad_tk_keyboard(p, 0);

    return (pad->side[a & 1] & 0xFF);
}

/*
 * ============================================================================
 *  PAD_WRITE    -- Looks for changes in I/O mode on PSG I/O ports.
 * ============================================================================
 */
void pad_write(periph_t *p, periph_t *r, uint_32 a, uint_32 d)
{
    pad_t *pad = (pad_t*)p;

    UNUSED(r);    

    /* -------------------------------------------------------------------- */
    /*  Capture writes to the 'control' register in the PSG, looking for    */
    /*  I/O direction setup.                                                */
    /* -------------------------------------------------------------------- */
    if (a == 8)
    {
        int io_0 = (d >> 6) & 1;
        int io_1 = (d >> 7) & 1;
        int need_reeval;

        need_reeval = ((!io_0) & pad->io[0]) |
                      ((!io_1) & pad->io[1]);

        pad->io[0] = io_0;
        pad->io[1] = io_1;

        /* ---------------------------------------------------------------- */
        /*  Force pad state to be re-evaluated if changing a port from      */
        /*  output to input.                                                */
        /* ---------------------------------------------------------------- */
        if (need_reeval)
        {
            pad->last_evt--;
            pad_tk_event(p, 0);
        }

/*jzp_printf("pad io: %d %d\n", io_0, io_1);*/

        /* ---------------------------------------------------------------- */
        /*  If we set a side to output, clear the outputted value. (?)      */
        /* ---------------------------------------------------------------- */
        /*if (io_0) pad->side[0] = 0;*/
        /*if (io_1) pad->side[1] = 0;*/
    }

    /* -------------------------------------------------------------------- */
    /*  Look for writes to I/O port 0 and 1.  If they're set to output,     */
    /*  record the writes.                                                  */
    /* -------------------------------------------------------------------- */
    if (a >= 14 && pad->io[a & 1])
    {
        pad->side[a & 1] = d;
    }
/*jzp_printf("(b) side[0]=%.2X side[1]=%.2X\n", pad->side[0], pad->side[1]);*/

    return ;
}

/*
 * ============================================================================
 *  PAD_INIT     -- Makes a dummy pad
 * ============================================================================
 */
int pad_init
(
    pad_t           *pad,       /*  pad_t structure to initialize       */
    uint_32         addr,       /*  Base address of pad.                */
    pad_mode_t      initial     /*  Initial controller/keyboard mode    */
)
{
    pad->periph.read      = pad_read;
    pad->periph.write     = pad_write;
    pad->periph.peek      = pad_read;
    pad->periph.poke      = pad_write;
    pad->periph.tick      = pad_tk_event;
    pad->periph.min_tick  = 3579545 / (4*120);  /* 120Hz scanning rate. */
    pad->periph.max_tick  = 3579545 / (4* 60);  /* 60Hz scanning rate.  */

    pad->periph.addr_base = addr;
    pad->periph.addr_mask = 0xF;

    pad->mode             = initial;
    pad->side[0]          = 0xFF;
    pad->side[1]          = 0xFF;
    pad->io  [0]          = 0;
    pad->io  [1]          = 0;
                  
    memset(pad->l, 0, sizeof(pad->l));
    memset(pad->r, 0, sizeof(pad->r));
    memset(pad->k, 0, sizeof(pad->k));

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
/*                 Copyright (c) 1998-2004, Joseph Zbiciak                  */
/* ======================================================================== */
