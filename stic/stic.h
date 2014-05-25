/*
 * ============================================================================
 *  Title:    STIC -- Standard Television Interface Chip, AY-3-8900
 *  Author:   J. Zbiciak
 *  $Id$
 * ============================================================================
 *  This module implements the STIC Chip.  Its functionality is currently
 *  far from complete.  For instance, I don't implement MOBs yet.
 * ============================================================================
 *  The STIC processor is active over several regions of memory:
 *
 *   0x0000 - 0x001F     -- MOB (Movable OBject) controls 
 *                          0x0000 + n  X Position, MOB #n
 *                          0x0008 + n  Y Position, MOB #n
 *                          0x0010 + n  Attribute Register, MOB #n
 *                          0x0018 + n  Collision Detect, MOB #n
 *
 *   0x0020 - 0x0021     -- STIC control registers  
 *                          0x0020      Display Enable
 *                          0x0021      Graphics Mode
 *
 *   0x0028 - 0x002C     -- Global color settings
 *                          0x0028 + n  Color Stack (n == 0..3)
 *                          0x002C      Border Color
 *
 *   0x0030 - 0x0032     -- Display framing
 *                          0x0030      Pixel Column Delay (0..7)
 *                          0x0031      Pixel Row Delay (0..7)
 *                          0x0032      Edge inhibit (bit0->left, bit1->top)
 *
 *   0x0200 - 0x035F     -- Background Card Table (BACKTAB)
 *
 *   0x3800 - 0x39FF     -- Graphics RAM
 *  
 *                          Note:  BACKTAB and GRAM are actually physically
 *                          separate.  This STIC implementation 'snoops'
 *                          these accesses for performance reasons.
 *
 *  For now, I handle all of the control register accesses into one 
 *  peripheral entry and all bus-snooping accesses in a second peripheral
 *  entry.
 * ============================================================================
 */

#ifndef _STIC_H
#define _STIC_H 1

#include "cp1600/req_bus.h"


/*
 * ============================================================================
 *  STIC_T           -- Main STIC structure.
 *
 *  Note: These bitfields seem to be broken at the present.
 *
 *  STIC_MOB_X_T     -- MOB X Position Bitfield Structure
 *  STIC_MOB_Y_T     -- MOB Y Position Bitfield Structure
 *  STIC_MOB_A_T     -- MOB Attribute Bitfield Structure
 *  STIC_MOB_C_T     -- MOB Collision Bitfield Structure
 * ============================================================================
 */

struct stic_t
{
    /* -------------------------------------------------------------------- */
    /*  The STIC is split into three separate peripherals, since a RD/WR    */
    /*  to something the STIC is interested in needs to go one of three     */
    /*  places -- either (1) to update the general state of the STIC via a  */
    /*  control register, (2) to update the BACKTAB, or (3) to update a     */
    /*  card definition in GRAM.                                            */
    /* -------------------------------------------------------------------- */
    periph_t    stic_cr;
    periph_t    snoop_btab;
    periph_t    snoop_gram;

    /* -------------------------------------------------------------------- */
    /*  We just keep a raw image of the STIC registers and decode manually  */
    /* -------------------------------------------------------------------- */
    uint_32     raw [0x40];
    uint_8      gmem[0x140 * 8];

    /* -------------------------------------------------------------------- */
    /*  We store several bitmaps and display lists.                         */
    /* -------------------------------------------------------------------- */
    int         fifo_ptr;                   /* Video FIFO pointer.          */
    uint_16     btab_sr [240];              /* BACKTAB as it is in Sys. RAM */
    uint_16     btab    [240];              /* BACKTAB as STIC sees it      */
    uint_32     last_bg [12];               /* Last background color by row */
 /* uint_32     bt_img  [240*8]; */         /* BACKTAB 4-bpp display list.  */
    uint_8      bt_bmp  [240*8];            /* BACKTAB 1-bpp display list.  */
    uint_32     mob_img [ 16*16  / 8];      /* Expanded/mirrored MOB 4bpp.  */
    uint_16     mob_bmp [8][16];            /* Expanded/mirrored MOBs 1bpp. */
    uint_32     mpl_img [192*224 / 8];      /* MOB image.                   */
    uint_32     mpl_vsb [192*224 /32];      /* MOB visibility bitmap.       */
    uint_32     mpl_pri [192*224 /32];      /* MOB priority bitmap.         */
    uint_32     xbt_img [192*112 / 8];      /* Re-tiled BACKTAB image.      */
    uint_32     xbt_bmp [192*112 /32];      /* Re-tiled BACKTAB 1-bpp.      */
    uint_32     image   [192*224 / 8];      /* Final 192x224 image, 4-bpp.  */

    uint_8      *disp;
    gfx_t       *gfx;

    /* -------------------------------------------------------------------- */
    /*  IRQ and BUSRQ generation.                                           */
    /* -------------------------------------------------------------------- */
    req_bus_t   *req_bus;       /* Bus for INTRQ, BUSRQ, INTAK, BUSAK.      */

    /* -------------------------------------------------------------------- */
    /*  STIC internal flags.                                                */
    /* -------------------------------------------------------------------- */
    void        (*upd)(struct stic_t*); /* Update fxn for curr disp mode.   */
    uint_8      phase;          /* 0 == Start of VBlank, 1 == Start of Pic  */
    uint_8      cp_row;         /* Row to copy at BUSAK                     */
    uint_8      ve_post;        /* Where vid-enables are posted.            */
    uint_8      vid_enable;     /* Must be set every vsync to enable vid.   */
    uint_8      mode, p_mode;   /* 1 == FG/BG mode, 0 == Color Stack.       */
    uint_8      bt_dirty;       /* BACKTAB is dirty.                        */
    uint_8      gr_dirty;       /* GRAM is dirty.                           */
    uint_8      ob_dirty;       /* MOBs are dirty.                          */
    uint_8      rand_regs;      /* Flag: Randomize registers on reset       */
    uint_8      pal;            /* Flag: 0 == NTSC, 1 == PAL                */
    int         drop_frame;     /* Frames to drop because we're behind.     */
    uint_64     gmem_accessible;/* CPU can access GRAM/GROM.                */
    uint_64     stic_accessible;/* CPU can access STIC registers            */
    uint_64     next_busrq;     /* Time of next STIC->CPU bus request.      */
    uint_64     next_irq;       /* Time of next STIC->CPU Interrupt request */
    uint_64     next_phase;     /* Don't switch phases until after this     */

    /* -------------------------------------------------------------------- */
    /*  Performance monitoring.  :-)                                        */
    /* -------------------------------------------------------------------- */
    struct stic_time_t
    {
        double  full_update;
        double  draw_btab;
        double  draw_mobs;
        double  fix_bord; 
        double  merge_planes;
        double  push_vid;
        double  mob_colldet;
        double  gfx_vid_enable;
        int     total_frames;
    } time;

    /* -------------------------------------------------------------------- */
    /*  Demo recording                                                      */
    /* -------------------------------------------------------------------- */
    demo_t      *demo;

    /* -------------------------------------------------------------------- */
    /*  Debugger support                                                    */
    /* -------------------------------------------------------------------- */
    uint_32     debug_flags;
};

#ifndef STIC_T_
#define STIC_T_ 1
typedef struct stic_t stic_t;
#endif

/*
 * ============================================================================
 *  Debugger flags
 * ============================================================================
 */

#define STIC_SHOW_WR_DROP               (1 <<  0)
#define STIC_SHOW_RD_DROP               (1 <<  1)
#define STIC_SHOW_FIFO_LOAD             (1 <<  2)
#define STIC_DBG_CTRL_ACCESS_WINDOW     (1 <<  3)
#define STIC_DBG_GMEM_ACCESS_WINDOW     (1 <<  4)
#define STIC_HALT_ON_BLANK              (1 <<  5)


/*
 * ============================================================================
 *  STIC_INIT        -- Initialize the STIC
 *  STIC_TICK        -- Perform a STIC update
 * ============================================================================
 */

int stic_init
(
    stic_t          *stic,
    uint_16         *grom_img,
    req_bus_t       *irq,
    gfx_t           *gfx,
    demo_t          *demo,
    int             rand_regs,
    int             pal_mode
);

uint_32 stic_tick
(
    periph_p        per,
    uint_32         len
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
/*                 Copyright (c) 1998-2011, Joseph Zbiciak                  */
/* ======================================================================== */
