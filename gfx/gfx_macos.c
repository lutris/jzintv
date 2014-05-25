/*
 * ============================================================================
 *  Title:    Graphics Interface Routines
 *  Author:   J. Zbiciak & Tim Lindner
 *  $Id: gfx_macos.c,v 1.4 1999/11/11 04:14:31 tlindner Exp $
 * ============================================================================
 *  GFX_INIT         -- Initializes a gfx_t object.
 *  GFX_TICK         -- Services a gfx_t tick.
 *  GFX_VID_ENABLE   -- Alert gfx that video has been enabled or blanked
 *  GFX_SET_BORD     -- Set the border / offset parameters for the display
 * ============================================================================
 *  GFX_T            -- Graphics subsystem object.
 *  GFX_PVT_T        -- Private internal state to gfx_t structure.
 *  GFX_STIC_PALETTE -- The STIC palette.
 * ============================================================================
 *  The graphics subsystem provides an abstraction layer between the 
 *  emulator and the graphics library being used.  Theoretically, this 
 *  should allow easy porting to other graphics libraries.
 *
 *  TODO:  
 *   -- Make use of dirty rectangle updating for speed.
 *   -- Add mechanism for dropping frames.
 * ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <time.h>
#include "config.h"
#include "periph.h"
#include "gfx.h"

#include <Quickdraw.h>
#include <QDOffscreen.h>
#include <MacWindows.h>

/* Protypes Required */
uint_32 gfx_tick(periph_p gfx_periph, uint_32 len);

/*
 * ============================================================================
 *  GFX_PVT_T        -- Private internal state to gfx_t structure.
 * ============================================================================
 */

typedef struct gfx_pvt_t
{
    WindowPtr       wind;          /*  Window to draw into             */
    GWorldPtr       scr;           /*  Offscreen world.                */
    PixMapHandle    thePixMap;     /*  Where the pixels live           */
    RGBColor        pal_on [32];   /*  Palette when video is enabled.  */
    RGBColor        pal_off[32];   /*  Palette when video is blanked.  */
    int             vid_enable;    /*  Video enable flag.              */
} gfx_pvt_t;

/*
 * ============================================================================
 *  GFX_STIC_PALETTE -- The STIC palette.
 * ============================================================================
 */
static uint_8 gfx_stic_palette[17][3] = 
{
    /* -------------------------------------------------------------------- */
    /*  These were derived using a bit of magic from the I and Q            */
    /*  values in the color processor's spec sheet.  They're still          */
    /*  very much stabs in the dark.                                        */
    /* -------------------------------------------------------------------- */
    { 0x0C, 0x00, 0x05 },
    { 0x00, 0x5F, 0xB8 },
    { 0xFF, 0x21, 0x10 },
    { 0xBE, 0xA9, 0x60 },
    { 0x49, 0x89, 0x00 },
    { 0x00, 0xA7, 0x00 },
    { 0xFA, 0xEA, 0x50 },
    { 0xFF, 0xFC, 0xFF },
    { 0xBD, 0xAC, 0xA8 },
    { 0x6B, 0xBE, 0xB7 },
    { 0xFF, 0x7D, 0x1F },
    { 0x73, 0x7E, 0x00 },
    { 0xFF, 0x41, 0xFF },
    { 0xB7, 0xBC, 0xFF },
    { 0x75, 0xAD, 0x00 },
    { 0xC6, 0x1F, 0xF8 },
    /* -------------------------------------------------------------------- */
    /*  This pink color is used for drawing rectangles around sprites.      */
    /*  It's a temporary hack.                                              */
    /* -------------------------------------------------------------------- */
    { 0xFF, 0x80, 0x80}
};

/*
 * ============================================================================
 *  GFX_INIT         -- Initializes a gfx_t object.
 * ============================================================================
 */
int gfx_init (gfx_t   *gfx, int x, int y, int bpp, int fs)
{
#pragma unused( fs )
    int             i;
    Rect            bounds;
    GDHandle        screensDevice;
    short           depth = 8;
    CTabHandle      gClut;

    
    /* -------------------------------------------------------------------- */
    /*  Sanity checks and cleanups.                                         */
    /* -------------------------------------------------------------------- */
    assert(gfx);
    memset((void*)gfx, 0, sizeof(gfx_t));
    

    /* -------------------------------------------------------------------- */
    /*  TEMPORARY:  For now, only support 320x200x8bpp.                     */
    /* -------------------------------------------------------------------- */
    if (x != 320 || y != 200 || bpp != 8)
    {
        fprintf(stderr, "gfx panic:  Right now, only 320x200x8bpp mode is "
                        "supported.\n");
        return -1;
    }

    /* -------------------------------------------------------------------- */
    /*  Allocate memory for the gfx_t.                                      */
    /* -------------------------------------------------------------------- */
    gfx->vid = calloc(160, 200);
    gfx->pvt = calloc(1, sizeof(gfx_pvt_t));

    if (!gfx->vid && !gfx->pvt)
    {
        fprintf(stderr, "gfx panic:  Could not allocate memory.\n");
        return -1;
    }

    /* -------------------------------------------------------------------- */
    /*  These routines opens a new window (data from a resource file        */
    /*  then gets a color table (again from a resource file) then creates   */
    /*  an offscreen GWorld to draw into (of the same size).                */
    /* -------------------------------------------------------------------- */
    
    gfx->pvt->wind = GetNewWindow( 4000, NULL, (WindowPtr)-1L );
    
    ShowWindow( gfx->pvt->wind );
    
    SetRect( &bounds, 0, 0,
             gfx->pvt->wind->portRect.right - gfx->pvt->wind->portRect.left,
             gfx->pvt->wind->portRect.bottom - gfx->pvt->wind->portRect.top );


    gClut = GetCTable( 130 );

    screensDevice = GetMainDevice();
    if (screensDevice != nil)
        (**gClut).ctSeed = (**(**(**screensDevice).gdPMap).pmTable).ctSeed;
        
    NewGWorld( &(gfx->pvt->scr), depth, &bounds, gClut, nil, 0 );
    
    gfx->pvt->thePixMap = GetGWorldPixMap( gfx->pvt->scr ); 
    

    /* -------------------------------------------------------------------- */
    /*  This is ignored on the mac (temporary)                              */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 17; i++)
    {
        gfx->pvt->pal_on [i].red   = gfx_stic_palette[i][0];
        gfx->pvt->pal_on [i].green = gfx_stic_palette[i][1];
        gfx->pvt->pal_on [i].blue  = gfx_stic_palette[i][2];
        gfx->pvt->pal_off[i].red   = gfx_stic_palette[i][0] >> 1;
        gfx->pvt->pal_off[i].green = gfx_stic_palette[i][1] >> 1;
        gfx->pvt->pal_off[i].blue  = gfx_stic_palette[i][2] >> 1;
    }
    gfx->pvt->vid_enable = 0;

    /* -------------------------------------------------------------------- */
    /*  Set up the gfx_t's internal structures.                             */
    /* -------------------------------------------------------------------- */
    gfx->periph.read        = NULL;
    gfx->periph.write       = NULL;
    gfx->periph.tick        = gfx_tick;
    gfx->periph.min_tick    = 14914;
    gfx->periph.max_tick    = 14915;
    gfx->periph.addr_base   = 0;
    gfx->periph.addr_mask   = 0;


    return 0;
}

/*
 * ============================================================================
 *  GFX_SET_TITLE    -- Sets the window title - unused on the mac
 * ============================================================================
 */
int gfx_set_title(gfx_t *gfx, const char *title)
{
    UNUSED(gfx);
    UNUSED(title);
    /*
    SDL_WM_SetCaption(title, title);
    */
    
    return 0;
}

/*
 * ============================================================================
 *  GFX_TICK         -- Services a gfx_t tick
 * ============================================================================
 */
uint_32 gfx_tick(periph_p gfx_periph, uint_32 len)
{
    gfx_t   *gfx = (gfx_t*) gfx_periph;
    uint_8  *vid, *scr, *scr1, pix;
    int i, j, theRowBytes;
    WindowPtr   pushPort;

    gfx->tot_frames ++;

    /* -------------------------------------------------------------------- */
    /*  Drop a frame if we need to.                                         */
    /* -------------------------------------------------------------------- */
    if (gfx->drop_frame)
    {
        gfx->drop_frame--;
        if (gfx->dirty) gfx->dropped_frames++;
        return len;
    }

    /* -------------------------------------------------------------------- */
    /*  Don't bother if display isn't dirty.                                */
    /* -------------------------------------------------------------------- */
    if (!gfx->dirty)
    {
        return len;
    }

    /* -------------------------------------------------------------------- */
    /*  DEBUG: Report blocks of dropped frames.                             */
    /* -------------------------------------------------------------------- */
    if (gfx->dropped_frames)
    {
#if 0
        printf("Dropped %d frames.\n", gfx->dropped_frames);
        fflush(stdout);
#endif
        gfx->tot_dropped_frames += gfx->dropped_frames;
        gfx->dropped_frames = 0;
    }

    /* -------------------------------------------------------------------- */
    /*  Draw the frame to the offscreen GWorld.                             */
    /* -------------------------------------------------------------------- */

    LockPixels(gfx->pvt->thePixMap );
    scr1 = (uint_8 *) GetPixBaseAddr( gfx->pvt->thePixMap );
    
    vid = (uint_8 *) gfx->vid;
    
    /* -------------------------------------------------------------------- */
    /*  A pixmap's rows are always longer than the number of bytes it       */
    /*  would normally take to contain the data.  We use this rowBytes      */
    /*  field to determine how many bytes between each row of pixels.       */
    /* -------------------------------------------------------------------- */
    theRowBytes = (*(*(gfx->pvt->thePixMap))).rowBytes & 0x3FFF;
                        /* 0x3fff is to mask out the flags ^^^^ */

    /* -------------------------------------------------------------------- */
    /*  Block copy the 160x200 STIC image to the display with pixel         */
    /*  replication.                                                        */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 200; i++)
    {
        scr = scr1 + i * theRowBytes;
    
        for( j = 0; j < 160 ; j++)
        {
            pix    = *vid++;
            
            scr[0] = scr[1] = pix;
            scr   += 2;
        }
    }
    
    UnlockPixels( gfx->pvt->thePixMap );

    /* -------------------------------------------------------------------- */
    /*  Actually update the display.                                        */
    /* -------------------------------------------------------------------- */
    GetPort( &pushPort );
    SetPort( gfx->pvt->wind );
    
    LockPixels(gfx->pvt->thePixMap );
    CopyBits( (BitMap*)*(gfx->pvt->thePixMap), &(gfx->pvt->wind->portBits),
                &(**(gfx->pvt->thePixMap)).bounds,
                &(gfx->pvt->wind->portRect), srcCopy, 0l );
    UnlockPixels( gfx->pvt->thePixMap );

    SetPort( pushPort );

    gfx->dirty = 0;

    return len;
}

/*
 * ============================================================================
 *  GFX_VID_ENABLE   -- Alert gfx that video has been enabled or blanked
 * ============================================================================
 */
void gfx_vid_enable(gfx_t *gfx, int enabled)
{
    /* -------------------------------------------------------------------- */
    /*  Force 'enabled' to be 0 or 1.                                       */
    /* -------------------------------------------------------------------- */
    enabled = enabled != 0;

    /* -------------------------------------------------------------------- */
    /*  Update the palette if there's been a change in blanking state.      */
    /* -------------------------------------------------------------------- */
    if (gfx->pvt->vid_enable ^ enabled)
    {
    /*            Not implemented yet.                                      */
    ;
    }

    gfx->pvt->vid_enable = enabled;
}

/*
 * ============================================================================
 *  GFX_SET_BORD     -- Set the border / offset parameters for the display
 * ============================================================================
 */
void gfx_set_bord
(
    gfx_t *gfx,         /*  Graphics object.                        */
    int x_blank,        /*  Blanking along the left 8 columns.      */
    int y_blank,        /*  Blanking along the top 16 rows.         */
    int x_delay,        /*  Horizontal delay (0..7)                 */
    int y_delay,        /*  Vertical delay (0..14)                  */
    int b_color
)
{
    int dirty = 0;

    if (gfx->x_blank != x_blank) { gfx->x_blank = x_blank; dirty = 1; }
    if (gfx->y_blank != y_blank) { gfx->y_blank = y_blank; dirty = 1; }
    if (gfx->x_delay != x_delay) { gfx->x_delay = x_delay; dirty = 1; }
    if (gfx->y_delay != y_delay) { gfx->y_delay = y_delay; dirty = 1; }
    if (gfx->b_color != b_color) { gfx->b_color = b_color; dirty = 3; }

    if (dirty)     { gfx->dirty   = 1; }
    if (dirty & 2) { gfx->b_dirty = 1; }
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
/*                 Copyright (c) 1998-1999, Joseph Zbiciak                  */
/*        Portions Copyright (c) 1999, Tim Lindner                          */
/* ======================================================================== */
