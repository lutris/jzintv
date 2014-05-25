/*
 * ============================================================================
 *  Title:    Platform Portability "Library":  Macintosh Specific
 *  Author:   T. Lindner, J. Zbiciak
 *  $Id: plat_mac.c,v 1.5 1999/10/10 08:44:30 im14u2c Exp $
 * ============================================================================
 *  This module fills in missing features on various platforms.
 * ============================================================================
 *  GETTIMEOFDAY     -- Return current time in seconds/microseconds.
 *  STRDUP           -- Copy a string into freshly malloc'd storage.
 *  PLAT_DELAY       -- Sleep w/ millisecond precision.
 * ============================================================================
 */

 
#include <Fonts.h>
#include <Quickdraw.h>
#include <Windows.h>
#include <Memory.h>
#include <Menus.h>
#include <Events.h>
#include <Resources.h>
#include <SegLoad.h>
#include <Sound.h>
#include <ToolUtils.h>
#include <Processes.h>
#include "../config.h"
#include "SIOUX.h"
#include "plat.h"

#define MS_PER_TICK     (1000/60)               /* MacOS tick = 1/60 second */

MenuHandle          mymenu1, mymenu0;

int plat_init(void)
{

    SIOUXSettings.standalone = true;
    SIOUXSettings.setupmenus = false;
    SIOUXSettings.asktosaveonclose = false;
    
    SIOUXSettings.initializeTB = false; 
    
/*_____________________________________________________*/
/*                    Set menus                         */
/*______________________________________________________*/
    
    InitGraf(&qd.thePort);
    InitFonts();
    FlushEvents(everyEvent, 0);
    InitWindows();
    InitMenus();
    TEInit();
    InitDialogs(nil);
    InitCursor();

    printf( "Initializing Macintosh.\n\n" );
    
    SetMenuBar (GetNewMBar (128));
    mymenu0 = GetMenuHandle (128);
    AppendResMenu(mymenu0, 'DRVR');
    mymenu1 = GetMenuHandle(129);
    DrawMenuBar();

    return 0;
}


/* ======================================================================== */
/*  MACINTOSH-SPECIFIC VERSIONS...                                          */
/* ======================================================================== */

#ifdef macintosh

#include <Timer.h>
#include <Math64.h>

/*
 * ============================================================================
 *  GETTIMEOFDAY     -- Return current time in seconds/microseconds.
 * ============================================================================
 */
void gettimeofday( struct timeval  *tv, int *i )
{
#pragma unused( i )

    SInt64  number;
    UnsignedWide macTime;
    
    Microseconds( &macTime );

    number = S64Div ( WideToSInt64( macTime ), 1000000 );
    
    tv->tv_sec  = number;
    tv->tv_usec = S64Subtract( WideToSInt64( macTime ), number * 1000000 );;
}

/*
 * ============================================================================
 *  PLAT_DELAY       -- Sleep w/ millisecond precision.
 * ============================================================================
 */
void plat_delay( unsigned i )
{
    Delay(i/MS_PER_TICK, nil);
}

#endif /* macintosh */


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
/*           Copyright (c) 1998-1999, Joseph Zbiciak, Tim Lindner           */
/* ======================================================================== */
