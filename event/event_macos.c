/*
 * ============================================================================
 *  Title:    Event Handling Subsystem
 *  Author:   J. Zbiciak, tim lindner
 *  $Id: event_macos.c,v 1.4 1999/10/10 08:44:28 im14u2c Exp $
 * ============================================================================
    This port of the event subsystem calls MacOS' WaitNextEvent. Contrary to to
    its name, this function does not wait for an event to occur. If there is no
    event the a NULL event is passed.
 * ============================================================================
 *  EVENT_INIT       -- Initializes the Event Subsystem
 *  EVENT_TICK       -- Processes currently pending events in the event queue
 *  EVENT_MAP        -- Map an event to a set/clear mask and a word pointer.
 * ============================================================================
 */


#include <Fonts.h>
#include <Quickdraw.h>
#include <Windows.h>
#include <Memory.h>
#include <Menus.h>
#include <Events.h>
#include <Devices.h>
#include <Resources.h>
#include <SegLoad.h>
#include <Sound.h>
#include <ToolUtils.h>
#include <Processes.h>
#include "SIOUX.h"

#include "../config.h"
#include "periph/periph.h"
#include "pads/pads.h"
#include "event.h"
#include "gfx/gfx.h"
#include "speed/speed.h"
#include "keyboard_macos.h"
#include "event_tbl.h"

uint_32             event_count;
int                 yieldTime;

#define appleID         128         
#define appleMenu       0
#define aboutMeCommand  1

#define fileID          129
#define startCommand    1
#define printCommand    3
#define quitCommand     7

#define editID          130

extern MenuHandle           mymenu1, mymenu0;
extern gfx_t                gfx;
extern speed_t              speed;
extern int                  please_die;

void event_menu(long mResult);
void event_windowupdate ( WindowPtr theWindow );
void event_handlekey( periph_p p, int key_updn, char theKey  );
void event_handlemousedown( EventRecord myEvent );

/*
 * ============================================================================
 *  EVENT_INIT       -- Initializes the Event subsystem.
 * ============================================================================
 */
int event_init(event_t *event)
{
    /* -------------------------------------------------------------------- */
    /*  The event 'peripheral' is ticked every so often in order to         */
    /*  drain input events from the event queue and post inputs to the      */
    /*  emulator.                                                           */
    /* -------------------------------------------------------------------- */
    event->periph.read      = NULL;
    event->periph.write     = NULL;
    event->periph.tick      = event_tick;
    event->periph.min_tick  = 3579545 / 480;    /* 120Hz */
    event->periph.max_tick  = 3579545 / 120;    /*  30Hz */
    event->periph.addr_base = ~0U;
    event->periph.addr_mask = 0;

    /* -------------------------------------------------------------------- */
    /*  Allocate the event lookup table (this will be huge!).               */
    /* -------------------------------------------------------------------- */
#define EVENT_LAST          1000
 
    event->mask_tbl         = calloc (sizeof(event_mask_t), EVENT_LAST);
    event->max_event        = EVENT_LAST;
    
    SetEventMask( everyEvent );
    if (!event->mask_tbl)
    {
        fprintf(stderr, "event_init: Unable to allocate event mask table\n");
        return -1;
    }

    /* -------------------------------------------------------------------- */
    /*  Drain the event queue right now to clear any initial events.        */
    /* -------------------------------------------------------------------- */
    FlushEvents(everyEvent, 0);
    yieldTime = 0;
    event_count = 0;

    /* -------------------------------------------------------------------- */
    /*  Done!                                                               */
    /* -------------------------------------------------------------------- */
    return 0;
}

/*
 * ============================================================================
 *  EVENT_TICK       -- Processes currently pending events in the event queue
 * ============================================================================
 */
uint_32 event_tick(periph_p p, uint_32 len)
{
    EventRecord         myEvent;
    
    /* -------------------------------------------------------------------- */
    /*  Now, process all pending events.                                    */
    /* -------------------------------------------------------------------- */

    if (WaitNextEvent(everyEvent, &myEvent, yieldTime, nil))
    {
        if ( !SIOUXHandleOneEvent( &myEvent ) )
        {
            switch (myEvent.what)
            {
                case updateEvt:
                    event_windowupdate((WindowPtr)(myEvent.message));
                    break;
                case mouseDown:
                    event_handlemousedown( myEvent );
                    break;
                case keyUp:
                {
                    char theChar = (myEvent.message & keyCodeMask) >> 8;
                    event_handlekey( p, 0, theChar  );
                }
                    break;
                case keyDown:
                {
                    char theChar = (myEvent.message & keyCodeMask) >> 8;
                    
                    if ((myEvent.modifiers & cmdKey) != 0)
                        event_menu(MenuKey(theChar));
                    else
                        event_handlekey( p, 1, theChar  );
                }
                    break;
                case 15:
                    if ((myEvent.message << 31) == 0)
                    { /* Suspend */
                        yieldTime = 30;
                    }
                    else
                    { /* Resume */
                        yieldTime = 0;
                    }
                    break;
                default:
                    break;
            }
        }

        /* ---------------------------------------------------------------- */
        /*  Increment the global event counter.                             */
        /* ---------------------------------------------------------------- */
        event_count++;
    }

    return len;
}

/*
 * ============================================================================
 *  EVENT_MAP        -- Maps an event to a particular AND/OR mask set
 *
 *  NOTE:  This implementation currently uses a gawdawful slow linear search
 *  to look up event names.  Eventually, I'll sort the event name list and
 *  use a binary search, which should be many times faster.  I can justify
 *  this slow lookup for now since this event mapping only occurs at startup.
 * ============================================================================
 */
int event_map
(
    event_t     *event,         /* Event_t structure being set up.          */
    const char  *name,          /* Name of event to map.                    */
    v_uint_32   *word,          /* Word modified by event, (NULL to ignore) */
    uint_32     and_mask[2],    /* AND masks for event up/down.             */
    uint_32     or_mask[2]      /* OR masks for event up/down.              */
)
{
    const char *s1;
    char buf[64], *s2;
    int  i, j, num;

    /* -------------------------------------------------------------------- */
    /*  Copy the event name to our local buffer and capitalize it.          */
    /* -------------------------------------------------------------------- */
    i  = 63;
    s1 = name;
    s2 = buf;
    while (i-->0 && *s1)
        *s2++ = toupper(*s1++);
    *s2 = 0;

    /* -------------------------------------------------------------------- */
    /*  Step through the event names and try to find a match.               */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < event_name_count; i++)
        if (!strcmp(buf, event_names[i].name))
            break;

    if (i == event_name_count)
    {
        fprintf(stderr, "event_map:  Invalid event name '%s'\n", buf);
        return -1;
    }

    /* -------------------------------------------------------------------- */
    /*  Sanity check.                                                       */
    /* -------------------------------------------------------------------- */
    if (event_names[i].event_num > event->max_event)
    {
        fprintf(stderr, "event_map:  Event number %d is too large (max %d)\n",
                event_names[i].event_num, event->max_event);
        return -1;
    }

    num = event_names[i].event_num;

    /* -------------------------------------------------------------------- */
    /*  Register ourselves with this event.                                 */
    /* -------------------------------------------------------------------- */
    event->mask_tbl[num].word = word;

    for (j = 0; j < 2; j++)
    {
        event->mask_tbl[num].and_mask[j] = and_mask[j];
        event->mask_tbl[num].or_mask [j] = or_mask [j];
    }

    /* -------------------------------------------------------------------- */
    /*  Done:  Return success.                                              */
    /* -------------------------------------------------------------------- */
    return 0;
}


/*
 * ============================================================================
 *  event_menu        -- Decodes and handle the various menu commands
 * ============================================================================
 */
void event_menu(long mResult)
{
    int                     theMenu, theItem;
    Str255                  daName;
    GrafPtr                 savePort;

    theItem = LoWord(mResult);
    theMenu = HiWord(mResult);
    
    switch (theMenu) {
/*
 * ============================================================================
 *  Do Apple Menu
 * ============================================================================
 */
        case appleID:
            if (theItem == aboutMeCommand)
                ;
            else {
                GetMenuItemText(mymenu0, theItem, daName);
                GetPort(&savePort);
                (void) OpenDeskAcc((ConstStr255Param)daName);
                SetPort(savePort);
            }
            break;
/*
 * ============================================================================
 *  Do File Menu
 * ============================================================================
 */
        case fileID:
            switch (theItem) {
                case startCommand:
                    ; //start();
                    break;
                case printCommand:
                    break;
                case quitCommand:
                    please_die = 1;
                    break;
                default:
                    break;
                }
            break;
/*
 * ============================================================================
 *  Do Edit Menu
 * ============================================================================
 */
        case editID:
            switch (theItem) {
                default:
                    break;
                }
            break;
        default:
            break;
        }
    HiliteMenu(0);
    return;
}

/*
 * ============================================================================
 *  event_windowupdate        -- local function that handles the MacOS portion
 *                             of window updates
 * ============================================================================
 */
void event_windowupdate( WindowPtr theWindow )
{
    GrafPtr     savePort;
    
    GetPort ( &savePort );
    SetPort ( theWindow );
    BeginUpdate ( theWindow );
    
    gfx.dirty = 1;  /* flag to update the games window */

    EndUpdate(theWindow);

    SetPort ( savePort );
    
}

void event_handlekey( periph_p p, int key_updn, char theKey  )
{
    event_t         *event = (event_t*)p;
    event_mask_t    *mask_tbl;


        /* ---------------------------------------------------------------- */
        /*  Sanity check the event number, to make sure it's within our     */
        /*  valid range of events.  If the event number appears to be for   */
        /*  an upper-case letter, convert it to lower-case.                 */
        /* ---------------------------------------------------------------- */
//        if (theKey >= 'A' && theKey <= 'Z')
//            theKey = tolower(theKey);

        /* ---------------------------------------------------------------- */
        /*  Process the event.  If event->mask_tbl[ev_num].word == NULL,    */
        /*  then we aren't interested in this event.                        */
        /* ---------------------------------------------------------------- */

        mask_tbl = &event->mask_tbl[theKey];
        if (mask_tbl->word == NULL)
            return;

        /* ---------------------------------------------------------------- */
        /*  Apply the appropriate AND and OR masks to the word.             */
        /* ---------------------------------------------------------------- */
        *mask_tbl->word &= mask_tbl->and_mask[key_updn];
        *mask_tbl->word |= mask_tbl->or_mask [key_updn];
}

void event_handlemousedown( EventRecord  myEvent )
{
    WindowPtr        whichWindow;
    
    switch (FindWindow(myEvent.where, &whichWindow))
    {
        case inSysWindow:
            SystemClick(&myEvent, whichWindow);
            break;
        case inMenuBar:
            event_menu(MenuSelect(myEvent.where));
            break;
        case inDrag:
        {
            Rect    boundsRect = (*GetGrayRgn())->rgnBBox;
            DragWindow ( whichWindow, myEvent.where, &boundsRect );
        }
            break;
        case inContent: 
            BringToFront(whichWindow);
            break;
        default:
            break;
    }
    speed_resync( &speed );
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
/* ======================================================================== */
