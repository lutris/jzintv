/*
 * ============================================================================
 *  Title:    
 *  Author:   J. Zbiciak
 * ============================================================================
 *
 * ============================================================================
 *
 * ============================================================================
 */
#ifndef _EVENT_H_
#define _EVENT_H_


/*
 * ============================================================================
 *  EVENT_NAME_T     -- Structure used to map event names to numbers.
 * ============================================================================
 */
typedef struct event_name_t
{
    const char *name;           /* Printable name for event.        */
    uint_32     event_num;      /* Event index number into mask_tbl */
} event_name_t;

/*
 * ============================================================================
 *  EVENT_MASK_T     -- Structure containing AND/OR masks for an event.
 * ============================================================================
 */
typedef struct event_mask_t
{
    v_uint_32   *word;          /* Word to mask, or NULL if none.   */
    uint_32     and_mask[2];    /* AND masks (up/down)              */
    uint_32     or_mask [2];    /* OR masks (up/down)               */
} event_mask_t;


/*
 * ============================================================================
 *  EVENT_T          -- Event Subsystem object
 * ============================================================================
 */
typedef struct evt_pvt_t evt_pvt_t;
typedef struct event_t
{
    periph_t    periph;         /* Yes, it's a peripheral.  Surprise!       */

    event_mask_t*mask_tbl[4];   /* Event mask tables.                       */
    uint_32     max_event;      /* Highest event number supported.          */
    uint_32     change_kbd;     /* Keyboard map change request.             */
    uint_32     cur_kbd;        /* Current keyboard mapping                 */
    uint_32     prv_kbd;        /* Previous keyboard mapping                */

    double      soon;           /* When to begin processing events again    */
    double      coalesce_time;  /* How long to delay when processing combos */

    evt_pvt_t   *pvt;           /* Private structure                        */
} event_t;

/*
 * ============================================================================
 *  EVENT_INIT       -- Initializes the Event subsystem.
 * ============================================================================
 */
int event_init(event_t *event, int enable_mouse);

/*
 * ============================================================================
 *  EVENT_TICK       -- Processes currently pending events in the event queue
 * ============================================================================
 */
uint_32 event_tick(periph_p p, uint_32 len);

enum { DOWN = 1, UP = 0 } ;

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
    int         map_num,        /* Keyboard mapping number                  */
    v_uint_32   *word,          /* Word modified by event, (NULL to ignore) */
    uint_32     and_mask[2],    /* AND masks for event up/down.             */
    uint_32     or_mask[2]      /* OR masks for event up/down.              */
);

/*
 * ============================================================================
 *  EVENT_COMBINE    -- Register a combo event as COMBOxx
 * ============================================================================
 */
int event_combine
(
    event_t     *event,
    const char  *name1,
    const char  *name2,
    int         combo_num
);


extern uint_32 event_count;

#endif /*EVENT_H*/
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



