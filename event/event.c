/*
 * ============================================================================
 *  Title:    Event Handling Subsystem
 *  Author:   J. Zbiciak
 *  $Id: event.c,v 1.10 2001/02/03 02:34:21 im14u2c Exp $
 * ============================================================================
 *  The Event subsystem receives input from the keyboard and/or joystick.
 *  Currently, only keyboard input is being processed, as the joystick
 *  driver in SDL is not yet written.
 *
 *  EVENT notifies other subsystems of events by setting flags.  When
 *  an event comes in, the event is looked up in a table and bits in a
 *  uint_32 are set/cleared according to masks assigned to the event.
 *  The masks are specified as four uint_32's:  "AND" and "OR" masks
 *  for a "down" event, and "AND" and "OR" masks for an "up" event.
 *
 *  Event->mask mappings are registered with EVENT via calls to "event_map".
 *  Each event can only be mapped to ONE set of masks, and so the order
 *  in which events are mapped determines the final set of mappings.  (This
 *  allows a specialized config file to override a standard config.)  Mapping
 *  an event to a NULL pointer or to an empty set/clear mask disables the
 *  event.
 * ============================================================================
 *  The following event classes are handled by the EVENT subsystem:
 *
 *   -- Quit events       (eg. somebody hitting the little 'X' in the corner)
 *   -- Keyboard events   (key down / key up)
 *   -- Joystick events   (not yet implemented)
 *   -- Mouse events      (not yet implemented, may never be implemented)
 *   -- Activation events (hide / unhide window)
 *
 *  Event symbol names are assigned in 'event.h', and are stored as strings.
 *  This should simplify dynamic configuration from CFG files.  For 
 *  simplicity's sake, I will convert joystick and quit events to 
 *  keyboard-like events.
 *
 *  While this code currently relies on SDL's event delivery mechanism,
 *  I'm taking great pains to keep it largely independent of SDL.  Here's
 *  hoping I succeed.
 * ============================================================================
 *  EVENT_INIT       -- Initializes the Event Subsystem
 *  EVENT_TICK       -- Processes currently pending events in the event queue
 *  EVENT_MAP        -- Map an event to a set/clear mask and a word pointer.
 * ============================================================================
 */

#include "../config.h"
#include "periph/periph.h"
#include "pads/pads.h"
#include "sdl.h"
#include "joy/joy.h"
#include "mouse/mouse.h"
#include "event.h"
#include "event_tbl.h"
#include "cp1600/cp1600.h"
#include "cp1600/emu_link.h"


struct evt_pvt_t
{
    uint_32 half_combos;            /* combos that might be forming         */
    uint_32 full_combos;            /* combos actively asserted             */
    uint_32 allocated_combos;       /* combos that are currently defined    */

    uint_32 combo_event[64][2];     /* event numbers associated w/ combos   */
    int     combo_event_cnt;        /* total # of unique events in combos   */

    uint_32 combo_pairs[32][2];     /* event #s assoc'd with each combo     */
    
    uint_32 in_combo[(EVENT_COMBO0 + 31) >> 5]; /* Bmap: event's in a combo */
    uint_32 key_dn  [(EVENT_COMBO0 + 31) >> 5]; /* Bmap: key is down        */
};


uint_32 event_count = 0;
static int mouse_enabled = 0;
static const char *idx_to_name[EVENT_LAST];
LOCAL int event_emu_link(cp1600_t *, int *);


#define EV_Q_LEN (64)
/* Event history queue, for emulink mode. */
LOCAL int     ev_updn_q[EV_Q_LEN];
LOCAL uint_32 ev_num_q [EV_Q_LEN];
LOCAL uint_32 ev_q_wr, ev_q_el_rd, ev_q_ev_rd;

LOCAL void event_dtor(periph_p);
LOCAL int  event_chk_combo_press  (event_t *event, uint_32 ev_num);
LOCAL int  event_chk_combo_release(event_t *event, uint_32 ev_num);

/* ======================================================================== */
/*  EVENT_INIT       -- Initializes the Event subsystem.                    */
/* ======================================================================== */
int event_init(event_t *event, int enable_mouse)
{
    SDL_Event dummy;
    int i;

    /* -------------------------------------------------------------------- */
    /*  The event 'peripheral' is ticked every so often in order to         */
    /*  drain input events from the event queue and post inputs to the      */
    /*  emulator.                                                           */
    /* -------------------------------------------------------------------- */
    event->periph.read      = NULL;
    event->periph.write     = NULL;
    event->periph.peek      = NULL;
    event->periph.poke      = NULL;
    event->periph.tick      = event_tick;
    event->periph.min_tick  = 3579545 / 480;    /* 120Hz */
    event->periph.max_tick  = 3579545 / 480;    /* 120Hz */
    event->periph.addr_base = ~0U;
    event->periph.addr_mask = 0;
    event->periph.dtor      = event_dtor;

    /* -------------------------------------------------------------------- */
    /*  Set up our event-corking variables.                                 */
    /* -------------------------------------------------------------------- */
    event->soon             = 0.;
    event->coalesce_time    = 0.0001;   /* 1ms */

    /* -------------------------------------------------------------------- */
    /*  Allocate the event lookup table (this will be huge!).               */
    /* -------------------------------------------------------------------- */
    event->mask_tbl[0]      = CALLOC(event_mask_t, EVENT_LAST * 4);
    event->max_event        = EVENT_LAST;

    if (!event->mask_tbl[0])
    {
        fprintf(stderr, "event_init: Unable to allocate event mask table\n");
        return -1;
    }
    event->mask_tbl[1]      = event->mask_tbl[0] + EVENT_LAST;
    event->mask_tbl[2]      = event->mask_tbl[1] + EVENT_LAST;
    event->mask_tbl[3]      = event->mask_tbl[2] + EVENT_LAST;

    for (i = 0; i < EVENT_LAST; i++)
        idx_to_name[i] = "bad";
    for (i = 0; i < event_name_count; i++)
    {
        if (!strcmp(event_names[i].name, " "))
            continue;

        idx_to_name[event_names[i].event_num] = event_names[i].name;
    }

    /* -------------------------------------------------------------------- */
    /*  Set up our "private" structure.  Mostly handles combo tracking.     */
    /* -------------------------------------------------------------------- */
    event->pvt = CALLOC(evt_pvt_t, 1);

    /* -------------------------------------------------------------------- */
    /*  Set up SDL to filter out the events we're NOT interested in...      */
    /* -------------------------------------------------------------------- */
    if (!enable_mouse)
    {
        SDL_EventState(SDL_MOUSEMOTION,         SDL_IGNORE);
        SDL_EventState(SDL_MOUSEBUTTONDOWN,     SDL_IGNORE);
        SDL_EventState(SDL_MOUSEBUTTONUP,       SDL_IGNORE);
    } else
    {
        mouse_enabled = 1;
        SDL_EventState(SDL_MOUSEMOTION,         SDL_ENABLE);
        SDL_EventState(SDL_MOUSEBUTTONDOWN,     SDL_ENABLE);
        SDL_EventState(SDL_MOUSEBUTTONUP,       SDL_ENABLE);
    }
#ifdef N900
    SDL_EventState(SDL_SYSWMEVENT,          SDL_ENABLE);
#else
    SDL_EventState(SDL_SYSWMEVENT,          SDL_IGNORE);
#endif

    /* -------------------------------------------------------------------- */
    /*  ...and leave us only with the events we ARE interested in.          */
    /* -------------------------------------------------------------------- */
    SDL_EventState(SDL_ACTIVEEVENT,         SDL_ENABLE);
    SDL_EventState(SDL_KEYDOWN,             SDL_ENABLE);
    SDL_EventState(SDL_KEYUP,               SDL_ENABLE);
    SDL_EventState(SDL_QUIT,                SDL_ENABLE);
#ifdef WII
    SDL_EventState(SDL_JOYAXISMOTION,       SDL_IGNORE);
    SDL_EventState(SDL_JOYHATMOTION,        SDL_IGNORE);
    SDL_EventState(SDL_JOYBUTTONDOWN,       SDL_IGNORE);
    SDL_EventState(SDL_JOYBUTTONUP,         SDL_IGNORE);
    SDL_EventState(SDL_JOYBALLMOTION,       SDL_IGNORE);
    SDL_JoystickEventState(SDL_IGNORE);
#else
    SDL_EventState(SDL_JOYAXISMOTION,       SDL_ENABLE);
    SDL_EventState(SDL_JOYHATMOTION,        SDL_ENABLE);
    SDL_EventState(SDL_JOYBUTTONDOWN,       SDL_ENABLE);
    SDL_EventState(SDL_JOYBUTTONUP,         SDL_ENABLE);
    SDL_EventState(SDL_JOYBALLMOTION,       SDL_ENABLE);
    SDL_JoystickEventState(SDL_ENABLE);
#endif
    /* -------------------------------------------------------------------- */
    /*  Drain the event queue right now to clear any initial events.        */
    /* -------------------------------------------------------------------- */
    while (SDL_PollEvent(&dummy))
        ;

    /* -------------------------------------------------------------------- */
    /*  Register us on Emu-Link as major API #9.                            */
    /* -------------------------------------------------------------------- */
    emu_link_register(event_emu_link, 9);

    /* -------------------------------------------------------------------- */
    /*  Done!                                                               */
    /* -------------------------------------------------------------------- */
    return 0;
}

/* ======================================================================== */
/*  EVENT_DTOR   -- Tear down the event engine.                             */
/* ======================================================================== */
void event_dtor(periph_p p)
{
    event_t *event = (event_t *)p;

    CONDFREE(event->mask_tbl[0]);

    event->mask_tbl[0] = NULL;
    event->mask_tbl[1] = NULL;
    event->mask_tbl[2] = NULL;
    event->mask_tbl[3] = NULL;

    ev_q_el_rd = ev_q_ev_rd = ev_q_wr = 0;
    event_count = 0;

    joy_dtor();
}

/* ======================================================================== */
/*  EVENT_QUEUE  -- Internal event queue containing expanded events.        */
/*                  This also gets used by EMULINK.                         */
/* ======================================================================== */
LOCAL void event_queue(event_t *event, int ev_updn, uint_32 ev_num)
{
    /* -------------------------------------------------------------------- */
    /*  Ignore events that are out of range or marked "ignore"              */
    /* -------------------------------------------------------------------- */
    if (ev_num > event->max_event || ev_num == EVENT_IGNORE)
        return;

    /* -------------------------------------------------------------------- */
    /*  Canonicalize alphabetic events.                                     */
    /* -------------------------------------------------------------------- */
    if (ev_num >= 'A' && ev_num <= 'Z')
        ev_num = tolower(ev_num);

    /* -------------------------------------------------------------------- */
    /*  Drop oldest EMULINK event on overflow                               */
    /* -------------------------------------------------------------------- */
    if (ev_q_wr - ev_q_el_rd == EV_Q_LEN)
        ev_q_el_rd++;

    /* -------------------------------------------------------------------- */
    /*  We should never drop events internally.                             */
    /* -------------------------------------------------------------------- */
    assert(ev_q_wr - ev_q_ev_rd < EV_Q_LEN);

    /* -------------------------------------------------------------------- */
    /*  Remember the current event.                                         */
    /* -------------------------------------------------------------------- */
    ev_num_q [ev_q_wr % EV_Q_LEN] = ev_num;
    ev_updn_q[ev_q_wr % EV_Q_LEN] = ev_updn;

    ev_q_wr++;
}

/* ======================================================================== */
/*  EVENT_DEQUEUE   Dequeue events queued by event_queue.  Does not         */
/*                  disturb the EMULINK queue pointer.                      */
/* ======================================================================== */
LOCAL int event_dequeue(int *ev_updn, uint_32 *ev_num)
{
    int q_rd;

    if (ev_q_ev_rd == ev_q_wr)
        return 0;

    while (ev_q_el_rd >= EV_Q_LEN && ev_q_ev_rd >= EV_Q_LEN)
    {
        ev_q_el_rd -= EV_Q_LEN;
        ev_q_ev_rd -= EV_Q_LEN;
        ev_q_wr    -= EV_Q_LEN;
    }

    q_rd = ev_q_ev_rd++ % EV_Q_LEN;

    *ev_updn = ev_updn_q[q_rd];
    *ev_num  = ev_num_q [q_rd];

    return 1;
}
    

/* ======================================================================== */
/*  EVENT_TICK   -- Processes currently pending events in the event queue   */
/* ======================================================================== */
uint_32 event_tick(periph_p p, uint_32 len)
{
    event_t         *event = (event_t*)p;
    event_mask_t    *mask_tbl;
    SDL_Event       ev;
    uint_32         ev_num; 
    int             ev_updn = 0;      /* 0 = up, 1 = down */
    double          now = get_time();

    /* -------------------------------------------------------------------- */
    /*  First, pump the event loop and gather some events.                  */
    /* -------------------------------------------------------------------- */
    SDL_PumpEvents();

    /* -------------------------------------------------------------------- */
    /*  If we've corked event queue processing, leave early if we're still  */
    /*  before the deadline.                                                */
    /* -------------------------------------------------------------------- */
    if (now < event->soon)
        return len;

    event->soon = 0.;

    /* -------------------------------------------------------------------- */
    /*  Now, process all pending events.                                    */
    /* -------------------------------------------------------------------- */
#ifdef WII
	getWiiJoyEvents();
#endif
    while (SDL_PollEvent(&ev))
    {
        switch (ev.type)
        {
            /* ------------------------------------------------------------ */
            /*  Activation events:  Only look at whether we're iconified    */
            /*  or not, and convert it to an up/down event on EVENT_HIDE.   */
            /* ------------------------------------------------------------ */
            case SDL_ACTIVEEVENT:
            {
                if (ev.active.state & SDL_APPACTIVE)
                    event_queue(event, ev.active.gain ? UP : DOWN, EVENT_HIDE);
                break;
            }

            /* ------------------------------------------------------------ */
            /*  Handle keypresses by grabbing the keysym value as event #   */
            /* ------------------------------------------------------------ */
            case SDL_KEYDOWN:
            {
                if (event_chk_combo_press(event, (uint_32)ev.key.keysym.sym))
                    goto leave;

                break;
            }

            /* ------------------------------------------------------------ */
            /*  Key releases are almost the same as keypresses...           */
            /* ------------------------------------------------------------ */
            case SDL_KEYUP:
            {
                if (event_chk_combo_release(event, (uint_32)ev.key.keysym.sym))
                    goto leave;

                break;
            }

            /* ------------------------------------------------------------ */
            /*  Outsource all the joystick event decoding...                */
            /* ------------------------------------------------------------ */
            case SDL_JOYAXISMOTION: 
            case SDL_JOYHATMOTION:  
            case SDL_JOYBUTTONDOWN: 
            case SDL_JOYBUTTONUP:   
            {
                int ex_updn;
                uint_32 ex_num = EVENT_IGNORE;

                joy_decode_event(&ev, &ev_updn, &ev_num, &ex_updn, &ex_num);

                event_queue(event, ev_updn, ev_num);
                event_queue(event, ex_updn, ex_num);
                break;
            }

            /* ------------------------------------------------------------ */
            /*  Outsource all mouse event decoding...                       */
            /* ------------------------------------------------------------ */
            case SDL_MOUSEMOTION:
            case SDL_MOUSEBUTTONDOWN:
            case SDL_MOUSEBUTTONUP:
            {
                int ex_updn;
                uint_32 ex_num = EVENT_IGNORE;

                mouse_decode_event(&ev, &ev_updn, &ev_num, &ex_updn, &ex_num);

                event_queue(event, ev_updn, ev_num);
                event_queue(event, ex_updn, ex_num);
                break;
            }

            /* ------------------------------------------------------------ */
            /*  And finally, handle the QUIT event.                         */
            /* ------------------------------------------------------------ */
            case SDL_QUIT:
            {
                event_queue(event, DOWN, EVENT_QUIT);
                break;
            }

            default:
            {
                break;
            }
        }

        while (event_dequeue(&ev_updn, &ev_num))
        {
            /* ------------------------------------------------------------ */
            /*  Process the event.  If event->mask_tbl[ev_num].word == NULL */
            /*  then we aren't interested in this event.                    */
            /* ------------------------------------------------------------ */
            mask_tbl = &event->mask_tbl[event->cur_kbd][ev_num];
            if (mask_tbl->word == NULL)
                continue;

            /* ------------------------------------------------------------ */
            /*  Apply the appropriate AND and OR masks to the word.         */
            /* ------------------------------------------------------------ */
            *mask_tbl->word &= mask_tbl->and_mask[ev_updn];
            *mask_tbl->word |= mask_tbl->or_mask [ev_updn];

            /* ------------------------------------------------------------ */
            /*  Increment the global event counter.                         */
            /* ------------------------------------------------------------ */
            event_count++;
        }
    }

    /* -------------------------------------------------------------------- */
    /*  If the mouse is enabled, see if we need to do any delayed events.   */
    /* -------------------------------------------------------------------- */
    if (mouse_enabled)
    {
        mouse_pump(&ev_updn, &ev_num);
        event_queue(event, ev_updn, ev_num);
    }

    /* -------------------------------------------------------------------- */
    /*  Done!  Return elapsed time.                                         */
    /* -------------------------------------------------------------------- */
leave:
    return len;
}

/* ======================================================================== */
/*  EVENT_LOOKUP     -- Look up an event by name, returning its number.     */
/* ======================================================================== */
LOCAL int event_lookup(const char *name, uint_32 max_event)
{
    const char *s1;
    char buf[64], *s2;
    int  i;

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
        fprintf(stderr, "event:  Invalid event name '%s'\n", buf);
        return -1;
    }

    /* -------------------------------------------------------------------- */
    /*  Sanity check.                                                       */
    /* -------------------------------------------------------------------- */
    if (event_names[i].event_num > max_event)
    {
        fprintf(stderr, "event:  Event number %d is too large (max %d)\n",
                event_names[i].event_num, max_event);
        return -1;
    }

    return event_names[i].event_num;
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
    int         map,            /* Keyboard map number to map within.       */
    v_uint_32   *word,          /* Word modified by event, (NULL to ignore) */
    uint_32     and_mask[2],    /* AND masks for event up/down.             */
    uint_32     or_mask[2]      /* OR masks for event up/down.              */
)
{
    int num, j;

    /* -------------------------------------------------------------------- */
    /*  Get the event number for this name.                                 */
    /* -------------------------------------------------------------------- */
    if ((num = event_lookup(name, event->max_event)) < 0)
        return -1;

    /* -------------------------------------------------------------------- */
    /*  Register ourselves with this event.                                 */
    /* -------------------------------------------------------------------- */
    event->mask_tbl[map][num].word = word;

    for (j = 0; j < 2; j++)
    {
        event->mask_tbl[map][num].and_mask[j] = and_mask[j];
        event->mask_tbl[map][num].or_mask [j] = or_mask [j];
    }

    /* -------------------------------------------------------------------- */
    /*  Done:  Return success.                                              */
    /* -------------------------------------------------------------------- */
    return 0;
}

/* ======================================================================== */
/*  EVENT_ADD_TO_COMBO   -- Bind a name to half of a combo.                 */
/* ======================================================================== */
LOCAL int event_add_to_combo
(
    event_t     *event, 
    const char  *name, 
    int         combo_num, 
    int         which
)
{
    int         num, idx;
    evt_pvt_t  *pvt = event->pvt;
    uint_32     event_bit = 1 << combo_num;

    if ((num = event_lookup(name, event->max_event)) < 0) return -1;

    if (num >= EVENT_COMBO0)
    {
        fprintf(stderr, "event:  Cannot use '%s' in a combo event\n", name);
        return -1;
    }

    for (idx = 0; idx < pvt->combo_event_cnt; idx++)
        if (pvt->combo_event[idx][0] == (uint_32)num)
            break;

    if (idx == pvt->combo_event_cnt)
    {
        pvt->combo_event_cnt++;
        pvt->combo_event[idx][0] = num;
        pvt->in_combo[num >> 5] |= 1 << (num & 31);
    }

    if (pvt->combo_event[idx][1] & event_bit)
    {
        fprintf(stderr, "event:  Must use two distinct events in a combo\n");
        return -1;
    }

    pvt->combo_event[idx][1] |= event_bit;

    pvt->combo_pairs[combo_num][which] = num;

    return 0;
}


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
)
{
    uint_32 event_bit = 1 << combo_num;

    if (event->pvt->allocated_combos & event_bit)
    {
        fprintf(stderr, "event:  Error: COMBO%d already in use\n", combo_num);
        return -1;
    }

    event->pvt->allocated_combos |= event_bit;

    if (event_add_to_combo(event, name1, combo_num, 0)) return -1;
    if (event_add_to_combo(event, name2, combo_num, 1)) return -1;

    return 0;
}


/* ======================================================================== */
/*  EVENT_EMU_LINK -- Allow games to get raw event feed from event queue.   */
/* ======================================================================== */
int event_emu_link(cp1600_t *cpu, int *fail)
{
    int q_rd;

    /* -------------------------------------------------------------------- */
    /*  The event Emu-Link API is very simple:                              */
    /*                                                                      */
    /*  INPUTS:                                                             */
    /*      R2 == 0x0000:  Just return event number and up/down             */
    /*      R2 != 0x0000:  Try to write ASCII name of event @R1.            */
    /*                     ASCII names are bounded to 18 chars + NUL.       */
    /*                                                                      */
    /*  OUTPUTS:                                                            */
    /*      R0:   0xFFFF = No events, otherwise event #                     */
    /*      R1:   0 = UP, 1 = DOWN                                          */
    /*      R2:   Unmodified.                                               */
    /* -------------------------------------------------------------------- */

    *fail = 0;

    if (ev_q_wr == ev_q_el_rd)
    {
        cpu->r[1] = 0;
        return 0xFFFF;
    }

    while (ev_q_el_rd >= EV_Q_LEN && ev_q_ev_rd >= EV_Q_LEN)
    {
        ev_q_el_rd -= EV_Q_LEN;
        ev_q_ev_rd -= EV_Q_LEN;
        ev_q_wr    -= EV_Q_LEN;
    }

    q_rd = ev_q_el_rd++ % EV_Q_LEN;

    if (cpu->r[2] != 0)
    {
        const char *s;
        int i;
        int addr = cpu->r[2];

        for (i = 0, s = idx_to_name[ev_num_q[q_rd]]; i < 18 && *s; i++, s++)
            CP1600_WR(cpu, addr++, *s);

        CP1600_WR(cpu, addr, 0);
    }

    cpu->r[1] = ev_updn_q[q_rd];

    return ev_num_q[q_rd];
} 

/* ======================================================================== */
/*  GET_TRAILING_1      Helper function for event_check_combo..             */
/*  Source:  http://graphics.stanford.edu/~seander/bithacks.html            */
/*  Uses a deBruijn sequence to do the bit position calc.                   */
/* ======================================================================== */
static const int mult_DeBruijn_bit_pos[32] = 
{
    0,   1, 28,  2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17,  4, 8, 
    31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18,  6, 11,  5, 10, 9
};

LOCAL int get_trailing_1(uint_32 v)
{
    return mult_DeBruijn_bit_pos[((uint_32)((v & -v) * 0x077CB531U)) >> 27];
}
    
/* ======================================================================== */
/*  EVENT_CHECK_COMBO   Look for compound keypresses, and convert them      */
/*                      into special compound events.                       */
/* ======================================================================== */
LOCAL int event_chk_combo_press(event_t *event, uint_32 ev_num)
{
    evt_pvt_t *pvt = event->pvt;
    int i;
    int ev_num_i = (ev_num >> 5), ev_num_m = 1 << (ev_num & 31);
    uint_32 new_combos, this_combos, prev_half_combos;
    int cork = 0;

    /* -------------------------------------------------------------------- */
    /*  If this event isn't part of a combo, or is already down, pass thru  */
    /* -------------------------------------------------------------------- */
    if (ev_num >= EVENT_COMBO0 ||
        ((~pvt->in_combo[ev_num_i] | pvt->key_dn[ev_num_i]) & ev_num_m) != 0)
    {
        event_queue(event, DOWN, ev_num);
        return 0;
    }

    /* -------------------------------------------------------------------- */
    /*  Mark this key as "pressed".                                         */
    /* -------------------------------------------------------------------- */
    pvt->key_dn[ev_num_i] |= ev_num_m;

    /* -------------------------------------------------------------------- */
    /*  Now find its combo mask.                                            */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < pvt->combo_event_cnt; i++)
        if (pvt->combo_event[i][0] == ev_num)
            break;

    assert(i != pvt->combo_event_cnt);

    this_combos = pvt->combo_event[i][1];

    /* -------------------------------------------------------------------- */
    /*  New "full" combos will have a bit already set in "half_combo" that  */
    /*  is also set in this event's combo list, and isn't already in the    */
    /*  full-combos list.                                                   */
    /* -------------------------------------------------------------------- */
    new_combos = this_combos & pvt->half_combos & ~pvt->full_combos;

    prev_half_combos  = pvt->half_combos;
    pvt->full_combos |= new_combos;
    pvt->half_combos |= this_combos;

    /* -------------------------------------------------------------------- */
    /*  If we made any new "half" combos, cork further event processing     */
    /*  for "coalesce_time".                                                */
    /* -------------------------------------------------------------------- */
    if (prev_half_combos != pvt->half_combos)
    {
        event->soon = get_time() + event->coalesce_time;
        cork = 1;
    }

    /* -------------------------------------------------------------------- */
    /*  If this didn't make any new combos, go ahead and send a DOWN.       */
    /* -------------------------------------------------------------------- */
    if (!new_combos)
    {
        event_queue(event, DOWN, ev_num);
        return cork;
    }

    /* -------------------------------------------------------------------- */
    /*  If we made any new "full" combos, go send the appropriate up/dn     */
    /* -------------------------------------------------------------------- */
    while (new_combos)
    {
        uint_32 combo = get_trailing_1(new_combos);
        new_combos &= new_combos - 1;

        /* ---------------------------------------------------------------- */
        /*  Send a key-up for the first guy in the combo.                   */
        /* ---------------------------------------------------------------- */
        for (i = 0; i < 2; i++)
            if (pvt->combo_pairs[combo][i] != ev_num)
                event_queue(event, UP, pvt->combo_pairs[combo][i]);

        /* ---------------------------------------------------------------- */
        /*  Send a key-down for the newly created combo.                    */
        /* ---------------------------------------------------------------- */
        event_queue(event, DOWN, EVENT_COMBO0 + combo);
    }

    return cork;
}

LOCAL int event_chk_combo_release(event_t *event, uint_32 ev_num)
{
    evt_pvt_t *pvt = event->pvt;
    int i;
    int ev_num_i = (ev_num >> 5), ev_num_m = 1 << (ev_num & 31);
    uint_32 dying_combos, this_combos;
    int cork = 0;

    /* -------------------------------------------------------------------- */
    /*  If this event isn't part of a combo, or is already up, pass thru    */
    /* -------------------------------------------------------------------- */
    if (ev_num >= EVENT_COMBO0 ||
        ((~pvt->in_combo[ev_num_i] | ~pvt->key_dn[ev_num_i]) & ev_num_m) != 0)
    {
        event_queue(event, UP, ev_num);
        return 0;
    }

    /* -------------------------------------------------------------------- */
    /*  Mark this key as "unpressed".                                       */
    /* -------------------------------------------------------------------- */
    pvt->key_dn[ev_num_i] &= ~ev_num_m;

    /* -------------------------------------------------------------------- */
    /*  Now find its combo mask.                                            */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < pvt->combo_event_cnt; i++)
        if (pvt->combo_event[i][0] == ev_num)
            break;

    assert(i != pvt->combo_event_cnt);

    this_combos = pvt->combo_event[i][1];

    /* -------------------------------------------------------------------- */
    /*  Look for full combos that now got broken by this event, and half    */
    /*  combos that are no longer even half-combos.                         */
    /* -------------------------------------------------------------------- */
    dying_combos = this_combos & pvt->full_combos;

    pvt->full_combos &= ~dying_combos;
    pvt->half_combos &= ~(this_combos & ~dying_combos);

    /* -------------------------------------------------------------------- */
    /*  If we have any dying combos, cork further event processing for      */
    /*  "coalesce_time".                                                    */
    /* -------------------------------------------------------------------- */
    if (dying_combos)
    {
        event->soon = get_time() + event->coalesce_time;
        cork = 1;
    }

    /* -------------------------------------------------------------------- */
    /*  If this didn't break any combos, go ahead and send an UP.           */
    /* -------------------------------------------------------------------- */
    if (!dying_combos)
    {
        event_queue(event, UP, ev_num);
        return cork;
    }

    /* -------------------------------------------------------------------- */
    /*  If we broke any new "full" combos, go send the appropriate up/dn    */
    /* -------------------------------------------------------------------- */
    while (dying_combos)
    {
        uint_32 combo = get_trailing_1(dying_combos);
        dying_combos &= dying_combos - 1;

        /* ---------------------------------------------------------------- */
        /*  Send a key-up for the dying combo.                              */
        /* ---------------------------------------------------------------- */
        event_queue(event, UP, EVENT_COMBO0 + combo);

        /* ---------------------------------------------------------------- */
        /*  Send a key-down for the remaining guy in the combo, if he's     */
        /*  not active in any other combos.                                 */
        /* ---------------------------------------------------------------- */
        for (i = 0; i < 2; i++)
            if (pvt->combo_pairs[combo][i] != ev_num)
            {
                int j;
                for (j = 0; j < pvt->combo_event_cnt; j++)
                    if (pvt->combo_event[j][0] == pvt->combo_pairs[combo][i])
                    {
                        if (!(pvt->full_combos & pvt->combo_event[j][1]))
                            event_queue(event, DOWN, 
                                                pvt->combo_pairs[combo][i]);
                        break;
                    }
            }
    }

    return cork;
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
/*                 Copyright (c) 1998-2011, Joseph Zbiciak                  */
/* ======================================================================== */
