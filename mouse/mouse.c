/*
 * ============================================================================
 *  Title:    Mouse Support via SDL
 *  Author:   J. Zbiciak
 * ============================================================================
 *  
 *  jzIntv doesn't really support mice at this time.  This module is a 
 *  simple module to convert xrel/yrel events into direction events, and
 *  capture mouse button events similarly to joystick buttons.
 *  
 *  It should be considered highly experimental.
 *  
 * ============================================================================
 */

#include "config.h"
#include "sdl.h"
#include "periph/periph.h"
#include "pads/pads.h"
#include "event/event.h"
#include "event/event_tbl.h"
#include "mouse/mouse.h"


static int    last_x = -1, last_y = -1;    /* yes, ugly & evil static vars */
static int    last_dir = -1;
static double to_release = 0;

/* ======================================================================== */
/*  MOUSE_DECODE_MOTION                                                     */
/* ======================================================================== */
LOCAL void mouse_decode_motion
(
    SDL_Event *ev, 
    int       *ev_updn, uint_32 *ev_num,
    int       *ex_updn, uint_32 *ex_num
)
{
    int x, y, dx, dy, mag, dir;

    x = ev->motion.x;
    y = ev->motion.y;

    if (last_x == -1)
    {
        last_x = x;
        last_y = y;

        return;
    }

    dx  = last_x - x;
    dy  = last_y - y;
    mag = dx * dx + dy * dy;

    if (mag < 16)   /* about 4 pixels in any direction before a dir event */
        return;

    /* For now just decode 4 directions.  Later, decode 16 like joy does. */
    dir = abs(dx) > abs(dy) ? ( dx > 0 ? EVENT_MOUSE_W : EVENT_MOUSE_E )
        :                     ( dy > 0 ? EVENT_MOUSE_N : EVENT_MOUSE_S );

    if (dir != last_dir)
    {
        if (last_dir != -1)
        {
            *ev_updn = UP;   *ev_num = last_dir; 
            *ex_updn = DOWN; *ex_num = dir;
        } else
        {
            *ev_updn = DOWN; *ev_num = dir;
        }
    }

    last_x   = x;
    last_y   = y;
    last_dir = dir;

    /* Release mouse after 0.1 seconds, unless a new event comes in */
    to_release = get_time() + 0.1;  

    return;
}

/* ======================================================================== */
/*  MOUSE_DECODE_BUTTON                                                     */
/* ======================================================================== */
LOCAL void mouse_decode_button(SDL_Event *ev, uint_32 *ev_num)
{
    if (ev->button.button > 31)
    {
        *ev_num = EVENT_IGNORE;
        return;
    }

    *ev_num = EVENT_MOUSE_BTN_00 + ev->button.button;
    return;
}


/* ======================================================================== */
/*  MOUSE_DECODE_EVENT -- Pull apart an SDL_EVENT and turn it into our      */
/*                        internal event numbers.                           */
/* ======================================================================== */
void mouse_decode_event(SDL_Event *ev, int *ev_updn, uint_32 *ev_num,
                                       int *ex_updn, uint_32 *ex_num)
{
    *ev_num = EVENT_IGNORE;
    *ex_num = EVENT_IGNORE;
    
    switch (ev->type)
    {
        case SDL_MOUSEMOTION: 
        { 
            mouse_decode_motion(ev, ev_updn, ev_num, ex_updn, ex_num); 
            break; 
        }

        case SDL_MOUSEBUTTONDOWN:
        {
            *ev_updn = DOWN;
            mouse_decode_button(ev, ev_num);
            break;
        }

        case SDL_MOUSEBUTTONUP:
        {
            *ev_updn = UP;
            mouse_decode_button(ev, ev_num);
            break;
        }

        default: *ev_num = EVENT_IGNORE; break;
    }
}

/* ======================================================================== */
/*  MOUSE_PUMP          -- Decide whether to send a mouse up event          */
/* ======================================================================== */
void mouse_pump(int *ev_updn, uint_32 *ev_num)
{
    if (last_dir != -1 && get_time() > to_release)
    {
        *ev_num  = last_dir;
        *ev_updn = UP;
        last_dir = -1;
    } else
    {
        *ev_num  = EVENT_IGNORE;
    }
}
