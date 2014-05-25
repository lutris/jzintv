/*
 * ============================================================================
 *  Title:    Joystick Support via SDL
 *  Author:   J. Zbiciak
 * ============================================================================
 *  This module implements jzIntv's joystick support.  Specifically, it:
 *  
 *   -- Enumerates available joysticks
 *   -- Discovers capabilities of available joysticks
 *   -- Binds joystick inputs to internal event tags
 *   -- Allows configuring joystick->event mapping
 *   -- Decodes analog inputs into Inty's 16-direction input
 *  
 * ============================================================================
 */

#include "config.h"
#include "sdl.h"
#include "periph/periph.h"
#include "pads/pads.h"
#include "event/event.h"
#include "event/event_tbl.h"
#include "joy/joy.h"
#include "cp1600/cp1600.h"
#include "cp1600/emu_link.h"


joy_t *joy     = NULL;
int    joy_cnt = 0;

#define DO_AC       1
#define AC_INIT_X   2
#define AC_INIT_Y   4
#define AC_INIT     (AC_INIT_X + AC_INIT_Y)

#define DIR_MAG     (32768)
#ifdef linux
#define PUSH_THRESH (128*DIR_MAG / 6)
#define RELS_THRESH (128*DIR_MAG /10)
#define AUTOCENTER  (0)
#else
#define PUSH_THRESH (128*DIR_MAG / 4)
#define RELS_THRESH (128*DIR_MAG / 6)
#define AUTOCENTER  (DO_AC + AC_INIT)
#endif

/* convert percentage to threshold and back */
#define P2T(x)  ((int)(((x)*128.*DIR_MAG + 50)/ 100.))
#define T2P(x)  ((int)(((100. * (x)) + 64.*DIR_MAG)/ (128.*DIR_MAG)))

/* convert percentage to direction magnitude and back */
#define P2M(x)  ((int)(((x)*DIR_MAG + 50.)/ 100.))
#define M2P(x)  ((int)(((100. * (x)) + 0.5*DIR_MAG)/ ((double)DIR_MAG)))

/*
SDL_NumJoysticks        Count available joysticks.
SDL_JoystickName        Get joystick name.
SDL_JoystickOpen        Opens a joystick for use.
SDL_JoystickOpened      Determine if a joystick has been opened
SDL_JoystickIndex       Get the index of an SDL_Joystick.
SDL_JoystickNumAxes     Get the number of joystick axes
SDL_JoystickNumBalls    Get the number of joystick trackballs
SDL_JoystickNumHats     Get the number of joystick hats
SDL_JoystickNumButtons  Get the number of joysitck buttons
SDL_JoystickUpdate      Updates the state of all joysticks
SDL_JoystickGetAxis     Get the current state of an axis
SDL_JoystickGetHat      Get the current state of a joystick hat
SDL_JoystickGetButton   Get the current state of a given button on a given joystick
SDL_JoystickGetBall     Get relative trackball motion
SDL_JoystickClose       Closes a previously opened joystick
*/

static int dir_vect[16][2]; 
static const int joy_dir_map[4] = 
{ 
    EVENT_JS0_E, EVENT_JS1_E, EVENT_JS2_E, EVENT_JS3_E 
};
static const int joy_btn_map[4] = 
{ 
    EVENT_JS0_BTN_00, EVENT_JS1_BTN_00, EVENT_JS2_BTN_00, EVENT_JS3_BTN_00
};
static const int joy_hat_map[4] = 
{ 
    EVENT_JS0_HAT0_E, EVENT_JS1_HAT0_E, EVENT_JS2_HAT0_E, EVENT_JS3_HAT0_E
};

extern int joy_emu_link(cp1600_t *, int *);

LOCAL void joy_config(int i, char *cfg);

/* ======================================================================== */
/*  JOY_DTOR                                                                */
/* ======================================================================== */
void joy_dtor(void)
{
    int i;

    for (i = 0; i < joy_cnt; i++)
        CONDFREE(joy[i].name);

    CONDFREE(joy);
    joy_cnt = 0;
    memset(dir_vect, 0, sizeof(dir_vect));
}

/* ======================================================================== */
/*  JOY_INIT -- Enumerate available joysticks and features                  */
/* ======================================================================== */
int joy_init(int verbose, char *cfg[])
{
    double now = get_time();
    int i, j;

    /* -------------------------------------------------------------------- */
    /*  Initialize the direction vector table.                              */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 16; i++)
    {
        double ang = M_PI * (i / 8.);

        dir_vect[i][0] = DIR_MAG * cos(ang);
        dir_vect[i][1] = DIR_MAG *-sin(ang);
    }

    /* -------------------------------------------------------------------- */
    /*  How many do we have?                                                */
    /* -------------------------------------------------------------------- */
    joy_cnt = SDL_NumJoysticks();
    if (joy_cnt > MAX_JOY)
        joy_cnt = MAX_JOY;

    if (!joy_cnt)
        return 0;

    if (verbose)
        jzp_printf("joy:  Found %d joystick(s)\n", joy_cnt);

    if (!(joy = CALLOC(joy_t, joy_cnt)))
    {
        fprintf(stderr, "joy: out of memory\n");
        exit(1);
    }

    /* -------------------------------------------------------------------- */
    /*  Initialize default behavior.                                        */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < joy_cnt; i++)
    {
        joy[i].push_thresh = PUSH_THRESH;
        joy[i].rels_thresh = RELS_THRESH;
        joy[i].autocenter  = AUTOCENTER;
        joy[i].dir_type    = 1;
    }
            
    /* -------------------------------------------------------------------- */
    /*  Ok, what do they look like?                                         */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < joy_cnt; i++)
    {
        SDL_Joystick *jy;
        joy[i].name        = strdup(SDL_JoystickName(i));
        jy = SDL_JoystickOpen(i);
        if (SDL_JoystickOpened(i) && jy)
        {
            /* ------------------------------------------------------------ */
            /*  Get the info available from SDL.                            */
            /* ------------------------------------------------------------ */
            joy[i].ptr         = (void*)jy;
            joy[i].num_axes    = SDL_JoystickNumAxes   (jy);
            joy[i].num_balls   = SDL_JoystickNumBalls  (jy);
            joy[i].num_hats    = SDL_JoystickNumHats   (jy);
            joy[i].num_buttons = SDL_JoystickNumButtons(jy);

            for (j = 0; j < MAX_AXES; j++)
            {
                joy[i].axis[j].pos = SDL_JoystickGetAxis(jy, j);
                joy[i].axis[j].ctr = joy[i].axis[j].pos;

                if (abs(joy[i].axis[j].pos) > 1000)
                    joy[i].axis[j].ctr = 0; /* assume accidentally pressed */

                joy[i].axis[j].prv = joy[i].axis[j].ctr;
                joy[i].axis[j].min = joy[i].axis[j].ctr + P2M(-50.0);
                joy[i].axis[j].max = joy[i].axis[j].ctr + P2M(+50.0);
                joy[i].axis[j].last = now;
            }

            joy[i].x_axis = 0;
            joy[i].y_axis = 1;

            if (verbose)
            {
                jzp_printf("joy:  Joystick JS%d \"%s\"\n", i, joy[i].name);
                jzp_printf("joy:     %d axes, %d trackball(s), "
                                "%d hat(s), %d button(s)\n",
                        joy[i].num_axes, joy[i].num_balls,
                        joy[i].num_hats, joy[i].num_buttons);
            }

            /* ------------------------------------------------------------ */
            /*  Parse configuration strings.  The following four aspects    */
            /*  can be configured in this way:                              */
            /*                                                              */
            /*   -- X/Y axis bindings                                       */
            /*   -- Autocentering on/off                                    */
            /*   -- Push/Release thresholds                                 */
            /*   -- X/Y axis range                                          */
            /* ------------------------------------------------------------ */
            if (cfg[i])
            {
                joy_config(i, cfg[i]);
            }
        } else
        {
            if (verbose)
            {
                jzp_printf("joy:  Joystick JS%d \"%s\"\n", i, joy[i].name);
                jzp_printf("joy:     Unavailable:  Could not open.\n");
            }
        }
    }

    /* -------------------------------------------------------------------- */
    /*  Register our EMU_LINK API.  I'll put this on API #8.                */
    /* -------------------------------------------------------------------- */
    emu_link_register(joy_emu_link, 8);

    return 0;
}

/* ======================================================================== */
/*  JOY_CONFIG -- Parse configuration string for a joystick.                */
/* ======================================================================== */
LOCAL void joy_config(int i, char *cfg)
{
    char *s1, *s2;
    int v, j;
    int lo, hi;
    char *tmp = strdup(cfg);
    char *mem = tmp;

    /* -------------------------------------------------------------------- */
    /*  Strip off any quotes that get to us.                                */
    /* -------------------------------------------------------------------- */
    if (tmp[0] == '"' || tmp[0] == '\'')
        tmp++;

    if ((s1 = strrchr(tmp, '"' )) != NULL ||
        (s1 = strrchr(tmp, '\'')) != NULL)
        *s1 = 0;

    /* -------------------------------------------------------------------- */
    /*  Pull off comma-separated sections and parse them.                   */
    /* -------------------------------------------------------------------- */
    s1 = strtok(tmp, ",");

    while (s1)
    {
        s2 = s1;
        while (*s2 && *s2 != '=')
            s2++;
        if (*s2)
            *s2++ = '\0';
        v = atoi(s2);

        if      (!strcmp(s1, "xaxis") && *s2) joy[i].x_axis = v;
        else if (!strcmp(s1, "yaxis") && *s2) joy[i].y_axis = v;
        else if (!strcmp(s1, "ac")          ) joy[i].autocenter = AC_INIT;
        else if (!strcmp(s1, "noac")        ) joy[i].autocenter = 0;
        else if (!strcmp(s1, "push")  && *s2) joy[i].push_thresh=P2T(atoi(s2));
        else if (!strcmp(s1, "rels")  && *s2) joy[i].rels_thresh=P2T(atoi(s2));
        else if (!strcmp(s1, "4diag")       ) joy[i].dir_type = -4;
        else if (!strcmp(s1, "4dir")        ) joy[i].dir_type = 4;
        else if (!strcmp(s1, "8dir")        ) joy[i].dir_type = 2;
        else if (!strcmp(s1, "16dir")       ) joy[i].dir_type = 1;
        else if ((!strcmp(s1, "xrng") || !strcmp(s1, "yrng")) && 
                 *s2 && 2 == sscanf(s2, "%d:%d", &lo, &hi))
        {
            int ax = s1[0] == 'x' ? joy[i].x_axis : joy[i].y_axis;

            joy[i].axis[ax].min = P2M(lo);
            joy[i].axis[ax].max = P2M(hi);
        } else
        {
            fprintf(stderr, "joy:  Unknown joystick config key '%s'!\n", s1);
        }
        s1 = strtok(NULL, ",");
    }

    /* -------------------------------------------------------------------- */
    /*  Sanity check push/release                                           */
    /* -------------------------------------------------------------------- */
    if (joy[i].push_thresh < joy[i].rels_thresh)
    {
        joy[i].push_thresh = joy[i].rels_thresh;
        jzp_printf("joy:    Warning: Push threshold below release.  "
               "Setting push = release.\n");
    }

    /* -------------------------------------------------------------------- */
    /*  Summarize resulting configuration.                                  */
    /* -------------------------------------------------------------------- */
    jzp_printf(
"joy:     Cfg:  X-Axis = axis %d  Y-Axis = axis %d  Autocenter = %-3s\n"
"joy:           Push threshold = %d%%  Release threshold = %d%%\n"
"joy:           X range = [%d%%,%d%%]  Y range = [%d%%,%d%%]\n"
"joy:           Directions = %d%s\n"
"--js%d=\"xaxis=%d,yaxis=%d,%sac,push=%d,rels=%d,xrng=%d:%d,yrng=%d:%d,%s\"\n",
            joy[i].x_axis, joy[i].y_axis,
            joy[i].autocenter ? "ON" : "off",
            T2P(joy[i].push_thresh), T2P(joy[i].rels_thresh),
            M2P(joy[i].axis[joy[i].x_axis].min), 
            M2P(joy[i].axis[joy[i].x_axis].max),  
            M2P(joy[i].axis[joy[i].y_axis].min), 
            M2P(joy[i].axis[joy[i].y_axis].max),
            16 / abs(joy[i].dir_type), joy[i].dir_type ? " diagonal bias" :"",
            i, joy[i].x_axis, joy[i].y_axis,
            joy[i].autocenter ? "" : "no",
            T2P(joy[i].push_thresh), T2P(joy[i].rels_thresh),
            M2P(joy[i].axis[joy[i].x_axis].min), 
            M2P(joy[i].axis[joy[i].x_axis].max),  
            M2P(joy[i].axis[joy[i].y_axis].min), 
            M2P(joy[i].axis[joy[i].y_axis].max),
            joy[i].dir_type ==-4 ? "4diag":
            joy[i].dir_type == 4 ? "4dir" :
            joy[i].dir_type == 2 ? "8dir" : "16dir");


    /* -------------------------------------------------------------------- */
    /*  Invert any axes for which inversion was requested.                  */
    /* -------------------------------------------------------------------- */
    for (j = 0; j < MAX_AXES; j++)
    {
        if (joy[i].axis[j].min > joy[i].axis[j].max)
        {
            int t;
            jzp_printf("joy:    Inverting axis %d because max < min\n",
                    j);

            t = joy[i].axis[j].min;
            joy[i].axis[j].min = joy[i].axis[j].max;
            joy[i].axis[j].max = t;
            joy[i].axis[j].inv = 1;
        }
    }

    free(mem);
}

/* ======================================================================== */
/*  JOY_NORMALIZE_AXIS                                                      */
/* ======================================================================== */
LOCAL int joy_normalize_axis(int idx, int ax)
{
    int pos, ctr, min, max;

    /* -------------------------------------------------------------------- */
    /*  Linearly interpolate the swing from ctr to edge to the range 0-128  */
    /* -------------------------------------------------------------------- */
    pos = joy[idx].axis[ax].pos;
    ctr = joy[idx].axis[ax].ctr;
    min = joy[idx].axis[ax].min;
    max = joy[idx].axis[ax].max;
    if (min == ctr) min--;
    if (max == ctr) max++;

    if (pos > ctr)  return   ((pos - ctr) * 128) / (max - ctr);
    else            return - ((ctr - pos) * 128) / (ctr - min);
}


/* ======================================================================== */
/*  JOY_DECODE_AXIS                                                         */
/* ======================================================================== */
LOCAL void joy_decode_axis(SDL_Event *ev, int *ev_updn, uint_32 *ev_num,
                                          int *ex_updn, uint_32 *ex_num)
{
    int a = ev->jaxis.axis;
    int i = ev->jaxis.which;
    int v = ev->jaxis.value;
    int u, f;
    int norm_x, norm_y, xa = joy[i].x_axis, ya = joy[i].y_axis;
    int j, dotp, best = joy[i].disc_dir, best_dotp = 0;
    int ox, oy;

    UNUSED(ex_updn);
    UNUSED(ex_num);

    /* -------------------------------------------------------------------- */
    /*  Ignore axes and joysticks we can't track.                           */
    /* -------------------------------------------------------------------- */
    if (ev->jaxis.which >= joy_cnt || ev->jaxis.axis >= MAX_AXES)
    {
        *ev_num = EVENT_IGNORE;
        return;
    }

    /* -------------------------------------------------------------------- */
    /*  Update autoranging.                                                 */
    /* -------------------------------------------------------------------- */
    if (joy[i].axis[a].inv)
        v = -v;

    if (joy[i].axis[a].min > v)
        joy[i].axis[a].min = v;

    if (joy[i].axis[a].max < v)
        joy[i].axis[a].max = v;

    joy[i].axis[a].pos = v;

    /* -------------------------------------------------------------------- */
    /*  The remaining code is specific to X/Y axis updates.                 */
    /* -------------------------------------------------------------------- */
    if (joy[i].x_axis != a && joy[i].y_axis != a)
    {
        *ev_num = EVENT_IGNORE;
        return;
    }

    /* -------------------------------------------------------------------- */
    /*  Update autocentering.  Do a decaying average of all samples that    */
    /*  are less than 1/8th the current release-threshold away from what    */
    /*  we consider as the current center.  An enterprising gamer might     */
    /*  fool this algorithm by rocking the joystick just off center. BFD.   */
    /*                                                                      */
    /*  The autoweighting function actually looks at the previous sample    */
    /*  and how long it was held for.  Samples that were held longer are    */
    /*  more likely to be "center."  The adaptation is simple:  Assume      */
    /*  a desired sample rate and corresponding exponent.  For samples      */
    /*  held longer, convert the held sample into multiple equal-valued     */
    /*  samples.                                                            */
    /*                                                                      */
    /*  In this case, the exponent is 1/64, and the assumed sample rate     */
    /*  is 32Hz.  (Powers of two, ya know.)  If more than 1 sec elapsed     */
    /*  we clamp at 1 sec.                                                  */
    /* -------------------------------------------------------------------- */
    f = a == joy[i].x_axis ? AC_INIT_X : AC_INIT_Y;

    /* ugh, gotta get the float out of this someday. */
    ox = joy[i].axis[xa].prv - joy[i].axis[xa].ctr;
    oy = joy[i].axis[ya].prv - joy[i].axis[ya].ctr;
    dotp = ((double)ox*128./SHRT_MAX) * ((double)ox*DIR_MAG/SHRT_MAX) + 
           ((double)oy*128./SHRT_MAX) * ((double)oy*DIR_MAG/SHRT_MAX);

/*  jzp_printf("dotp=%d rt=%d\n",dotp*8, joy[i].rels_thresh); */

    if ((joy[i].autocenter & f) == f && v != 0)
    {
        /* ---------------------------------------------------------------- */
        /*  If it's a digital joystick, it'll snap to a dir.  Otherwise,    */
        /*  if it's a slow motion, use it as initial centering estimate.    */
        /* ---------------------------------------------------------------- */
        if (abs(v) * 8 < SHRT_MAX) joy[i].axis[a].ctr = v;
        else                       joy[i].axis[a].ctr = 0;

        joy[i].autocenter &= ~f;
    } else 
    if ((joy[i].autocenter & DO_AC) && dotp*8 < joy[i].rels_thresh &&
        abs(v) * 8 < SHRT_MAX)
    {
        double now  = get_time();
        double then = joy[i].axis[a].last;
        int iters;
        
        u = joy[i].axis[a].prv;

        iters = 32 * (now - then) + 0.5;
        if (iters > 32) iters = 32;
        if (iters <= 0) iters = 1;

/*jzp_printf("iters=%d axis=%d then=%f now=%f\n", iters, a, then*16, now*16);*/
        while (iters-- > 0)
            joy[i].axis[a].ctr = (joy[i].axis[a].ctr * 63 + 31 + u) >> 6;

        joy[i].axis[a].last = now;
    }

    joy[i].axis[a].prv = v;


    /* -------------------------------------------------------------------- */
    /*  Ok, if this was either the X or Y axis, determine if we generate a  */
    /*  DISC event.                                                         */
    /*                                                                      */
    /*  Decoding strategy:                                                  */
    /*                                                                      */
    /*   -- Normalize the input to a +/- 128 range based on our autocenter  */
    /*      and autoranging.                                                */
    /*                                                                      */
    /*   -- Decide whether disc is up or down based on hysteresis.          */
    /*                                                                      */
    /*       -- The disc gets pressed when the joystick is more than        */
    /*          1/4 of the way to the edge from the center along its        */
    /*          closest direction line.                                     */
    /*                                                                      */
    /*       -- The disc gets released when the joystick is less than       */
    /*          1/6 along its closest direction line.                       */
    /*                                                                      */
    /*   -- If the disc is pressed, decode the direction into one of 16.    */
    /*      Rather than do trig with arctan and all that jazz, I instead    */
    /*      take the dot product of the joystick position with 16           */
    /*      different normalized direction vectors, and return the largest  */
    /*      as the best match.  Yay vector algebra.                         */
    /*                                                                      */
    /* -------------------------------------------------------------------- */

    /* -------------------------------------------------------------------- */
    /*  Normalize the X/Y.  This returns stuff in a +/- 128 range.          */
    /* -------------------------------------------------------------------- */
    norm_x = joy_normalize_axis(i,xa);                                    
    norm_y = joy_normalize_axis(i,ya);                                    
                                                                          
    /* -------------------------------------------------------------------- */
    /*  Figure out which of the 16 directions is closest to the dir we're   */
    /*  pointing.  We apply the "press" and "release" thresholds to the     */
    /*  dot product we calculate below.                                     */
    /* -------------------------------------------------------------------- */
    for (j = joy[i].dir_type < 0 ? 2 : 0; j < 16; j += abs(joy[i].dir_type))
    {
        dotp = dir_vect[j][0] * norm_x + dir_vect[j][1] * norm_y;
        if (best_dotp < dotp)
        {
            best_dotp = dotp;
            best = j;
        }
    }

    *ev_updn = joy[i].disc_dir == -1 ? UP : DOWN;

    if (best_dotp < joy[i].rels_thresh && joy[i].disc_dir != -1)
    {
        *ev_updn = UP;
        *ev_num  = joy_dir_map[i] + joy[i].disc_dir;
        joy[i].disc_dir = -1;
        return;
    }

    if (best_dotp <= joy[i].push_thresh && joy[i].disc_dir == -1)
    {
        *ev_updn = UP;
        *ev_num  = EVENT_IGNORE;
        return;
    }

    if (((best_dotp > joy[i].push_thresh && joy[i].disc_dir == -1) ||
         (best_dotp > joy[i].rels_thresh && joy[i].disc_dir != best)) && 
         best != -1)
    {
        *ev_updn = DOWN;
        *ev_num  = joy_dir_map[i] + best;
        joy[i].disc_dir = best;
        return;
    }

    *ev_num = EVENT_IGNORE;
    return;
}

/* ======================================================================== */
/*  JOY_DECODE_HAT                                                          */
/* ======================================================================== */
LOCAL void joy_decode_hat(SDL_Event *ev, int *ev_updn, uint_32 *ev_num,
                                         int *ex_updn, uint_32 *ex_num)
{
    int base;
    if (ev->jhat.which >= joy_cnt || ev->jhat.hat >= MAX_HATS)
    {
        *ev_num = EVENT_IGNORE;
        return;
    }

    base = joy_hat_map[ev->jhat.which] + 8*ev->jhat.hat;
    *ev_updn = DOWN;

    if (ev->jhat.value != joy[ev->jhat.which].hat_dir[ev->jhat.hat])
    {
        *ex_updn = UP;
        switch (joy[ev->jhat.which].hat_dir[ev->jhat.hat])
        {
            case SDL_HAT_RIGHT:    *ex_num = base + 0;   break; /* E    */
            case SDL_HAT_RIGHTUP:  *ex_num = base + 1;   break; /* NE   */
            case SDL_HAT_UP:       *ex_num = base + 2;   break; /* N    */
            case SDL_HAT_LEFTUP:   *ex_num = base + 3;   break; /* NW   */
            case SDL_HAT_LEFT:     *ex_num = base + 4;   break; /* W    */
            case SDL_HAT_LEFTDOWN: *ex_num = base + 5;   break; /* SW   */
            case SDL_HAT_DOWN:     *ex_num = base + 6;   break; /* S    */
            case SDL_HAT_RIGHTDOWN:*ex_num = base + 7;   break; /* SE   */
            case SDL_HAT_CENTERED: *ex_num = EVENT_IGNORE; break;
            default: jzp_printf("Warning: Unknown hat input %d\n", ev->jhat.value);
        }
    }

    switch (ev->jhat.value)
    {
        case SDL_HAT_RIGHT:        *ev_num = base + 0;   break; /* E    */
        case SDL_HAT_RIGHTUP:      *ev_num = base + 1;   break; /* NE   */
        case SDL_HAT_UP:           *ev_num = base + 2;   break; /* N    */
        case SDL_HAT_LEFTUP:       *ev_num = base + 3;   break; /* NW   */
        case SDL_HAT_LEFT:         *ev_num = base + 4;   break; /* W    */
        case SDL_HAT_LEFTDOWN:     *ev_num = base + 5;   break; /* SW   */
        case SDL_HAT_DOWN:         *ev_num = base + 6;   break; /* S    */
        case SDL_HAT_RIGHTDOWN:    *ev_num = base + 7;   break; /* SE   */
        case SDL_HAT_CENTERED:     *ev_num = EVENT_IGNORE; break;
        default: jzp_printf("Warning: Unknown hat input %d\n", ev->jhat.value);
    }

    joy[ev->jhat.which].hat_dir[ev->jhat.hat] = ev->jhat.value;

    return;
}

/* ======================================================================== */
/*  JOY_DECODE_BUTTON                                                       */
/* ======================================================================== */
LOCAL void joy_decode_button(SDL_Event *ev, uint_32 *ev_num)
{
    if (ev->jbutton.which >= joy_cnt || ev->jbutton.button > 31)
    {
        *ev_num = EVENT_IGNORE;
        return;
    }

    *ev_num = joy_btn_map[ev->jbutton.which] + ev->jbutton.button;
    return;
}



/* ======================================================================== */
/*  JOY_DECODE_EVENT -- Pull apart an SDL_EVENT and turn it into our        */
/*                      internal event numbers.                             */
/* ======================================================================== */
void joy_decode_event(SDL_Event *ev, int *ev_updn, uint_32 *ev_num,
                                     int *ex_updn, uint_32 *ex_num)
{
    
    switch (ev->type)
    {
        case SDL_JOYAXISMOTION: 
        { 
            joy_decode_axis(ev, ev_updn, ev_num, ex_updn, ex_num); 
            break; 
        }
        case SDL_JOYHATMOTION:  
        { 
            joy_decode_hat (ev, ev_updn, ev_num, ex_updn, ex_num); 
            break; 
        }

        case SDL_JOYBUTTONDOWN:
        {
            *ev_updn = DOWN;
            joy_decode_button(ev, ev_num);
            break;
        }

        case SDL_JOYBUTTONUP:
        {
            *ev_updn = UP;
            joy_decode_button(ev, ev_num);
            break;
        }

        case SDL_JOYBALLMOTION:
        {
            /* ignored */
#ifdef DEBUG
            printf("\rball event: %d %d [%4d,%4d]     \n", 
                   ev->jball.which, ev->jball.ball, 
                   ev->jball.xrel, ev->jball.yrel);
            fflush(stdout);
#endif

            *ev_num = EVENT_IGNORE; 
            break;
        }

        default: *ev_num = EVENT_IGNORE; break;
    }
}

/* ======================================================================== */
/*  JOY_EMU_LINK -- Allow programs to get analog joystick info.             */
/* ======================================================================== */
int joy_emu_link(cp1600_t *cpu, int *fail)
{
    int js;

    /* -------------------------------------------------------------------- */
    /*  Sub-APIs we export:  (Specified in R2.  Joystick number in R3.)     */
    /*                                                                      */
    /*  00: Number of joysticks.  Result in R0.  Ignores R3.                */
    /*  01: Get geometry: Returns # of axes, balls, hats, buttons in R0..R3 */
    /*  02: Get X/Y raw pos:  Returns 16-bit X/Y pos in R1, R2.             */
    /*  03: Get X/Y raw min:  Returns 16-bit X/Y min in R1, R2.             */
    /*  04: Get X/Y raw max:  Returns 16-bit X/Y max in R1, R2.             */
    /*  05: Get X/Y raw ctr:  Returns 16-bit X/Y max in R1, R2.             */
    /*  06: Get X/Y cooked:   Norm'd 8-bit X/Y in R1, R2. Disc Dir in R0.   */
    /*  07: Get buttons.  Returns 32-bit bitmap in R1, R2.                  */
    /*  08: Get hats.  Returns hats 0..3 in 4 x 4-bit fields in R0.         */
    /* -------------------------------------------------------------------- */
    if (cpu->r[2] == 0x00)
    {
        *fail = 0;
        return joy_cnt;
    }

    if (cpu->r[2] > 0x08 || cpu->r[3] >= joy_cnt)
    {
        *fail = 1;
        return 0xFFFF;
    }
    
    js = cpu->r[3];

    switch (cpu->r[2])
    {
        case 0x01:
        {
            *fail = 0;
            cpu->r[1] = joy[js].num_balls;
            cpu->r[2] = joy[js].num_hats;
            cpu->r[3] = joy[js].num_buttons;
            return      joy[js].num_axes;
        }

        case 0x02:
        {
            *fail = 0;
            cpu->r[1] = joy[js].axis[joy[js].x_axis].pos;
            cpu->r[2] = joy[js].axis[joy[js].y_axis].pos;
            return 0;
        }

        case 0x03:
        {
            *fail = 0;
            cpu->r[1] = joy[js].axis[joy[js].x_axis].min;
            cpu->r[2] = joy[js].axis[joy[js].y_axis].min;
            return 0;
        }

        case 0x04:
        {
            *fail = 0;
            cpu->r[1] = joy[js].axis[joy[js].x_axis].max;
            cpu->r[2] = joy[js].axis[joy[js].y_axis].max;
            return 0;
        }

        case 0x05:
        {
            *fail = 0;
            cpu->r[1] = joy[js].axis[joy[js].x_axis].ctr;
            cpu->r[2] = joy[js].axis[joy[js].y_axis].ctr;
            return 0;
        }

        case 0x06:
        {
            *fail = 0;
            cpu->r[1] = joy_normalize_axis(js, joy[js].x_axis);
            cpu->r[2] = joy_normalize_axis(js, joy[js].y_axis);
            return joy[js].disc_dir;
        }

        case 0x07:
        {
            uint_32 buttons = 0;
            int i;

            *fail = 0;
            for (i = 0; i < joy[js].num_buttons; i++)
                buttons |= (SDL_JoystickGetButton(
                                (SDL_Joystick *)joy[js].ptr,i) != 0) << i;
            
            cpu->r[1] =  buttons        & 0xFFFF;
            cpu->r[2] = (buttons >> 16) & 0xFFFF;

            return 0;
        }

        case 0x08:
        {
            uint_32 hats = 0;
            int i;

            *fail = 0;
            for (i = 0; i < joy[js].num_hats; i++)
            {
                int p;

                switch (joy[js].hat_dir[i])
                {
                    case SDL_HAT_RIGHT:         p = 0;  break; /* E    */
                    case SDL_HAT_RIGHTUP:       p = 1;  break; /* NE   */
                    case SDL_HAT_UP:            p = 2;  break; /* N    */
                    case SDL_HAT_LEFTUP:        p = 3;  break; /* NW   */
                    case SDL_HAT_LEFT:          p = 4;  break; /* W    */
                    case SDL_HAT_LEFTDOWN:      p = 5;  break; /* SW   */
                    case SDL_HAT_DOWN:          p = 6;  break; /* S    */
                    case SDL_HAT_RIGHTDOWN:     p = 7;  break; /* SE   */
                    case SDL_HAT_CENTERED:      p = 15; break; /* center */
                    default:                    p = 15; break;
                }
                hats |= p << (4 * i);
            }

            return hats & 0xFFFF;
        }

        default:
        {
            *fail = 1;
            return 0xFFFF;
        }
    }

    *fail = 1;
    return 0xFFFF;
}

