/*
 * ============================================================================
 *  Title:    Joystick Support via SDL
 *  Author:   J. Zbiciak
 * ============================================================================
 */

#ifndef JOY_H_
#define JOY_H_

typedef struct joy_t joy_t;
extern joy_t *joy;
extern int    joy_cnt;

#define MAX_AXES (8)
#define MAX_HATS (4)
#define MAX_JOY  (4)

/* ======================================================================== */
/*  Joystick Information Structure.                                         */
/* ======================================================================== */
struct joy_t
{
    /* -------------------------------------------------------------------- */
    /*  Information from SDL's APIs directly.                               */
    /* -------------------------------------------------------------------- */
    char    *name;          /* Name of the joystick, if available.          */
    int     num_axes;       /* Number of analog axes.                       */
    int     num_balls;      /* Number of trackballs                         */
    int     num_hats;       /* Number of hats                               */
    int     num_buttons;    /* Number of buttons                            */

    void    *ptr;           /* Pointer to SDL joystick structure.           */

    /* -------------------------------------------------------------------- */
    /*  Joystick configuration, etc.                                        */
    /* -------------------------------------------------------------------- */
    int     push_thresh;    /* not-pushed to pushed threshold               */
    int     rels_thresh;    /* pushed to not-pushed threshold               */
    uint_8  autocenter;     /* Flag:  Autocentering enabled?                */
    sint_8  dir_type;       /* flag:  1=16-dir, 2=8-dir, 4=4-dir, -4=4-diag */

    /* -------------------------------------------------------------------- */
    /*  Current X/Y axis state, and ranges                                  */
    /* -------------------------------------------------------------------- */
    struct
    {
        int max, min, ctr, pos, inv, prv;
        double last;
    } axis[MAX_AXES];

    int disc_dir;
    int hat_dir[MAX_HATS];
    int x_axis, y_axis;
};


int  joy_init(int, char *cfg[]);
void joy_dtor(void);

#ifdef _SDL_events_h
/* ======================================================================== */
/*  JOY_DECODE_EVENT -- Pull apart an SDL_EVENT and turn it into our        */
/*                      internal event numbers.                             */
/* ======================================================================== */
void joy_decode_event(SDL_Event *ev, int *ev_updn, uint_32 *ev_num,
                                     int *ex_updn, uint_32 *ex_num);
#endif

#endif

