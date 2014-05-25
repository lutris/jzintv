/*
 * ============================================================================
 *  Title:    Platform Portability "Library"
 *  Author:   J. Zbiciak, T. Lindner
 *  $Id: plat_lib.c,v 1.2 2002/04/17 18:32:53 im14u2c Exp $
 * ============================================================================
 *  This module fills in missing features on various platforms.
 * ============================================================================
 *  GETTIMEOFDAY     -- Return current time in seconds/microseconds.
 *  STRDUP           -- Copy a string into freshly malloc'd storage.
 *  STRICMP          -- Case-insensitive string compare.
 *  SNPRINTF         -- Like sprintf(), only with bounds checking.
 *  PLAT_DELAY       -- Sleep w/ millisecond precision.
 * ============================================================================
 */

 
#include "config.h"
#include "sdl.h"

#ifdef USE_TERMIO
# include <termio.h>
#endif

#ifdef USE_SYS_IOCTL
# include <sys/ioctl.h>
#endif

/* ======================================================================== */
/*  GENERIC PORTABLE VERSIONS...                                            */
/* ======================================================================== */

/* ------------------------------------------------------------------------ */
/*  GET_TIME -- Return current time in seconds as a double.                 */
/* ------------------------------------------------------------------------ */
#ifndef NO_GETTIMEOFDAY
double get_time(void)
{
    struct timeval now;
    double seconds;

    gettimeofday(&now, NULL);

    seconds = (double)now.tv_sec + (double)now.tv_usec * 1e-6;

    return seconds;
}
#else

# if defined(WIN32) && !defined(NO_QUERY_PERF_COUNTER)

#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
#  include <math.h>


LOCAL double perf_rate = -1;

double get_time(void)
{
    LARGE_INTEGER li;

    if (perf_rate < 0)
    {
        if (QueryPerformanceFrequency(&li))
        {
            perf_rate = 1.0 / (double)li.QuadPart;
        } else
        {
            perf_rate = 0;
        }
    }

    if (perf_rate > 0)
    {
        double seconds;
        QueryPerformanceCounter(&li);
        seconds = (double)li.QuadPart * perf_rate;
        return seconds;
    }

    return SDL_GetTicks() / 1000.0;
}
# else

# if defined(WII)
double get_time(void)
{
    extern double wii_get_time();

    return wii_get_time();
}

#  else
double get_time(void)
{
    return SDL_GetTicks() / 1000.0;
}
#  endif
# endif
#endif


/* ------------------------------------------------------------------------ */
/*  STRDUP           -- Copy a string into freshly malloc'd storage.        */
/*                                                                          */
/*  Unfortunately, strdup() is not specified by ANSI.  *sigh*               */
/* ------------------------------------------------------------------------ */
#ifdef NO_STRDUP

char * strdup(const char *s)
{
    size_t len = strlen(s) + 1;
    char *new_str = malloc(len);

    if (new_str) strcpy(new_str, s);

    return new_str;
}

#endif /* NO_STRDUP */

/* ------------------------------------------------------------------------ */
/*  STRICMP          -- Case-insensitive strcmp.                            */
/* ------------------------------------------------------------------------ */
#ifdef NO_STRICMP
int stricmp(const char *s1, const char *s2)
{
    while (*s1 && *s2 && tolower(*s1) == tolower(*s2))
    {
        s1++;
        s2++;
    }
    return *s1 == *s2 ? 0 : (*s1 <  *s2 ? -1 : 1);
}
#endif

/* ------------------------------------------------------------------------ */
/*  SNPRINTF         -- Like sprintf(), only with bounds checking.          */
/* ------------------------------------------------------------------------ */
/*  WARNING:  THIS COULD CAUSE BUFFER OVERFLOW PROBLEMS AND IS MERELY       */
/*            A SHIM WHICH IS BEING USED TO GET jzIntv TO COMPILE.          */
/* ------------------------------------------------------------------------ */
#ifdef NO_SNPRINTF
# include <stdarg.h>

/* ------------------------------------------------------------------------ */
/*  WARNING:  THIS COULD CAUSE BUFFER OVERFLOW PROBLEMS AND IS MERELY       */
/*            A SHIM WHICH IS BEING USED TO GET jzIntv TO COMPILE.          */
/* ------------------------------------------------------------------------ */
void snprintf(char * buf, int len, const char * fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    UNUSED(len);
}
#endif /* NO_SNPRINTF */

/* ------------------------------------------------------------------------ */
/*  PLAT_DELAY       -- Sleep w/ millisecond precision.                     */
/* ------------------------------------------------------------------------ */
#if defined(NO_SDL_DELAY) && !defined(macintosh)
void plat_delay(unsigned delay)
{
    double now, soon = get_time() + ((double)delay / 1000.0);
    
    /* -------------------------------------------------------------------- */
    /*  BAD BAD BAD: Sit in a busy loop until time expires.                 */
    /* -------------------------------------------------------------------- */
    while (get_time() < soon)
        ;
    
    return;
}
#endif /* NO_SDL_DELAY */
#if !defined(NO_SDL_DELAY) && !defined(macintosh)
void plat_delay(unsigned delay)
{
    SDL_Delay(delay);
}
#endif


/* ======================================================================== */
/*  Portable random number generator, from Knuth vol 2.                     */
/*  J. Zbiciak, 1998                                                        */
/*                                                                          */
/*  This code is provided without any waranty of fitness for any            */
/*  purpose.  Caveat emptor.  Your mileage may vary.  Void where            */
/*  prohibited or taxed by law.  Not part of this nutritious breakfast.     */
/* ======================================================================== */

LOCAL unsigned __rand_buf[128], __rand_ptr = 0;

/* ======================================================================== */
/*  RAND_JZ      -- Return a random integer in the range  [0, 2^32)         */
/* ======================================================================== */
uint_32 rand_jz(void)
{
    uint_32 p, p1, p2;

    /* -------------------------------------------------------------------- */
    /*  Lagged Fibonacci Sequence Random Number Generator.                  */
    /*                                                                      */
    /*  This random number generator comes from Knuth vol 2., 3rd Ed,       */
    /*  p27-29.  The algorithm should produce a sequence whose most         */
    /*  significant bits have a period of 2^31 * (2^127 - 1) and whose      */
    /*  least significant bits have a period of 2^127 - 1.  Not bad.        */
    /*  The lags of 30 and 127 come from Table 1 on p29.                    */
    /*                                                                      */
    /*  The final XOR that this function performs is my own invention.      */
    /*  XOR'ing with a constant should not negatively impact the random     */
    /*  sequence, but it may slightly obscure a poor initialization         */
    /*  sequence.                                                           */
    /* -------------------------------------------------------------------- */

    p  = __rand_ptr++ & 127;
    p1 = (p -  30) & 127;
    p2 = (p - 127) & 127;

    return 0x5A4A3A2A ^ (__rand_buf[p] = __rand_buf[p1] + __rand_buf[p2]);
}

/* ======================================================================== */
/*  DRAND_JZ     -- Return a random double in the range [0.0, 1.0).         */
/* ======================================================================== */
double drand_jz(void)
{
    return rand_jz() / (((double)(~0U)) + 1.0);
}

/* ======================================================================== */
/*  SRAND_JZ     -- Seed the random number generator, setting it to a       */
/*                  known initial state.                                    */
/* ======================================================================== */
void srand_jz(uint_32 seed)
{
    uint_32 s = seed ^ 0x2A3A4A5A;
    int i, j;

    /* -------------------------------------------------------------------- */
    /*  This initializer uses the user-provided seed to drive a linear-     */
    /*  feedback-shift-register (LFSR) random number generator to produce   */
    /*  the initial random number buffer for the lagged-Fibonacci           */
    /*  generator that rand_jz() uses.  The LFSR uses the equation          */
    /*  x = x^-29 + x^-31, which gives a maximal LFSR sequence of 2^32 - 1  */
    /*  values.                                                             */
    /*                                                                      */
    /*  The user-provided seed is salted with my favorite magic constant,   */
    /*  0x2A3A4A5A, to provide the initial LSFR setting.  If that comes     */
    /*  up zero, then the salt is removed, giving an initial LSFR value of  */
    /*  0x2A3A4A5A.                                                         */
    /*                                                                      */
    /*  The LFSR is run for 43 iterations between buffer writes.  43 is     */
    /*  relatively prime to 2^32 - 1, and so all 2^32 - 1 unique seeds      */
    /*  will produce unique LFSR sequences to be written to the lagged-     */
    /*  Fibonacci generator buffer.                                         */
    /*                                                                      */
    /*  The output of the LFSR is XORed with the initial seed for a         */
    /*  touch of randomness, but I doubt that it significantly impacts      */
    /*  its randomness.  LFSR's are already pretty good random number       */
    /*  generators.  :-)                                                    */
    /* -------------------------------------------------------------------- */

    if (!s) 
        s = 0x2A3A4A5A;

    for (i = 0; i < 127; i++)
    {
        for (j = 0; j <= 42; j++)
            s = (((s ^ (s >> 2)) >> 29) & 1) | (s << 1);

        __rand_buf[i] = seed ^ s;
    }
}

/* ======================================================================== */
/*  Window size functions                                                   */
/*                                                                          */
/*  GET_DISP_WIDTH   -- Get the width of the text display.                  */
/*  SET_DISP_WIDTH   -- Try to set the width of the text display.           */
/*  INIT_DISP_WIDTH  -- Initialize the display width.                       */
/*                                                                          */
/*  If the display width is unknown, this should return '80', the default   */
/*  width jzIntv historically assumed.  Because 'set_disp_width' can fail,  */
/*  it returns the new width, which may be unchanged.                       */
/*                                                                          */
/*  On platforms which cannot detect display width, "INIT_DISP_WIDTH" can   */
/*  set the display width.  Passing in 0 here just tells it to detect or    */
/*  use the default width.                                                  */
/* ======================================================================== */


/* ------------------------------------------------------------------------ */
/*  Weakest implementation:  Just return 80 unless we're told to return     */
/*  something else.                                                         */
/* ------------------------------------------------------------------------ */
#if !defined(WIN32) && !defined(CAN_TIOCGWINSZ)
LOCAL int disp_width = -1;

int get_disp_width(void)
{
    return disp_width;
}

int set_disp_width(int new_width)
{
    disp_width = new_width > 0 ? new_width : disp_width;
    return disp_width;
}

void init_disp_width(int width)
{
    disp_width = width > 0 ? width : 80;
}
#endif

/* ------------------------------------------------------------------------ */
/*  WIN32 Implementation:  Eventually use "mode con:" to change size.       */
/* ------------------------------------------------------------------------ */
#if defined(WIN32)
LOCAL int disp_width = -1;

LOCAL void set_width_from_mode_con(void)
{
    FILE *f;
    f = popen("mode con:", "r");
    if (f)
    {
        char buf[80];
        int w;

        while (fgets(buf, sizeof(buf), f) != NULL)
        {
            if (sscanf(buf, " Columns: %d", &w) > 0)
                disp_width = w;
        }

        pclose(f);
    }
}

int get_disp_width(void)
{
    return disp_width;
}

int set_disp_width(int new_width)
{
    char buf[80];

    if (new_width == disp_width || new_width < 1)
        return disp_width;

    sprintf(buf, "mode con: cols=%d", new_width);

    if (system(buf) == 0)
        set_width_from_mode_con();

    return disp_width;
}

void init_disp_width(int width)
{

    disp_width = width > 0 ? width : 80;

}

#endif

/* ------------------------------------------------------------------------ */
/*  TIOCGWINSZ w/out SIGWINCH:  Always call the ioctl when asked the size.  */
/* ------------------------------------------------------------------------ */
#if !defined(WIN32) && defined(CAN_TIOCGWINSZ) && !defined(CAN_SIGWINCH)
LOCAL int disp_width = 80;

int get_disp_width(void)
{
    Dims my_win;

    if (ioctl(0, TIOCGWINSZ, &my_win) != 0)
        return disp_width;

    return my_win.ws_col;
}

int set_disp_width(int new_width)
{
    disp_width = new_width > 0 ? new_width : disp_width;
    return get_disp_width();
}

void init_disp_width(int new_width)
{
    disp_width = new_width > 0 ? new_width : disp_width;
}
#endif

/* ------------------------------------------------------------------------ */
/*  TIOCGWINSZ with SIGWINCH:  Only call the ioctl if a SIGWINCH happened.  */
/* ------------------------------------------------------------------------ */
#if !defined(WIN32) && defined(CAN_TIOCGWINSZ) && defined(CAN_SIGWINCH)
#include <signal.h>

LOCAL volatile char need_width_ioctl = 1;
LOCAL int           disp_width       = 80;

LOCAL void sigwinch_handler(int sig)
{
    UNUSED(sig);
    signal(SIGWINCH, sigwinch_handler);
    need_width_ioctl = 1;
}

int get_disp_width(void)
{
    struct winsize my_win;

    if (need_width_ioctl)
    {
        need_width_ioctl = 0;
        if (ioctl(0, TIOCGWINSZ, &my_win) == 0)
            disp_width = my_win.ws_col;
    }

    return disp_width;
}

int set_disp_width(int new_width)
{
    disp_width = new_width > 0 ? new_width : disp_width;
    need_width_ioctl = 1;
    return get_disp_width();
}

void init_disp_width(int new_width)
{
    disp_width = new_width > 0 ? new_width : disp_width;
    need_width_ioctl = 1;

    signal(SIGWINCH, sigwinch_handler);
}
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
/*           Copyright (c) 1998-2010, Joseph Zbiciak, Tim Lindner           */
/* ======================================================================== */

