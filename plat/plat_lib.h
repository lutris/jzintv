/*
 * ============================================================================
 *  Title:    Platform Portability "Library"
 *  Author:   J. Zbiciak
 *  $Id: plat_lib.h,v 1.3 2002/04/17 18:32:53 im14u2c Exp $
 * ============================================================================
 *  This module fills in missing features on various platforms.
 * ============================================================================
 *  GETTIMEOFDAY     -- Return current time in seconds/microseconds.
 *  STRDUP           -- Copy a string into freshly malloc'd storage.
 *  SNPRINTF         -- Like sprintf(), only with bounds checking.
 *  PLAT_DELAY       -- Sleep w/ millisecond precision.
 * ============================================================================
 */

#ifndef _PLAT_LIB_H
#define _PLAT_LIB_H

/* ------------------------------------------------------------------------ */
/*  GET_TIME -- Return current time in seconds as a double.                 */
/* ------------------------------------------------------------------------ */
double get_time(void);

#ifdef NEED_INOUT
/* ------------------------------------------------------------------------ */
/*  IN/INB/OUT/OUTB -- borrowed from glibc                                  */
/* ------------------------------------------------------------------------ */

extern inline unsigned char
inb (unsigned short port)
{
  unsigned char _v;

  __asm__ __volatile__ ("inb %w1,%0":"=a" (_v):"Nd" (port));
  return _v;
}

extern inline unsigned char
inb_p (unsigned short port)
{
  unsigned char _v;

  __asm__ __volatile__ ("inb %w1,%0\noutb %%al,$0x80":"=a" (_v):"Nd" (port));
  return _v;
}

extern inline unsigned short
inw (unsigned short port)
{
  unsigned short _v;

  __asm__ __volatile__ ("inw %w1,%0":"=a" (_v):"Nd" (port));
  return _v;
}

extern inline unsigned short
inw_p (unsigned short port)
{
  unsigned short _v;

  __asm__ __volatile__ ("inw %w1,%0\noutb %%al,$0x80":"=a" (_v):"Nd" (port));
  return _v;
}

extern inline unsigned int
inl (unsigned short port)
{
  unsigned int _v;

  __asm__ __volatile__ ("inl %w1,%0":"=a" (_v):"Nd" (port));
  return _v;
}

extern inline unsigned int
inl_p (unsigned short port)
{
  unsigned int _v;
  __asm__ __volatile__ ("inl %w1,%0\noutb %%al,$0x80":"=a" (_v):"Nd" (port));
  return _v;
}

extern inline void
outb (unsigned char value, unsigned short port)
{
  __asm__ __volatile__ ("outb %b0,%w1"::"a" (value), "Nd" (port));
}

extern inline void
outb_p (unsigned char value, unsigned short port)
{
  __asm__ __volatile__ ("outb %b0,%w1\noutb %%al,$0x80"::"a" (value),
            "Nd" (port));
}

extern inline void
outw (unsigned short value, unsigned short port)
{
  __asm__ __volatile__ ("outw %w0,%w1"::"a" (value), "Nd" (port));

}

extern inline void
outw_p (unsigned short value, unsigned short port)
{
  __asm__ __volatile__ ("outw %w0,%w1\noutb %%al,$0x80"::"a" (value),
            "Nd" (port));
}

extern inline void
outl (unsigned int value, unsigned short port)
{
  __asm__ __volatile__ ("outl %0,%w1"::"a" (value), "Nd" (port));
}

extern inline void
outl_p (unsigned int value, unsigned short port)
{
  __asm__ __volatile__ ("outl %0,%w1\noutb %%al,$0x80"::"a" (value),
            "Nd" (port));
}

extern inline void
insb (unsigned short port, void *addr, unsigned long count)
{
  __asm__ __volatile__ ("cld ; rep ; insb":"=D" (addr),
            "=c" (count):"d" (port), "0" (addr), "1" (count));
}

extern inline void
insw (unsigned short port, void *addr, unsigned long count)
{
  __asm__ __volatile__ ("cld ; rep ; insw":"=D" (addr),
            "=c" (count):"d" (port), "0" (addr), "1" (count));
}

extern inline void
insl (unsigned short port, void *addr, unsigned long count)
{
  __asm__ __volatile__ ("cld ; rep ; insl":"=D" (addr),
            "=c" (count):"d" (port), "0" (addr), "1" (count));
}

extern inline void
outsb (unsigned short port, const void *addr, unsigned long count)
{
  __asm__ __volatile__ ("cld ; rep ; outsb":"=S" (addr),
            "=c" (count):"d" (port), "0" (addr), "1" (count));
}

extern inline void
outsw (unsigned short port, const void *addr, unsigned long count)
{
  __asm__ __volatile__ ("cld ; rep ; outsw":"=S" (addr),
            "=c" (count):"d" (port), "0" (addr), "1" (count));
}

extern inline void
outsl (unsigned short port, const void *addr, unsigned long count)
{
  __asm__ __volatile__ ("cld ; rep ; outsl":"=S" (addr),
            "=c" (count):"d" (port), "0" (addr), "1" (count));
}

#endif

#ifdef NO_INOUT
# define in(port) ((void)port, 0)
# define inb(port) ((void)port, 0)
# define out(val,port) ((void)port, (void)val)
# define outb(val,port) ((void)port, (void)val)
#endif

/* ------------------------------------------------------------------------ */
/*  STRDUP           -- Copy a string into freshly malloc'd storage.        */
/*                                                                          */
/*  Unfortunately, strdup() is not specified by ANSI.  *sigh*               */
/* ------------------------------------------------------------------------ */
#ifdef NO_STRDUP
char * strdup(const char *s);
#endif /* NO_STRDUP */

/* ------------------------------------------------------------------------ */
/*  SNPRINTF         -- Like sprintf(), only with bounds checking.          */
/* ------------------------------------------------------------------------ */
/*  WARNING:  THIS COULD CAUSE BUFFER OVERFLOW PROBLEMS AND IS MERELY       */
/*            A SHIM WHICH IS BEING USED TO GET jzIntv TO COMPILE.          */
/* ------------------------------------------------------------------------ */
#ifdef NO_SNPRINTF
void snprintf(char * buf, int len, const char * fmt, ...);
#endif /* NO_SNPRINTF */

/* ------------------------------------------------------------------------ */
/*  PLAT_DELAY       -- Sleep w/ millisecond precision.                     */
/* ------------------------------------------------------------------------ */
void plat_delay(unsigned delay);

#ifdef NO_RAND48
#define srand48 srand_jz
#define lrand48 rand_jz
#define drand48 drand_jz
#endif

void    srand_jz(uint_32);
uint_32 rand_jz(void);
double  drand_jz(void);

/* ------------------------------------------------------------------------ */
/*  CALLOC wrapper for type checking / C++ compatibility.                   */
/* ------------------------------------------------------------------------ */
#define CALLOC(t,s) ((t*)calloc(sizeof(t),(s)))

#if 0
static inline void *CALLOC_(const char *file, const int line, size_t x, size_t y)
{
    void *tmp = calloc(x, y); 
    
    printf("%s:%d allocated %d*%d @ %16p\n", file, line, x, y, tmp);
    return tmp;
}

#define CALLOC(t,s)  (t*)CALLOC_(__FILE__, __LINE__, sizeof(t), (s))
#endif

/* ------------------------------------------------------------------------ */
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
/* ------------------------------------------------------------------------ */
int  get_disp_width(void);
int  set_disp_width(int);
void init_disp_width(int);

#endif /* PORT_LIB_H */

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
