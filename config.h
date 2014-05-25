/*
 * ============================================================================
 *  Title:    Project-Wide Config
 *  Author:   J. Zbiciak
 *  $Id: config.h,v 1.14 2001/11/02 02:00:02 im14u2c Exp $
 * ============================================================================
 *  _BIG_ENDIAN         -- Host machine is big endian
 *  _LITTLE_ENDIAN      -- Host machine is little endian
 * ============================================================================
 *
 * ============================================================================
 */

#ifndef _CONFIG_H
#define _CONFIG_H


/*
 * ============================================================================
 *  If you get an error here, define BYTE_BE or BYTE_LE as is required for 
 *  your host machine!  You can do that in your Makefile by adding it to
 *  CFLAGS, or by uncommenting the appropriate #define below.
 * ============================================================================
 */

/* #define BYTE_BE */  /* Uncomment for big endian    */
/* #define BYTE_LE */  /* Uncomment for little endian */

#if !defined(BYTE_BE) && !defined(BYTE_LE)

#  if defined(__BIG_ENDIAN__)
#    define BYTE_BE
#  endif

#  if defined(__LITTLE_ENDIAN__)
#    define BYTE_LE
#  endif

#  if !(defined(BYTE_BE) || defined(BYTE_LE)) && \
       (defined(sparc)   || defined(__sparc)    || defined(__sparc__)   || \
        defined(sparc64) || defined(__sparc64)  || defined(__sparc64__) || \
        defined(ppc)     || defined(__ppc)      || defined(__ppc__)     || \
        defined(ppc64)   || defined(__ppc64)    || defined(__ppc64__)   || \
        defined(POWERPC) || defined(__POWERPC)  || defined(__POWERPC__))
#    define BYTE_BE    
#  endif

#  if !(defined(BYTE_BE) || defined(BYTE_LE)) && \
       (defined(i386)    || defined(__i386)     || defined(__i386__)    || \
        defined(x86_64)  || defined(__x86_64)   || defined(__x86_64__)  || \
        defined(amd64)   || defined(__amd64)    || defined(__amd64__)   || \
        defined(ia64)    || defined(__ia64)     || defined(__ia64__)    || \
        defined(alpha)   || defined(__alpha)    || defined(__alpha__)) 
#    define BYTE_LE
#  endif

#  if !defined(BYTE_BE) && !defined(BYTE_LE)
#    include <endian.h>
#    ifndef __BYTE_ORDER
#      error Please manually set your machine endian in 'config.h'
#    endif
#    if __BYTE_ORDER==4321
#      define BYTE_BE
#    endif
#    if __BYTE_ORDER==1234
#      define BYTE_LE
#    endif
#    if !defined(BYTE_BE) && !defined(BYTE_LE)
#      error Cannot determine target endian.  See 'config.h' for details.
#    endif
#  endif

#endif

#if defined(BYTE_BE) && defined(BYTE_LE)
#  error Both BYTE_BE and BYTE_LE defined.  Pick only 1!
#endif

#if !defined(BYTE_BE) && !defined(BYTE_LE)
#  error One of BYTE_BE or BYTE_LE must be defined.
#endif

/* ======================================================================== */
/*  BFE         -- Builds a `B'it`F'ield structure in the correct order as  */
/*                 required for the host machine's `E'ndian.                */
/* ======================================================================== */
#ifdef BYTE_BE
#  define BFE(x,y) y; x
#else /* BYTE_LE */
#  define BFE(x,y) x; y
#endif

/* ======================================================================== */
/*  BSWAP_16         -- Byte-swap a 16-bit value.                           */
/*  BSWAP_32         -- Byte-swap a 16-bit value.                           */
/*  HOST_TO_LE_16    -- Converts host byte order to little endian           */
/*  HOST_TO_LE_32    -- Converts host byte order to little endian           */
/*  HOST_TO_BE_16    -- Converts host byte order to big endian              */
/*  HOST_TO_BE_32    -- Converts host byte order to big endian              */
/*  LE_TO_HOST_16    -- Converts little endian to host byte order           */
/*  LE_TO_HOST_32    -- Converts little endian to host byte order           */
/*  BE_TO_HOST_16    -- Converts big endian to host byte order              */
/*  BE_TO_HOST_32    -- Converts big endian to host byte order              */
/* ======================================================================== */

# define BSWAP_16(x) ((((x) << 8) & 0xFF00) | (((x) >> 8) & 0x00FF))
# define BSWAP_32(x) ((((x) << 24) & 0xFF000000) |                     \
                      (((x) <<  8) & 0x00FF0000) |                     \
                      (((x) >>  8) & 0x0000FF00) |                     \
                      (((x) >> 24) & 0x000000FF))                      \

#ifdef BYTE_LE
# define HOST_TO_LE_16(x) (x)
# define HOST_TO_LE_32(x) (x)
# define HOST_TO_BE_16(x) BSWAP_16(x)
# define HOST_TO_BE_32(x) BSWAP_32(x)
# define LE_TO_HOST_16(x) (x)
# define LE_TO_HOST_32(x) (x)
# define BE_TO_HOST_16(x) BSWAP_16(x)
# define BE_TO_HOST_32(x) BSWAP_32(x)
#else
# define HOST_TO_BE_16(x) (x)
# define HOST_TO_BE_32(x) (x)
# define HOST_TO_LE_16(x) BSWAP_16(x)
# define HOST_TO_LE_32(x) BSWAP_32(x)
# define BE_TO_HOST_16(x) (x)
# define BE_TO_HOST_32(x) (x)
# define LE_TO_HOST_16(x) BSWAP_16(x)
# define LE_TO_HOST_32(x) BSWAP_32(x)
#endif

/* ======================================================================== */
/*  Use void casts to indicate when variables or values are unused.         */
/* ======================================================================== */
#define UNUSED(x) ((void)(x))

/* ======================================================================== */
/*  If our compiler supports 'inline', enable it here.                      */
/* ======================================================================== */
#if defined(GNUC) || defined(_TMS320C6X)
# define INLINE inline
#else
# define INLINE
#endif

/* ======================================================================== */
/*  Target-specific types, according to size.                               */
/* ======================================================================== */
typedef unsigned char       uint_8;
typedef unsigned short      uint_16;
typedef unsigned int        uint_32;
typedef unsigned long long  uint_64;

typedef signed   char       sint_8;
typedef signed   short      sint_16;
typedef signed   int        sint_32;
typedef signed   long long  sint_64;

typedef volatile uint_8     v_uint_8;
typedef volatile uint_16    v_uint_16;
typedef volatile uint_32    v_uint_32;
typedef volatile uint_64    v_uint_64;

typedef volatile sint_8     v_sint_8;
typedef volatile sint_16    v_sint_16;
typedef volatile sint_32    v_sint_32;
typedef volatile sint_64    v_sint_64;



/* ======================================================================== */
/*  Endian conversion macros                                                */
/* ======================================================================== */
#ifdef BYTE_BE
#define be_to_host_16(x) (x)
#define be_to_host_32(x) (x)
#define le_to_host_16(x) ((0x00FF & ((uint_16)(x) >> 8)) | \
                          (0xFF00 & ((uint_16)(x) << 8)))
#define le_to_host_32(x) ((0x000000FF & ((uint_32)(x) >> 24)) | \
                          (0x0000FF00 & ((uint_32)(x) >>  8)) | \
                          (0x00FF0000 & ((uint_32)(x) <<  8)) | \
                          (0xFF000000 & ((uint_32)(x) << 24)))
#endif

#ifdef BYTE_LE
#define le_to_host_16(x) (x)
#define le_to_host_32(x) (x)
#define be_to_host_16(x) ((0x00FF & ((uint_16)(x) >> 8)) | \
                          (0xFF00 & ((uint_16)(x) << 8)))
#define be_to_host_32(x) ((0x000000FF & ((uint_32)(x) >> 24)) | \
                          (0x0000FF00 & ((uint_32)(x) >>  8)) | \
                          (0x00FF0000 & ((uint_32)(x) <<  8)) | \
                          (0xFF000000 & ((uint_32)(x) << 24)))

#endif

#define host_to_le_16(x) le_to_host_16(x)
#define host_to_be_16(x) be_to_host_16(x)
#define host_to_le_32(x) le_to_host_32(x)
#define host_to_be_32(x) be_to_host_32(x)


/* ======================================================================== */
/*  Target-specific library compatibility issues                            */
/* ======================================================================== */

#if defined (__APPLE__) && defined(__MACH__) && !defined(macosx)
# define macosx
#endif

#ifdef linux
# define USE_STRCASECMP
# define DEFAULT_ROM_PATH ".:=../rom:/usr/local/share/jzintv/rom"
# define HAS_LINK
# define DEFAULT_AUDIO_HZ     (48000)
# define SND_BUF_SIZE_DEFAULT (2048)
# define SND_BUF_CNT_DEFAULT  (3)
# define CAN_TIOCGWINSZ
# define CAN_SIGWINCH
# define USE_TERMIO
#endif

#ifdef SOLARIS
/*# define NO_SNPRINTF*/
# define NO_GETOPT_LONG
# define NO_INOUT
# define USE_STRCASECMP
# define DEFAULT_ROM_PATH ".:=../rom:/usr/local/share/jzintv/rom"
# define HAS_LINK
# define CAN_TIOCGWINSZ
# define CAN_SIGWINCH
#endif

#ifdef WIN32
# define NO_GETTIMEOFDAY
/*# define NO_SNPRINTF*/
# define NO_GETOPT_LONG
# define NOGETOPT
# define NO_SETUID
# define NO_NANOSLEEP
# define NO_RAND48
# define NO_FCNTL
# define USE_FCNTL_H
# define USE_MKTEMP
# define NEED_INOUT
# define DEFAULT_ROM_PATH ".;=..\\rom"
# define PATH_SEP '\\'
# define PATH_COMPONENT_SEP ";"
# define NO_SETUID
# define DEFAULT_AUDIO_HZ (48000)
#endif

#ifdef macintosh
# define NO_GETTIMEOFDAY
# define NO_STRDUP
# define NO_SYS_TIME_H
# define NO_SDL_DELAY
# define NO_SETUID
# define NO_NANOSLEEP
# define NO_RAND48
# define NO_INOUT
# define NO_STRICMP /* ? */
# define NO_FCNTL
# define DEFAULT_ROM_PATH "."
# define PATH_SEP ':'
# define PATH_COMPONENT_SEP ";"
# define NO_SETUID
# define NO_GETCWD
#endif

#ifdef macosx
# define NO_RAND48
# define NO_INOUT
# define NO_GETOPT_LONG
# define NOGETOPT
# define USE_STRCASECMP /* ? */
# define DEFAULT_ROM_PATH ".:=../rom"
# define HAS_LINK
# define DEFAULT_AUDIO_HZ (48000)
# define CAN_TIOCGWINSZ
# define CAN_SIGWINCH
# define USE_SYS_IOCTL
#endif

#ifdef __FreeBSD__
# define NO_INOUT
# define USE_STRCASECMP
# define DEFAULT_ROM_PATH ".:=../rom:/usr/local/share/jzintv/rom"
# define HAS_LINK
# define CAN_TIOCGWINSZ
# define CAN_SIGWINCH
# define USE_SYS_IOCTL
#endif

#ifdef _TMS320C6X
# define NO_GETTIMEOFDAY
# define NO_STRDUP
# define NO_SYS_TIME_H
# define NO_UNISTD_H
# define NO_GETOPT_LONG
# define NO_SETUID
# define NO_NANOSLEEP
# define NO_RAND48
# define NO_INOUT
# define NO_STRICMP
# define HAVE_RESTRICT
# define NO_FCNTL
# ifndef CLK_TCK
#  define CLK_TCK 200000000 /* Assume 200MHz C6201 device */
# endif
# define DEFAULT_ROM_PATH "."
# define FULLSC_START_DLY   (0)
# define NO_SETUID
# define NO_GETCWD
#endif

#ifdef GP2X
# define FULLSC_START_DLY   (0)
# define SMALLMEM
#endif

#ifdef __AMIGAOS4__
# define NO_GETOPT_LONG
# define NO_SETUID
# define PATH_SEP '/'
# define PATH_COMPONENT_SEP ";"
# define DEFAULT_ROM_PATH ".:=../rom"
# define NO_GETCWD
#endif

#ifdef WII
# define NO_ACCESS
# define NO_GETOPT_LONG
# define NO_GETTIMEOFDAY
# define NO_FCNTL
# define NO_SETUID
# define NO_SDL_DELAY
# define NO_CLOCK
# define PATH_SEP '/'
# define PATH_COMPONENT_SEP ";"
# define DEFAULT_CFG_PATH "sd:/jzintvWii/cfg"
# define DEFAULT_ROM_PATH "sd:/jzintvWii/roms"
# define DEFAULT_AUDIO_HZ (48000)
# define SND_BUF_SIZE_DEFAULT (512)
# define SND_BUF_CNT_DEFAULT  (3)
# define NO_GETCWD
#endif

/*
 * ============================================================================
 *  Clean up per-arch configs w/ some defaults.
 * ============================================================================
 */

#ifndef PATH_SEP
# define PATH_SEP '/'
#endif

#ifndef PATH_COMPONENT_SEP 
# define PATH_COMPONENT_SEP ":"
#endif

#ifndef DEFAULT_ROM_PATH
# define DEFAULT_ROM_PATH NULL
#endif

#ifndef FULLSC_START_DLY
# define FULLSC_START_DLY (500)
#endif

#ifndef DEFAULT_AUDIO_HZ
# define DEFAULT_AUDIO_HZ (44100)
#endif

#if !defined(SND_BUF_SIZE_DEFAULT)
# define SND_BUF_SIZE_DEFAULT (2048)
#endif

#if !defined(SND_BUF_CNT_DEFAULT)
# define SND_BUF_CNT_DEFAULT  (2)
#endif


/*
 * ============================================================================
 *  CGC support configuration
 * ============================================================================
 */

#if defined(WIN32)
#define CGC_SUPPORTED
#define CGC_DLL
#endif

#if defined(macosx)
#define CGC_SUPPORTED
#define CGC_THREAD
#endif


#if defined(linux)
#define CGC_SUPPORTED
#define CGC_THREAD
#endif

/*
 * ============================================================================
 *  Standard #includes that almost everyone needs
 * ============================================================================
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>
#include <math.h>

#ifndef NO_SYS_TIME_H
# include <sys/time.h>
#endif

#ifndef NO_UNISTD_H
# include <unistd.h>
#endif

#ifdef _TMS320C6X           /* This seems to be TI-specific.    */
# include <file.h>
#endif

#ifdef linux
# include <sys/io.h>
#endif

#ifdef NO_GETOPT_LONG
# include "plat/gnu_getopt.h"
#else
# include <getopt.h>
#endif

#ifdef USE_STRCASECMP
# include <strings.h>
# define stricmp strcasecmp
#endif

#if !defined(NO_FCNTL) || defined(USE_FCNTL_H)
# include <fcntl.h>
#endif

#ifndef M_PI
# ifdef PI
#  define M_PI PI
# else
#  define M_PI (3.14159265358979323846)
# endif
#endif

/*
 * ============================================================================
 *  If this compiler implements the C99 'restrict' keyword, then enable it.
 * ============================================================================
 */
#ifdef HAVE_RESTRICT
# define RESTRICT restrict
#endif

#ifdef GNU_RESTRICT
# define RESTRICT __restrict__
#endif

#ifndef RESTRICT
# define RESTRICT
#endif

/*
 * ============================================================================
 *  Allow exposing "local" symbols by using LOCAL instead of static
 * ============================================================================
 */
#ifndef LOCAL
# define LOCAL static
#else
# warning "LOCAL already defined before config.h"
#endif

/*
 * ============================================================================
 *  Include the "platform library" to handle missing functions
 * ============================================================================
 */

#include "plat/plat_lib.h"

/* We use this everywhere, so just include it here. */
#include "misc/jzprint.h"

/*
 * ============================================================================
 *  No other good place for this at the moment.  :-(
 * ============================================================================
 */
extern void dump_state(void);
#define CONDFREE(x) do { if (x) free((void*)x); (x) = NULL; } while (0)

#define NO_SERIALIZER

/*
 * ============================================================================
 *  Version number
 * ============================================================================
 */

#ifndef JZINTV_VERSION_MAJOR
# define JZINTV_VERSION_MAJOR (0)
#endif

#ifndef JZINTV_VERSION_MINOR
# define JZINTV_VERSION_MINOR (0)
#endif

#define JZINTV_VERSION ((JZINTV_VERSION_MAJOR << 8) | JZINTV_VERSION_MINOR)

#endif /* CONFIG_H */

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
/*                 Copyright (c) 1998-2006, Joseph Zbiciak                  */
/* ======================================================================== */
