/*
 * ============================================================================
 *  Title:    CGC Interface for controller pads.
 *  Author:   J. Zbiciak
 *  $Id$
 * ============================================================================
 *  This module implements the controller pads.
 *  Pads are peripherals that extend periph_t.
 * ============================================================================
 */

#ifndef PAD_CGC_H_
#define PAD_CGC_H_

/*
 * ============================================================================
 *  PAD_CGC_T -- CGC interface structure
 * ============================================================================
 */
typedef struct pad_cgc_t
{
#ifdef CGC_SUPPORTED
    periph_t    periph;     /*  Peripheral structure.                       */
    int         num_errors; /*  Number of errors reading CGC.               */
    uint_8      io  [2];    /*  Flag bits:  Is this side set for output?    */
#else
    char        unused;     /*  nearly empty struct if compiled out.        */
#endif

#ifdef CGC_DLL
    uint_32     cgc_num;    /*  Which CGC are we hooked to?                 */
#endif  

#ifdef CGC_THREAD
    int         fd;         /*  File descriptor of CGC.                     */
    v_uint_8    val[2];     /*  Last values read from CGC.                  */
    v_uint_8    die;        /*  Flag telling CGC thread to die.             */
#endif
} pad_cgc_t;

#ifdef CGC_SUPPORTED

/*
 * ============================================================================
 *  PAD_CGC_READ -- Get the current state of the CGC controller.
 * ============================================================================
 */
uint_32 pad_cgc_read(periph_t *, periph_t *, uint_32, uint_32);

/*
 * ============================================================================
 *  PAD_CGC_WRITE -- We need to monitor the I/O state for the pads' ports.
 * ============================================================================
 */
void    pad_cgc_write(periph_t *, periph_t *, uint_32, uint_32);

#endif /*CGC_SUPPORTED*/

/*
 * ============================================================================
 *  PAD_CGC_INIT  -- Initializes an Classic Game Controller
 * ============================================================================
 */
int pad_cgc_init
(
    pad_cgc_t   *pad,           /*  CGC structure to initialize             */
    uint_32     addr,           /*  Base address of pad.                    */
    int         cgc_num,        /*  Which CGC in system to hook to  (win32) */
    const char  *cgc_dev        /*  Device node associated w/ CGC.  (linux) */
);

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
/*                 Copyright (c) 1998-2004, Joseph Zbiciak                  */
/* ======================================================================== */
