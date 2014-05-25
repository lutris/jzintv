/* ======================================================================== */
/*  REQ_BUS:        Structure to encapsulate INTRQ/BUSRQ/BUSAK stuff.       */
/*  Author:         J. Zbiciak                                              */
/* ======================================================================== */
/*  $Id$ */

#ifndef REQ_BUS_H_
#define REQ_BUS_H_

#define ASSERT_INTRQ (1)
#define ASSERT_BUSRQ (2)

typedef struct req_bus_t
{
    uint_64 intrq_until;
    uint_64 busrq_until;
    uint_64 next_intrq;
    uint_64 next_busrq;
    uint_64 intak;
    uint_64 busak;
    int     intrq;
    void    (*do_busak)(void *);
    void    *do_busak_opaque;
} req_bus_t;

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
/*                   Copyright (c) 2003, Joseph Zbiciak                     */
/* ======================================================================== */
