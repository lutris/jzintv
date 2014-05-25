/*
 * ============================================================================
 *  Title:    Peripheral Subsystem
 *  Author:   J. Zbiciak
 *  $Id: periph.c,v 1.22 2001/02/03 02:34:21 im14u2c Exp $
 * ============================================================================
 *  PERIPH_NEW       -- Creates a new peripheral bus
 *  PERIPH_DELETE    -- Disposes a peripheral bus
 *  PERIPH_REGISTER  -- Registers a peripheral on the bus
 *  PERIPH_READ      -- Perform a read on a peripheral bus
 *  PERIPH_WRITE     -- Perform a write on a peripheral bus
 *  PERIPH_TICK      -- Perform a tick on a peripheral bus
 * ============================================================================
 *  Peripheral bus information is divided into two sets of information:
 *
 *   -- Bus-wide information stored in a periph_bus_t
 *   -- Per-device information stored in a periph_t
 * 
 *  Peripherals wishing to use the peripheral subsystem need to 'extend'
 *  periph_t by declaring a structure which has a periph_t as its first
 *  element.  This extended structure can then be passed into the periph_*
 *  routines as if it were a 'periph_t', and can be passed to each of the
 *  peripherals as a 'this' pointer.
 * ============================================================================
 */


#include "../config.h"
#include "periph.h"
#include "serializer/serializer.h"


/*
 * ============================================================================
 *  PERIPH_NEW       -- Creates a new peripheral bus
 * ============================================================================
 */
periph_bus_p periph_new
(
    int addr_size,              /*  Address size (in bits)              */
    int data_size,              /*  Data size (in bits)                 */
    int decode_shift            /*  Decode granularity control          */
)
{
    periph_bus_p    bus;
    int             bins, i;

    /* -------------------------------------------------------------------- */
    /*  Sanity check arguments:                                             */
    /*   -- addr_size, data_size must be <= 32 bits.                        */
    /*   -- decode_shift must be < addr_size.                               */
    /* -------------------------------------------------------------------- */
    if (addr_size > 32 || data_size > 32 || decode_shift >= addr_size)
    {
        fprintf(stderr,"FATAL:  invalid periph bus args: %d,%d,%d\n",
                addr_size, data_size, decode_shift);
    }

    /* -------------------------------------------------------------------- */
    /*  Allocate memory for the peripheral bus.                             */
    /* -------------------------------------------------------------------- */
    bus = CALLOC(periph_bus_t, 1);

    if (!bus)
    {   
        fprintf(stderr,"FATAL:  cannot allocate memory for periph bus.\n");
        exit(1);
    }

    /* -------------------------------------------------------------------- */
    /*  Initialize the peripheral bus' private fields.                      */
    /* -------------------------------------------------------------------- */
    bus->addr_mask = ~(~0U << addr_size);
    bus->data_mask = ~(~0U << addr_size);

    bus->decode_shift = decode_shift;

    bus->list = 0;
    bins = addr_size - decode_shift;

    if (! (bus->rd[0] = CALLOC(periph_t *, MAX_PERIPH_BIN << bins)) ||
        ! (bus->wr[0] = CALLOC(periph_t *, MAX_PERIPH_BIN << bins)) )
    {   
        fprintf(stderr,"FATAL:  cannot allocate memory for periph bus.\n");
        exit(1);
    }

    /* -------------------------------------------------------------------- */
    /*  Allocate memory for the decode arrays.                              */
    /* -------------------------------------------------------------------- */
    for (i = 1; i < MAX_PERIPH_BIN; i++)
    {
        bus->rd[i] = bus->rd[i - 1] + (1 << bins);
        bus->wr[i] = bus->wr[i - 1] + (1 << bins);
    }


    /* -------------------------------------------------------------------- */
    /*  Initialize the peripheral substructure of the bus.  Yes, peripheral */
    /*  busses can be peripherals on other busses.  Strange but true.       */
    /* -------------------------------------------------------------------- */
    bus->periph.busy        = 0;
    bus->periph.now         = 0;
    bus->periph.bus         = NULL;
    bus->periph.next        = NULL;
    bus->periph.min_tick    = 1;
    bus->periph.max_tick    = ~0U;
    bus->periph.next_tick   = 0;
    bus->periph.read        = periph_read;
    bus->periph.write       = periph_write;
    bus->periph.tick        = periph_tick;
    //bus->periph.dtor        = periph_delete;

    /* -------------------------------------------------------------------- */
    /*  Set the peripheral bus' default name.                               */
    /* -------------------------------------------------------------------- */
    strncpy(bus->periph.name, "Bus", sizeof(bus->periph.name));

    return bus;
}

/*
 * ============================================================================
 *  PERIPH_DELETE    -- Disposes a peripheral bus, destructing everything
 *                      that's attached to it.
 * ============================================================================
 */
void periph_delete
(
    periph_bus_p bus                /*  Peripheral bus to dispose           */
)
{
    periph_p curr, next;

    /* -------------------------------------------------------------------- */
    /*  Avoid recursion by marking ourselves busy.                          */
    /* -------------------------------------------------------------------- */
    bus->periph.busy = 1;

    /* -------------------------------------------------------------------- */
    /*  Step through all attached periphs and call their dtors.             */
    /* -------------------------------------------------------------------- */
    for (curr = bus->list; curr; curr = next)
    {
        next = curr->next;
        if (curr->dtor && !curr->busy)
        {
            curr->busy = 1;
            curr->dtor(curr);       /* not safe to write to after this */
        }
    }

    /* -------------------------------------------------------------------- */
    /*  Next free the periph_p array ande bus itself.                       */
    /* -------------------------------------------------------------------- */
    free(bus->rd[0]);
    free(bus->wr[0]);
    free(bus);
}

/*
 * ============================================================================
 *  PERIPH_REGISTER  -- Registers a peripheral on the bus
 * ============================================================================
 */
void periph_register
(
    periph_bus_p    bus,        /*  Peripheral bus being registered on. */
    periph_p        periph,     /*  Peripheral being (re)registered.    */
    uint_32         addr_lo,    /*  Low end of address range.           */
    uint_32         addr_hi,    /*  High end of address range.          */
    const char      *name       /*  Name of peripheral.                 */
)
{
    uint_32 bin, addr;
    int i;
    static int unnamed = 0;
    char buf[32];

    /* -------------------------------------------------------------------- */
    /*  Make sure we're registering this peripheral on at most one bus.     */
    /* -------------------------------------------------------------------- */
    if (periph->bus != NULL && periph->bus != bus)
    {
        fprintf(stderr, "FATAL:  Registering a peripheral on multiple "
                        "busses!\n");
        exit(1);
    }

    /* -------------------------------------------------------------------- */
    /*  Make sure we're not registering ourself on ourself.                 */
    /* -------------------------------------------------------------------- */
    if ((periph_p) bus == periph)
    {
        fprintf(stderr, "FATAL:  Loopback peripheral registry not allowed!\n");
        exit(1);
    }
    
    /* -------------------------------------------------------------------- */
    /*  If this is the first time we've registered this peripheral, add it  */
    /*  to our linked list of peripherals.  Since order isn't greatly       */
    /*  important, we just shove it in front.  Also, while we're here,      */
    /*  set the peripheral's name.                                          */
    /* -------------------------------------------------------------------- */
    if (periph->bus == NULL)
    {
        periph->bus  = bus;
        periph->next = bus->list;
        bus->list    = periph;

        /* ---------------------------------------------------------------- */
        /*  If the device has a 'tick' func, add it to our tickable list.   */
        /* ---------------------------------------------------------------- */
        if (periph->tick)
        {
            periph_p tick;

            tick = bus->tickable;
            if (!tick)
            {
                bus->tickable = periph;
            } else 
            {
                while (tick->tickable)
                    tick = tick->tickable;
                tick->tickable = periph;
            }
        }

        /* ---------------------------------------------------------------- */
        /*  Poke in the user-readable name for the device, if supplied.     */
        /*  Otherwise name it 'Unnamed %d'.                                 */
        /* ---------------------------------------------------------------- */
        if (!name)
        {
            snprintf(buf, 32, "Unnamed %d\n", ++unnamed);
            name = buf;
        }
        strncpy(periph->name, name, sizeof(periph->name) - 1);
        periph->name[15] = 0;

        /* ---------------------------------------------------------------- */
        /*  Now register this guy for serialization.                        */
        /* ---------------------------------------------------------------- */
#ifndef NO_SERIALIZER
        if (periph->ser_init)
        {
            periph->ser_init(periph);
            periph->ser_init = NULL;  /* don't double-initialize */
        }
#endif
    }

    /* -------------------------------------------------------------------- */
    /*  Poke the device into our address decode structures.                 */
    /* -------------------------------------------------------------------- */
    if (periph->read)
    for (addr = addr_lo; addr <= addr_hi; addr += 1 << bus->decode_shift)
    {
        bin = (addr & bus->addr_mask) >> bus->decode_shift;

        for (i = 0; i < MAX_PERIPH_BIN && bus->rd[i][bin]; i++)
            if (bus->rd[i][bin] == periph)
                break;

        if (i == MAX_PERIPH_BIN)
        {   
            fprintf(stderr, "FATAL:  >%d read devices in address range "
                            "%.8x..%.8x\n",
                            MAX_PERIPH_BIN,
                            bin << bus->decode_shift, 
                            ((bin + 1) << bus->decode_shift) - 1);
            exit(1);
        }
        
        bus->rd[i][bin] = periph;
    }

    if (periph->write)
    for (addr = addr_lo; addr <= addr_hi; addr += 1 << bus->decode_shift)
    {
        bin = (addr & bus->addr_mask) >> bus->decode_shift;

        for (i = 0; i < MAX_PERIPH_BIN && bus->wr[i][bin]; i++)
            if (bus->wr[i][bin] == periph)
                break;

        if (i == MAX_PERIPH_BIN)
        {   
            fprintf(stderr, "FATAL:  >%d write devices in address range "
                            "%.8x..%.8x\n",
                            MAX_PERIPH_BIN,
                            bin << bus->decode_shift, 
                            ((bin + 1) << bus->decode_shift) - 1);
            exit(1);
        }
        
        bus->wr[i][bin] = periph;
    }

    jzp_printf("%-16s [0x%.4X...0x%.4X]\n", periph->name, 
            addr_lo & bus->addr_mask, addr_hi & bus->addr_mask);
}


/*
 * ============================================================================
 *  PERIPH_READ      -- Perform a read on a peripheral bus
 * ============================================================================
 */
uint_32 periph_read
(
    periph_p    bus,        /*  Peripheral bus being read.          */
    periph_p    req,        /*  Peripheral requesting the read.     */
    uint_32     addr,       /*  Address being read.                 */
    uint_32     data1
)
{
    periph_bus_p    busp = (periph_bus_p)bus;
    periph_p        periph;
    uint_32         bin;
    int             i;
    uint_32         data = busp->data_mask;

    UNUSED(data1);    

    /* -------------------------------------------------------------------- */
    /*  Any peripheral which forwards a read/write request is required to   */
    /*  set its busy and requestor fields around the forwarded request.     */
    /* -------------------------------------------------------------------- */
    bus->busy = 1;
    bus->req  = req;

    bin = (addr & busp->addr_mask) >> busp->decode_shift;

    /* -------------------------------------------------------------------- */
    /*  Perform the peripheral reads.  Peripherals which merely generate    */
    /*  side effects and don't actually drive the bus should return ~0U,    */
    /*  as we return the logical AND of all values received.  (TTL open-    */
    /*  collector behavior of driving the bus low or floating the bus hi.)  */
    /* -------------------------------------------------------------------- */
    for (i = 0; busp->rd[i][bin]; i++)
    {
        periph = busp->rd[i][bin];
        if (!periph->busy)
            data &= periph->read(periph, bus, 
                    (addr - periph->addr_base) & periph->addr_mask, data);
    }

    bus->busy = 0;
    bus->req  = NULL;
    return data;
}

/*
 * ============================================================================
 *  PERIPH_PEEK      -- Perform a read on a peripheral bus w/out side effects
 * ============================================================================
 */
uint_32 periph_peek
(
    periph_p    bus,        /*  Peripheral bus being read.          */
    periph_p    req,        /*  Peripheral requesting the read.     */
    uint_32     addr,       /*  Address being read.                 */
    uint_32     data1
)
{
    periph_bus_p    busp = (periph_bus_p)bus;
    periph_p        periph;
    uint_32         bin;
    int             i;
    uint_32         data = busp->data_mask;

    UNUSED(data1);    

    /* -------------------------------------------------------------------- */
    /*  Any peripheral which forwards a read/write request is required to   */
    /*  set its busy and requestor fields around the forwarded request.     */
    /* -------------------------------------------------------------------- */
    bus->busy = 1;
    bus->req  = req;

    bin = (addr & busp->addr_mask) >> busp->decode_shift;

    /* -------------------------------------------------------------------- */
    /*  Perform the peripheral reads.  Peripherals which merely generate    */
    /*  side effects and don't actually drive the bus should return ~0U,    */
    /*  as we return the logical AND of all values received.  (TTL open-    */
    /*  collector behavior of driving the bus low or floating the bus hi.)  */
    /* -------------------------------------------------------------------- */
    for (i = 0; busp->rd[i][bin]; i++)
    {
        periph = busp->rd[i][bin];
        if (!periph->busy)
            data &= periph->peek(periph, bus, 
                    (addr - periph->addr_base) & periph->addr_mask, data);
    }

    bus->busy = 0;
    bus->req  = NULL;
    return data;
}


/*
 * ============================================================================
 *  PERIPH_WRITE     -- Perform a write on a peripheral bus
 * ============================================================================
 */
void periph_write
(
    periph_p        bus,        /*  Peripheral bus being written.       */
    periph_p        req,        /*  Peripheral requesting the write.    */
    uint_32         addr,       /*  Address being written.              */
    uint_32         data        /*  Data being written.                 */
)
{
    periph_bus_p    busp = (periph_bus_p)bus;
    periph_p        periph;
    uint_32         bin;
    int             i; 

    /* -------------------------------------------------------------------- */
    /*  Any peripheral which forwards a read/write request is required to   */
    /*  set its busy and requestor fields around the forwarded request.     */
    /* -------------------------------------------------------------------- */
    bus->busy = 1;
    bus->req  = req;

    bin = (addr & busp->addr_mask) >> busp->decode_shift;

    /* -------------------------------------------------------------------- */
    /*  Perform the peripheral writes.  Make sure we AND the data being     */
    /*  written with the actual width of the bus.                           */
    /* -------------------------------------------------------------------- */
    data &= busp->data_mask;
    for (i = 0; busp->wr[i][bin]; i++)
    {
        periph = busp->wr[i][bin];
        if (!periph->busy)
            periph->write(periph, bus, 
                          (addr-periph->addr_base) & periph->addr_mask, data);

    }


    bus->busy = 0;
    bus->req  = NULL;
}

/*
 * ============================================================================
 *  PERIPH_POKE      -- Perform a write on a peripheral bus, incl to ROM
 * ============================================================================
 */
void periph_poke
(
    periph_p        bus,        /*  Peripheral bus being written.       */
    periph_p        req,        /*  Peripheral requesting the write.    */
    uint_32         addr,       /*  Address being written.              */
    uint_32         data        /*  Data being written.                 */
)
{
    periph_bus_p    busp = (periph_bus_p)bus;
    periph_p        periph;
    uint_32         bin;
    int             i; 

    /* -------------------------------------------------------------------- */
    /*  Any peripheral which forwards a read/write request is required to   */
    /*  set its busy and requestor fields around the forwarded request.     */
    /* -------------------------------------------------------------------- */
    bus->busy = 1;
    bus->req  = req;

    bin = (addr & busp->addr_mask) >> busp->decode_shift;

    /* -------------------------------------------------------------------- */
    /*  Perform the peripheral writes.  Make sure we AND the data being     */
    /*  written with the actual width of the bus.                           */
    /* -------------------------------------------------------------------- */
    data &= busp->data_mask;
    for (i = 0; busp->wr[i][bin]; i++)
    {
        periph = busp->wr[i][bin];
        if (!periph->busy)
            periph->poke(periph, bus, 
                          (addr-periph->addr_base) & periph->addr_mask, data);

    }


    bus->busy = 0;
    bus->req  = NULL;
}

/*
 * ============================================================================
 *  PERIPH_TICK      -- Perform a tick on a peripheral bus
 * ============================================================================
 */
uint_32 periph_tick
(
    periph_p        bus,        /*  Peripheral bus being ticked.        */
    uint_32         len         /*  How much time has passed.           */
)
{
    periph_bus_p    busp = (periph_bus_p)bus;
    periph_p        tick;
    uint_32         step, diff;
    uint_32         elapsed = 0, ticked;
    uint_64         now = bus->now, soon;


    bus->busy = 1;

    /* -------------------------------------------------------------------- */
    /*  The Tick routine attempts to advance time a total of 'len' units    */
    /*  within the constraints imposed by the peripherals attached.         */
    /*  When calling a peripheral's tick function, it is informed of how    */
    /*  much time has passed since its last tick, and it is expected to     */
    /*  report back how much time it actually processed (since, for some    */
    /*  peripherals, the granularity of the passage of time is larger than  */
    /*  one cycle).                                                         */
    /*                                                                      */
    /*  Different peripherals have different "ticking" requirements.        */
    /*  For instance, the STIC must be ticked exactly 16 times a frame,     */
    /*  giving a tick rate of exactly 960 Hz.  Each tick represents the     */
    /*  passing of 1/960th of a second.  In contrast, the CP-1610 can be    */
    /*  ticked at any time, and said tick can be any duration.              */
    /*                                                                      */
    /*  In order to cope with these diverse requirements, each peripheral   */
    /*  defines a minimum and maximum tick size, the combination of         */
    /*  which specify the constraints on when the devices may be ticked.    */
    /*  For performance reasons, the peripheral bus tries to tick each      */
    /*  peripheral as infrequently as possible (eg. maximize the ticks),    */
    /*  but peripheral interactions will occasionally require more          */
    /*  frequent ticking.                                                   */
    /*                                                                      */
    /*  Ticking is performed in two passes.  The first pass identifies      */
    /*  the "minimum of the maximums" -- eg. the largest step forward in    */
    /*  time that the attached peripherals will let us acheive.  The        */
    /*  second pass will then tick all peripherals whose minimum tick       */
    /*  sizes allow them to be ticked with this size tick.  The process     */
    /*  is repeated (both passes) until the entire total tick size is       */
    /*  consumed.                                                           */
    /*                                                                      */
    /*  One side problem is the fact that each device has a slightly        */
    /*  different picture of what "now" is.  The way this code decides      */
    /*  who to tick and when is by looking at how far each peripheral's     */
    /*  view of "now" is from the peripheral bus' view of "now".  This      */
    /*  implies that the peripheral bus' view of "now" advances once at     */
    /*  the beginning of the process, and the peripherals then stagger      */
    /*  to catch up as they can.                                            */
    /* -------------------------------------------------------------------- */
    
    /* -------------------------------------------------------------------- */
    /*  Iterate until we've used up all of our time.                        */
    /* -------------------------------------------------------------------- */
    soon = bus->now;
    bus->now = now += len;
    while (elapsed < len)
    {
        step = len - elapsed;

        /* ---------------------------------------------------------------- */
        /*  Pass 1:  Iterate through the list of tickables looking for      */
        /*  the peripheral whose view of "now" is sufficiently behind ours  */
        /*  to warrant a tick.  Remember the size of the smallest such      */
        /*  differential.                                                   */
        /* ---------------------------------------------------------------- */
        for (tick = busp->tickable, ticked = 0; tick ; tick = tick->tickable)
        {
            /* ------------------------------------------------------------ */
            /*  If the peripheral's already busy, skip it.                  */
            /* ------------------------------------------------------------ */
            if (tick->busy)
                continue;

            /* ------------------------------------------------------------ */
            /*  Is this device tickable from the standpoint of its minimum  */
            /*  tick length?  If not, then don't even consider it.          */
            /* ------------------------------------------------------------ */
            if (tick->now + tick->min_tick >= now)
                continue;

            /* ------------------------------------------------------------ */
            /*  Does this peripheral represent a new constraint on our      */
            /*  tick size?  If so, then adjust our tick step size.          */
            /* ------------------------------------------------------------ */
            diff = now - tick->now;     /* How far apart we are.            */


            if (diff > tick->max_tick)  /* Bounded by the periph's max tick */
                diff = tick->max_tick;

            if (step > diff)            /* Apply the constraint.            */
                step = diff;

#if 0
jzp_printf("[%-16s] now=%6u  tick->now=%6u  diff=%6u  step=%6u\n",tick->name,(unsigned)now,(unsigned)tick->now, (unsigned)diff, (unsigned)step);
#endif

            ticked++;
        }

/*jzp_printf("now=%d step=%d\n", (int)now, (int)step);*/
        /* ---------------------------------------------------------------- */
        /*  If nobody was considered for ticking, get out of here.          */
        /* ---------------------------------------------------------------- */
        if (!ticked)
            break;
        
        /* ---------------------------------------------------------------- */
        /*  Sanity check.                                                   */
        /* ---------------------------------------------------------------- */
        assert(step);
        soon += step;   /* When will then be now?  SOON! */

        /* ---------------------------------------------------------------- */
        /*  Pass 2:  Actually tick all peripherals by the size of the       */
        /*  tick step we calculated in pass 1.                              */
        /* ---------------------------------------------------------------- */
        for (tick = busp->tickable, ticked = 0 ; tick ; tick = tick->tickable)
        {
            uint_32 periph_step;

            /* ------------------------------------------------------------ */
            /*  If the peripheral's already busy, skip it.                  */
            /* ------------------------------------------------------------ */
            if (tick->busy)
                continue;

            /* ------------------------------------------------------------ */
            /*  Calculate the peripheral-specific step.  We need to do      */
            /*  this because each peripheral has a different concept of     */
            /*  'now', but we're trying to step all of time forward to a    */
            /*  fixed destination.                                          */
            /* ------------------------------------------------------------ */
            periph_step = soon - tick->now;

            /* ------------------------------------------------------------ */
            /*  Is this tick step larger than the peripheral's min_tick?    */
            /*  And if it is, is its concept of 'now' far enough from ours  */
            /*  to allow us to tick it?                                     */
            /* ------------------------------------------------------------ */
            if (tick->min_tick > periph_step || tick->now > soon)
                continue;   /*  Nope:  Skip it. */

#if 0
jzp_printf("ticking %16s with step %.8X, now = %.8X\n", tick->name, periph_step, (uint_32)tick->now);
#endif

            /* ------------------------------------------------------------ */
            /*  Go ahead and tick the peripheral.  Bound the tick size by   */
            /*  the peripheral's maximum tick value.                        */
            /* ------------------------------------------------------------ */
            if (periph_step > tick->max_tick) periph_step = tick->max_tick;

            tick->busy++;
            periph_step = tick->tick(tick, periph_step);
            tick->now += abs(periph_step);
            tick->busy--;

            /* ------------------------------------------------------------ */
            /*  Record whether this peripheral really advanced time.  We    */
            /*  use this to detect the case that none of the peripherals    */
            /*  are able to get useful work done because insufficient time  */
            /*  has passed.                                                 */
            /* ------------------------------------------------------------ */
            ticked += periph_step != 0;
        }

        /* ---------------------------------------------------------------- */
        /*  Update the elapsed time, and break out if nobody ticked.        */
        /* ---------------------------------------------------------------- */
        elapsed += step;

        if (!ticked) break;
    }

    bus->busy = 0;

    return elapsed;
}

/*
 * ============================================================================
 *  PERIPH_RESET     -- Resets all of the peripherals on the bus
 * ============================================================================
 */
void periph_reset
(
    periph_bus_p    bus
)
{
    periph_p        p;

    bus->periph.busy = 1;

    p = bus->list;

    while (p)
    {
        if (p->reset) 
            p->reset(p);
        
        p = p->next; 
    }

    bus->periph.busy = 0;
}


/*
 * ============================================================================
 *  PERIPH_SER_REGISTER -- registers a peripheral for serialization
 * ============================================================================
 */
void periph_ser_register
(
    periph_p    per,
    ser_hier_t  *hier
)
{

#ifndef NO_SERIALIZER
#define SER_REG(x,t,l,f)\
        ser_register(hier, #x, &per->x, t, l, f)

    SER_REG(name,      ser_string, 1, SER_INFO);
    SER_REG(addr_base, ser_u32,    1, SER_INFO|SER_HEX);
    SER_REG(addr_mask, ser_u32,    1, SER_INFO|SER_HEX);
    SER_REG(min_tick,  ser_u32,    1, SER_INFO);
    SER_REG(max_tick,  ser_u32,    1, SER_INFO);
    SER_REG(now,       ser_u64,    1, SER_MAND);
    SER_REG(next_tick, ser_u32,    1, SER_MAND);
#else
    UNUSED(per);
    UNUSED(hier);
#endif
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
/*                 Copyright (c) 1998-1999, Joseph Zbiciak                  */
/* ======================================================================== */
