
/*
 * ============================================================================
 *  CP_1600:        CP-1600 Main Core
 *
 *  Author:         J. Zbiciak
 *
 *  $Id: cp1600.c,v 1.24 2001/11/02 02:00:03 im14u2c Exp $
 *
 * ============================================================================
 *  CP1600_INIT        --  Initializes the CP-1600 structure to a basic setup
 *  CP1600_RUN         --  Runs the CP1600 for some number of microcycles.
 *  CP1600_RD          --  Perform a read from the CP1600   (macro in cp_1600.h)
 *  CP1600_WR          --  Perform a write from the CP1600  (macro in cp_1600.h)
 * ============================================================================
 *
 *  Notes/To-do:  
 * 
 *   -- The CP1600_RD_xxx, CP1600_WR_xxx functions must not be called 
 *      directly.  Rather, the CP1600_RD, CP1600_WR macros must be used 
 *      to assure proper address decoding and dispatch.
 *
 *   -- The CP1600 supports interrupts but doesn't currently have a method for
 *      receiving them.  Peripherals wishing to interrupt the CP1600 need to
 *      somehow set cp1600->intrq to '1' to trigger an interrupt.
 *
 *   -- The CP1600 supports external branch conditions.  Peripherals need to 
 *      set cp1600->ext to the appropriate state to trigger these.
 *
 *   -- SIN, TCI, etc. don't do anything yet.  What do they need to do?
 * 
 *   -- Functions should be provided for setting up RAM, ROM images, and
 *      registering peripherals.
 * 
 * ============================================================================
 */


#include "config.h"
#include "periph/periph.h"
#include "cp1600.h"
#include "op_decode.h"
#include "op_exec.h"
#include "emu_link.h"
#include <limits.h>


LOCAL void cp1600_dtor(periph_p p);

/*
 * ============================================================================
 *  CP1600_INIT        -- Initializes a CP1600_T structure
 *
 *  This function sets up a basic CP1600 structure.  It allocates a local "flat
 *  memory" which corresponds to the CP-1600 memory map, and it sets up the
 *  initial state of the instruction decoder logic.
 *
 *  When it's finished, the CP1600 structure is configured to *not* have any
 *  memory deviced enabled at all.  Calls to "CP1600_ADD_xxx" should be issued
 *  to configure the memory map as appropriate.
 * ============================================================================
 */

int
cp1600_init
(
    cp1600_t *cp1600,
    uint_16  rst_vec,
    uint_16  int_vec
)
{
    int i;

    /* -------------------------------------------------------------------- */
    /*  Avoid problems with a garbage cp1600 structure by setting it to     */
    /*  all-bits-zero.  Note:  This could be a portability problem to       */
    /*  machines which represent NULL pointers as something other than      */
    /*  all-bits-zero, but what interesting modern machines do that?        */
    /* -------------------------------------------------------------------- */
    memset((void*)cp1600, 0, sizeof(cp1600_t));


    /* -------------------------------------------------------------------- */
    /*  Set up our interrupt and reset vectors.                             */
    /* -------------------------------------------------------------------- */
    cp1600->r[7]    = rst_vec;
    cp1600->int_vec = int_vec;

    /* -------------------------------------------------------------------- */
    /*  Set the entire memory map to not-cacheable.                         */
    /* -------------------------------------------------------------------- */
    /*  TODO:                                                               */
    /*  Outside routines will need to configure the memory map to make the  */
    /*  CP1600 useful.  :-)  It is especially important to set the          */
    /*  cacheable bits for performance.                                     */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < (1 << (CP1600_MEMSIZE - CP1600_DECODE_PAGE - 5)); i++)
    {
        cp1600->cacheable[i] = 0;
    }

    /* -------------------------------------------------------------------- */
    /*  Mark all instructions as needing decode.  The fn_decode function    */
    /*  will cache the decoded instruction if the "cacheable" bit is set    */
    /*  for the page containing the instruction.                            */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < (1 << CP1600_MEMSIZE); i++)
    {
        cp1600->execute[i] = fn_decode_1st;
    }

    /* -------------------------------------------------------------------- */
    /*  Set up the CP-1600 as a peripheral.                                 */
    /* -------------------------------------------------------------------- */
    cp1600->periph.read     = NULL;
    cp1600->periph.write    = NULL;
    cp1600->periph.peek     = NULL;
    cp1600->periph.poke     = NULL;
    cp1600->periph.reset    = cp1600_reset;
    cp1600->periph.tick     = cp1600_run;
    cp1600->periph.min_tick = 1;
    cp1600->periph.max_tick = 1000000;
/*  cp1600->periph.max_tick = 4; */
    cp1600->periph.dtor     = cp1600_dtor;

    cp1600->snoop.read      = NULL;
    cp1600->snoop.write     = cp1600_write; /* Bus snoop for cache inval.   */
    cp1600->snoop.peek      = NULL;
    cp1600->snoop.poke      = cp1600_write; /* Bus snoop for cache inval.   */
    cp1600->snoop.addr_base = 0;
    cp1600->snoop.addr_mask = 0xFFFF;
    cp1600->snoop.tick      = NULL;
    cp1600->snoop.min_tick  = 0;
    cp1600->snoop.max_tick  = ~0U;
    cp1600->snoop.parent    = (void*)cp1600;
    cp1600->snoop.dtor      = NULL;
    return 0;
}

/*
 * ============================================================================
 *  CP1600_RESET       -- Reset the CP1060
 * ============================================================================
 */
void cp1600_reset(periph_t *p)
{
    cp1600_t *cp1600 = (cp1600_t*)p;

    cp1600->r[0] = 0;
    cp1600->r[1] = 0;
    cp1600->r[2] = 0;
    cp1600->r[3] = 0;
    cp1600->r[4] = 0;
    cp1600->r[5] = 0;
    cp1600->r[6] = 0;
    cp1600->r[7] = 0x1000;

    cp1600->S = cp1600->Z = cp1600->O = cp1600->Z = cp1600->D = 0;
    cp1600->intr = 0;
}

/*
 * ============================================================================
 *  CP1600_RUN         -- Runs the CP1600 for some number of microcycles
 *
 *  This is the main CP1600 loop.  It is responsible for fetching instructions,
 *  decoding them if necessary (or using predecoded instructions if possible)
 *  and calling the required execute functions.
 *
 *  The cp1600_run function will run as many instructions as are necessary to 
 *  just barely exceed the specified number of microcycles.  eg.  It will
 *  execute a new instruction if the specified total number of microcycles
 *  has not yet been exceeded.  The new instruction may exceed the specified
 *  number of microcycles.  The total number of microcycles exhausted is
 *  returned as an int.
 * ============================================================================
 */

uint_32 
cp1600_run 
(
    periph_t *periph, 
    uint_32 microcycles
)
{
    cp1600_t *cp1600 = (cp1600_t*) periph;
    uint_64 start, now, future, next;
    uint_32 tot_microcycles, instrs;
    sint_32 cycles = 0, step, period;
    uint_16 pc;
    cp1600_ins_t execute;
    periph_tick_t instr_tick = cp1600->instr_tick;
    instr_t  *instr;

    start  = cp1600->periph.now;
    now    = start;
    next   = start;
    future = start + microcycles;

    cp1600->hit_bkpt = 0;
/*printf("busrq_until = %d now = %d\n", (int)cp1600->req_bus.busrq_until, (int)now);*/

    /* -------------------------------------------------------------------- */
    /*  Loop until our total count of microcycles exceeds the requested     */
    /*  run period.  If microcycles is greater than 0, then we execute a    */
    /*  minimum of 1 instruction.  Setting "microcycles" to 1 is a great    */
    /*  way to single-step (say, in a debugger or something).               */
    /* -------------------------------------------------------------------- */
    while (now < future)
    {
        int req_bus;
        uint_64 next_rq;

        instrs = 0;
        cycles = 0;

        /* ---------------------------------------------------------------- */
        /*  Notes on encoding of ->intrq, ->intr:                           */
        /*                                                                  */
        /*    cp1600->req_bus.intrq:                                        */
        /*        0 == No interrupts or bus requests pending.               */
        /*        1 == Interrupt Request pending.                           */
        /*        2 == Bus Request pending.                                 */
        /*        3 == Interrupt and Bus Request pending.                   */
        /*                                                                  */
        /*    cp1600->intr:                                                 */
        /*        0 == Uninterruptible instruction (shifts, MVO, etc.)      */
        /*        1 == invalid -- can't happen.                             */
        /*        2 == Interruptible instruction, interrupts disabled.      */
        /*        3 == Interruptible instruction, interrupts enabled.       */
        /*                                                                  */
        /*  Thus, cp1600->req_bus.intrq & cp1600->intr will be nonzero      */
        /*  when the CPU can take the requested action (IRQ or BUSRQ).      */
        /* ---------------------------------------------------------------- */

        /* ---------------------------------------------------------------- */
        /*  If there is an interrupt pending, and we are interruptible,     */
        /*  take the interrupt.                                             */
        /* ---------------------------------------------------------------- */
        req_bus = cp1600->req_bus.intrq & cp1600->intr;
        if (req_bus == 1 || req_bus == 3)
        {
            cp1600->req_bus.intrq &= 2; /* clear pending INTRM */

            if (cp1600->req_bus.intrq_until >= now)
            {
/*printf("saw INTRQ = %d, now = %d, until = %d\n", req_bus, (int)now, (int)cp1600->req_bus.intrq_until);*/
                /* -------------------------------------------------------- */
                /*  Take an interrupt.                                      */
                /*  -- Push new PC onto stack.                              */
                /*  -- Set PC to interrupt vector.                          */
                /*  -- Clear interrupt-pending flag.                        */
                /* -------------------------------------------------------- */
                CP1600_WR(cp1600, cp1600->r[6], cp1600->r[7]);
                cp1600->r[6]++;
                now   += 12;
                cycles = 12;

                pc = cp1600->r[7] = cp1600->int_vec;

                /* -------------------------------------------------------- */
                /*  Ack the interrupt.                                      */
                /* -------------------------------------------------------- */
                cp1600->req_bus.intak = now;
                cp1600->periph.now    = now;

                goto tick;
            }
        }

        /* ---------------------------------------------------------------- */
        /*  If there is a bus request pending and our last instruction      */
        /*  was interruptible, go ahead and yield the bus.                  */
        /* ---------------------------------------------------------------- */
        if (req_bus == 2)
        {
/*printf("saw BUSRQ = %d, now = %d, until = %d diff = %d\n", req_bus, (int)now, (int)cp1600->req_bus.busrq_until,(int)cp1600->req_bus.busrq_until-(int)now);*/
            cp1600->req_bus.intrq &= 1;  /* clear pending BUSRQ */
           
            if (cp1600->req_bus.busrq_until >= now)
            {
                cp1600->req_bus.busak = now;
                if (cp1600->req_bus.do_busak)
                    cp1600->req_bus.do_busak(cp1600->req_bus.do_busak_opaque);

                cycles = cp1600->req_bus.busrq_until - now;
                now    = cp1600->req_bus.busrq_until;
                cp1600->periph.now = now;

                goto tick;
            } else
            {
                cp1600->req_bus.busak = ~0ULL; /* tell requestor we dropped */
            }
        }

        /* ---------------------------------------------------------------- */
        /*  We copy register 7 into the "pc" variable for speed.            */
        /* ---------------------------------------------------------------- */
        pc = cp1600->r[7];

        /* ---------------------------------------------------------------- */
        /*  Execute for up to 5000 microcycles, unless we have a pending    */
        /*  interrupt.  Then only run for about 6 microcycles.              */
        /* ---------------------------------------------------------------- */
        
        step    = (cp1600->req_bus.intrq ? 6 : 5000);

        next_rq = cp1600->req_bus.next_intrq < cp1600->req_bus.next_busrq ?
                  cp1600->req_bus.next_intrq : cp1600->req_bus.next_busrq;

        if (next_rq > now && next_rq < future)
            future = next_rq;

        period = cp1600->instr_tick_per << 2;
        if (instr_tick && period)
            step = step > period ? period : step;

        next += step;
        if (next > future)
        {
            step = future - now;
            next = future;
        }

        if (step < 1)
            continue;

        while (step > 0)
        {
            /* ------------------------------------------------------------ */
            /*  Grab our execute function and instruction pointer.          */
            /* ------------------------------------------------------------ */
            cp1600->oldpc = pc;
            execute       = cp1600->execute[pc];
            instr         = cp1600->instr[pc];

            /* ------------------------------------------------------------ */
            /*  The flag cp1600->intr is our interruptibility state. It is  */
            /*  set equal to our interrupt enable bit, and is cleared by    */
            /*  non-interruptible instructions, thus making it a "logical-  */
            /*  AND" of the two conditions.                                 */
            /* ------------------------------------------------------------ */
            cp1600->intr = 2 | cp1600->I;

            /* ------------------------------------------------------------ */
            /*  Execute the next instruction, and record its cycle count    */
            /*  and new PC value.  Count-down the DBD state.                */
            /* ------------------------------------------------------------ */
            cycles    = execute(instr,cp1600);
            pc        = cp1600->r[7];
            cp1600->D >>= 1;

            /* ------------------------------------------------------------ */
            /*  Tally up instruction count and microcycle count.            */
            /*  We need to update our "now" value for accurate sound emu.   */
            /* ------------------------------------------------------------ */
            cp1600->periph.now = now += cycles;
            step -= cycles;
            instrs++;
        }

        if (cycles == CYC_MAX)
        {
            cp1600->periph.now = now -= cycles;
            next -= step + CYC_MAX;
            instrs--;
            cycles = -1;
        }
            
        /* ---------------------------------------------------------------- */
        /*  Accumulate instructions.                                        */
        /* ---------------------------------------------------------------- */
        cp1600->tot_instr += instrs;

        /* ---------------------------------------------------------------- */
        /*  If we have an "instruction tick" function registered,           */
        /*  go run it.  This is usually the debugger.                       */
        /* ---------------------------------------------------------------- */
tick:   if (instr_tick)
            instr_tick(cp1600->instr_tick_periph, cycles);
    }

    /* -------------------------------------------------------------------- */
    /*  Back out our updates to periph.now and move them to tot_cycles.     */
    /* -------------------------------------------------------------------- */
    tot_microcycles     = now - start;
    cp1600->tot_cycle  += tot_microcycles;
    cp1600->periph.now  = start;

    /* -------------------------------------------------------------------- */
    /*  Return how long we ran for.                                         */
    /* -------------------------------------------------------------------- */
    return tot_microcycles;
}

/*
 * ============================================================================
 *  CP1600_CACHEABLE     -- Marks a region of instruction space as 
 *                          cacheable in the decoder, so that we can cache
 *                          the decoded instructions.  It doesn't actually
 *                          trigger the decode of the instructions though.
 * ============================================================================
 */
void
cp1600_cacheable
(
    cp1600_t    *cp1600,
    uint_32     addr_lo,
    uint_32     addr_hi,
    int         need_snoop
)
{
    uint_32 addr;

    /* -------------------------------------------------------------------- */
    /*  First, pull in addresses at either end of range.  If address range  */
    /*  doesn't span full decode pages at the ends, then don't include the  */
    /*  pages at the ends of the range.                                     */
    /* -------------------------------------------------------------------- */

    if ( (addr_lo >> CP1600_DECODE_PAGE) << CP1600_DECODE_PAGE != addr_lo)
        addr_lo = ((addr_lo >> CP1600_DECODE_PAGE) + 1) << CP1600_DECODE_PAGE;

    addr_hi = (((addr_hi+1) >> CP1600_DECODE_PAGE) << CP1600_DECODE_PAGE) - 1;

    if (addr_hi < addr_lo)
        return;

    /* -------------------------------------------------------------------- */
    /*  Now, step through all of the addresses and mark the instructions    */
    /*  cacheable.                                                          */
    /* -------------------------------------------------------------------- */
    for (addr = addr_lo; addr <= addr_hi; addr += 1 << CP1600_DECODE_PAGE)
    {
        cp1600->cacheable[addr >> (CP1600_DECODE_PAGE + 5)] |= 
                    1 << ((addr >> CP1600_DECODE_PAGE) & 31);
    }

    /* -------------------------------------------------------------------- */
    /*  If this memory range needs snooping (eg. is writable), inform the   */
    /*  peripheral subsystem that the CP-1610 is interested in seeing       */
    /*  memory events in these regions, since it now considers these        */
    /*  locations to be cacheable.                                          */
    /* -------------------------------------------------------------------- */
    if (need_snoop)
        periph_register(cp1600->periph.bus, &cp1600->snoop, 
                        addr_lo, addr_hi, "CP-1610 Snoop");
}

/*
 * ============================================================================
 *  CP1600_INVALIDATE    -- Invalidates a region of cached instructions.
 * ============================================================================
 */
void
cp1600_invalidate
(
    cp1600_t    *cp1600,
    uint_32     addr_lo,
    uint_32     addr_hi
)
{
    uint_32 addr;

    /* -------------------------------------------------------------------- */
    /*  Step through all of the addresses and mark the instructions for     */
    /*  decode.                                                             */
    /* -------------------------------------------------------------------- */
    for (addr = addr_lo; addr <= addr_hi; addr ++)
    {
        if (cp1600->execute[addr] != fn_breakpt)
            cp1600->execute[addr] = fn_decode;
        cp1600->disasm [addr] = NULL;
    }
}

/*
 * ============================================================================
 *  CP1600_WRITE         -- Snoops bus writes and invalidates its cache.
 * ============================================================================
 */
void
cp1600_write
(
    periph_p        per,        /*  Peripheral being written to.        */
    periph_p        req,        /*  Peripheral requesting the write.    */
    uint_32         addr,       /*  Address being written.              */
    uint_32         data        /*  Data being written.                 */
)
{
    cp1600_p cp1600 = (cp1600_p)per->parent;
    uint_16 a0 = addr, a1 = addr-1, a2 = addr-2;

    /* -------------------------------------------------------------------- */
    /*  Step through "addr - 2" to "addr" to invalidate.                    */
    /* -------------------------------------------------------------------- */
    if (cp1600->execute[a0] != fn_breakpt) cp1600->execute[a0] = fn_decode;
    if (cp1600->execute[a1] != fn_breakpt) cp1600->execute[a1] = fn_decode;
    if (cp1600->execute[a2] != fn_breakpt) cp1600->execute[a2] = fn_decode;

    cp1600->disasm[a0] = NULL;
    cp1600->disasm[a1] = NULL;
    cp1600->disasm[a2] = NULL;

    /* -------------------------------------------------------------------- */
    /*  Unused.                                                             */
    /* -------------------------------------------------------------------- */
    UNUSED(req);
    UNUSED(data);
}

/*
 * ============================================================================
 *  CP1600_INSTR_TICK    -- Sets/unsets an per-instruction ticker
 *  
 *  Note:  I may eventually split cp1600_run into two flavors that vary
 *  depending on whether or not I have an instr_tick function registered.
 * ============================================================================
 */ 
void
cp1600_instr_tick
(
    cp1600_t        *cp1600,
    periph_tick_t   instr_tick,
    periph_p        instr_tick_periph
)
{
    cp1600->instr_tick        = instr_tick;
    cp1600->instr_tick_periph = instr_tick_periph;
}

/*
 * ============================================================================
 *  CP1600_SET_BREAKPT   -- Sets a breakpoint at a given address.
 *  CP1600_SET_TRACEPT   -- Like a breakpoint, except it resets itself
 *  
 *  Note:  Instructions which overlap a breakpoint address won't trigger the
 *  breakpoint.
 * ============================================================================
 */ 
int
cp1600_set_breakpt
(
    cp1600_t        *cp1600,
    uint_16         addr, 
    uint_16         flags
)
{
    int was_bkpt = 0;

    if (cp1600->execute[addr] == fn_decode_bkpt ||
        cp1600->execute[addr] == fn_breakpt)
        was_bkpt = 1;

    if (!cp1600->instr[addr])
        cp1600->instr[addr] = get_instr();

    cp1600->instr[addr]->opcode.breakpt.flags |= flags;

    cp1600->execute[addr] = addr == cp1600->r[7] ? fn_decode_bkpt : fn_breakpt;

    return was_bkpt;
}

/*
 * ============================================================================
 *  CP1600_CLR_BREAKPT   -- Clears a breakpoint at a given address.
 * ============================================================================
 */ 
void
cp1600_clr_breakpt
(
    cp1600_t        *cp1600,
    uint_16         addr,
    uint_16         flags
)
{
    if (cp1600->execute[addr] != fn_breakpt &&
        cp1600->execute[addr] != fn_decode_bkpt)
        return;

    cp1600->instr[addr]->opcode.breakpt.flags &= ~flags;

    if (!cp1600->instr[addr]->opcode.breakpt.flags)
        cp1600->execute[addr] = fn_decode;
}

/*
 * ============================================================================
 *  CP1600_CLR_BREAKPT   -- Clears a breakpoint at a given address.
 * ============================================================================
 */ 
void cp1600_dtor(periph_p p)
{
    cp1600_t *cp1600 = (cp1600_t *)p;
    uint_32  addr;

    for (addr = 0; addr <= 0xFFFF; addr++)
    {
        if (cp1600->instr[addr])
        {
            put_instr(cp1600->instr[addr]);
            cp1600->instr[addr] = NULL;
        }
    }

    emu_link_dtor();
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
