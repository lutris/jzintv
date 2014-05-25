/*
 * ============================================================================
 *  Title:    Simple Debugger
 *  Author:   J. Zbiciak
 *  $Id: debug.h,v 1.13 2000/03/21 00:40:22 im14u2c Exp $
 * ============================================================================
 *
 * ============================================================================
 *
 * ============================================================================
 */

#ifndef DEBUG_H_
#define DEBUG_H_

/*
 * ============================================================================
 *  DEBUG_T          -- Debugger peripheral.  Yes, it's a peripheral.
 * ============================================================================
 */
#define DEBUG_MAX_BKPTS   (16)
#define DEBUG_MAX_FILESTK (16)
typedef struct debug_t
{
    periph_t    periph;         /*  Debugger looks like a peripheral.   */
    int         show_rd;        /*  FLAG: show reads being performed.   */
    int         show_wr;        /*  FLAG: show writes being performed.  */
    int         show_ins;       /*  FLAG: show instructions / regs .    */
    cp1600_t    *cp1600;        /*  Pointer to actual CPU.              */
    speed_t     *speed;         /*  Rate control (needed for resync)    */
    gfx_t       *gfx;           /*  So we can toggle windowed mode.     */
    int         step_count;     /*  Number of CPU instrs to step thru.  */
    int         step_over;      /*  FLAG:  Are we stepping over JSRs?   */
    uint_64     tot_instr;      /*  Last observed tot_instr value.      */
    uint_16     bkpt[DEBUG_MAX_BKPTS];
    int         bkpts;
    uint_8      *vid_enable;
    uint_32     *stic_dbg_flags;
    FILE        *filestk[DEBUG_MAX_FILESTK];
    int         filestk_depth;
    int         symb_addr_format;
} debug_t;

/*
 * ============================================================================
 *  DEBUG_RD         -- Capture/print a read event.
 *  DEBUG_WR         -- Capture/print a write event.
 *  DEBUG_TK         -- Debugger 'tick' function.  This where the user command
 *                      line called from.
 * ============================================================================
 */
uint_32 debug_rd(periph_t *p, periph_t *r, uint_32 a, uint_32 d);
void    debug_wr(periph_t *p, periph_t *r, uint_32 a, uint_32 d);
uint_32 debug_tk(periph_t *p, uint_32 len);

/*
 * ============================================================================
 *  DEBUG_DISASM      -- Disassembles one instruction, returning a pointer
 *                       to the disassembled text.  Uses the disassembly
 *                       cache if possible.
 * ============================================================================
 */
char * debug_disasm(periph_t *p, cp1600_t *cp, uint_16 addr, 
                    uint_32 *len, int dbd);

const char * debug_disasm_src(periph_t *p, cp1600_t *cp, uint_16 addr, 
                              uint_32 *len, int dbd, int disasm_width);
/*
 * ============================================================================
 *  DEBUG_DISASM_MEM  -- Disassembles a range of memory locations.
 * ============================================================================
 */
void debug_disasm_mem(periph_t *p, cp1600_t *cp, uint_16 *paddr, uint_32 cnt);

/*
 * ============================================================================
 *  DEBUG_DISPMEM     -- Displays ten lines of "hex dump" memory information.
 *                       The first arg is the address to start dumping at.
 *                       The second arg is the number of addresses to dump.
 * ============================================================================
 */
void debug_dispmem(periph_t *p, uint_16 addr, uint_16 len);

/*
 * ============================================================================
 *  DEBUG_INIT       -- Initializes a debugger object and registers a CPU
 *                      pointer.
 * ============================================================================
 */
int debug_init(debug_t *debug, cp1600_t *cp1600, speed_t *speed, gfx_t *gfx,
               const char *symtbl, uint_8 *vid_enable, uint_32 *stic_dbg_flags,
               const char *script);

/* ======================================================================== */
/*  Some asynchronous fault controls                                        */
/* ======================================================================== */
#define DEBUG_CRASHING   ( 1)
#define DEBUG_ASYNC_HALT (-1)
extern        int  debug_fault_detected;
extern const char *debug_halt_reason;
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
/*                    Copyright (c) 2006, Joseph Zbiciak                    */
/* ======================================================================== */
