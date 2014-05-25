/*
 * ============================================================================
 *  Title:    MAIN
 *  Author:   J. Zbiciak
 *  $Id: jzintv.c,v 1.41 2001/11/02 02:00:02 im14u2c Exp $
 * ============================================================================
 *  Main Simulator Driver File
 * ============================================================================
 *  This doesn't do much yet.  :-)
 * ============================================================================
 */

#include "config.h"

#ifndef macintosh
#ifndef USE_AS_BACKEND
#include "sdl.h"
#endif
#endif

#include <signal.h>
#include "plat/plat.h"
#include "file/file.h"
#include "periph/periph.h"
#include "cp1600/cp1600.h"
#include "mem/mem.h"
#include "icart/icart.h"
#include "bincfg/bincfg.h"
#include "bincfg/legacy.h"
#include "pads/pads.h"
#include "pads/pads_cgc.h"
#include "pads/pads_intv2pc.h"
#include "gfx/gfx.h"
#include "snd/snd.h"
#include "ay8910/ay8910.h"
#include "demo/demo.h"
#include "stic/stic.h"
#include "speed/speed.h"
#include "debug/debug_.h"
#include "event/event.h"
#include "ivoice/ivoice.h"
#include "jlp/jlp.h"
#include "cfg/mapping.h"
#include "cfg/cfg.h"

#ifdef macintosh
# include "console.h"
#endif

cfg_t intv;

double elapsed(int);

/*volatile int please_die = 0;*/
/*volatile int reset = 0;*/

/*static void usage(void);*/

/*
 * ============================================================================
 *  RELEASE      -- Program name / release
 * ============================================================================
 */
static char * release(void)
{
    static char buf[16];

    snprintf(buf, 16, "%d.%d", JZINTV_VERSION_MAJOR, JZINTV_VERSION_MINOR);

    return buf;
}

#if 1
/*
 * ============================================================================
 *  CART_NAME    -- Look for a game name in a cartridge image.
 * ============================================================================
 */
static const char * cart_name(void)
{
    static char name_buf[64];
    uint_16 title_addr, lo, hi, ch;
    int year;
    int i, ps;
    const char *base_name; 
    char *s1, *s2;

    if ((base_name = intv.cart_name) != NULL)
    {
        year = intv.cart_year;
        i = 0;
        goto got_name;
    }

    if ((base_name = strrchr(intv.fn_game, '/')) == NULL &&
        (base_name = strrchr(intv.fn_game, '\\')) == NULL)
        base_name = intv.fn_game;
    else
        base_name++;


    lo = periph_peek((periph_p)intv.intv, (periph_p)intv.intv, 0x500A, ~0);
    hi = periph_peek((periph_p)intv.intv, (periph_p)intv.intv, 0x500B, ~0);

    if ((lo | hi) & 0xFF00)
        return base_name;

    title_addr = ((hi << 8) | lo);

    year = 1900 + periph_peek((periph_p)intv.intv, 
                              (periph_p)intv.intv, title_addr, ~0);

    if (year < 1977 || year > 2050)
        return base_name;

    for (i = 0; i < 64 - 8; i++)
    {
        ch = periph_peek((periph_p)intv.intv, 
                         (periph_p)intv.intv, title_addr + i + 1, ~0);

        name_buf[i] = ch;

        if (ch == 0)
            break;

        if (ch < 32 || ch > 126)
            return base_name;
    }

    ps = 1;
    i  = 0;
    for (s1 = s2 = name_buf; *s1; s1++)
        if (!isspace(*s1) || !ps)
        {
            *s2++ = *s1;
            ps = isspace(*s1);
            if (!ps)
                i = s2 - name_buf;
        }

got_name:
    if (i == 0)
    {
        strncpy(name_buf, base_name, 64-8);
        name_buf[64-8] = 0; 
        i = strlen(name_buf);
    }

    snprintf(name_buf + i, 8, " (%4d)", year);


    return name_buf;
}
#endif

/*
 * ============================================================================
 *  GRACEFUL_DEATH -- Die gracefully.
 * ============================================================================
 */
static void graceful_death(int x)
{
    if (intv.do_exit < 2)
    {
        if (intv.do_exit) fprintf(stderr, "\nOUCH!\n");
        fprintf(stderr, 
                "\nRequesting exit:  Received signal %d.\n"
                "(You may need to press enter if you're at a prompt.)\n", x);
    } else
    {
        fprintf(stderr, "\nReceived 3 signals:  Aborting on signal %d.\n", x);
        exit(1);
    }
    intv.do_exit++;
}

/*
 * ============================================================================
 *  ELAPSED      -- Returns amount of time that's elapsed since the program
 *                  started, in CP1610 clock cycles (895kHz)
 * ============================================================================
 */
double elapsed(int restart)
{
    static double start;
    static int init = 0;
    double now;

    if (!init || restart) 
    {
        start = get_time();
        init = 1;
    }

    now = get_time();

    return (now - start) * 894886.25;
}

/*
 * ============================================================================
 *  DO_GUI_MODE  -- Implement the simple GUI mode remote controls on stdin.
 * ============================================================================
 */

static void do_gui_mode(void)
{
    char cmd;

    while (read(STDIN_FILENO, &cmd, 1) == 1)
    {
        switch (cmd)
        {
            case '\r' : case '\n' : goto na; break; /* ignore CR, LF */
            case 'p'  : intv.do_pause = !intv.do_pause;     break;
            case 'q'  : intv.do_exit  = 1;                  break;
            case 'r'  : intv.do_reset = 2;                  break;
            default   : putchar('!');  goto bad;            break;
        }

        putchar('.');
bad:
        fflush(stdout);
na:     ;
    }
}


/*
 * ============================================================================
 *  In the beginning, there was a main....
 * ============================================================================
 */
#ifdef USE_AS_BACKEND
int jzintv_entry_point(int argc, char *argv[]) 
#else
int main(int argc, char *argv[]) 
#endif
{ 
    int iter = 0, arg;
    double cycles = 0, rate, irate, then, now, icyc;
    uint_32 s_cnt = 0;
    int paused = 0;
    char title[128];

    /* -------------------------------------------------------------------- */
    /*  On Windows, let's try to get back our stdio.                        */
    /* -------------------------------------------------------------------- */
#if 0
#ifdef WIN32
    {
        FILE *newfp;
        newfp   = fopen("CON:", "w");
        if (newfp) *stdout = *newfp;
        newfp   = fopen("CON:", "w");
        if (newfp) *stderr = *newfp;
        newfp   = fopen("CON:", "r");
        if (newfp) *stdin  = *newfp;
    }
#endif
#endif

#ifndef macintosh
    /* -------------------------------------------------------------------- */
    /*  Sneak real quick and see if the user included -h, --help, -?, or    */
    /*  no flags whatsoever.  In those cases, print a message and leave.    */
    /* -------------------------------------------------------------------- */
    if (argc < 2)
    {
        license();
    }

    for (iter = 1; iter < argc; iter++)
    {
        if (!strcmp(argv[iter], "--help") ||
            !strcmp(argv[iter], "-h")     ||
            !strcmp(argv[iter], "-?"))
        {
            usage();
        }
    }
#endif

    /* -------------------------------------------------------------------- */
    /*  Platform-specific initialization.                                   */
    /* -------------------------------------------------------------------- */
    if (plat_init())
    {
        fprintf(stderr, "Error initializing.\n");
        exit(1);
    }

    /* -------------------------------------------------------------------- */
    /*  Temporary hack so ^C, kill work.                                    */
    /* -------------------------------------------------------------------- */
    signal( 2, graceful_death);
    signal(15, graceful_death);

    /* -------------------------------------------------------------------- */
    /*  Parse our arguments and go get an Intellivision!                    */
    /* -------------------------------------------------------------------- */
#ifdef macintosh
    argc = ccommand( &argv );
#endif
#ifdef USE_AS_BACKEND
	optind=0;
#endif	
    cfg_init(&intv, argc, argv);
    init_disp_width(0);
    jzp_flush();

    /* -------------------------------------------------------------------- */
    /*  On Windows, open \\.\UserPort\ if the program has requested         */
    /*  INTV2PC access.  This causes the UserPort driver to yield port      */
    /*  access, if UserPort is active.                                      */
    /* -------------------------------------------------------------------- */
#ifdef WIN32
    if (intv.i2pc0_port || intv.i2pc1_port)
    {
        FILE *UserPortFP;

        UserPortFP = fopen("\\\\.\\UserPort\\", "r");

        if (!UserPortFP)
        {
#if 0
            perror("fopen()");
            fprintf
            (
            stderr, 
            "Warning: Unable to open \\\\.\\UserPort\\.\n"
            "         Under WinNT/2K/XP, jzIntv may be unable to use INTV2PC.\n"
            "         Win95/98/ME users can ignore this warning.\n"
            );
#endif
        }
    }
#endif

    
    /* -------------------------------------------------------------------- */
    /*  Set the window title.  If we recognize a standard Intellivision     */
    /*  ROM header at 0x5000, then also include the cartridge name.         */
    /* -------------------------------------------------------------------- */
    #if 1
    snprintf(title, 128, "jzintv %.32s : %.64s", release(), cart_name());
    title[127] = 0;
    gfx_set_title(&intv.gfx,title);
    #endif

    /* -------------------------------------------------------------------- */
    /*  Run the simulator.                                                  */
    /* -------------------------------------------------------------------- */
    if (intv.debugging)
        debug_tk((periph_p)&(intv.debug),1);

    jzp_printf("Starting jzIntv...\n");
    jzp_flush();

    if (intv.start_dly > 0)
        plat_delay(intv.start_dly);

restart:

    iter = 1;
    now = elapsed(1);
    while (now == elapsed(0))    /* Burn some time. */
        ;

    icyc   = 0;
    s_cnt  = 0;
    cycles = 0;
    if (intv.rate_ctl > 0.0)
        speed_resync(&(intv.speed));

    if (!intv.debugging)
        intv.debug.step_count = ~0U;

    paused = 0;

    while (intv.do_exit == 0)
    {
        uint_64 max_step;
        int do_reset = intv.do_reset;

        if (intv.gui_mode)
            do_gui_mode();

        if (do_reset)
        {
            if (intv.do_reset == 2)
                intv.do_reset = 0;
            max_step = 1000; /* arbitrary */
        } else
        {
            if (s_cnt)
            {
                s_cnt = 0;
                periph_reset(intv.intv);
            }
            /* This is incredibly hackish, and is an outgrowth of my
             * decoupled tick architecture.  */
            max_step = intv.stic.next_phase - intv.stic.stic_cr.now;
#if 1
            if (intv.cp1600.periph.now > intv.stic.stic_cr.now)
            {
                uint_64 diff;
                diff = intv.cp1600.periph.now - intv.stic.stic_cr.now;
                if (diff < max_step)
                    max_step -= diff;
            } else if (intv.stic.stic_cr.now > intv.cp1600.periph.now)
            {
                uint_64 diff;
                diff = intv.stic.stic_cr.now - intv.cp1600.periph.now;
                if (diff < max_step)
                    max_step -= diff;
            } 
#endif
            if (max_step < 5) max_step = 5;
        }

#if 0
jzp_printf("cpu.now = %-8d  stic.now = %-8d diff = %-8d step = %-8d\n", (int)intv.cp1600.periph.now, (int)intv.stic.stic_cr.now, (int)intv.cp1600.periph.now-(int)intv.stic.stic_cr.now, (int)max_step);
#endif

        if (intv.do_pause) 
        { 
            paused = !paused; intv.do_pause = 0; 
            if (!paused)
                speed_resync(&(intv.speed));
        }

        if (intv.event.change_kbd)
        {
            if (intv.event.change_kbd == 5)
            {
                intv.event.cur_kbd = (intv.event.cur_kbd + 1) & 3;
            } else
            if (intv.event.change_kbd == 6)
            {
                intv.event.cur_kbd = (intv.event.cur_kbd - 1) & 3;
            } else
            if (intv.event.change_kbd == 7)
            {
                if (intv.event.prv_kbd)
                {
                    intv.event.cur_kbd = intv.event.prv_kbd - 1;
                    intv.event.prv_kbd = 0;
                }
            } else
            if (intv.event.change_kbd >= 8 && intv.event.change_kbd < 12)
            {
                intv.event.prv_kbd = intv.event.cur_kbd + 1;
                intv.event.cur_kbd = (intv.event.change_kbd - 8) & 3;
            } else
            {
                intv.event.cur_kbd = (intv.event.change_kbd - 1) & 3;
            }

            jzp_clear_and_eol(
                jzp_printf("Change keyboard to %d", intv.event.cur_kbd));
            jzp_flush();
            intv.event.change_kbd = 0;
            memset(intv.pad0.l, 0, sizeof(intv.pad0.l));
            memset(intv.pad0.r, 0, sizeof(intv.pad0.r));
            memset(intv.pad0.k, 0, sizeof(intv.pad0.k));
            memset(intv.pad1.l, 0, sizeof(intv.pad1.l));
            memset(intv.pad1.r, 0, sizeof(intv.pad1.r));
            memset(intv.pad1.k, 0, sizeof(intv.pad1.k));
        }


        if (paused)
        {
            intv.gfx.dirty = 1;
            intv.gfx.periph.tick  ((periph_p)&(intv.gfx),   20000);
            intv.event.periph.tick((periph_p)&(intv.event), 0);
            plat_delay(1000/60);
        } else if (do_reset)
        {
            intv.gfx.dirty = 1;
            intv.gfx.periph.tick  ((periph_p)&(intv.gfx),   20000);
            intv.event.periph.tick((periph_p)&(intv.event), 20000);
            plat_delay(1000/60);
            cycles += 20000;
        } else
            cycles += periph_tick((periph_p)(intv.intv), max_step);

        if (!intv.debugging && intv.debug.step_count == 0)
            intv.debug.step_count = ~0U;

        if (!intv.debugging && !do_reset && (iter++&1023) == 0)
        {

            then  = now;
            now   = elapsed(0);
            rate  = (cycles / now);
            if (now - then > 0.01)
            {
                irate = (cycles - icyc) / (now - then);
                icyc  = cycles;

                jzp_printf("Rate: [%6.2f%% %6.2f%%]  Drop Gfx:[%6.2f%% %6d] "
                       "Snd:[%6.2f%% %2d %6.3f]\r", 
                        rate * 100., irate * 100.,
                        100. * intv.gfx.tot_dropped_frames / intv.gfx.tot_frames, 
                        (int)intv.gfx.tot_dropped_frames,
                        100. * intv.snd.mixbuf.tot_drop / intv.snd.tot_frame, 
                        (int)intv.snd.mixbuf.tot_drop,
                        (double)intv.snd.tot_dirty / intv.snd.tot_frame);

#if 0
                jzp_printf("speed: min=%-8d max=%-8d thresh=%-8.1f frame=%-8d\n", 
                        intv.speed.periph.min_tick,
                        intv.speed.periph.max_tick,
                        intv.speed.threshold * 1e6,
                        intv.gfx.tot_frames);
#endif
                jzp_flush();
            }

            if ((iter&65535) == 1)
            {
                then = elapsed(1);
                cycles = icyc = 0;
            }
        }

        if (do_reset)
        {
            intv.cp1600.r[7] = 0x1000;
            gfx_vid_enable(&(intv.gfx), 0);
            s_cnt++;
        } else 
        {
            if (s_cnt > 140) break;
            s_cnt = 0;
        }
    }

    s_cnt = 0x2A3A4A5A;

    arg = 0;
    gfx_set_bord  (&(intv.gfx), 0);
    gfx_vid_enable(&(intv.gfx), 1);
    intv.gfx.scrshot |= GFX_RESET;

    while (intv.do_exit == 0)
    {
        int i, j;
        uint_8 p;

        if (intv.gui_mode)
            do_gui_mode();

        if (intv.do_reset) arg = 1;
        if (intv.do_reset != 1 && arg) 
        {
            intv.do_reset = 0;

            p = intv.stic.raw[0x2C] & 15;
            for (i = 0; i < 160 * 200; i++)
                intv.stic.disp[i] = p;

            gfx_set_bord  (&(intv.gfx), p);
            intv.gfx.scrshot &= ~GFX_RESET;
            goto restart;
        }
        
        for (i = 0; i < 160 * 200; i++)
        {
            for (j = 0; j < 16; j++)
                s_cnt = (s_cnt << 1) | (1 & ((s_cnt >> 28) ^ (s_cnt >> 30)));


            p = (s_cnt & 0xF) + 16;
            if (p == 16) p = 0;

            intv.stic.disp[i] = p;
        }

        intv.gfx.dirty = 1;
        intv.gfx.periph.tick  ((periph_p)&(intv.gfx),   0);
        intv.event.periph.tick((periph_p)&(intv.event), 0);
        plat_delay(1000/60);
    }

    if (intv.do_exit)
        jzp_printf("\nExited on user request.\n");

    cfg_dtor(&intv);

    return 0;
}


#include "cp1600/op_decode.h"
#include "cp1600/op_exec.h"

/*
 * ============================================================================
 *  DUMP_STATE   -- Called by op_exec:fn_HLT.  Dumps state of universe.
 * ============================================================================
 */

void dump_state(void)
{
    FILE *f;
    int addr, data, i, j;

    f = fopen("dump.mem","wb");
    if (!f) 
    { 
        perror("fopen(\"dump.mem\", \"w\")");
        jzp_printf("couldn't open dump.mem, not dumping memory.\n"); 
    }
    else
    {
        intv.debug.show_rd = 0;
        intv.debug.show_wr = 0;
        for (addr = 0; addr <= 0xFFFF; addr++)
        {   
            data = periph_peek((periph_p)intv.intv, (periph_p)intv.intv,
                               addr, 0);
            fputc((data >> 8) & 0xFF, f);
            fputc((data     ) & 0xFF, f);
        }

        fclose(f);
    }

    f = fopen("dump.cpu", "wb");
    if (!f) 
    { 
        perror("fopen(\"dump.cpu\", \"w\")");
        jzp_printf("couldn't open dump.cpu, not dumping cpu info.\n"); return; 
    }

    fprintf(f, "CP-1600 State Dump\n");
    fprintf(f, "Tot Cycles:   %lld\n", intv.cp1600.tot_cycle);
    fprintf(f, "Tot Instrs:   %lld\n", intv.cp1600.tot_instr);
    fprintf(f, "Tot Cache:    %d\n", intv.cp1600.tot_cache);
    fprintf(f, "Tot NonCache: %d\n", intv.cp1600.tot_noncache);
    fprintf(f, "Registers:    %.4x %.4x %.4x %.4x %.4x %.4x %.4x %.4x\n",
            intv.cp1600.r[0], intv.cp1600.r[1], 
            intv.cp1600.r[2], intv.cp1600.r[3],
            intv.cp1600.r[4], intv.cp1600.r[5], 
            intv.cp1600.r[6], intv.cp1600.r[7]);
    fprintf(f, "Flags:        S:%d C:%d O:%d Z:%d I:%d D:%d intr:%d irq:%d\n",
            intv.cp1600.S, intv.cp1600.C, intv.cp1600.O, intv.cp1600.Z, 
            intv.cp1600.I, intv.cp1600.D,
            intv.cp1600.intr, intv.cp1600.req_bus.intrq);

    fprintf(f, "Cacheability Map:\n");

    for (i = 0; i < 1 << (CP1600_MEMSIZE-CP1600_DECODE_PAGE - 5); i++)
    {
        addr = (i << (CP1600_DECODE_PAGE + 5));

        fprintf(f, "   %.4x-%.4x:", addr, addr+(32<<CP1600_DECODE_PAGE)-1);
        for (j = 0; j < 32; j++)
        {
            fprintf(f, " %d", 1 & (intv.cp1600.cacheable[i] >> j));
        }
        fprintf(f, "\n");
    }


    fprintf(f,"Decoded Instruction Map:\n");

    for (i = 0; i < 1 << (CP1600_MEMSIZE - 6); i++)
    {
        addr = i << 6;

        fprintf(f, "   %.4x-%.4x:", addr, addr + 63);
        for (j = 0; j < 64; j++)
        {
            fprintf(f, "%c", 
                    intv.cp1600.execute[addr + j] == fn_decode_1st ? '-' :
                    intv.cp1600.execute[addr + j] == fn_decode     ? 'N' :
                    intv.cp1600.execute[addr + j] == fn_invalid    ? '!' :
                                                                     'C');
        }
        fprintf(f, "\n");
    }

    fclose(f);
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
/*                 Copyright (c) 1998-2006, Joseph Zbiciak                  */
/* ======================================================================== */
