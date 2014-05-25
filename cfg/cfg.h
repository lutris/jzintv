/*
 * ============================================================================
 *  Title:    Configuration Manager
 *  Author:   J. Zbiciak
 *  $Id: cfg.h,v 1.4 2001/02/03 02:34:21 im14u2c Exp $
 * ============================================================================
 *  This module manages the machine configuration.  It does commandline
 *  parsing and processes the configuration elements that were read in 
 *  via the config-file parser.
 *
 *  CFG owns the entire machine -- it is encapsulated in a cfg_t.
 * ============================================================================
 *  CFG_INIT     -- Parse command line and get started
 *  CFG_FILE     -- Parse a config file and extend the state of the machine.
 * ============================================================================
 */

#ifndef _CFG_H_
#define _CFG_H_

#ifndef USAGE_C_
/*
 * ============================================================================
 *  CFG_T            -- The current machine and its configuration
 * ============================================================================
 */

typedef struct cfg_t
{
    /* -------------------------------------------------------------------- */
    /*  Our baby, the Intellivision itself.                                 */
    /* -------------------------------------------------------------------- */
    periph_bus_p intv;          /* Top level Intellivision object.          */

    /* -------------------------------------------------------------------- */
    /*  System 'peripherals' -- these aren't actual hardware devices.       */
    /* -------------------------------------------------------------------- */
    gfx_t       gfx;            /* Graphics interface layer.                */
    snd_t       snd;            /* Sound interface layer.                   */
    speed_t     speed;          /* Rate controller.                         */
    event_t     event;          /* Event subsystem.                         */
    debug_t     debug;          /* Debugger hooks.                          */

    /* -------------------------------------------------------------------- */
    /*  Hardware peripherals -- these model actual pieces of the Intv.      */
    /* -------------------------------------------------------------------- */
    cp1600_t    cp1600;         /* CPU.                                     */
    ay8910_t    psg0;           /* Base unit's Prog. Sound Generator.       */
    ay8910_t    psg1;           /* ECS's Prog. Sound Generator.             */
    ivoice_t    ivoice;         /* SP0256 Emulation.                        */
    stic_t      stic;           /* Standard Television Interface Chip       */
    pad_t       pad0;           /* Base unit's controller pads.             */
    pad_t       pad1;           /* ECS's controller pads.                   */
    pad_intv2pc_t i2pc0,i2pc1;  /* Up to 2 INTV2PCs may be attached.        */
    pad_cgc_t   cgc0, cgc1;     /* Up to 2 CGCs may be attached.            */
    mem_t       exec;           /* EXEC ROM at 0x1000 - 0x1FFF              */
    mem_t       exec2;          /* EXEC2 ROM at 0x0400 - 0x04FF             */
/*  mem_t       grom; */        /* Graphics ROM at 0x3000 - 0x37FF          */
/*  mem_t       gram; */        /* Graphics RAM at 0x3800 - 0x3FFF          */
    mem_t       sys_ram;        /* 16-bit System RAM at 0x200 - 0x35F       */
    mem_t       sys_ram2;       /* RA-3-9600A (SysRAM), 0x360 - 0x3FF       */
    mem_t       scr_ram;        /* 8-bit Scratchpad RAM at 0x100 - 0x1EF    */
    mem_t       glt_ram;        /* glitch RAM */
    icart_t     icart;          /* Game program.                            */
    legacy_t    legacy;         /* Legacy BIN+CFG game program              */
    mem_t       game0;
    mem_t       game1;
    mem_t       game2;
    mem_t       ecs0;
    mem_t       ecs1;
    mem_t       ecs2;
    mem_t       ecs_ram;        /* 8-bit Scratchpad RAM at 0x4000 - 0x47FF  */
    jlp_t       jlp;            /* Jean-Luc Project Support                 */

    uint_16     exec_img[4096 + 256];
    uint_16     grom_img[2048      ];
    uint_16     ecs_img [4096 * 3];

    /* -------------------------------------------------------------------- */
    /*  User-modifiable flags, etc.                                         */
    /* -------------------------------------------------------------------- */
    int         audio_rate;     /* Sample rate for audio.  0 == no audio    */
    int         accutick;       /* Min PSG ticks to sim on PSG write.       */
    int         psg_window;     /* Window size for PSG sliding window       */
    int         ecs_enable;     /* ECS enable/disable flag.                 */
    int         ivc_enable;     /* Ivoice enable/disable flag.              */
    int         ivc_window;     /* Window size for Ivoice sliding window.   */
    char       *ivc_tname;      /* Intellivoice sample file name template.  */
    int         gfx_flags;      /* SDL mode flags (fullsc/windowed, etc)    */
    int         disp_res;       /* Display resolution.                      */
    uint_32     i2pc0_port;     /* INTV2PC #0 I/O address.                  */
    uint_32     i2pc1_port;     /* INTV2PC #1 I/O address.                  */
    int         cgc0_num;       /* CGC #0's ID number.                      */
    int         cgc1_num;       /* CGC #1's ID number.                      */
    char       *cgc0_dev;       /* CGC #0's device node                     */
    char       *cgc1_dev;       /* CGC #1's device node                     */
    int         debugging;      /* Debugger enabled flag.                   */
    int         gui_mode;       /* Are we running in "GUI mode?"            */
    int         start_dly;      /* Delay at startup if we need to.          */
    double      rate_ctl;       /* Target rate.  1.0 is "normal speed."     */

    char       *fn_exec;        /* File name of EXEC image.                 */
    char       *fn_grom;        /* File name of GROM image.                 */
    char       *fn_game;        /* File name of GAME image.                 */
    char       *fn_ecs;         /* File name of ECS image.                  */

    /* -------------------------------------------------------------------- */
    /*  State flags, such as the global "exit" flag, etc.                   */
    /* -------------------------------------------------------------------- */
    v_uint_32   do_exit;        /* Signal that an exit is requested.        */
    v_uint_32   do_reset;       /* Signal that a RESET is requested.        */
    v_uint_32   do_pause;       /* Signal that we are paused.               */

    /* -------------------------------------------------------------------- */
    /*  Key bindings                                                        */
    /* -------------------------------------------------------------------- */
    cfg_kbd_t   *binding;

    /* -------------------------------------------------------------------- */
    /*  Demo recorder.                                                      */
    /* -------------------------------------------------------------------- */
    demo_t      demo;

    /* -------------------------------------------------------------------- */
    /*  Other misc details about the game                                   */
    /* -------------------------------------------------------------------- */
    const char *cart_name;
    int         cart_year;
} cfg_t;

extern cfg_t intv;

/* ======================================================================== */
/*  CFG_GET_EVTACT   -- Convert an event action name into an event action   */
/*                      index.  This is a horrible linear search.  :-P      */
/*                      Hey, it works for now.                              */
/* ======================================================================== */
int cfg_get_evtact(const char *event);

/* ======================================================================== */
/*  CFG_SETBIND  -- Set all of the key-bindings for the Intellivision.      */
/* ======================================================================== */
int cfg_setbind(cfg_t *cfg, const char *kbdhackfile);

/* ======================================================================== */
/*  CFG_INIT     -- Parse command line and get started                      */
/* ======================================================================== */
void cfg_init(cfg_t *cfg, int argc, char * argv[]);

/* ======================================================================== */
/*  CFG_DTOR     -- Destroy a constructed Intellivision                     */
/* ======================================================================== */
void cfg_dtor(cfg_t *cfg);
#endif

/* ======================================================================== */
/*  USAGE            -- Just give usage info and exit.                      */
/* ======================================================================== */
void usage(void);

/* ======================================================================== */
/*  LICENSE          -- Just give license/authorship info and exit.         */
/* ======================================================================== */
void license(void);

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
/*                 Copyright (c) 1998-2001, Joseph Zbiciak                  */
/* ======================================================================== */
