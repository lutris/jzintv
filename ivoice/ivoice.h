/*
 * ============================================================================
 *  Title:    Intellivoice Emulation
 *  Author:   J. Zbiciak
 *  $Id: ivoice.h,v 1.9 2001/11/02 02:00:03 im14u2c Exp $
 * ============================================================================
 *  This is just a dummy object right now.
 * ============================================================================
 */
#ifndef _IVOICE_H_
#define _IVOICE_H_

typedef struct lpc12_t
{
    int     rpt, cnt;       /* Repeat counter, Period down-counter.         */
    uint_32 per, rng;       /* Period, Amplitude, Random Number Generator   */
    int     amp;
    sint_16 f_coef[6];      /* F0 through F5.                               */
    sint_16 b_coef[6];      /* B0 through B5.                               */
    sint_16 z_data[6][2];   /* Time-delay data for the filter stages.       */
    uint_8  r[16];          /* The encoded register set.                    */
    int     interp; 
} lpc12_t;


typedef struct ivoice_t
{
    periph_t    periph;     /* Yup, this is a peripheral.  :-)              */
    snd_buf_t   snd_buf;    /* Sound circular buffer.                       */

    sint_16    *cur_buf;    /* Current sound buffer.                        */
    int         cur_len;    /* Fullness of current sound buffer.            */

    int         silent;     /* Flag:  Intellivoice is silent.               */

    sint_16    *scratch;    /* Scratch buffer for audio.                    */
    uint_32     sc_head;    /* Head/Tail pointer into scratch circular buf  */
    uint_32     sc_tail;    /* Head/Tail pointer into scratch circular buf  */
    uint_64     sound_current;
    int         sample_frc, sample_int;

    int        *window;     /* Sliding Window.                              */
    int         wind_sum;   /* Window sum.                                  */
    int         wind_ptr;   /* Window pointer.                              */
    int         rate, wind; /* Sample rate, Window size.                    */

    lpc12_t     filt;       /* 12-pole filter                               */
    int         lrq;        /* Load ReQuest.  == 0 if we can accept a load  */
    int         ald;        /* Address LoaD.  < 0 if no command pending.    */
    int         pc;         /* Microcontroller's PC value.                  */
    int         stack;      /* Microcontroller's PC stack.                  */
    int         fifo_sel;   /* True when executing from FIFO.               */
    int         halted;     /* True when CPU is halted.                     */
    uint_32     mode;       /* Mode register.                               */
    uint_32     page;       /* Page set by SETPAGE                          */

    uint_32     fifo_head;  /* FIFO head pointer (where new data goes).     */
    uint_32     fifo_tail;  /* FIFO tail pointer (where data comes from).   */
    uint_32     fifo_bitp;  /* FIFO bit-pointer (for partial decles).       */
    uint_16     fifo[64];   /* The 64-decle FIFO.                           */

    const uint_8 *rom[16];  /* 4K ROM pages.                                */

    char        *smp_tname; /* File name template for samples.              */
    char        *smp_cname; /* File name template for samples.              */
    FILE        *smp_file;  /* File to put Intellivoice samples in.         */
    int         smp_number; /* Sequence number for samples.                 */
    uint_32     *smp_cksum; /* Checksum history to avoid saving repeats.    */
    uint_32     smp_cursum; /* Current checksum.                            */
    uint_32     smp_totsmp; /* total number of samples in sample file.      */
} ivoice_t;


/* ======================================================================== */
/*  IVOICE_TK    -- Where the magic happens.  Generate voice data for       */
/*                  our good friend, the Intellivoice.                      */
/* ======================================================================== */
uint_32 ivoice_tk(periph_t *per, uint_32 len);

/* ======================================================================== */
/*  IVOICE_RD    -- Handle reads from the Intellivoice.                     */
/* ======================================================================== */
uint_32 ivoice_rd(periph_t *per, periph_t *ign, uint_32 addr, uint_32 data);

/* ======================================================================== */
/*  IVOICE_WR    -- Handle writes to the Intellivoice.                      */
/* ======================================================================== */
void ivoice_wr(periph_t *per, periph_t *ign, uint_32 addr, uint_32 data);

/* ======================================================================== */
/*  IVOICE_INIT  -- Makes a new Intellivoice                                */
/* ======================================================================== */
int ivoice_init
(
    ivoice_t        *ivoice,    /*  Structure to initialize.        */
    uint_32         addr,       /*  Base address of ivoice.         */
    snd_t           *snd,       /*  Sound device to register w/.    */
    int             rate,       /*  Desired sample rate.            */
    int             wind,       /*  Sliding window size.            */
    const char      *smp_tname  /*  Sample file name template.      */
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
/*                 Copyright (c) 1998-2000, Joseph Zbiciak                  */
/* ======================================================================== */
