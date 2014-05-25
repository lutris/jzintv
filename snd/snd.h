/*
 * ============================================================================
 *  Title:    Sound Interface Abstraction
 *  Author:   J. Zbiciak
 *  $Id: snd.h,v 1.10 1999/10/10 08:44:30 im14u2c Exp $
 * ============================================================================
 *
 * ============================================================================
 *  SND_TICK     -- Update state of the sound universe.  Drains audio data
 *                  from the PSGs and prepares it for SDL's sound layer.
 *  SND_FILL     -- Audio callback used by SDL for filling SDL's buffers.
 *  SND_REGISTER -- Registers a PSG with the SND module.
 *  SND_INIT     -- Initialize a SND_T
 * ============================================================================
 */
#ifndef _SND_H_
#define _SND_H_

/*
 * ============================================================================
 *  SND_T        -- Main sound interface object.
 *  SND_BUF_T    -- Sound buffer abstraction.
 * ============================================================================
 */

typedef struct snd_pvt_t *snd_pvt_p;
typedef struct snd_t     *snd_p;

typedef struct snd_buf_t
{
    /* Sound data */
    sint_16     *buf;           /* Audio data buffers.                  */
    
    sint_16     **dirty;        /* Array of dirty buffers.              */
    sint_16     **clean;        /* Array of clean buffers.              */

    int         num_dirty;      /* Number of dirty buffers              */
    int         num_clean;      /* Number of clean buffers.             */
    int         tot_buf;        /* Total number of buffers.             */

    int         drop;           /* Request a frame drop.                */
    int         tot_drop;       /* Total number of buffers dropped.     */

    int         top_dirty_ptr;  /* Pointer into top dirty buffer        */
                                /*  used by snd_fill in case a partial  */
                                /*  buffer request is made (ick).       */

    snd_p       snd;            /* parent pointer */
} snd_buf_t;

typedef struct snd_t
{
    periph_t    periph;         /* Yes, sound is a peripheral.          */
    snd_buf_t   **src;          /* Input sound buffers.                 */
    int         src_cnt;        /* Number of input sources.             */
    snd_buf_t   mixbuf;         /* Mixed audio data to go out.          */
    uint_64     samples;        /* Cumulative # of samples processed.   */
    uint_64     tot_frame;      /* Cumulative # of frames processed.    */
    uint_64     tot_dirty;      /* Cumulative # dirty from snd_fill.    */
    uint_32     rate;           /* Sample rate in Hz.                   */

    v_uint_32   change_vol;     /* Requests to change volume            */
    int         atten;          /* Attenuation. 0=full blast, 16=mute   */

    FILE        *raw_file;      /* Raw audio data dump file.            */
    int         raw_start;      /* FLAG: To suppress silence @ start    */

    int         buf_size;
    int         buf_cnt;

    snd_pvt_p   pvt;            /* Private stuff (API specific)         */
} snd_t;



/*
 * ============================================================================
 *  SND_TICK     -- Update state of the sound universe.  Drains audio data
 *                  from the PSGs and prepares it for SDL's sound layer.
 * ============================================================================
 */
uint_32 snd_tick(periph_p snd, uint_32 len);

/*
 * ============================================================================
 *  SND_FILL     -- Audio callback used by SDL for filling SDL's buffers.
 * ============================================================================
 */
void    snd_fill(void *udata, uint_8 *stream, int len);

/*
 * ============================================================================
 *  SND_REGISTER -- Registers a sound input buffer with the sound object
 * ============================================================================
 */
int     snd_register(periph_p snd, snd_buf_t *src);

/*
 * ============================================================================
 *  SND_INIT     -- Initialize a SND_T
 * ============================================================================
 */
int     snd_init(snd_t *snd, int rate, char *raw_file,
                 int user_snd_buf_size, int user_snd_buf_cnt);


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
/*                 Copyright (c) 1998-1999, Joseph Zbiciak                  */
/* ======================================================================== */

