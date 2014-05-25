/*
 * ============================================================================
 *  Title:    Sound Interface Abstraction
 *  Author:   J. Zbiciak and Tim Lindner
 *  $Id: snd_macos.c,v 1.7 2000/02/03 07:33:03 tlindner Exp $
 * ============================================================================
 *
 * ============================================================================
 *  This file is fairly complete. I pass the samples to the Asgard sound
 *  and it buffers, mixes and pushes them through the MacOS Sound Manager at
 *  interrupt time.
 * ============================================================================
 *  SND_TICK     -- Update state of the sound universe.  Drains audio data
 *                  from the PSGs and prepares it for SDL's sound layer.
 *  SND_FILL     -- No call backs used with the Asgard Sound Lib.
 *  SND_REGISTER -- Registers a PSG with the SND module.
 *  SND_INIT     -- Initialize a SND_T
 * ============================================================================
 */


#include "config.h"
#include "periph.h"
#include "snd.h"
#include "AsgardESS.h"

int snd_buf = SND_BUF_REQ;
static sint_32 *mixbuf = NULL;

/*
 * ============================================================================
 *  SND_TICK     -- Update state of the sound universe.  Drains audio data
 *                  from the PSGs and prepares it for SDL's sound layer.
 * ============================================================================
 */
 

uint_32 snd_tick(periph_p periph, uint_32 len)
{
    snd_t *snd = (snd_t*)periph;
    int i, j, drop, samplesProcessed;
    uint_64 new_now;
    int not_silent = snd->raw_start, min_dirty;

    /* -------------------------------------------------------------------- */
    /*  Trival case:  No sound devices == no work for us.                   */
    /* -------------------------------------------------------------------- */
    if (snd->src_cnt == 0)
    {
        return len;
    }

    samplesProcessed = 0;

    /* -------------------------------------------------------------------- */
    /*  Find the minimum number of dirty buffers across all sources.        */
    /* -------------------------------------------------------------------- */
    min_dirty = snd->src[0]->num_dirty;
    for(i = 1; i < snd->src_cnt; i++)
        if (snd->src[i]->num_dirty < min_dirty) 
            min_dirty = snd->src[i]->num_dirty;

    samplesProcessed = snd_buf * min_dirty;

    /* -------------------------------------------------------------------- */
    /*  Determine how many buffers we need to drop.                         */
    /* -------------------------------------------------------------------- */
    drop = 0;
    for (i = 0; i < snd->src_cnt; i++)
        if (snd->src[i]->drop > drop) drop = snd->src[i]->drop;

    if (drop > min_dirty) drop = min_dirty;

    for (i = 0; i < snd->src_cnt; i++)
        snd->src[i]->drop -= drop;

    /* -------------------------------------------------------------------- */
    /*  Push the remaining buffers into AsgardESS.                          */
    /* -------------------------------------------------------------------- */
    for(i = 0; i < snd->src_cnt; i++ )
        for(j = drop; j < min_dirty; j++ )
            AsgardESS_PlayStreamedSample16(i, snd->src[i]->dirty[j], snd_buf,
                                  snd->rate, 255, 255);

    /* -------------------------------------------------------------------- */
    /*  Mark all of consumed buffers as clean.                              */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < snd->src_cnt; i++)
    {
        for(j = 0; j < min_dirty; j++)
        {
            snd->src[i]->clean[ snd->src[i]->num_clean++ ] = 
                snd->src[i]->dirty[j];
            snd->src[i]->num_dirty--;
        }
    }
        
    /* -------------------------------------------------------------------- */
    /*  Finally, figure out how many system ticks this accounted for.       */
    /* -------------------------------------------------------------------- */
    snd->samples += samplesProcessed;
    new_now = snd->samples * 894886 / snd->rate;

    if (new_now < snd->periph.now)      /* Uhoh... are we slipping away? */
        fprintf(stderr, "snd_t:  Too slow...\n");   
    else
        len = new_now - snd->periph.now;

    return len;
}

/*
 * ============================================================================
 *  SND_FILL     -- Audio callback used by SDL for filling SDL's buffers.
 *                  SDL is the graphic/sound library used in the Linux port.
 * ============================================================================
 */
void    snd_fill(void *udata, uint_8 *stream, int len)
{
#pragma unused ( udata )
#pragma unused ( stream )
#pragma unused ( len )
;}

/*
 * ============================================================================
 *  SND_REGISTER -- Registers a sound input buffer with the sound object
 * ============================================================================
 */
int snd_register
(
    periph_p    per,            /* Sound object.                            */
    snd_buf_t   *src            /* Sound input buffer.                      */
)
{   
    int i;
    snd_t *snd = (snd_t*)per;

    /* -------------------------------------------------------------------- */
    /*  Initialize the sound buffer to all 0's.                             */
    /* -------------------------------------------------------------------- */
    memset(src, 0, sizeof(snd_buf_t));

    /* -------------------------------------------------------------------- */
    /*  Set up its buffers as 'clean'.                                      */
    /* -------------------------------------------------------------------- */
    src->num_clean = BUF_CNT;
    src->num_dirty = 0;

    src->buf   = calloc(sizeof(sint_16) * snd_buf, src->num_clean);
    src->clean = calloc(src->num_clean, sizeof(sint_16 *));
    src->dirty = calloc(src->num_clean, sizeof(sint_16 *));

    if (!src->buf || !src->clean || !src->dirty)
    {
        fprintf(stderr, "snd_register: Out of memory allocating sndbuf.\n");
        return -1;
    }

    for (i = 0; i < src->num_clean; i++)
        src->clean[i] = src->buf + i * snd_buf;


    /* -------------------------------------------------------------------- */
    /*  Add this sound source to our list of sound sources.                 */
    /* -------------------------------------------------------------------- */
    snd->src_cnt++;
    snd->src = realloc(snd->src, snd->src_cnt * sizeof(snd_buf_t*));
    if (!snd->src) 
    {
        fprintf(stderr, "Error:  Out of memory in snd_register()\n");
        return -1;
    }
    snd->src[snd->src_cnt - 1] = src;

    return 0;
}

/*
 * ============================================================================
 *  SND_INIT     -- Initialize a SND_T
 * ============================================================================
 */
int snd_init(snd_t *snd, int rate, char *raw_file)
{
    OSErr   myErr;

    memset(snd, 0, sizeof(snd_t));

    /* -------------------------------------------------------------------- */
    /*  Setup Asgard.                                                       */
    /* -------------------------------------------------------------------- */
   
    myErr = AsgardESS_Init(rate, 2, AESS_GENERATE_16BIT, AESS_GENERATE_MONO,
                           AESS_NO_INTERPOLATION, AESS_UNNORMALIZED_SAMPLES,
                           AESS_MIXER_LINEAR_NOCLIP, AESS_STANDARD_GAIN);
    
    if ( myErr != noErr ) 
    {
            fprintf(stderr, "Couldn't open audio: %s\n", myErr);
            return -1;
    }
    
    printf( "AagardESS emulated sound system © 1997 Aaron Giles\n" );

    
    /* -------------------------------------------------------------------- */
    /*  Set up SND's internal varables.                                     */
    /*  Channels and Interleave are ignored currently.                      */
    /* -------------------------------------------------------------------- */
    snd->rate = rate;

    /* -------------------------------------------------------------------- */
    /*  Set up SND as a peripheral.                                         */
    /* -------------------------------------------------------------------- */
    snd->periph.read      = NULL;
    snd->periph.write     = NULL;
    snd->periph.tick      = snd_tick;
    snd->periph.min_tick  = snd_buf * 894886 / rate;
    snd->periph.max_tick  = snd->periph.min_tick * 3;
    snd->periph.addr_base = ~0U;
    snd->periph.addr_mask = ~0U;

    if ( raw_file)
        printf( "Sorry, raw audio files are not currently supported.\nSound Disabled.\n" );
        
    return 0;
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

