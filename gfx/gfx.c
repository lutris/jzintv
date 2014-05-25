//#define BENCHMARK_GFX
/*
 * ============================================================================
 *  Title:    Graphics Interface Routines
 *  Author:   J. Zbiciak, J. Tanner
 *  $Id: gfx.c,v 1.18 2001/02/03 02:34:21 im14u2c Exp $
 * ============================================================================
 *  GFX_INIT         -- Initializes a gfx_t object.
 *  GFX_TICK         -- Services a gfx_t tick.
 *  GFX_VID_ENABLE   -- Alert gfx that video has been enabled or blanked
 *  GFX_SET_BORD     -- Set the border / offset parameters for the display
 * ============================================================================
 *  GFX_T            -- Graphics subsystem object.
 *  GFX_PVT_T        -- Private internal state to gfx_t structure.
 *  GFX_STIC_PALETTE -- The STIC palette.
 * ============================================================================
 *  The graphics subsystem provides an abstraction layer between the 
 *  emulator and the graphics library being used.  Theoretically, this 
 *  should allow easy porting to other graphics libraries.
 *
 *  TODO:  
 *   -- Make use of dirty rectangle updating for speed.
 * ============================================================================
 */

#include "sdl.h"
#include "config.h"
#include "periph/periph.h"
#include "gfx.h"
#include "gfx_prescale.h"
#include "gfx_scale.h"
#include "file/file.h"
#include "mvi/mvi.h"
#include "gif/gif_enc.h"


/*
 * ============================================================================
 *  GFX_PVT_T        -- Private internal state to gfx_t structure.
 * ============================================================================
 */
typedef struct gfx_pvt_t
{
    SDL_Surface *scr;               /*  Screen surface.                     */
    SDL_Color   pal_on [32];        /*  Palette when video is enabled.      */
    SDL_Color   pal_off[32];        /*  Palette when video is blanked.      */
    int         vid_enable;         /*  Video enable flag.                  */
    int         ofs_x, ofs_y;       /*  X/Y offsets for centering img.      */
    int         bpp;                /*  Actual color depth.                 */
    int         flags;              /*  Flags for current display surf.     */

    int         movie_init;         /*  Is movie structure initialized?     */
    mvi_t       *movie;             /*  Pointer to mvi_t to reduce deps     */

    uint_8  *RESTRICT inter_vid;    /*  Intermediate video after prescaler  */
    uint_8  *RESTRICT prev;         /*  previous frame for dirty-rect       */

    gfx_prescaler_t     prescaler;  /*  Scale 160x200 to an intermediate    */
    void                *ps_opaque; /*  Prescaler opaque structure          */
    gfx_scale_spec_t    scaler; 

    gfx_dirtyrect_spec  dr_spec;    /*  Dirty-rectangle control spec.       */


    uint_32     *dirty_rows;        /*  dirty-row bitmap for scaler         */
    int         dirty_rows_sz;
    
    int         num_rects;
    SDL_Rect    *dirty_rects;
    

} gfx_pvt_t;

LOCAL void gfx_dtor(periph_p p);
LOCAL void gfx_tick_generic(gfx_t *gfx);
LOCAL void gfx_find_dirty_rects(gfx_t *gfx);

/*
 * ============================================================================
 *  GFX_STIC_PALETTE -- The STIC palette.
 * ============================================================================
 */
LOCAL uint_8 gfx_stic_palette[32][3] = 
{
    /* -------------------------------------------------------------------- */
    /*  I generated these colors by directly eyeballing my television       */
    /*  while it was next to my computer monitor.  I then tweaked each      */
    /*  color until it was pretty close to my TV.  Bear in mind that        */
    /*  NTSC (said to mean "Never The Same Color") is highly susceptible    */
    /*  to Tint/Brightness/Contrast settings, so your mileage may vary      */
    /*  with this particular palette setting.                               */
    /* -------------------------------------------------------------------- */
    { 0x00, 0x00, 0x00 },
    { 0x00, 0x2D, 0xFF },
    { 0xFF, 0x3D, 0x10 },
    { 0xC9, 0xCF, 0xAB },
    { 0x38, 0x6B, 0x3F },
    { 0x00, 0xA7, 0x56 },
    { 0xFA, 0xEA, 0x50 },
    { 0xFF, 0xFC, 0xFF },
    { 0xBD, 0xAC, 0xC8 },
    { 0x24, 0xB8, 0xFF },
    { 0xFF, 0xB4, 0x1F },
    { 0x54, 0x6E, 0x00 },
    { 0xFF, 0x4E, 0x57 },
    { 0xA4, 0x96, 0xFF },
    { 0x75, 0xCC, 0x80 },
    { 0xB5, 0x1A, 0x58 },

    /* -------------------------------------------------------------------- */
    /*  This pink color is used for drawing rectangles around sprites.      */
    /*  It's a temporary hack.                                              */
    /* -------------------------------------------------------------------- */
    { 0xFF, 0x80, 0x80 },
    /* -------------------------------------------------------------------- */
    /*  Grey shades used for misc tasks (not currently used).               */
    /* -------------------------------------------------------------------- */
    { 0x11, 0x11, 0x11 },
    { 0x22, 0x22, 0x22 },
    { 0x33, 0x33, 0x33 },
    { 0x44, 0x44, 0x44 },
    { 0x55, 0x55, 0x55 },
    { 0x66, 0x66, 0x66 },
    { 0x77, 0x77, 0x77 },
    { 0x88, 0x88, 0x88 },
    { 0x99, 0x99, 0x99 },
    { 0xAA, 0xAA, 0xAA },
    { 0xBB, 0xBB, 0xBB },
    { 0xCC, 0xCC, 0xCC },
    { 0xDD, 0xDD, 0xDD },
    { 0xEE, 0xEE, 0xEE },
    { 0xFF, 0xFF, 0xFF },
};


/*  01234567890123
**  ###  ####  ### 
**  #  # #    #
**  ###  ###  #
**  #  # #    #
**  #  # ####  ### 
*/

LOCAL const char* gfx_rec_bmp[5] =
{
   "###  ####  ###",
   "#  # #    #   ",
   "###  ###  #   ",
   "#  # #    #   ",
   "#  # ####  ###"
};




/* ======================================================================== */
/*  GFX_SDL_ABORT    -- Abort due to SDL errors.                            */
/* ======================================================================== */
LOCAL void gfx_sdl_abort(void)
{
    fprintf(stderr, "gfx/SDL Error:%s\n", SDL_GetError());
    exit(1);
}

/* ======================================================================== */
/*  GFX_SET_SCALER_PALETTE                                                  */
/* ======================================================================== */
LOCAL void gfx_set_scaler_palette
(
    SDL_Surface         *scr, 
    gfx_scale_spec_t    *scaler,
    SDL_Color           pal[32]
)
{
    int i;
    uint_32 t;

    for (i = 0; i < 32; i++)
    {
        t = SDL_MapRGB(scr->format, pal[i].r, pal[i].g, pal[i].b);
        gfx_scale_set_palette(scaler, i, t);
    }
}

/* ======================================================================== */
/*  GFX_SETUP_SDL_SURFACE:  Do all the dirty SDL dirty work for setting up  */
/*                          the display.  This gets called during init, or  */
/*                          when toggling between full-screen and windowed  */
/* ======================================================================== */
LOCAL int gfx_setup_sdl_surface
(
    gfx_t *gfx, int flags, int quiet
)
{
    int i;
    int actual_x, actual_y; 
    int desire_x   = gfx->pvt->scaler.actual_x; 
    int desire_y   = gfx->pvt->scaler.actual_y;
    int desire_bpp = gfx->pvt->scaler.bpp;
    uint_32 sdl_flags = 0;
    SDL_Surface *scr;


    /* -------------------------------------------------------------------- */
    /*  Set up the SDL video flags from our flags.                          */
    /* -------------------------------------------------------------------- */
    if (desire_bpp != 8)
        flags &= ~GFX_HWPAL;     /* ignore if not 8 bpp */

    if ((flags & GFX_DRECTS) != 0)
        flags &= ~GFX_DBLBUF;

    sdl_flags  = flags & GFX_SWSURF ? SDL_SWSURFACE  : SDL_HWSURFACE;
    sdl_flags |= flags & GFX_DBLBUF ? SDL_DOUBLEBUF  : 0;
    sdl_flags |= flags & GFX_ASYNCB ? SDL_ASYNCBLIT  : 0;
    sdl_flags |= flags & GFX_HWPAL  ? SDL_HWPALETTE  : 0;
    sdl_flags |= flags & GFX_FULLSC ? SDL_FULLSCREEN : 0;

    /* -------------------------------------------------------------------- */
    /*  Try to allocate a screen surface at the desired size, etc.          */
    /* -------------------------------------------------------------------- */
    /*  NOTE:  This eventually should do better things about finding        */
    /*  resolutions / color depths that we like, etc.  For now just be      */
    /*  braindead, even if it means SDL will run our video in "emulation."  */
    /* -------------------------------------------------------------------- */
    if (!quiet)
    {
        jzp_printf("gfx:  Searching for video modes near %dx%dx%d with:\n"
           "gfx:      %s surf, %s buf, %s blit, %s pal, %s\n",
           desire_x, desire_y, desire_bpp,
           flags & GFX_SWSURF ? "Software" : "Hardware",
           flags & GFX_DBLBUF ? "Double"   : "Single",
           flags & GFX_ASYNCB ? "Async"    : "Sync",
           flags & GFX_HWPAL  ? "Hardware" : desire_bpp!=8 ? "No" : "Software",
           flags & GFX_FULLSC ? "Full screen" : "Windowed");

        jzp_flush();
    }

    /* -------------------------------------------------------------------- */
    /*  JJT: First, the program must check that the video hardware          */
    /*  actually supports the requested resolution.  For instance, some     */
    /*  Macs cannot handle 320x200 fullscreen.                              */
    /*                                                                      */
    /*  While SDL can try to emulate a low resolution, this feature is      */
    /*  currently broken on SDL for Mac OS X.  This program must handle     */
    /*  such emulation itself.                                              */
    /*                                                                      */
    /*  For now, the program assumes if it can get a mode with the proper   */
    /*  resolution, that mode will suport 8 bits-per-pixel.                 */
    /*  Play this on a EGA machine at your own risk. ;-)                    */
    /* -------------------------------------------------------------------- */
#ifdef GP2X
    actual_x = 320;
    actual_y = 240;
#else
    {
        SDL_Rect **available_modes;
        
        available_modes = SDL_ListModes(NULL, sdl_flags);

        /* No available mode! */
        if (available_modes == NULL)
            gfx_sdl_abort();
        else
        /* All modes are available for a windowed display. */
        if (available_modes == (SDL_Rect **)-1)
        {
            actual_x = desire_x;
            actual_y = desire_y;
        }
        else
        /* ListModes returns a list sorted largest to smallest. */
        /* Find the smallest mode >= the size requested.        */
        {
            int best = -1, area_diff, best_area_diff = INT_MAX;

            i = 0;
            while (available_modes[i])
            {
                if (!quiet)
                    jzp_printf("gfx:  Considering %dx%d... ", 
                               available_modes[i]->w, available_modes[i]->h);
                if (available_modes[i]->w >= desire_x &&
                    available_modes[i]->h >= desire_y)
                {
                    area_diff = available_modes[i]->w * available_modes[i]->h -
                                desire_x * desire_y;

                    if (best_area_diff > area_diff)
                    {
                        best_area_diff = area_diff;
                        best = i;
                        if (!quiet)
                            jzp_printf("New best fit.  Diff = %d\n", area_diff);
                        if (best_area_diff == 0)
                            break;
                    } else
                        if (!quiet)
                            jzp_printf("Poorer fit.    Diff = %d\n", area_diff);
                } else
                {
                    if (!quiet)
                        jzp_printf("Too small.\n");
                }
                i++;
            }
           
            /* No suitable mode available. */
            if (best == -1)
                gfx_sdl_abort();

            actual_x = available_modes[best]->w;
            actual_y = available_modes[best]->h;
        }
    }
#endif

    scr = SDL_SetVideoMode(actual_x, actual_y, desire_bpp, sdl_flags);

    if (scr) 
        gfx->pvt->scr = scr;
    else
        return -1;

    gfx->pvt->ofs_x = ((actual_x - desire_x) >> 1) & (~3);
    gfx->pvt->ofs_y =  (actual_y - desire_y) >> 1;
    gfx->pvt->bpp   = gfx->pvt->scr->format->BitsPerPixel;
    gfx->pvt->flags = flags;
    sdl_flags       = gfx->pvt->scr->flags;

    if (!quiet)
    {
        jzp_printf("gfx:  Selected:  %dx%dx%d with:\n"
           "gfx:      %s surf, %s buf, %s blit, %s pal, %s\n",
           actual_x, actual_y, gfx->pvt->bpp,
           sdl_flags & SDL_HWSURFACE  ? "Hardware"      : "Software",
           sdl_flags & SDL_DOUBLEBUF  ? "Double"        : "Single",
           sdl_flags & SDL_ASYNCBLIT  ? "Async"         : "Sync",
           sdl_flags & SDL_HWPALETTE  ? "Hardware"      : "Software/No",
           sdl_flags & SDL_FULLSCREEN ? "Full screen"   : "Windowed");
    }

    /* -------------------------------------------------------------------- */
    /*  TEMPORARY: Verify that the surface's format is as we expect.  This  */
    /*  is just a temporary bit of paranoia to ensure that scr->pixels      */
    /*  is in the format I _think_ it's in.                                 */
    /* -------------------------------------------------------------------- */
    if ((desire_bpp == 8 && (gfx->pvt->scr->format->BitsPerPixel  !=  8   ||
                             gfx->pvt->scr->format->BytesPerPixel !=  1))   ||
        (desire_bpp ==16 && (gfx->pvt->scr->format->BitsPerPixel  != 16   ||
                             gfx->pvt->scr->format->BytesPerPixel !=  2))   ||
        (desire_bpp ==32 && (gfx->pvt->scr->format->BitsPerPixel  != 32   ||
                             gfx->pvt->scr->format->BytesPerPixel !=  4)))
    {
        fprintf(stderr,"gfx panic: BitsPerPixel = %d, BytesPerPixel = %d\n",
                gfx->pvt->scr->format->BitsPerPixel,
                gfx->pvt->scr->format->BytesPerPixel);
        return -1;
    }

    /* -------------------------------------------------------------------- */
    /*  New surface will may need palette initialization.                   */
    /* -------------------------------------------------------------------- */
    if (gfx->pvt->bpp == 8)
    {
        SDL_SetColors(gfx->pvt->scr, 
                      gfx->pvt->vid_enable ? gfx->pvt->pal_on 
                                           : gfx->pvt->pal_off, 0, 32);
    } else
    {
        gfx_set_scaler_palette( gfx->pvt->scr,
                               &gfx->pvt->scaler,
                                gfx->pvt->vid_enable ? gfx->pvt->pal_on
                                                     : gfx->pvt->pal_off);
    }

    return 0;
}

#ifdef BENCHMARK_GFX
LOCAL int dr_hist[244];   /* histogram of number of dirty rects   */
LOCAL int drw_hist[21];   /* histogram of dirty rectangle widths  */

LOCAL void gfx_dr_hist_dump(void);
#endif

/* ======================================================================== */
/*  GFX_INIT         -- Initializes a gfx_t object.                         */
/* ======================================================================== */
int gfx_init(gfx_t *gfx, int desire_x, int desire_y, 
             int desire_bpp, int flags, int verbose)
{
    uint_32 inter_x = 160, inter_y = 200;
    int     i, need_inter_vid = 0; 
    void    *prescaler_opaque;
    gfx_dirtyrect_spec dr_spec;

    int     prescaler = 0;  /* temporary */

    /* -------------------------------------------------------------------- */
    /*  Set up prescaler (ie. Scale2X/3X/4X or similar)                     */
    /* -------------------------------------------------------------------- */
    if (prescaler > gfx_prescaler_registry_size || prescaler < 0)
    {
        fprintf(stderr, "gfx:  Prescaler number %d out of range\n", prescaler);
        goto die;
    } else if (prescaler > 0)
    {
        jzp_printf("gfx:  Configuring prescaler %s\n", 
                    gfx_prescaler_registry[prescaler].name);
    }

    prescaler_opaque = gfx_prescaler_registry[prescaler].prescaler_init
                       ( 
                            160,      200,
                            &inter_x, &inter_y,
                            &dr_spec
                       );

    /* -------------------------------------------------------------------- */
    /*  Sanity checks and cleanups.                                         */
    /* -------------------------------------------------------------------- */
    assert(gfx);
    memset((void*)gfx, 0, sizeof(gfx_t));

    /* -------------------------------------------------------------------- */
    /*  If inter_x/inter_y differ from 160x200, we need an intermediate     */
    /*  video buffer before the scaler.                                     */
    /* -------------------------------------------------------------------- */
    need_inter_vid = (inter_x != 160 || inter_y != 200);

    /* -------------------------------------------------------------------- */
    /*  Allocate memory for the gfx_t.                                      */
    /* -------------------------------------------------------------------- */
    gfx->vid = CALLOC(uint_8,    160 * 200);
    gfx->pvt = CALLOC(gfx_pvt_t, 1);

    if (gfx->pvt) 
    {
        int dr_count, dr_x_dim, dr_y_dim;

        dr_x_dim = (dr_spec.active_last_x - dr_spec.active_first_x + 1);
        dr_y_dim = (dr_spec.active_last_y - dr_spec.active_first_y + 1);

        dr_count = (dr_x_dim / dr_spec.x_step) * 
                   (dr_y_dim / dr_spec.y_step);

        if (need_inter_vid)
            gfx->pvt->inter_vid = CALLOC(uint_8, inter_x * inter_y);
        else
            gfx->pvt->inter_vid = gfx->vid;

        gfx->pvt->prescaler     = gfx_prescaler_registry[prescaler].prescaler;
        gfx->pvt->ps_opaque     = prescaler_opaque;

        gfx->pvt->prev          = CALLOC(uint_8,   inter_x * inter_y);
        gfx->pvt->dirty_rects   = CALLOC(SDL_Rect, dr_count);

        gfx->pvt->dirty_rows    = CALLOC(uint_32,  ((inter_y+31) >> 5));
        gfx->pvt->dirty_rows_sz = 4 * ((inter_y+31) >> 5);

        gfx->pvt->dr_spec       = dr_spec;
    }

    if (!gfx->vid || !gfx->pvt || !gfx->pvt->prev || !gfx->pvt->dirty_rows ||
        !gfx->pvt->dirty_rects || !gfx->pvt->inter_vid)
    {

        fprintf(stderr, "gfx:  Panic:  Could not allocate memory.\n");

        goto die;
    }

    /* -------------------------------------------------------------------- */
    /*  Select the appropriate tick function based on our display res.      */
    /*  For now, only support 320x200x8bpp or 640x480x8bpp.                 */
    /* -------------------------------------------------------------------- */
    if (gfx->tick_core == NULL)
    {
        if (desire_bpp == 24)
            desire_bpp = 32;

        gfx->tick_core = gfx_tick_generic;
        if (gfx_scale_init_spec(&(gfx->pvt->scaler), 
                                 inter_x,  inter_y, 
                                 desire_x, desire_y, desire_bpp))
        {
            fprintf(stderr, 
                    "Could not configure scaler for %d x %d @ %d bpp\n",
                    desire_x, desire_y, desire_bpp);
            goto die;
        }
    }

    /* -------------------------------------------------------------------- */
    /*  Set up our color palette.  We start with video blanked.             */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 16; i++)
    {
        gfx->pvt->pal_on [i].r = gfx_stic_palette[i][0];
        gfx->pvt->pal_on [i].g = gfx_stic_palette[i][1];
        gfx->pvt->pal_on [i].b = gfx_stic_palette[i][2];
        gfx->pvt->pal_off[i].r = gfx_stic_palette[i][0] >> 1;
        gfx->pvt->pal_off[i].g = gfx_stic_palette[i][1] >> 1;
        gfx->pvt->pal_off[i].b = gfx_stic_palette[i][2] >> 1;
    }
    for (i = 16; i < 32; i++)
    {
        gfx->pvt->pal_on [i].r = gfx_stic_palette[i][0];
        gfx->pvt->pal_on [i].g = gfx_stic_palette[i][1];
        gfx->pvt->pal_on [i].b = gfx_stic_palette[i][2];
        gfx->pvt->pal_off[i].r = gfx_stic_palette[i][0];
        gfx->pvt->pal_off[i].g = gfx_stic_palette[i][1];
        gfx->pvt->pal_off[i].b = gfx_stic_palette[i][2];
    }
    gfx->pvt->vid_enable = 0;

    /* -------------------------------------------------------------------- */
    /*  Set up initial graphics mode.                                       */
    /* -------------------------------------------------------------------- */
    if (gfx_setup_sdl_surface(gfx, flags, !verbose) < 0)
        gfx_sdl_abort();

    /* -------------------------------------------------------------------- */
    /*  Ok, see if we succeeded in setting our initial video mode, and do   */
    /*  some minor tidying.                                                 */
    /* -------------------------------------------------------------------- */
    if (!gfx->pvt->scr || SDL_Flip(gfx->pvt->scr) == -1)
        gfx_sdl_abort();

    /* -------------------------------------------------------------------- */
    /*  Hide the mouse.                                                     */
    /* -------------------------------------------------------------------- */
    SDL_ShowCursor(0);

    /* -------------------------------------------------------------------- */
    /*  Set up the gfx_t's internal structures.                             */
    /* -------------------------------------------------------------------- */
    gfx->periph.read        = NULL;
    gfx->periph.write       = NULL;
    gfx->periph.peek        = NULL;
    gfx->periph.poke        = NULL;
    gfx->periph.tick        = gfx_tick_common;
    gfx->periph.min_tick    = 14934;
    gfx->periph.max_tick    = 14934;
    gfx->periph.addr_base   = 0;
    gfx->periph.addr_mask   = 0;
    gfx->periph.dtor        = gfx_dtor;

#ifdef BENCHMARK_GFX
    atexit(gfx_dr_hist_dump);
#endif

    return 0;

die:
    CONDFREE(gfx->pvt->dirty_rows);
    CONDFREE(gfx->pvt->dirty_rects);
    CONDFREE(gfx->pvt->prev);
    if (gfx->pvt->inter_vid != gfx->vid) CONDFREE(gfx->pvt->inter_vid);
    CONDFREE(gfx->pvt);
    CONDFREE(gfx->vid);
    return -1;
}

/* ======================================================================== */
/*  GFX_DTOR     -- Tear down the gfx_t                                     */
/* ======================================================================== */
LOCAL void gfx_dtor(periph_p p)
{
    gfx_t *gfx = (gfx_t *)p;

    if (gfx->pvt &&
        gfx->pvt->movie)
    {
        if (gfx->pvt->movie->f)
            fclose(gfx->pvt->movie->f);

        CONDFREE(gfx->pvt->movie);
    }

    CONDFREE(gfx->pvt->dirty_rows);
    CONDFREE(gfx->pvt->dirty_rects);
    CONDFREE(gfx->pvt->prev);
    if (gfx->pvt->inter_vid != gfx->vid) CONDFREE(gfx->pvt->inter_vid);
    CONDFREE(gfx->pvt);
    CONDFREE(gfx->vid);
}

/* ======================================================================== */
/*  GFX_TOGGLE_WINDOWED -- Try to toggle windowed vs. full-screen.          */
/* ======================================================================== */
void gfx_toggle_windowed(gfx_t *gfx, int quiet)
{
    int tmp;

    if (!quiet) 
        jzp_printf("\n");

    gfx->toggle = 0;
    if (gfx_setup_sdl_surface(gfx, gfx->pvt->flags ^ GFX_FULLSC, quiet) < 0)
        gfx_setup_sdl_surface(gfx, gfx->pvt->flags, quiet);

    gfx->b_dirty |= 2;
    gfx->dirty   |= 2;

    tmp = gfx->scrshot;
    gfx->scrshot = 0;
    gfx->periph.tick(&gfx->periph, 0);
    gfx->scrshot = tmp;
    plat_delay(2000);  /* Let monitor come up to speed w/ new res. */
}

/* ======================================================================== */
/*  GFX_FORCE_WINDOWED -- Force display to be windowed mode; Returns 1 if   */
/*                        display was previously full-screen.               */
/* ======================================================================== */
int gfx_force_windowed(gfx_t *gfx, int quiet)
{
    if (gfx->pvt->flags & GFX_FULLSC)
    {
        gfx_toggle_windowed(gfx, quiet);
        return 1;
    }

    return 0;
}

/* ======================================================================== */
/*  GFX_SET_TITLE    -- Sets the window title                               */
/* ======================================================================== */
int gfx_set_title(gfx_t *gfx, const char *title)
{
    UNUSED(gfx);
    SDL_WM_SetCaption(title, title);
    return 0;
}
#ifdef BENCHMARK_GFX
LOCAL double bm_max = 0, bm_min = 1e30, bm_tot = 0;
LOCAL int bm_cnt = 0;
#endif
/* ======================================================================== */
/*  GFX_TICK_COMMON  -- Services a gfx_t tick                               */
/* ======================================================================== */
uint_32 gfx_tick_common(periph_p gfx_periph, uint_32 len)
{
    gfx_t   *gfx = (gfx_t*) gfx_periph;
#ifdef BENCHMARK_GFX
    double start, end, diff;

    start = get_time();
#endif

    gfx->tot_frames++;

    /* -------------------------------------------------------------------- */
    /*  Update a movie if one's active, or user requested toggle in movie   */
    /*  state.  We do this prior to dropping frames so that movies always   */
    /*  have a consistent frame rate.                                       */
    /* -------------------------------------------------------------------- */
    if (gfx->scrshot & (GFX_MOVIE | GFX_MVTOG))
        gfx_movieupd(gfx);

    /* -------------------------------------------------------------------- */
    /*  Toggle full-screen/windowed if req'd.                               */
    /* -------------------------------------------------------------------- */
    if (gfx->toggle)
        gfx_toggle_windowed(gfx, 0);


    /* -------------------------------------------------------------------- */
    /*  Drop a frame if we need to.                                         */
    /* -------------------------------------------------------------------- */
    if (gfx->drop_frame)
    {
        gfx->drop_frame--;
        if (gfx->dirty) gfx->dropped_frames++;
        return len;
    }

    /* -------------------------------------------------------------------- */
    /*  Don't bother if display isn't dirty or if we're iconified.          */
    /* -------------------------------------------------------------------- */
    if (!gfx->scrshot && (!gfx->dirty || gfx->hidden))
    {
        return len;
    }

    /* -------------------------------------------------------------------- */
    /*  DEBUG: Report blocks of dropped frames.                             */
    /* -------------------------------------------------------------------- */
    if (gfx->dropped_frames)
    {
#if 0
        jzp_printf("Dropped %d frames.\n", gfx->dropped_frames);
        jzp_flush();
#endif
        gfx->tot_dropped_frames += gfx->dropped_frames;
        gfx->dropped_frames = 0;
    }

    /* -------------------------------------------------------------------- */
    /*  Update the palette if there's been a change in blanking state.      */
    /* -------------------------------------------------------------------- */
    if (gfx->pvt->vid_enable & 2)
    {
        gfx->pvt->vid_enable &= 1;
        gfx->pvt->vid_enable ^= 1;

        if (gfx->pvt->scaler.bpp == 8)
        {
            SDL_SetColors(gfx->pvt->scr,
                          gfx->pvt->vid_enable ? gfx->pvt->pal_on 
                                               : gfx->pvt->pal_off, 0, 16);
        } else 
        {
            gfx_set_scaler_palette( gfx->pvt->scr,
                                   &gfx->pvt->scaler,
                                    gfx->pvt->vid_enable ? gfx->pvt->pal_on
                                                         : gfx->pvt->pal_off);
 
        }
        gfx->dirty |= 2;
    }

    /* -------------------------------------------------------------------- */
    /*  If dirty-rectangle disabled, force a dirty frame to a full flip.    */
    /* -------------------------------------------------------------------- */
    if ((gfx->pvt->flags & GFX_DRECTS) == 0 && 
        (gfx->dirty || gfx->b_dirty))
    {
        gfx->dirty |= 3;
    }


    /* -------------------------------------------------------------------- */
    /*  Run the prescaler if any part of the frame is dirty.                */
    /* -------------------------------------------------------------------- */
    if (gfx->dirty)
        gfx->pvt->prescaler(gfx->vid, gfx->pvt->inter_vid, gfx->pvt->ps_opaque);

    /* -------------------------------------------------------------------- */
    /*  Push whole frame if dirty == 2, else do dirty-rectangle update.     */
    /* -------------------------------------------------------------------- */
    if (gfx->dirty >= 2)
    {
        memset(gfx->pvt->dirty_rows, 0xFF, gfx->pvt->dirty_rows_sz);
        gfx->tick_core(gfx);
        SDL_Flip(gfx->pvt->scr);
    } else
    {
        /* ---------------------------------------------------------------- */
        /*  Compute dirty rectangles based on the intermediate bitmap       */
        /* ---------------------------------------------------------------- */
        gfx_find_dirty_rects(gfx);

        if (gfx->pvt->num_rects > 0)
        {
            /* ------------------------------------------------------------ */
            /*  Expand the source bitmap to final display resolution.       */
            /* ------------------------------------------------------------ */
            gfx->tick_core(gfx);

            /* ------------------------------------------------------------ */
            /*  Actually update the display.                                */
            /* ------------------------------------------------------------ */
            SDL_UpdateRects(gfx->pvt->scr, 
                            gfx->pvt->num_rects, 
                            gfx->pvt->dirty_rects);
        }
    }

    gfx->dirty = 0;

    /* -------------------------------------------------------------------- */
    /*  If a screen-shot was requested, go write out a PPM file of the      */
    /*  screen right now.  Screen-shot PPMs are always 320x200.             */
    /* -------------------------------------------------------------------- */
    if (gfx->scrshot & GFX_SHOT)
    {
        gfx_scrshot(gfx->vid);
        gfx->scrshot &= ~GFX_SHOT;
    }

#ifdef BENCHMARK_GFX
    end = get_time();
    diff = end - start;
    if (diff > bm_max) bm_max = diff;
    if (diff < bm_min) bm_min = diff;
    bm_tot += diff;

    if (++bm_cnt == 120)
    {
        jzp_printf("gfx_tick: min = %8.3f max = %8.3f avg = %8.3f\n",
                   bm_min * 1000., bm_max * 1000., bm_tot * 1000. / 120);
        bm_max = bm_tot = 0;
        bm_cnt = 0;
        bm_min = 1e30;
    }    
#endif

    return len;
}

/* ======================================================================== */
/*  GFX_TICK_GENERIC -- Services a gfx_t tick in any graphics format        */
/* ======================================================================== */
LOCAL void gfx_tick_generic(gfx_t *gfx)
{
    uint_8 *scr;

    if (SDL_MUSTLOCK(gfx->pvt->scr))
        SDL_LockSurface(gfx->pvt->scr);

    scr = gfx->pvt->ofs_x * gfx->pvt->scr->format->BytesPerPixel + 
          gfx->pvt->ofs_y * gfx->pvt->scr->pitch +
          (uint_8 *) gfx->pvt->scr->pixels;

    gfx_scale
    (
        &gfx->pvt->scaler,
        gfx->pvt->inter_vid,
        scr,
        gfx->pvt->scr->pitch,
        gfx->pvt->dirty_rows
    );

    if (SDL_MUSTLOCK(gfx->pvt->scr))
        SDL_UnlockSurface(gfx->pvt->scr);
}

/* ======================================================================== */
/*  GFX_VID_ENABLE   -- Alert gfx that video has been enabled or blanked    */
/* ======================================================================== */
void gfx_vid_enable(gfx_t *gfx, int enabled)
{
    /* -------------------------------------------------------------------- */
    /*  Force 'enabled' to be 0 or 1.                                       */
    /* -------------------------------------------------------------------- */
    enabled = enabled != 0;

    /* -------------------------------------------------------------------- */
    /*  If enabled state changed, schedule a palette update.                */
    /* -------------------------------------------------------------------- */
    if ((gfx->pvt->vid_enable ^ enabled) & 1)
    {
        gfx->pvt->vid_enable |= 2;
        gfx->dirty |= 2;
    } else
    {
        gfx->pvt->vid_enable = enabled;
    }
}

/* ======================================================================== */
/*  GFX_SET_BORD     -- Set the border color for the display                */
/* ======================================================================== */
void gfx_set_bord
(
    gfx_t *gfx,         /*  Graphics object.                        */
    int b_color
)
{
    int dirty = 0;

    /* -------------------------------------------------------------------- */
    /*  Set up the display parameters.                                      */
    /* -------------------------------------------------------------------- */
    if (gfx->b_color != b_color) { gfx->b_color = b_color; dirty = 3; }

    /* -------------------------------------------------------------------- */
    /*  If we're using the normal STIC blanking behavior, set our "off"     */
    /*  colors to the currently selected border color.  The alternate mode  */
    /*  (which is useful for debugging) sets the blanked colors to be       */
    /*  dimmed versions of the normal palette.                              */
    /* -------------------------------------------------------------------- */
    if (gfx->debug_blank == 0)
    {
        int i;

        for (i = 0; i < 16; i++)
            gfx->pvt->pal_off[i] = gfx->pvt->pal_on[b_color];
    }

    if (dirty)     { gfx->dirty   |= 1; }
    if (dirty & 2) { gfx->b_dirty |= 2; }
}

/* ======================================================================== */
/*  GFX_SCRSHOT      -- Write a 320x200 screen shot to a GIF file.          */
/* ======================================================================== */
LOCAL uint_8 scrshot_buf[320*200];
void gfx_scrshot(uint_8 *scr)
{
    static int last = -1;
    FILE * f;
    char f_name[32];
    int num = last, i, len;


    /* -------------------------------------------------------------------- */
    /*  Search for an unused screen-shot file name.                         */
    /* -------------------------------------------------------------------- */
    do 
    {
        num = (num + 1) % 10000;

        snprintf(f_name, sizeof(f_name), "shot%.4d.gif", num);

        if (!file_exists(f_name)) 
            break;

    } while (num != last);

    /* -------------------------------------------------------------------- */
    /*  Warn the user if we wrapped all 10000 screen shots...               */
    /* -------------------------------------------------------------------- */
    if (num == last)
    {
        num = (num + 1) % 10000;
        snprintf(f_name, sizeof(f_name), "shot%.4d.gif", num);
        fprintf(stderr, "Warning:  Overwriting %s...\n", f_name);
    }

    /* -------------------------------------------------------------------- */
    /*  Update our 'last' pointer and open the file and dump the PPM.       */
    /* -------------------------------------------------------------------- */
    last = num;
    f    = fopen(f_name, "wb");

    if (!f)
    {
        fprintf(stderr, "Error:  Could not open '%s' for screen dump.\n",
                f_name);
        return; 
    }


    /* -------------------------------------------------------------------- */
    /*  Do the screen dump.  Write it as a nice GIF.  We need to pixel      */
    /*  double the image ahead of time.                                     */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < 200*160; i++)
        scrshot_buf[i*2 + 0] = scrshot_buf[i*2 + 1] = scr[i];

    len = gif_write(f, scrshot_buf, 320, 200, gfx_stic_palette, 16);
    if (len > 0)
    {
        jzp_printf("\nWrote screen shot to '%s', %d bytes\n", f_name, len);
    } else
    {
        jzp_printf("\nError writing screen shot to '%s'\n", f_name);
    }
    jzp_flush();
    fclose(f);

    return;
}

/* ======================================================================== */
/*  GFX_MOVIEUPD     -- Start/Stop/Update a movie in progress               */
/* ======================================================================== */
void gfx_movieupd(gfx_t *gfx)
{
    gfx_pvt_t *pvt = gfx->pvt;


    /* -------------------------------------------------------------------- */
    /*  Toggle current movie state if user requested.                       */
    /* -------------------------------------------------------------------- */
    if (gfx->scrshot & GFX_MVTOG)
    {
        static int last = -1;
        int num = last;
        char f_name[32];

        /* ---------------------------------------------------------------- */
        /*  Whatever happens, clear the toggle.                             */
        /* ---------------------------------------------------------------- */
        gfx->scrshot &= ~GFX_MVTOG;

        /* ---------------------------------------------------------------- */
        /*  Make sure movie subsystem initialized.  We only init this if    */
        /*  someone tries to take a movie.                                  */
        /* ---------------------------------------------------------------- */
        if (!pvt->movie_init)
        {
            if (!pvt->movie) pvt->movie = CALLOC(mvi_t, 1);
            if (!pvt->movie)
            {
                fprintf(stderr, "No memory for movie structure\n");
                return;
            }

            mvi_init(pvt->movie, 160, 200);
            pvt->movie_init = 1;
        }

        /* ---------------------------------------------------------------- */
        /*  If a movie's open, close it.                                    */
        /* ---------------------------------------------------------------- */
        if ((gfx->scrshot & GFX_MOVIE) != 0) 
        {
            if (pvt->movie->f)
            {
                fclose(pvt->movie->f);
                jzp_printf("\nDone writing movie:\n"
                       "    Total frames:        %10d\n"
                       "    Total size:          %10d\n"
                       "    Bytes/frame:         %10d\n"
#ifndef NO_LZO
                       "    Bytes saved LZO:     %10d\n"
#endif
                       "    Dupe frames:         %10d\n"
                       "    Dupe rows:           %10d\n"
                       "    Compression ratio:   %8.2f:1\n", 
                       pvt->movie->fr,
                       pvt->movie->tot_bytes,
                       pvt->movie->tot_bytes / pvt->movie->fr,
#ifndef NO_LZO
                       pvt->movie->tot_lzosave,
#endif
                       pvt->movie->rpt_frames,
                       pvt->movie->rpt_rows,
                       (16032.*pvt->movie->fr) / pvt->movie->tot_bytes);
                jzp_flush();
            }

            gfx->scrshot &= ~GFX_MOVIE;
            pvt->movie->f  = NULL;
            pvt->movie->fr = 0;

            return;
        }

        /* ---------------------------------------------------------------- */
        /*  Otherwise, open a new movie.                                    */
        /*  Search for an unused movie file name.                           */
        /* ---------------------------------------------------------------- */
        do 
        {
            num = (num + 1) % 10000;

            snprintf(f_name, sizeof(f_name), "mvi_%.4d.imv", num);

            if (!file_exists(f_name)) 
                break;

        } while (num != last);

        /* ---------------------------------------------------------------- */
        /*  Warn the user if we wrapped all 10000 movie slots...            */
        /* ---------------------------------------------------------------- */
        if (num == last)
        {
            num = (num + 1) % 10000;
            snprintf(f_name, sizeof(f_name), "mvi_%.4d.imv", num);
            fprintf(stderr, "Warning:  Overwriting %s...\n", f_name);
        }

        /* ---------------------------------------------------------------- */
        /*  Update our 'last' pointer, and start the movie.                 */
        /* ---------------------------------------------------------------- */
        last = num;
        pvt->movie->f = fopen(f_name, "wb");

        if (!pvt->movie->f)
        {
            fprintf(stderr, "Error:  Could not open '%s' for movie.\n",
                    f_name);
            return; 
        }

        jzp_printf("\nStarted movie file '%s'\n", f_name); jzp_flush();

        /* ---------------------------------------------------------------- */
        /*  Success:  Turn on the movie.                                    */
        /* ---------------------------------------------------------------- */
        gfx->scrshot |= GFX_MOVIE;
        pvt->movie->fr = 0;
    }

    if ((gfx->scrshot & GFX_RESET) == 0)
        mvi_wr_frame(pvt->movie, gfx->vid, gfx->bbox);
}


/* ======================================================================== */
/*  GFX_FIND_DIRTY_RECTS -- Finds dirty rectangles in the current image.    */
/*                                                                          */
/*  Current algorithm just divides the display into 240 8x16 tiles aligned  */
/*  with the STIC's cards.  A tile is considered either clean or dirty      */
/*  in its entirety for now.  A tile can be merged with tiles to its        */
/*  right if they're contiguous, or there's a gap of at most one tile.      */
/*                                                                          */
/*  The algorithm is also responsible for copying the new image into the    */
/*  reference image, and constructing a bitmap of which rows need to be     */
/*  expanded by the scaler code.                                            */
/* ======================================================================== */
LOCAL void gfx_find_dirty_rects(gfx_t *gfx)
{
    int x, y, xx, yy, i, j, t;
    int nr = 0, row_start;
    uint_32 *RESTRICT old_pix = (uint_32 *)gfx->pvt->prev;
    uint_32 *RESTRICT new_pix = (uint_32 *)gfx->pvt->inter_vid;
    uint_32 is_dirty;
    SDL_Rect *rect = gfx->pvt->dirty_rects;

    int wpitch = gfx->pvt->dr_spec.pitch >> 2;
    int y0 = gfx->pvt->dr_spec.active_first_y;
    int y1 = gfx->pvt->dr_spec.active_last_y + 1;
    int ys = gfx->pvt->dr_spec.y_step;

    int x0 = (gfx->pvt->dr_spec.active_first_x >> 3);
    int x1 = (gfx->pvt->dr_spec.active_last_x  >> 3) + 1;
    int xs = (gfx->pvt->dr_spec.x_step         >> 3);

    int bo = (gfx->pvt->dr_spec.bord_first_x >> 2) +
             (gfx->pvt->dr_spec.bord_first_y * wpitch);

    /* -------------------------------------------------------------------- */
    /*  Set our merge threshold based on whether we're allowed to include   */
    /*  a clean rectangle between two dirty rectangles when coalescing.     */
    /* -------------------------------------------------------------------- */
    t = gfx->pvt->flags & GFX_DRCMRG ? 1 : 0;

    /* -------------------------------------------------------------------- */
    /*  Initally mark all rows clean.                                       */
    /* -------------------------------------------------------------------- */
    memset((void *)gfx->pvt->dirty_rows, 0, gfx->pvt->dirty_rows_sz);

    /* -------------------------------------------------------------------- */
    /*  Scan the source image tile-row-wise looking for differences.        */
    /* -------------------------------------------------------------------- */
    for (y = y0; y < y1; y += ys)
    {
        row_start = nr;

        /* ---------------------------------------------------------------- */
        /*  Find dirty rectangles in this row of cards.                     */
        /* ---------------------------------------------------------------- */
        for (x  = x0; x < x1; x += xs)
        {
            is_dirty = 0;
            switch (xs)
            {
                case 1:
                {
                    for (yy = y; yy < y + ys; yy++)
                        is_dirty  |= (old_pix[yy * wpitch + x*2 + 0] !=
                                      new_pix[yy * wpitch + x*2 + 0])
                                  |  (old_pix[yy * wpitch + x*2 + 1] !=
                                      new_pix[yy * wpitch + x*2 + 1]);
                    break;
                }

                case 2:
                {
                    for (yy = y; yy < y + ys; yy++)
                        is_dirty  |= (old_pix[yy * wpitch + x*2 + 0] !=
                                      new_pix[yy * wpitch + x*2 + 0])
                                  |  (old_pix[yy * wpitch + x*2 + 1] !=
                                      new_pix[yy * wpitch + x*2 + 1]) 
                                  |  (old_pix[yy * wpitch + x*2 + 2] !=
                                      new_pix[yy * wpitch + x*2 + 2]) 
                                  |  (old_pix[yy * wpitch + x*2 + 3] !=
                                      new_pix[yy * wpitch + x*2 + 3]);
                    break;
                }

                case 3:
                {
                    for (yy = y; yy < y + ys; yy++)
                        is_dirty  |= (old_pix[yy * wpitch + x*2 + 0] !=
                                      new_pix[yy * wpitch + x*2 + 0])
                                  |  (old_pix[yy * wpitch + x*2 + 1] !=
                                      new_pix[yy * wpitch + x*2 + 1]) 
                                  |  (old_pix[yy * wpitch + x*2 + 2] !=
                                      new_pix[yy * wpitch + x*2 + 2]) 
                                  |  (old_pix[yy * wpitch + x*2 + 3] !=
                                      new_pix[yy * wpitch + x*2 + 3]) 
                                  |  (old_pix[yy * wpitch + x*2 + 4] !=
                                      new_pix[yy * wpitch + x*2 + 4]) 
                                  |  (old_pix[yy * wpitch + x*2 + 5] !=
                                      new_pix[yy * wpitch + x*2 + 5]);
                    break;
                }

                case 4:
                {
                    for (yy = y; yy < y + ys; yy++)
                        is_dirty  |= (old_pix[yy * wpitch + x*2 + 0] !=
                                      new_pix[yy * wpitch + x*2 + 0])
                                  |  (old_pix[yy * wpitch + x*2 + 1] !=
                                      new_pix[yy * wpitch + x*2 + 1]) 
                                  |  (old_pix[yy * wpitch + x*2 + 2] !=
                                      new_pix[yy * wpitch + x*2 + 2]) 
                                  |  (old_pix[yy * wpitch + x*2 + 3] !=
                                      new_pix[yy * wpitch + x*2 + 3]) 
                                  |  (old_pix[yy * wpitch + x*2 + 4] !=
                                      new_pix[yy * wpitch + x*2 + 4]) 
                                  |  (old_pix[yy * wpitch + x*2 + 5] !=
                                      new_pix[yy * wpitch + x*2 + 5]) 
                                  |  (old_pix[yy * wpitch + x*2 + 6] !=
                                      new_pix[yy * wpitch + x*2 + 6]) 
                                  |  (old_pix[yy * wpitch + x*2 + 7] !=
                                      new_pix[yy * wpitch + x*2 + 7]);
                    break;
                }

                default:
                {
                    for (yy = y; yy < y + ys; yy++)
                        for (xx = x; xx < x + xs; xx++)
                            is_dirty |= (old_pix[yy * wpitch + xx*2 + 0] !=
                                         new_pix[yy * wpitch + xx*2 + 0])
                                     |  (old_pix[yy * wpitch + xx*2 + 1] !=
                                         new_pix[yy * wpitch + xx*2 + 1]);

                    break;
                }
            }

            if (is_dirty)
            {
                rect[nr].x = x;
                rect[nr].y = y;
                rect[nr].w = xs;
                rect[nr].h = ys;
                nr++;
            }
/*fprintf(stderr, "%3d %3d %3d\n", x, y, nr); */
        }

        /* ---------------------------------------------------------------- */
        /*  While it's still hot in the cache, copy "new" to "old"          */
        /* ---------------------------------------------------------------- */
        memcpy((void *)&old_pix[y * wpitch], 
               (void *)&new_pix[y * wpitch], 
               4 * wpitch * ys);

        /* ---------------------------------------------------------------- */
        /*  Mark these rows as dirty in the dirty_row bitmap                */
        /* ---------------------------------------------------------------- */
        if (nr > row_start)
            for (yy = y; yy < y + ys; yy++)
                gfx->pvt->dirty_rows[yy >> 5] |= 1 << (yy & 31);

        /* ---------------------------------------------------------------- */
        /*  Coalesce rectangles if they're adjacent or separated by at      */
        /*  most one clean rectangle.                                       */
        /* ---------------------------------------------------------------- */
        if (nr - row_start < 2)
            continue;

        for (i = row_start, j = row_start + 1; j < nr; j++)
        {
            if (rect[i].x + rect[i].w + t >= rect[j].x)
            {
                rect[i].w = rect[j].x - rect[i].x + rect[j].w;
                continue;
            } else
            {
                rect[++i] = rect[j];
            }
        }

        nr = i + 1;
    }

    /* -------------------------------------------------------------------- */
    /*  If border areas changed color, update those too.                    */
    /*  XXX:  This needs to get fixed when I fix scaler's border handler.   */
    /* -------------------------------------------------------------------- */
    if (old_pix[bo] != new_pix[bo])
    {
        int x0l, x0h, y0l, y0h;     /* upper rectangle */
        int x1l, x1h, y1l, y1h;     /* lower rectangle */

        old_pix[bo] =  new_pix[bo];

        x0l = x1l = gfx->pvt->dr_spec.bord_first_x >> 3;    /* in dwords */
        x0h = x1h = gfx->pvt->dr_spec.bord_last_x  >> 3;    /* in dwords */

        y0l = gfx->pvt->dr_spec.bord_first_y;               /* in pixels */
        y0h = gfx->pvt->dr_spec.active_first_y - 1;         /* in pixels */

        y1l = gfx->pvt->dr_spec.active_last_y + 1;          /* in pixels */
        y1h = gfx->pvt->dr_spec.bord_last_y;                /* in pixels */

        rect[nr].x = x0l;
        rect[nr].y = y0l;
        rect[nr].w = x0h - x0l + 1;
        rect[nr].h = y0h - y0l + 1;
        nr++;

        rect[nr].x = x1l;
        rect[nr].y = y1l;
        rect[nr].w = x1h - x1l + 1;
        rect[nr].h = y1h - y1l + 1;
        nr++;

        for (yy = y0l; yy <= y0h; yy++)
            gfx->pvt->dirty_rows[yy >> 5] |= 1 << (yy & 31);

        for (yy = y1l; yy <= y1h; yy++)
            gfx->pvt->dirty_rows[yy >> 5] |= 1 << (yy & 31);
    }

    /* -------------------------------------------------------------------- */
    /*  Convert the rectangles to display coordinates.  Ick.                */
    /* -------------------------------------------------------------------- */
    for (i = 0; i < nr; i++)
    {
        int w, h;
#ifdef BENCHMARK_GFX
        drw_hist[rect[i].w]++;
#endif
        x = rect[i].x * 8;
        y = rect[i].y;
        w = rect[i].w * 8;
        h = rect[i].h;

        rect[i].x  = gfx->pvt->scaler.scaled_x[x];
        rect[i].y  = gfx->pvt->scaler.scaled_y[y];
        rect[i].w  = gfx->pvt->scaler.scaled_x[x + w] - rect[i].x;
        rect[i].h  = gfx->pvt->scaler.scaled_y[y + h] - rect[i].y;

        rect[i].x += gfx->pvt->ofs_x;
        rect[i].y += gfx->pvt->ofs_y;
    }

    gfx->pvt->num_rects = nr;

#ifdef BENCHMARK_GFX
    dr_hist[nr]++;
#endif

    return;
}

#ifdef BENCHMARK_GFX
LOCAL void gfx_dr_hist_dump(void)
{
    int i;

    jzp_printf("Dirty rectangle counts:\n");
    for (i = 0; i <= 244; i++)
        if (dr_hist[i])
            jzp_printf("%4d: %7d\n", i, dr_hist[i]);

    jzp_printf("Dirty rectangle width counts:\n");
    for (i = 0; i <= 20; i++)
        if (drw_hist[i])
            jzp_printf("%4d: %7d\n", i, drw_hist[i]);
}
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
/*          Copyright (c) 1998-2006, Joseph Zbiciak, John Tanner            */
/* ======================================================================== */
