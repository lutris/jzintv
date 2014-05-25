/* ======================================================================== */
/*  Graphics Prescaler                                                      */
/*                                                                          */
/*  The graphics prescaler allows scaling up the 160x200 bitmap to some     */
/*  other size prior to final scaling to display resolution.  This is       */
/*  where we'd apply transforms such as Scale2X or a variant.               */
/* ======================================================================== */

#include "config.h"
#include "periph/periph.h"
#include "gfx.h"
#include "gfx_prescale.h"



typedef struct gfx_prescaler_typ_pvt_t
{
    int orig_x, orig_y;
} gfx_prescaler_typ_pvt_t;


                                 

/* ======================================================================== */
/*  Prescalers                                                              */
/* ======================================================================== */

/* ------------------------------------------------------------------------ */
/*  NULL prescaler                                                          */
/* ------------------------------------------------------------------------ */
LOCAL void *gfx_prescaler_null_init(int orig_x, int orig_y,
                                    int *RESTRICT new_x,
                                    int *RESTRICT new_y,
                                    gfx_dirtyrect_spec *RESTRICT dr_spec)
{
    *new_x = orig_x;
    *new_y = orig_y;

    dr_spec->active_first_x = 0;
    dr_spec->active_first_y = 4;
    dr_spec->active_last_x  = orig_x - 1;
    dr_spec->active_last_y  = orig_y - 5;

    dr_spec->x_step         = 8;
    dr_spec->y_step         = 16;

    dr_spec->pitch          = orig_x;

    dr_spec->bord_first_x   = 0;
    dr_spec->bord_first_y   = 0;
    dr_spec->bord_last_x    = orig_x - 1;
    dr_spec->bord_last_y    = orig_y - 1;

    return NULL;
}

LOCAL void gfx_prescaler_null(const uint_8 *RESTRICT src,
                                    uint_8 *RESTRICT dst,
                                    void *RESTRICT opaque)
{
    return;
}

/* ------------------------------------------------------------------------ */
/*  SCALE2X                                                                 */
/* ------------------------------------------------------------------------ */
LOCAL void *gfx_prescaler_scale2x_init(int orig_x, int orig_y,
                                       int *RESTRICT new_x,
                                       int *RESTRICT new_y,
                                       gfx_dirtyrect_spec *RESTRICT dr_spec)
{
    gfx_prescaler_typ_pvt_t *pvt = CALLOC(gfx_prescaler_typ_pvt_t, 1);

    if (!pvt)
    {
        fprintf(stderr, "Out of memory in gfx_prescale\n");
        exit(1);
    }

    pvt->orig_x = orig_x;
    pvt->orig_y = orig_y;

    *new_x = orig_x * 2;
    *new_y = orig_y * 2;

    dr_spec->active_first_x = 0;
    dr_spec->active_first_y = 8;
    dr_spec->active_last_x  = 2*orig_x - 1;
    dr_spec->active_last_y  = 2*orig_y - 9;

    dr_spec->x_step         = 16;
    dr_spec->y_step         = 32;

    dr_spec->pitch          = 2*orig_x;

    dr_spec->bord_first_x   = 0;
    dr_spec->bord_first_y   = 0;
    dr_spec->bord_last_x    = 2*orig_x - 1;
    dr_spec->bord_last_y    = 2*orig_y - 1;

    return pvt;
}

LOCAL void  gfx_prescaler_scale2x(const uint_8 *RESTRICT src,
                                        uint_8 *RESTRICT dst,
                                          void *RESTRICT opaque)
{
    gfx_prescaler_typ_pvt_t *pvt = (gfx_prescaler_typ_pvt_t *)opaque;
    int orig_x = pvt->orig_x, orig_y = pvt->orig_y;
    int x, y;

    /* TODO: Use actual Scale2X algorithm here! */

    for (y = 0; y < orig_y; y++)
        for (x = 0; x < orig_x; x++)
        {
            dst[x*2+0 + y*orig_x*4] = src[x + y * orig_x];
            dst[x*2+1 + y*orig_x*4] = src[x + y * orig_x];
            dst[x*2+0 + y*orig_x*4 + 2*orig_x] = src[x + y * orig_x];
            dst[x*2+1 + y*orig_x*4 + 2*orig_x] = src[x + y * orig_x];
        }

    return;
}




/* ======================================================================== */
/*  Prescaler Registry                                                      */
/* ======================================================================== */
gfx_prescaler_registry_t gfx_prescaler_registry[] =
{
    {   "None",     gfx_prescaler_null,     gfx_prescaler_null_init         },
    {   "Scale2x",  gfx_prescaler_scale2x,  gfx_prescaler_scale2x_init      }
};


int gfx_prescaler_registry_size = sizeof(gfx_prescaler_registry) /
                                  sizeof(gfx_prescaler_registry_t);

