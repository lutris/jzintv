/* ======================================================================== */
/*  INTELLICART ROM MetaData Tag routines.              J. Zbiciak, 2001    */
/*                                                                          */
/*  These routines manipulate metadata tags that might be appended to an    */
/*  Intellicart ROM file.  Routines are provided for generating a linked    */
/*  list of decoded tags and for converting a list of tags into an          */
/*  encoded image.                                                          */
/* ======================================================================== */

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
/*                 Copyright (c) 2001-+Inf, Joseph Zbiciak                  */
/* ======================================================================== */

#include <stdio.h>
#include <assert.h>
#include "config.h"
#include "misc/crc16.h"


#define ICT_MAX_TAG_SIZE (1 << 17)

/* ======================================================================== */
/*  ICT_DECODE       -- Decode a set of tags from a file.                   */
/* ======================================================================== */
icarttag_t  *ict_decode(FILE *f, int ignore_crc, int drop_unknown)
{
    int         c, i, tot;
    uint_32     len;
    uint_16     crc_expect, crc_actual;
    uint_8      *data;
    ict_type_t  type;
    icarttag_t  *tags = NULL, *tag;

    /* -------------------------------------------------------------------- */
    /*  Each tag starts of with length bytes.  The last tag has a length    */
    /*  of zero.  So, loop until we fall off the end of file or hit a zero  */
    /*  length tag.                                                         */
    /* -------------------------------------------------------------------- */
    while ((c = fgetc(f)) != EOF && c != 0)
    {
        /* ---------------------------------------------------------------- */
        /*  Parse the encoded length.  The two MSBs of the length byte      */
        /*  say how many additional length bytes are present.               */
        /* ---------------------------------------------------------------- */
        len = c & 0x3F;
        tot = 6 + ((c >> 3) & 24);
        for (i = 6; i < tot; i += 8)
            len |= fgetc(f) << i;

        /* ---------------------------------------------------------------- */
        /*  Grab the tag type.  Abort if we fall off end.                   */
        /* ---------------------------------------------------------------- */
        if ((c = fgetc()) == EOF)   /* Abort if we fall off end.    */
            break;
        
        /* ---------------------------------------------------------------- */
        /*  Skip overly large tags -- we just don't try to handle them.     */
        /* ---------------------------------------------------------------- */
        if (len > ICT_MAX_TAG_SIZE) /* For now, skip large tags. */
        {
            fprintf(stderr, "ict_decode:  Warning: Skipping tag type %.2X "
                            "of length %d\n", (int)type, len);
            fseek(f, len + 2, SEEK_CUR);
            continue;
        }

        /* ---------------------------------------------------------------- */
        /*  Allocate some temporary memory to hold the tag's payload.       */
        /* ---------------------------------------------------------------- */
        if ((data = malloc(len))                  == NULL || 
            (tag  = calloc(sizeof(icarttag_t), 1) == NULL))
        {
            fprintf(stderr, "ict_decode:  Warning: Out of memory\n");
            if (data) free(data);
            break;
        }

        /* ---------------------------------------------------------------- */
        /*  Read in the tag body and check the CRC.  Abort on a short read. */
        /* ---------------------------------------------------------------- */
        if (fread(data, 1, len, f) != len)
        {
            fprintf(stderr, "ict_decode:  Short read.  File truncated?\n");
            break;
        }
        crc_expect  = fgetc(f) << 8;
        crc_expect |= fgetc(f);
        crc_actual  = crc16_block(0xFFFF, data, len);

        if (!ignore_crc && crc_actual != crc_expect)
        {
            fprintf(stderr, "ict_decode:  CRC mismatch %.4X != %.4X."
                            "  Skipping.\n"
                            "             Tag type %.2X, length %.8X\n",
                            (int)type, len, crc_expect, crc_actual);
            free(data);
            free(tag);
            continue;
        }
        
        /* ---------------------------------------------------------------- */
        /*  Now decode the payload according to type.                       */
        /* ---------------------------------------------------------------- */
        tag->type = type;
    
        switch (type)
        {
            /* ------------------------------------------------------------ */
            /*  Skip IGNORE tags.                                           */
            /* ------------------------------------------------------------ */
            case ICT_IGNORE:    
            {
                free(data); free(tag); 
                tag = data = NULL;
                break;
            }

            /* ------------------------------------------------------------ */
            /*  Title tag.                                                  */
            /* ------------------------------------------------------------ */
            case ICT_TITLE:
            {
                /* Make sure it's zero-terminated. */
                if (data[len - 1] == 0)
                    tag->d.title = data;
                else
                {
                    fprintf(stderr, "ict_decode: Warning: Invalid TITLE tag\n");
                    free(tag);
                    free(data);
                    tag = NULL
                }
                data = NULL;
                break;
            }

            /* ------------------------------------------------------------ */
            /*  Publisher tag.                                              */
            /* ------------------------------------------------------------ */
            case ICT_PUBLISHER:
            {
                if (data[0] >= 0x80)
                {
                    char buf[64];

                    snprintf(buf, sizeof(buf), "PUBID: $%.2X\n", data[0]);
                    tag->d.publisher = strdup(buf);

                    free(data);
                    data = NULL;
                } else
                {
                    /* Make sure it's zero-terminated. */
                    if (data[len - 1] == 0)
                        tag->d.publisher = data;
                    else
                    {
                        fprintf(stderr, "ict_decode: Warning: "
                                        "Invalid PUBLISHER tag\n");
                        free(tag);
                        free(data);
                        tag = data = NULL;
                    }
                }
                break;
            }
                    
            /* ------------------------------------------------------------ */
            /*  Credits tag.                                                */
            /* ------------------------------------------------------------ */
            case ICT_CREDITS:
            case ICT_INFOURLS:
            {
                /* drop it for now */
                free(data); free(tag);
                data = tag = NULL;
                break;
            }



/* ======================================================================== */
/*  API functions for reading/writing tags.                                 */
/* ======================================================================== */
uint_8      *ict_gentag(icarttag_t *tags);

/* ======================================================================== */
/*  API functions for querying the tags.                                    */
/* ======================================================================== */
icarttag_t  *ict_findtag        (icarttag_t *tag, ict_type_t type);

char        *ict_get_title      (icarttag_t *tag);
char        *ict_get_publisher  (icarttag_t *tag);
char        *ict_get_credits    (icarttag_t *tag);
char        *ict_get_date       (icarttag_t *tag);   /* Human Readable Date */

int         ict_get_supports    (icarttag_t *tag, uint_32 what);
int         ict_get_requires    (icarttag_t *tag, uint_32 what);
int         ict_get_incompat    (icarttag_t *tag, uint_32 what);

char*       ict_addr2symb       (icarttag_t *tag, uint_32 addr);
uint_32     ict_symb2addr       (icarttag_t *tag, char *symb);

/* ======================================================================== */
/*  API functions for generating tags.                                      */
/* ======================================================================== */
void        ict_set_title       (icarttag_t *tag, char *title);
void        ict_set_publisher   (icarttag_t *tag, char *name);
void        ict_add_credit      (icarttag_t *tag, char *name, uint_8 flags);
void        ict_set_date        (icarttag_t *tag, int yr, int mo, int day);

void        ict_set_supports    (icarttag_t *tag, uint_32 what);
void        ict_set_requires    (icarttag_t *tag, uint_32 what);
void        ict_set_incompat    (icarttag_t *tag, uint_32 what);

int         ict_def_symbol      (icarttag_t *tag, char *symb, uint_32 addr);

/* ======================================================================== */
/*  Useful arrays                                                           */
/* ======================================================================== */
extern char *ict_publisher_names[];
extern char *ict_credit_names[];


#endif
/* ======================================================================== */
/*                 Copyright (c) 2001-+Inf, Joseph Zbiciak                  */
/* ======================================================================== */
