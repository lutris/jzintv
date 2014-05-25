/* ======================================================================== */
/*  EMITTERS:  Functions that emit a record to the intermediate file.       */
/*                                                                          */
/*  This file is an attempt to collect up all the functions that write to   */
/*  the intermediate file in one place, increasing the abstraction level.   */
/*  The goal is to eventually eliminate the intermediate file altogether,   */
/*  once all direct references to it have been removed.                     */
/*                                                                          */
/*  Initially, though, this will just be a series of functions that wrap    */
/*  the fprintf statements collected from elsewhere.                        */
/* ======================================================================== */

#include "config.h"
#include "types.h"
#include "file/file.h"
#include "frasmdat.h"
#include "fragcon.h"
#include "protos.h"
#include "intermed.h"
#include "memo_string.h"
/*#include "malloc.h"*/

/* ------------------------------------------------------------------------ */
/*  Render buffer for emitters.                                             */
/* ------------------------------------------------------------------------ */
static char   *emit_buf = NULL;
static size_t  eb_len   = 0;

/* ------------------------------------------------------------------------ */
/*  Replace intermediate file with in-memory list.                          */
/* ------------------------------------------------------------------------ */

static irec_union *irec_tbl = NULL;
static unsigned irec_head = 0, irec_tail = 0, irec_total = 0;

/* ------------------------------------------------------------------------ */
/*  ALLOC_IREC           -- Allocate an intermediate record that's already  */
/*                          appended to the list, but must be populated.    */
/* ------------------------------------------------------------------------ */
static intermed_rec *alloc_irec(irec_type type)
{
    irec_union *irec;

    if (irec_tail - irec_head >= irec_total)
    {
        assert(irec_total < (1 << 29));
        irec_total = irec_total ? (irec_total << 2) : (1 << 16);
        irec_tbl   = (irec_union *)realloc(irec_tbl, 
                                           irec_total * sizeof(irec_union));
        assert(irec_tbl);
    }

    irec = &irec_tbl[irec_tail++ % irec_total];

    irec->irec.type = type;
    irec->irec.line = infilestk[currfstk].line;

    return (intermed_rec *)irec;
}

#define GET_IREC(type,TYPE) type *irec = (type *)alloc_irec(TYPE)


/* ------------------------------------------------------------------------ */
/*  EMIT_LISTING_MODE    -- This changes the output listing display mode    */
/* ------------------------------------------------------------------------ */
void emit_listing_mode(listing_mode m)
{
    GET_IREC(irec_list_mode, REC_LIST_MODE);
    irec->mode = m;
}

/* ------------------------------------------------------------------------ */
/*  EMIT_SET_EQU         -- Output value associated with a SET or EQU.      */
/* ------------------------------------------------------------------------ */
void emit_set_equ(unsigned int value)
{
    GET_IREC(irec_set_equ, REC_SET_EQU);
    irec->value = value;
}


/* ------------------------------------------------------------------------ */
/*  EMIT_COMMENT         -- Emit a comment either for SET/EQU or a user's   */
/*                          CMSG/SMSG.                                      */
/* ------------------------------------------------------------------------ */
void emit_comment(int is_user, const char *format, ...)
{
    GET_IREC(irec_string, is_user ? REC_USER_COMMENT : REC_COMMENT);
    va_list ap;

    assert(emit_buf);

again:
    va_start(ap, format);
    vsnprintf(emit_buf, eb_len, format, ap);
    va_end(ap);

    if (eb_len < 65536 && strlen(emit_buf) >= eb_len - 1)
    {
        eb_len <<= 2;
        emit_buf = (char *)realloc(emit_buf, eb_len);
        goto again;
    }

    irec->string = memoize_string(emit_buf);
}

/* ------------------------------------------------------------------------ */
/*  EMIT_LOCATION        -- Establish the current address we're assembling  */
/*                          at, and what sort of memory mode.               */
/* ------------------------------------------------------------------------ */
void emit_location(int seg, int loc, int type, const char *mode)
/*
       description output to the intermediate file, a 'P' record
                   giving the current location counter.  Segment
                   is not used at this time.
*/
{
    GET_IREC(irec_loc_set, REC_LOC_SET);

    irec->seg  = seg;
    irec->loc  = loc;
    irec->type = type;
    irec->mode = mode;
}

/* ------------------------------------------------------------------------ */
/*  EMIT_MARK_WITH_MODE  -- Mark addresses lo to hi with a given mode.      */
/* ------------------------------------------------------------------------ */
void emit_mark_with_mode(int lo, int hi, const char *mode)
{
    GET_IREC(irec_mark_range, REC_MARK_RANGE);
    
    irec->lo   = lo;
    irec->hi   = hi;
    irec->mode = mode;
}

/* ------------------------------------------------------------------------ */
/*  EMIT_RESERVE         -- Reserve 'len' addresses.                        */
/* ------------------------------------------------------------------------ */
void emit_reserve(int len)
{
    GET_IREC(irec_reserve_range, REC_RESERVE_RANGE);
    irec->len = len;
}

/* ------------------------------------------------------------------------ */
/*  EMIT_ENTERING_FILE   -- Record that we're entering a new file.          */
/* ------------------------------------------------------------------------ */
void emit_entering_file(const char *name)
{
    GET_IREC(irec_string, REC_FILE_START);
    irec->string = name;
}

/* ------------------------------------------------------------------------ */
/*  EMIT_EXITING_FILE    -- Record that we're leaving a file.               */
/* ------------------------------------------------------------------------ */
void emit_exiting_file(const char *name)
{
    GET_IREC(irec_string, REC_FILE_EXIT);
    irec->string = name;
}

/* ------------------------------------------------------------------------ */
/*  EMIT_WARNERR         -- Emit a warning or an error                      */
/* ------------------------------------------------------------------------ */
void emit_warnerr(const char *file, int line, warnerr type,
                  const char *format, ...)
{
    GET_IREC(irec_string, REC_ERROR);
    va_list ap;

    size_t pf_len, file_len = strlen(file), emit_len;
    char *final;

    if (file_len + 100 > eb_len)
    {
        free(emit_buf);
        eb_len   = 2 * (file_len + 100);
        emit_buf = (char *)malloc(eb_len);
    }

    sprintf(emit_buf,
            type == WARNING ? "%s:%d: WARNING - " : "%s:%d: ERROR - ",
            file, line);

    pf_len = strlen(emit_buf);  

    if (type == WARNING) warncnt++;
    else                 errorcnt++;

again:
    va_start(ap, format);
    vsnprintf(emit_buf + pf_len, eb_len - pf_len, format, ap);
    va_end(ap);

    emit_len = strlen(emit_buf);
    if (eb_len < 65536 && emit_len >= eb_len - 1)
    {
        eb_len <<= 2;
        emit_buf = (char *)realloc(emit_buf, eb_len);
        goto again;
    }

    final = (char *)malloc(emit_len + 2);
    strcpy(final, emit_buf);
    final[emit_len] = 0;

    irec->string = final;
}

/* ------------------------------------------------------------------------ */
/*  EMIT_RAW_ERROR       -- Emit a raw error (typically as reported by      */
/*                          the macro preprocessor).                        */
/* ------------------------------------------------------------------------ */
void emit_raw_error(const char *raw_error)
{
    GET_IREC(irec_string, REC_ERROR);
    irec->string = memoize_string(raw_error);
    errorcnt++;
}

/* ------------------------------------------------------------------------ */
/*  EMIT_LISTED_LINE     -- A line to put directly in the listing file.     */
/* ------------------------------------------------------------------------ */
void emit_listed_line(const char *buf)
{
    GET_IREC(irec_string, REC_LIST_LINE);
    irec->string = memoize_string(buf);
}

/* ------------------------------------------------------------------------ */
/*  EMIT_UNLISTED_LINE   -- Keep track of source lines consumed, even if    */
/*                          they won't be replayed in the listing file.     */
/* ------------------------------------------------------------------------ */
void emit_unlisted_line(void)
{
}

/* ------------------------------------------------------------------------ */
/*  EMIT_GENERATED_INSTR -- Output the "polish notation" string for this    */
/*                          instruction.                                    */
/* ------------------------------------------------------------------------ */
void emit_generated_instr(const char *buf)
{
    GET_IREC(irec_string, REC_DATA_BLOCK);
    irec->string = memoize_string(buf);
}


/* ------------------------------------------------------------------------ */
/*  INTERMED_START_PASS_1 -- Initialize the intermediate file (and later,   */
/*                           other data structures) for pass 1.             */
/* ------------------------------------------------------------------------ */
void intermed_start_pass_1(void)
{
    intermed_finish(0);

    if (!emit_buf)
    {
        emit_buf = (char *)malloc(4096);
        eb_len   = 4096;
    }
}

/* ------------------------------------------------------------------------ */
/*  INTERMED_START_PASS_2 -- Initialize the intermediate file (and later,   */
/*                           other data structures) for pass 2.             */
/* ------------------------------------------------------------------------ */
void intermed_start_pass_2(void)
{
    //fprintf(stderr, "total allocs = %d\n", allocs);
#if 0
    struct mallinfo m = mallinfo();

#   define M(x)    printf("%-15s %ld\n", #x, (long)m.x);
    M(arena)
    M(ordblks)
    M(smblks)
    M(hblks)
    M(hblkhd)
    M(usmblks)
    M(fsmblks)
    M(uordblks)
    M(fordblks)
    M(keepcost)
#endif
}

/* ------------------------------------------------------------------------ */
/*  INTERMED_FINISH       -- Clean up all the intermediate file/data/etc.   */
/*                           Optionally output debug info.                  */
/* ------------------------------------------------------------------------ */
void intermed_finish(int debugmode)
{
    UNUSED(debugmode);

    if (irec_tbl)
    {
        while (irec_head < irec_tail)
            pass2_release_rec(&irec_tbl[irec_head++ % irec_total]);

        free(irec_tbl);
        irec_head = irec_tail = irec_total = 0;
    }
}

/* ------------------------------------------------------------------------ */
/*  PASS2_NEXT_LINE      -- Gets the next line of the intermediate data.    */
/* ------------------------------------------------------------------------ */
irec_union *pass2_next_rec(void)
{
    if (irec_head > irec_total)
    {
        irec_head -= irec_total;
        irec_tail -= irec_total;
    }
    return irec_head < irec_tail ? &irec_tbl[irec_head++ % irec_total] : NULL;
}

void pass2_release_rec(irec_union *irec)
{
    UNUSED(irec);
}
