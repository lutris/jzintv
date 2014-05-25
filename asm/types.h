#ifndef TYPES_H_
#define TYPES_H_

typedef enum { LIST_ON, LIST_CODE, LIST_OFF, LIST_PREV } listing_mode;
typedef enum { WARNING, ERROR } warnerr;

typedef enum 
{
    REC_INVALID = 0,
    REC_LIST_MODE,
    REC_ERROR,
    REC_LIST_LINE,
    REC_SET_EQU,
    REC_COMMENT,
    REC_USER_COMMENT,
    REC_LOC_SET,
    REC_RESERVE_RANGE,
    REC_MARK_RANGE,
    REC_DATA_BLOCK,
    REC_FILE_START,
    REC_FILE_EXIT
} irec_type;

typedef struct intermed_rec intermed_rec;

struct intermed_rec
{
    int             line : 24;
    irec_type       type :  8;
};

typedef struct irec_string
{
    intermed_rec    irec;
    const char      *string;
} irec_string;

typedef struct irec_list_mode
{
    intermed_rec    irec;
    listing_mode    mode;
} irec_list_mode;

typedef struct irec_set_equ
{
    intermed_rec    irec;
    unsigned int    value;
} irec_set_equ;

typedef struct irec_loc_set
{
    intermed_rec    irec;
    int             type : 16;
    int             seg  : 16;
    int             loc;
    const char      *mode;
} irec_loc_set;

typedef struct irec_mark_range
{
    intermed_rec    irec;
    const char      *mode;
    int             lo;
    int             hi;
} irec_mark_range;

typedef struct irec_reserve_range
{
    intermed_rec    irec;
    int             len;
} irec_reserve_range;

typedef union irec_union
{
    intermed_rec        irec;
    irec_string         string;
    irec_set_equ        set_equ;
    irec_list_mode      list_mode;
    irec_loc_set        loc_set;
    irec_mark_range     mark_range;
    irec_reserve_range  reserve_range;
} irec_union;

#endif
