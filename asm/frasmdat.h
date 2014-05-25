
/*
HEADER:     ;
TITLE:      Frankenstein Cross Assemblers;
VERSION:    2.0;
DESCRIPTION: "  Reconfigurable Cross-assembler producing Intel (TM)
        Hex format object records.  ";
FILENAME:   frasmdat.h;
SEE-ALSO:   ;
AUTHORS:    Mark Zenier;
*/

/*
    description structures and data used in parser and output phases
    history     September 15, 1987
            August 3, 1988   Global
            September 14, 1990   6 char portable var
*/

#include <ctype.h>
#define PRINTCTRL(char) ((char)+'@')

#ifndef Global
#define Global  extern
#endif

#ifdef USEINDEX
#define strchr index
#endif

#ifdef NOSTRING
extern char * strncpy();
extern char * strchr();
extern int strcmp();
extern int strlen();
#else
#include <string.h>
#endif

#define local 

#define TRUE 1
#define FALSE 0

#define hexch(cv) (hexcva[(cv)&0xf])
extern char hexcva[];

struct slidx { int first, last; struct symel *sym; };


/* symbol table element */
struct symel
{
    const char  *symstr;
    int         tok;
    int         seg;
    unsigned    flags;
    int         value;
    struct      symel *nextsym;
    int         symnum;
};

#define SSG_UNUSED  ( 0)
#define SSG_UNDEF   (-1)
#define SSG_ABS     ( 8)
#define SSG_RESV    (-2)
#define SSG_EQU     ( 2)
#define SSG_SET     ( 3)

#define SFLAG_NONE  (     0)
#define SFLAG_QUIET (1 << 0)
#define SFLAG_ARRAY (1 << 1)

#define SYMNULL (struct symel *) NULL

/* opcode symbol table element */

struct opsym
{
    const char *opstr;
    int         token;
    int         numsyn;
    int         subsyn;
};

struct opsynt
{
    int         syntaxgrp;
    int         elcnt;
    int         gentabsub;
};

struct igel 
{
    int         selmask;
    int         criteria;
    const char *genstr;
};
    
#define PPEXPRLEN 256

struct evalrel
{
    int     seg;
    int     value;
    char    exprstr[PPEXPRLEN];
};

#define INBUFFSZ (16384)

extern int nextsymnum;
Global struct symel **symbindex;

#define EXPRLSIZE (INBUFFSZ/2)
extern int nextexprs;
Global int exprlist[EXPRLSIZE];
Global int exprvals[EXPRLSIZE];

#define STRLSIZE (INBUFFSZ/2)
extern int  nextstrs;
Global char *stringlist[STRLSIZE];

extern struct opsym optab[];
extern int    gnumopcode;
extern struct opsynt ostab[];
extern struct igel igtab[];
extern int    ophashlnk[];

#define NUMPEXP 6
Global struct evalrel evalr[NUMPEXP];

#define PESTKDEPTH 32
struct evstkel
{
    int v;
    int s;
};

Global struct evstkel   estk[PESTKDEPTH], *estkm1p;

Global int  currseg; 
Global int  locctr; 

extern FILE *yyin;
extern int  listflag;
extern int hexvalid, hexflag;
Global FILE *romoutf, *binoutf, *cfgoutf, *loutf;
Global char *loutfn;
Global int listlineno;
extern int errorcnt, warncnt;


#define IFSTKDEPTH 32
extern int  ifstkpt; 
Global enum { If_Active, If_Skip, If_Err } 
    elseifstk[IFSTKDEPTH], endifstk[IFSTKDEPTH];

extern int  frarptact, frarptcnt, frarptskip;

#define FILESTKDPTH 32
Global int currfstk;
#define nextfstk (currfstk+1)
Global struct fstkel
{
    const char  *fnm;
    FILE        *fpt;
    int         line;
} infilestk[FILESTKDPTH];

Global int lnumstk[FILESTKDPTH];

enum readacts
{
    Nra_normal, 
    Nra_new, 
    Nra_end 
} ;

extern enum readacts nextreadact;


#ifndef macintosh
#include <stdlib.h>
#endif

extern struct symel * endsymbol;
extern char ignosyn[] ;
extern char ignosel[] ;

#define NUM_CHTA 6
extern int chtnxalph, *chtcpoint, *chtnpoint ;
Global int *(chtatab[NUM_CHTA]);

#define CF_END      -2
#define CF_INVALID  -1
#define CF_UNDEF    0
#define CF_CHAR     1
#define CF_NUMBER   2

#include "asm/typetags.h"

Global path_t * as1600_search_path;
