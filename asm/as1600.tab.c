
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "asm/as1600_real.y"


#if 0
#   define YYDEBUG 1
#   define YYERROR_VERBOSE 1
#   define YYTOKEN_TABLE   1
    int yydebug = 1;
#endif

/*  NOTICE:  This code is based on the Public Domain AS2650.Y that comes
 *           with the Frankenstein Assembler, by Mark Zenier.  The changes
 *           that I, Joseph Zbiciak, have made are being placed under GPL.
 *           See GPL notice immediately below. 
 */

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


/*
HEADER:     ;
TITLE:      Frankenstein Cross Assemblers;
VERSION:    2.0;
DESCRIPTION: "  Reconfigurable Cross-assembler producing Intel (TM)
                Hex format object records.  ";
KEYWORDS:   cross-assemblers, 1600, 1805, 2650, 6301, 6502, 6805, 6809, 
            6811, tms7000, 8048, 8051, 8096, z8, z80;
SYSTEM:     UNIX, MS-Dos ;
FILENAME:   as1600.y;
WARNINGS:   "This software is in the public domain.  
             Any prior copyright claims are relinquished.  

             This software is distributed with no warranty whatever.  
             The author takes no responsibility for the consequences 
             of its use.

             Yacc (or Bison) required to compile."  ;
SEE-ALSO:   as1600.ps, frasmain.c;  
AUTHORS:    Mark Zenier; Joe Zbiciak 
COMPILERS:  GCC
*/

/* 1600 instruction generation file, GI standard syntax */
/* September 25, 1999 */

/*
    description frame work parser description for framework cross assemblers
    history     February 2, 1988
                September 11, 1990 - merge table definition
                September 12, 1990 - short file names
                September 14, 1990 - short variable names
                September 17, 1990 - use yylex as external
*/

/* ======================================================================== *\

The CP-1610 supports the following basic opcode formats:

 ---------------------------------------  -------  --------------------------
  Format                                   Words    Description
 ---------------------------------------  -------  --------------------------
  0000 000 0oo                               1      Implied 1-op insns
  0000 000 100  bbppppppii  pppppppppp       3      Jump insns
  0000 000 1oo                               1      Implied 1-op insns
  0000 ooo ddd                               1      1-op insns, comb src/dst
  0000 110 0dd                               1      GSWD
  0000 110 1om                               1      NOP(2), SIN(2)
  0001 ooo mrr                               1      Rotate/Shift insns
  0ooo sss ddd                               1      2-op arith, reg->reg
  1000 zxc ccc  pppppppppp                   2      Branch insns
  1ooo 000 ddd  pppppppppp                   2      2-op arith, direct, reg
  1ooo mmm ddd                               1*     2-op arith, ind., reg
  1ooo 111 ddd  iiiiiiiiii                   2*     2-op arith, immed., reg
 ---------------------------------------  -------  --------------------------


 -----
  Key
 -----

  oo    -- Opcode field (dependent on format)
  sss   -- Source register,      R0 ... R7 (binary encoding)
  ddd   -- Destination register, R0 ... R7 (binary encoding)
  0dd   -- Destination register, R0 ... R3
  cccc  -- Condition codes (branches)
  x     -- External branch condition (0 == internal, 1 == examine BEXT)
  z     -- Branch displacement direction (1 == negative)
  m     -- Shift amount (0 == shift by 1, 1 == shift by 2)
  bb    -- Branch return register
  ii    -- Branch interrupt flag mode

 --------------------------------
  Branch Condition Codes  (cccc)
 --------------------------------
           n == 0                    n == 1
  n000  -- Always                    Never
  n001  -- Carry set/Greater than    Carry clear/Less than or equal
  n010  -- Overflow set              Overflow clear
  n011  -- Positive                  Negative
  n100  -- Equal                     Not equal
  n101  -- Less than                 Greater than or equal
  n110  -- Less than or equal        Greater than
  n111  -- Unequal sign and carry    Equal sign and carry


 -------------------------------
  Branch Return Registers  (bb)
 -------------------------------

  00   -- R4
  01   -- R5
  10   -- R6
  11   -- none (do not save return address)

 -------------------------------
  Branch Interrupt Modes   (ii)
 -------------------------------

  00   -- Do not change interrupt enable state
  01   -- Enable interrupts
  10   -- Disable interrupts
  11   -- Undefined/Reserved ?

 ------------
  SDBD notes
 ------------

  -- SDBD is supported on "immediate" and "indirect" modes only.

  -- An SDBD prefix on an immediate instruction sets the immediate constant
     to be 16 bits, stored in two adjacent 8-bit words.  The ordering is
     little-endian.

  -- An SDBD prefix on an indirect instruction causes memory to be
     accessed twice, bringing in (or out) two 8-bit words, again in
     little-endian order.  If a non-incrementing data counter is used,
     both accesses are to the same address.  Otherwise, the counter
     is post-incremented with each access.  Indirect through R6
     (stack addressing) is not allowed, although I suspect it works
     as expected (performing two accesses through R6).

 ------------------------
  General encoding notes
 ------------------------

  -- "Immediate" mode is encoded the same as "Indirect" mode, except that
     R7 is given as the indirect register.  I'm guessing R7 is implemented
     the same as R4 and R5, especially since MVOI does what you'd
     expect -- it (attempts to) write over its immediate operand!!!

  -- The PC value (in R7) used for arithmetic always points to the first
     byte after the instruction for purposes of arithmetic.  This is
     consistent with the observation that immediate mode is really
     indirect mode in disguise, with the instruction being only one word
     long initially.

  -- Several instructions are just special cases of other instructions,
     and therefore do not need special decoder treatment:

      -- TSTR Rx  -->  MOVR Rx, Rx
      -- JR Rx    -->  MOVR Rx, R7
      -- CLRR Rx  -->  XORR Rx, Rx
      -- B        -->  Branch with condition code 0000 ("Always")
      -- NOPP     -->  Branch with condition code 1000 ("Never")
      -- PSHR Rx  -->  MVO@ Rx, R6
      -- PULR Rx  -->  MVI@ R6, Rx

  -- "Direct" mode is encoded the same as "Indirect" mode, except 000
     (which corresponds to R0) is encoded in the indirect register field.
     This is why R0 cannot be used as a data counter, and why it has no
     "special use."

  -- Relative branches encode their sign bit in the opcode word, rather
     than relying on a sign-extended relative offset in their second word.
     This allows +/- 10-bit range in a 10-bit wide memory, or +/-
     16-bit range in a 16-bit memory.  To avoid redundant encoding, the
     offset is calculated slightly differently for negative vs. positive
     offset:

      -- Negative: address_of_branch + 1 - offset
      -- Positive: address_of_branch + 2 + offset

     I'm guessing it is implemented about like so in hardware:

      -- offset == pc + (offset ^ (sign ? -1 : 0))

 ---------------
  Opcode Spaces
 ---------------

  I've divided the CP-1610 opcode map into 12 different opcode
  spaces.  (I really should merge the two separate Implied 1-op
  spaces into one space.  Oh well...)  In the descriptions below,
  "n/i" means "not interruptible".  Defined flags: Sign, Zero, Carry,
  Overflow, Interrupt-enable, Double-byte-data.  Interrupt-enable and
  Double-byte-data are not user visible.

  -- Implied 1-op instructions, part A:     0000 000 0oo
     Each has a single, implied operand, if any.

         opcode   mnemonic n/i  SZCOID  description
      --   00       HLT                 Halts the CPU (until next interrupt?)
      --   01       SDBD    *        1  Set Double Byte Data
      --   10       EIS     *       1   Enable Interrupt System
      --   11       DIS     *       1   Disable Interrupt System

  -- Implied 1-op instructions, part B:     0000 000 1oo
     Each has a single, implied operand, if any.

         opcode   mnemonic n/i  SZCOID  description
      --   00       n/a                 Aliases the "Jump" opcode space
      --   01       TCI     *           Terminate Current Interrupt.
      --   10       CLRC    *           Clear carry flag
      --   11       SETC    *           Set carry flag

  -- Jump Instructions:                     0000 000 100 bbppppppii pppppppppp
     Unconditional jumps with optional return-address save and
     interrupt enable/disable.

          bb  ii   mnemonic n/i  SZCOID description
      --  11  00    J                   Jump.
      --  xx  00    JSR                 Jump.  Save return address in R4..R6
      --  11  01    JE              1   Jump and enable ints.
      --  xx  01    JSRE            1   Jump and enable ints.  Save ret addr.
      --  11  10    JD              0   Jump and disable ints
      --  xx  10    JSRD            0   Jump and disable ints.  Save ret addr.
      --  xx  11    n/a                 Invalid opcode.

  -- Register 1-op instructions             0000 ooo rrr
     Each has one register operand, encoded as 000 through 111.

         opcode   mnemonic n/i  SZCOID  description
      --   000      n/a                 Aliases "Implied", "Jump" opcode space
      --   001      INCR        XX      INCrement register
      --   010      DECR        XX      DECrement register
      --   011      COMR        XX      COMplement register (1s complement)
      --   100      NEGR        XXXX    NEGate register     (2s complement)
      --   101      ADCR        XXXX    ADd Carry to Register
      --   110      n/a                 Aliases "GSWD", "NOP/SIN" opcode space
      --   111      RSWD        XXXX    Restore Status Word from Register


  -- Get Status WorD                        0000 110 0rr
     This was given its own opcode space due to limited encoding on its
     destination register and complication with the NOP/SIN encodings.

  -- NOP/SIN                                0000 110 1om
     I don't know what the "m" bit is for.  I don't know what to do with SIN.

         opcode   mnemonic n/i  SZCOID  description
      --    0       NOP                 No operation
      --    1       SIN                 Software Interrupt (pulse PCIT pin) ?

  -- Shift/Rotate 1-op instructions         0001 ooo mrr
     These can operate only on R0...R3.  The "m" bit specifies whether the
     operation is performed once or twice.  The overflow bit is used for
     catching the second bit on the rotates/shifts that use the carry.

         opcode   mnemonic n/i  SZCOID  description
      --   000      SWAP    *   XX      Swaps bytes in word once or twice.
      --   001      SLL     *   XX      Shift Logical Left
      --   010      RLC     *   XXX2    Rotate Left through Carry/overflow
      --   011      SLLC    *   XXX2    Shift Logical Left thru Carry/overflow
      --   100      SLR     *   XX      Shift Logical Right
      --   101      SAR     *   XX      Shift Arithmetic Right
      --   110      RRC     *   XXX2    Rotate Left through Carry/overflow
      --   111      SARC    *   XXX2    Shift Arithmetic Right thru Carry/over

  -- Register/Register 2-op instructions    0ooo sss ddd
     Register to register arithmetic.  Second operand acts as src2 and dest.

         opcode   mnemonic n/i  SZCOID  description
      --   00x      n/a                 Aliases other opcode spaces
      --   010      MOVR        XX      Move register to register
      --   011      ADDR        XXXX    Add src1 to src2->dst
      --   100      SUBR        XXXX    Sub src1 from src2->dst
      --   101      CMPR        XXXX    Sub src1 from src2, don't store
      --   110      ANDR        XX      AND src1 with src2->dst
      --   111      XORR        XX      XOR src1 with src2->dst

  -- Conditional Branch instructions        1000 zxn ccc pppppppppppppppp
     The "z" bit specifies the direction for the offset.  The "x" bit
     specifies using an external branch condition instead of using flag
     bits.  Conditional brances are interruptible.  The "n" bit specifies
     branching on the opposite condition from 'ccc'.

          cond      n=0         Condition       n=1         Condition
      --  n000      B           always          NOPP        never
      --  n001      BC          C = 1           BNC         C = 0
      --  n010      BOV         O = 1           BNOV        O = 0
      --  n011      BPL         S = 0           BMI         S = 1
      --  n100      BZE/BEQ     Z = 1           BNZE/BNEQ   Z = 0
      --  n101      BLT/BNGE    S^O = 1         BGE/BNLT    S^O = 0
      --  n110      BLE/BNGT    Z|(S^O) = 1     BGT/BNLE    Z|(S^O) = 0
      --  n111      BUSC        S^C = 1         BESC        S^C = 0

  -- Direct/Register 2-op instructions      1ooo 000 rrr  pppppppppppppppp
     Direct memory to register arithmetic.  MVO uses direct address as
     a destination, all other operations use it as a source, with
     the register as the destination.

         opcode   mnemonic n/i  SZCOID  description
      --   000      n/a                 Aliases conditional branch opcodes
      --   001      MVO     *           Move register to direct address
      --   010      MVI                 Move direct address to register
      --   011      ADD         XXXX    Add src1 to src2->dst
      --   100      SUB         XXXX    Sub src1 from src2->dst
      --   101      CMP         XXXX    Sub src1 from src2, don't store
      --   110      AND         XX      AND src1 with src2->dst
      --   111      XOR         XX      XOR src1 with src2->dst


  -- Indirect/Register 2-op instructions    1ooo sss ddd
     A source of "000" is actually a Direct/Register opcode.
     A source of "111" is actually a Immediate/Register opcode.
     R4, R5 increment after each access.  If the D bit is set, two
     accesses are made through the indirect register, updating the
     address if R4 or R5.  R6 increments after writes, decrements
     before reads.

         opcode   mnemonic n/i  SZCOID  description
      --   000      n/a                 Aliases conditional branch opcodes
      --   001      MVO@    *           Move register to indirect address
      --   010      MVI@                Move indirect address to register
      --   011      ADD@        XXXX    Add src1 to src2->dst
      --   100      SUB@        XXXX    Sub src1 from src2->dst
      --   101      CMP@        XXXX    Sub src1 from src2, don't store
      --   110      AND@        XX      AND src1 with src2->dst
      --   111      XOR@        XX      XOR src1 with src2->dst

  -- Immediate/Register 2-op instructions   1ooo 111 ddd  pppp
     If DBD is set, the immediate value spans two adjacent bytes, little
     endian order.  Otherwise the immediate value spans one word.  This
     instruction really looks like indirect through R7, and I suspect
     that's how the silicon implements it.

         opcode   mnemonic n/i  SZCOID  description
      --   000      n/a                 Aliases conditional branch opcodes
      --   001      MVOI    *           Move register to immediate field!
      --   010      MVII                Move immediate field to register
      --   011      ADDI        XXXX    Add src1 to src2->dst
      --   100      SUBI        XXXX    Sub src1 from src2->dst
      --   101      CMPI        XXXX    Sub src1 from src2, don't store
      --   110      ANDI        XX      AND src1 with src2->dst
      --   111      XORI        XX      XOR src1 with src2->dst

\* ======================================================================== */

#include <stdio.h>
#include <string.h>
#include "config.h"
#include "types.h"
#include "intermed.h"
#include "file/file.h"
#include "asm/frasmdat.h"
#include "asm/fragcon.h"
#include "asm/protos.h"
#include "asm/as1600.tab.h"
#include "asm/memo_string.h"

#define yylex lexintercept

#define JSR_RG    0x0001
#define SHF_RG    0x0002
#define IND_RG    0x0004
#define SDBD      0x0008

#define ST_REGREG 0x0001
#define ST_REGEXP 0x0002
#define ST_EXPREG 0x0004
#define ST_REGCEX 0x0008
#define ST_CEXREG 0x0010
#define ST_REG    0x0020
#define ST_EXP    0x0040
#define ST_IMP    0x0080
#define ST_EXPEXP 0x0100
    
/* ======================================================================== */
/*  R0 .. R7 can be used as general-purpose registers.                      */
/*  R0 .. R3 can be used for shifts and GSWD.                               */
/*  R1 .. R6 can be used for indirect addressing.                           */
/*  R4 .. R6 can be used for JSR.                                           */
/* ======================================================================== */
static int  reg_type[8] = 
{ 
    SHF_RG,
    SHF_RG | IND_RG,
    SHF_RG | IND_RG,
    SHF_RG | IND_RG,
    JSR_RG | IND_RG,
    JSR_RG | IND_RG,
    JSR_RG | IND_RG,
    0
};
    
/* ======================================================================== */
/*  BDEF outputs a number as a ROMW width word directly.  Allowed width is  */
/*       determined by argument #2 to the expression.                       */
/*  WDEF outputs a 16-bit word as a Double Byte Data.                       */
/* ======================================================================== */
static char genbdef[] = "[1=].[2#]I$[1=]x";
static char genwdef[] = "[1=].10I$[1=].FF&x[1=].8}.FF&x"; 

/*static char gensdbd[] = "0001x";*/

char ignosyn[] = "[Xinvalid syntax for instruction";
char ignosel[] = "[Xinvalid operands";

int labelloc;
static int satsub;
int ifstkpt = 0;
int fraifskip = FALSE, frarptskip = FALSE;
int frarptact = 0,     frarptcnt = -1;
int struct_locctr = -1;

#define MAX_PROC_STK (64)

static char *proc_stk[MAX_PROC_STK];
static int   proc_stk_depth = 0;

extern char *proc;
extern int   proc_len;
static const char *currmode = "";


static int sdbd = 0, is_sdbd = 0;
static int romw = 16; 
static unsigned romm = 0xFFFF;
static int first = 1;

static int fwd_sdbd = 0;

struct symel * endsymbol = SYMNULL;


#define SDBD_CHK \
    if (sdbd) { sdbd = 0; frawarn("SDBD not allowed with this instruction."); }

LOCAL void do_set_equ_(int isequ, int flags, 
                       struct symel *sym, int value, 
                       const char *equerr)
{
    if (sym->seg == SSG_UNDEF
        || (sym->seg == SSG_SET && isequ == FALSE))
    {
        sym->seg    = isequ ? SSG_EQU : SSG_SET;
        sym->value  = value;
        sym->flags |= flags;
        emit_set_equ(value);
    } else
    {
        fraerror(equerr);
    }
}

LOCAL void do_set_equ_list(int isslice,
                           int isequ,         
                           int flags, 
                           struct symel *sym, int numexpr, 
                           int firstidx,      int lastidx,
                           const char *ncerr, const char *equerr)
{
    struct symel *newsym;
    int i, idx, stp;

    for (i = 0; i < numexpr; i++)
    {
        pevalexpr(0, exprlist[i]);

        exprvals[i] = evalr[0].value;

        if (evalr[0].seg != SSG_ABS) 
            fraerror(ncerr);
    }

    if (numexpr == 1 && !isslice)
    {
        do_set_equ_(isequ, flags, sym, exprvals[0], equerr);
    } 
    else if (abs(lastidx - firstidx) + 1 != numexpr)
    {
        fraerror("Array slice length doesn't match expression list length");
    } 
    else if (sym->seg == SSG_UNDEF || sym->seg == SSG_SET)
    {
        if (sym->value < firstidx)
            sym->value = firstidx;

        if (sym->value < lastidx)
            sym->value = lastidx;

        stp = firstidx > lastidx ? -1 : 1;

        sym->seg    = SSG_SET;
        sym->flags |= SFLAG_QUIET;

        for (i = 0, idx = firstidx; i < numexpr; i++, idx += stp)
        {
            newsym         = symbentryidx(sym->symstr, LABEL, 1, idx);
            newsym->flags |= SFLAG_QUIET | SFLAG_ARRAY;

            do_set_equ_(isequ, flags, newsym, exprvals[i], equerr);
        }
    } else
    {
        fraerror("Cannot convert symbol to array");
    }
}

typedef enum { USRERR, USRWARN, USRSTAT, USRCMT } usrmsg;

LOCAL void usr_message(usrmsg type, const char *msg)
{
    char *copy = strdup((msg && *msg) ? msg : " "), *s, *ss;

    /* force all characters to be printing characters */
    for (s = copy; *s; s++)
        if (!(isprint(*s) || isspace(*s)))
            *s = '?';

    /* Print all the lines of the message, breaking at newlines */
    for (s = copy; s && *s; s = ss)
    {
        ss = strpbrk(s, "\n\r");
        
        if (ss)
            *ss++ = 0;

        switch (type)
        {
            case USRERR:   fraerror(s); break;
            case USRWARN:  frawarn (s); break;
            case USRSTAT:  puts(s);     /* continue */
            case USRCMT:   emit_comment(1, "%s", s); break;
            default:       fraerror("internal error in usr_message");
        }
    }

    free(copy);
}

LOCAL void chardef(char *sourcestr, int numexpr)
{
    int findrv, numret, *charaddr;
    char *before;

    if(chtnpoint != (int *)NULL)
    {
        for(satsub = 0; satsub < numexpr; satsub++)
        {
            before = sourcestr;

            pevalexpr(0, exprlist[satsub]);
            findrv = chtcfind(chtnpoint, &sourcestr, &charaddr, &numret);

            if(findrv == CF_END)
            {
                fraerror("more expressions than characters");
                break;
            }

            if(evalr[0].seg == SSG_ABS)
            {
                switch(findrv)
                {
                case CF_UNDEF:
                    {
                        if(evalr[0].value < 0 || evalr[0].value > 255)
                            frawarn("character translation value truncated");
                        
                        *charaddr = evalr[0].value & 0xff;
                        emit_set_equ(evalr[0].value);
                    }
                    break;

                case CF_INVALID:
                case CF_NUMBER:
                    fracherror("invalid character to define", 
                                before, sourcestr);
                    break;

                case CF_CHAR:
                    fracherror("character already defined", 
                               before, sourcestr);
                    break;
                }
            }
            else
                fraerror("noncomputable expression");
        }

        if( *sourcestr != '\0')
            fraerror("more characters than expressions");
    }
    else
        fraerror("no CHARSET statement active");
}


#define MAXTEMPSTR (16384)
static char tempstr[MAXTEMPSTR];
static int  tempstrlen = 0;




/* Line 189 of yacc.c  */
#line 695 "asm/as1600.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     REGISTER = 258,
     KOC_BDEF = 259,
     KOC_ELSE = 260,
     KOC_END = 261,
     KOC_ENDI = 262,
     KOC_EQU = 263,
     KOC_IF = 264,
     KOC_INCLUDE = 265,
     KOC_ORG = 266,
     KOC_RESM = 267,
     KOC_SDEF = 268,
     KOC_SET = 269,
     KOC_WDEF = 270,
     KOC_CHSET = 271,
     KOC_CHDEF = 272,
     KOC_CHUSE = 273,
     KOC_opcode = 274,
     KOC_opcode_i = 275,
     KOC_relbr = 276,
     KOC_relbr_x = 277,
     KOC_SDBD = 278,
     KOC_ROMW = 279,
     KOC_PROC = 280,
     KOC_ENDP = 281,
     KOC_STRUCT = 282,
     KOC_ENDS = 283,
     KOC_MEMATTR = 284,
     KOC_DDEF = 285,
     KOC_RPT = 286,
     KOC_ENDR = 287,
     KOC_USRERR = 288,
     KOC_LIST = 289,
     KOC_QEQU = 290,
     KOC_QSET = 291,
     KOC_MACERR = 292,
     KOC_BRKIF = 293,
     KOC_CMSG = 294,
     KOC_SMSG = 295,
     KOC_WMSG = 296,
     CONSTANT = 297,
     EOL = 298,
     KEOP_AND = 299,
     KEOP_DEFINED = 300,
     KEOP_EQ = 301,
     KEOP_GE = 302,
     KEOP_GT = 303,
     KEOP_HIGH = 304,
     KEOP_LE = 305,
     KEOP_LOW = 306,
     KEOP_LT = 307,
     KEOP_MOD = 308,
     KEOP_MUN = 309,
     KEOP_NE = 310,
     KEOP_NOT = 311,
     KEOP_OR = 312,
     KEOP_SHL = 313,
     KEOP_SHR = 314,
     KEOP_SHRU = 315,
     KEOP_XOR = 316,
     KEOP_locctr = 317,
     KEOP_STRLEN = 318,
     KEOP_ASC = 319,
     LABEL = 320,
     STRING = 321,
     QCHAR = 322,
     SYMBOL = 323,
     FEATURE = 324,
     KTK_invalid = 325
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 621 "asm/as1600_real.y"

    int             intv;
    int             longv;
    char            *strng;
    struct symel    *symb;
    struct slidx    slidx;



/* Line 214 of yacc.c  */
#line 811 "asm/as1600.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 823 "asm/as1600.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  93
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1461

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  84
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  15
/* YYNRULES -- Number of rules.  */
#define YYNRULES  129
/* YYNRULES -- Number of states.  */
#define YYNSTATES  262

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   325

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,    80,    77,    81,     2,     2,
      78,    79,    73,    71,    75,    72,     2,    74,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    76,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    82,     2,    83,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     6,     8,    11,    13,    16,    19,    22,
      25,    28,    31,    33,    35,    38,    42,    46,    50,    54,
      58,    62,    66,    70,    73,    75,    78,    81,    85,    88,
      90,    92,    94,    98,   101,   107,   112,   120,   127,   134,
     137,   139,   142,   147,   152,   154,   156,   159,   161,   164,
     167,   170,   173,   176,   178,   181,   183,   186,   190,   194,
     196,   198,   202,   204,   206,   211,   217,   223,   229,   232,
     234,   238,   240,   243,   248,   250,   253,   258,   260,   263,
     268,   274,   279,   285,   288,   293,   298,   301,   304,   307,
     310,   313,   317,   321,   325,   329,   333,   337,   341,   345,
     349,   353,   357,   361,   365,   369,   373,   377,   381,   384,
     386,   389,   391,   393,   395,   397,   402,   407,   414,   421,
     425,   427,   432,   434,   439,   446,   451,   458,   465,   470
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      85,     0,    -1,    85,    86,    -1,    86,    -1,    87,    43,
      -1,    43,    -1,     1,    43,    -1,    90,     6,    -1,    33,
      93,    -1,    41,    93,    -1,    40,    93,    -1,    39,    93,
      -1,    37,    -1,     6,    -1,    10,    66,    -1,    90,     8,
      92,    -1,    90,    35,    92,    -1,    90,    14,    92,    -1,
      90,    36,    92,    -1,    91,     8,    92,    -1,    91,    35,
      92,    -1,    91,    14,    92,    -1,    91,    36,    92,    -1,
      31,    94,    -1,    32,    -1,    38,    94,    -1,    34,    66,
      -1,    90,    34,    66,    -1,     9,    94,    -1,     9,    -1,
       5,    -1,     7,    -1,    90,    11,    94,    -1,    11,    94,
      -1,    90,    11,    94,    75,    94,    -1,    11,    94,    75,
      94,    -1,    90,    11,    94,    75,    94,    75,    66,    -1,
      11,    94,    75,    94,    75,    66,    -1,    29,    94,    75,
      94,    75,    66,    -1,    90,    16,    -1,    18,    -1,    18,
      94,    -1,    17,    93,    75,    92,    -1,    17,    67,    75,
      92,    -1,    90,    -1,    88,    -1,    90,    89,    -1,    89,
      -1,     4,    92,    -1,    30,    92,    -1,    13,    92,    -1,
      15,    92,    -1,    12,    94,    -1,    95,    -1,    95,    76,
      -1,    98,    -1,    98,    76,    -1,    92,    75,    94,    -1,
      92,    75,    93,    -1,    94,    -1,    93,    -1,    92,    75,
      97,    -1,    97,    -1,    66,    -1,    77,    78,    92,    79,
      -1,    77,    80,    78,    94,    79,    -1,    77,    77,    78,
      94,    79,    -1,    77,    81,    78,    94,    79,    -1,    90,
      25,    -1,    26,    -1,    90,    27,    94,    -1,    28,    -1,
      24,    94,    -1,    24,    94,    75,    94,    -1,    23,    -1,
      21,    94,    -1,    22,    94,    75,    94,    -1,    19,    -1,
      19,    94,    -1,    19,     3,    75,    94,    -1,    19,     3,
      75,    80,    94,    -1,    19,    94,    75,     3,    -1,    19,
      80,    94,    75,     3,    -1,    19,     3,    -1,    19,     3,
      75,     3,    -1,    20,     3,    75,     3,    -1,    71,    94,
      -1,    72,    94,    -1,    56,    94,    -1,    49,    94,    -1,
      51,    94,    -1,    94,    73,    94,    -1,    94,    74,    94,
      -1,    94,    71,    94,    -1,    94,    72,    94,    -1,    94,
      53,    94,    -1,    94,    58,    94,    -1,    94,    59,    94,
      -1,    94,    60,    94,    -1,    94,    48,    94,    -1,    94,
      47,    94,    -1,    94,    52,    94,    -1,    94,    50,    94,
      -1,    94,    55,    94,    -1,    94,    46,    94,    -1,    94,
      44,    94,    -1,    94,    57,    94,    -1,    94,    61,    94,
      -1,    45,    96,    -1,    96,    -1,    45,    69,    -1,    69,
      -1,    77,    -1,    42,    -1,    67,    -1,    63,    78,    93,
      79,    -1,    63,    78,    67,    79,    -1,    64,    78,    93,
      75,    94,    79,    -1,    64,    78,    67,    75,    94,    79,
      -1,    78,    94,    79,    -1,    65,    -1,    95,    82,    94,
      83,    -1,    68,    -1,    96,    82,    94,    83,    -1,    96,
      82,    94,    75,    94,    83,    -1,    97,    82,    94,    83,
      -1,    97,    82,    94,    75,    94,    83,    -1,    95,    82,
      94,    75,    94,    83,    -1,    98,    82,    94,    83,    -1,
      98,    82,    94,    75,    94,    83,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   720,   720,   721,   724,   729,   730,   738,   743,   747,
     751,   755,   759,   763,   767,   810,   816,   822,   828,   834,
     841,   848,   855,   862,   881,   892,   907,   919,   939,   971,
     992,  1010,  1029,  1052,  1068,  1092,  1109,  1136,  1155,  1170,
    1186,  1191,  1216,  1220,  1225,  1237,  1240,  1261,  1272,  1283,
    1295,  1306,  1315,  1332,  1333,  1335,  1336,  1339,  1344,  1357,
    1363,  1377,  1382,  1409,  1410,  1442,  1465,  1492,  1543,  1577,
    1598,  1625,  1642,  1667,  1704,  1718,  1734,  1759,  1770,  1783,
    1798,  1813,  1828,  1863,  1875,  1888,  1901,  1905,  1909,  1913,
    1917,  1921,  1925,  1929,  1933,  1937,  1941,  1945,  1949,  1953,
    1957,  1961,  1965,  1969,  1973,  1977,  1981,  1985,  1989,  1993,
    1997,  2001,  2005,  2009,  2013,  2019,  2032,  2036,  2061,  2077,
    2088,  2089,  2109,  2110,  2131,  2157,  2161,  2168,  2183,  2187
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "REGISTER", "KOC_BDEF", "KOC_ELSE",
  "KOC_END", "KOC_ENDI", "KOC_EQU", "KOC_IF", "KOC_INCLUDE", "KOC_ORG",
  "KOC_RESM", "KOC_SDEF", "KOC_SET", "KOC_WDEF", "KOC_CHSET", "KOC_CHDEF",
  "KOC_CHUSE", "KOC_opcode", "KOC_opcode_i", "KOC_relbr", "KOC_relbr_x",
  "KOC_SDBD", "KOC_ROMW", "KOC_PROC", "KOC_ENDP", "KOC_STRUCT", "KOC_ENDS",
  "KOC_MEMATTR", "KOC_DDEF", "KOC_RPT", "KOC_ENDR", "KOC_USRERR",
  "KOC_LIST", "KOC_QEQU", "KOC_QSET", "KOC_MACERR", "KOC_BRKIF",
  "KOC_CMSG", "KOC_SMSG", "KOC_WMSG", "CONSTANT", "EOL", "KEOP_AND",
  "KEOP_DEFINED", "KEOP_EQ", "KEOP_GE", "KEOP_GT", "KEOP_HIGH", "KEOP_LE",
  "KEOP_LOW", "KEOP_LT", "KEOP_MOD", "KEOP_MUN", "KEOP_NE", "KEOP_NOT",
  "KEOP_OR", "KEOP_SHL", "KEOP_SHR", "KEOP_SHRU", "KEOP_XOR",
  "KEOP_locctr", "KEOP_STRLEN", "KEOP_ASC", "LABEL", "STRING", "QCHAR",
  "SYMBOL", "FEATURE", "KTK_invalid", "'+'", "'-'", "'*'", "'/'", "','",
  "':'", "'$'", "'('", "')'", "'#'", "'%'", "'['", "']'", "$accept",
  "file", "allline", "line", "labeledline", "genline", "labelcolon",
  "labelslicecolon", "exprlist", "string", "expr", "label", "symbol",
  "symslice", "labelslice", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,    43,    45,    42,    47,    44,    58,    36,    40,    41,
      35,    37,    91,    93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    84,    85,    85,    86,    86,    86,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    88,    88,    89,    89,
      89,    89,    89,    90,    90,    91,    91,    92,    92,    92,
      92,    92,    92,    93,    93,    93,    93,    93,    89,    89,
      89,    89,    89,    89,    89,    89,    89,    89,    89,    89,
      89,    89,    89,    89,    89,    89,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      95,    95,    96,    96,    97,    97,    97,    98,    98,    98
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     1,     2,     1,     2,     2,     2,     2,
       2,     2,     1,     1,     2,     3,     3,     3,     3,     3,
       3,     3,     3,     2,     1,     2,     2,     3,     2,     1,
       1,     1,     3,     2,     5,     4,     7,     6,     6,     2,
       1,     2,     4,     4,     1,     1,     2,     1,     2,     2,
       2,     2,     2,     1,     2,     1,     2,     3,     3,     1,
       1,     3,     1,     1,     4,     5,     5,     5,     2,     1,
       3,     1,     2,     4,     1,     2,     4,     1,     2,     4,
       5,     4,     5,     2,     4,     4,     2,     2,     2,     2,
       2,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     2,     1,
       2,     1,     1,     1,     1,     4,     4,     6,     6,     3,
       1,     4,     1,     4,     6,     4,     6,     6,     4,     6
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,    30,    13,    31,    29,     0,     0,     0,
       0,     0,     0,    40,    77,     0,     0,     0,    74,     0,
      69,    71,     0,     0,     0,    24,     0,     0,    12,     0,
       0,     0,     0,     5,   120,     0,     3,     0,    45,    47,
      44,     0,    53,    55,     6,   113,     0,     0,     0,     0,
       0,     0,    63,   114,   122,   111,     0,     0,   112,     0,
      48,    60,    59,   109,    62,   112,    28,   109,    14,    33,
      52,    50,    51,     0,     0,     0,    41,    83,     0,    78,
       0,    75,     0,    72,     0,    49,    23,     8,    26,    25,
      11,    10,     9,     1,     2,     4,     7,     0,     0,     0,
      39,    68,     0,     0,     0,     0,    46,     0,    53,     0,
       0,     0,     0,    54,     0,    56,     0,   110,   108,    89,
      90,    88,     0,     0,    86,    87,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    15,    32,    17,    70,    27,    16,    18,     0,
      19,    21,    20,    22,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,    58,    57,    61,   105,   104,
     100,    99,   102,   101,    95,   103,   106,    96,    97,    98,
     107,    93,    94,    91,    92,     0,     0,     0,    35,    43,
      42,    84,     0,    79,     0,    81,    85,    76,    73,     0,
       0,     0,     0,   121,     0,   128,   116,   115,     0,     0,
       0,    64,     0,     0,     0,   123,     0,   125,     0,    80,
      82,     0,    34,     0,     0,     0,     0,    66,    65,    67,
       0,     0,    37,    38,     0,   127,   129,   118,   117,   124,
     126,    36
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    35,    36,    37,    38,    39,    40,    41,    60,    61,
      62,    42,    67,    64,    43
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -71
static const yytype_int16 yypact[] =
{
     389,   -26,   708,   -71,   -71,   -71,   739,   -62,   739,   739,
     708,   708,   -41,   739,   218,    19,   739,   739,   -71,   739,
     -71,   -71,   739,   708,   739,   -71,   -18,   -52,   -71,   739,
     -18,   -18,   -18,   -71,   -71,   340,   -71,    -4,   -71,   -71,
      54,    -2,   -27,     9,   -71,   -71,    -5,   739,   739,   739,
     -46,   -38,   -71,   -71,   -71,   -71,   739,   739,    20,   739,
     -23,   -71,  1298,   -28,   -21,   -71,  1298,     1,   -71,   978,
    1298,   -23,   -23,   -19,    20,    24,  1298,    27,   739,  1010,
      28,  1298,  1042,  1074,  1106,   -23,  1298,   -71,   -71,  1298,
     -71,   -71,   -71,   -71,   -71,   -71,   -71,   708,   739,   708,
     -71,   -71,   739,     5,   708,   708,   -71,    10,    11,   708,
     708,   708,   708,   -71,   739,   -71,   739,   -71,     1,  1298,
    1298,  1358,   -39,   -20,   -71,   -71,     8,   708,    26,    31,
     774,   708,   739,   739,   739,   739,   739,   739,   739,   739,
     739,   739,   739,   739,   739,   739,   739,   739,   739,   739,
     739,   739,   739,   708,   708,   259,  1138,   102,   103,   739,
     739,   739,   -23,  1170,   -23,  1298,   -71,   -23,   -23,   739,
     -23,   -23,   -23,   -23,   387,   419,    34,    35,    40,    41,
     739,   -70,   739,   739,   -71,   -71,  1298,   -21,  1358,  1387,
    1387,  1387,  1387,  1387,   -71,  1387,  1329,   -71,   -71,   -71,
    1329,   -29,   -29,   -71,   -71,   451,   483,   515,  1202,   -23,
     -23,   -71,   739,  1298,   114,   -71,   -71,  1298,  1298,  1234,
     739,   547,   739,   -71,   739,   -71,   -71,   -71,   739,   739,
     808,   -71,   842,   876,   739,   -71,   739,   -71,    52,  1298,
     -71,    55,  1266,   579,   611,   910,   944,   -71,   -71,   -71,
     643,   675,   -71,   -71,    56,   -71,   -71,   -71,   -71,   -71,
     -71,   -71
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -71,   -71,    88,   -71,   -71,   107,   110,   -71,    84,   -11,
      -6,   111,   146,    -7,   -71
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_int16 yytable[] =
{
      66,    75,    69,    70,    68,   131,   109,    76,    79,   231,
      81,    82,   110,    83,    88,    87,    84,    44,    86,    90,
      91,    92,    80,    89,   138,    52,    73,    52,   176,   141,
     142,   143,   122,   111,   112,   101,    74,   102,    74,    95,
     123,   119,   120,   121,   147,   148,    52,   178,    52,   113,
     124,   125,   131,   130,   149,   114,   153,    74,     2,    74,
      96,   150,    97,    54,   117,    98,     9,    10,    99,    11,
     100,   166,   156,    14,    15,    16,    17,    18,    19,   101,
      20,   102,    21,   151,    23,   115,   180,   113,   103,   104,
     105,   116,   163,   169,    71,    72,   165,   126,   127,   154,
     128,   129,   155,   158,   182,   215,   216,    85,   174,   183,
     175,   177,   179,   226,   227,   228,   229,   240,   252,    34,
     185,   253,   261,    94,   187,   186,   188,   189,   190,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,   201,
     202,   203,   204,   205,   206,   207,   208,   106,    63,   213,
     107,   108,     0,   217,   218,   219,    63,    63,     0,     0,
       0,     0,     0,   221,     0,     0,     0,     0,     0,    63,
       0,     0,     0,     0,   230,     0,   232,   233,     0,     0,
       0,   162,     0,   164,     0,     0,     0,     0,   167,   168,
       0,     0,   118,   170,   171,   172,   173,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   239,     0,     0,     0,
       0,   181,     0,     0,   242,     0,   243,     0,   244,     0,
       0,    77,   245,   246,     0,     0,     0,     0,   250,     0,
     251,     0,     0,     0,     0,     0,     0,   209,   210,     0,
       0,     0,     0,    63,     0,    63,     0,     0,     0,     0,
      63,    63,     0,     0,     0,    63,    63,    63,    63,     0,
      45,     0,   211,    46,     0,     0,     0,    47,     0,    48,
       0,     0,     0,    63,    49,     0,     0,    63,     0,     0,
       0,    50,    51,     0,     0,    53,    54,    55,     0,    56,
      57,     0,     0,     0,     0,    65,    59,     0,    78,    63,
      63,    45,     0,     0,    46,     0,     0,     0,    47,     0,
      48,     0,     0,     0,     0,    49,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,    53,    54,    55,     0,
      56,    57,     0,     0,     0,     0,    65,    59,     0,   212,
      93,     1,     0,     0,     2,     3,     4,     5,     0,     6,
       7,     8,     9,    10,     0,    11,     0,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,     0,     0,    28,    29,    30,
      31,    32,     0,    33,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     2,     3,     4,     5,     0,     6,     7,
       8,     9,    10,     0,    11,    34,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,     0,     0,    28,    29,    30,    31,
      32,   132,    33,   133,   134,   135,     0,   136,     0,   137,
     138,     0,   139,     0,   140,   141,   142,   143,   144,     0,
       0,     0,     0,     0,    34,     0,     0,     0,   145,   146,
     147,   148,   222,   132,     0,   133,   134,   135,     0,   136,
     223,   137,   138,     0,   139,     0,   140,   141,   142,   143,
     144,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     145,   146,   147,   148,   224,   132,     0,   133,   134,   135,
       0,   136,   225,   137,   138,     0,   139,     0,   140,   141,
     142,   143,   144,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   145,   146,   147,   148,   234,   132,     0,   133,
     134,   135,     0,   136,   235,   137,   138,     0,   139,     0,
     140,   141,   142,   143,   144,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   145,   146,   147,   148,   236,   132,
       0,   133,   134,   135,     0,   136,   237,   137,   138,     0,
     139,     0,   140,   141,   142,   143,   144,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   145,   146,   147,   148,
       0,   132,     0,   133,   134,   135,     0,   136,   235,   137,
     138,     0,   139,     0,   140,   141,   142,   143,   144,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   145,   146,
     147,   148,     0,   132,     0,   133,   134,   135,     0,   136,
     223,   137,   138,     0,   139,     0,   140,   141,   142,   143,
     144,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     145,   146,   147,   148,     0,   132,     0,   133,   134,   135,
       0,   136,   255,   137,   138,     0,   139,     0,   140,   141,
     142,   143,   144,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   145,   146,   147,   148,     0,   132,     0,   133,
     134,   135,     0,   136,   256,   137,   138,     0,   139,     0,
     140,   141,   142,   143,   144,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   145,   146,   147,   148,     0,   132,
       0,   133,   134,   135,     0,   136,   259,   137,   138,     0,
     139,     0,   140,   141,   142,   143,   144,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   145,   146,   147,   148,
      45,     0,     0,    46,     0,     0,     0,    47,   260,    48,
       0,     0,     0,     0,    49,     0,     0,     0,     0,     0,
       0,    50,    51,     0,    52,    53,    54,    55,     0,    56,
      57,    45,     0,     0,    46,    58,    59,     0,    47,     0,
      48,     0,     0,     0,     0,    49,     0,     0,     0,     0,
       0,     0,    50,    51,     0,     0,    53,    54,    55,     0,
      56,    57,     0,     0,     0,     0,    65,    59,   132,     0,
     133,   134,   135,     0,   136,     0,   137,   138,     0,   139,
       0,   140,   141,   142,   143,   144,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   145,   146,   147,   148,     0,
       0,     0,   132,   184,   133,   134,   135,     0,   136,     0,
     137,   138,     0,   139,     0,   140,   141,   142,   143,   144,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   145,
     146,   147,   148,     0,     0,     0,   132,   247,   133,   134,
     135,     0,   136,     0,   137,   138,     0,   139,     0,   140,
     141,   142,   143,   144,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   145,   146,   147,   148,     0,     0,     0,
     132,   248,   133,   134,   135,     0,   136,     0,   137,   138,
       0,   139,     0,   140,   141,   142,   143,   144,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   145,   146,   147,
     148,     0,     0,     0,   132,   249,   133,   134,   135,     0,
     136,     0,   137,   138,     0,   139,     0,   140,   141,   142,
     143,   144,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   145,   146,   147,   148,     0,     0,     0,   132,   257,
     133,   134,   135,     0,   136,     0,   137,   138,     0,   139,
       0,   140,   141,   142,   143,   144,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   145,   146,   147,   148,     0,
       0,     0,   132,   258,   133,   134,   135,     0,   136,     0,
     137,   138,     0,   139,     0,   140,   141,   142,   143,   144,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   145,
     146,   147,   148,   152,   132,     0,   133,   134,   135,     0,
     136,     0,   137,   138,     0,   139,     0,   140,   141,   142,
     143,   144,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   145,   146,   147,   148,   157,   132,     0,   133,   134,
     135,     0,   136,     0,   137,   138,     0,   139,     0,   140,
     141,   142,   143,   144,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   145,   146,   147,   148,   159,   132,     0,
     133,   134,   135,     0,   136,     0,   137,   138,     0,   139,
       0,   140,   141,   142,   143,   144,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   145,   146,   147,   148,   160,
     132,     0,   133,   134,   135,     0,   136,     0,   137,   138,
       0,   139,     0,   140,   141,   142,   143,   144,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   145,   146,   147,
     148,   161,   132,     0,   133,   134,   135,     0,   136,     0,
     137,   138,     0,   139,     0,   140,   141,   142,   143,   144,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   145,
     146,   147,   148,   214,   132,     0,   133,   134,   135,     0,
     136,     0,   137,   138,     0,   139,     0,   140,   141,   142,
     143,   144,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   145,   146,   147,   148,   220,   132,     0,   133,   134,
     135,     0,   136,     0,   137,   138,     0,   139,     0,   140,
     141,   142,   143,   144,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   145,   146,   147,   148,   238,   132,     0,
     133,   134,   135,     0,   136,     0,   137,   138,     0,   139,
       0,   140,   141,   142,   143,   144,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   145,   146,   147,   148,   241,
     132,     0,   133,   134,   135,     0,   136,     0,   137,   138,
       0,   139,     0,   140,   141,   142,   143,   144,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   145,   146,   147,
     148,   254,   132,     0,   133,   134,   135,     0,   136,     0,
     137,   138,     0,   139,     0,   140,   141,   142,   143,   144,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   145,
     146,   147,   148,   132,     0,   133,   134,   135,     0,   136,
       0,   137,   138,     0,   139,     0,     0,   141,   142,   143,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     145,   146,   147,   148,   133,   134,   135,     0,   136,     0,
     137,   138,     0,   139,     0,     0,   141,   142,   143,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   145,
     146,   147,   148,    -1,    -1,    -1,     0,    -1,     0,    -1,
     138,     0,    -1,     0,     0,   141,   142,   143,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   145,   146,
     147,   148
};

static const yytype_int16 yycheck[] =
{
       6,    12,     8,     9,    66,    75,     8,    13,    14,    79,
      16,    17,    14,    19,    66,    26,    22,    43,    24,    30,
      31,    32,     3,    29,    53,    66,    67,    66,    67,    58,
      59,    60,    78,    35,    36,    25,    77,    27,    77,    43,
      78,    47,    48,    49,    73,    74,    66,    67,    66,    76,
      56,    57,    75,    59,    82,    82,    75,    77,     4,    77,
       6,    82,     8,    68,    69,    11,    12,    13,    14,    15,
      16,    66,    78,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    82,    30,    76,    78,    76,    34,    35,
      36,    82,    98,    82,    10,    11,   102,    77,    78,    75,
      80,    81,    75,    75,    78,     3,     3,    23,   114,    78,
     116,   122,   123,    79,    79,    75,    75,     3,    66,    65,
     131,    66,    66,    35,   131,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,    40,     2,   155,
      40,    40,    -1,   159,   160,   161,    10,    11,    -1,    -1,
      -1,    -1,    -1,   169,    -1,    -1,    -1,    -1,    -1,    23,
      -1,    -1,    -1,    -1,   180,    -1,   182,   183,    -1,    -1,
      -1,    97,    -1,    99,    -1,    -1,    -1,    -1,   104,   105,
      -1,    -1,    46,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   212,    -1,    -1,    -1,
      -1,   127,    -1,    -1,   220,    -1,   222,    -1,   224,    -1,
      -1,     3,   228,   229,    -1,    -1,    -1,    -1,   234,    -1,
     236,    -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,
      -1,    -1,    -1,    97,    -1,    99,    -1,    -1,    -1,    -1,
     104,   105,    -1,    -1,    -1,   109,   110,   111,   112,    -1,
      42,    -1,     3,    45,    -1,    -1,    -1,    49,    -1,    51,
      -1,    -1,    -1,   127,    56,    -1,    -1,   131,    -1,    -1,
      -1,    63,    64,    -1,    -1,    67,    68,    69,    -1,    71,
      72,    -1,    -1,    -1,    -1,    77,    78,    -1,    80,   153,
     154,    42,    -1,    -1,    45,    -1,    -1,    -1,    49,    -1,
      51,    -1,    -1,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    63,    64,    -1,    -1,    67,    68,    69,    -1,
      71,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,    80,
       0,     1,    -1,    -1,     4,     5,     6,     7,    -1,     9,
      10,    11,    12,    13,    -1,    15,    -1,    17,    18,    19,
      20,    21,    22,    23,    24,    -1,    26,    -1,    28,    29,
      30,    31,    32,    33,    34,    -1,    -1,    37,    38,    39,
      40,    41,    -1,    43,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,     4,     5,     6,     7,    -1,     9,    10,
      11,    12,    13,    -1,    15,    65,    17,    18,    19,    20,
      21,    22,    23,    24,    -1,    26,    -1,    28,    29,    30,
      31,    32,    33,    34,    -1,    -1,    37,    38,    39,    40,
      41,    44,    43,    46,    47,    48,    -1,    50,    -1,    52,
      53,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,    71,    72,
      73,    74,    75,    44,    -1,    46,    47,    48,    -1,    50,
      83,    52,    53,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    74,    75,    44,    -1,    46,    47,    48,
      -1,    50,    83,    52,    53,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,    74,    75,    44,    -1,    46,
      47,    48,    -1,    50,    83,    52,    53,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,    74,    75,    44,
      -1,    46,    47,    48,    -1,    50,    83,    52,    53,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,
      -1,    44,    -1,    46,    47,    48,    -1,    50,    83,    52,
      53,    -1,    55,    -1,    57,    58,    59,    60,    61,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,    74,    -1,    44,    -1,    46,    47,    48,    -1,    50,
      83,    52,    53,    -1,    55,    -1,    57,    58,    59,    60,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    74,    -1,    44,    -1,    46,    47,    48,
      -1,    50,    83,    52,    53,    -1,    55,    -1,    57,    58,
      59,    60,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    73,    74,    -1,    44,    -1,    46,
      47,    48,    -1,    50,    83,    52,    53,    -1,    55,    -1,
      57,    58,    59,    60,    61,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    72,    73,    74,    -1,    44,
      -1,    46,    47,    48,    -1,    50,    83,    52,    53,    -1,
      55,    -1,    57,    58,    59,    60,    61,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,
      42,    -1,    -1,    45,    -1,    -1,    -1,    49,    83,    51,
      -1,    -1,    -1,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    63,    64,    -1,    66,    67,    68,    69,    -1,    71,
      72,    42,    -1,    -1,    45,    77,    78,    -1,    49,    -1,
      51,    -1,    -1,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    63,    64,    -1,    -1,    67,    68,    69,    -1,
      71,    72,    -1,    -1,    -1,    -1,    77,    78,    44,    -1,
      46,    47,    48,    -1,    50,    -1,    52,    53,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,    -1,
      -1,    -1,    44,    79,    46,    47,    48,    -1,    50,    -1,
      52,    53,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    74,    -1,    -1,    -1,    44,    79,    46,    47,
      48,    -1,    50,    -1,    52,    53,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    74,    -1,    -1,    -1,
      44,    79,    46,    47,    48,    -1,    50,    -1,    52,    53,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      74,    -1,    -1,    -1,    44,    79,    46,    47,    48,    -1,
      50,    -1,    52,    53,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    74,    -1,    -1,    -1,    44,    79,
      46,    47,    48,    -1,    50,    -1,    52,    53,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,    -1,
      -1,    -1,    44,    79,    46,    47,    48,    -1,    50,    -1,
      52,    53,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    74,    75,    44,    -1,    46,    47,    48,    -1,
      50,    -1,    52,    53,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    74,    75,    44,    -1,    46,    47,
      48,    -1,    50,    -1,    52,    53,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    74,    75,    44,    -1,
      46,    47,    48,    -1,    50,    -1,    52,    53,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,    75,
      44,    -1,    46,    47,    48,    -1,    50,    -1,    52,    53,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      74,    75,    44,    -1,    46,    47,    48,    -1,    50,    -1,
      52,    53,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    74,    75,    44,    -1,    46,    47,    48,    -1,
      50,    -1,    52,    53,    -1,    55,    -1,    57,    58,    59,
      60,    61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    73,    74,    75,    44,    -1,    46,    47,
      48,    -1,    50,    -1,    52,    53,    -1,    55,    -1,    57,
      58,    59,    60,    61,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    73,    74,    75,    44,    -1,
      46,    47,    48,    -1,    50,    -1,    52,    53,    -1,    55,
      -1,    57,    58,    59,    60,    61,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    73,    74,    75,
      44,    -1,    46,    47,    48,    -1,    50,    -1,    52,    53,
      -1,    55,    -1,    57,    58,    59,    60,    61,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    73,
      74,    75,    44,    -1,    46,    47,    48,    -1,    50,    -1,
      52,    53,    -1,    55,    -1,    57,    58,    59,    60,    61,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    74,    44,    -1,    46,    47,    48,    -1,    50,
      -1,    52,    53,    -1,    55,    -1,    -1,    58,    59,    60,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    73,    74,    46,    47,    48,    -1,    50,    -1,
      52,    53,    -1,    55,    -1,    -1,    58,    59,    60,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    73,    74,    46,    47,    48,    -1,    50,    -1,    52,
      53,    -1,    55,    -1,    -1,    58,    59,    60,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,
      73,    74
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     4,     5,     6,     7,     9,    10,    11,    12,
      13,    15,    17,    18,    19,    20,    21,    22,    23,    24,
      26,    28,    29,    30,    31,    32,    33,    34,    37,    38,
      39,    40,    41,    43,    65,    85,    86,    87,    88,    89,
      90,    91,    95,    98,    43,    42,    45,    49,    51,    56,
      63,    64,    66,    67,    68,    69,    71,    72,    77,    78,
      92,    93,    94,    96,    97,    77,    94,    96,    66,    94,
      94,    92,    92,    67,    77,    93,    94,     3,    80,    94,
       3,    94,    94,    94,    94,    92,    94,    93,    66,    94,
      93,    93,    93,     0,    86,    43,     6,     8,    11,    14,
      16,    25,    27,    34,    35,    36,    89,    90,    95,     8,
      14,    35,    36,    76,    82,    76,    82,    69,    96,    94,
      94,    94,    78,    78,    94,    94,    77,    78,    80,    81,
      94,    75,    44,    46,    47,    48,    50,    52,    53,    55,
      57,    58,    59,    60,    61,    71,    72,    73,    74,    82,
      82,    82,    75,    75,    75,    75,    94,    75,    75,    75,
      75,    75,    92,    94,    92,    94,    66,    92,    92,    82,
      92,    92,    92,    92,    94,    94,    67,    93,    67,    93,
      78,    92,    78,    78,    79,    93,    94,    97,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    94,    94,    94,    94,    92,
      92,     3,    80,    94,    75,     3,     3,    94,    94,    94,
      75,    94,    75,    83,    75,    83,    79,    79,    75,    75,
      94,    79,    94,    94,    75,    83,    75,    83,    75,    94,
       3,    75,    94,    94,    94,    94,    94,    79,    79,    79,
      94,    94,    66,    66,    75,    83,    83,    79,    79,    83,
      83,    66
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 4:

/* Line 1455 of yacc.c  */
#line 725 "asm/as1600_real.y"
    {
                clrexpr();
                tempstrlen = 0;
            ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 731 "asm/as1600_real.y"
    {
                clrexpr();
                tempstrlen = 0;
                yyerrok;
            ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 739 "asm/as1600_real.y"
    {
                endsymbol = (yyvsp[(1) - (2)].symb);
                nextreadact = Nra_end;
            ;}
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 744 "asm/as1600_real.y"
    {
                usr_message(USRERR, (yyvsp[(2) - (2)].strng));
            ;}
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 748 "asm/as1600_real.y"
    {
                usr_message(USRWARN, (yyvsp[(2) - (2)].strng));
            ;}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 752 "asm/as1600_real.y"
    {
                usr_message(USRSTAT, (yyvsp[(2) - (2)].strng));
            ;}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 756 "asm/as1600_real.y"
    {
                usr_message(USRCMT, (yyvsp[(2) - (2)].strng));
            ;}
    break;

  case 12:

/* Line 1455 of yacc.c  */
#line 760 "asm/as1600_real.y"
    {
                fraerror("Unexpected MACRO or ENDM directive");
            ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 764 "asm/as1600_real.y"
    {
                nextreadact = Nra_end;
            ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 768 "asm/as1600_real.y"
    {
                if(frarptact)
                {
                    fraerror("INCLUDE not permitted inside REPEAT");
                } else if(nextfstk >= FILESTKDPTH)
                {
                    fraerror("include file nesting limit exceeded");
                }
                else
                {
                    infilestk[nextfstk].line = 0;
                    infilestk[nextfstk].fnm  = memoize_string((yyvsp[(2) - (2)].strng));
                    if( (infilestk[nextfstk].fpt = 
                        path_fopen(as1600_search_path,(yyvsp[(2) - (2)].strng),"r")) ==(FILE *)NULL )
                    {
                        static char *incl_file = NULL;
                        static int   incl_file_size = 0;
                        int          incl_file_len  = strlen((yyvsp[(2) - (2)].strng)) + 80;
                        if (incl_file_size < incl_file_len)
                        {
                            incl_file_size = incl_file_len << 1;
                            if (incl_file) free(incl_file);
                            incl_file      = (char *)malloc(incl_file_size);
                            if (!incl_file)
                                incl_file_size = 0;
                        }
                        if (incl_file_size == 0)
                            fraerror("cannot open include file");
                        else
                        {
                            sprintf(incl_file, "cannot open include file "
                                               "\"%s\"", (yyvsp[(2) - (2)].strng));
                            
                            fraerror(incl_file);
                        }
                    }
                    else
                    {
                        nextreadact = Nra_new;
                    }
                }
            ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 811 "asm/as1600_real.y"
    {
                do_set_equ_list(FALSE, TRUE, 0, (yyvsp[(1) - (3)].symb), (yyvsp[(3) - (3)].intv), 0, (yyvsp[(3) - (3)].intv) - 1,
                               "noncomputable expression for EQU",
                               "cannot change symbol value with EQU");
            ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 817 "asm/as1600_real.y"
    {
                do_set_equ_list(FALSE, TRUE, SFLAG_QUIET, (yyvsp[(1) - (3)].symb), (yyvsp[(3) - (3)].intv), 0, (yyvsp[(3) - (3)].intv) - 1,
                               "noncomputable expression for QEQU",
                               "cannot change symbol value with QEQU");
            ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 823 "asm/as1600_real.y"
    {
                do_set_equ_list(FALSE, FALSE, 0, (yyvsp[(1) - (3)].symb), (yyvsp[(3) - (3)].intv), 0, (yyvsp[(3) - (3)].intv) - 1,
                               "noncomputable expression for SET",
                               "cannot change symbol value with SET");
            ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 829 "asm/as1600_real.y"
    {
                do_set_equ_list(FALSE, FALSE, SFLAG_QUIET, (yyvsp[(1) - (3)].symb), (yyvsp[(3) - (3)].intv), 0, (yyvsp[(3) - (3)].intv) - 1,
                               "noncomputable expression for QSET",
                               "cannot change symbol value with QSET");
            ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 835 "asm/as1600_real.y"
    {
                do_set_equ_list(TRUE, TRUE, 0, 
                                (yyvsp[(1) - (3)].slidx).sym, (yyvsp[(3) - (3)].intv), (yyvsp[(1) - (3)].slidx).first, (yyvsp[(1) - (3)].slidx).last,
                               "noncomputable expression for EQU",
                               "cannot change symbol value with EQU");
            ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 842 "asm/as1600_real.y"
    {
                do_set_equ_list(TRUE, TRUE, SFLAG_QUIET, 
                                (yyvsp[(1) - (3)].slidx).sym, (yyvsp[(3) - (3)].intv), (yyvsp[(1) - (3)].slidx).first, (yyvsp[(1) - (3)].slidx).last,
                               "noncomputable expression for QEQU",
                               "cannot change symbol value with QEQU");
            ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 849 "asm/as1600_real.y"
    {
                do_set_equ_list(TRUE, FALSE, 0, 
                                (yyvsp[(1) - (3)].slidx).sym, (yyvsp[(3) - (3)].intv), (yyvsp[(1) - (3)].slidx).first, (yyvsp[(1) - (3)].slidx).last,
                               "noncomputable expression for SET",
                               "cannot change symbol value with SET");
            ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 856 "asm/as1600_real.y"
    {
                do_set_equ_list(TRUE, FALSE, SFLAG_QUIET, 
                                (yyvsp[(1) - (3)].slidx).sym, (yyvsp[(3) - (3)].intv), (yyvsp[(1) - (3)].slidx).first, (yyvsp[(1) - (3)].slidx).last,
                               "noncomputable expression for QSET",
                               "cannot change symbol value with QSET");
            ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 863 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(2) - (2)].intv));
                if (evalr[0].seg == SSG_ABS)
                {
                    if (evalr[0].value < 0)
                    {
                        fraerror("REPEAT count must be >= 0");
                        frarptpush(0);  /* treat it as a 0 count. */
                    } else
                    {
                        frarptpush(frarptskip ? 0 : evalr[0].value);
                    }
                } else
                {
                    fraerror("Computable expression required for REPEAT block");
                    frarptpush(0);  /* treat it as a 0 count. */
                }
            ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 882 "asm/as1600_real.y"
    {
                if (frarptact == 0)
                {
                    fraerror("ENDR without REPEAT");
                    frarptreset();  /* make sure repeat stack is reset. */
                } else
                {
                    frarptendr();   /* loop back to most recent REPEAT */
                }
            ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 893 "asm/as1600_real.y"
    {
                if (frarptcnt < 0)
                    fraerror("BRKIF without REPEAT");
                
                pevalexpr(0, (yyvsp[(2) - (2)].intv));
                if (evalr[0].seg == SSG_ABS)
                {
                    if (evalr[0].value != 0)
                        frarptbreak();  /* skip rest of repeat block */
                } else
                {
                    fraerror("Computable expression required for BRKIF");
                }
            ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 908 "asm/as1600_real.y"
    {
                if      (stricmp((yyvsp[(2) - (2)].strng), "ON"  )==0) emit_listing_mode(LIST_ON);
                else if (stricmp((yyvsp[(2) - (2)].strng), "OFF" )==0) emit_listing_mode(LIST_OFF);
                else if (stricmp((yyvsp[(2) - (2)].strng), "CODE")==0) emit_listing_mode(LIST_CODE);
                else if (stricmp((yyvsp[(2) - (2)].strng), "PREV")==0) emit_listing_mode(LIST_PREV);
                else 
                {
                    fraerror("LISTING must be followed by \"ON\", \"OFF\" "
                             "or \"CODE\"");
                }
            ;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 920 "asm/as1600_real.y"
    {
                if      (stricmp((yyvsp[(3) - (3)].strng), "ON"  )==0) emit_listing_mode(LIST_ON);
                else if (stricmp((yyvsp[(3) - (3)].strng), "OFF" )==0) emit_listing_mode(LIST_OFF);
                else if (stricmp((yyvsp[(3) - (3)].strng), "CODE")==0) emit_listing_mode(LIST_CODE);
                else if (stricmp((yyvsp[(3) - (3)].strng), "PREV")==0) emit_listing_mode(LIST_PREV);
                else 
                {
                    fraerror("LISTING must be followed by \"ON\", \"OFF\" "
                             "or \"CODE\"");
                }

                if((yyvsp[(1) - (3)].symb)->seg == SSG_UNDEF)
                {
                    (yyvsp[(1) - (3)].symb)->seg   = SSG_ABS;
                    (yyvsp[(1) - (3)].symb)->value = labelloc;
                }
                else
                    fraerror( "multiple definition of label");
            ;}
    break;

  case 28:

/* Line 1455 of yacc.c  */
#line 940 "asm/as1600_real.y"
    {
                if((++ifstkpt) < IFSTKDEPTH)
                {
                    pevalexpr(0, (yyvsp[(2) - (2)].intv));
                    if(evalr[0].seg == SSG_ABS)
                    {
                        if(evalr[0].value != 0)
                        {
                            elseifstk[ifstkpt] = If_Skip;
                            endifstk[ifstkpt] = If_Active;
                        }
                        else
                        {
                            fraifskip = TRUE;
                            elseifstk[ifstkpt] = If_Active;
                            endifstk[ifstkpt] = If_Active;
                        }
                    }
                    else
                    {
                        fraifskip = TRUE;
                        elseifstk[ifstkpt] = If_Active;
                        endifstk[ifstkpt] = If_Active;
                    }
                }
                else
                {
                    fraerror("IF stack overflow");
                }
            ;}
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 972 "asm/as1600_real.y"
    {
                if(fraifskip) 
                {
                    if((++ifstkpt) < IFSTKDEPTH)
                    {
                            elseifstk[ifstkpt] = If_Skip;
                            endifstk[ifstkpt] = If_Skip;
                    }
                    else
                    {
                        fraerror("IF stack overflow");
                    }
                }
                else
                {
                    yyerror("syntax error");
                    YYERROR;
                }
            ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 993 "asm/as1600_real.y"
    {
                switch(elseifstk[ifstkpt])
                {
                case If_Active:
                    fraifskip = FALSE;
                    break;
                
                case If_Skip:
                    fraifskip = TRUE;
                    break;
                
                case If_Err:
                    fraerror("ELSE with no matching if");
                    break;
                }
            ;}
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 1011 "asm/as1600_real.y"
    {
                switch(endifstk[ifstkpt])
                {
                case If_Active:
                    fraifskip = FALSE;
                    ifstkpt--;
                    break;
                
                case If_Skip:
                    fraifskip = TRUE;
                    ifstkpt--;
                    break;
                
                case If_Err:
                    fraerror("ENDI with no matching if");
                    break;
                }
            ;}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 1030 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(3) - (3)].intv));
                if(evalr[0].seg == SSG_ABS)
                {
                    locctr   = 2 * (labelloc = evalr[0].value);
                    currseg  = 0;
                    currmode = memoize_string("+R");
                    if((yyvsp[(1) - (3)].symb)->seg == SSG_UNDEF)
                    {
                        (yyvsp[(1) - (3)].symb)->seg   = SSG_ABS;
                        (yyvsp[(1) - (3)].symb)->value = labelloc;
                    }
                    else
                        fraerror( "multiple definition of label");

                    emit_set_equ(evalr[0].value);
                }
                else
                {
                    fraerror( "noncomputable expression for ORG");
                }
            ;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 1053 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(2) - (2)].intv));
                if(evalr[0].seg == SSG_ABS)
                {
                    locctr   = 2 * (labelloc = evalr[0].value);
                    currseg  = 0;
                    currmode = memoize_string("+R");
                    emit_set_equ(evalr[0].value);
                }
                else
                {
                    fraerror(
                     "noncomputable expression for ORG");
                }
            ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 1069 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(3) - (5)].intv));
                pevalexpr(1, (yyvsp[(5) - (5)].intv));
                if(evalr[0].seg == SSG_ABS && evalr[1].seg == SSG_ABS)
                {
                    locctr   = 2 * (labelloc = evalr[0].value);
                    currseg  = (evalr[1].value - labelloc);
                    currmode = memoize_string(currseg ? "" : "+R");
                    if((yyvsp[(1) - (5)].symb)->seg == SSG_UNDEF)
                    {
                        (yyvsp[(1) - (5)].symb)->seg   = SSG_ABS;
                        (yyvsp[(1) - (5)].symb)->value = labelloc;
                    }
                    else
                        fraerror( "multiple definition of label");

                    emit_set_equ(evalr[0].value);
                }
                else
                {
                    fraerror( "noncomputable expression for ORG");
                }
            ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 1093 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(2) - (4)].intv));
                pevalexpr(1, (yyvsp[(4) - (4)].intv));
                if(evalr[0].seg == SSG_ABS && evalr[1].seg == SSG_ABS)
                {
                    locctr   = 2 * (labelloc = evalr[0].value);
                    currseg  = (evalr[1].value - labelloc);
                    currmode = memoize_string(currseg ? "" : "+R");
                    emit_set_equ(evalr[0].value);
                }
                else
                {
                    fraerror(
                     "noncomputable expression for ORG");
                }
            ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 1110 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(3) - (7)].intv));
                pevalexpr(1, (yyvsp[(5) - (7)].intv));
                if(evalr[0].seg == SSG_ABS && evalr[1].seg == SSG_ABS)
                {
                    char *s = (yyvsp[(7) - (7)].strng);

                    locctr   = 2 * (labelloc = evalr[0].value);
                    currseg  = (evalr[1].value - labelloc);
                    currmode = memoize_string(s);

                    if((yyvsp[(1) - (7)].symb)->seg == SSG_UNDEF)
                    {
                        (yyvsp[(1) - (7)].symb)->seg = SSG_ABS;
                        (yyvsp[(1) - (7)].symb)->value = labelloc;
                    }
                    else
                        fraerror( "multiple definition of label");

                    emit_set_equ(evalr[0].value);
                }
                else
                {
                    fraerror( "noncomputable expression for ORG");
                }
            ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 1137 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(2) - (6)].intv));
                pevalexpr(1, (yyvsp[(4) - (6)].intv));
                if(evalr[0].seg == SSG_ABS && evalr[1].seg == SSG_ABS)
                {
                    char *s = (yyvsp[(6) - (6)].strng);

                    locctr   = 2 * (labelloc = evalr[0].value);
                    currseg  = (evalr[1].value - labelloc);
                    currmode = memoize_string(s);
                    emit_set_equ(evalr[0].value);
                }
                else
                {
                    fraerror(
                     "noncomputable expression for ORG");
                }
            ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 1156 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(2) - (6)].intv));
                pevalexpr(1, (yyvsp[(4) - (6)].intv));
                if(evalr[0].seg == SSG_ABS && evalr[1].seg == SSG_ABS)
                {
                    const char *s = memoize_string((yyvsp[(6) - (6)].strng));
                    emit_mark_with_mode(evalr[0].value, evalr[1].value, s);
                }
                else
                {
                    fraerror("noncomputable expression for MEMATTR");
                }
            ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 1171 "asm/as1600_real.y"
    {
                if((yyvsp[(1) - (2)].symb)->seg == SSG_UNDEF)
                {
                    (yyvsp[(1) - (2)].symb)->seg = SSG_EQU;
                    if( ((yyvsp[(1) - (2)].symb)->value = chtcreate()) <= 0)
                    {
                        fraerror("cannot create character translation table");
                    }
                    emit_set_equ((yyvsp[(1) - (2)].symb)->value);
                }
                else
                {
                    fraerror("multiple definition of label");
                }
            ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 1187 "asm/as1600_real.y"
    {
                chtcpoint = (int *) NULL;
                emit_set_equ(0L);
            ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 1192 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(2) - (2)].intv));
                if( evalr[0].seg == SSG_ABS)
                {
                    if( evalr[0].value == 0)
                    {
                        chtcpoint = (int *)NULL;
                        emit_set_equ(0L);
                    }
                    else if(evalr[0].value < chtnxalph)
                    {
                        chtcpoint = chtatab[evalr[0].value];
                        emit_set_equ(evalr[0].value);
                    }
                    else
                    {
                        fraerror("nonexistent character translation table");
                    }
                }
                else
                {
                    fraerror("noncomputable expression");
                }
            ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 1217 "asm/as1600_real.y"
    {
                chardef((yyvsp[(2) - (4)].strng), (yyvsp[(4) - (4)].intv));
            ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 1221 "asm/as1600_real.y"
    {
                char st[2] = { (yyvsp[(2) - (4)].longv), 0 };
                chardef(st, (yyvsp[(4) - (4)].intv));
            ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 1226 "asm/as1600_real.y"
    {
                if((yyvsp[(1) - (1)].symb)->seg == SSG_UNDEF)
                {
                    (yyvsp[(1) - (1)].symb)->seg = SSG_ABS;
                    (yyvsp[(1) - (1)].symb)->value = labelloc;
                    emit_set_equ(labelloc);

                }
                else
                    fraerror("multiple definition of label");
            ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 1241 "asm/as1600_real.y"
    {
                if (sdbd)
                    frawarn("label between SDBD and instruction");

                if((yyvsp[(1) - (2)].symb)->seg == SSG_UNDEF)
                {
                    (yyvsp[(1) - (2)].symb)->seg   = SSG_ABS;
                    (yyvsp[(1) - (2)].symb)->value = labelloc;
                }
                else
                    fraerror("multiple definition of label");

                if (locctr & 1) fraerror("internal error: PC misaligned.");

                labelloc = locctr >> 1;

                sdbd    = is_sdbd;
                is_sdbd = 0;
                first   = 0;
            ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 1262 "asm/as1600_real.y"
    {
                if (locctr & 1) fraerror("internal error: PC misaligned.");
                labelloc = locctr >> 1;

                sdbd    = is_sdbd;
                is_sdbd = 0;
                first   = 0;
            ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 1273 "asm/as1600_real.y"
    {
                emit_location(currseg, labelloc, TYPE_DATA, currmode);
                evalr[2].seg   = SSG_ABS;
                evalr[2].value = 8;
                for( satsub = 0; satsub < (yyvsp[(2) - (2)].intv); satsub++)
                {
                    pevalexpr(1, exprlist[satsub]);
                    locctr += geninstr(genbdef);
                }
            ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 1284 "asm/as1600_real.y"
    {
                emit_location(currseg, labelloc, TYPE_DATA, currmode);
                evalr[2].seg   = SSG_ABS;
                evalr[2].value = romw;
                for( satsub = 0; satsub < (yyvsp[(2) - (2)].intv); satsub++)
                {
                    pevalexpr(1, exprlist[satsub]);
                    locctr += geninstr(genbdef);
                }
            ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 1296 "asm/as1600_real.y"
    {
                emit_location(currseg, labelloc, TYPE_STRING, currmode);
                evalr[2].seg   = SSG_ABS;
                evalr[2].value = romw;
                for( satsub = 0; satsub < (yyvsp[(2) - (2)].intv); satsub++)
                {
                    pevalexpr(1, exprlist[satsub]);
                    locctr += geninstr(genbdef);
                }
            ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 1307 "asm/as1600_real.y"
    {
                emit_location(currseg, labelloc, TYPE_DBDATA|TYPE_DATA, currmode);
                for( satsub = 0; satsub < (yyvsp[(2) - (2)].intv); satsub++)
                {
                    pevalexpr(1, exprlist[satsub]);
                    locctr += geninstr(genwdef);
                }
            ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 1316 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(2) - (2)].intv));
                if(evalr[0].seg == SSG_ABS)
                {
                    locctr = 2 * (labelloc + evalr[0].value);
                    emit_set_equ(labelloc);
                    emit_location(currseg, labelloc, TYPE_HOLE, currmode);
                    emit_reserve(labelloc + evalr[0].value);
                }
                else
                {
                    fraerror("noncomputable expression for RMB");
                }
            ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 1340 "asm/as1600_real.y"
    {
                exprlist[nextexprs++] = (yyvsp[(3) - (3)].intv);
                (yyval.intv) = nextexprs;
            ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 1345 "asm/as1600_real.y"
    {
                char *s = (yyvsp[(3) - (3)].strng);
                int  accval = 0;

                while (*s)
                {
                    accval = chtran(&s);
                    exprlist[nextexprs++] = 
                        exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,accval,SYMNULL);
                }
                (yyval.intv) = nextexprs;
            ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 1358 "asm/as1600_real.y"
    {
                nextexprs = 0;
                exprlist[nextexprs++] = (yyvsp[(1) - (1)].intv);
                (yyval.intv) = nextexprs;
            ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 1364 "asm/as1600_real.y"
    {
                char *s = (yyvsp[(1) - (1)].strng);
                int  accval = 0;

                nextexprs = 0;
                while (*s)
                {
                    accval = chtran(&s);
                    exprlist[nextexprs++] = 
                        exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,accval,SYMNULL);
                }
                (yyval.intv) = nextexprs;
            ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 1378 "asm/as1600_real.y"
    {
                (yyval.intv) = (yyvsp[(3) - (3)].intv);
            ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 1411 "asm/as1600_real.y"
    {   
                char *s = &tempstr[tempstrlen];

                if (chtcpoint != NULL)
                {
                    frawarn("Stringifying expression list while character translation active");
                }

                tempstrlen += (yyvsp[(3) - (4)].intv) + 1;

                if (tempstrlen > MAXTEMPSTR)
                {
                    fraerror("Temporary string buffer overflow");
                    (yyval.strng) = "";
                } else
                {
                    int i;
                    (yyval.strng) = s;
                    for (i = 0; i < (yyvsp[(3) - (4)].intv); i++)
                    {
                        pevalexpr(0, exprlist[i]);

                        if (evalr[0].seg == SSG_ABS)
                            *s++ = evalr[0].value;
                        else
                            *s++ = '?';
                    }
                    *s = 0;
                }
                nextexprs = 0;
            ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 1443 "asm/as1600_real.y"
    {   
                char *s = &tempstr[tempstrlen];

                if (tempstrlen + 32 > MAXTEMPSTR)
                {
                    fraerror("Temporary string buffer overflow");
                    (yyval.strng) = "";
                } else
                {
                    (yyval.strng) = s;
                    pevalexpr(0, (yyvsp[(4) - (5)].intv));

                    if (evalr[0].seg == SSG_ABS)
                        sprintf(s, "%d", (int)evalr[0].value);
                    else
                    {
                        s[0] = '?';
                        s[1] = 0;
                    }
                    tempstrlen += strlen(s) + 1;
                }
            ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 1466 "asm/as1600_real.y"
    {   
                char *s = &tempstr[tempstrlen];

                if (tempstrlen + 5 > MAXTEMPSTR)
                {
                    fraerror("Temporary string buffer overflow");
                    (yyval.strng) = "";
                } else
                {
                    (yyval.strng) = s;
                    pevalexpr(0, (yyvsp[(4) - (5)].intv));

                    if (evalr[0].seg == SSG_ABS)
                        sprintf(s, "%4.4X", 
                                (unsigned int)(0xFFFF & evalr[0].value));
                    else
                    {
                        s[0] = '?';
                        s[1] = '?';
                        s[2] = '?';
                        s[3] = '?';
                        s[4] = 0;
                    }
                    tempstrlen += 5;
                }
            ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 1493 "asm/as1600_real.y"
    {   
                char *s = &tempstr[tempstrlen];

                if (tempstrlen + 5 > MAXTEMPSTR)
                {
                    fraerror("Temporary string buffer overflow");
                    (yyval.strng) = "";
                } else
                {
                    (yyval.strng) = s;
                    pevalexpr(0, (yyvsp[(4) - (5)].intv));

                    if (evalr[0].seg == SSG_ABS)
                        sprintf(s, "%8.8X", 
                                (unsigned int)(0xFFFFFFFF & evalr[0].value));
                    else
                    {
                        s[0] = '?';
                        s[1] = '?';
                        s[2] = '?';
                        s[3] = '?';
                        s[4] = '?';
                        s[5] = '?';
                        s[6] = '?';
                        s[7] = '?';
                        s[8] = 0;
                    }
                    tempstrlen += 5;
                }
            ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 1544 "asm/as1600_real.y"
    {
                if (proc && struct_locctr != -1)
                    fraerror("PROC cannot nest inside STRUCT.");
                else if (proc && proc_stk_depth == MAX_PROC_STK)
                    fraerror("PROC nesting limit reached.");
                else if (((yyvsp[(1) - (2)].symb)->flags & SFLAG_ARRAY) != 0)
                    fraerror("array element can not be defined by PROC");
                else if ((yyvsp[(1) - (2)].symb)->seg != SSG_UNDEF)
                    fraerror("multiple definition of label");
                else
                {
                    if (proc)
                    {
                        char *old_proc     = proc;
                        int   old_proc_len = proc_len;
                        proc_stk[proc_stk_depth++] = proc;
                        proc_len = strlen(proc) + strlen((yyvsp[(1) - (2)].symb)->symstr) + 1;
                        proc     = malloc(proc_len + 1);
                        strcpy(proc, old_proc);
                        proc[old_proc_len] = '.';
                        strcpy(proc + old_proc_len + 1, (yyvsp[(1) - (2)].symb)->symstr);
                    } else
                    {
                        proc     = strdup((yyvsp[(1) - (2)].symb)->symstr);
                        proc_len = strlen(proc);
                    }

                    (yyvsp[(1) - (2)].symb)->seg   = SSG_ABS;
                    (yyvsp[(1) - (2)].symb)->value = labelloc;
                    emit_set_equ(labelloc);
                }
            ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 1578 "asm/as1600_real.y"
    {
                if (!proc || struct_locctr != -1)
                    fraerror("ENDP w/out PROC.");

                free(proc);

                if (proc_stk_depth > 0)
                {
                    proc     = proc_stk[--proc_stk_depth];
                    proc_len = strlen(proc);
                } else
                {
                    proc     = NULL;
                    proc_len = 0;
                }
            ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 1599 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(3) - (3)].intv));

                if (proc)
                    fraerror("STRUCT can not nest inside other STRUCTs or PROCs.");
                else if (((yyvsp[(1) - (3)].symb)->flags & SFLAG_ARRAY) != 0)
                    fraerror("array element can not be defined by STRUCT");
                else if (evalr[0].seg != SSG_ABS)
                    fraerror( "noncomputable expression for ORG");
                else if ((yyvsp[(1) - (3)].symb)->seg != SSG_UNDEF)
                    fraerror( "multiple definition of label");
                else
                {
                    proc     = strdup((yyvsp[(1) - (3)].symb)->symstr);
                    proc_len = strlen(proc);
                    struct_locctr = locctr;

                    locctr = 2 * (labelloc = evalr[0].value);

                    (yyvsp[(1) - (3)].symb)->seg = SSG_ABS;
                    (yyvsp[(1) - (3)].symb)->value = labelloc;

                    emit_set_equ(evalr[0].value);
                }
            ;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 1626 "asm/as1600_real.y"
    {
                if (!proc || struct_locctr == -1)
                    fraerror("ENDS w/out STRUCT.");
                else
                {
                    free(proc);
                    proc     = NULL;
                    proc_len = 0;
                    locctr = struct_locctr;
                    struct_locctr = -1;
                }
            ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 1643 "asm/as1600_real.y"
    {
                emit_location(currseg, labelloc, TYPE_HOLE, currmode);
                pevalexpr(0, (yyvsp[(2) - (2)].intv));
                if(evalr[0].seg == SSG_ABS)
                {
                    romw = evalr[0].value;

                    if (romw < 8 || romw > 16)
                        fraerror("ROMWIDTH out of range");

                    romm = 0xFFFFU >> (16 - romw);
                }
                else
                {
                    fraerror("noncomputable expression for ROMWIDTH");
                }

                if (!first)
                {
                    frawarn("Code appears before ROMW directive.");
                }

                fwd_sdbd = 0;
            ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 1668 "asm/as1600_real.y"
    {
                emit_location(currseg, labelloc, TYPE_HOLE, currmode);
                pevalexpr(0, (yyvsp[(2) - (4)].intv));
                pevalexpr(1, (yyvsp[(4) - (4)].intv));
                if(evalr[0].seg == SSG_ABS)
                {
                    romw = evalr[0].value;

                    if (romw < 8 || romw > 16)
                        fraerror("ROMWIDTH out of range");

                    romm = 0xFFFFU >> (16 - romw);
                }
                else
                    fraerror("noncomputable expression for ROMWIDTH");

                if (!first)
                {
                    frawarn("Code appears before ROMW directive.");
                }

                if (evalr[1].seg == SSG_ABS)
                {
                    fwd_sdbd = evalr[1].value;

                    if (fwd_sdbd > 1 || fwd_sdbd < 0)
                        fraerror("SDBD mode flag must be 0 or 1.");
                } else
                    fraerror("noncomputable expression for ROMWIDTH");

            ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 1705 "asm/as1600_real.y"
    {
                if (sdbd)
                    frawarn("Two SDBDs in a row.");

                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                locctr += geninstr(findgen((yyvsp[(1) - (1)].intv), ST_IMP, 0));
                is_sdbd = SDBD;
            ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 1719 "asm/as1600_real.y"
    {
                /*unsigned rel_addr = labelloc + 2;*/
                /*int dir;*/

                SDBD_CHK

                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                pevalexpr(1, (yyvsp[(2) - (2)].intv));

                evalr[3].seg   = SSG_ABS;
                evalr[3].value = romw;

                locctr += geninstr(findgen((yyvsp[(1) - (2)].intv), ST_EXP, sdbd));
            ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 1735 "asm/as1600_real.y"
    {
                /*unsigned rel_addr = labelloc + 2;*/
                /*int dir;*/

                SDBD_CHK

                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                pevalexpr(1, (yyvsp[(2) - (4)].intv));
                pevalexpr(4, (yyvsp[(4) - (4)].intv));

                if (evalr[4].seg != SSG_ABS)
                    fraerror("Must have constant expr for BEXT condition");

                evalr[3].seg   = SSG_ABS;
                evalr[3].value = romw;

                locctr += geninstr(findgen((yyvsp[(1) - (4)].intv), ST_EXPEXP, sdbd));
            ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 1760 "asm/as1600_real.y"
    {
                SDBD_CHK
                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                locctr += geninstr(findgen((yyvsp[(1) - (1)].intv), ST_IMP, sdbd));
            ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 1771 "asm/as1600_real.y"
    {
                SDBD_CHK
                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                pevalexpr(1, (yyvsp[(2) - (2)].intv));
                locctr += geninstr(findgen((yyvsp[(1) - (2)].intv), ST_EXP, sdbd));
            ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 1784 "asm/as1600_real.y"
    {
                SDBD_CHK
                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                evalr[1].value = (yyvsp[(2) - (4)].intv);
                pevalexpr(2, (yyvsp[(4) - (4)].intv));
                evalr[3].seg    = SSG_ABS;
                evalr[3].value  = romw;
                locctr += geninstr(findgen((yyvsp[(1) - (4)].intv), ST_REGEXP, reg_type[(yyvsp[(2) - (4)].intv)]|sdbd));
            ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 1799 "asm/as1600_real.y"
    {
                SDBD_CHK
                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                evalr[1].value  = (yyvsp[(2) - (5)].intv);
                evalr[3].seg    = SSG_ABS;
                evalr[3].value  = romw;
                pevalexpr(2, (yyvsp[(5) - (5)].intv));
                locctr += geninstr(findgen((yyvsp[(1) - (5)].intv), ST_REGCEX, reg_type[(yyvsp[(2) - (5)].intv)]|sdbd));
            ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 1814 "asm/as1600_real.y"
    {
                SDBD_CHK
                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                pevalexpr(1, (yyvsp[(2) - (4)].intv));
                evalr[2].value = (yyvsp[(4) - (4)].intv);
                evalr[3].seg   = SSG_ABS;
                evalr[3].value = romw;
                locctr += geninstr(findgen((yyvsp[(1) - (4)].intv), ST_EXPREG, reg_type[(yyvsp[(4) - (4)].intv)]|sdbd));
            ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 1829 "asm/as1600_real.y"
    {
                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                pevalexpr(1, (yyvsp[(3) - (5)].intv));
                evalr[2].value = (yyvsp[(5) - (5)].intv);

                evalr[3].seg   = SSG_ABS;
                evalr[3].value = romw;

                if (sdbd == 0 && romw != 16)
                {
                    if (evalr[1].seg == SSG_ABS && 
                        (0xFFFF & evalr[1].value & ~romm) != 0)
                    {
                        /*frawarn("Constant is wider than ROM width.  "
                                "Inserting SDBD.");*/
                        locctr += geninstr("0001x");
                        sdbd = SDBD;
                    }

                    if (evalr[1].seg != SSG_ABS && fwd_sdbd)
                    {   
                        frawarn("Inserting SDBD due to forward reference.");
                        locctr += geninstr("0001x");
                        sdbd = SDBD;
                    }
                }

                locctr += geninstr(findgen((yyvsp[(1) - (5)].intv), ST_CEXREG, reg_type[(yyvsp[(5) - (5)].intv)]|sdbd));
            ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 1864 "asm/as1600_real.y"
    {
                SDBD_CHK
                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                evalr[1].value = (yyvsp[(2) - (2)].intv);
                locctr += geninstr(findgen((yyvsp[(1) - (2)].intv), ST_REG, reg_type[(yyvsp[(2) - (2)].intv)]|sdbd));
            ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 1876 "asm/as1600_real.y"
    {
                SDBD_CHK
                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                evalr[1].value = (yyvsp[(2) - (4)].intv);
                evalr[2].value = (yyvsp[(4) - (4)].intv);
                locctr += geninstr(findgen((yyvsp[(1) - (4)].intv), ST_REGREG, reg_type[(yyvsp[(2) - (4)].intv)]|sdbd));
            ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 1889 "asm/as1600_real.y"
    {
                emit_location(currseg, labelloc, TYPE_CODE, currmode);
                evalr[1].value = (yyvsp[(2) - (4)].intv);
                evalr[2].value = (yyvsp[(4) - (4)].intv);
                locctr += geninstr(findgen((yyvsp[(1) - (4)].intv), ST_REGREG, reg_type[(yyvsp[(2) - (4)].intv)]|sdbd));
            ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 1902 "asm/as1600_real.y"
    {
                (yyval.intv) = (yyvsp[(2) - (2)].intv);
            ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 1906 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_UN,(yyvsp[(2) - (2)].intv),IFC_NEG,0,0L, SYMNULL);
            ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 1910 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_UN,(yyvsp[(2) - (2)].intv),IFC_NOT,0,0L, SYMNULL);
            ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 1914 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_UN,(yyvsp[(2) - (2)].intv),IFC_HIGH,0,0L, SYMNULL);
            ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 1918 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_UN,(yyvsp[(2) - (2)].intv),IFC_LOW,0,0L, SYMNULL);
            ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 1922 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_MUL,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 1926 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_DIV,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 1930 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_ADD,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 1934 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_SUB,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 1938 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_MOD,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 1942 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_SHL,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 1946 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_SHR,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 1950 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_SHRU,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 1954 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_GT,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 1958 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_GE,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 1962 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_LT,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 1966 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_LE,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 1970 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_NE,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 1974 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_EQ,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 1978 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_AND,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 1982 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_OR,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 1986 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_BIN,(yyvsp[(1) - (3)].intv),IFC_XOR,(yyvsp[(3) - (3)].intv),0L, SYMNULL);
            ;}
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 1990 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_DEF,0,IGP_DEFINED,0,0L,(yyvsp[(2) - (2)].symb));
            ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 1994 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_SYMB,0,IFC_SYMB,0,0L,(yyvsp[(1) - (1)].symb));
            ;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 1998 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,(yyvsp[(2) - (2)].longv), SYMNULL);
            ;}
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 2002 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,(yyvsp[(1) - (1)].longv), SYMNULL);
            ;}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 2006 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_PROGC,0,IFC_PROGCTR,0,labelloc,SYMNULL);
            ;}
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 2010 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,(yyvsp[(1) - (1)].longv), SYMNULL);
            ;}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 2014 "asm/as1600_real.y"
    {
                char st[2] = { (yyvsp[(1) - (1)].longv), 0 }, *s = st;
                int  accval = chtran(&s);
                (yyval.intv) = exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,accval,SYMNULL);
            ;}
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 2020 "asm/as1600_real.y"
    {
                char *s = (yyvsp[(3) - (4)].strng);
                int  accval = 0;
                int  length = 0;

                while (*s)
                {
                    accval = chtran(&s);
                    length++;
                }
                (yyval.intv) = exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,length++,SYMNULL);
            ;}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 2033 "asm/as1600_real.y"
    {
                (yyval.intv) = exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,1,SYMNULL);
            ;}
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 2037 "asm/as1600_real.y"
    {
                char *s = (yyvsp[(3) - (6)].strng);
                int  accval = 0;
                int  sindex = 0;

                pevalexpr(0, (yyvsp[(5) - (6)].intv));
                if(evalr[0].seg == SSG_ABS)
                {
                    sindex = evalr[0].value;
                    while (*s && sindex >= 0)
                    {
                        accval = chtran(&s);
                        sindex--;
                    }
                    if (sindex >= 0) 
                        accval = 0;

                    (yyval.intv) = exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,accval,SYMNULL);
                }
                else
                {
                    fraerror("noncomputable expression for index to ASC");
                }
            ;}
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 2062 "asm/as1600_real.y"
    {
                char st[2] = { (yyvsp[(3) - (6)].longv), 0 }, *s = st;
                int  accval = 0;

                pevalexpr(0, (yyvsp[(5) - (6)].intv));
                if(evalr[0].seg == SSG_ABS)
                {
                    accval = evalr[0].value == 0 ? chtran(&s) : 0;
                    (yyval.intv) = exprnode(PCCASE_CONS,0,IGP_CONSTANT,0,accval,SYMNULL);
                }
                else
                {
                    fraerror("noncomputable expression for index to ASC");
                }
            ;}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 2078 "asm/as1600_real.y"
    {
                (yyval.intv) = (yyvsp[(2) - (3)].intv);
            ;}
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 2090 "asm/as1600_real.y"
    {   
                pevalexpr(0, (yyvsp[(3) - (4)].intv));
                if (evalr[0].seg == SSG_ABS)
                {
                    (yyval.symb) = symbentryidx((yyvsp[(1) - (4)].symb)->symstr, LABEL, 1, evalr[0].value);
                    (yyval.symb)->flags |= SFLAG_QUIET | SFLAG_ARRAY;
                   
                    /* track "high water mark" in LABEL's own value */
                    (yyvsp[(1) - (4)].symb)->seg    = SSG_SET;
                    (yyvsp[(1) - (4)].symb)->flags |= SFLAG_QUIET;
                    if ((yyvsp[(1) - (4)].symb)->value < evalr[0].value)
                        (yyvsp[(1) - (4)].symb)->value = evalr[0].value;
                } else
                {
                    fraerror("noncomputable expression for label array index");
                }
            ;}
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 2111 "asm/as1600_real.y"
    {   
                pevalexpr(0, (yyvsp[(3) - (4)].intv));
                if (evalr[0].seg == SSG_ABS)
                {
                    (yyval.symb) = symbentryidx((yyvsp[(1) - (4)].symb)->symstr, LABEL, 1, evalr[0].value);
                    (yyval.symb)->flags |= SFLAG_QUIET | SFLAG_ARRAY;
                    
                    /* track "high water mark" in LABEL's own value */
                    (yyvsp[(1) - (4)].symb)->seg    = SSG_SET;
                    (yyvsp[(1) - (4)].symb)->flags |= SFLAG_QUIET;
                    if ((yyvsp[(1) - (4)].symb)->value < evalr[0].value)
                        (yyvsp[(1) - (4)].symb)->value = evalr[0].value;
                } else
                {
                    fraerror("noncomputable expression for symbol array index");
                }
            ;}
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 2132 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(3) - (6)].intv));
                pevalexpr(1, (yyvsp[(5) - (6)].intv));

                if (evalr[0].seg != SSG_ABS || evalr[1].seg != SSG_ABS)
                {
                    fraerror("noncomputable expression for symbol slice index");
                } else
                {
                    int i, s;

                    s = evalr[0].value > evalr[1].value ? -1 : 1;

                    for (i = evalr[0].value; i != evalr[1].value + s; i += s)
                    {
                        struct symel *sym;
                        exprlist[nextexprs++] = 
                            exprnode(PCCASE_SYMB,0,IFC_SYMB,0,0L,
                                sym = symbentryidx((yyvsp[(1) - (6)].symb)->symstr, LABEL, 1, i));

                        sym->flags |= SFLAG_ARRAY | SFLAG_QUIET;
                    }
                    (yyval.intv) = nextexprs;
                }
            ;}
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 2158 "asm/as1600_real.y"
    {
                fraerror("array slice allowed on last index only");
            ;}
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 2162 "asm/as1600_real.y"
    {
                fraerror("array slice allowed on last index only");
            ;}
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 2169 "asm/as1600_real.y"
    {
                pevalexpr(0, (yyvsp[(3) - (6)].intv));
                pevalexpr(1, (yyvsp[(5) - (6)].intv));

                if (evalr[0].seg != SSG_ABS || evalr[1].seg != SSG_ABS)
                {
                    fraerror("noncomputable expression for label slice index");
                } else
                {
                    (yyval.slidx).first = evalr[0].value;
                    (yyval.slidx).last  = evalr[1].value;
                    (yyval.slidx).sym   = (yyvsp[(1) - (6)].symb);
                }
            ;}
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 2184 "asm/as1600_real.y"
    {
                fraerror("array slice allowed on last index only");
            ;}
    break;

  case 129:

/* Line 1455 of yacc.c  */
#line 2188 "asm/as1600_real.y"
    {
                fraerror("array slice allowed on last index only");
            ;}
    break;



/* Line 1455 of yacc.c  */
#line 4382 "asm/as1600.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 2193 "asm/as1600_real.y"


int lexintercept(void)
/*
    description intercept the call to yylex (the lexical analyzer)
            and filter out all unnecessary tokens when skipping
            the input between a failed IF and its matching ENDI or
            ELSE
    globals     fraifskip   the enable flag
*/
{
#undef yylex
    int rv;

    if (!(frarptskip | fraifskip))
    {
        int token = yylex();
        return token;
    }

    if(frarptskip)
    {
        for(;;)
        {

            switch(rv = yylex())
            {
            case 0:
            case KOC_END:
            case KOC_ENDR:
            case EOL:
                return rv;
            case KOC_RPT:
                frarptpush(0);  /* push a dummy loop */
            default:
                break;
            }
        }
    } else if(fraifskip)
    {
        for(;;)
        {

            switch(rv = yylex())
            {
            case 0:
            case KOC_END:
            case KOC_IF:
            case KOC_ELSE:
            case KOC_ENDI:
            case EOL:
                return rv;
            default:
                break;
            }
        }
    } else
    {
        int token = yylex();
        return token;
    }

#define yylex lexintercept
}



void setreserved(void)
{

    reservedsym("and",      KEOP_AND,       0);
    reservedsym("defined",  KEOP_DEFINED,   0);
    reservedsym("ge",       KEOP_GE,        0);
    reservedsym("high",     KEOP_HIGH,      0);
    reservedsym("le",       KEOP_LE,        0);
    reservedsym("low",      KEOP_LOW,       0);
    reservedsym("mod",      KEOP_MOD,       0);
    reservedsym("ne",       KEOP_NE,        0);
    reservedsym("not",      KEOP_NOT,       0);
    reservedsym("or",       KEOP_OR,        0);
    reservedsym("shl",      KEOP_SHL,       0);
    reservedsym("shr",      KEOP_SHR,       0);
    reservedsym("shru",     KEOP_SHRU,      0);
    reservedsym("xor",      KEOP_XOR,       0);
    reservedsym("AND",      KEOP_AND,       0);
    reservedsym("DEFINED",  KEOP_DEFINED,   0);
    reservedsym("GE",       KEOP_GE,        0);
    reservedsym("HIGH",     KEOP_HIGH,      0);
    reservedsym("LE",       KEOP_LE,        0);
    reservedsym("LOW",      KEOP_LOW,       0);
    reservedsym("MOD",      KEOP_MOD,       0);
    reservedsym("NE",       KEOP_NE,        0);
    reservedsym("NOT",      KEOP_NOT,       0);
    reservedsym("OR",       KEOP_OR,        0);
    reservedsym("SHL",      KEOP_SHL,       0);
    reservedsym("SHR",      KEOP_SHR,       0);
    reservedsym("SHRU",     KEOP_SHRU,      0);
    reservedsym("XOR",      KEOP_XOR,       0);
    reservedsym("STRLEN",   KEOP_STRLEN,    0);
    reservedsym("ASC",      KEOP_ASC,       0);

    /* machine specific token definitions */
    reservedsym("r0",       REGISTER,       0);
    reservedsym("r1",       REGISTER,       1);
    reservedsym("r2",       REGISTER,       2);
    reservedsym("r3",       REGISTER,       3);
    reservedsym("r4",       REGISTER,       4);
    reservedsym("r5",       REGISTER,       5);
    reservedsym("r6",       REGISTER,       6);
    reservedsym("r7",       REGISTER,       7);
    reservedsym("sp",       REGISTER,       6);
    reservedsym("pc",       REGISTER,       7);
    reservedsym("R0",       REGISTER,       0);
    reservedsym("R1",       REGISTER,       1);
    reservedsym("R2",       REGISTER,       2);
    reservedsym("R3",       REGISTER,       3);
    reservedsym("R4",       REGISTER,       4);
    reservedsym("R5",       REGISTER,       5);
    reservedsym("R6",       REGISTER,       6);
    reservedsym("R7",       REGISTER,       7);
    reservedsym("SP",       REGISTER,       6);
    reservedsym("PC",       REGISTER,       7);

    reservedsym("__FEATURE.MACRO", FEATURE, 99);
    reservedsym("MACRO",           FEATURE, 99);
    reservedsym("ENDM",            FEATURE, 99);
}

int cpumatch(char *str)
{
    (void)str;
    return TRUE;
}


/* ======================================================================== */
/*  Opcode and Instruction Generation Tables                                */
/*                                                                          */
/*  These tables are used by the assembler framework to generate            */
/*  instructions from the parsed input.                                     */
/*                                                                          */
/*  OPTAB    -- OPcode TABle.  Contains the set of supported mnemonics.     */
/*  OSTAB    -- Opcode Syntax TABle.  Syntax definition sets for instrs.    */
/*  IGTAB    -- Instruction Generation TABle.  Contains RPN code for        */
/*              generating the instructions.                                */
/* ======================================================================== */



/* ======================================================================== */
/*  OPTAB    -- OPcode TABle.  Contains the set of supported mnemonics.     */
/* ======================================================================== */
struct opsym optab[] =
{
    {   "invalid",  KOC_opcode,     2,  0   },

    {   "MVO",      KOC_opcode,     1,  2   },
    {   "MVI",      KOC_opcode,     1,  3   },
    {   "ADD",      KOC_opcode,     1,  4   },
    {   "SUB",      KOC_opcode,     1,  5   },
    {   "CMP",      KOC_opcode,     1,  6   },
    {   "AND",      KOC_opcode,     1,  7   },
    {   "XOR",      KOC_opcode,     1,  8   },

    {   "MVO@",     KOC_opcode,     1,  9   },
    {   "MVI@",     KOC_opcode_i,   1,  10  },
    {   "ADD@",     KOC_opcode_i,   1,  11  },
    {   "SUB@",     KOC_opcode_i,   1,  12  },
    {   "CMP@",     KOC_opcode_i,   1,  13  },
    {   "AND@",     KOC_opcode_i,   1,  14  },
    {   "XOR@",     KOC_opcode_i,   1,  15  },

    {   "MVOI",     KOC_opcode,     1,  16  },
    {   "MVII",     KOC_opcode,     1,  17  },
    {   "ADDI",     KOC_opcode,     1,  18  },
    {   "SUBI",     KOC_opcode,     1,  19  },
    {   "CMPI",     KOC_opcode,     1,  20  },
    {   "ANDI",     KOC_opcode,     1,  21  },
    {   "XORI",     KOC_opcode,     1,  22  },

    {   "MOVR",     KOC_opcode,     1,  24  },
    {   "ADDR",     KOC_opcode,     1,  25  },
    {   "SUBR",     KOC_opcode,     1,  26  },
    {   "CMPR",     KOC_opcode,     1,  27  },
    {   "ANDR",     KOC_opcode,     1,  28  },
    {   "XORR",     KOC_opcode,     1,  29  },

    {   "B",        KOC_relbr,      1,  30  },
    {   "BC",       KOC_relbr,      1,  31  },
    {   "BOV",      KOC_relbr,      1,  32  },
    {   "BPL",      KOC_relbr,      1,  33  },
    {   "BZE",      KOC_relbr,      1,  34  },
    {   "BEQ",      KOC_relbr,      1,  34  },
    {   "BLT",      KOC_relbr,      1,  35  },
    {   "BNGE",     KOC_relbr,      1,  35  },
    {   "BLE",      KOC_relbr,      1,  36  },
    {   "BNGT",     KOC_relbr,      1,  36  },
    {   "BUSC",     KOC_relbr,      1,  37  },

    {   "NOPP",     KOC_opcode,     2,  92  },
    {   "BNC",      KOC_relbr,      1,  39  },
    {   "BNOV",     KOC_relbr,      1,  40  },
    {   "BMI",      KOC_relbr,      1,  41  },
    {   "BNZE",     KOC_relbr,      1,  42  },
    {   "BNZ",      KOC_relbr,      1,  42  },
    {   "BNEQ",     KOC_relbr,      1,  42  },
    {   "BNE",      KOC_relbr,      1,  42  },
    {   "BGE",      KOC_relbr,      1,  43  },
    {   "BNLT",     KOC_relbr,      1,  43  },
    {   "BGT",      KOC_relbr,      1,  44  },
    {   "BNLE",     KOC_relbr,      1,  44  },
    {   "BESC",     KOC_relbr,      1,  45  },

    {   "BEXT",     KOC_relbr_x,    1,  96  },

    {   "SWAP",     KOC_opcode,     2,  46  },
    {   "SLL",      KOC_opcode,     2,  48  },
    {   "RLC",      KOC_opcode,     2,  50  },
    {   "SLLC",     KOC_opcode,     2,  52  },
    {   "SLR",      KOC_opcode,     2,  54  },
    {   "SAR",      KOC_opcode,     2,  56  },
    {   "RRC",      KOC_opcode,     2,  58  },
    {   "SARC",     KOC_opcode,     2,  60  },

    {   "NOP",      KOC_opcode,     1,  62  },
    {   "NOP2",     KOC_opcode,     1,  94  },
    {   "SIN",      KOC_opcode,     1,  63  },
    {   "SIN2",     KOC_opcode,     1,  95  },

    {   "J",        KOC_opcode,     1,  64  },
    {   "JE",       KOC_opcode,     1,  65  },
    {   "JD",       KOC_opcode,     1,  66  },
    {   "JSR",      KOC_opcode,     1,  67  },
    {   "JSRE",     KOC_opcode,     1,  68  },
    {   "JSRD",     KOC_opcode,     1,  69  },

    {   "INCR",     KOC_opcode,     1,  70  },
    {   "DECR",     KOC_opcode,     1,  71  },
    {   "COMR",     KOC_opcode,     1,  72  },
    {   "NEGR",     KOC_opcode,     1,  73  },
    {   "ADCR",     KOC_opcode,     1,  74  },
    {   "GSWD",     KOC_opcode,     1,  75  },
    {   "RSWD",     KOC_opcode,     1,  76  },

    {   "HLT",      KOC_opcode,     1,  77  },
    {   "SDBD",     KOC_SDBD,       1,  78  },
    {   "EIS",      KOC_opcode,     1,  79  },
    {   "DIS",      KOC_opcode,     1,  80  },
    {   "TCI",      KOC_opcode,     1,  81  },
    {   "CLRC",     KOC_opcode,     1,  82  },
    {   "SETC",     KOC_opcode,     1,  83  },

    {   "TSTR",     KOC_opcode,     1,  84  },  /*  MOVR  Rx, Rx    */
    {   "CLRR",     KOC_opcode,     1,  85  },  /*  XORR  Rx, Rx    */
    {   "PSHR",     KOC_opcode,     1,  86  },  /*  MVO@  Rx, SP    */
    {   "PULR",     KOC_opcode,     1,  87  },  /*  MVI@  SP, Rx    */
    {   "JR",       KOC_opcode,     1,  88  },  /*  MOVR  Rx, PC    */
    {   "CALL",     KOC_opcode,     1,  89  },  /*  JSR   R5, addr  */
    {   "BEGIN",    KOC_opcode,     1,  90  },  /*  MVO@  R5, SP    */
    {   "RETURN",   KOC_opcode,     1,  91  },  /*  MVI@  SP, PC    */

    {   "DECLE",    KOC_DDEF,       0,  0   },  /* Generates ROMW values  */
    {   "DCW",      KOC_DDEF,       0,  0   },  /* Generates ROMW values  */
    {   "BIDECLE",  KOC_WDEF,       0,  0   },  /* Generates SDBD values  */
    {   "ROMWIDTH", KOC_ROMW,       0,  0   },
    {   "ROMW",     KOC_ROMW,       0,  0   },
    {   "PROC",     KOC_PROC,       0,  0   },
    {   "ENDP",     KOC_ENDP,       0,  0   },

    {   "BYTE",     KOC_BDEF,       0,  0   },  /* Generates 8-bit values */
    {   "CHARDEF",  KOC_CHDEF,      0,  0   },
    {   "CHARSET",  KOC_CHSET,      0,  0   },
    {   "CHARUSE",  KOC_CHUSE,      0,  0   },
    {   "CHD",      KOC_CHDEF,      0,  0   },
    {   "DATA",     KOC_DDEF,       0,  0   },  /* Generates ROMW values  */
    {   "DB",       KOC_BDEF,       0,  0   },  /* Generates 8-bit values */
    {   "DW",       KOC_WDEF,       0,  0   },  /* Generates SDBD values  */
    {   "ELSE",     KOC_ELSE,       0,  0   },
    {   "END",      KOC_END,        0,  0   },
    {   "ENDI",     KOC_ENDI,       0,  0   },
    {   "EQU",      KOC_EQU,        0,  0   },
    {   "FCB",      KOC_BDEF,       0,  0   },  /* Generates 8-bit values */
    {   "FCC",      KOC_SDEF,       0,  0   },
    {   "FDB",      KOC_WDEF,       0,  0   },  /* Generates SDBD values  */
    {   "IF",       KOC_IF,         0,  0   },
    {   "INCL",     KOC_INCLUDE,    0,  0   },
    {   "INCLUDE",  KOC_INCLUDE,    0,  0   },
    {   "ORG",      KOC_ORG,        0,  0   },
    {   "RES",      KOC_RESM,       0,  0   },
    {   "RESERVE",  KOC_RESM,       0,  0   },
    {   "RMB",      KOC_RESM,       0,  0   },
    {   "SET",      KOC_SET,        0,  0   },
    {   "STRING",   KOC_SDEF,       0,  0   },
    {   "WORD",     KOC_WDEF,       0,  0   },  /* Generates SDBD values  */

    {   "STRUCT",   KOC_STRUCT,     0,  0   },  /* Opens a struct def'n */
    {   "ENDS",     KOC_ENDS,       0,  0   },  /* Closes a struct def'n */

    {   "MEMATTR",  KOC_MEMATTR,    0,  0   },  /* Set memory attributes */

    {   "RPT",      KOC_RPT,        0,  0   },  /* Repeat a block of code */
    {   "REPEAT",   KOC_RPT,        0,  0   },  /* Repeat a block of code */
    {   "ENDR",     KOC_ENDR,       0,  0   },  /* End repeated block     */

    {   "ERR",      KOC_USRERR,     0,  0   },  /* User-designated error */

    {   "STRLEN",   KEOP_STRLEN,    0,  0   },  /* Returns length of string */
    {   "ASC",      KEOP_ASC,       0,  0   },  /* ASCII val of char in str */

    {   "LISTING",  KOC_LIST,       0,  0   },  /* Assembler listing control */
    {   "QEQU",     KOC_QSET,       0,  0   },  /* EQU and mark as "quiet" */
    {   "QSET",     KOC_QSET,       0,  0   },  /* SET and mark as "quiet" */

    {   "MACRO",    KOC_MACERR,     0,  0   },  /* We shouldn't see MACRO   */
    {   "ENDM",     KOC_MACERR,     0,  0   },  /* We shouldn't see ENDM    */

    {   "BRKIF",    KOC_BRKIF,      0,  0   },  /* Break out of RPT if true */

    {   "CMSG",     KOC_CMSG,       0,  0   },  /* Comment message in listing */
    {   "SMSG",     KOC_SMSG,       0,  0   },  /* Status message to stdout */
    {   "WMSG",     KOC_WMSG,       0,  0   },  /* Warning message */ 

    {   "",         0,              0,  0   }
};


/* ======================================================================== */
/*  OSTAB    -- Opcode Syntax TABle.  Syntax definition sets for instrs.    */
/*                                                                          */
/*  Legend:                                                                 */
/*      REG      Register.                                                  */
/*      EXP      EXPression                                                 */
/*      CEX      Constant EXpression (eg. exp. prefixed w/ #).              */
/*      IMP      Implied operand.                                           */
/* ======================================================================== */
struct opsynt ostab[] = 
{
    /*  invalid 0   */  {   0,          1,  0   },
    /*  invalid 1   */  {   0xFFFF,     1,  1   },

    /*  MVO     2   */  {   ST_REGEXP,  1,  2   },
    /*  MVI     3   */  {   ST_EXPREG,  1,  3   },
    /*  ADD     4   */  {   ST_EXPREG,  1,  4   },
    /*  SUB     5   */  {   ST_EXPREG,  1,  5   },
    /*  CMP     6   */  {   ST_EXPREG,  1,  6   },
    /*  AND     7   */  {   ST_EXPREG,  1,  7   },
    /*  XOR     8   */  {   ST_EXPREG,  1,  8   },

    /*  MVO@    9   */  {   ST_REGREG,  1,  9   },
    /*  MVI@    10  */  {   ST_REGREG,  1,  10  },
    /*  ADD@    11  */  {   ST_REGREG,  1,  11  },
    /*  SUB@    12  */  {   ST_REGREG,  1,  12  },
    /*  CMP@    13  */  {   ST_REGREG,  1,  13  },
    /*  AND@    14  */  {   ST_REGREG,  1,  14  },
    /*  XOR@    15  */  {   ST_REGREG,  1,  15  },

    /*  MVOI    16  */  {   ST_REGCEX,  1,  16  },
    /*  MVII    17  */  {   ST_CEXREG,  2,  17  },
    /*  ADDI    18  */  {   ST_CEXREG,  2,  19  },
    /*  SUBI    19  */  {   ST_CEXREG,  2,  21  },
    /*  CMPI    20  */  {   ST_CEXREG,  2,  23  },
    /*  ANDI    21  */  {   ST_CEXREG,  2,  25  },
    /*  XORI    22  */  {   ST_CEXREG,  2,  27  },

    /*  unused  23  */  {   0,          1,  0   },  /* oops */
    /*  MOVR    24  */  {   ST_REGREG,  1,  29  },
    /*  ADDR    25  */  {   ST_REGREG,  1,  30  },
    /*  SUBR    26  */  {   ST_REGREG,  1,  31  },
    /*  CMPR    27  */  {   ST_REGREG,  1,  32  },
    /*  ANDR    28  */  {   ST_REGREG,  1,  33  },
    /*  XORR    29  */  {   ST_REGREG,  1,  34  },

    /*  B       30  */  {   ST_EXP,     1,  35  },
    /*  BC      31  */  {   ST_EXP,     1,  36  },
    /*  BOV     32  */  {   ST_EXP,     1,  37  },
    /*  BPL     33  */  {   ST_EXP,     1,  38  },
    /*  BEQ     34  */  {   ST_EXP,     1,  39  },
    /*  BLT     35  */  {   ST_EXP,     1,  40  },
    /*  BLE     36  */  {   ST_EXP,     1,  41  },
    /*  BUSC    37  */  {   ST_EXP,     1,  42  },

    /*  unused  38  */  {   0,          1,  0   },  /* oops */
    /*  BNC     39  */  {   ST_EXP,     1,  44  },
    /*  BNOV    40  */  {   ST_EXP,     1,  45  },
    /*  BMI     41  */  {   ST_EXP,     1,  46  },
    /*  BNEQ    42  */  {   ST_EXP,     1,  47  },
    /*  BGE     43  */  {   ST_EXP,     1,  48  },
    /*  BGT     44  */  {   ST_EXP,     1,  49  },
    /*  BESC    45  */  {   ST_EXP,     1,  50  },

    /*  SWAP    46  */  {   ST_REG,     1,  51  },
    /*  SWAP    47  */  {   ST_REGEXP,  1,  52  },
    /*  SLL     48  */  {   ST_REG,     1,  53  },
    /*  SLL     49  */  {   ST_REGEXP,  1,  54  },
    /*  RLC     50  */  {   ST_REG,     1,  55  },
    /*  RLC     51  */  {   ST_REGEXP,  1,  56  },
    /*  SLLC    52  */  {   ST_REG,     1,  57  },
    /*  SLLC    53  */  {   ST_REGEXP,  1,  58  },
    /*  SLR     54  */  {   ST_REG,     1,  59  },
    /*  SLR     55  */  {   ST_REGEXP,  1,  60  },
    /*  SAR     56  */  {   ST_REG,     1,  61  },
    /*  SAR     57  */  {   ST_REGEXP,  1,  62  },
    /*  RRC     58  */  {   ST_REG,     1,  63  },
    /*  RRC     59  */  {   ST_REGEXP,  1,  64  },
    /*  SARC    60  */  {   ST_REG,     1,  65  },
    /*  SARC    61  */  {   ST_REGEXP,  1,  66  },

    /*  NOP     62  */  {   ST_IMP,     1,  67  },
    /*  SIN     63  */  {   ST_IMP,     1,  68  },

    /*  J       64  */  {   ST_EXP,     1,  69  },
    /*  JE      65  */  {   ST_EXP,     1,  70  },
    /*  JD      66  */  {   ST_EXP,     1,  71  },
    /*  JSR     67  */  {   ST_REGEXP,  1,  72  },
    /*  JSRE    68  */  {   ST_REGEXP,  1,  73  },
    /*  JSRD    69  */  {   ST_REGEXP,  1,  74  },

    /*  INCR    70  */  {   ST_REG,     1,  75  },
    /*  DECR    71  */  {   ST_REG,     1,  76  },
    /*  COMR    72  */  {   ST_REG,     1,  77  },
    /*  NEGR    73  */  {   ST_REG,     1,  78  },
    /*  ADCR    74  */  {   ST_REG,     1,  79  },
    /*  GSWD    75  */  {   ST_REG,     1,  80  },
    /*  RSWD    76  */  {   ST_REG,     1,  81  },

    /*  HLT     77  */  {   ST_IMP,     1,  82  },
    /*  SDBD    78  */  {   ST_IMP,     1,  83  },
    /*  EIS     79  */  {   ST_IMP,     1,  84  },
    /*  DIS     80  */  {   ST_IMP,     1,  85  },
    /*  TCI     81  */  {   ST_IMP,     1,  86  },
    /*  CLRC    82  */  {   ST_IMP,     1,  87  },
    /*  SETC    83  */  {   ST_IMP,     1,  88  },

    /*  TSTR    84  */  {   ST_REG,     1,  89  },
    /*  CLRR    85  */  {   ST_REG,     1,  90  },
    /*  PSHR    86  */  {   ST_REG,     1,  91  },
    /*  PULR    87  */  {   ST_REG,     1,  92  },
    /*  JR      88  */  {   ST_REG,     1,  93  },
    /*  CALL    89  */  {   ST_EXP,     1,  94  },
    /*  BEGIN   90  */  {   ST_IMP,     1,  95  },
    /*  RETURN  91  */  {   ST_IMP,     1,  96  },

    /*  NOPP    92  */  {   ST_EXP,     1,  43  },
    /*  NOPP    93  */  {   ST_IMP,     1,  97  },

    /*  NOP2    94  */  {   ST_IMP,     1,  98  },
    /*  SIN2    95  */  {   ST_IMP,     1,  99  },

    /*  BEXT    95  */  {   ST_EXPEXP,  1,  100 },

    /*  end         */  {   0,          0,  0   }
};


/* ======================================================================== */
/*  Helper macros.                                                          */
/*  MVO_OK  Tests arg 2 to make sure it's R1 .. R6.                         */
/*  SH_OK   Tests if shift amount is ok.                                    */
/*  CST_OK  Tests if constant is ok (within field width).                   */
/*  DBD     Generate double-byte-data.                                      */
/*  RR      Register/Register generator. Reused for Direct, Immediate.      */
/*  BR      Branch Relative generator.                                      */
/*  SH      Shift generator.                                                */
/*  SR      Single-register generator                                       */
/*  JSR     Jump/JSR generator                                              */
/*  CST     Constant arg generator (eg. immediate argument)                 */
/* ======================================================================== */
#define MVO_OK      "[2#].0=.[2#].7=+T$"
#define SH_OK(n)    #n ".1>T$" #n ".<0T$"
#define CST_OK(w,c) #c "." #w"I$"
#define DBD(x)      #x ".FF&x" #x ".8}.FF&x"
#define RR(o,x,y)   #o "." #x ".3{|." #y "|x"
#define BRDIR(a)    "P.2+." #a ">."
#define BROFS(a)    #a ".P.2+-." BRDIR(a) "!_^"
#define BR(c,a,w)   "0200." #c "|." BRDIR(a) "5{|x" BROFS(a) "~x" #w "I$"
#define BX(c,a,w)   #c ".4I$" \
                    "0210." #c "|." BRDIR(a) "5{|x" BROFS(a) "~x" #w "I$"
/*#define BR(c,a,m)   "0200." #c "|." BRDIR(a) "5{|x" #a "x"*/
#define CST(c,m)    CST_OK(m,c) #c "x"
#define SH(o,n,r)   SH_OK(n) "0040." #o ".3{|." #n ".1&.2{|." #r "|x"
#define SR(o,r)     "0000." #o ".3{|." #r "|x"
#define JSR(r,e,a)  "0004x" #r ".3&.8{." #a ".8}.FC&|." #e "|x" #a ".3FF&x"


/* ======================================================================== */
/*  IGTAB    -- Instruction Generator Table.                                */
/* ======================================================================== */
struct igel igtab[] = 
{
    /* inv  0   */  {   SDBD,       0,      "[Xnullentry"                   },
    /* inv  1   */  {   SDBD,       0,      "[Xinvalid opcode"              },

    /* MVO  2   */  {   SDBD,       0,      RR(0240,0,[1#]) CST([2=],[3#])  },
    /* MVI  3   */  {   SDBD,       0,      RR(0280,0,[2#]) CST([1=],[3#])  },
    /* ADD  4   */  {   SDBD,       0,      RR(02C0,0,[2#]) CST([1=],[3#])  },
    /* SUB  5   */  {   SDBD,       0,      RR(0300,0,[2#]) CST([1=],[3#])  },
    /* CMP  6   */  {   SDBD,       0,      RR(0340,0,[2#]) CST([1=],[3#])  },
    /* AND  7   */  {   SDBD,       0,      RR(0380,0,[2#]) CST([1=],[3#])  },
    /* XOR  8   */  {   SDBD,       0,      RR(03C0,0,[2#]) CST([1=],[3#])  },
    
    /* MVO@ 9   */  {   SDBD,       0,      MVO_OK
                                            RR(0240,[2#],[1#])              },
    /* MVI@ 10  */  {   IND_RG,     IND_RG, RR(0280,[1#],[2#])              },
    /* ADD@ 11  */  {   IND_RG,     IND_RG, RR(02C0,[1#],[2#])              },
    /* SUB@ 12  */  {   IND_RG,     IND_RG, RR(0300,[1#],[2#])              },
    /* CMP@ 13  */  {   IND_RG,     IND_RG, RR(0340,[1#],[2#])              },
    /* AND@ 14  */  {   IND_RG,     IND_RG, RR(0380,[1#],[2#])              },
    /* XOR@ 15  */  {   IND_RG,     IND_RG, RR(03C0,[1#],[2#])              },

    /* MVOI 16  */  {   SDBD,       0,      RR(0240,7,[1#]) CST([2=],[3#])  },
    /* MVII 17  */  {   SDBD,       0,      RR(0280,7,[2#]) CST([1=],[3#])  },
    /* MVII 18  */  {   SDBD,       SDBD,   RR(0280,7,[2#]) DBD([1=])       },
    /* ADDI 19  */  {   SDBD,       0,      RR(02C0,7,[2#]) CST([1=],[3#])  },
    /* ADDI 20  */  {   SDBD,       SDBD,   RR(02C0,7,[2#]) DBD([1=])       },
    /* SUBI 21  */  {   SDBD,       0,      RR(0300,7,[2#]) CST([1=],[3#])  },
    /* SUBI 22  */  {   SDBD,       SDBD,   RR(0300,7,[2#]) DBD([1=])       },
    /* CMPI 23  */  {   SDBD,       0,      RR(0340,7,[2#]) CST([1=],[3#])  },
    /* CMPI 24  */  {   SDBD,       SDBD,   RR(0340,7,[2#]) DBD([1=])       },
    /* ANDI 25  */  {   SDBD,       0,      RR(0380,7,[2#]) CST([1=],[3#])  },
    /* ANDI 26  */  {   SDBD,       SDBD,   RR(0380,7,[2#]) DBD([1=])       },
    /* XORI 27  */  {   SDBD,       0,      RR(03C0,7,[2#]) CST([1=],[3#])  },
    /* XORI 28  */  {   SDBD,       SDBD,   RR(03C0,7,[2#]) DBD([1=])       },

    /* MOVR 29  */  {   SDBD,       0,      RR(0080,[1#],[2#])              },
    /* ADDR 30  */  {   SDBD,       0,      RR(00C0,[1#],[2#])              },
    /* SUBR 31  */  {   SDBD,       0,      RR(0100,[1#],[2#])              },
    /* CMPR 32  */  {   SDBD,       0,      RR(0140,[1#],[2#])              },
    /* ANDR 33  */  {   SDBD,       0,      RR(0180,[1#],[2#])              },
    /* XORR 34  */  {   SDBD,       0,      RR(01C0,[1#],[2#])              },

    /* B    35  */  {   SDBD,       0,      BR(0,[1=],[3#])                 },
    /* BC   36  */  {   SDBD,       0,      BR(1,[1=],[3#])                 },
    /* BOV  37  */  {   SDBD,       0,      BR(2,[1=],[3#])                 },
    /* BPL  38  */  {   SDBD,       0,      BR(3,[1=],[3#])                 },
    /* BEQ  39  */  {   SDBD,       0,      BR(4,[1=],[3#])                 },
    /* BLT  40  */  {   SDBD,       0,      BR(5,[1=],[3#])                 },
    /* BLE  41  */  {   SDBD,       0,      BR(6,[1=],[3#])                 },
    /* BUSC 42  */  {   SDBD,       0,      BR(7,[1=],[3#])                 },
    /* NOPP 43  */  {   SDBD,       0,      BR(8,[1=],[3#])                 },
    /* BNC  44  */  {   SDBD,       0,      BR(9,[1=],[3#])                 },
    /* BNOV 45  */  {   SDBD,       0,      BR(A,[1=],[3#])                 },
    /* BMI  46  */  {   SDBD,       0,      BR(B,[1=],[3#])                 },
    /* BNEQ 47  */  {   SDBD,       0,      BR(C,[1=],[3#])                 },
    /* BGE  48  */  {   SDBD,       0,      BR(D,[1=],[3#])                 },
    /* BGT  49  */  {   SDBD,       0,      BR(E,[1=],[3#])                 },
    /* BESC 50  */  {   SDBD,       0,      BR(F,[1=],[3#])                 },

    /* SWAP 51  */  {   SDBD|SHF_RG,SHF_RG, SH(0,0,[1#])                    },
    /* SWAP 52  */  {   SDBD|SHF_RG,SHF_RG, SH(0,[2=].1-,[1#])              },
    /* SLL  53  */  {   SDBD|SHF_RG,SHF_RG, SH(1,0,[1#])                    },
    /* SLL  54  */  {   SDBD|SHF_RG,SHF_RG, SH(1,[2=].1-,[1#])              },
    /* RLC  55  */  {   SDBD|SHF_RG,SHF_RG, SH(2,0,[1#])                    },
    /* RLC  56  */  {   SDBD|SHF_RG,SHF_RG, SH(2,[2=].1-,[1#])              },
    /* SLLC 57  */  {   SDBD|SHF_RG,SHF_RG, SH(3,0,[1#])                    },
    /* SLLC 58  */  {   SDBD|SHF_RG,SHF_RG, SH(3,[2=].1-,[1#])              },
    /* SLR  59  */  {   SDBD|SHF_RG,SHF_RG, SH(4,0,[1#])                    },
    /* SLR  60  */  {   SDBD|SHF_RG,SHF_RG, SH(4,[2=].1-,[1#])              },
    /* SAR  61  */  {   SDBD|SHF_RG,SHF_RG, SH(5,0,[1#])                    },
    /* SAR  62  */  {   SDBD|SHF_RG,SHF_RG, SH(5,[2=].1-,[1#])              },
    /* RRC  63  */  {   SDBD|SHF_RG,SHF_RG, SH(6,0,[1#])                    },
    /* RRC  64  */  {   SDBD|SHF_RG,SHF_RG, SH(6,[2=].1-,[1#])              },
    /* SARC 65  */  {   SDBD|SHF_RG,SHF_RG, SH(7,0,[1#])                    },
    /* SARC 66  */  {   SDBD|SHF_RG,SHF_RG, SH(7,[2=].1-,[1#])              },

    /* NOP  67  */  {   SDBD,       0,      "0034x"                         },
    /* SIN  68  */  {   SDBD,       0,      "0036x"                         },

    /* J    69  */  {   SDBD,       0,      JSR(3,0,[1=])                   },
    /* JE   70  */  {   SDBD,       0,      JSR(3,1,[1=])                   },
    /* JD   71  */  {   SDBD,       0,      JSR(3,2,[1=])                   },
    /* JSR  72  */  {   SDBD|JSR_RG,JSR_RG, JSR([1#],0,[2=])                },
    /* JSRE 73  */  {   SDBD|JSR_RG,JSR_RG, JSR([1#],1,[2=])                },
    /* JSRD 74  */  {   SDBD|JSR_RG,JSR_RG, JSR([1#],2,[2=])                },

    /* INCR 75  */  {   SDBD,       0,      SR(1,[1#])                      },
    /* DECR 76  */  {   SDBD,       0,      SR(2,[1#])                      },
    /* COMR 77  */  {   SDBD,       0,      SR(3,[1#])                      },
    /* NEGR 78  */  {   SDBD,       0,      SR(4,[1#])                      },
    /* ADCR 79  */  {   SDBD,       0,      SR(5,[1#])                      },
    /* GSWD 80  */  {   SDBD|SHF_RG,SHF_RG, SR(6,[1#])                      },
    /* RSWD 81  */  {   SDBD,       0,      SR(7,[1#])                      },

    /* HLT  82  */  {   SDBD,       0,      "0000x"                         },
    /* SDBD 83  */  {   SDBD,       0,      "0001x"                         },
    /* EIS  84  */  {   SDBD,       0,      "0002x"                         },
    /* DIS  85  */  {   SDBD,       0,      "0003x"                         },
    /* TCI  86  */  {   SDBD,       0,      "0005x"                         },
    /* CLRC 87  */  {   SDBD,       0,      "0006x"                         },
    /* SETC 88  */  {   SDBD,       0,      "0007x"                         },

    /* TSTR 89  */  {   SDBD,       0,      RR(0080,[1#],[1#])              },
    /* CLRR 90  */  {   SDBD,       0,      RR(01C0,[1#],[1#])              },
    /* PSHR 91  */  {   SDBD,       0,      RR(0240,6,[1#])                 },
    /* PULR 92  */  {   0,          0,      RR(0280,6,[1#])                 },
    /* JR   93  */  {   SDBD,       0,      RR(0080,[1#],7)                 },
    /* CALL 94  */  {   SDBD,       0,      JSR(5,0,[1=])                   },
    /* BEGIN 95 */  {   SDBD,       0,      RR(0240,6,5)                    },
    /* RETURN 96*/  {   SDBD,       0,      RR(0280,6,7)                    },

    /* NOPP 97  */  {   SDBD,       0,      "0208x0000x"                    },
    /* NOP2 98  */  {   SDBD,       0,      "0035x"                         },
    /* SIN2 99  */  {   SDBD,       0,      "0037x"                         },

    /* BESC 100 */  {   SDBD,       0,      BX([4#],[1=],[3#])              },

    /* end      */  {   0,          0,      "[Xinvalid opcode"              },
};

#define NUMOPCODE (sizeof(optab)/sizeof(struct opsym))

int gnumopcode = NUMOPCODE;
int ophashlnk[NUMOPCODE];

/* ======================================================================== */
/*  End of file:  fraptabdef.c                                              */
/* ======================================================================== */

