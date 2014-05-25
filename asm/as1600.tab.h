
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
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

/* Line 1676 of yacc.c  */
#line 621 "asm/as1600_real.y"

    int             intv;
    int             longv;
    char            *strng;
    struct symel    *symb;
    struct slidx    slidx;



/* Line 1676 of yacc.c  */
#line 132 "asm/as1600.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


