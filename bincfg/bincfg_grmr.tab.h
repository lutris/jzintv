
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
     TOK_SEC_BANKSWITCH = 258,
     TOK_SEC_MAPPING = 259,
     TOK_SEC_ECSBANK = 260,
     TOK_SEC_MEMATTR = 261,
     TOK_SEC_PRELOAD = 262,
     TOK_SEC_MACRO = 263,
     TOK_SEC_VARS = 264,
     TOK_SEC_JOYSTICK = 265,
     TOK_SEC_KEYS = 266,
     TOK_SEC_CAPSLOCK = 267,
     TOK_SEC_NUMLOCK = 268,
     TOK_SEC_SCROLLLOCK = 269,
     TOK_SEC_DISASM = 270,
     TOK_SEC_VOICES = 271,
     TOK_SEC_UNKNOWN = 272,
     TOK_RAM = 273,
     TOK_ROM = 274,
     TOK_WOM = 275,
     TOK_PAGE = 276,
     TOK_MAC_QUIET = 277,
     TOK_MAC_REG = 278,
     TOK_MAC_AHEAD = 279,
     TOK_MAC_BLANK = 280,
     TOK_MAC_INSPECT = 281,
     TOK_MAC_LOAD = 282,
     TOK_MAC_RUN = 283,
     TOK_MAC_POKE = 284,
     TOK_MAC_RUNTO = 285,
     TOK_MAC_TRACE = 286,
     TOK_MAC_VIEW = 287,
     TOK_MAC_WATCH = 288,
     TOK_DECONLY = 289,
     TOK_DEC = 290,
     TOK_HEX = 291,
     TOK_NAME = 292,
     TOK_ERROR_BAD = 293,
     TOK_ERROR_OOM = 294
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 102 "bincfg/bincfg_grmr_real.y"

    int                 intv;
    char                *strv;
    bc_varlike_t        varlike;
    bc_varlike_types_t  varlike_type;
    bc_var_t            *var_list;
    bc_strnum_t         strnum;
    bc_mac_watch_t      mac_watch;
    bc_macro_t          macro;
    bc_macro_t          *macro_list;
    bc_memspan_t        *memspan_list;
    bc_cfgfile_t        *cfgfile;



/* Line 1676 of yacc.c  */
#line 107 "bincfg/bincfg_grmr.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE bc_lval;


