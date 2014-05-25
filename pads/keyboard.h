/*
 * ============================================================================
 *  Title:    Controller pads and Keyboard.
 *  Author:   J. Zbiciak
 *  $Id$
 * ============================================================================
 *  This module implements the ECS keyboard, which hooks to the controller 
 *  pads.
 * ============================================================================
 */

#ifndef PADS_KEYBOARD_H_
#define PADS_KEYBOARD_H_

/* ======================================================================== */
/*  The following enum maps symbolic names for each pseudo-key we support.  */
/*  Pseudo-keys fall into three classes:                                    */
/*                                                                          */
/*   -- Directly mapped keys.  These are keys on the PC keyboard that       */
/*      exactly map to the ECS keyboard.                                    */
/*                                                                          */
/*   -- Pseudo keys.  These keys on the PC side map to some key combo on    */
/*      the ECS keyboard.  Example:  The PC's single-quote/double-quote     */
/*      key maps to either [SHIFT]-[LEFT] or [SHIFT]-[2] depending on       */
/*      whether the PC's shift key is pressed.                              */
/*                                                                          */
/*   -- Extra keys.  These keys did not exist on the ECS keyboard.  They    */
/*      are only useful with a patched ECS ROM.                             */
/* ======================================================================== */


typedef enum
{
    /* -------------------------------------------------------------------- */
    /*  Direct Mappings.                                                    */
    /* -------------------------------------------------------------------- */
    ECSKEY_1_EQUAL = 0,
    ECSKEY_2_DQUOTE,
    ECSKEY_3_HASH,
    ECSKEY_4_DOLLAR,
    ECSKEY_5_PLUS,
    ECSKEY_6_MINUS,
    ECSKEY_7_SLASH,
    ECSKEY_8_STAR,
    ECSKEY_9_LPAREN,
    ECSKEY_0_RPAREN,
    ECSKEY_ESC,

    ECSKEY_CTL,
    ECSKEY_Q, ECSKEY_W, ECSKEY_E, ECSKEY_R, ECSKEY_T,
    ECSKEY_Y, ECSKEY_U, ECSKEY_I, ECSKEY_O, ECSKEY_P,

    ECSKEY_UP_CARET,
    ECSKEY_A, ECSKEY_S, ECSKEY_D, ECSKEY_F, ECSKEY_G,
    ECSKEY_H, ECSKEY_J, ECSKEY_K, ECSKEY_L, ECSKEY_SEMI_COLON,

    ECSKEY_LEFT_PERCENT, ECSKEY_RIGHT_SQUOTE,
    ECSKEY_Z, ECSKEY_X, ECSKEY_C, ECSKEY_V,
    ECSKEY_B, ECSKEY_N, ECSKEY_M,
    ECSKEY_COMMA_LTHAN,  ECSKEY_PERIOD_GTHAN,

    ECSKEY_DOWN_QUESTION, ECSKEY_SHIFT, ECSKEY_SPACE, ECSKEY_RTN
    

    /* -------------------------------------------------------------------- */
    /*  Pseudo Keys.                                                        */
    /* -------------------------------------------------------------------- */
    ECSKEY_1, ECSKEY_2, ECSKEY_3, ECSKEY_4, ECSKEY_5,
    ECSKEY_6, ECSKEY_7, ECSKEY_8, ECSKEY_9, ECSKEY_0,

    ECSKEY_EQUAL,
    ECSKEY_DQUOTE,
    ECSKEY_HASH,
    ECSKEY_DOLLAR,
    ECSKEY_PLUS,
    ECSKEY_MINUS,
    ECSKEY_SLASH,
    ECSKEY_STAR,
    ECSKEY_LPAREN,
    ECSKEY_RPAREN,

    ECSKEY_SEMI,   ECSKEY_COLON,
    ECSKEY_UP,     ECSKEY_CARET,
    ECSKEY_LEFT,   ECSKEY_RIGHT,
    ECSKEY_DOWN,   ECSKEY_PERCENT,
    ECSKEY_SQUOTE, ECSKEY_QUESTION,

    ECSKEY_COMMA,  ECSKEY_GTHAN,
    ECSKEY_PERIOD, ECSKEY_LTHAN,

    /* -------------------------------------------------------------------- */
    /*  Extra Keys -- These don't exist on the real ECS Keyboard.           */
    /* -------------------------------------------------------------------- */
    ECSKEY_AT,
    ECSKEY_AND,
    ECSKEY_UNDER,
    ECSKEY_SLASH,
    ECSKEY_BSLASH,
    ECSKEY_TILDE,
    ECSKEY_PIPE,
    ECSKEY_LBRACKET,
    ECSKEY_RBRACKET,
    ECSKEY_LBRACE,
    ECSKEY_RBRACE,
    ECSKEY_EXCLAIM,
    ECSKEY_TAB
} ecskey_t;

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
/*                 Copyright (c) 2003-+Inf, Joseph Zbiciak                  */
/* ======================================================================== */
