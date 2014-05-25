/*
 * ============================================================================
 *  Title:    Event Subsystem Tables
 *  Author:   J. Zbiciak
 *  $Id: event_tbl.h,v 1.2 2001/02/03 02:31:50 im14u2c Exp $
 * ============================================================================
 * ============================================================================
 * ============================================================================
 */

#ifndef _EVENT_TBL_H_
#define _EVENT_TBL_H_

/*
 * ============================================================================
 *  EVENT_NUM_T      -- An enumeration of all of the event numbers supported.
 * ============================================================================
 */
typedef enum
{
    /* -------------------------------------------------------------------- */
    /*  Events that correspond to SDL Key events                            */
    /* -------------------------------------------------------------------- */
    EVENT_BACKSPACE     =   SDLK_BACKSPACE,
    EVENT_TAB           =   SDLK_TAB,
    EVENT_CLEAR         =   SDLK_CLEAR,
    EVENT_RETURN        =   SDLK_RETURN,
    EVENT_PAUSE         =   SDLK_PAUSE,
    EVENT_ESCAPE        =   SDLK_ESCAPE,
    EVENT_SPACE         =   SDLK_SPACE,
    EVENT_EXCLAIM       =   SDLK_EXCLAIM,
    EVENT_QUOTEDBL      =   SDLK_QUOTEDBL,
    EVENT_HASH          =   SDLK_HASH,
    EVENT_DOLLAR        =   SDLK_DOLLAR,
    EVENT_AMPERSAND     =   SDLK_AMPERSAND,
    EVENT_QUOTE         =   SDLK_QUOTE,
    EVENT_LEFTPAREN     =   SDLK_LEFTPAREN,
    EVENT_RIGHTPAREN    =   SDLK_RIGHTPAREN,
    EVENT_ASTERISK      =   SDLK_ASTERISK,
    EVENT_PLUS          =   SDLK_PLUS,
    EVENT_COMMA         =   SDLK_COMMA,
    EVENT_MINUS         =   SDLK_MINUS,
    EVENT_PERIOD        =   SDLK_PERIOD,
    EVENT_SLASH         =   SDLK_SLASH,
    EVENT_0             =   SDLK_0,
    EVENT_1             =   SDLK_1,
    EVENT_2             =   SDLK_2,
    EVENT_3             =   SDLK_3,
    EVENT_4             =   SDLK_4,
    EVENT_5             =   SDLK_5,
    EVENT_6             =   SDLK_6,
    EVENT_7             =   SDLK_7,
    EVENT_8             =   SDLK_8,
    EVENT_9             =   SDLK_9,
    EVENT_COLON         =   SDLK_COLON,
    EVENT_SEMICOLON     =   SDLK_SEMICOLON,
    EVENT_LESS          =   SDLK_LESS,
    EVENT_EQUALS        =   SDLK_EQUALS,
    EVENT_GREATER       =   SDLK_GREATER,
    EVENT_QUESTION      =   SDLK_QUESTION,
    EVENT_AT            =   SDLK_AT,
    EVENT_LEFTBRACKET   =   SDLK_LEFTBRACKET,
    EVENT_BACKSLASH     =   SDLK_BACKSLASH,
    EVENT_RIGHTBRACKET  =   SDLK_RIGHTBRACKET,
    EVENT_CARET         =   SDLK_CARET,
    EVENT_UNDERSCORE    =   SDLK_UNDERSCORE,
    EVENT_BACKQUOTE     =   SDLK_BACKQUOTE,
    EVENT_a             =   SDLK_a,
    EVENT_b             =   SDLK_b,
    EVENT_c             =   SDLK_c,
    EVENT_d             =   SDLK_d,
    EVENT_e             =   SDLK_e,
    EVENT_f             =   SDLK_f,
    EVENT_g             =   SDLK_g,
    EVENT_h             =   SDLK_h,
    EVENT_i             =   SDLK_i,
    EVENT_j             =   SDLK_j,
    EVENT_k             =   SDLK_k,
    EVENT_l             =   SDLK_l,
    EVENT_m             =   SDLK_m,
    EVENT_n             =   SDLK_n,
    EVENT_o             =   SDLK_o,
    EVENT_p             =   SDLK_p,
    EVENT_q             =   SDLK_q,
    EVENT_r             =   SDLK_r,
    EVENT_s             =   SDLK_s,
    EVENT_t             =   SDLK_t,
    EVENT_u             =   SDLK_u,
    EVENT_v             =   SDLK_v,
    EVENT_w             =   SDLK_w,
    EVENT_x             =   SDLK_x,
    EVENT_y             =   SDLK_y,
    EVENT_z             =   SDLK_z,
    EVENT_DELETE        =   SDLK_DELETE,
#if 1
    EVENT_WORLD_0       =   SDLK_WORLD_0,
    EVENT_WORLD_1       =   SDLK_WORLD_1,
    EVENT_WORLD_2       =   SDLK_WORLD_2,
    EVENT_WORLD_3       =   SDLK_WORLD_3,
    EVENT_WORLD_4       =   SDLK_WORLD_4,
    EVENT_WORLD_5       =   SDLK_WORLD_5,
    EVENT_WORLD_6       =   SDLK_WORLD_6,
    EVENT_WORLD_7       =   SDLK_WORLD_7,
    EVENT_WORLD_8       =   SDLK_WORLD_8,
    EVENT_WORLD_9       =   SDLK_WORLD_9,
    EVENT_WORLD_10      =   SDLK_WORLD_10,
    EVENT_WORLD_11      =   SDLK_WORLD_11,
    EVENT_WORLD_12      =   SDLK_WORLD_12,
    EVENT_WORLD_13      =   SDLK_WORLD_13,
    EVENT_WORLD_14      =   SDLK_WORLD_14,
    EVENT_WORLD_15      =   SDLK_WORLD_15,
    EVENT_WORLD_16      =   SDLK_WORLD_16,
    EVENT_WORLD_17      =   SDLK_WORLD_17,
    EVENT_WORLD_18      =   SDLK_WORLD_18,
    EVENT_WORLD_19      =   SDLK_WORLD_19,
    EVENT_WORLD_20      =   SDLK_WORLD_20,
    EVENT_WORLD_21      =   SDLK_WORLD_21,
    EVENT_WORLD_22      =   SDLK_WORLD_22,
    EVENT_WORLD_23      =   SDLK_WORLD_23,
    EVENT_WORLD_24      =   SDLK_WORLD_24,
    EVENT_WORLD_25      =   SDLK_WORLD_25,
    EVENT_WORLD_26      =   SDLK_WORLD_26,
    EVENT_WORLD_27      =   SDLK_WORLD_27,
    EVENT_WORLD_28      =   SDLK_WORLD_28,
    EVENT_WORLD_29      =   SDLK_WORLD_29,
    EVENT_WORLD_30      =   SDLK_WORLD_30,
    EVENT_WORLD_31      =   SDLK_WORLD_31,
    EVENT_WORLD_32      =   SDLK_WORLD_32,
    EVENT_WORLD_33      =   SDLK_WORLD_33,
    EVENT_WORLD_34      =   SDLK_WORLD_34,
    EVENT_WORLD_35      =   SDLK_WORLD_35,
    EVENT_WORLD_36      =   SDLK_WORLD_36,
    EVENT_WORLD_37      =   SDLK_WORLD_37,
    EVENT_WORLD_38      =   SDLK_WORLD_38,
    EVENT_WORLD_39      =   SDLK_WORLD_39,
    EVENT_WORLD_40      =   SDLK_WORLD_40,
    EVENT_WORLD_41      =   SDLK_WORLD_41,
    EVENT_WORLD_42      =   SDLK_WORLD_42,
    EVENT_WORLD_43      =   SDLK_WORLD_43,
    EVENT_WORLD_44      =   SDLK_WORLD_44,
    EVENT_WORLD_45      =   SDLK_WORLD_45,
    EVENT_WORLD_46      =   SDLK_WORLD_46,
    EVENT_WORLD_47      =   SDLK_WORLD_47,
    EVENT_WORLD_48      =   SDLK_WORLD_48,
    EVENT_WORLD_49      =   SDLK_WORLD_49,
    EVENT_WORLD_50      =   SDLK_WORLD_50,
    EVENT_WORLD_51      =   SDLK_WORLD_51,
    EVENT_WORLD_52      =   SDLK_WORLD_52,
    EVENT_WORLD_53      =   SDLK_WORLD_53,
    EVENT_WORLD_54      =   SDLK_WORLD_54,
    EVENT_WORLD_55      =   SDLK_WORLD_55,
    EVENT_WORLD_56      =   SDLK_WORLD_56,
    EVENT_WORLD_57      =   SDLK_WORLD_57,
    EVENT_WORLD_58      =   SDLK_WORLD_58,
    EVENT_WORLD_59      =   SDLK_WORLD_59,
    EVENT_WORLD_60      =   SDLK_WORLD_60,
    EVENT_WORLD_61      =   SDLK_WORLD_61,
    EVENT_WORLD_62      =   SDLK_WORLD_62,
    EVENT_WORLD_63      =   SDLK_WORLD_63,
    EVENT_WORLD_64      =   SDLK_WORLD_64,
    EVENT_WORLD_65      =   SDLK_WORLD_65,
    EVENT_WORLD_66      =   SDLK_WORLD_66,
    EVENT_WORLD_67      =   SDLK_WORLD_67,
    EVENT_WORLD_68      =   SDLK_WORLD_68,
    EVENT_WORLD_69      =   SDLK_WORLD_69,
    EVENT_WORLD_70      =   SDLK_WORLD_70,
    EVENT_WORLD_71      =   SDLK_WORLD_71,
    EVENT_WORLD_72      =   SDLK_WORLD_72,
    EVENT_WORLD_73      =   SDLK_WORLD_73,
    EVENT_WORLD_74      =   SDLK_WORLD_74,
    EVENT_WORLD_75      =   SDLK_WORLD_75,
    EVENT_WORLD_76      =   SDLK_WORLD_76,
    EVENT_WORLD_77      =   SDLK_WORLD_77,
    EVENT_WORLD_78      =   SDLK_WORLD_78,
    EVENT_WORLD_79      =   SDLK_WORLD_79,
    EVENT_WORLD_80      =   SDLK_WORLD_80,
    EVENT_WORLD_81      =   SDLK_WORLD_81,
    EVENT_WORLD_82      =   SDLK_WORLD_82,
    EVENT_WORLD_83      =   SDLK_WORLD_83,
    EVENT_WORLD_84      =   SDLK_WORLD_84,
    EVENT_WORLD_85      =   SDLK_WORLD_85,
    EVENT_WORLD_86      =   SDLK_WORLD_86,
    EVENT_WORLD_87      =   SDLK_WORLD_87,
    EVENT_WORLD_88      =   SDLK_WORLD_88,
    EVENT_WORLD_89      =   SDLK_WORLD_89,
    EVENT_WORLD_90      =   SDLK_WORLD_90,
    EVENT_WORLD_91      =   SDLK_WORLD_91,
    EVENT_WORLD_92      =   SDLK_WORLD_92,
    EVENT_WORLD_93      =   SDLK_WORLD_93,
    EVENT_WORLD_94      =   SDLK_WORLD_94,
    EVENT_WORLD_95      =   SDLK_WORLD_95,
#endif
    EVENT_KP0           =   SDLK_KP0,
    EVENT_KP1           =   SDLK_KP1,
    EVENT_KP2           =   SDLK_KP2,
    EVENT_KP3           =   SDLK_KP3,
    EVENT_KP4           =   SDLK_KP4,
    EVENT_KP5           =   SDLK_KP5,
    EVENT_KP6           =   SDLK_KP6,
    EVENT_KP7           =   SDLK_KP7,
    EVENT_KP8           =   SDLK_KP8,
    EVENT_KP9           =   SDLK_KP9,
    EVENT_KP_PERIOD     =   SDLK_KP_PERIOD,
    EVENT_KP_DIVIDE     =   SDLK_KP_DIVIDE,
    EVENT_KP_MULTIPLY   =   SDLK_KP_MULTIPLY,
    EVENT_KP_MINUS      =   SDLK_KP_MINUS,
    EVENT_KP_PLUS       =   SDLK_KP_PLUS,
    EVENT_KP_ENTER      =   SDLK_KP_ENTER,
    EVENT_KP_EQUALS     =   SDLK_KP_EQUALS,
    EVENT_UP            =   SDLK_UP,
    EVENT_DOWN          =   SDLK_DOWN,
    EVENT_RIGHT         =   SDLK_RIGHT,
    EVENT_LEFT          =   SDLK_LEFT,
    EVENT_INSERT        =   SDLK_INSERT,
    EVENT_HOME          =   SDLK_HOME,
    EVENT_END           =   SDLK_END,
    EVENT_PAGEUP        =   SDLK_PAGEUP,
    EVENT_PAGEDOWN      =   SDLK_PAGEDOWN,
    EVENT_F1            =   SDLK_F1,
    EVENT_F2            =   SDLK_F2,
    EVENT_F3            =   SDLK_F3,
    EVENT_F4            =   SDLK_F4,
    EVENT_F5            =   SDLK_F5,
    EVENT_F6            =   SDLK_F6,
    EVENT_F7            =   SDLK_F7,
    EVENT_F8            =   SDLK_F8,
    EVENT_F9            =   SDLK_F9,
    EVENT_F10           =   SDLK_F10,
    EVENT_F11           =   SDLK_F11,
    EVENT_F12           =   SDLK_F12,
    EVENT_F13           =   SDLK_F13,
    EVENT_F14           =   SDLK_F14,
    EVENT_F15           =   SDLK_F15,
    EVENT_NUMLOCK       =   SDLK_NUMLOCK,
    EVENT_CAPSLOCK      =   SDLK_CAPSLOCK,
    EVENT_SCROLLOCK     =   SDLK_SCROLLOCK,
    EVENT_RSHIFT        =   SDLK_RSHIFT,
    EVENT_LSHIFT        =   SDLK_LSHIFT,
    EVENT_RCTRL         =   SDLK_RCTRL,
    EVENT_LCTRL         =   SDLK_LCTRL,
    EVENT_RALT          =   SDLK_RALT,
    EVENT_LALT          =   SDLK_LALT,
    EVENT_RMETA         =   SDLK_RMETA,
    EVENT_LMETA         =   SDLK_LMETA,
    EVENT_LSUPER        =   SDLK_LSUPER,
    EVENT_RSUPER        =   SDLK_RSUPER,
    EVENT_MODE          =   SDLK_MODE,
    EVENT_COMPOSE       =   SDLK_COMPOSE,
    EVENT_HELP          =   SDLK_HELP,
    EVENT_PRINT         =   SDLK_PRINT,
    EVENT_SYSREQ        =   SDLK_SYSREQ,
    EVENT_BREAK         =   SDLK_BREAK,
    EVENT_MENU          =   SDLK_MENU,
    EVENT_POWER         =   SDLK_POWER,
    EVENT_EURO          =   SDLK_EURO,
    EVENT_UNDO          =   SDLK_UNDO,


    /* -------------------------------------------------------------------- */
    /*  Spacer event number, just to ensure that our EVENT numbers don't    */
    /*  overlap SDL's keysym numbers.                                       */
    /* -------------------------------------------------------------------- */
    EVENT_UNUSED        =   SDLK_LAST + 32,

    /* -------------------------------------------------------------------- */
    /*  COMBO events -- these are synthesized out of two other events       */
    /*  Note that event numbers below COMBO0 are OK for combos.  Events     */
    /*  greater than or equal to COMBO0 cannot be part of a combo.          */
    /*                                                                      */
    /*  Currently, that excludes all joystick and "system" events from      */
    /*  combos.  We may consider adding joystick button events someday.     */
    /* -------------------------------------------------------------------- */
    EVENT_COMBO0,
    EVENT_COMBO1,
    EVENT_COMBO2,
    EVENT_COMBO3,
    EVENT_COMBO4,
    EVENT_COMBO5,
    EVENT_COMBO6,
    EVENT_COMBO7,
    EVENT_COMBO8,
    EVENT_COMBO9,
    EVENT_COMBO10,
    EVENT_COMBO11,
    EVENT_COMBO12,
    EVENT_COMBO13,
    EVENT_COMBO14,
    EVENT_COMBO15,
    EVENT_COMBO16,
    EVENT_COMBO17,
    EVENT_COMBO18,
    EVENT_COMBO19,
    EVENT_COMBO20,
    EVENT_COMBO21,
    EVENT_COMBO22,
    EVENT_COMBO23,
    EVENT_COMBO24,
    EVENT_COMBO25,
    EVENT_COMBO26,
    EVENT_COMBO27,
    EVENT_COMBO28,
    EVENT_COMBO29,
    EVENT_COMBO30,
    EVENT_COMBO31,

    /* -------------------------------------------------------------------- */
    /*  The QUIT event, which corresponds to SDLQuit                        */
    /* -------------------------------------------------------------------- */
    EVENT_QUIT,

    /* -------------------------------------------------------------------- */
    /*  The HIDE event, which corresponds to Activate/Deactivate            */
    /* -------------------------------------------------------------------- */
    EVENT_HIDE,

    /* -------------------------------------------------------------------- */
    /*  Note:  The joystick event numbers are laid out in a particular      */
    /*  pattern.  The joystick event decoder expects that the direction     */
    /*  events start with EVENT_JSx_E and work their way CCW through all    */
    /*  16 supported directions with adjacent event #'s.  Similarly,        */
    /*  the joystick decoder expects that the 32 button events start        */
    /*  with EVENT_JSx_BTN_00 with adjacent event #'s.                      */
    /* -------------------------------------------------------------------- */

    /* -------------------------------------------------------------------- */\
    /*  The 16 joystick directions that we resolve, on joystick N.          */\
    /* -------------------------------------------------------------------- */\
#define JOY_DIR_DECL(n) \
    EVENT_##n##_E, EVENT_##n##_ENE, EVENT_##n##_NE, EVENT_##n##_NNE,          \
    EVENT_##n##_N, EVENT_##n##_NNW, EVENT_##n##_NW, EVENT_##n##_WNW,          \
    EVENT_##n##_W, EVENT_##n##_WSW, EVENT_##n##_SW, EVENT_##n##_SSW,          \
    EVENT_##n##_S, EVENT_##n##_SSE, EVENT_##n##_SE, EVENT_##n##_ESE

    /* -------------------------------------------------------------------- */\
    /*  The joystick buttons (up to 32)                                     */\
    /* -------------------------------------------------------------------- */\
#define JOY_BTN_DECL(n) \
EVENT_##n##_BTN_00,EVENT_##n##_BTN_01,EVENT_##n##_BTN_02,EVENT_##n##_BTN_03,  \
EVENT_##n##_BTN_04,EVENT_##n##_BTN_05,EVENT_##n##_BTN_06,EVENT_##n##_BTN_07,  \
EVENT_##n##_BTN_08,EVENT_##n##_BTN_09,EVENT_##n##_BTN_10,EVENT_##n##_BTN_11,  \
EVENT_##n##_BTN_12,EVENT_##n##_BTN_13,EVENT_##n##_BTN_14,EVENT_##n##_BTN_15,  \
EVENT_##n##_BTN_16,EVENT_##n##_BTN_17,EVENT_##n##_BTN_18,EVENT_##n##_BTN_19,  \
EVENT_##n##_BTN_20,EVENT_##n##_BTN_21,EVENT_##n##_BTN_22,EVENT_##n##_BTN_23,  \
EVENT_##n##_BTN_24,EVENT_##n##_BTN_25,EVENT_##n##_BTN_26,EVENT_##n##_BTN_27,  \
EVENT_##n##_BTN_28,EVENT_##n##_BTN_29,EVENT_##n##_BTN_30,EVENT_##n##_BTN_31

    /* -------------------------------------------------------------------- */\
    /*  The joystick hats (up to 4)                                         */\
    /* -------------------------------------------------------------------- */\
#define JOY_HAT_DECL(n) \
EVENT_##n##_HAT0_E,EVENT_##n##_HAT0_NE,EVENT_##n##_HAT0_N,EVENT_##n##_HAT0_NW,\
EVENT_##n##_HAT0_W,EVENT_##n##_HAT0_SW,EVENT_##n##_HAT0_S,EVENT_##n##_HAT0_SE,\
EVENT_##n##_HAT1_E,EVENT_##n##_HAT1_NE,EVENT_##n##_HAT1_N,EVENT_##n##_HAT1_NW,\
EVENT_##n##_HAT1_W,EVENT_##n##_HAT1_SW,EVENT_##n##_HAT1_S,EVENT_##n##_HAT1_SE,\
EVENT_##n##_HAT2_E,EVENT_##n##_HAT2_NE,EVENT_##n##_HAT2_N,EVENT_##n##_HAT2_NW,\
EVENT_##n##_HAT2_W,EVENT_##n##_HAT2_SW,EVENT_##n##_HAT2_S,EVENT_##n##_HAT2_SE,\
EVENT_##n##_HAT3_E,EVENT_##n##_HAT3_NE,EVENT_##n##_HAT3_N,EVENT_##n##_HAT3_NW,\
EVENT_##n##_HAT3_W,EVENT_##n##_HAT3_SW,EVENT_##n##_HAT3_S,EVENT_##n##_HAT3_SE 

    JOY_DIR_DECL(JS0),    JOY_BTN_DECL(JS0),    JOY_HAT_DECL(JS0),
    JOY_DIR_DECL(JS1),    JOY_BTN_DECL(JS1),    JOY_HAT_DECL(JS1),
    JOY_DIR_DECL(JS2),    JOY_BTN_DECL(JS2),    JOY_HAT_DECL(JS2),
    JOY_DIR_DECL(JS3),    JOY_BTN_DECL(JS3),    JOY_HAT_DECL(JS3),

    /* -------------------------------------------------------------------- */
    /*  Mice look a little like joysticks, event wise.  No hats, though.    */
    /* -------------------------------------------------------------------- */
    JOY_DIR_DECL(MOUSE),  JOY_BTN_DECL(MOUSE),

    /* -------------------------------------------------------------------- */
    /*  The last event....                                                  */
    /* -------------------------------------------------------------------- */
    EVENT_LAST,

    /* -------------------------------------------------------------------- */
    /*  And if we just want to ignore an event, we set it to this.          */
    /* -------------------------------------------------------------------- */
    EVENT_IGNORE
} event_num_t;

extern event_name_t event_names[];
extern const int event_name_count;

#endif /*EVENT_TBL_H*/
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
