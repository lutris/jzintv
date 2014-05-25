/*
 * ============================================================================
 *  Title:    Event Subsystem Tables
 *  Author:   J. Zbiciak
 *  $Id: event_tbl.c,v 1.3 2001/02/03 02:31:50 im14u2c Exp $
 * ============================================================================
 *  This file contains lookup tables used by the event subsystem.
 *  I split this out into a separate file since this table is impressively
 *  large.
 * ============================================================================
 *  EVENT_NAMES      -- An array of event names that can be mapped.
 * ============================================================================
 */


#include "../config.h"

#ifndef macintosh
# include "sdl.h"
#else
# include "keyboard_macos.h"
#endif

#include "periph/periph.h"
#include "event/event.h"
#include "event/event_tbl.h"

/*
 * ============================================================================
 *  EVENT_NAMES      -- An array of event names that can be mapped.
 * ============================================================================
 */
event_name_t event_names[] =
{
    /* -------------------------------------------------------------------- */
    /*  Events that correspond to SDL Key events                            */
    /* -------------------------------------------------------------------- */
    {   "BACKSPACE",    EVENT_BACKSPACE     },
    {   "TAB",          EVENT_TAB           },
    {   "CLEAR",        EVENT_CLEAR         },
    {   "RETURN",       EVENT_RETURN        },
    {   "PAUSE",        EVENT_PAUSE         },
    {   "ESCAPE",       EVENT_ESCAPE        },
    {   "SPACE",        EVENT_SPACE         },
    {   "EXCLAIM",      EVENT_EXCLAIM       },
    {   "QUOTEDBL",     EVENT_QUOTEDBL      },
    {   "HASH",         EVENT_HASH          },
    {   "DOLLAR",       EVENT_DOLLAR        },
    {   "AMPERSAND",    EVENT_AMPERSAND     },
    {   "QUOTE",        EVENT_QUOTE         },
    {   "LEFTPAREN",    EVENT_LEFTPAREN     },
    {   "RIGHTPAREN",   EVENT_RIGHTPAREN    },
    {   "ASTERISK",     EVENT_ASTERISK      },
    {   "PLUS",         EVENT_PLUS          },
    {   "COMMA",        EVENT_COMMA         },
    {   "MINUS",        EVENT_MINUS         },
    {   "PERIOD",       EVENT_PERIOD        },
    {   "SLASH",        EVENT_SLASH         },
    {   "0",            EVENT_0             },
    {   "1",            EVENT_1             },
    {   "2",            EVENT_2             },
    {   "3",            EVENT_3             },
    {   "4",            EVENT_4             },
    {   "5",            EVENT_5             },
    {   "6",            EVENT_6             },
    {   "7",            EVENT_7             },
    {   "8",            EVENT_8             },
    {   "9",            EVENT_9             },
    {   "COLON",        EVENT_COLON         },
    {   "SEMICOLON",    EVENT_SEMICOLON     },
    {   "LESS",         EVENT_LESS          },
    {   "EQUALS",       EVENT_EQUALS        },
    {   "GREATER",      EVENT_GREATER       },
    {   "QUESTION",     EVENT_QUESTION      },
    {   "AT",           EVENT_AT            },
    {   "LEFTBRACKET",  EVENT_LEFTBRACKET   },
    {   "BACKSLASH",    EVENT_BACKSLASH     },
    {   "RIGHTBRACKET", EVENT_RIGHTBRACKET  },
    {   "CARET",        EVENT_CARET         },
    {   "UNDERSCORE",   EVENT_UNDERSCORE    },
    {   "BACKQUOTE",    EVENT_BACKQUOTE     },
    {   "A",            EVENT_a             },
    {   "B",            EVENT_b             },
    {   "C",            EVENT_c             },
    {   "D",            EVENT_d             },
    {   "E",            EVENT_e             },
    {   "F",            EVENT_f             },
    {   "G",            EVENT_g             },
    {   "H",            EVENT_h             },
    {   "I",            EVENT_i             },
    {   "J",            EVENT_j             },
    {   "K",            EVENT_k             },
    {   "L",            EVENT_l             },
    {   "M",            EVENT_m             },
    {   "N",            EVENT_n             },
    {   "O",            EVENT_o             },
    {   "P",            EVENT_p             },
    {   "Q",            EVENT_q             },
    {   "R",            EVENT_r             },
    {   "S",            EVENT_s             },
    {   "T",            EVENT_t             },
    {   "U",            EVENT_u             },
    {   "V",            EVENT_v             },
    {   "W",            EVENT_w             },
    {   "X",            EVENT_x             },
    {   "Y",            EVENT_y             },
    {   "Z",            EVENT_z             },
    {   "DELETE",       EVENT_DELETE        },
#if 1
    {   "WORLD_0",      EVENT_WORLD_0       },
    {   "WORLD_1",      EVENT_WORLD_1       },
    {   "WORLD_2",      EVENT_WORLD_2       },
    {   "WORLD_3",      EVENT_WORLD_3       },
    {   "WORLD_4",      EVENT_WORLD_4       },
    {   "WORLD_5",      EVENT_WORLD_5       },
    {   "WORLD_6",      EVENT_WORLD_6       },
    {   "WORLD_7",      EVENT_WORLD_7       },
    {   "WORLD_8",      EVENT_WORLD_8       },
    {   "WORLD_9",      EVENT_WORLD_9       },
    {   "WORLD_10",     EVENT_WORLD_10      },
    {   "WORLD_11",     EVENT_WORLD_11      },
    {   "WORLD_12",     EVENT_WORLD_12      },
    {   "WORLD_13",     EVENT_WORLD_13      },
    {   "WORLD_14",     EVENT_WORLD_14      },
    {   "WORLD_15",     EVENT_WORLD_15      },
    {   "WORLD_16",     EVENT_WORLD_16      },
    {   "WORLD_17",     EVENT_WORLD_17      },
    {   "WORLD_18",     EVENT_WORLD_18      },
    {   "WORLD_19",     EVENT_WORLD_19      },
    {   "WORLD_20",     EVENT_WORLD_20      },
    {   "WORLD_21",     EVENT_WORLD_21      },
    {   "WORLD_22",     EVENT_WORLD_22      },
    {   "WORLD_23",     EVENT_WORLD_23      },
    {   "WORLD_24",     EVENT_WORLD_24      },
    {   "WORLD_25",     EVENT_WORLD_25      },
    {   "WORLD_26",     EVENT_WORLD_26      },
    {   "WORLD_27",     EVENT_WORLD_27      },
    {   "WORLD_28",     EVENT_WORLD_28      },
    {   "WORLD_29",     EVENT_WORLD_29      },
    {   "WORLD_30",     EVENT_WORLD_30      },
    {   "WORLD_31",     EVENT_WORLD_31      },
    {   "WORLD_32",     EVENT_WORLD_32      },
    {   "WORLD_33",     EVENT_WORLD_33      },
    {   "WORLD_34",     EVENT_WORLD_34      },
    {   "WORLD_35",     EVENT_WORLD_35      },
    {   "WORLD_36",     EVENT_WORLD_36      },
    {   "WORLD_37",     EVENT_WORLD_37      },
    {   "WORLD_38",     EVENT_WORLD_38      },
    {   "WORLD_39",     EVENT_WORLD_39      },
    {   "WORLD_40",     EVENT_WORLD_40      },
    {   "WORLD_41",     EVENT_WORLD_41      },
    {   "WORLD_42",     EVENT_WORLD_42      },
    {   "WORLD_43",     EVENT_WORLD_43      },
    {   "WORLD_44",     EVENT_WORLD_44      },
    {   "WORLD_45",     EVENT_WORLD_45      },
    {   "WORLD_46",     EVENT_WORLD_46      },
    {   "WORLD_47",     EVENT_WORLD_47      },
    {   "WORLD_48",     EVENT_WORLD_48      },
    {   "WORLD_49",     EVENT_WORLD_49      },
    {   "WORLD_50",     EVENT_WORLD_50      },
    {   "WORLD_51",     EVENT_WORLD_51      },
    {   "WORLD_52",     EVENT_WORLD_52      },
    {   "WORLD_53",     EVENT_WORLD_53      },
    {   "WORLD_54",     EVENT_WORLD_54      },
    {   "WORLD_55",     EVENT_WORLD_55      },
    {   "WORLD_56",     EVENT_WORLD_56      },
    {   "WORLD_57",     EVENT_WORLD_57      },
    {   "WORLD_58",     EVENT_WORLD_58      },
    {   "WORLD_59",     EVENT_WORLD_59      },
    {   "WORLD_60",     EVENT_WORLD_60      },
    {   "WORLD_61",     EVENT_WORLD_61      },
    {   "WORLD_62",     EVENT_WORLD_62      },
    {   "WORLD_63",     EVENT_WORLD_63      },
    {   "WORLD_64",     EVENT_WORLD_64      },
    {   "WORLD_65",     EVENT_WORLD_65      },
    {   "WORLD_66",     EVENT_WORLD_66      },
    {   "WORLD_67",     EVENT_WORLD_67      },
    {   "WORLD_68",     EVENT_WORLD_68      },
    {   "WORLD_69",     EVENT_WORLD_69      },
    {   "WORLD_70",     EVENT_WORLD_70      },
    {   "WORLD_71",     EVENT_WORLD_71      },
    {   "WORLD_72",     EVENT_WORLD_72      },
    {   "WORLD_73",     EVENT_WORLD_73      },
    {   "WORLD_74",     EVENT_WORLD_74      },
    {   "WORLD_75",     EVENT_WORLD_75      },
    {   "WORLD_76",     EVENT_WORLD_76      },
    {   "WORLD_77",     EVENT_WORLD_77      },
    {   "WORLD_78",     EVENT_WORLD_78      },
    {   "WORLD_79",     EVENT_WORLD_79      },
    {   "WORLD_80",     EVENT_WORLD_80      },
    {   "WORLD_81",     EVENT_WORLD_81      },
    {   "WORLD_82",     EVENT_WORLD_82      },
    {   "WORLD_83",     EVENT_WORLD_83      },
    {   "WORLD_84",     EVENT_WORLD_84      },
    {   "WORLD_85",     EVENT_WORLD_85      },
    {   "WORLD_86",     EVENT_WORLD_86      },
    {   "WORLD_87",     EVENT_WORLD_87      },
    {   "WORLD_88",     EVENT_WORLD_88      },
    {   "WORLD_89",     EVENT_WORLD_89      },
    {   "WORLD_90",     EVENT_WORLD_90      },
    {   "WORLD_91",     EVENT_WORLD_91      },
    {   "WORLD_92",     EVENT_WORLD_92      },
    {   "WORLD_93",     EVENT_WORLD_93      },
    {   "WORLD_94",     EVENT_WORLD_94      },
    {   "WORLD_95",     EVENT_WORLD_95      },
#endif
    {   "KP0",          EVENT_KP0           },
    {   "KP1",          EVENT_KP1           },
    {   "KP2",          EVENT_KP2           },
    {   "KP3",          EVENT_KP3           },
    {   "KP4",          EVENT_KP4           },
    {   "KP5",          EVENT_KP5           },
    {   "KP6",          EVENT_KP6           },
    {   "KP7",          EVENT_KP7           },
    {   "KP8",          EVENT_KP8           },
    {   "KP9",          EVENT_KP9           },
    {   "KP_PERIOD",    EVENT_KP_PERIOD     },
    {   "KP_DIVIDE",    EVENT_KP_DIVIDE     },
    {   "KP_MULTIPLY",  EVENT_KP_MULTIPLY   },
    {   "KP_MINUS",     EVENT_KP_MINUS      },
    {   "KP_PLUS",      EVENT_KP_PLUS       },
    {   "KP_ENTER",     EVENT_KP_ENTER      },
    {   "KP_EQUALS",    EVENT_KP_EQUALS     },
    {   "UP",           EVENT_UP            },
    {   "DOWN",         EVENT_DOWN          },
    {   "RIGHT",        EVENT_RIGHT         },
    {   "LEFT",         EVENT_LEFT          },
    {   "INSERT",       EVENT_INSERT        },
    {   "HOME",         EVENT_HOME          },
    {   "END",          EVENT_END           },
    {   "PAGEUP",       EVENT_PAGEUP        },
    {   "PAGEDOWN",     EVENT_PAGEDOWN      },
    {   "F1",           EVENT_F1            },
    {   "F2",           EVENT_F2            },
    {   "F3",           EVENT_F3            },
    {   "F4",           EVENT_F4            },
    {   "F5",           EVENT_F5            },
    {   "F6",           EVENT_F6            },
    {   "F7",           EVENT_F7            },
    {   "F8",           EVENT_F8            },
    {   "F9",           EVENT_F9            },
    {   "F10",          EVENT_F10           },
    {   "F11",          EVENT_F11           },
    {   "F12",          EVENT_F12           },
    {   "F13",          EVENT_F13           },
    {   "F14",          EVENT_F14           },
    {   "F15",          EVENT_F15           },
    {   "NUMLOCK",      EVENT_NUMLOCK       },
    {   "CAPSLOCK",     EVENT_CAPSLOCK      },
    {   "SCROLLOCK",    EVENT_SCROLLOCK     },
    {   "LSHIFT",       EVENT_LSHIFT        },
    {   "RSHIFT",       EVENT_RSHIFT        },
    {   "LCTRL",        EVENT_LCTRL         },
    {   "RCTRL",        EVENT_RCTRL         },
    {   "LALT",         EVENT_LALT          },
    {   "RALT",         EVENT_RALT          },
    {   "LMETA",        EVENT_LMETA         },
    {   "RMETA",        EVENT_RMETA         },
    {   "LSUPER",       EVENT_LSUPER        },
    {   "RSUPER",       EVENT_RSUPER        },
    {   "MODE",         EVENT_MODE          },
    {   "COMPOSE",      EVENT_COMPOSE       },
    {   "HELP",         EVENT_HELP          },
    {   "PRINT",        EVENT_PRINT         },
    {   "SYSREQ",       EVENT_SYSREQ        },
    {   "BREAK",        EVENT_BREAK         },
    {   "MENU",         EVENT_MENU          },
    {   "POWER",        EVENT_POWER         },
    {   "EURO",         EVENT_EURO          },
    {   "UNDO",         EVENT_UNDO          },

    /* -------------------------------------------------------------------- */
    /*  Handy aliases                                                       */
    /* -------------------------------------------------------------------- */
    {   "LWIN",         EVENT_LSUPER        }, /* left Windows key          */
    {   "RWIN",         EVENT_RSUPER        }, /* right Windows key (rare?) */
    {   "ALTGR",        EVENT_MODE          }, /* AltGR key (European?)     */
    {   "LCMD",         EVENT_LMETA         }, /* Mac left CMD key          */
    {   "RCMD",         EVENT_RMETA         }, /* Mac right CMD key         */

    /* -------------------------------------------------------------------- */
    /*  More common aliases for some of the SDLKey names                    */
    /* -------------------------------------------------------------------- */
    {   " ",            EVENT_SPACE         },
    {   "!",            EVENT_EXCLAIM       },
    {   "\"",           EVENT_QUOTEDBL      },
    {   "#",            EVENT_HASH          },
    {   "$",            EVENT_DOLLAR        },
    {   "&",            EVENT_AMPERSAND     },
    {   "'",            EVENT_QUOTE         },
    {   "(",            EVENT_LEFTPAREN     },
    {   ")",            EVENT_RIGHTPAREN    },
    {   "*",            EVENT_ASTERISK      },
    {   "+",            EVENT_PLUS          },
    {   ",",            EVENT_COMMA         },
    {   "-",            EVENT_MINUS         },
    {   ".",            EVENT_PERIOD        },
    {   "/",            EVENT_SLASH         },
    {   ":",            EVENT_COLON         },
    {   ";",            EVENT_SEMICOLON     },
    {   "<",            EVENT_LESS          },
    {   "=",            EVENT_EQUALS        },
    {   ">",            EVENT_GREATER       },
    {   "?",            EVENT_QUESTION      },
    {   "@",            EVENT_AT            },
    {   "[",            EVENT_LEFTBRACKET   },
    {   "\\",           EVENT_BACKSLASH     },
    {   "]",            EVENT_RIGHTBRACKET  },
    {   "^",            EVENT_CARET         },
    {   "_",            EVENT_UNDERSCORE    },
    {   "`",            EVENT_BACKQUOTE     },

    /* -------------------------------------------------------------------- */
    /*  COMBO events -- these are synthesized out of two other events       */
    /* -------------------------------------------------------------------- */
    {   "COMBO0",       EVENT_COMBO0        },
    {   "COMBO1",       EVENT_COMBO1        },
    {   "COMBO2",       EVENT_COMBO2        },
    {   "COMBO3",       EVENT_COMBO3        },
    {   "COMBO4",       EVENT_COMBO4        },
    {   "COMBO5",       EVENT_COMBO5        },
    {   "COMBO6",       EVENT_COMBO6        },
    {   "COMBO7",       EVENT_COMBO7        },
    {   "COMBO8",       EVENT_COMBO8        },
    {   "COMBO9",       EVENT_COMBO9        },
    {   "COMBO10",      EVENT_COMBO10       },
    {   "COMBO11",      EVENT_COMBO11       },
    {   "COMBO12",      EVENT_COMBO12       },
    {   "COMBO13",      EVENT_COMBO13       },
    {   "COMBO14",      EVENT_COMBO14       },
    {   "COMBO15",      EVENT_COMBO15       },
    {   "COMBO16",      EVENT_COMBO16       },
    {   "COMBO17",      EVENT_COMBO17       },
    {   "COMBO18",      EVENT_COMBO18       },
    {   "COMBO19",      EVENT_COMBO19       },
    {   "COMBO20",      EVENT_COMBO20       },
    {   "COMBO21",      EVENT_COMBO21       },
    {   "COMBO22",      EVENT_COMBO22       },
    {   "COMBO23",      EVENT_COMBO23       },
    {   "COMBO24",      EVENT_COMBO24       },
    {   "COMBO25",      EVENT_COMBO25       },
    {   "COMBO26",      EVENT_COMBO26       },
    {   "COMBO27",      EVENT_COMBO27       },
    {   "COMBO28",      EVENT_COMBO28       },
    {   "COMBO29",      EVENT_COMBO29       },
    {   "COMBO30",      EVENT_COMBO30       },
    {   "COMBO31",      EVENT_COMBO31       },

    /* -------------------------------------------------------------------- */
    /*  The QUIT event, which corresponds to SDLQuit                        */
    /* -------------------------------------------------------------------- */
    {   "QUIT",         EVENT_QUIT          },

    /* -------------------------------------------------------------------- */
    /*  The HIDE  event is lets us know when we're iconified or not.        */
    /*  A "HIDE DOWN" event means we're iconified, and a "HIDE UP" event    */
    /*  means we're visible.  I use this to disable graphics updates while  */
    /*  we're iconified.                                                    */
    /* -------------------------------------------------------------------- */
    {   "HIDE",         EVENT_HIDE          },

#define JOY_DIR_EVT_DECL(n) \
    /* -------------------------------------------------------------------- */\
    /*  The 16 joystick directions that we resolve, on joystick 0.          */\
    /* -------------------------------------------------------------------- */\
    {   #n "_E",        EVENT_##n##_E         },                              \
    {   #n "_ENE",      EVENT_##n##_ENE       },                              \
    {   #n "_NE",       EVENT_##n##_NE        },                              \
    {   #n "_NNE",      EVENT_##n##_NNE       },                              \
    {   #n "_N",        EVENT_##n##_N         },                              \
    {   #n "_NNW",      EVENT_##n##_NNW       },                              \
    {   #n "_NW",       EVENT_##n##_NW        },                              \
    {   #n "_WNW",      EVENT_##n##_WNW       },                              \
    {   #n "_W",        EVENT_##n##_W         },                              \
    {   #n "_WSW",      EVENT_##n##_WSW       },                              \
    {   #n "_SW",       EVENT_##n##_SW        },                              \
    {   #n "_SSW",      EVENT_##n##_SSW       },                              \
    {   #n "_S",        EVENT_##n##_S         },                              \
    {   #n "_SSE",      EVENT_##n##_SSE       },                              \
    {   #n "_SE",       EVENT_##n##_SE        },                              \
    {   #n "_ESE",      EVENT_##n##_ESE       }

#define JOY_BTN_EVT_DECL(n) \
    /* -------------------------------------------------------------------- */\
    /*  The joystick buttons (up to 32)                                     */\
    /* -------------------------------------------------------------------- */\
    {   #n "_BTN_00",   EVENT_##n##_BTN_00    },                              \
    {   #n "_BTN_01",   EVENT_##n##_BTN_01    },                              \
    {   #n "_BTN_02",   EVENT_##n##_BTN_02    },                              \
    {   #n "_BTN_03",   EVENT_##n##_BTN_03    },                              \
    {   #n "_BTN_04",   EVENT_##n##_BTN_04    },                              \
    {   #n "_BTN_05",   EVENT_##n##_BTN_05    },                              \
    {   #n "_BTN_06",   EVENT_##n##_BTN_06    },                              \
    {   #n "_BTN_07",   EVENT_##n##_BTN_07    },                              \
    {   #n "_BTN_08",   EVENT_##n##_BTN_08    },                              \
    {   #n "_BTN_09",   EVENT_##n##_BTN_09    },                              \
    {   #n "_BTN_10",   EVENT_##n##_BTN_10    },                              \
    {   #n "_BTN_11",   EVENT_##n##_BTN_11    },                              \
    {   #n "_BTN_12",   EVENT_##n##_BTN_12    },                              \
    {   #n "_BTN_13",   EVENT_##n##_BTN_13    },                              \
    {   #n "_BTN_14",   EVENT_##n##_BTN_14    },                              \
    {   #n "_BTN_15",   EVENT_##n##_BTN_15    },                              \
    {   #n "_BTN_16",   EVENT_##n##_BTN_16    },                              \
    {   #n "_BTN_17",   EVENT_##n##_BTN_17    },                              \
    {   #n "_BTN_18",   EVENT_##n##_BTN_18    },                              \
    {   #n "_BTN_19",   EVENT_##n##_BTN_19    },                              \
    {   #n "_BTN_20",   EVENT_##n##_BTN_20    },                              \
    {   #n "_BTN_21",   EVENT_##n##_BTN_21    },                              \
    {   #n "_BTN_22",   EVENT_##n##_BTN_22    },                              \
    {   #n "_BTN_23",   EVENT_##n##_BTN_23    },                              \
    {   #n "_BTN_24",   EVENT_##n##_BTN_24    },                              \
    {   #n "_BTN_25",   EVENT_##n##_BTN_25    },                              \
    {   #n "_BTN_26",   EVENT_##n##_BTN_26    },                              \
    {   #n "_BTN_27",   EVENT_##n##_BTN_27    },                              \
    {   #n "_BTN_08",   EVENT_##n##_BTN_28    },                              \
    {   #n "_BTN_29",   EVENT_##n##_BTN_29    },                              \
    {   #n "_BTN_30",   EVENT_##n##_BTN_30    },                              \
    {   #n "_BTN_31",   EVENT_##n##_BTN_31    }
                                                                              
#define JOY_HAT_EVT_DECL(n) \
    /* -------------------------------------------------------------------- */\
    /*  The 8 joystick hat dirs on 4 hats that we resolve, on joystick 0.   */\
    /* -------------------------------------------------------------------- */\
    {   #n "_HAT0_E",   EVENT_##n##_HAT0_E    },                              \
    {   #n "_HAT0_NE",  EVENT_##n##_HAT0_NE   },                              \
    {   #n "_HAT0_N",   EVENT_##n##_HAT0_N    },                              \
    {   #n "_HAT0_NW",  EVENT_##n##_HAT0_NW   },                              \
    {   #n "_HAT0_W",   EVENT_##n##_HAT0_W    },                              \
    {   #n "_HAT0_SW",  EVENT_##n##_HAT0_SW   },                              \
    {   #n "_HAT0_S",   EVENT_##n##_HAT0_S    },                              \
    {   #n "_HAT0_SE",  EVENT_##n##_HAT0_SE   },                              \
    {   #n "_HAT1_E",   EVENT_##n##_HAT1_E    },                              \
    {   #n "_HAT1_NE",  EVENT_##n##_HAT1_NE   },                              \
    {   #n "_HAT1_N",   EVENT_##n##_HAT1_N    },                              \
    {   #n "_HAT1_NW",  EVENT_##n##_HAT1_NW   },                              \
    {   #n "_HAT1_W",   EVENT_##n##_HAT1_W    },                              \
    {   #n "_HAT1_SW",  EVENT_##n##_HAT1_SW   },                              \
    {   #n "_HAT1_S",   EVENT_##n##_HAT1_S    },                              \
    {   #n "_HAT1_SE",  EVENT_##n##_HAT1_SE   },                              \
    {   #n "_HAT2_E",   EVENT_##n##_HAT2_E    },                              \
    {   #n "_HAT2_NE",  EVENT_##n##_HAT2_NE   },                              \
    {   #n "_HAT2_N",   EVENT_##n##_HAT2_N    },                              \
    {   #n "_HAT2_NW",  EVENT_##n##_HAT2_NW   },                              \
    {   #n "_HAT2_W",   EVENT_##n##_HAT2_W    },                              \
    {   #n "_HAT2_SW",  EVENT_##n##_HAT2_SW   },                              \
    {   #n "_HAT2_S",   EVENT_##n##_HAT2_S    },                              \
    {   #n "_HAT2_SE",  EVENT_##n##_HAT2_SE   },                              \
    {   #n "_HAT3_E",   EVENT_##n##_HAT3_E    },                              \
    {   #n "_HAT3_NE",  EVENT_##n##_HAT3_NE   },                              \
    {   #n "_HAT3_N",   EVENT_##n##_HAT3_N    },                              \
    {   #n "_HAT3_NW",  EVENT_##n##_HAT3_NW   },                              \
    {   #n "_HAT3_W",   EVENT_##n##_HAT3_W    },                              \
    {   #n "_HAT3_SW",  EVENT_##n##_HAT3_SW   },                              \
    {   #n "_HAT3_S",   EVENT_##n##_HAT3_S    },                              \
    {   #n "_HAT3_SE",  EVENT_##n##_HAT3_SE   }                               

    JOY_DIR_EVT_DECL(JS0), JOY_BTN_EVT_DECL(JS0), JOY_HAT_EVT_DECL(JS0),
    JOY_DIR_EVT_DECL(JS1), JOY_BTN_EVT_DECL(JS1), JOY_HAT_EVT_DECL(JS1),
    JOY_DIR_EVT_DECL(JS2), JOY_BTN_EVT_DECL(JS2), JOY_HAT_EVT_DECL(JS2),
    JOY_DIR_EVT_DECL(JS3), JOY_BTN_EVT_DECL(JS3), JOY_HAT_EVT_DECL(JS3),

    /* -------------------------------------------------------------------- */
    /*  Mouse events -- experimental.                                       */
    /* -------------------------------------------------------------------- */
    JOY_DIR_EVT_DECL(MOUSE), JOY_BTN_EVT_DECL(MOUSE), 
};

const int event_name_count = sizeof(event_names) / sizeof(event_name_t);

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
/*                 Copyright (c) 1998-2011, Joseph Zbiciak                  */
/* ======================================================================== */
