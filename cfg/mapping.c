/*
 * ============================================================================
 *  Title:    Event binding tables.
 *  Author:   J. Zbiciak, R. Reynolds (GP2X)
 * ============================================================================
 *  These tables specify the bindable events and the default bindings.
 * ============================================================================
 */

#include "config.h"
#include "file/file.h"
#include "periph/periph.h"
#include "cp1600/cp1600.h"
#include "cp1600/emu_link.h"
#include "mem/mem.h"
#include "icart/icart.h"
#include "bincfg/bincfg.h"
#include "bincfg/legacy.h"
#include "pads/pads.h"
#include "pads/pads_cgc.h"
#include "pads/pads_intv2pc.h"
#include "gfx/gfx.h"
#include "snd/snd.h"
#include "ay8910/ay8910.h"
#include "demo/demo.h"
#include "stic/stic.h"
#include "ivoice/ivoice.h"
#include "speed/speed.h"
#include "debug/debug_.h"
#include "event/event.h"
#include "joy/joy.h"
#include "serializer/serializer.h"
#include "jlp/jlp.h"
#include "mapping.h"
#include "cfg.h"
#include <errno.h>


#define V(v) ((v_uint_32*)&intv.v)

/* ------------------------------------------------------------------------ */
/*  jzIntv internal event action table.  Keyboard and joystick inputs may   */
/*  be bound to any of these actions.  This table also ties the actions     */
/*  to the actual bits that the event action fiddles with.                  */
/*                                                                          */
/*  Notes on mnemonics:                                                     */
/*      PD0L        Left controller pad on base unit.                       */
/*      PD0R        Right controller pad on base unit.                      */
/*      PD1L        Left controller pad on ECS unit.                        */
/*      PD1R        Right controller pad on ECS unit.                       */
/*      PDxx_KP     Left Key Pad                                            */
/*      PDxx_A      Left Action Button  ([T]op, [L]eft, [R]ight)            */
/*      PDxx_D      Right Disc                                              */
/*      N, NE, etc  Compass directions.                                     */
/*                                                                          */
/*  The bit patterns at the right are AND and OR masks.  The first two      */
/*  are the AND masks for release/press.  The second two are OR masks       */
/*  for release/press.                                                      */
/* ------------------------------------------------------------------------ */
cfg_evtact_t  cfg_event_action[] =
{
    /* -------------------------------------------------------------------- */
    /*  Miscellaneous.                                                      */
    /* -------------------------------------------------------------------- */
    { "QUIT",       V(do_exit           ),  { ~0U, ~0U },   { ~0U, ~0U } },
    { "RESET",      V(do_reset          ),  { 0,   ~0U },   { 0,   ~0U } },
    { "PAUSE",      V(do_pause          ),  { ~0U, 0   },   { 0,   5   } },
    { "MOVIE",      V(gfx.scrshot       ),  { ~0U, ~0U },   { GFX_MVTOG, 0} },
    { "SHOT",       V(gfx.scrshot       ),  { ~0U, ~0U },   { GFX_SHOT,  0} },
    { "HIDE",       V(gfx.hidden        ),  { 0,   1   },   { 0,   1   } },
    { "WTOG",       V(gfx.toggle        ),  { ~0U, ~0U },   { 1,   0   } },
    { "BREAK",      V(debug.step_count  ),  { ~0U, 0   },   { 0,   0   } },
    { "KBD0",       V(event.change_kbd  ),  { ~0U, 0   },   { 0,   1   } },
    { "KBD1",       V(event.change_kbd  ),  { ~0U, 0   },   { 0,   2   } },
    { "KBD2",       V(event.change_kbd  ),  { ~0U, 0   },   { 0,   3   } },
    { "KBD3",       V(event.change_kbd  ),  { ~0U, 0   },   { 0,   4   } },
    { "KBDn",       V(event.change_kbd  ),  { ~0U, 0   },   { 0,   5   } },
    { "KBDp",       V(event.change_kbd  ),  { ~0U, 0   },   { 0,   6   } },
    { "SHF10",      V(event.change_kbd  ),  { 0,   0   },   { 1,   2   } },
    { "SHF20",      V(event.change_kbd  ),  { 0,   0   },   { 1,   3   } },
    { "SHF30",      V(event.change_kbd  ),  { 0,   0   },   { 1,   4   } },
    { "SHF01",      V(event.change_kbd  ),  { 0,   0   },   { 2,   1   } },
    { "SHF21",      V(event.change_kbd  ),  { 0,   0   },   { 2,   3   } },
    { "SHF31",      V(event.change_kbd  ),  { 0,   0   },   { 2,   4   } },
    { "SHF02",      V(event.change_kbd  ),  { 0,   0   },   { 3,   1   } },
    { "SHF12",      V(event.change_kbd  ),  { 0,   0   },   { 3,   2   } },
    { "SHF32",      V(event.change_kbd  ),  { 0,   0   },   { 3,   4   } },
    { "SHF03",      V(event.change_kbd  ),  { 0,   0   },   { 4,   1   } },
    { "SHF13",      V(event.change_kbd  ),  { 0,   0   },   { 4,   2   } },
    { "SHF23",      V(event.change_kbd  ),  { 0,   0   },   { 4,   3   } },
    { "PSH0",       V(event.change_kbd  ),  { 0,   0   },   { 7,   8   } },
    { "PSH1",       V(event.change_kbd  ),  { 0,   0   },   { 7,   9   } },
    { "PSH2",       V(event.change_kbd  ),  { 0,   0   },   { 7,   10  } },
    { "PSH3",       V(event.change_kbd  ),  { 0,   0   },   { 7,   11  } },
    { "POP",        V(event.change_kbd  ),  { ~0U, 0   },   { 0,   7   } },
    { "VOLUP",      V(snd.change_vol    ),  { ~0U, 0   },   { 0,   1   } },
    { "VOLDN",      V(snd.change_vol    ),  { ~0U, 0   },   { 0,   2   } },
    { "NA",         NULL,                   { 0,   0   },   { 0,   0   } },

    /* -------------------------------------------------------------------- */
    /*  PAD0: Left-hand controller keypad                                   */
    /* -------------------------------------------------------------------- */
    { "PD0L_KP1",   V(pad0.l[1]         ),  { 0,    ~0U  }, { 0,    0x81 } },
    { "PD0L_KP2",   V(pad0.l[2]         ),  { 0,    ~0U  }, { 0,    0x41 } },
    { "PD0L_KP3",   V(pad0.l[3]         ),  { 0,    ~0U  }, { 0,    0x21 } },
    { "PD0L_KP4",   V(pad0.l[4]         ),  { 0,    ~0U  }, { 0,    0x82 } },
    { "PD0L_KP5",   V(pad0.l[5]         ),  { 0,    ~0U  }, { 0,    0x42 } },
    { "PD0L_KP6",   V(pad0.l[6]         ),  { 0,    ~0U  }, { 0,    0x22 } },
    { "PD0L_KP7",   V(pad0.l[7]         ),  { 0,    ~0U  }, { 0,    0x84 } },
    { "PD0L_KP8",   V(pad0.l[8]         ),  { 0,    ~0U  }, { 0,    0x44 } },
    { "PD0L_KP9",   V(pad0.l[9]         ),  { 0,    ~0U  }, { 0,    0x24 } },
    { "PD0L_KPC",   V(pad0.l[10]        ),  { 0,    ~0U  }, { 0,    0x88 } },
    { "PD0L_KP0",   V(pad0.l[0]         ),  { 0,    ~0U  }, { 0,    0x48 } },
    { "PD0L_KPE",   V(pad0.l[11]        ),  { 0,    ~0U  }, { 0,    0x28 } },

    /* -------------------------------------------------------------------- */
    /*  PAD0: Right-hand controller keypad                                  */
    /* -------------------------------------------------------------------- */
    { "PD0R_KP1",   V(pad0.r[1]         ),  { 0,    ~0U  }, { 0,    0x81 } },
    { "PD0R_KP2",   V(pad0.r[2]         ),  { 0,    ~0U  }, { 0,    0x41 } },
    { "PD0R_KP3",   V(pad0.r[3]         ),  { 0,    ~0U  }, { 0,    0x21 } },
    { "PD0R_KP4",   V(pad0.r[4]         ),  { 0,    ~0U  }, { 0,    0x82 } },
    { "PD0R_KP5",   V(pad0.r[5]         ),  { 0,    ~0U  }, { 0,    0x42 } },
    { "PD0R_KP6",   V(pad0.r[6]         ),  { 0,    ~0U  }, { 0,    0x22 } },
    { "PD0R_KP7",   V(pad0.r[7]         ),  { 0,    ~0U  }, { 0,    0x84 } },
    { "PD0R_KP8",   V(pad0.r[8]         ),  { 0,    ~0U  }, { 0,    0x44 } },
    { "PD0R_KP9",   V(pad0.r[9]         ),  { 0,    ~0U  }, { 0,    0x24 } },
    { "PD0R_KPC",   V(pad0.r[10]        ),  { 0,    ~0U  }, { 0,    0x88 } },
    { "PD0R_KP0",   V(pad0.r[0]         ),  { 0,    ~0U  }, { 0,    0x48 } },
    { "PD0R_KPE",   V(pad0.r[11]        ),  { 0,    ~0U  }, { 0,    0x28 } },

    /* -------------------------------------------------------------------- */
    /*  PAD0: Action buttons.                                               */
    /* -------------------------------------------------------------------- */
    { "PD0L_A_T",   V(pad0.l[12]        ), { 0,    ~0U  }, { 0,    0xA0 } },
    { "PD0L_A_L",   V(pad0.l[13]        ), { 0,    ~0U  }, { 0,    0x60 } },
    { "PD0L_A_R",   V(pad0.l[14]        ), { 0,    ~0U  }, { 0,    0xC0 } },

    { "PD0R_A_T",   V(pad0.r[12]        ), { 0,    ~0U  }, { 0,    0xA0 } },
    { "PD0R_A_L",   V(pad0.r[13]        ), { 0,    ~0U  }, { 0,    0x60 } },
    { "PD0R_A_R",   V(pad0.r[14]        ), { 0,    ~0U  }, { 0,    0xC0 } },

    /* -------------------------------------------------------------------- */
    /*  PAD0: The Controller DISC via Keyboard etc.                         */
    /* -------------------------------------------------------------------- */
    { "PD0L_D_E",   V(pad0.l[15]        ), { ~1,   ~0U  }, { 0,    1  } },
    { "PD0L_D_ENE", V(pad0.l[15]        ), { ~3,   ~0U  }, { 0,    3  } },
    { "PD0L_D_NE",  V(pad0.l[15]        ), { ~2,   ~0U  }, { 0,    2  } },
    { "PD0L_D_NNE", V(pad0.l[15]        ), { ~6,   ~0U  }, { 0,    6  } },
    { "PD0L_D_N",   V(pad0.l[15]        ), { ~4,   ~0U  }, { 0,    4  } },
    { "PD0L_D_NNW", V(pad0.l[15]        ), { ~12,  ~0U  }, { 0,    12 } },
    { "PD0L_D_NW",  V(pad0.l[15]        ), { ~8,   ~0U  }, { 0,    8  } },
    { "PD0L_D_WNW", V(pad0.l[15]        ), { ~24,  ~0U  }, { 0,    24 } },
    { "PD0L_D_W",   V(pad0.l[15]        ), { ~16,  ~0U  }, { 0,    16 } },
    { "PD0L_D_WSW", V(pad0.l[15]        ), { ~48,  ~0U  }, { 0,    48 } },
    { "PD0L_D_SW",  V(pad0.l[15]        ), { ~32,  ~0U  }, { 0,    32 } },
    { "PD0L_D_SSW", V(pad0.l[15]        ), { ~96,  ~0U  }, { 0,    96 } },
    { "PD0L_D_S",   V(pad0.l[15]        ), { ~64,  ~0U  }, { 0,    64 } },
    { "PD0L_D_SSE", V(pad0.l[15]        ), { ~192, ~0U  }, { 0,    192} },
    { "PD0L_D_SE",  V(pad0.l[15]        ), { ~128, ~0U  }, { 0,    128} },
    { "PD0L_D_ESE", V(pad0.l[15]        ), { ~129, ~0U  }, { 0,    129} },
                                                                      
    { "PD0R_D_E",   V(pad0.r[15]        ), { ~1,   ~0U  }, { 0,    1  } },
    { "PD0R_D_ENE", V(pad0.r[15]        ), { ~3,   ~0U  }, { 0,    3  } },
    { "PD0R_D_NE",  V(pad0.r[15]        ), { ~2,   ~0U  }, { 0,    2  } },
    { "PD0R_D_NNE", V(pad0.r[15]        ), { ~6,   ~0U  }, { 0,    6  } },
    { "PD0R_D_N",   V(pad0.r[15]        ), { ~4,   ~0U  }, { 0,    4  } },
    { "PD0R_D_NNW", V(pad0.r[15]        ), { ~12,  ~0U  }, { 0,    12 } },
    { "PD0R_D_NW",  V(pad0.r[15]        ), { ~8,   ~0U  }, { 0,    8  } },
    { "PD0R_D_WNW", V(pad0.r[15]        ), { ~24,  ~0U  }, { 0,    24 } },
    { "PD0R_D_W",   V(pad0.r[15]        ), { ~16,  ~0U  }, { 0,    16 } },
    { "PD0R_D_WSW", V(pad0.r[15]        ), { ~48,  ~0U  }, { 0,    48 } },
    { "PD0R_D_SW",  V(pad0.r[15]        ), { ~32,  ~0U  }, { 0,    32 } },
    { "PD0R_D_SSW", V(pad0.r[15]        ), { ~96,  ~0U  }, { 0,    96 } },
    { "PD0R_D_S",   V(pad0.r[15]        ), { ~64,  ~0U  }, { 0,    64 } },
    { "PD0R_D_SSE", V(pad0.r[15]        ), { ~192, ~0U  }, { 0,    192} },
    { "PD0R_D_SE",  V(pad0.r[15]        ), { ~128, ~0U  }, { 0,    128} },
    { "PD0R_D_ESE", V(pad0.r[15]        ), { ~129, ~0U  }, { 0,    129} },

    /* -------------------------------------------------------------------- */
    /*  PAD0: The Controller DISC via Joystick                              */
    /* -------------------------------------------------------------------- */
    { "PD0L_J_E",   V(pad0.l[16]        ), { 0,    0    }, { 0,    1   }},
    { "PD0L_J_ENE", V(pad0.l[16]        ), { 0,    0    }, { 0,    3   }},
    { "PD0L_J_NE",  V(pad0.l[16]        ), { 0,    0    }, { 0,    2   }},
    { "PD0L_J_NNE", V(pad0.l[16]        ), { 0,    0    }, { 0,    6   }},
    { "PD0L_J_N",   V(pad0.l[16]        ), { 0,    0    }, { 0,    4   }},
    { "PD0L_J_NNW", V(pad0.l[16]        ), { 0,    0    }, { 0,    12  }},
    { "PD0L_J_NW",  V(pad0.l[16]        ), { 0,    0    }, { 0,    8   }},
    { "PD0L_J_WNW", V(pad0.l[16]        ), { 0,    0    }, { 0,    24  }},
    { "PD0L_J_W",   V(pad0.l[16]        ), { 0,    0    }, { 0,    16  }},
    { "PD0L_J_WSW", V(pad0.l[16]        ), { 0,    0    }, { 0,    48  }},
    { "PD0L_J_SW",  V(pad0.l[16]        ), { 0,    0    }, { 0,    32  }},
    { "PD0L_J_SSW", V(pad0.l[16]        ), { 0,    0    }, { 0,    96  }},
    { "PD0L_J_S",   V(pad0.l[16]        ), { 0,    0    }, { 0,    64  }},
    { "PD0L_J_SSE", V(pad0.l[16]        ), { 0,    0    }, { 0,    192 }},
    { "PD0L_J_SE",  V(pad0.l[16]        ), { 0,    0    }, { 0,    128 }},
    { "PD0L_J_ESE", V(pad0.l[16]        ), { 0,    0    }, { 0,    129 }},
                                                                       
    { "PD0R_J_E",   V(pad0.r[16]        ), { 0,    0    }, { 0,    1   }},
    { "PD0R_J_ENE", V(pad0.r[16]        ), { 0,    0    }, { 0,    3   }},
    { "PD0R_J_NE",  V(pad0.r[16]        ), { 0,    0    }, { 0,    2   }},
    { "PD0R_J_NNE", V(pad0.r[16]        ), { 0,    0    }, { 0,    6   }},
    { "PD0R_J_N",   V(pad0.r[16]        ), { 0,    0    }, { 0,    4   }},
    { "PD0R_J_NNW", V(pad0.r[16]        ), { 0,    0    }, { 0,    12  }},
    { "PD0R_J_NW",  V(pad0.r[16]        ), { 0,    0    }, { 0,    8   }},
    { "PD0R_J_WNW", V(pad0.r[16]        ), { 0,    0    }, { 0,    24  }},
    { "PD0R_J_W",   V(pad0.r[16]        ), { 0,    0    }, { 0,    16  }},
    { "PD0R_J_WSW", V(pad0.r[16]        ), { 0,    0    }, { 0,    48  }},
    { "PD0R_J_SW",  V(pad0.r[16]        ), { 0,    0    }, { 0,    32  }},
    { "PD0R_J_SSW", V(pad0.r[16]        ), { 0,    0    }, { 0,    96  }},
    { "PD0R_J_S",   V(pad0.r[16]        ), { 0,    0    }, { 0,    64  }},
    { "PD0R_J_SSE", V(pad0.r[16]        ), { 0,    0    }, { 0,    192 }},
    { "PD0R_J_SE",  V(pad0.r[16]        ), { 0,    0    }, { 0,    128 }},
    { "PD0R_J_ESE", V(pad0.r[16]        ), { 0,    0    }, { 0,    129 }},


    /* -------------------------------------------------------------------- */
    /*  PAD1: Left-hand controller keypad                                   */
    /* -------------------------------------------------------------------- */
    { "PD1L_KP1",   V(pad1.l[1]         ),  { 0,    ~0U  }, { 0,    0x81 } },
    { "PD1L_KP2",   V(pad1.l[2]         ),  { 0,    ~0U  }, { 0,    0x41 } },
    { "PD1L_KP3",   V(pad1.l[3]         ),  { 0,    ~0U  }, { 0,    0x21 } },
    { "PD1L_KP4",   V(pad1.l[4]         ),  { 0,    ~0U  }, { 0,    0x82 } },
    { "PD1L_KP5",   V(pad1.l[5]         ),  { 0,    ~0U  }, { 0,    0x42 } },
    { "PD1L_KP6",   V(pad1.l[6]         ),  { 0,    ~0U  }, { 0,    0x22 } },
    { "PD1L_KP7",   V(pad1.l[7]         ),  { 0,    ~0U  }, { 0,    0x84 } },
    { "PD1L_KP8",   V(pad1.l[8]         ),  { 0,    ~0U  }, { 0,    0x44 } },
    { "PD1L_KP9",   V(pad1.l[9]         ),  { 0,    ~0U  }, { 0,    0x24 } },
    { "PD1L_KPC",   V(pad1.l[10]        ),  { 0,    ~0U  }, { 0,    0x88 } },
    { "PD1L_KP0",   V(pad1.l[0]         ),  { 0,    ~0U  }, { 0,    0x48 } },
    { "PD1L_KPE",   V(pad1.l[11]        ),  { 0,    ~0U  }, { 0,    0x28 } },

    /* -------------------------------------------------------------------- */
    /*  PAD1: Right-hand controller keypad                                  */
    /* -------------------------------------------------------------------- */
    { "PD1R_KP1",   V(pad1.r[1]         ),  { 0,    ~0U  }, { 0,    0x81 } },
    { "PD1R_KP2",   V(pad1.r[2]         ),  { 0,    ~0U  }, { 0,    0x41 } },
    { "PD1R_KP3",   V(pad1.r[3]         ),  { 0,    ~0U  }, { 0,    0x21 } },
    { "PD1R_KP4",   V(pad1.r[4]         ),  { 0,    ~0U  }, { 0,    0x82 } },
    { "PD1R_KP5",   V(pad1.r[5]         ),  { 0,    ~0U  }, { 0,    0x42 } },
    { "PD1R_KP6",   V(pad1.r[6]         ),  { 0,    ~0U  }, { 0,    0x22 } },
    { "PD1R_KP7",   V(pad1.r[7]         ),  { 0,    ~0U  }, { 0,    0x84 } },
    { "PD1R_KP8",   V(pad1.r[8]         ),  { 0,    ~0U  }, { 0,    0x44 } },
    { "PD1R_KP9",   V(pad1.r[9]         ),  { 0,    ~0U  }, { 0,    0x24 } },
    { "PD1R_KPC",   V(pad1.r[10]        ),  { 0,    ~0U  }, { 0,    0x88 } },
    { "PD1R_KP0",   V(pad1.r[0]         ),  { 0,    ~0U  }, { 0,    0x48 } },
    { "PD1R_KPE",   V(pad1.r[11]        ),  { 0,    ~0U  }, { 0,    0x28 } },

    /* -------------------------------------------------------------------- */
    /*  PAD1: Action buttons.                                               */
    /* -------------------------------------------------------------------- */
    { "PD1L_A_T",   V(pad1.l[12]        ), { 0,    ~0U  }, { 0,    0xA0 } },
    { "PD1L_A_L",   V(pad1.l[13]        ), { 0,    ~0U  }, { 0,    0x60 } },
    { "PD1L_A_R",   V(pad1.l[14]        ), { 0,    ~0U  }, { 0,    0xC0 } },

    { "PD1R_A_T",   V(pad1.r[12]        ), { 0,    ~0U  }, { 0,    0xA0 } },
    { "PD1R_A_L",   V(pad1.r[13]        ), { 0,    ~0U  }, { 0,    0x60 } },
    { "PD1R_A_R",   V(pad1.r[14]        ), { 0,    ~0U  }, { 0,    0xC0 } },

    /* -------------------------------------------------------------------- */
    /*  PAD1: The Controller DISC.                                          */
    /* -------------------------------------------------------------------- */
    { "PD1L_D_E",   V(pad1.l[15]        ), { ~1,   ~0U  }, { 0,    1  } },
    { "PD1L_D_NE",  V(pad1.l[15]        ), { ~2,   ~0U  }, { 0,    2  } },
    { "PD1L_D_N",   V(pad1.l[15]        ), { ~4,   ~0U  }, { 0,    4  } },
    { "PD1L_D_NW",  V(pad1.l[15]        ), { ~8,   ~0U  }, { 0,    8  } },
    { "PD1L_D_W",   V(pad1.l[15]        ), { ~16,  ~0U  }, { 0,    16 } },
    { "PD1L_D_SW",  V(pad1.l[15]        ), { ~32,  ~0U  }, { 0,    32 } },
    { "PD1L_D_S",   V(pad1.l[15]        ), { ~64,  ~0U  }, { 0,    64 } },
    { "PD1L_D_SE",  V(pad1.l[15]        ), { ~128, ~0U  }, { 0,    128} },

    { "PD1R_D_E",   V(pad1.r[15]        ), { ~1,   ~0U  }, { 0,    1  } },
    { "PD1R_D_NE",  V(pad1.r[15]        ), { ~2,   ~0U  }, { 0,    2  } },
    { "PD1R_D_N",   V(pad1.r[15]        ), { ~4,   ~0U  }, { 0,    4  } },
    { "PD1R_D_NW",  V(pad1.r[15]        ), { ~8,   ~0U  }, { 0,    8  } },
    { "PD1R_D_W",   V(pad1.r[15]        ), { ~16,  ~0U  }, { 0,    16 } },
    { "PD1R_D_SW",  V(pad1.r[15]        ), { ~32,  ~0U  }, { 0,    32 } },
    { "PD1R_D_S",   V(pad1.r[15]        ), { ~64,  ~0U  }, { 0,    64 } },
    { "PD1R_D_SE",  V(pad1.r[15]        ), { ~128, ~0U  }, { 0,    128} },

    /* -------------------------------------------------------------------- */
    /*  PAD1: The Controller DISC via Joystick                              */
    /* -------------------------------------------------------------------- */
    { "PD1L_J_E",   V(pad1.l[16]        ), { 0,    0    }, { 0,    1   }},
    { "PD1L_J_ENE", V(pad1.l[16]        ), { 0,    0    }, { 0,    3   }},
    { "PD1L_J_NE",  V(pad1.l[16]        ), { 0,    0    }, { 0,    2   }},
    { "PD1L_J_NNE", V(pad1.l[16]        ), { 0,    0    }, { 0,    6   }},
    { "PD1L_J_N",   V(pad1.l[16]        ), { 0,    0    }, { 0,    4   }},
    { "PD1L_J_NNW", V(pad1.l[16]        ), { 0,    0    }, { 0,    12  }},
    { "PD1L_J_NW",  V(pad1.l[16]        ), { 0,    0    }, { 0,    8   }},
    { "PD1L_J_WNW", V(pad1.l[16]        ), { 0,    0    }, { 0,    24  }},
    { "PD1L_J_W",   V(pad1.l[16]        ), { 0,    0    }, { 0,    16  }},
    { "PD1L_J_WSW", V(pad1.l[16]        ), { 0,    0    }, { 0,    48  }},
    { "PD1L_J_SW",  V(pad1.l[16]        ), { 0,    0    }, { 0,    32  }},
    { "PD1L_J_SSW", V(pad1.l[16]        ), { 0,    0    }, { 0,    96  }},
    { "PD1L_J_S",   V(pad1.l[16]        ), { 0,    0    }, { 0,    64  }},
    { "PD1L_J_SSE", V(pad1.l[16]        ), { 0,    0    }, { 0,    192 }},
    { "PD1L_J_SE",  V(pad1.l[16]        ), { 0,    0    }, { 0,    128 }},
    { "PD1L_J_ESE", V(pad1.l[16]        ), { 0,    0    }, { 0,    129 }},
                                                                       
    { "PD1R_J_E",   V(pad1.r[16]        ), { 0,    0    }, { 0,    1   }},
    { "PD1R_J_ENE", V(pad1.r[16]        ), { 0,    0    }, { 0,    3   }},
    { "PD1R_J_NE",  V(pad1.r[16]        ), { 0,    0    }, { 0,    2   }},
    { "PD1R_J_NNE", V(pad1.r[16]        ), { 0,    0    }, { 0,    6   }},
    { "PD1R_J_N",   V(pad1.r[16]        ), { 0,    0    }, { 0,    4   }},
    { "PD1R_J_NNW", V(pad1.r[16]        ), { 0,    0    }, { 0,    12  }},
    { "PD1R_J_NW",  V(pad1.r[16]        ), { 0,    0    }, { 0,    8   }},
    { "PD1R_J_WNW", V(pad1.r[16]        ), { 0,    0    }, { 0,    24  }},
    { "PD1R_J_W",   V(pad1.r[16]        ), { 0,    0    }, { 0,    16  }},
    { "PD1R_J_WSW", V(pad1.r[16]        ), { 0,    0    }, { 0,    48  }},
    { "PD1R_J_SW",  V(pad1.r[16]        ), { 0,    0    }, { 0,    32  }},
    { "PD1R_J_SSW", V(pad1.r[16]        ), { 0,    0    }, { 0,    96  }},
    { "PD1R_J_S",   V(pad1.r[16]        ), { 0,    0    }, { 0,    64  }},
    { "PD1R_J_SSE", V(pad1.r[16]        ), { 0,    0    }, { 0,    192 }},
    { "PD1R_J_SE",  V(pad1.r[16]        ), { 0,    0    }, { 0,    128 }},
    { "PD1R_J_ESE", V(pad1.r[16]        ), { 0,    0    }, { 0,    129 }},


    /*
00FFh|                  00FEh bits
bits |   0       1    2  3  4    5        6      7
-----+------------------------------------------------
  0  | left,   comma, n, v, x, space,   [n/a], [n/a]
  1  | period, m,     b, c, z, down,    [n/a], [n/a]
  2  | scolon, k,     h, f, s, up,      [n/a], [n/a]
  3  | p,      i,     y, r, w, q,       [n/a], [n/a]
  4  | esc,    9,     7, 5, 3, 1,       [n/a], [n/a]
  5  | 0,      8,     6, 4, 2, right,   [n/a], [n/a]
  6  | enter,  o,     u, t, e, ctl,     [n/a], [n/a]
  7  | [n/a],  l,     j, g, d, a,       shift, [n/a]
  */
    
    /* -------------------------------------------------------------------- */
    /*  ECS Keyboard                                                        */
    /* -------------------------------------------------------------------- */

    /* bit 0 */
    { "KEYB_LEFT",  V(pad1.k[ 0]        ), { ~  1, ~0U  }, { 0,      1} },
    { "KEYB_PERIOD",V(pad1.k[ 0]        ), { ~  2, ~0U  }, { 0,      2} },
    { "KEYB_SEMI",  V(pad1.k[ 0]        ), { ~  4, ~0U  }, { 0,      4} },
    { "KEYB_P",     V(pad1.k[ 0]        ), { ~  8, ~0U  }, { 0,      8} },
    { "KEYB_ESC",   V(pad1.k[ 0]        ), { ~ 16, ~0U  }, { 0,     16} },
    { "KEYB_0",     V(pad1.k[ 0]        ), { ~ 32, ~0U  }, { 0,     32} },
    { "KEYB_ENTER", V(pad1.k[ 0]        ), { ~ 64, ~0U  }, { 0,     64} },

    /* bit 1 */                                                       
    { "KEYB_COMMA", V(pad1.k[ 1]        ), { ~  1, ~0U  }, { 0,      1} },
    { "KEYB_M",     V(pad1.k[ 1]        ), { ~  2, ~0U  }, { 0,      2} },
    { "KEYB_K",     V(pad1.k[ 1]        ), { ~  4, ~0U  }, { 0,      4} },
    { "KEYB_I",     V(pad1.k[ 1]        ), { ~  8, ~0U  }, { 0,      8} },
    { "KEYB_9",     V(pad1.k[ 1]        ), { ~ 16, ~0U  }, { 0,     16} },
    { "KEYB_8",     V(pad1.k[ 1]        ), { ~ 32, ~0U  }, { 0,     32} },
    { "KEYB_O",     V(pad1.k[ 1]        ), { ~ 64, ~0U  }, { 0,     64} },
    { "KEYB_L",     V(pad1.k[ 1]        ), { ~128, ~0U  }, { 0,    128} },
                                                                      
    /* bit 2 */                                                       
    { "KEYB_N",     V(pad1.k[ 2]        ), { ~  1, ~0U  }, { 0,      1} },
    { "KEYB_B",     V(pad1.k[ 2]        ), { ~  2, ~0U  }, { 0,      2} },
    { "KEYB_H",     V(pad1.k[ 2]        ), { ~  4, ~0U  }, { 0,      4} },
    { "KEYB_Y",     V(pad1.k[ 2]        ), { ~  8, ~0U  }, { 0,      8} },
    { "KEYB_7",     V(pad1.k[ 2]        ), { ~ 16, ~0U  }, { 0,     16} },
    { "KEYB_6",     V(pad1.k[ 2]        ), { ~ 32, ~0U  }, { 0,     32} },
    { "KEYB_U",     V(pad1.k[ 2]        ), { ~ 64, ~0U  }, { 0,     64} },
    { "KEYB_J",     V(pad1.k[ 2]        ), { ~128, ~0U  }, { 0,    128} },
                                                                      
    /* bit 3 */                                                       
    { "KEYB_V",     V(pad1.k[ 3]        ), { ~  1, ~0U  }, { 0,      1} },
    { "KEYB_C",     V(pad1.k[ 3]        ), { ~  2, ~0U  }, { 0,      2} },
    { "KEYB_F",     V(pad1.k[ 3]        ), { ~  4, ~0U  }, { 0,      4} },
    { "KEYB_R",     V(pad1.k[ 3]        ), { ~  8, ~0U  }, { 0,      8} },
    { "KEYB_5",     V(pad1.k[ 3]        ), { ~ 16, ~0U  }, { 0,     16} },
    { "KEYB_4",     V(pad1.k[ 3]        ), { ~ 32, ~0U  }, { 0,     32} },
    { "KEYB_T",     V(pad1.k[ 3]        ), { ~ 64, ~0U  }, { 0,     64} },
    { "KEYB_G",     V(pad1.k[ 3]        ), { ~128, ~0U  }, { 0,    128} },
                                                                      
    /* bit 4 */                                                       
    { "KEYB_X",     V(pad1.k[ 4]        ), { ~  1, ~0U  }, { 0,      1} },
    { "KEYB_Z",     V(pad1.k[ 4]        ), { ~  2, ~0U  }, { 0,      2} },
    { "KEYB_S",     V(pad1.k[ 4]        ), { ~  4, ~0U  }, { 0,      4} },
    { "KEYB_W",     V(pad1.k[ 4]        ), { ~  8, ~0U  }, { 0,      8} },
    { "KEYB_3",     V(pad1.k[ 4]        ), { ~ 16, ~0U  }, { 0,     16} },
    { "KEYB_2",     V(pad1.k[ 4]        ), { ~ 32, ~0U  }, { 0,     32} },
    { "KEYB_E",     V(pad1.k[ 4]        ), { ~ 64, ~0U  }, { 0,     64} },
    { "KEYB_D",     V(pad1.k[ 4]        ), { ~128, ~0U  }, { 0,    128} },
                                                                      
    /* bit 5 */                                                       
    { "KEYB_SPACE", V(pad1.k[ 5]        ), { ~  1, ~0U  }, { 0,      1} },
    { "KEYB_DOWN",  V(pad1.k[ 5]        ), { ~  2, ~0U  }, { 0,      2} },
    { "KEYB_UP",    V(pad1.k[ 5]        ), { ~  4, ~0U  }, { 0,      4} },
    { "KEYB_Q",     V(pad1.k[ 5]        ), { ~  8, ~0U  }, { 0,      8} },
    { "KEYB_1",     V(pad1.k[ 5]        ), { ~ 16, ~0U  }, { 0,     16} },
    { "KEYB_RIGHT", V(pad1.k[ 5]        ), { ~ 32, ~0U  }, { 0,     32} },
    { "KEYB_CTRL",  V(pad1.k[ 5]        ), { ~ 64, ~0U  }, { 0,     64} },
    { "KEYB_A",     V(pad1.k[ 5]        ), { ~128, ~0U  }, { 0,    128} },
                                                                      
    /* bit 6 */                                                       
    { "KEYB_SHIFT", V(pad1.k[ 6]        ), { ~128, ~0U  }, { 0,    128} },

    /* -------------------------------------------------------------------- */
    /*  ECS Keyboard "Shifted" Keys.                                        */
    /* -------------------------------------------------------------------- */
    { "KEYB_EQUAL", V(pad1.k[ 5]), { ~( 16 << 8), ~0U  }, { 0, (16 << 8)} },
    { "KEYB_QUOTE", V(pad1.k[ 4]), { ~( 32 << 8), ~0U  }, { 0, (32 << 8)} },
    { "KEYB_HASH",  V(pad1.k[ 4]), { ~( 16 << 8), ~0U  }, { 0, (16 << 8)} },
    { "KEYB_DOLLAR",V(pad1.k[ 3]), { ~( 32 << 8), ~0U  }, { 0, (32 << 8)} },
    { "KEYB_PLUS",  V(pad1.k[ 3]), { ~( 16 << 8), ~0U  }, { 0, (16 << 8)} },
    { "KEYB_MINUS", V(pad1.k[ 2]), { ~( 32 << 8), ~0U  }, { 0, (32 << 8)} },
    { "KEYB_SLASH", V(pad1.k[ 2]), { ~( 16 << 8), ~0U  }, { 0, (16 << 8)} },
    { "KEYB_STAR",  V(pad1.k[ 1]), { ~( 32 << 8), ~0U  }, { 0, (32 << 8)} },
    { "KEYB_LPAREN",V(pad1.k[ 1]), { ~( 16 << 8), ~0U  }, { 0, (16 << 8)} },
    { "KEYB_RPAREN",V(pad1.k[ 0]), { ~( 32 << 8), ~0U  }, { 0, (32 << 8)} },
    { "KEYB_CARET", V(pad1.k[ 5]), { ~(  4 << 8), ~0U  }, { 0, ( 4 << 8)} },
    { "KEYB_PCT",   V(pad1.k[ 5]), { ~(  2 << 8), ~0U  }, { 0, ( 2 << 8)} },
    { "KEYB_SQUOTE",V(pad1.k[ 0]), { ~(  1 << 8), ~0U  }, { 0, ( 1 << 8)} },
    { "KEYB_QUEST", V(pad1.k[ 5]), { ~( 32 << 8), ~0U  }, { 0, (32 << 8)} },

    /* -------------------------------------------------------------------- */
    /*  Synthesizer keyboard bindings.                                      */
    /* -------------------------------------------------------------------- */
    { "SYNTH_00",   V(pad1.k[ 0]), { ~(  1 << 16), ~0U  }, { 0, (  1 << 16)} },
    { "SYNTH_01",   V(pad1.k[ 0]), { ~(  2 << 16), ~0U  }, { 0, (  2 << 16)} },
    { "SYNTH_02",   V(pad1.k[ 0]), { ~(  4 << 16), ~0U  }, { 0, (  4 << 16)} },
    { "SYNTH_03",   V(pad1.k[ 0]), { ~(  8 << 16), ~0U  }, { 0, (  8 << 16)} },
    { "SYNTH_04",   V(pad1.k[ 0]), { ~( 16 << 16), ~0U  }, { 0, ( 16 << 16)} },
    { "SYNTH_05",   V(pad1.k[ 0]), { ~( 32 << 16), ~0U  }, { 0, ( 32 << 16)} },
    { "SYNTH_06",   V(pad1.k[ 0]), { ~( 64 << 16), ~0U  }, { 0, ( 64 << 16)} },
    { "SYNTH_07",   V(pad1.k[ 0]), { ~(128 << 16), ~0U  }, { 0, (128 << 16)} },
    { "SYNTH_08",   V(pad1.k[ 1]), { ~(  1 << 16), ~0U  }, { 0, (  1 << 16)} },
    { "SYNTH_09",   V(pad1.k[ 1]), { ~(  2 << 16), ~0U  }, { 0, (  2 << 16)} },
    { "SYNTH_10",   V(pad1.k[ 1]), { ~(  4 << 16), ~0U  }, { 0, (  4 << 16)} },
    { "SYNTH_11",   V(pad1.k[ 1]), { ~(  8 << 16), ~0U  }, { 0, (  8 << 16)} },
    { "SYNTH_12",   V(pad1.k[ 1]), { ~( 16 << 16), ~0U  }, { 0, ( 16 << 16)} },
    { "SYNTH_13",   V(pad1.k[ 1]), { ~( 32 << 16), ~0U  }, { 0, ( 32 << 16)} },
    { "SYNTH_14",   V(pad1.k[ 1]), { ~( 64 << 16), ~0U  }, { 0, ( 64 << 16)} },
    { "SYNTH_15",   V(pad1.k[ 1]), { ~(128 << 16), ~0U  }, { 0, (128 << 16)} },
    { "SYNTH_16",   V(pad1.k[ 2]), { ~(  1 << 16), ~0U  }, { 0, (  1 << 16)} },
    { "SYNTH_17",   V(pad1.k[ 2]), { ~(  2 << 16), ~0U  }, { 0, (  2 << 16)} },
    { "SYNTH_18",   V(pad1.k[ 2]), { ~(  4 << 16), ~0U  }, { 0, (  4 << 16)} },
    { "SYNTH_19",   V(pad1.k[ 2]), { ~(  8 << 16), ~0U  }, { 0, (  8 << 16)} },
    { "SYNTH_20",   V(pad1.k[ 2]), { ~( 16 << 16), ~0U  }, { 0, ( 16 << 16)} },
    { "SYNTH_21",   V(pad1.k[ 2]), { ~( 32 << 16), ~0U  }, { 0, ( 32 << 16)} },
    { "SYNTH_22",   V(pad1.k[ 2]), { ~( 64 << 16), ~0U  }, { 0, ( 64 << 16)} },
    { "SYNTH_23",   V(pad1.k[ 2]), { ~(128 << 16), ~0U  }, { 0, (128 << 16)} },
    { "SYNTH_24",   V(pad1.k[ 3]), { ~(  1 << 16), ~0U  }, { 0, (  1 << 16)} },
    { "SYNTH_25",   V(pad1.k[ 3]), { ~(  2 << 16), ~0U  }, { 0, (  2 << 16)} },
    { "SYNTH_26",   V(pad1.k[ 3]), { ~(  4 << 16), ~0U  }, { 0, (  4 << 16)} },
    { "SYNTH_27",   V(pad1.k[ 3]), { ~(  8 << 16), ~0U  }, { 0, (  8 << 16)} },
    { "SYNTH_28",   V(pad1.k[ 3]), { ~( 16 << 16), ~0U  }, { 0, ( 16 << 16)} },
    { "SYNTH_29",   V(pad1.k[ 3]), { ~( 32 << 16), ~0U  }, { 0, ( 32 << 16)} },
    { "SYNTH_30",   V(pad1.k[ 3]), { ~( 64 << 16), ~0U  }, { 0, ( 64 << 16)} },
    { "SYNTH_31",   V(pad1.k[ 3]), { ~(128 << 16), ~0U  }, { 0, (128 << 16)} },
    { "SYNTH_32",   V(pad1.k[ 4]), { ~(  1 << 16), ~0U  }, { 0, (  1 << 16)} },
    { "SYNTH_33",   V(pad1.k[ 4]), { ~(  2 << 16), ~0U  }, { 0, (  2 << 16)} },
    { "SYNTH_34",   V(pad1.k[ 4]), { ~(  4 << 16), ~0U  }, { 0, (  4 << 16)} },
    { "SYNTH_35",   V(pad1.k[ 4]), { ~(  8 << 16), ~0U  }, { 0, (  8 << 16)} },
    { "SYNTH_36",   V(pad1.k[ 4]), { ~( 16 << 16), ~0U  }, { 0, ( 16 << 16)} },
    { "SYNTH_37",   V(pad1.k[ 4]), { ~( 32 << 16), ~0U  }, { 0, ( 32 << 16)} },
    { "SYNTH_38",   V(pad1.k[ 4]), { ~( 64 << 16), ~0U  }, { 0, ( 64 << 16)} },
    { "SYNTH_39",   V(pad1.k[ 4]), { ~(128 << 16), ~0U  }, { 0, (128 << 16)} },
    { "SYNTH_40",   V(pad1.k[ 5]), { ~(  1 << 16), ~0U  }, { 0, (  1 << 16)} },
    { "SYNTH_41",   V(pad1.k[ 5]), { ~(  2 << 16), ~0U  }, { 0, (  2 << 16)} },
    { "SYNTH_42",   V(pad1.k[ 5]), { ~(  4 << 16), ~0U  }, { 0, (  4 << 16)} },
    { "SYNTH_43",   V(pad1.k[ 5]), { ~(  8 << 16), ~0U  }, { 0, (  8 << 16)} },
    { "SYNTH_44",   V(pad1.k[ 5]), { ~( 16 << 16), ~0U  }, { 0, ( 16 << 16)} },
    { "SYNTH_45",   V(pad1.k[ 5]), { ~( 32 << 16), ~0U  }, { 0, ( 32 << 16)} },
    { "SYNTH_46",   V(pad1.k[ 5]), { ~( 64 << 16), ~0U  }, { 0, ( 64 << 16)} },
    { "SYNTH_47",   V(pad1.k[ 5]), { ~(128 << 16), ~0U  }, { 0, (128 << 16)} },
    { "SYNTH_48",   V(pad1.k[ 6]), { ~(  1 << 16), ~0U  }, { 0, (  1 << 16)} },
    
     /* Press 1+9 on controller 0 on the master component */
    { "IPAUSE0",    V(pad0.l[1]         ),  { 0,    ~0U  }, { 0,    0xA5 } },

    /* Press 1+9 on controller 1 on the master component */
    { "IPAUSE1",    V(pad0.r[1]         ),  { 0,    ~0U  }, { 0,    0xA5 } },

};

int cfg_event_action_cnt = (int)(sizeof(cfg_event_action)/sizeof(cfg_evtact_t));

/* ------------------------------------------------------------------------ */
/*  Default key bindings table.                                             */
/*                                                                          */
/*  I really need to make sure there are rows in here for all possible      */
/*  key inputs, so that when I process a config file, I can just update     */
/*  this table directly.  Otherwise, I need to duplicate this table in      */
/*  order to change it.                                                     */
/*                                                                          */
/*  Column 1 is the default.                                                */
/*  Column 2 is the ECS Keyboard setup.                                     */
/*  Column 3 is the ECS Piano setup.                                        */
/*  Column 4 is the ??? setup.                                              */
/* ------------------------------------------------------------------------ */

cfg_kbd_t  cfg_key_bind[] =
{
/* ------------------------------------------------------------------------ */
/*  Miscellaneous.                                                          */
/* ------------------------------------------------------------------------ */
{ "QUIT",   {   "QUIT",         "QUIT",         "QUIT",         "QUIT"      }},
{ "F1",     {   "QUIT",         "QUIT",         "QUIT",         "QUIT"      }},
{ "ESCAPE", {   "NA",           "NA",           "KEYB_ESC",     "NA"        }},
#ifdef macosx
{ "F3",     {   "WTOG",         "WTOG",         "WTOG",         "WTOG"      }},
{ "LCMD",   {   "PSH3",         "PSH3",         "PSH3",         "PSH3"      }},
{ "RCMD",   {   "PSH3",         "PSH3",         "PSH3",         "PSH3"      }},
#else
{ "F9",     {   "WTOG",         "WTOG",         "WTOG",         "WTOG"      }},
#endif
#ifdef WIN32
{ "LWIN",   {   "PSH3",         "PSH3",         "PSH3",         "PSH3"      }},
{ "RWIN",   {   "PSH3",         "PSH3",         "PSH3",         "PSH3"      }},
#endif
{ "F10",    {   "MOVIE",        "MOVIE",        "MOVIE",        "MOVIE"     }},
{ "F11",    {   "SHOT",         "SHOT",         "SHOT",         "SHOT"      }},
{ "F12",    {   "RESET",        "RESET",        "RESET",        "RESET"     }},
{ "HIDE",   {   "HIDE",         "HIDE",         "HIDE",         "HIDE"      }},
{ "F2",     {   "HIDE",         "HIDE",         "HIDE",         "HIDE"      }},
{ "F4",     {   "BREAK",        "BREAK",        "BREAK",        "BREAK"     }},
{ "F5",     {   "KBD0",         "KBD0",         "KBD0",         "KBD0"      }},
{ "F6",     {   "KBD1",         "KBD1",         "KBD1",         "KBD1"      }},
{ "F7",     {   "KBD2",         "KBD2",         "KBD2",         "KBD2"      }},
/*{ "F8",   {   "KBD3",         "KBD3",         "KBD3",         "KBD3"      }},*/
{ "PAUSE",  {   "PAUSE",        "PAUSE",        "PAUSE",        "PAUSE"     }},
{ "PAGEUP", {   "VOLUP",        "VOLUP",        "VOLUP",        "VOLUP"     }},
{ "PAGEDOWN",{  "VOLDN",        "VOLDN",        "VOLDN",        "VOLDN"     }},
{ "F8",     {   "PSH3",         "PSH3",         "PSH3",         "PSH3"      }},

/* ------------------------------------------------------------------------ */
/*  The numeric keypad.                                                     */
/* ------------------------------------------------------------------------ */
{ "KP7",    {   "PD0L_KP1",     "PD0L_KP1",     "KEYB_1",       "NA"        }},
{ "KP8",    {   "PD0L_KP2",     "PD0L_KP2",     "KEYB_2",       "NA"        }},
{ "KP9",    {   "PD0L_KP3",     "PD0L_KP3",     "KEYB_3",       "NA"        }},
{ "KP4",    {   "PD0L_KP4",     "PD0L_KP4",     "KEYB_4",       "NA"        }},
{ "KP5",    {   "PD0L_KP5",     "PD0L_KP5",     "KEYB_5",       "NA"        }},
{ "KP6",    {   "PD0L_KP6",     "PD0L_KP6",     "KEYB_6",       "NA"        }},
{ "KP1",    {   "PD0L_KP7",     "PD0L_KP7",     "KEYB_7",       "NA"        }},
{ "KP2",    {   "PD0L_KP8",     "PD0L_KP8",     "KEYB_8",       "NA"        }},
{ "KP3",    {   "PD0L_KP9",     "PD0L_KP9",     "KEYB_9",       "NA"        }},
{ "KP0",    {   "PD0L_KPC",     "PD0L_KPC",     "KEYB_0",       "NA"        }},
{ "KP_PERIOD",{ "PD0L_KP0",     "PD0L_KP0",     "KEYB_PERIOD",  "NA"        }},
{ "KP_ENTER", { "PD0L_KPE",     "PD0L_KPE",     "KEYB_ENTER",   "NA"        }},
                                                                
/* ------------------------------------------------------------------------ */
/*  The number keys.                                                        */
/* ------------------------------------------------------------------------ */
{ "1",      {   "PD0R_KP1",     "PD0L_KP1",     "KEYB_1",       "NA"        }},
{ "2",      {   "PD0R_KP2",     "PD0L_KP2",     "KEYB_2",       "NA"        }},
{ "3",      {   "PD0R_KP3",     "PD0L_KP3",     "KEYB_3",       "NA"        }},
{ "4",      {   "PD0R_KP4",     "PD0L_KP4",     "KEYB_4",       "NA"        }},
{ "5",      {   "PD0R_KP5",     "PD0L_KP5",     "KEYB_5",       "NA"        }},
{ "6",      {   "PD0R_KP6",     "PD0L_KP6",     "KEYB_6",       "NA"        }},
{ "7",      {   "PD0R_KP7",     "PD0L_KP7",     "KEYB_7",       "NA"        }},
{ "8",      {   "PD0R_KP8",     "PD0L_KP8",     "KEYB_8",       "NA"        }},
{ "9",      {   "PD0R_KP9",     "PD0L_KP9",     "KEYB_9",       "NA"        }},
{ "-",      {   "PD0R_KPC",     "PD0L_KPC",     "KEYB_MINUS",   "NA"        }},
{ "0",      {   "PD0R_KP0",     "PD0L_KP0",     "KEYB_0",       "NA"        }},
{ "=",      {   "PD0R_KPE",     "PD0L_KPE",     "KEYB_EQUAL",   "NA"        }},
                                                                
/* ------------------------------------------------------------------------ */
/*  Action buttons.                                                         */
/* ------------------------------------------------------------------------ */
{ "RSHIFT", {   "PD0L_A_T",     "PD0L_A_T",     "KEYB_SHIFT",   "NA"        }},
{ "RALT",   {   "PD0L_A_L",     "PD0L_A_L",     "NA",           "NA"        }},
{ "RCTRL",  {   "PD0L_A_R",     "PD0L_A_R",     "KEYB_CTRL",    "NA"        }},
                                                                
{ "LSHIFT", {   "PD0R_A_T",     "PD0L_A_T",     "KEYB_SHIFT",   "NA"        }},
{ "LALT",   {   "PD0R_A_L",     "PD0L_A_L",     "NA",           "NA"        }},
{ "LCTRL",  {   "PD0R_A_R",     "PD0L_A_R",     "KEYB_CTRL",    "NA"        }},
                                                                
/* ------------------------------------------------------------------------ */
/*  Movement keys.                                                          */
/* ------------------------------------------------------------------------ */
{ "RIGHT",  {   "PD0L_D_E",     "PD0L_D_E",     "KEYB_RIGHT",   "NA"        }},
{ "UP",     {   "PD0L_D_N",     "PD0L_D_N",     "KEYB_UP",      "NA"        }},
{ "LEFT",   {   "PD0L_D_W",     "PD0L_D_W",     "KEYB_LEFT",    "NA"        }},
{ "DOWN",   {   "PD0L_D_S",     "PD0L_D_S",     "KEYB_DOWN",    "NA"        }},
                                                                
{ "K",      {   "PD0L_D_E",     "PD0L_D_E",     "KEYB_K",       "NA"        }},
{ "O",      {   "PD0L_D_NE",    "PD0L_D_NE",    "KEYB_O",       "NA"        }},
{ "I",      {   "PD0L_D_N",     "PD0L_D_N",     "KEYB_I",       "NA"        }},
{ "U",      {   "PD0L_D_NW",    "PD0L_D_NW",    "KEYB_U",       "NA"        }},
{ "J",      {   "PD0L_D_W",     "PD0L_D_W",     "KEYB_J",       "NA"        }},
{ "N",      {   "PD0L_D_SW",    "PD0L_D_SW",    "KEYB_N",       "NA"        }},
{ "M",      {   "PD0L_D_S",     "PD0L_D_S",     "KEYB_M",       "MOVIE"     }},
{ ",",      {   "PD0L_D_SE",    "PD0L_D_SE",    "KEYB_COMMA",   "NA"        }},
                                                                
{ "D",      {   "PD0R_D_E",     "PD0L_D_E",     "KEYB_D",       "NA"        }},
{ "R",      {   "PD0R_D_NE",    "PD0L_D_NE",    "KEYB_R",       "RESET"     }},
{ "E",      {   "PD0R_D_N",     "PD0L_D_N",     "KEYB_E",       "NA"        }},
{ "W",      {   "PD0R_D_NW",    "PD0L_D_NW",    "KEYB_W",       "WTOG"      }},
{ "S",      {   "PD0R_D_W",     "PD0L_D_W",     "KEYB_S",       "SHOT"      }},
{ "Z",      {   "PD0R_D_SW",    "PD0L_D_SW",    "KEYB_Z",       "NA"        }},
{ "X",      {   "PD0R_D_S",     "PD0L_D_S",     "KEYB_X",       "NA"        }},
{ "C",      {   "PD0R_D_SE",    "PD0L_D_SE",    "KEYB_C",       "BREAK"     }},
                                                                
    /*                                                          
00FFh|                  00FEh bits                              
bits |   0       1    2  3  4    5        6      7              
-----+------------------------------------------------          
  0  | left,   comma, n, v, x, space,   [n/a], [n/a]            
  1  | period, m,     b, c, z, down,    [n/a], [n/a]            
  2  | scolon, k,     h, f, s, up,      [n/a], [n/a]            
  3  | p,      i,     y, r, w, q,       [n/a], [n/a]            
  4  | esc,    9,     7, 5, 3, 1,       [n/a], [n/a]            
  5  | 0,      8,     6, 4, 2, right,   [n/a], [n/a]            
  6  | enter,  o,     u, t, e, ctl,     [n/a], [n/a]            
  7  | [n/a],  l,     j, g, d, a,       shift, [n/a]            
  */                                                            
                                                                
/* ------------------------------------------------------------------------ */
/*  ECS Keyboard remaining keys.                                            */
/* ------------------------------------------------------------------------ */
{ "Q",      {   "NA",           "NA",           "KEYB_Q",       "QUIT"      }},
{ "T",      {   "NA",           "NA",           "KEYB_T",       "NA"        }},
{ "Y",      {   "NA",           "NA",           "KEYB_Y",       "NA"        }},
{ "P",      {   "NA",           "NA",           "KEYB_P",       "PAUSE"     }},
                                                                
{ "A",      {   "NA",           "NA",           "KEYB_A",       "NA"        }},
{ "F",      {   "NA",           "NA",           "KEYB_F",       "NA"        }},
{ "G",      {   "NA",           "NA",           "KEYB_G",       "NA"        }},
{ "H",      {   "NA",           "NA",           "KEYB_H",       "NA"        }},
{ "L",      {   "NA",           "NA",           "KEYB_L",       "NA"        }},
                                                                
{ "V",      {   "NA",           "NA",           "KEYB_V",       "NA"        }},
#ifdef N900
{ "B",      {   "QUIT",         "QUIT",         "QUIT",         "QUIT"      }},
#else
{ "B",      {   "NA",           "NA",           "KEYB_B",       "NA"        }},
#endif
{ ".",      {   "NA",           "NA",           "KEYB_PERIOD",  "NA"        }},
{ ";",      {   "NA",           "NA",           "KEYB_SEMI",    "NA"        }},
                                                                
{ " ",      {   "NA",           "NA",           "KEYB_SPACE",   "NA"        }},
{ "RETURN", {   "NA",           "NA",           "KEYB_ENTER",   "NA"        }},
{"BACKSPACE",{  "NA",           "NA",           "KEYB_LEFT",    "NA"        }},

{ "QUOTEDBL",{  "NA",           "NA",           "KEYB_QUOTE",   "NA"        }},
{ "QUOTE",  {   "NA",           "NA",           "KEYB_QUOTE",   "NA"        }},
{ "CARET",  {   "NA",           "NA",           "KEYB_CARET",   "NA"        }},
{ "HASH",   {   "NA",           "NA",           "KEYB_HASH",    "NA"        }},
{ "PLUS",   {   "NA",           "NA",           "KEYB_PLUS",    "NA"        }},
{ "SLASH",  {   "NA",           "NA",           "KEYB_SLASH",   "NA"        }},
{ "DOLLAR", {   "NA",           "NA",           "KEYB_DOLLAR",  "NA"        }},
{ "ASTERISK",{  "NA",           "NA",           "KEYB_STAR",    "NA"        }},
{ "LEFTPAREN",{ "NA",           "NA",           "KEYB_LPAREN",  "NA"        }},
{ "RIGHTPAREN",{"NA",           "NA",           "KEYB_RPAREN",  "NA"        }},
{ "QUESTION",{  "NA",           "NA",           "KEYB_QUEST",   "NA"        }},

/* ------------------------------------------------------------------------ */
/*  Default Joystick 0 mapping.                                             */
/* ------------------------------------------------------------------------ */
{ "JS0_E",    { "PD0L_J_E",     "PD0L_J_E",     "PD0L_J_E",     "PD0L_J_E"  }},
{ "JS0_ENE",  { "PD0L_J_ENE",   "PD0L_J_ENE",   "PD0L_J_ENE",   "PD0L_J_ENE"}},
{ "JS0_NE",   { "PD0L_J_NE",    "PD0L_J_NE",    "PD0L_J_NE",    "PD0L_J_NE" }},
{ "JS0_NNE",  { "PD0L_J_NNE",   "PD0L_J_NNE",   "PD0L_J_NNE",   "PD0L_J_NNE"}},
{ "JS0_N",    { "PD0L_J_N",     "PD0L_J_N",     "PD0L_J_N",     "PD0L_J_N"  }},
{ "JS0_NNW",  { "PD0L_J_NNW",   "PD0L_J_NNW",   "PD0L_J_NNW",   "PD0L_J_NNW"}},
{ "JS0_NW",   { "PD0L_J_NW",    "PD0L_J_NW",    "PD0L_J_NW",    "PD0L_J_NW" }},
{ "JS0_WNW",  { "PD0L_J_WNW",   "PD0L_J_WNW",   "PD0L_J_WNW",   "PD0L_J_WNW"}},
{ "JS0_W",    { "PD0L_J_W",     "PD0L_J_W",     "PD0L_J_W",     "PD0L_J_W"  }},
{ "JS0_WSW",  { "PD0L_J_WSW",   "PD0L_J_WSW",   "PD0L_J_WSW",   "PD0L_J_WSW"}},
{ "JS0_SW",   { "PD0L_J_SW",    "PD0L_J_SW",    "PD0L_J_SW",    "PD0L_J_SW" }},
{ "JS0_SSW",  { "PD0L_J_SSW",   "PD0L_J_SSW",   "PD0L_J_SSW",   "PD0L_J_SSW"}},
{ "JS0_S",    { "PD0L_J_S",     "PD0L_J_S",     "PD0L_J_S",     "PD0L_J_S"  }},
{ "JS0_SSE",  { "PD0L_J_SSE",   "PD0L_J_SSE",   "PD0L_J_SSE",   "PD0L_J_SSE"}},
{ "JS0_SE",   { "PD0L_J_SE",    "PD0L_J_SE",    "PD0L_J_SE",    "PD0L_J_SE" }},
{ "JS0_ESE",  { "PD0L_J_ESE",   "PD0L_J_ESE",   "PD0L_J_ESE",   "PD0L_J_ESE"}},

#ifndef GP2X
{"JS0_BTN_00",{ "PD0L_A_T",     "PD0L_A_T",     "PD0L_A_T",     "PD0L_A_T"  }},
{"JS0_BTN_01",{ "PD0L_A_L",     "PD0L_A_L",     "PD0L_A_L",     "PD0L_A_L"  }},
{"JS0_BTN_02",{ "PD0L_A_R",     "PD0L_A_R",     "PD0L_A_R",     "PD0L_A_R"  }},
{"JS0_BTN_03",{ "PD0L_A_T",     "PD0L_A_T",     "PD0L_A_T",     "PD0L_A_T"  }},
{"JS0_BTN_04",{ "PD0L_A_L",     "PD0L_A_L",     "PD0L_A_L",     "PD0L_A_L"  }},
{"JS0_BTN_05",{ "PD0L_A_R",     "PD0L_A_R",     "PD0L_A_R",     "PD0L_A_R"  }},
{"JS0_BTN_06",{ "PD0L_A_T",     "PD0L_A_T",     "PD0L_A_T",     "PD0L_A_T"  }},
{"JS0_BTN_07",{ "PD0L_A_L",     "PD0L_A_L",     "PD0L_A_L",     "PD0L_A_L"  }},
{"JS0_BTN_08",{ "PD0L_A_R",     "PD0L_A_R",     "PD0L_A_R",     "PD0L_A_R"  }},

#else /* GP2X specific mappings. */

/* Directional controller */
{"JS0_BTN_00",{ "PD0L_D_N" ,    "PD0L_D_N" ,    "PD0L_D_N" ,    "PD0L_D_N"  }},
{"JS0_BTN_01",{ "PD0L_D_NW",    "PD0L_D_NW",    "PD0L_D_NW",    "PD0L_D_NW" }},
{"JS0_BTN_02",{ "PD0L_D_W" ,    "PD0L_D_W" ,    "PD0L_D_W" ,    "PD0L_D_W"  }},
{"JS0_BTN_03",{ "PD0L_D_SW",    "PD0L_D_SW",    "PD0L_D_SW",    "PD0L_D_SW" }},
{"JS0_BTN_04",{ "PD0L_D_S" ,    "PD0L_D_S" ,    "PD0L_D_S" ,    "PD0L_D_S"  }},
{"JS0_BTN_05",{ "PD0L_D_SE",    "PD0L_D_SE",    "PD0L_D_SE",    "PD0L_D_SE" }},
{"JS0_BTN_06",{ "PD0L_D_E" ,    "PD0L_D_E" ,    "PD0L_D_E" ,    "PD0L_D_E"  }},
{"JS0_BTN_07",{ "PD0L_D_NE",    "PD0L_D_NE",    "PD0L_D_NE",    "PD0L_D_NE" }},

/* Others:  Action, Volume, Quit, etc. */
/* GP2X button mapping:
    JS0_BTN_08  # start button
    JS0_BTN_09  # select button
    JS0_BTN_10  # button L
    JS0_BTN_11  # button R
    JS0_BTN_12  # button A
    JS0_BTN_13  # button B
    JS0_BTN_14  # button Y
    JS0_BTN_15  # button X
    JS0_BTN_16  # volume up
    JS0_BTN_17  # volume down
    JS0_BTN_18  # stick press, used for shifting to map 1
*/

{"JS0_BTN_08",{ "PD0R_KPE",   "QUIT"    ,   "PD0R_KPE",   "QUIT"       }},
{"JS0_BTN_09",{ "PD0R_KPC",   "RESET"   ,   "PD0R_KPC",   "RESET"      }},
{"JS0_BTN_10",{ "PD0L_D_W",   "PD0R_KP1",   "PD0L_D_W",   "PD0R_KP1"   }},
{"JS0_BTN_11",{ "PD0L_D_E",   "PD0R_KP2",   "PD0L_D_E",   "PD0R_KP2"   }},
{"JS0_BTN_12",{ "PD0L_A_T",   "PD0R_KP4",   "PD0L_A_T",   "PD0R_KP4"   }},
{"JS0_BTN_13",{ "PD0L_A_R",   "PD0R_KP5",   "PD0L_A_R",   "PD0R_KP5"   }},
{"JS0_BTN_14",{ "PD0L_A_T",   "PD0R_KP3",   "PD0L_A_T",   "PD0R_KP3"   }},
{"JS0_BTN_15",{ "PD0L_A_L",   "PD0R_KP6",   "PD0L_A_L",   "PD0R_KP6"   }},
{"JS0_BTN_16",{ "VOLUP"   ,   "PD0R_KP8",   "VOLUP"   ,   "PD0R_KP8"   }},
{"JS0_BTN_17",{ "VOLDN"   ,   "PD0R_KP7",   "VOLDN"   ,   "PD0R_KP7"   }},
{"JS0_BTN_18",{ "SHF10"   ,   "SHF10"   ,   "SHF10"   ,   "SHF10"      }},
#endif
                                                                
#ifndef WII
{"JS0_HAT0_E", {"PD0R_KP6",     "PD0R_KP6",     "PD0R_KP6",     "PD0R_KP6"  }},
{"JS0_HAT0_NE",{"PD0R_KP3",     "PD0R_KP3",     "PD0R_KP3",     "PD0R_KP3"  }},
{"JS0_HAT0_N", {"PD0R_KP2",     "PD0R_KP2",     "PD0R_KP2",     "PD0R_KP2"  }},
{"JS0_HAT0_NW",{"PD0R_KP1",     "PD0R_KP1",     "PD0R_KP1",     "PD0R_KP1"  }},
{"JS0_HAT0_W", {"PD0R_KP4",     "PD0R_KP4",     "PD0R_KP4",     "PD0R_KP4"  }},
{"JS0_HAT0_SW",{"PD0R_KP7",     "PD0R_KP7",     "PD0R_KP7",     "PD0R_KP7"  }},
{"JS0_HAT0_S", {"PD0R_KP8",     "PD0R_KP8",     "PD0R_KP8",     "PD0R_KP8"  }},
{"JS0_HAT0_SE",{"PD0R_KP9",     "PD0R_KP9",     "PD0R_KP9",     "PD0R_KP9"  }},
#else /* WII Specific Bindings */

{"JS0_BTN_09", {"PD0L_KPE",     "QUIT"    ,     "PD0R_KPE",     "QUIT"      }},
{"JS0_BTN_10", {"PD0L_KPC",     "RESET"   ,     "PD0R_KPC",     "RESET"     }},
{"JS0_BTN_11", {"PD0L_KP5",     "PD0R_KP1",     "PD0L_D_W",     "PD0R_KP1"  }},
{"JS0_BTN_12", {"PD0L_KP0",     "PD0R_KP2",     "PD0L_D_E",     "PD0R_KP2"  }},
{"JS0_HAT0_E", {"PD0L_KP6",     "PD0R_KP6",     "PD0R_KP6",     "PD0R_KP6"  }},
{"JS0_HAT0_NE",{"PD0L_KP3",     "PD0R_KP3",     "PD0R_KP3",     "PD0R_KP3"  }},
{"JS0_HAT0_N", {"PD0L_KP2",     "PD0R_KP2",     "PD0R_KP2",     "PD0R_KP2"  }},
{"JS0_HAT0_NW",{"PD0L_KP1",     "PD0R_KP1",     "PD0R_KP1",     "PD0R_KP1"  }},
{"JS0_HAT0_W", {"PD0L_KP4",     "PD0R_KP4",     "PD0R_KP4",     "PD0R_KP4"  }},
{"JS0_HAT0_SW",{"PD0L_KP7",     "PD0R_KP7",     "PD0R_KP7",     "PD0R_KP7"  }},
{"JS0_HAT0_S", {"PD0L_KP8",     "PD0R_KP8",     "PD0R_KP8",     "PD0R_KP8"  }},
{"JS0_HAT0_SE",{"PD0L_KP9",     "PD0R_KP9",     "PD0R_KP9",     "PD0R_KP9"  }},
{"JS1_BTN_00", {"PD0R_A_T",     "PD0L_A_T",     "PD0L_A_T",     "PD0L_A_T"  }},
{"JS1_BTN_01", {"PD0R_A_L",     "PD0L_A_L",     "PD0L_A_L",     "PD0L_A_L"  }},
{"JS1_BTN_02", {"PD0R_A_R",     "PD0L_A_R",     "PD0L_A_R",     "PD0L_A_R"  }},
{"JS1_BTN_09", {"PD0R_KPE",     "QUIT"    ,     "PD0R_KPE",     "QUIT"      }},
{"JS1_BTN_10", {"PD0R_KPC",     "RESET"   ,     "PD0R_KPC",     "RESET"     }},
{"JS1_BTN_11", {"PD0R_KP5",     "PD0R_KP1",     "PD0L_D_W",     "PD0R_KP1"  }},
{"JS1_BTN_12", {"PD0R_KP0",     "PD0R_KP2",     "PD0L_D_E",     "PD0R_KP2"  }},
{"JS1_E",      {"PD0R_J_E",     "PD0L_J_E",     "PD0L_J_E",     "PD0L_J_E"  }},
{"JS1_ENE",    {"PD0R_J_ENE",   "PD0L_J_ENE",   "PD0L_J_ENE",   "PD0L_J_ENE"}},
{"JS1_NE",     {"PD0R_J_NE",    "PD0L_J_NE",    "PD0L_J_NE",    "PD0L_J_NE" }},
{"JS1_NNE",    {"PD0R_J_NNE",   "PD0L_J_NNE",   "PD0L_J_NNE",   "PD0L_J_NNE"}},
{"JS1_N",      {"PD0R_J_N",     "PD0L_J_N",     "PD0L_J_N",     "PD0L_J_N"  }},
{"JS1_NNW",    {"PD0R_J_NNW",   "PD0L_J_NNW",   "PD0L_J_NNW",   "PD0L_J_NNW"}},
{"JS1_NW",     {"PD0R_J_NW",    "PD0L_J_NW",    "PD0L_J_NW",    "PD0L_J_NW" }},
{"JS1_WNW",    {"PD0R_J_WNW",   "PD0L_J_WNW",   "PD0L_J_WNW",   "PD0L_J_WNW"}},
{"JS1_W",      {"PD0R_J_W",     "PD0L_J_W",     "PD0L_J_W",     "PD0L_J_W"  }},
{"JS1_WSW",    {"PD0R_J_WSW",   "PD0L_J_WSW",   "PD0L_J_WSW",   "PD0L_J_WSW"}},
{"JS1_SW",     {"PD0R_J_SW",    "PD0L_J_SW",    "PD0L_J_SW",    "PD0L_J_SW" }},
{"JS1_SSW",    {"PD0R_J_SSW",   "PD0L_J_SSW",   "PD0L_J_SSW",   "PD0L_J_SSW"}},
{"JS1_S",      {"PD0R_J_S",     "PD0L_J_S",     "PD0L_J_S",     "PD0L_J_S"  }},
{"JS1_SSE",    {"PD0R_J_SSE",   "PD0L_J_SSE",   "PD0L_J_SSE",   "PD0L_J_SSE"}},
{"JS1_SE",     {"PD0R_J_SE",    "PD0L_J_SE",    "PD0L_J_SE",    "PD0L_J_SE" }},
{"JS1_ESE",    {"PD0R_J_ESE",   "PD0L_J_ESE",   "PD0L_J_ESE",   "PD0L_J_ESE"}},
{"JS1_HAT0_E", {"PD0R_KP6",     "PD0R_KP6",     "PD0R_KP6",     "PD0R_KP6"  }},
{"JS1_HAT0_NE",{"PD0R_KP3",     "PD0R_KP3",     "PD0R_KP3",     "PD0R_KP3"  }},
{"JS1_HAT0_N", {"PD0R_KP2",     "PD0R_KP2",     "PD0R_KP2",     "PD0R_KP2"  }},
{"JS1_HAT0_NW",{"PD0R_KP1",     "PD0R_KP1",     "PD0R_KP1",     "PD0R_KP1"  }},
{"JS1_HAT0_W", {"PD0R_KP4",     "PD0R_KP4",     "PD0R_KP4",     "PD0R_KP4"  }},
{"JS1_HAT0_SW",{"PD0R_KP7",     "PD0R_KP7",     "PD0R_KP7",     "PD0R_KP7"  }},
{"JS1_HAT0_S", {"PD0R_KP8",     "PD0R_KP8",     "PD0R_KP8",     "PD0R_KP8"  }},
{"JS1_HAT0_SE",{"PD0R_KP9",     "PD0R_KP9",     "PD0R_KP9",     "PD0R_KP9"  }},

/* Press 1+9 on controller 0 on the master component */
{ "JS0_BTN_13",{"IPAUSE0",      "IPAUSE0",      "IPAUSE0",      "IPAUSE0"   }},
/* Press 1+9 on controller 1 on the master component */
{ "JS1_BTN_13",{"IPAUSE1",      "IPAUSE1",      "IPAUSE1",      "IPAUSE1"   }},
#endif
#ifdef N900
{ "Q",      {   "PD0R_KP1",     "PD0L_KP1",     "KEYB_1",       "NA"        }},
{ "W",      {   "PD0R_KP2",     "PD0L_KP2",     "KEYB_2",       "NA"        }},
{ "E",      {   "PD0R_KP3",     "PD0L_KP3",     "KEYB_3",       "NA"        }},
{ "R",      {   "PD0R_KP4",     "PD0L_KP4",     "KEYB_4",       "NA"        }},
{ "T",      {   "PD0R_KP5",     "PD0L_KP5",     "KEYB_5",       "NA"        }},
{ "Y",      {   "PD0R_KP6",     "PD0L_KP6",     "KEYB_6",       "NA"        }},
{ "U",      {   "PD0R_KP7",     "PD0L_KP7",     "KEYB_7",       "NA"        }},
{ "I",      {   "PD0R_KP8",     "PD0L_KP8",     "KEYB_8",       "NA"        }},
{ "O",      {   "PD0R_KP9",     "PD0L_KP9",     "KEYB_9",       "NA"        }},
{ "COMMA",  {   "PD0R_KPC",     "PD0L_KPC",     "KEYB_MINUS",   "NA"        }},
{ "P",      {   "PD0R_KP0",     "PD0L_KP0",     "KEYB_0",       "NA"        }},
{ "F",      {   "BREAK",        "BREAK",        "BREAK",        "BREAK"     }},
{ "G",      {   "KBD0",         "KBD0",         "KBD0",         "KBD0"      }},
{ "H",      {   "KBD1",         "KBD1",         "KBD1",         "KBD1"      }},
{ "J",      {   "RESET",        "RESET",        "RESET",        "RESET"     }},
#endif
{ NULL,     {   NULL,           NULL,           NULL,           NULL        }},
};

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
/*                 Copyright (c) 1998-2006, Joseph Zbiciak                  */
/* ======================================================================== */
