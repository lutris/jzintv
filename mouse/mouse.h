/*
 * ============================================================================
 *  Title:    Mouse Support via SDL
 *  Author:   J. Zbiciak
 * ============================================================================
 *  
 *  jzIntv doesn't really support mice at this time.  This module is a 
 *  simple module to convert xrel/yrel events into direction events, and
 *  capture mouse button events similarly to joystick buttons.
 *  
 *  It should be considered highly experimental.
 *  
 * ============================================================================
 */
#ifndef MOUSE_H_
#define MOUSE_H_

/* ======================================================================== */
/*  MOUSE_DECODE_EVENT -- Pull apart an SDL_EVENT and turn it into our      */
/*                        internal event numbers.                           */
/* ======================================================================== */
void mouse_decode_event(SDL_Event *ev, int *ev_updn, uint_32 *ev_num,
                                       int *ex_updn, uint_32 *ex_num);


/* ======================================================================== */
/*  MOUSE_PUMP          -- Decide whether to send a mouse up event          */
/* ======================================================================== */
void mouse_pump(int *ev_updn, uint_32 *ev_num);

#endif
