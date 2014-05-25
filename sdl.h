/* ======================================================================== */
/*  Change the following to #if 1 if using an old FreeBSD SDL port.         */
/* ======================================================================== */
#if 0
# ifdef __FreeBSD__
#  define SDL_H_ 1
#  include <SDL11/SDL.h>
#  include <SDL11/SDL_audio.h>
#  include <SDL11/SDL_events.h>
#  include <SDL11/SDL_error.h>
#  include <SDL11/SDL_thread.h>
# endif
#endif

#ifndef SDL_H_ 
# define SDL_H_ 1
# include <SDL/SDL.h>
# include <SDL/SDL_audio.h>
# include <SDL/SDL_events.h>
# include <SDL/SDL_error.h>
# include <SDL/SDL_thread.h>
#endif
