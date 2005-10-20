/* hosts/amiga/nsignal.h */

#ifndef NSIGNAL_H
#define NSIGNAL_H

#include <exec/types.h>
#include <signal.h>

/* LPMud signals aren't raised by other programs, but by external events,
** so the normal signal()/raise() mechanism provided by DICE isn't enough
** since it doesn't uses task-exception handlers.
*/

#define signal(a,b) new_signal(a,b)

#ifdef __SASC

#define SIGPIPE 13

typedef void (*__sig_func)(int);

#define STKARGS __stdargs
#define GETA4   __saveds
#define ASM     __asm
#define REGD0   register __d0
#define SIGFUNC __sig_func

#else /* _DCC */

#define STKARGS __stkargs
#define GETA4   __saveds
#define ASM
#define REGD0   __D0
#define SIGFUNC __sigfunc

#endif

/* New signals */

#define SIGALRM (NSIG-1)
#define SIGUSR1 (NSIG-2)
#define SIGURG  (NSIG-3)
#ifdef __SASC
#define SIGHUP  (NSIG-4)
#endif

/* Used standard task signals */

#define EXT_SIGHUP  SIGBREAKF_CTRL_C  /* Ctrl-C: hang-up */
#define EXT_SIGINT  SIGBREAKF_CTRL_E  /* Ctrl-E: interrupt select() */
#define EXT_SIGUSR  SIGBREAKF_CTRL_F  /* Ctrl-F: update master */

/* Prototypes */

extern STKARGS unsigned int alarm (unsigned int);
extern STKARGS SIGFUNC new_signal (int, SIGFUNC);
extern STKARGS int start_timer(struct timeval *, struct timerequest *);
extern STKARGS int setup_timer (LONG, struct timerequest **);
extern STKARGS void cleanup_timer (struct timerequest **);
extern STKARGS void cleanup_alarm ( void );
extern STKARGS ULONG check_signals ( void );

#endif
