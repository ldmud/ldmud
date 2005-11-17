#ifndef P_DEBUGGER__H
#define P_DEBUGGER__H

#define DEBUG(x, y) "/p/Tool/apps/debugger"->debug(x, this_object(), y)
#define DEBUG2(x) "/p/Tool/apps/debugger"->add_debug2(this_object(), sprintf("%O\n", x))

#define DMASTER touch("/p/Tool/apps/debugger.c")
#define D_SCHON_ANGEMELDET 1
#define D_ANMELDUNG_OK 2
#define D_ABMELDUNG_OK 3
#define D_OWNER_NICHT_ANGEMELDET 4
#define D_ANMELDUNG_ALREADY 5
#define D_NO_ANMELDUNG 6
#define D_WRONG_ANMELDUNG 7

#endif /* P_DEBUGGER__H */

