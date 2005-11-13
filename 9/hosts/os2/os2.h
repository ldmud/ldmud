#define SOCKET_HEADER "hosts/os2/socket.h"
#define SOCKET_INC "hosts/os2/socket.c"

#undef ALARM_HANDLER
#define ALARM_HANDLER(name, body)				\
void name() {							\
    (void)signal(SIGALRM, (RETSIGTYPE(*)PROT((int)))name);	\
    (void)signal(SIGALRM, SIG_ACK);				\
    {body}							\
}

