#include <windows.h>

#define SOCKET_HEADER "hosts/GnuWin32/socket.h"
#define SOCKET_INC "hosts/GnuWin32/socket.c"

#undef HOST_DEPENDENT_INIT
#define HOST_DEPENDENT_INIT
  win32osi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO); \
  GetVersionEx(&win32osi); \
  GetSystemInfo(&win32si); \

extern OSVERSIONINFO win32osi;
extern SYSTEM_INFO win32si;

#define alarm(n)

#undef ALARM_HANDLER
#define ALARM_HANDLER(name, body)				\
VOID CALLBACK catch_alarm(UINT uID,   UINT uMsg, DWORD dwUser,	\
			  DWORD dw1, DWORD dw2) {		\
    WSACancelBlockingCall();					\
    {body}                                                      \
}

#undef ALARM_HANDLER_FIRST_CALL
#define ALARM_HANDLER_FIRST_CALL(name) {			\
    VOID CALLBACK name(UINT uID,   UINT uMsg,			\
		       DWORD dwUser, DWORD dw1, DWORD dw2);	\
								\
    if(!timeSetEvent(2000,0,name,NULL,TIME_PERIODIC)) {		\
	perror("SetTimer()"); 					\
	return;		/* Abort */				\
    }								\
    name(0,0,0,0,0);						\
}
