#include <errno.h>
#include <winsock.h>
#include <windows.h>

void set_socket_errno()
{
    switch(WSAGetLastError())
      {
	case WSAEWOULDBLOCK:		errno = EWOULDBLOCK; return;
	case WSAEINPROGRESS:		errno = EINPROGRESS; return;
	case WSAEALREADY:		errno = EALREADY; return;
	case WSAENOTSOCK:		errno = ENOTSOCK; return;
	case WSAEDESTADDRREQ:		errno = EDESTADDRREQ; return;
	case WSAEMSGSIZE:		errno = EMSGSIZE; return;
	case WSAEPROTOTYPE:		errno = EPROTOTYPE; return;
	case WSAENOPROTOOPT:		errno = ENOPROTOOPT; return;
	case WSAEPROTONOSUPPORT:	errno = EPROTONOSUPPORT; return;
	case WSAESOCKTNOSUPPORT:	errno = ESOCKTNOSUPPORT; return;
	case WSAEOPNOTSUPP:		errno = EOPNOTSUPP; return;
	case WSAEPFNOSUPPORT:		errno = EPFNOSUPPORT; return;
	case WSAEAFNOSUPPORT:		errno = EAFNOSUPPORT; return;
	case WSAEADDRINUSE:		errno = EADDRINUSE; return;
	case WSAEADDRNOTAVAIL:		errno = EADDRNOTAVAIL; return;
	case WSAENETDOWN:		errno = ENETDOWN; return;
	case WSAENETUNREACH:		errno = ENETUNREACH; return;
	case WSAENETRESET:		errno = ENETRESET; return;
	case WSAECONNABORTED:		errno = ECONNABORTED; return;
	case WSAECONNRESET:		errno = ECONNRESET; return;
	case WSAENOBUFS:		errno = ENOBUFS; return;
	case WSAEISCONN:		errno = EISCONN; return;
	case WSAENOTCONN:		errno = ENOTCONN; return;
	case WSAESHUTDOWN:		errno = ESHUTDOWN; return;
	case WSAETOOMANYREFS:		errno = ETOOMANYREFS; return;
	case WSAETIMEDOUT:		errno = ETIMEDOUT; return;
	case WSAECONNREFUSED:		errno = ECONNREFUSED; return;
	case WSAELOOP:			errno = ELOOP; return;
	case WSAENAMETOOLONG:		errno = ENAMETOOLONG; return;
	case WSAEHOSTDOWN:		errno = EHOSTDOWN; return;
	case WSAEHOSTUNREACH:		errno = EHOSTUNREACH; return;
	case WSAENOTEMPTY:		errno = ENOTEMPTY; return;
	case WSAEPROCLIM:		errno = EPROCLIM; return;
	case WSAEUSERS:			errno = EUSERS; return;
	case WSAEDQUOT:			errno = EDQUOT; return;
	case WSAESTALE:			errno = ESTALE; return;
	case WSAEREMOTE:		errno = EREMOTE; return;
      }
}

OSVERSIONINFO win32osi;
SYSTEM_INFO win32si;
