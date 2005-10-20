#include <netinet/in.h>
#include <sys/socket.h>

#include "mudipv6.h"

char *inet6_ntoa(struct in6_addr in)
{
	static char str[4097];

	inet_ntop(AF_INET6, &in, str, 4096);
	return str;
}


struct in6_addr inet6_addr(char *to_host)
{
	struct in6_addr addr;

	inet_pton(AF_INET6, to_host, &addr);
	return addr;
}

