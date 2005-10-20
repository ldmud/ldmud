#ifndef _driverv6_

	#define _driverv6_
	#define sockaddr_in sockaddr_in6

	#define sin_port sin6_port
	#define sin_addr sin6_addr
	#define sin_family sin6_family
	#define s_addr s6_addr
	#define in_addr in6_addr

char *inet6_ntoa(struct in6_addr in);

struct in6_addr inet6_addr(char *to_host);

#endif

