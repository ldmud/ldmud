/* hosts/amiga/socket_sim_protos.h */

#ifndef SOCKET_SIM_PROTOS_H
#define SOCKET_SIM_PROTOS_H

#include <exec/types.h>
#include <sys/types.h>

extern int sim_shutdown (int, int );
extern int sim_write (int, char * , int );
extern int sim_read (int , char * , int );
extern int find_free_socket ();
extern int sim_socket (int , int , int );
extern int sim_accept (int , struct sockaddr * , int * );
extern void sim_close (int);
extern int sim_bind (int , struct sockaddr * , int );
extern int sim_select (int , fd_set * , fd_set * , fd_set *
                      , struct timeval * );
extern struct hostent *sim_gethostbyname (char * );
extern int sim_gethostname(char * , int );
extern int sim_getdomainname(char * , int );
extern int sim_getsockname(int, struct sockaddr *, int *);
extern char *sim_inet_ntoa(struct in_addr );
extern unsigned long sim_inet_addr (const char *);
extern LONG getpeername (LONG, struct sockaddr * , LONG * );

#ifdef __SASC
extern int errno;
#endif
#endif /* SOCKET_SIM_PROTOS_H */
