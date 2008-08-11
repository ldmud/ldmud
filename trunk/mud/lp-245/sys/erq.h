/* external request demon interface definitions */

#ifndef LPC__ERQ_H__
#define LPC__ERQ_H__ 1

/* servive request types */

#define ERQ_RLOOKUP     0  /* Lookup ip -> name */
#define ERQ_EXECUTE     1  /* Execute a program */
#define ERQ_FORK        2  /* Fork a program */
#define ERQ_AUTH        3  /* Connect to a remote authd */
#define ERQ_SPAWN       4  /* Spawn a program */
#define ERQ_SEND        5  /* Send data to a program or connection */
#define ERQ_KILL        6  /* Kill a program or connection */
#define ERQ_OPEN_UDP    7  /* Open a UDP socket */
#define ERQ_OPEN_TCP    8  /* Open a TCP connection */
#define ERQ_LISTEN      9  /* Open a TCP accept-socket */
#define ERQ_ACCEPT     10  /* Accept a connection from a accept-socket */
#define ERQ_LOOKUP     11  /* Lookup name -> ip */

#ifdef __IPV6__
#define ERQ_RLOOKUPV6  12  /* Lookup name/ip6 */
#endif

/* Additional service request type flags evaluated by efun send_erq().
 * The ERQ itself won't get to see it.
 */

#define ERQ_CB_STRING  (1 << 31)  /* Callback closure takes a string arg */


/* answers from ERQ_EXECUTE / ERQ_FORK */

#define ERQ_OK		  0
#define ERQ_SIGNALED	  1
#define ERQ_E_NOTFOUND	  2  /* process not found by wait */
#define ERQ_E_UNKNOWN	  3  /* unknown exit condition from wait() */
#define ERQ_E_ARGLENGTH	  4
#define ERQ_E_ARGFORMAT	  5
#define ERQ_E_ARGNUMBER	  6
#define ERQ_E_ILLEGAL	  7
#define ERQ_E_PATHLEN	  8
#define ERQ_E_FORKFAIL	  9
#define ERQ_E_TICKET	 11
#define ERQ_E_INCOMPLETE 12
#define ERQ_E_WOULDBLOCK 13
#define ERQ_E_PIPE	 14
#define ERQ_STDOUT	 15  /* Normal data received */
#define ERQ_STDERR	 16
#define ERQ_EXITED	 17  /* Connection closed on EOF */
#define ERQ_E_NSLOTS	 18

/* reserved handles */

#define ERQ_HANDLE_RLOOKUP (-1)
#define ERQ_HANDLE_KEEP_HANDLE (-2)
#define ERQ_HANDLE_RLOOKUPV6 (-3)

#endif /* LPC__ERQ_H__ */
