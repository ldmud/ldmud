#ifndef SYS_TYPES_H
#define SYS_TYPES_H

typedef	char * 		caddr_t;
typedef unsigned long 	u_long;
typedef unsigned int	u_int;
typedef unsigned short	u_short;
typedef unsigned char	u_char;
typedef unsigned short	ushort;
typedef unsigned long	ulong;
typedef unsigned char	uchar;
typedef short		dev_t;
typedef long		off_t;
typedef short		uid_t;
typedef short		gid_t;
typedef short		mode_t;
typedef unsigned long	ino_t;

#define	btod(p, t) ((t)(((long)p)<<2))
#define dtob(p) ((BPTR)((long)(p)>>2))

#define MIN(x, y) ((x) < (y) ? (x):(y))
#define MAX(x, y) ((x) > (y) ? (x):(y))
#ifndef min
#define min(x, y) MIN(x, y)
#endif
#ifndef max
#define max(x, y) MAX(x, y)
#endif

#define DOS_TRUE -1
#define DOS_FALSE 0

#ifndef NULL
#define NULL 0L
#endif

/* Unix bstring macros */
#define bzero(a,n)	memset(a,'\0',n)
#define bcmp(a,b,n)	memcmp(a,b,n)
#define bcopy(from,to,len) memcpy(to,from,len)

/*
 * Select mask manipulations from Berkeley Unix.
 */
#define NFDPOW		5
#define NFDBITS		(1 << NFDPOW)		/* 32 bits/word */

/* There is a max of 128 sockets per process allowed */
#ifndef FD_SETSIZE
#define FD_SETSIZE	128
#endif

typedef struct fd_set {
	u_long	fds_bits[FD_SETSIZE/NFDBITS];
} fd_set;

#define OFF(bit) ((bit)>>NFDPOW)
#define BIT(bit) ((bit)&(NFDBITS-1))

#define FD_SET(bit, setp) ((setp)->fds_bits[OFF(bit)] |= (1 << BIT(bit)))
#define FD_CLR(bit, setp) ((setp)->fds_bits[OFF(bit)] &= ~(1 << BIT(bit)))
#define FD_ISSET(bit, setp) ((setp)->fds_bits[OFF(bit)] & (1 << BIT(bit)))
#define FD_ZERO(setp) bzero(setp, sizeof(fd_set));

#endif /* SYS_TYPES_H */
