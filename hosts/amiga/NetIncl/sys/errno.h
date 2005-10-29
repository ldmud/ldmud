#ifndef SYS_ERRNO_H
#define SYS_ERRNO_H
/*
 * Amiga standard errno.h
 */
#define UNIXERR

#define EOSERR		-1	/* AmigaDOS error -- see IoErr()	*/

#define EPERM		1	/* no permission to access object	*/
#define ENOENT		2	/* no such file, dir, or volume		*/
#define ESRCH		3	/* no such process			*/
#define EINTR		4	/* interrupted system call		*/
#define EIO		5	/* I/O error				*/
#define ENXIO		6	/* No such device or address		*/
#define E2BIG		7	/* agrument is too big			*/
#define ENOEXEC		8	/* exec error				*/
#define EBADF		9	/* bad file handle			*/
#define ECHILD		10	/* child process error			*/
#define EAGAIN		11	/* no more processes allowed		*/
#define ENOMEM		12	/* no memory				*/
#define EACCES		13	/* access denied			*/
#define EFAULT		14	/* bad address				*/
#define ENOTBLK		15	/* block device required		*/
#define EBUSY		16	/* object (file, dir, volume) is busy	*/
#define EEXIST		17	/* object already exists		*/
#define EXDEV		18	/* cross device link			*/
#define ENODEV		19	/* no such device			*/
#define ENOTDIR		20	/* object is not directory		*/
#define EISDIR		21	/* object is directory/volume		*/
#define EINVAL		22	/* invalid argument			*/
#define ENFILE		23	/* no more files allowed		*/
#define EMFILE		24	/* no more files allowed for this proc	*/
#define ENOTTY		25	/* not a terminal			*/
#define ETXTBSY		26	/* text file is busy			*/
#define EFBIG		27	/* file is too large			*/
#define ENOSPC		28	/* no space on device			*/
#define ESPIPE		29	/* seek on device not able to seek	*/
#define EROFS		30	/* read only filesystem			*/
#define EMLINK		31	/* too many links			*/
#define EPIPE		32	/* broken pipe: connection		*/
#define EDOM		33	/* math func domain error		*/
#define ERANGE		34	/* math func result out of range	*/

#ifdef UNIXERR
/* non-blocking and interrupt i/o */
#define	EWOULDBLOCK	35		/* Operation would block */
#define	EINPROGRESS	36		/* Operation now in progress */
#define	EALREADY	37		/* Operation already in progress */
/* ipc/network software */

	/* argument errors */
#define	ENOTSOCK	38		/* Socket operation on non-socket */
#define	EDESTADDRREQ	39		/* Destination address required */
#define	EMSGSIZE	40		/* Message too long */
#define	EPROTOTYPE	41		/* Protocol wrong type for socket */
#define	ENOPROTOOPT	42		/* Protocol not available */
#define	EPROTONOSUPPORT	43		/* Protocol not supported */
#define	ESOCKTNOSUPPORT	44		/* Socket type not supported */
#define	EOPNOTSUPP	45		/* Operation not supported on socket */
#define	EPFNOSUPPORT	46		/* Protocol family not supported */
#define	EAFNOSUPPORT	47		/* Address family not supported by protocol family */
#define	EADDRINUSE	48		/* Address already in use */
#define	EADDRNOTAVAIL	49		/* Can't assign requested address */

	/* operational errors */
#define	ENETDOWN	50		/* Network is down */
#define	ENETUNREACH	51		/* Network is unreachable */
#define	ENETRESET	52		/* Network dropped connection on reset */
#define	ECONNABORTED	53		/* Software caused connection abort */
#define	ECONNRESET	54		/* Connection reset by peer */
#define	ENOBUFS		55		/* No buffer space available */
#define	EISCONN		56		/* Socket is already connected */
#define	ENOTCONN	57		/* Socket is not connected */
#define	ESHUTDOWN	58		/* Can't send after socket shutdown */
#define	ETOOMANYREFS	59		/* Too many references: can't splice */
#define	ETIMEDOUT	60		/* Connection timed out */
#define	ECONNREFUSED	61		/* Connection refused */

	/* */
#define	ELOOP		62		/* Too many levels of symbolic links */
#define	ENAMETOOLONG	63		/* File name too long */

/* should be rearranged */
#define	EHOSTDOWN	64		/* Host is down */
#define	EHOSTUNREACH	65		/* No route to host */
#define	ENOTEMPTY	66		/* Directory not empty */

/* quotas & mush */
#define	EPROCLIM	67		/* Too many processes */
#define	EUSERS		68		/* Too many users */
#define	EDQUOT		69		/* Disc quota exceeded */

/* Network File System */
#define	ESTALE		70		/* Stale NFS file handle */
#define	EREMOTE		71		/* Too many levels of remote in path */

/* streams */
#define	ENOSTR		72		/* Device is not a stream */
#define	ETIME		73		/* Timer expired */
#define	ENOSR		74		/* Out of streams resources */
#define	ENOMSG		75		/* No message of desired type */
#define	EBADMSG		76		/* Trying to read unreadable message */

/* SystemV IPC */
#define EIDRM		77		/* Identifier removed */

/* SystemV Record Locking */
#define EDEADLK		78		/* Deadlock condition. */
#define ENOLCK		79		/* No record locks available. */

/* Non-standard UNIX, ie Amigados errors */
#define EBADVOL		80	/* Bad volume name			*/
#define ECONFIGPROBLEM	81	/* software configuration problem	*/

#else
#define ENETDOWN	35	/* network software is down		*/
#define ENETUNREACH	36	/* network is not reachable from here	*/
#define ENETRESET	37	/* network software was reset		*/
#define ECONNABORTED	38	/* connection was aborted		*/
#define ECONNRESET	39	/* connection was reset			*/
#define ENOBUFS		40	/* no network buffers available		*/
#define EISCONN		41	/* already connected			*/
#define ENOTCONN	42	/* not connected			*/
#define ESHUTDOWN	43	/* socket was shutdown			*/
#define ETIMEDOUT	44	/* operation timed out			*/
#define ECONNREFUSED	45	/* connect request was refused		*/
#define EHOSTDOWN	46	/* host is down				*/
#define EHOSTUNREACH	47	/* host is unreachable			*/
#define EWOULDBLOCK	48	/* operation would have blocked		*/
#define EPROTOTYPE	49
#define ENOTSOCK	50	/* socket required, supplied arg wasn't	*/
#define EADDRINUSE	51	/* address already in use		*/
#define EADDRNOTAVAIL	52	/* address not available on this node	*/
#define EMSGSIZE	53	/* MSG too big or too small		*/
#define EAFNOSUPPORT	54	/* address family not supported		*/
#define EPFNOSUPPORT	55	/* protocol family not supported	*/
#define ENOPROTOOPT	56	/* protocol doesn't support option	*/
#define EPROTONOSUPPORT	57
#define EALREADY	58
#define EDESTADDRREQ	59	/* destination address required		*/
#define EINPROGRESS	60	/* operation already in progress	*/
#define ESTALE		61	/* stale remote filesystem handle	*/
#define EBADVOL		62	/* Bad volume name			*/
#define ECONFIGPROBLEM	63	/* software configuration problem	*/
#define EOPNOTSUPP	64	/* operation is not supported		*/

#define	ENONSTD		100	/* Add non standard errors after ENONSTD*/

#endif

extern char *sys_errlist[];
extern int sys_nerr, errno;

#endif /* SYS_ERRNO_H */

