#ifndef __HOSTS_UNIX_H__
#define __HOSTS_UNIX_H__

/*------------------------------------------------------------------
 * Definitions and includes for Unixish hosts.
 * 
 * This file is included by port.h via the HOST_INCLUDE define.
 *------------------------------------------------------------------
 */

#if defined(SunOS4) || defined (ultrix)
extern void bzero(char *, int);
#endif

#if defined(SunOS4)
/* These prototypes used to have a wider scope, but I suspect they are
 * only needed on suns.
 */
extern char *_crypt(char *, char *);
extern int ioctl(int, ...); /* should be in <ioctl.h> */
#endif

#if defined(SunOS4) || defined(ultrix) || defined(__CYGWIN32__)
extern int gethostname(char *, int);
extern char *getdomainname(char *, int);
#endif

#ifdef SunOS4
extern int rename(const char *, const char *);
extern void perror(const char *);
extern long int strtol(const char *, char **, int);
#endif

#if defined(__CYGWIN32__) || defined(sun)
extern time_t time(time_t *tloc);
#endif

#if 0 && defined(__CYGWIN32__)
extern char * ctime(time_t *tloc);
#endif

#endif /* __HOSTS_UNIX_H__ */
