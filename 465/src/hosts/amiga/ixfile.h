/* hosts/amiga/ixfile.h
**
** Defines some macros for filefunctions to allow automatic filename
** conversion from unix style to amiga style.
*/

#ifndef IXFILE_H
#define IXFILE_H

/* We redefine some functions, but we need the prototypes... */
#include <sys/dir.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

extern char *ixconvert (char *);
extern char *ixconvert2 (char *);
#ifdef __SASC
extern int stat(const char *, struct stat *);
extern int open(const char *, int, ...);

#undef access
#endif

#define access(n)        access(ixconvert(n))
#define chdir(n)         chdir(ixconvert(n))
#define fopen(n,m)       fopen(ixconvert(n),m)
#define freopen(n,m,f)   freopen(ixconvert(n),m,f)
#if defined(_DCC) && (defined(DICE206) || defined(DICE30) || defined(DICE32))
#define mkdir(n,i)       mkdir(ixconvert(n))
#else
#define mkdir(n,i)       mkdir(ixconvert(n),i)
#endif
#define ixopen(n,m)      open(ixconvert(n),m)
#define ixopen3(n,m,b)   open(ixconvert(n),m,b)
#define opendir(n)       opendir(ixconvert(n))
#define remove(n)        remove(ixconvert(n))
#define rename(n,m)      rename(ixconvert(n),ixconvert2(m))
#define rmdir(n)         rmdir(ixconvert(n))
#define ixstat(n,m)      stat(ixconvert(n),m)
#define lstat(n,m)       stat(ixconvert(n),m)
#define unlink(n)        unlink(ixconvert(n))

#endif
