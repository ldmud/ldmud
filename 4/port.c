#include "config.h"

#include "machine.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#if defined(AMIGA) && !defined(__SASC)
#include <sys/resource.h>
#endif
#ifndef RUSAGE_SELF
#define RUSAGE_SELF 0
#endif

#include "lint.h"

#ifdef sun
time_t time PROT((time_t *));
#endif


/*
 * This file defines things that may have to be changed when porting
 * LPmud to new environments. Hopefully, there are #ifdef's that will take
 * care of everything.
 */

#if defined(RAND) || defined(DRAND48) || defined(RANDOM)

/*
 * Return a random number in the range 0 .. n-1
 */
mp_uint random_number(n)
    mp_uint n;
{
#ifdef RANDOM
    return random() % n;
#else /* RANDOM */
#ifdef DRAND48
    return (int)(drand48() * n);
#else /* DRAND48 */
#ifdef RAND
    return rand() % n;
#endif /* RAND */
#endif /* DRAND48 */
#endif /* RANDOM */
}

#endif

/*
 * The function time() can't really be trusted to return an integer.
 * But this game uses the 'current_time', which is an integer number
 * of seconds. To make this more portable, the following functions
 * should be defined in such a way as to return the number of seconds since
 * some chosen year. The old behaviour of time(), is to return the number
 * of seconds since 1970.
 */
/* On a SUN Sparc I, a negative time offset of 22 seconds between two
 * sucessive calls to time() has been observed. Negative time offsets
 * can mess up the call_out tables. Since they also could mess up the
 * mudlib, completely hide them by forcing the visible time to continue
 * to run in positive direction.
 */

mp_int get_current_time() {
    /* Don't write ever to total_alarms outside the interrupt, to avoid
     * race conditions.
     */
    extern volatile mp_int total_alarms;

    static mp_int last_time = 0;
    static mp_int noted_alarms = 0;

    mp_int offset;
    mp_int now;

    offset = total_alarms - noted_alarms >> 1;
    /* allow one alarm to happen without force-incrementing the time, so
     * that no time anomaly due to race conditions occurs.
     */
    noted_alarms += offset;
    last_time += offset;
    now = (mp_int)time((time_t *)NULL);	/* Just use the old time() for now */
    if (now >= last_time) {
	last_time = now;
	return now;
    }
    debug_message("Time anomaly, %ld seconds.\n", (long)(last_time - now));
    return last_time;
}

char *time_string(t)
    int t;
{
    return ctime((time_t *)&t);
}



/*
 * The functions below fix up some deficiencies of various platforms
 */


#ifdef STRTOL_BROKEN
#define DIGIT(x)	(isdigit(x) ? (x) - '0' : \
			islower(x) ? (x) + 10 - 'a' : (x) + 10 - 'A')
#define MBASE	('z' - 'a' + 1 + 10)

long
strtol(str, ptr, base)
#ifdef STRTOL_CONST_CHARP
const
#endif
register char *str;
char **ptr;
register int base;
{
    register long val;
    register int c;
    int xx, neg = 0;

    if (ptr != (char **)0)
	*ptr = (char*)str; /* in case no number is formed */
    if (base < 0 || base > MBASE)
	return (0); /* base is invalid -- should be a fatal error */
    if (!isalnum(c = *str)) {
	while (isspace(c))
		c = *++str;
	switch (c) {
	  case '-':
	    neg++;
	  case '+': /* fall-through */
	    c = *++str;
	}
    }
    if (base == 0)
	if (c != '0')
	    base = 10;
	else if (str[1] == 'x' || str[1] == 'X')
	    base = 16;
	else
	    base = 8;
    /*
     * for any base > 10, the digits incrementally following
     *	9 are assumed to be "abc...z" or "ABC...Z"
     */
    if (!isalnum(c) || (xx = DIGIT(c)) >= base)
	return (0); /* no number formed */
    if (base == 10) {
	/* accumulate neg avoids surprises near MAXLONG */
	for (val = '0'-c; isdigit(c = *++str); )
	    /* multiplication with a constant can be optimized */
	    val = 10 * val +'0'-c;
    } else if (base == 16) {
	/* str[1] might be '0', thus we must not access str[2] without check. */
	if (c == '0' && (str[1] == 'x' || str[1] == 'X') && isxdigit(str[2]))
	    c = *(str += 2); /* skip over leading "0x" or "0X" */
	for (val = -DIGIT(c); isalnum(c = *++str) && (xx = DIGIT(c)) < 16; )
	    val = (val << 4) - xx;
    } else {
	for (val = -DIGIT(c); isalnum(c = *++str) && (xx = DIGIT(c)) < base; )
	    val = base * val - xx;
    }
    if (ptr != (char **)0)
	*ptr = (char *)str;
    return (neg ? val : -val);
}
#endif /* STRTOL_BROKEN */

#ifndef HAVE_STRCSPN
strcspn(s, set)
    char *start;
    char *set;
{
    register char *t, *s, c, d;
	
    s = start;
    while (c = *s)
    {
	t = set;
	while (d = *t++) {
	    if (c == d)
		return s - start;
	}
	s++;
    }
}
#endif /* !HAVE_STRCSPN */

    
#ifndef HAVE_MEMSET
char *memset (s, c, n)
    char *s;
    int c, n;
{
#ifdef HAVE_BZERO
    if(c == 0)
	bzero(s, n);
#endif
    else {
	while(--n >= 0)
	    *s++ = c;
    }
}
#endif /* !HAVE_MEMSET */

#if 0 /* it's 'implemented' with a define, but without usable return value */
char *memcpy(b,a,s)
    char *b, *a;
    int s;
{
    bcopy(a,b,s);
    return b;
}
#endif

#if !defined(HAVE_CRYPT) && !defined(HAVE__CRYPT)
#include "hosts/crypt.c"
#endif

#if 0 /* If you can't get crypt to compile,  you can use this dummy */
char * crypt(pass,salt)
    char *pass, *salt;
{
    return pass;
}
#endif

#ifdef atarist
#ifndef ATARI_TT /* exp is in <math-688.h>, which is included by lint.h */
double exp(a) /* can't compute anything but exp(-n/900) */
    double a;
{
    float r = 1.;
    int i;

    i = (a * -0.033);
    while (--i >= 0) {
        r *= 6.911322453e-14      ; /* exp(-1/0.033) (inaccurate) */
        a += 30.3030303030303030303;
    }
    i = (a * -900. + 0.1);
    while (--i >= 0 ) {
        r *= .9988895059            ; /* exp(-1/900) (inaccurate) */
    }
    return (double)r;
}
#endif /* ATARI_TT */
#endif /* atarist */


#if !defined(HAVE_GETRUSAGE) && !defined(MSDOS)
#include <sys/times.h>
int getrusage (who, rusage)
    int who;
    struct rusage *rusage;
{
    struct tms buffer;

    if (who != RUSAGE_SELF) {

	errno = EINVAL;
	return -1;
    }
    if (times(&buffer)==-1) {
	/* pass errno */
	return -1;
    }
    rusage->ru_utime = buffer.tms_utime;
    rusage->ru_stime = buffer.tms_stime;
    return 0;
}
#endif /* getrusage implemented using times() */

#if defined(AMIGA)
/*-----------------------------------------------------------------------
** void init_rusage (void)
** int getrusage (int who, struct rusage *rusage)
**
** Get information about resource utilization, well, at least some
** idea of the time spent in the gamedriver.
** Reason is that some wiz use the efun getrusage() to measure times
** in subseconds.
*/

#ifdef __SASC

struct rtime {
  long tv_sec;
  long tv_usec;
};

struct rusage {
  struct rtime ru_utime;
  struct rtime ru_stime;
};

#endif

static clock_t first_clock;

void init_rusage (void) {
  first_clock = clock();
}

int getrusage (int who, struct rusage *rusage) {
  if (who != RUSAGE_SELF) {
    errno = EINVAL;
    return -1;
  }
  memset (rusage, 0, sizeof (struct rusage));
  rusage->ru_utime.tv_sec = (clock() - first_clock) / CLK_TCK;
  rusage->ru_utime.tv_usec =
    ((clock() - first_clock) % CLK_TCK) * (1000000 / CLK_TCK);
  return 0;
}

#endif /* AMIGA */

/*-----------------------------------------------------------------------*/


#if defined(MSDOS) && !defined(HAVE_GETRUSAGE)

static long rusage_timer = 0L;
static long rusage_milliseconds = 0;

void init_rusage () {
    (void)milliseconds(&rusage_timer);
}

int getrusage (who, rusage)
    int who;
    struct rusage *rusage;
{
    unsigned long offset;

    if (who != RUSAGE_SELF) {
	errno = EINVAL;
	return -1;
    }
    offset = milliseconds(&rusage_timer);
    rusage_milliseconds += offset;
    memset (rusage, 0, sizeof (struct rusage));
    rusage->ru_utime = rusage_milliseconds;
    return 0;
}

#endif /* MSDOS */


#if 0 /* 'implemented' with a define if missing */
void bzero(str, i)
    char *str;
    int i;
{
    memset(str, 0, i);
}
#endif

#ifndef HAVE_MEMMEM
/* This implementation is not very efficient, but still better than the old
 * match_string() .
 */
char *memmem(needle, needlelen, haystack, haystacklen)
    char *needle, *haystack;
    size_t needlelen, haystacklen;
{
    mp_int i;

    i = haystacklen - needlelen;
    if (i >= 0) do {
	if ( !strncmp(needle, haystack, needlelen) )
	    return haystack;
	haystack++;
    } while (--i >= 0);
    return 0;
}
#endif /* !HAVE_MEMMEM */

#if !defined(HAVE_MEMMOVE) && !defined(OVERLAPPING_BCOPY)
void move_memory(dest, src, n)
    char *dest, *src;
    size_t n; /* might be unsigned */
{
    if (!n)
	return;
    if (dest > src) {
	dest += n;
	src  += n;
	do
	    *--dest = *--src;
	while (--n);
    } else {
	do
	    *dest++ = *src++;
	while (--n);
    }
}
#endif /* !HAVE_MEMMOVE */
