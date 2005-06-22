/*---------------------------------------------------------------------------
 * Portability code.
 *
 *---------------------------------------------------------------------------
 * Implementation of missing system functions, as well as wrappers to
 * system functions with known bugs.
 *
 * The functions in here should be host-independent, though at the moment
 * some aren't.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <ctype.h>
#include "my-rusage.h"
#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>

#include "backend.h"
#include "main.h"

/*-------------------------------------------------------------------------*/

char current_time_stamp[21] = { 0 };
  /* Static buffer for the last timestamp computed by time_stamp().
   * This is for use only by those functions which must avoid memory
   * operations - all other functions should use time_stamp() itself.
   */

/*-------------------------------------------------------------------------*/
mp_int
get_current_time (void)

/* The function time() can't really be trusted to return an integer.
 * But this game uses the 'current_time', which is an integer number
 * of seconds. To make this more portable, the following functions
 * should be defined in such a way as to return the number of seconds since
 * some chosen year. The old behaviour of time() is to return the number
 * of seconds since 1970.
 *
 * On a SUN Sparc I, a negative time offset of 22 seconds between two
 * sucessive calls to time() has been observed. Similar discrepancies can
 * occur whenever a system clock is set, e.g. by automatic synchronisation
 * via ntp. These negative time offsets can mess up the call_out tables. Since
 * they also could mess up the mudlib, completely hide them by forcing the
 * visible time to continue to run in positive direction.
 */

{
    /* Don't write ever to total_alarms outside the interrupt, to avoid
     * race conditions.
     */
    static mp_int last_time = 0;
    static mp_int noted_alarms = 0;

    mp_int offset;
    mp_int now;
    mp_int total_alarms_now = total_alarms;

    offset = (total_alarms_now - noted_alarms) >> 1;
    noted_alarms = total_alarms_now;
    /* The division by two forces last_time to run at about 1/4 real time
     * so that the computer clock can catch up eventually. Furthermore it
     * allows to miss one single alarm to race conditions (total_alarms is
     * incremented in time, but after we read it) without causing a time
     * anomaly.
     */
    last_time += offset;
    now = (mp_int)time(NULL);        /* Just use the old time() for now */
    if (now >= last_time) {
        last_time = now;
        return now;
    }
    debug_message("Time anomaly, %ld seconds.\n", (long)(last_time - now));
    return last_time;
} /* get_current_time() */

/*-------------------------------------------------------------------------*/
char *
time_string (mp_int t)

/* Return a textual representation of the time <t>. */

{
    static char result[80];
    struct tm *tm;
    mp_int last_time = -1;

    if (t != last_time)
    {
        last_time = t;
        tm = localtime((time_t *)&t);
        strftime(result, sizeof(result)-1, "%a %b %d %H:%M:%S %Y", tm);
    }
    return result;
} /* time_string() */

/*-------------------------------------------------------------------------*/
char *
utime_string (mp_int t, mp_int ut)

/* Return a textual representation of the time <t> secs:<ut> microseconds. */

{
    static char result[80];
    struct tm *tm;
    size_t len;
    mp_int last_t = -1, last_ut = -1;

    if (t != last_t || ut != last_ut)
    {
        last_t= t;
        last_ut= ut;
        tm = localtime((time_t *)&t);
        len = strftime(result, sizeof(result)-1, "%a %b %d %H:%M:%S:", tm);
        sprintf(result+len, "%06ld", ut);
        strftime(result+len+6, sizeof(result)-7-len, " %Y", tm);
    }
    return result;
} /* utime_string() */

/*-------------------------------------------------------------------------*/
char *
time_stamp (void)

/* Return a textual representation of the current time
 * in the form "YYYY.MM.DD HH:MM:SS".
 * Result is a pointer to a static buffer.
 *
 * Putting this function in strfuns is not a good idea, because
 * it is need by almost every module anyway.
 */

{
    mp_int t;
    struct tm *tm;
    static mp_int last_time = -1;

    t = get_current_time();
    if (t != last_time)
    {
        last_time = t;
        tm = localtime((time_t *)&t);
        strftime( current_time_stamp, sizeof(current_time_stamp)-1
                , "%Y.%m.%d %H:%M:%S", tm);
    }
    return current_time_stamp;
} /* time_stamp() */

/*-------------------------------------------------------------------------*/
char *
xmemmem ( const char *haystack, size_t haystacklen
        , const char *needle, size_t needlelen
        )

/* Find the first occurance of <needle> (of length <needlelen>) in
 * <haystack> (of <haystacklen> length) and return a pointer to it.
 * A needle of length 0 is always found at <haystack>.
 * If not found, return NULL.
 *
#ifndef HAVE_MEMMEM
 * This function is a GNU/Linux extension, but up to and including
 * glibc 2 it wasn't implemented correctly. Since it is also used
 * only in the get_dir() implementation, we don't even bother to
 * use the glibc implementation.
#endif
 */

{
    mp_int i;

    i = (mp_int)(haystacklen - needlelen);
    if (i >= 0) do {
        if ( !memcmp(needle, haystack, needlelen) )
            return (char *)haystack;
        haystack++;
    } while (--i >= 0);
    return 0;
} /* xmemmem() */

/*-------------------------------------------------------------------------*/
/* Some UNIX functions which are not supported on all platforms. */

#if defined(__EMX__) || defined(OS2)
int socketpair (int a, int b, int c, int *d)
{
    errno = EPERM;
    return -1;
}
#endif

/*-------------------------------------------------------------------------*/
#ifdef STRTOL_BROKEN

#define DIGIT(x)        (isdigit(x) ? (x) - '0' : \
                        islower(x) ? (x) + 10 - 'a' : (x) + 10 - 'A')
#define MBASE        ('z' - 'a' + 1 + 10)

#ifdef STRTOL_CONST_CHARP
long
strtol(register const char *str, char **ptr, register int base)
#else
long
strtol(register char *str, char **ptr, register int base)
#endif
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
     *        9 are assumed to be "abc...z" or "ABC...Z"
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

/*-------------------------------------------------------------------------*/
#ifndef HAVE_STRCSPN

size_t
strcspn(char *s, char *set)
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

/*-------------------------------------------------------------------------*/
#ifndef HAVE_STRDUP

char *
strdup (const char *str)

/* Copy <str> into a freshly allocated memory block and return that one.
 */

{
    char *copy = malloc(strlen(str)+1);
    if (!copy)
        fatal("strdup failed\n");
    strcpy(copy, str);
    return copy;
}

#endif /* !HAVE_STRDUP */

/*-------------------------------------------------------------------------*/
#ifndef HAVE_MEMSET

char *
memset (char *s, int c, size_t n)
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

/*-------------------------------------------------------------------------*/
#if !defined(HAVE_MEMMOVE) && !defined(OVERLAPPING_BCOPY)

void
move_memory (char *dest, char *src, size_t n)
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

/*-------------------------------------------------------------------------*/
#if (!defined(HAVE_CRYPT) && !defined(HAVE__CRYPT)) || !defined(USE_SYSTEM_CRYPT)
#include "hosts/crypt.c"
#endif

#if 0 /* If you can't get crypt to compile,  you can use this dummy */
char * crypt(char *pass, char *salt)
{
    return pass;
}
#endif

/*-------------------------------------------------------------------------*/
#ifdef atarist

#ifndef ATARI_TT /* exp is in <math-688.h>, which is included by lint.h */
double
exp (double a) /* can't compute anything but exp(-n/900) */
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

/*-------------------------------------------------------------------------*/
#if !defined(HAVE_GETRUSAGE)

#include <sys/times.h>
int
getrusage (int who, struct rusage *rusage)
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

#if (defined(AMIGA) && !defined(__GNUC__)) || defined(CYGWIN) || defined(__EMX__) || defined(OS2)
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

void
init_rusage (void) {
  first_clock = clock();
}

int
getrusage (int who, struct rusage *rusage) {
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

#endif /* AMIGA || CYGWIN */

/***************************************************************************/

