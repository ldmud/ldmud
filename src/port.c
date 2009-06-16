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
#include <locale.h>
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
time_fstring (mp_int t, const char* str, Bool localized)
/* Return a textual representation of the time <t> according to the format
 * string <str>. Doesn't cache because it would be necessary to 
 * save the format string and compare.
 * If localized is true, this function sets the locale according to the 
 * environment variable before calling strftime and resets it afterwards.
 * TODO: It would be nicer to allocate the result buffer dynamically
 * TODO::for using longer format strings. */
{
    static char result[512];
    struct tm *tm;

    time_t ti = (time_t)t;
    tm = localtime(&ti);
    if (!tm)
        return NULL;

    if (!localized) {
        setlocale(LC_TIME, "C");
        strftime(result, sizeof(result)-1, str, tm);
        setlocale(LC_TIME, "");
    }
    else
       strftime(result, sizeof(result)-1, str, tm);
    
    return result;
} /* time_fstring() */

/*-------------------------------------------------------------------------*/
char *
utime_string (mp_int t, mp_int ut)

/* Return a textual representation of the time <t> secs:<ut> microseconds. */

{
    static char result[80];
    struct tm *tm;
    size_t len;

    time_t ti = (time_t)t;
    tm = localtime(&ti);
    if (!tm)
        return NULL;

    len = strftime(result, sizeof(result)-1, "%a %b %d %H:%M:%S:", tm);
    sprintf(result+len, "%06"PRIdMPINT, ut);
    strftime(result+len+6, sizeof(result)-7-len, " %Y", tm);

    return result;
} /* utime_string() */

/*-------------------------------------------------------------------------*/
char *
time_stamp (void)

/* Return a textual representation of the current time
 * in the form "YYYY.MM.DD HH:MM:SS".
 * Result is a pointer curent_time_stamp[].
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
        time_t ti = (time_t)t;
        last_time = t;
        tm = localtime(&ti);
        strftime( current_time_stamp, sizeof(current_time_stamp),
                  "%Y.%m.%d %H:%M:%S", tm);
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

#if defined(CYGWIN)
/*-----------------------------------------------------------------------
** void init_rusage (void)
** int getrusage (int who, struct rusage *rusage)
**
** Get information about resource utilization, well, at least some
** idea of the time spent in the gamedriver.
** Reason is that some wiz use the efun getrusage() to measure times
** in subseconds.
*/

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

#endif /* CYGWIN */

/***************************************************************************/
