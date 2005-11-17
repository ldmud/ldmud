#ifndef LPC_TIME_H_
#define LPC_TIME_H_ 1

/* Indices into the array returned from gmtime() and localtime(). */

#define TM_SEC    0  /* Seconds (0..59) */
#define TM_MIN    1  /* Minutes (0..59) */
#define TM_HOUR   2  /* Hours (0..23) */
#define TM_MDAY   3  /* Day of the month (1..31) */
#define TM_MON    4  /* Month of the year (0..11) */
#define TM_YEAR   5  /* Year (e.g.  2001) */
#define TM_WDAY   6  /* Day of the week (Sunday = 0) */
#define TM_YDAY   7  /* Day of the year (0..365) */
#define TM_ISDST  8  /* TRUE: Daylight saving time */

#define TM_MAX 9  /* Number of entries in the array */

#endif /* LPC_TIME_H_ */
