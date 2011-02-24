/*---------------------------------------------------------------------------
 * IP/time based access control.
 *
 * Copyright (C) 1995 by Joern Rennecke.
 *---------------------------------------------------------------------------
 * Access to the game is based on the user's IP address and the current time.
 * These are matched against a set of rules and if a match is detected, a
 * proper message is sent back and the connection is shutdown.
 *
 * This facility is active only if ACCESS_FILE has been defined in config.h
 * or given on the command line. If ACCESS_LOG is defined in config.h or
 * given on the command line, all checks and their results are logged by
 * comm.c in the specified file.
 *
 * The rules are read from the file <access_file> (typically "ACCESS.ALLOW")
 * which resides in the mudlib. Every line specifies one rule and has to
 * follow the syntax given below. Lines with a '#' as first character count
 * as comments and are ignored, as are lines which do not conform to the
 * rule syntax (but except for empty lines this should be relied upon).
 *
 * The syntax for a rule is (no leading whitespace allowed!):
 *
 *   <ipnum>:[p<port>]:<class>:<max>:<start>:<end>:<text>
 *   <ipnum>:[p<port>]:<class>:<max>:h<hours>:w<days>:m=<text>
 *
 * where
 *   ipnum:  <byte>.<byte>.<byte>.<byte>, with byte = * or number
 *             There is only loose error checking - specifying an illegal
 *             address will have interesting consequences, but would
 *             most likely cause no error to occur.
 *   port:   the port number to which the connection is made. Omission
 *             means 'any port'.
 *   class:  number
 *   max:    the maximum number of users, a number. The value -1 allows
 *             an unlimited number of users.
 *   start:  hour this rule starts to be valid (0..23).
 *   end:    hour this rule ceases to be valid (0..23).
 *             Setting both start and end to 0 skips any time check.
 *   hours:  hours this rule is valid.
 *             This form allows several entries, separated with a ','.
 *             Every entry can be a single hour (0..23), or a range in the
 *             form '<start>-<end>'
 *             Omitting the entry skips any time check.
 *   days:   the days this rule is valid.
 *             The syntax is similar to <hours> except for the
 *             allowable values: the days Sunday..Saturday are given as
 *             the numbers 0..6.
 *             Omitting the entry skips any day check.
 *   text:   string to send if the rule matches.
 *
 * A class is defined by the first rule using it's number. This
 * definition specifies the allowable <max>imum of users and the <text>
 * to send. Subsequent rules for the same class just add new ipnumber/
 * time rules, but don't change <max> or <text>
 *
 * ORDER MATTERS. That means if you allow 129.*.*.*, you have to put
 * any restrictions on 129.132.*.* BEFORE this rule.
 *
 * Addresses not matching any rule at all are not allowed. To get around
 * this, add an appropriate 'allow-all' rule *.*.*.* at the very end.
 *
 * An example rulefile:
 *
 *   # SPARC cluster has access denied. Class 1
 *   129.132.122.*:1:0:0:0:LPMUD access denied for your cluster.
 *
 *   # CALL-1A0 has access limited to some maximum, for now 5 logins. Class 2
 *   129.132.106.*:2:5:8:20:Sorry, LPMUD is currently full.
 *
 *   # CALL-1A0 at all other times, its a 10 limit.
 *   #   Due to the rule order, this is effectively limited to times
 *   #   outside 8-20.
 *   129.132.106.*:3:10:0:0:Sorry, LPMUD is currently full.
 *
 *   # No more than 5 users allowed from localhost while working hours :-)
 *   127.0.0.1:42:5:h8-12,13-18:w1-5:m=Pick a better time.
 *
 *   # Everybody else is welcome.
 *   *.*.*.*:0:-1:0:0:This message should never be printed.
 *
 * The rule file is (re)read whenever the gamedriver detects a change in its
 * timestamp.
 *
 * TODO:  This could also be made an
 * TODO:: efun "string|int access_control(file, [interactive])" to be used
 * TODO:: from TODO:: master.c::connect().
 * TODO:: Or a driver hook with the settings "file", ({ "file", "logfile" })
 * TODO:: and #'function(ip-address, port).
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <time.h>

#include "access_check.h"

#include "comm.h"
#include "filestat.h"
#include "xalloc.h"

#undef DEBUG_ACCESS_CHECK /* define to activate debug output */

/*-------------------------------------------------------------------------*/
#define MAX_MESSAGE_LENGTH 256

/* Command line arguments. */
char *access_file = NULL;
char *access_log = NULL;

/* IP address/time rules, kept in a linked list.
 * Every structure describes one rule, with a reference to the
 * corresponding class structure.
 */
static struct access_address {
    uint32 addr, mask; /* The IP address and a mask for the interesting parts */
    int32 port;        /* The port number (positive), or -1 for 'any port' */
    int32 hour_mask;   /* Bitmask: at which times is this rule valid? */
    int32 wday_mask;   /* Bitmask: at which days is this rule valid? */
    struct access_class *class;   /* The corresponding class */
    struct access_address *next;
} *all_access_addresses = NULL;

/* Class descriptions, kept in a linked list.
 * The structure is allocated big enough to keep the full message, the
 * 8 characters listed in the definition are just a placeholder.
 */
static struct access_class {
    long id;                  /* Class ID */
    mp_int max_usage, usage;  /* Max and current number of users */
    struct access_class *next;
    char message[8];          /* Placeholder for the message text buffer */
} *all_access_classes = NULL;

static time_t last_read_time = 0;

/*-------------------------------------------------------------------------*/
static struct access_class *
find_access_class (struct sockaddr_in *full_addr, int port)

/* Find and return the class structure for the given IP <full_addr> at
 * the current time. Return NULL if no rule covers the IP at this time.
 */

{
    uint32 addr;
    struct access_address *aap;
    time_t seconds;
    struct tm *tm_p;

#ifdef DEBUG_ACCESS_CHECK
    fprintf(stderr, "find_class for '%s':%d\n"
                  , inet_ntoa(*(struct in_addr*)&full_addr->sin_addr)
                  , port);
#endif
#ifndef USE_IPV6
    addr = full_addr->sin_addr.s_addr;
#else
    addr = (uint32) full_addr->sin_addr.s_addr;
    /* TODO: DANGER: The above cast might break under IPv6 */
#endif
    tm_p = NULL;
    for (aap = all_access_addresses; aap; aap = aap->next) {
#ifdef DEBUG_ACCESS_CHECK
        fprintf(stderr, "  '%s':%ld, %ld %ld\n",
                inet_ntoa(*(struct in_addr*)&aap->addr),
                aap->port,
                (long)aap->class->max_usage, (long)aap->class->usage);
#endif
        if (aap->port >= 0 && aap->port != port)
            continue;
        if ((aap->addr ^ addr) & aap->mask)
            continue;
        if (aap->wday_mask >= 0) {
            if (!tm_p) {
                time(&seconds);
                tm_p = localtime(&seconds);
#ifdef DEBUG_ACCESS_CHECK
                fprintf(stderr, "    h:%d w:%d\n", tm_p->tm_hour, tm_p->tm_wday);
#endif
            }
            if ( !((1 << tm_p->tm_hour) & aap->hour_mask) )
                continue;
            if ( !((1 << tm_p->tm_wday) & aap->wday_mask) )
                continue;
        }
#ifdef DEBUG_ACCESS_CHECK
        fprintf(stderr, "  found\n");
#endif
        return aap->class;
    }
#ifdef DEBUG_ACCESS_CHECK
        fprintf(stderr, "  not found\n");
#endif
    return NULL;
}

/*-------------------------------------------------------------------------*/
static void
add_access_entry (struct sockaddr_in *full_addr, int login_port, long *idp)

/* Find the class structure for <full_addr> and increments its count
 * of users. The id of the class is put into *idp.
 * If there is no class, *idp is not changed.
 *
 * This function is called after the access_file has been (re-)read
 * to reinitialize the current class usage counts.
 */

{
    struct access_class *acp;

    acp = find_access_class(full_addr, login_port);
    if (acp) {
        acp->usage++;
        *idp = acp->id;
    }
}

/*-------------------------------------------------------------------------*/
static void
read_access_file (void)

/* Read and parse the access_file, (re)creating all the datastructures.
 * After the file has been read, the usage counts will be updated by
 * calling comm::refresh_access_data, passing add_access_entry() as
 * callback.
 */

{
    FILE *infp;
    struct access_address *aap, *next_aap, **last;
    struct access_class *acp, *next_acp;
    char message[MAX_MESSAGE_LENGTH];
    int i;
    int32 addr, mask;

    /* Free the old datastructures */
    for (aap = all_access_addresses; aap; aap = next_aap) {
        next_aap = aap->next;
        pfree((char *)aap);
    }
    for (acp = all_access_classes; acp; acp = next_acp) {
        next_acp = acp->next;
        pfree((char *)acp);
    }
    all_access_classes = NULL;

    infp = fopen(access_file, "r");
    if (!infp)
        perror("driver: Can't read access file");
    FCOUNT_READ(access_file);
    last = &all_access_addresses;
      /* *last will be set to NULL at the end, so no dangling pointer
       * will exist.
       */

    /* The parse loop; it will terminate on file end or an error.
     * Every rule is parsed in four iterations: the first three read
     * the first bytes of the IP address, the fourth and last one
     * everything else.
     * If an address not specified in exactly 4 bytes, you will get
     * nonsense, but no error message.
     */
    if (infp) for(addr = mask = 0;;) {
        long max_usage, class_id, port;
        int first_hour, last_hour, m;

        /* Parse the next IP address byte, i.e. everything up to the
         * next . or : .
         */
        addr <<= 8;
        mask <<= 8;
        m = fscanf(infp, "%9[^.:\n]%[.:]", message, message+12);
        if (m != 2 || *message == '#')
        {
            do {
                i = fgetc(infp);
                if (i == EOF)
                    goto file_end;
            } while(i != '\n');
            addr = mask = 0;
            continue;
        }
        if (*message != '*') {
            int j;
            j = atoi(message);
            if ((unsigned int)j > 0xff)
                break;
            addr += j;
            mask += 0xff;
        }
        if (message[12] == '.') /* Then another byte follows */
            continue;

        /* The ip-number may be followed with a port specification */
        i = fscanf(infp, "p%ld:", &port);
        if (!i)
            port = -1;

        /* Parse the time specs next. Start by trying the first (old)
         * format.
         */
        max_usage = 0;
        message[0] = '\0';

        i = fscanf(infp, "%ld:%ld:%d:%d:",
          &class_id, &max_usage, &first_hour, &last_hour);
        if (!i)
            break;

        aap = pxalloc(sizeof *aap);
        if (!aap)
            break;
        *last = aap;
        aap->addr = htonl(addr);
        aap->mask = htonl(mask);
        aap->port = port;
        aap->wday_mask = -1; /* Default: valid on every day */

        if (i == 4) {            /* Old format */
            if (first_hour || last_hour) {
                aap->wday_mask = 0x7f;
                if (first_hour <= last_hour) {
                    aap->hour_mask = (2 << last_hour) - (1 << first_hour);
                } else {
                    aap->hour_mask = -(1 << first_hour) + (2 << last_hour) - 1;
                }
            }
        } else if (i == 2) {     /* New format */
            char c, c2[2];
            int32 *maskp;

            for (;;) {
                c = 'm';
                fscanf(infp, "%c %1[=]", &c, c2);
                switch(c)
                {
                case 'w':
                    maskp = &aap->wday_mask;
                    goto get_mask;

                case 'h':
                    maskp = &aap->hour_mask;

                get_mask:
                    mask = 0;
                    do {
                        int j, k;

                        *c2 = '\0';
                        if (!fscanf(infp, "%d %1[-,:] ", &j, c2))
                            break;
                        if (*c2 == '-') {
                            k = 24;
                            fscanf(infp, "%d %1[,:] ", &k, c2);
                            if (j <= k) {
                                mask |= (2 << k) - (1 << j);
                            } else {
                                mask |= -(1 << j) + (2 << k) - 1;
                            }
                        } else {
                            mask |= 1 << j;
                        }
                    } while (*c2 == ',');
                    *maskp = mask;
                    aap->wday_mask &= 0x7f; /* make sure it's not negative */
                    continue;

                default:
                    ungetc(c, infp);
                    /* FALLTHROUGH */
                case 'm':
                    break;
                } /* switch */

                break;
            } /* for */
        } /* if (i) */

        /* The rest of the line is the message to print.
         */
        fgets(message, (int)sizeof(message)-1, infp);
        message[sizeof(message) - 1] = '\0';

        /* Check if this rule creates a new class. If yes, allocate
         * a new structure and assign message text and usage to it.
         */
        for (acp = all_access_classes; acp; acp = acp->next) {
            if (acp->id == class_id)
                break;
        }
        if (!acp) {
            size_t len;
            len = strlen(message);
            if (len && message[len-1] == '\n')
                message[--len] = '\0';
            acp = pxalloc(sizeof *acp - sizeof acp->message + 1 + len);
            if (!acp) {
                pfree((char *)aap);
                break;
            }
            acp->id = class_id;
            acp->max_usage = max_usage == -1 ? MPINT_MAX : max_usage;
            acp->usage = 0;
            strcpy(acp->message, message);
            acp->next = all_access_classes;
            all_access_classes = acp;
        }

        /* Finishing touches. */
        aap->class = acp;
        last = &aap->next;
        addr = mask = 0;
    }
file_end: /* emergency exit from the loop */

    /* Terminate the address list properly, then refresh the data */
    *last = NULL;
    refresh_access_data(add_access_entry);
}


/*-------------------------------------------------------------------------*/
char *
allow_host_access (struct sockaddr_in *full_addr, int login_port, long *idp)

/* Check if the IP address <full_addr> is allowed access at the current
 * time. Return NULL if access is granted, else an error message.
 *
 * If access is allowed, *idp is set to the corresponding class id; this
 * information is used later to re-initialize the class structures after
 * a re-read of the access_file.
 *
 * The access_file is read by this function if it has been changed.
 */

{
    struct stat statbuf;
    struct access_class *acp;

    if (!access_file)
    {
        *idp = 0;
        return NULL;
    }
    
    if (ixstat(access_file, &statbuf))
        perror("driver: Can't stat access file");
    else if (statbuf.st_mtime > last_read_time) {
        last_read_time = statbuf.st_mtime;
        read_access_file();
    }

    acp = find_access_class(full_addr, login_port);
    if (acp) {
        if (acp->usage >= acp->max_usage)
            return acp->message;
        acp->usage++;
        *idp = acp->id;
        return NULL;
    }
    return "No matching entry";
}

/*-------------------------------------------------------------------------*/
void
release_host_access (long num)

/* One user from the class <num> logged out, update the class structure
 * accordingly.
 */

{
    struct access_class *acp;

#ifdef DEBUG_ACCESS_CHECK
    fprintf(stderr, "release_host_access %ld called.\n", num);
#endif
    for (acp = all_access_classes; acp; acp = acp->next) {
        if (acp->id != num)
            continue;
        acp->usage--;
        break;
    }
}

/*-------------------------------------------------------------------------*/
void
initialize_host_access ()

/* Check if the ACCESS_FILE is there, otherwise deactivate
   access control.
 */

{
    struct stat statbuf;

    if (access_file == NULL)
        return;

    if (ixstat(access_file, &statbuf))
    {
        fprintf(stderr, "%s Access file '%s' missing, "
                        "deactivating access control.\n", 
                        time_stamp(), access_file);
        access_file = NULL;
    }
}

/***************************************************************************/

