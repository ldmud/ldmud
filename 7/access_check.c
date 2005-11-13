/* Copyright 1995 Joern Rennecke */

#include "config.h"

#if defined(ACCESS_RESTRICTED) && defined(ACCESS_CONTROL)

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

#include "lint.h"
#include "comm.h"

#define MAX_MESSAGE_LENGTH 256

static struct access_address {
    int32 addr, mask;
    int32 hour_mask;
    int32 wday_mask;
    struct access_class *class;
    struct access_address *next;
} *all_access_addresses = 0;

static struct access_class {
    long id;
    mp_int max_usage, usage;
    struct access_class *next;
    char message[8];
} *all_access_classes = 0;

static time_t last_read_time = 0;

extern void refresh_access_data PROT((void (*)(struct sockaddr_in *, long *)));

static struct access_class *find_access_class(full_addr)
    struct sockaddr_in *full_addr;
{
    int32 addr;
    struct access_address *aap;
    time_t seconds;
    struct tm *tm_p;

    addr = full_addr->sin_addr.s_addr;
    tm_p = 0;
    for (aap = all_access_addresses; aap; aap = aap->next) {
#if DEBUG_ACCESS_CHECK
    fprintf(stderr, "'%s', %ld %ld\n",
	inet_ntoa(*(struct in_addr*)&aap->addr),
	(long)aap->class->max_usage, (long)aap->class->usage);
#endif
	if ((aap->addr ^ addr) & aap->mask)
	    continue;
	if (aap->wday_mask >= 0) {
	    if (!tm_p) {
		time(&seconds);
		tm_p = localtime(&seconds);
#if DEBUG_ACCESS_CHECK
		fprintf(stderr, "h:%d w:%d\n", tm_p->tm_hour, tm_p->tm_wday);
#endif
	    }
	    if ( !((1 << tm_p->tm_hour) & aap->hour_mask) )
		continue;
	    if ( !((1 << tm_p->tm_wday) & aap->wday_mask) )
		continue;
	}
	return aap->class;
    }
    return 0;
}

static void add_access_entry(full_addr, idp)
    struct sockaddr_in *full_addr;
    long *idp;
{
    struct access_class *acp;

    acp = find_access_class(full_addr);
    if (acp) {
	acp->usage++;
	*idp = acp->id;
    }
}

static void read_access_file() {
    FILE *infp;
    struct access_address *aap, *next_aap, **last;
    struct access_class *acp, *next_acp;
    char message[MAX_MESSAGE_LENGTH];
    int i;
    int32 addr, mask;

    for (aap = all_access_addresses; aap; aap = next_aap) {
	next_aap = aap->next;
	free((char *)aap);
    }
    for (acp = all_access_classes; acp; acp = next_acp) {
	next_acp = acp->next;
	free((char *)acp);
    }
    all_access_classes = 0;
    infp = fopen(ACCESS_FILE, "r");
    last = &all_access_addresses;
    if (infp) for(addr = mask = 0;;) {
	long max_usage, class_id;
	int first_hour, last_hour;

	addr <<= 8;
	mask <<= 8;
	if (fscanf(infp, "%9[^.:\n]%[.:]", message, message+12) != 2 ||
	    *message == '#')
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
	    if ((unsigned)j > 0xff)
		break;
	    addr += j;
	    mask += 0xff;
	}
	if (message[12] == '.')
	    continue;
	max_usage = 0;
	message[0] = '\0';
	i = fscanf(infp, "%ld:%ld:%d:%d:",
	  &class_id, &max_usage, &first_hour, &last_hour);
	if (!i)
	    break;
	aap = malloc(sizeof *aap);
	if (!aap)
	    break;
	*last = aap;
	aap->addr = htonl(addr);
	aap->mask = htonl(mask);
	aap->wday_mask = -1;
	if (i == 4) {
	    if (first_hour || last_hour) {
		aap->wday_mask = 0x7f;
		if (first_hour <= last_hour) {
		    aap->hour_mask = (2 << last_hour) - (1 << first_hour);
		} else {
		    aap->hour_mask = -(1 << first_hour) + (2 << last_hour) - 1;
		}
	    }
	} else if (i == 2) {
	    char c, c2[2];

	    for (;;) {
		c = 'm';
		fscanf(infp, "%c %1[=]", &c, c2);
		switch(c) {
		  case 'w':
		  {
		    int32 *maskp;

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
		  }
		  default:
		    ungetc(c, infp);
		  case 'm':
		    break;
		}
		break;
	    }
	}
	fgets(message, sizeof message, infp);
	for (acp = all_access_classes; acp; acp = acp->next) {
	    if (acp->id == class_id)
		break;
	}
	i = strlen(message);
	if (message[i-1] == '\n')
	    message[--i] = '\0';
	if (!acp) {
	    acp =
		malloc(sizeof *acp - sizeof acp->message + 1 + i);
	    if (!acp) {
		free((char *)aap);
		break;
	    }
	    acp->id = class_id;
	    acp->max_usage = max_usage == -1 ? MAXINT : max_usage;
	    acp->usage = 0;
	    strcpy(acp->message, message);
	    acp->next = all_access_classes;
	    all_access_classes = acp;
	}
	aap->class = acp;
	last = &aap->next;
	addr = mask = 0;
    }
file_end:
    *last = 0;
    refresh_access_data(add_access_entry);
}


char *allow_host_access(full_addr, idp)
    struct sockaddr_in *full_addr;
    long *idp;
{
    struct stat statbuf;
    struct access_class *acp;

    if (!ixstat(ACCESS_FILE, &statbuf) && statbuf.st_mtime > last_read_time) {
	last_read_time = statbuf.st_mtime;
	read_access_file();
    }
    acp = find_access_class(full_addr);
    if (acp) {
	if (acp->usage >= acp->max_usage)
	    return acp->message;
	acp->usage++;
	*idp = acp->id;
	return 0;
    }
    return "No matching entry";
}

void release_host_access(num)
    long num;
{
    struct access_class *acp;

#if DEBUG_ACCESS_CHECK
    fprintf(stderr, "release_host_access %ld called.\n", num);
#endif
    for (acp = all_access_classes; acp; acp = acp->next) {
	if (acp->id != num)
	    continue;
	acp->usage--;
	break;
    }
}

#endif /* ACCESS_RESTRICTED */
