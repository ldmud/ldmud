/*---------------------------------------------------------------------------
 * File Statistics
 *
 * Copyright (C) 1998 by Lars Duening.
 *---------------------------------------------------------------------------
 * This module counts the usage of files by the mud, separated into normal 
 * reading and writing, compiling and inheriting, and save/restore objects.
 * Also counted are the total number of usages ('total'), the number of first
 * uses within the current heart beat interval ('tick'), and the number of
 * first uses in the current player command/heart beat execution thread
 * ('cmd'). Differences between the 'total' and the 'tick' resp. 'cmd' figure
 * denote multiple usages of a file in one interval/execution.
 *
 * The current statistics can be appended to the file /FILESTAT from within
 * the mud using the special command 'status files'.
 *
 * To activate the statistics code, defined FILE_STAT in the config.h or
 * the Makefile.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#if defined(FILE_STAT)

#include <stdio.h>

#define NO_INCREMENT_STRING_REF

#include "filestat.h"

#include "backend.h"
#include "comm.h"
#include "gcollect.h"
#include "hash.h"
#include "simulate.h"
#include "stralloc.h"

/*-------------------------------------------------------------------------*/

/* Structure to keep track of one file. */

typedef struct filestat {
  struct filestat * next;               /* Next structure in hashchain */
  char            * name;               /* The filename as shared string */
  int               tstamp;             /* current_time of last update */
  uint32            lstamp;             /* Backend loopcount of last update */
  uint32            stats[FSTAT_MAX];   /* The statistics */
} filestat;

/* A hashtable keeping all filestats */

#define TABLESIZE 16384

#if !( (TABLESIZE) & (TABLESIZE)-1 )
#define StrHash(s) (whashstr((s), 40) & ((TABLESIZE)-1))
#else
#define StrHash(s) (whashstr((s), 40) % TABLESIZE)
#endif


static filestat *table[TABLESIZE];

static uint32 iNumFiles = 0; /* Number of files accounted for */
static uint32 iNumUsage = 0; /* Number of files usages accounted for */

/*-------------------------------------------------------------------------*/
void
init_filestat(void)

/* Initialize the filestat table */

{
    memset(table, 0, sizeof(table));
}

/*-------------------------------------------------------------------------*/
void
fstat_count (char * filename, int type)

/* Count <filename> in the statistic <type> (being one of the FSTAT values).
 */

{
    char     * fname;
    filestat * fstat = NULL;
    int        hash;
    uint32     clstamp = total_alarms + total_player_commands;


    hash = StrHash(filename);

    /* Get the shared string for this filename if any. */
    fname = findstring(filename);

    /* If there is a shared string, check if we already know the file.
     */
    if (fname != NULL)
    {
        for (fstat = table[hash]; fstat != NULL; fstat = fstat->next)
            if (fstat->name == fname)
                break;
    }

    /* If we don't know this file, create a new entry.
     */
    if (NULL == fstat)
    {
        fname = make_shared_string(filename);
        fstat = permanent_xalloc(sizeof(*fstat));
        if (NULL == fstat)
        {
            error("Out of memory");
            /* NOTREACHED */
            return;
        }
        fstat->name = fname;
        memset(fstat->stats, 0, sizeof(fstat->stats));

        fstat->next = table[hash];
        table[hash] = fstat;

        iNumFiles++;
    }

    /* Finally, count the file usage */
    iNumUsage++;
    fstat->stats[type]++;
    fstat->stats[FSTAT_TOTAL]++;
    if (clstamp != fstat->lstamp)
    {
        fstat->lstamp = clstamp;
        fstat->stats[FSTAT_LUSES]++;
    }
    if (current_time != fstat->tstamp)
    {
        fstat->tstamp = current_time;
        fstat->stats[FSTAT_TUSES]++;
    }
}

/*-------------------------------------------------------------------------*/
void
fstat_status (void)

/* Print the detailed file statistics directly to the interactive user.
 */

{
    char       buf[1024];
    int        i;
    filestat * fstat;
    FILE     * f;

    f = fopen("FILESTAT", "a");
    if (NULL == f)
    {
        add_message("Can't write file /FILESTAT.\n");
        return;
    }
    add_message("File Usage statistic:\n");
    add_message("---------------------\n");
    sprintf(buf, "%lu files used %lu times in %lu commands/%ld backend loops.\n"
               , iNumFiles, iNumUsage, total_player_commands, total_alarms);
    add_message(buf);
    sprintf(buf, "Table uses %lu bytes of memory.\n", iNumFiles * sizeof(filestat));
    add_message(buf);
    add_message("\nAppending detailed statistics to /FILESTAT...\n");
    
    fputs("File Usage statistic:\n", f);
    fputs("---------------------\n", f);
    fprintf(f, "%lu files used %lu times in %lu commands/%ld backend loops.\n"
             , iNumFiles, iNumUsage, total_player_commands, total_alarms);
    fprintf(f, "Table uses %lu bytes of memory.\n", iNumFiles * sizeof(filestat));
    fprintf(f, "\n%30s : %8s (%8s, %8s) : %8s %8s %8s %8s %8s %8s %8s\n"
             , "  Name", "  Total", "  Tick", "  Cmd", "  Read", "  Write", "  Del", "  Comp"
             , "  Incl", "  Save", "  Rest");
    for (i = 0; i < 30 + 10*9 + 2 + 2; i++)
        buf[i] = '-';
    buf[i] = '\n';
    buf[i+1] = '\0';
    fputs(buf, f);
    for (i = 0; i < TABLESIZE; i++)
    {
        for (fstat = table[i]; fstat != NULL; fstat = fstat->next)
        {
            fprintf(f, "%30s: %8lu (%8lu, %8lu) : %8lu %8lu %8lu %8lu %8lu %8lu %8lu\n"
                     , fstat->name
                     , fstat->stats[FSTAT_TOTAL]
                     , fstat->stats[FSTAT_TUSES]
                     , fstat->stats[FSTAT_LUSES]
                     , fstat->stats[FSTAT_READ]
                     , fstat->stats[FSTAT_WRITE]
                     , fstat->stats[FSTAT_DEL]
                     , fstat->stats[FSTAT_COMP]
                     , fstat->stats[FSTAT_INCL]
                     , fstat->stats[FSTAT_SAVE]
                     , fstat->stats[FSTAT_REST]
                   );
        }
    }
    fputs("\n", f);
    fclose(f);
}

/*--------------------------------------------------------------------*/
void
count_fstat_refs (void)

/* Mark all memory referenced from the hashtables. */

{
    int       i;
    filestat *fstat;

    for (i = 0; i < TABLESIZE; i++)
        for (fstat = table[i]; fstat != NULL; fstat = fstat->next)
            count_ref_from_string(fstat->name);

} /* count_fstat_refs() */

#endif /* FILE_STAT */

/***************************************************************************/


