/*------------------------------------------------------------------
 * Regular Expression Cache
 * Written 1998 by Lars Duening.
 * Share and Enjoy!
 *------------------------------------------------------------------
 * Implementation of a cache for compiled regular expressions.
 * Usage of the cache can reduce the _setup_ time for regexps
 * by factor 4; the actual regexp matching itself is as fast (or
 * slow) as before.
 *
 * Compiled expressions are stored together with their generator
 * strings in a hash table, hashed over the generator string content.
 * The table is of fixed size so that unused regexps don't hold too
 * much memory too long.
 *------------------------------------------------------------------
 */

/*--------------------------------------------------------------------*/

#include "config.h"
#include "lint.h"
#include "regexp.h"
#include "rxcache.h"
#include "smalloc.h"

#ifdef RXCACHE_TABLE

/*--------------------------------------------------------------------*/

/* Hash function, inspired by stralloc.c */

#if !( (RXCACHE_TABLE) & (RXCACHE_TABLE)-1 )
#define StrHash(s) (whashstr((s), 20) & ((RXCACHE_TABLE)-1))
#else
#define StrHash(s) (whashstr((s), 20) % RXCACHE_TABLE)
#endif

/* One Regexp cache entry */

typedef struct CacheEntry {
  char   * pString;  /* Generator string, a shared string
                      * NULL if unused */
  regexp * pRegexp;  /* The generated regular expression */
} CacheEntry;

/* One Hashtable Entry */

typedef struct HashEntry {
  short      iNext;                 /* Next slot to use */
  CacheEntry slots[RXCACHE_CHAIN];  /* The chain of cache entries */
} HashEntry;

/* Variables */
static HashEntry table[RXCACHE_TABLE];  /* The Hashtable */

static uint32 iNumRequests   = 0;  /* Number of calls to regcomp() */
static uint32 iNumFound      = 0;  /* Number of calls satisfied from table */
static uint32 iNumCollisions = 0;  /* Number of hashcollisions */

/*--------------------------------------------------------------------*/
void rxcache_init(void)

/* Initialise the module. */

{
  memset(table, 0, sizeof(table));
}

/*--------------------------------------------------------------------*/
regexp *
regcomp_cache(char * expr, int excompat)

/* Compile a regexp structure from the expression <expr>, more or
 * less ex compatible.
 *
 * If possible, take a ready-compiled structure from the hashtable,
 * else enter the newly compiled structure into the table.
 *
 * The caller must not free the structure returned!
 */

{
  int hash, i;
  regexp * pRegexp;

  iNumRequests++;

  expr = make_shared_string(expr); // adds one string ref
  hash = StrHash(expr);

  /* Search for a ready-compiled regexp */
  for (i = 0; i < RXCACHE_CHAIN; i++)
    if (table[hash].slots[i].pString == expr)
    {
      iNumFound++;
      free_string(expr);
      return table[hash].slots[i].pRegexp;
    }

  /* Regexp not found: compile a new one and enter it
   * into the table.
   */
  pRegexp = regcomp(expr, excompat);
  if (NULL == pRegexp)
  {
    free_string(expr);
    return NULL;
  }

  i = table[hash].iNext++;
  if (NULL != table[hash].slots[i].pString)
  {
    iNumCollisions++;
    free_string(table[hash].slots[i].pString);
    xfree(table[hash].slots[i].pRegexp);
  }
  table[hash].slots[i].pString = expr; // ref is transferred
  table[hash].slots[i].pRegexp = pRegexp;

  return pRegexp;
}

/*--------------------------------------------------------------------*/
int
rxcache_status (int verbose)

/* Gather (and optionally print) the statistics from the rxcache.
 * Return the amount of memory used.
 */

{
  char buf[250];
  uint   iNumHashEntries = 0;  /* Number of used hash table entries */
  uint32 iNumEntries = 0;      /* Number of used cache entries */
  uint32 iSizeAlloc = 0;       /* Memory held in regexp structures */
  int    i, j;
  uint32 iNumReq;              /* Number of Requests, made non-zero */

  /* Scan the whole table, counting entries */
  for (i = 0; i < RXCACHE_TABLE; i++)
  {
    short bEntryUsed = 0;

    for (j = 0; j < RXCACHE_CHAIN; j++)
      if (NULL != table[i].slots[j].pString)
      {
        bEntryUsed = 1;
        iNumEntries++;
        iSizeAlloc += table[i].slots[j].pRegexp->regalloc;
      }
    if (bEntryUsed)
      iNumHashEntries++;
  }

  /* In verbose mode, print the statistics */
  if (verbose) {
    add_message("\nRegexp cache status:\n");
    add_message(  "--------------------\n");
    sprintf(buf, "Entries in cache:      %lu (%.1f%%)\n"
               , iNumEntries, 100.0 * (float)iNumEntries / (RXCACHE_CHAIN * RXCACHE_TABLE));
    add_message("%s", buf);
    sprintf(buf, "Average chain length:  %.2f\n", (float)iNumEntries / iNumHashEntries);
    add_message("%s", buf);
    sprintf(buf, "Memory allocated:      %lu\n", iSizeAlloc);
    add_message("%s", buf);
    iNumReq = iNumRequests ? iNumRequests : 1;
    sprintf(buf, "Requests: %lu - Succeeded: %lu (%.1f%%) - Collisions: %lu (%.1f%%)\n"
               , iNumRequests, iNumFound, 100.0 * (float)iNumFound/(float)iNumReq
               , iNumCollisions, 100.0 * (float)iNumCollisions/(float)iNumReq
           );
    add_message("%s", buf);
  }
  else
  {
    sprintf(buf, "Regexp cache:\t\t\t\t%9lu\n", iSizeAlloc);
    add_message("%s", buf);
  }

  return iSizeAlloc;
}

/*--------------------------------------------------------------------*/

#if defined(MALLOC_smalloc)

/* Garbage collection support.
 *
 * The defines are all copied from gcollect.c
 */

#define	STRING_REFS(str)	(*(unsigned short *)((char *) (str)\
						   - sizeof(unsigned short)))
#define MARK_STRING_REF(str) ((void)(\
    STRING_REFS(str)++ || (\
	CHECK_REF( (str)-sizeof(short)-sizeof(char *) ) ||\
	    /* reached max ref count, which is given as 0... */ \
	    STRING_REFS(str)--\
    ))\
)

#define CLEAR_REF(p) ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] &= ~M_REF )

#define MARK_REF(p) ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] |= M_REF )

#define TEST_REF(p) ( !( ((p_uint *)(p))[-SMALLOC_OVERHEAD] & M_REF ) )

#define CHECK_REF(p) ( TEST_REF(p) && ( MARK_REF(p),MY_TRUE ) )

#define NOTE_REF(p) \
    ( \
	TEST_REF(p) ? \
	    MARK_REF(p) \
	: time_to_swap_variables + 1 == 0 && \
	  ( WRITE_SMALLOC_TRACE(p) \
	    ifatal("memory block referenced twice\n") ) \
    )

#ifdef SMALLOC_TRACE
extern void writex PROT((int, int));
extern void writed PROT((int, int));
extern int is_freed PROT((char *, p_uint));
#define WRITES(d, s) write((d), (s), strlen(s))
#define WRITE_SMALLOC_TRACE(p)	(WRITES(gout, ((char **)(p))[-3]), \
	WRITES(gout, " "), \
	((p_uint (*)PROT((int, int)))writed)(gout, ((p_uint *)(p))[-2]), \
	WRITES(gout, "\n") ),
#else
#define WRITE_SMALLOC_TRACE(p)
#endif

extern long time_to_swap_variables;

#ifdef __GNUC__
/* typecast would strip NORETURN */
#define ifatal(s) (fatal(s),0)
#else
#define ifatal(s) (((p_uint (*)PROT((char *)))fatal)(s))
#endif

/*--------------------------------------------------------------------*/
void
count_rxcache_refs (void)

/* Mark all memory referenced from the hashtable. */

{
  int i, j;

  for (i = 0; i < RXCACHE_TABLE; i++)
  {
    for (j = 0; j < RXCACHE_CHAIN; j++)
      if (NULL != table[i].slots[j].pString)
      {
        MARK_STRING_REF(table[i].slots[j].pString);
        NOTE_REF(table[i].slots[j].pRegexp);
      }
  }

}

#endif /* if MALLOC_smalloc */

#endif /* if RXCACHE_TABLE */

/*====================================================================*/

