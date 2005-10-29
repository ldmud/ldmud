#include <stdio.h>
#include <string.h>

#include "config.h"
#include "lint.h"

/*
 * stralloc.c - string management.
 *
 * All strings are stored in an extensible hash table, with reference counts.
 * free_string decreases the reference count; if it gets to zero, the string
 * will be deallocated.  add_string increases the ref count if it finds a
 * matching string, or allocates it if it cant.  There is no way to allocate
 * a string of a particular size to fill later (hash wont work!), so you'll
 * have to copy things freom a static (or malloced and later freed) buffer -
 * that is, if you want to avoid space leaks...
 *
 * Current overhead:
 *	8 bytes per string (next pointer, and 2 shorts for length and refs)
 *	Strings are nearly all fairly short, so this is a significant overhead-
 *	there is also the 4 byte malloc overhead and the fact that malloc
 *	generally allocates blocks which are a power of 2 (should write my
 *	own best-fit malloc specialised to strings); then again, GNU malloc
 *	is bug free...
 */

/*
 * there is also generic hash table management code, but strings can be shared
 * (that was the point of this code), will be unique in the table,
 * require a reference count, and are malloced, copied and freed at
 * will by the string manager.  Besides, I wrote this code first :-).
 * Look at htable.c for the other code.  It uses the Hash() function
 * defined here, and requires hashed objects to have a pointer to the
 * next element in the chain (which you specify when you call the functions).
 */

#define	MAXSHORT	(1 << (sizeof(short)*8 - 2))
#define	WORD_ALIGN_BIT	0x3	/* these are 0 for aligned ptrs */

char * xalloc();
#ifndef _AIX
char * strcpy();
#endif

static int num_distinct_strings = 0;
int bytes_distinct_strings = 0;
static int allocd_strings = 0;
static int allocd_bytes = 0;
int overhead_bytes = 0;
static int search_len = 0;
static int num_str_searches = 0;

/*
 * strings are stored:
 *	(char * next) (short numrefs) string
 *				      ^
 *				pointer points here
 */

#define	NEXT(str)	(*(char **)((char *) (str) - sizeof(short)	\
						   - sizeof(int)))
#define	REFS(str)	(*(short *)((char *) (str) - sizeof(short)))

/*
 * hash table - list of pointers to heads of string chains.
 * Each string in chain has a pointer to the next string and a
 * reference count (char *, int) stored just before the start of the string.
 * HTABLE_SIZE is in config.h, and should be a prime, probably between
 * 1000 and 5000.
 */

static char ** base_table = 0;

static void init_strings()
{
	int x;
	base_table = (char **) xalloc(sizeof(char *) * HTABLE_SIZE);
	overhead_bytes += (sizeof(char *) * HTABLE_SIZE);

	for (x=0; x<HTABLE_SIZE; x++)
		base_table[x] = 0;
}

/*
 * generic hash function.  This is probably overkill; I haven't checked the
 * stats for different prime numbers, etc.
 */

static int StrHash(s)
char * s;
{
	if (!base_table)
		init_strings();

	return hashstr(s, 20, HTABLE_SIZE);
}

/*
 * Looks for a string in the table.  If it finds it, returns a pointer to
 * the start of the string part, and moves the entry for the string to
 * the head of the pointer chain.  One thing (blech!) - puts the previous
 * pointer on the hash chain into fs_prev.
 */

char * findstring(s)
char * s;
{
	char * curr, *prev;
	int h = StrHash(s);

	curr = base_table[h];
	prev = 0;
	num_str_searches++;

	while (curr) {
	    search_len++;
	    if (*curr == *s && !strcmp(curr, s)) { /* found it */
		if (prev) { /* not at head of list */
		    NEXT(prev) = NEXT(curr);
		    NEXT(curr) = base_table[h];
		    base_table[h] = curr;
		    }
		return(curr);	/* pointer to string */
		}
	    prev = curr;
	    curr = NEXT(curr);
	    }
	
	return(0); /* not found */
}

/*
 * Make a space for a string.  This is rather nasty, as we want to use
 * alloc/free, but malloc overhead is generally severe.  Later, we
 * will have to compact strings...
 */

static char * alloc_new_string(string)
char * string;
{
	char * s = xalloc(1 + strlen(string) + sizeof(char *) + sizeof(short));
	int h = StrHash(string);

	s += sizeof(char *) + sizeof(short);
	strcpy(s, string);
	REFS(s) = 0;
	NEXT(s) = base_table[h];
	base_table[h] = s;
	num_distinct_strings++;
	bytes_distinct_strings += 4 + (strlen(s) +3) & ~3;
	overhead_bytes += sizeof(char *) + sizeof(short);
	return(s);
}

char * make_shared_string(str)
char * str;
{
	char * s;

	s = findstring(str);
	if (!s)
		s = alloc_new_string(str);
	REFS(s)++;
	allocd_strings++;
	allocd_bytes += 4 + (strlen(str) + 3) & ~3;
	return(s);
}

/*
 * free_string - reduce the ref count on a string.  Various sanity
 * checks applied, the best of them being that a add_string allocated
 * string will point to a word boundary + sizeof(int)+sizeof(short),
 * since malloc always allocates on a word boundary.
 * On systems where a short is 1/2 a word, this gives an easy check
 * to see whather we did in fact allocate it.
 *
 * Don't worry about the overhead for all those checks!
 */

/*
 * function called on free_string detected errors; things return checked(s).
 */

static void checked(s, str) char * s, *str;
{
	fprintf(stderr, "%s (\"%s\")\n", s, str);
	fatal(s); /* brutal - debugging */
}

void free_string(str)
char * str;
{
	char * s;

	allocd_strings--;
	allocd_bytes -= 4 + (strlen(str) + 3) & ~3;

#ifndef	BUG_FREE
#ifdef	dcheck	/* GNU malloc range check flag */
	{ int align;
	align = (((int)str) - sizeof(int) - sizeof(short)) & WORD_B_MASK;
	if (align)
		checked("Free string: improperly aligned string!", str);
	}
#endif /* dcheck */
#endif

	s = findstring(str); /* moves it to head of table if found */
#ifndef	BUG_FREE
	if (!s) {
	    checked("Free string: not found in string table!", str);
	    return;
	}
	if (s != str) {
	    checked("Free string: string didnt hash to the same spot!", str);
	    return;
	}

	if (REFS(s) <= 0) {
	    checked("Free String: String refs zero or -ve!", str);
	    return;
	}
#endif	/* BUG_FREE */

	if (REFS(s) > MAXSHORT) return;
	REFS(s)--;
	if (REFS(s) > 0) return;

	/* It will be at the head of the hash chain */
	base_table[StrHash(str)] = NEXT(s);
	num_distinct_strings--;
	/* We know how much overhead malloc has */
	bytes_distinct_strings-= 4 + (strlen(s) + 3) & ~3;
	overhead_bytes -= sizeof(short) + sizeof(char *);
	free(s-sizeof(short)-sizeof(char *));

	return;
}

/*
 * you think this looks bad!  and we didn't even tell them about the
 * GNU malloc overhead!  tee hee!
 */

int add_string_status(verbose)
    int verbose;
{
    if (verbose) {
	add_message("\nShared string hash table:\n");
	add_message("-------------------------\t Strings    Bytes\n");
    }
    add_message("Strings malloced\t\t%8d %8d + %d overhead\n",
		num_distinct_strings, bytes_distinct_strings, overhead_bytes);
    if (verbose) {
	add_message("Total asked for\t\t\t%8d %8d\n",
		    allocd_strings, allocd_bytes);
	add_message("Space actually required/total string bytes %d%%\n",
		    (bytes_distinct_strings + overhead_bytes)*100 / allocd_bytes);
	add_message("Searches: %d    Average search length: %6.3f\n",
		    num_str_searches, (double)search_len / num_str_searches);
    }
    return(bytes_distinct_strings + overhead_bytes);
}
