#include <stdio.h>

#define STRALLOC
#include "stralloc.h"

#ifndef DEBUG
#define BUG_FREE
#endif

/*
 * stralloc.c - string management.
 *
 * All strings are stored in an extensible hash table, with reference counts.
 * free_string decreases the reference count; if it gets to zero, the string
 * will be deallocated.  add_string increases the ref count if it finds a
 * matching string, or allocates it if it cant.  There is no way to allocate
 * a string of a particular size to fill later (hash wont work!), so you'll
 * have to copy things from a static (or malloced and later freed) buffer -
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

#define	WORD_ALIGN_BIT	0x3	/* these are 0 for aligned ptrs */

static mp_int num_distinct_strings = 0;
mp_int bytes_distinct_strings = 0;
mp_int stralloc_allocd_strings = 0;
mp_int stralloc_allocd_bytes = 0;
static int search_len = 0;
static int num_str_searches = 0;

/*
 * strings are stored:
 *	(char * next) (short numrefs) string
 *				      ^
 *				pointer points here
 */

#define NEXT(p) SHSTR_NEXT(p)
#define REFS(p) SHSTR_REFS(p)

/*
 * hash table - list of pointers to heads of string chains.
 * Each string in chain has a pointer to the next string and a
 * reference count (char *, int) stored just before the start of the string.
 * HTABLE_SIZE is in config.h, and should be a prime, probably between
 * 1000 and 5000.
 */

static char ** base_table = 0;
char *out_of_memory_string;
char *empty_shared_string;
extern struct ident *ident_table[ITABLE_SIZE];

void init_shared_strings()
{
    int x;
    base_table = (char **) xalloc(sizeof(char *) * HTABLE_SIZE);

    if (!base_table)
	fatal("Out of memory\n");
    for (x=0; x<HTABLE_SIZE; x++)
	base_table[x] = 0;
    out_of_memory_string = make_shared_string(
"This string is used as a substitute if allocating another one failed."
    );
    empty_shared_string = make_shared_string("");
}

void clear_shared_string_refs()
{
    int x;
    char *p;

    for (x=0; x<HTABLE_SIZE; x++)
	for (p = base_table[x]; p; p = NEXT(p) )
	    REFS(p) = 0;
}

#ifdef MALLOC_smalloc
void note_shared_string_table_ref() {
    note_malloced_block_ref((char *)base_table);
    count_ref_from_string(out_of_memory_string);
    count_ref_from_string(empty_shared_string);
}

void walk_shared_strings(func)
    void (*func) PROT((char *, char *));
{
    int x;
    char *p, *n;

    for (x=0; x<HTABLE_SIZE; x++)
	for (n = base_table[x]; p = n; ) {
	    n = NEXT(p); /* p may be freed by (*func)() . */
	    (*func)(p-sizeof(short)-sizeof(char *), p);
	}
}
#endif /* MALLOC_smalloc */

static mp_int overhead_bytes() {
    return (sizeof(char *) * HTABLE_SIZE) +
      num_distinct_strings * (sizeof(char *) + sizeof(short));
}

/*
 * generic hash function.  This is probably overkill; I haven't checked the
 * stats for different prime numbers, etc.
 */

/* some compilers can't grasp #if BITNUM(HTABLE_SIZE) == 1 */
/* some compilers can't even grasp #if BITNUM_IS_1(HTABLE_SIZE) *sigh* */
#if !( (HTABLE_SIZE) & (HTABLE_SIZE)-1 )
#define StrHash(s) (whashstr((s), 20) & ((HTABLE_SIZE)-1))
#else
#define StrHash(s) (whashstr((s), 20) % HTABLE_SIZE)
#endif

/*
 * Looks for a string in the table.  If it finds it, returns a pointer to
 * the start of the string part, and moves the entry for the string to
 * the head of the pointer chain.  One thing (blech!) - puts the previous
 * pointer on the hash chain into fs_prev.
 */

static int hash_index;	/* to be used by alloc_new_string
			   without further notice 		  */
			/* is also used opaque inside free_string */

char * findstring(s)
char * s;
{
	char * curr, *prev;
	int h = StrHash(s);
	hash_index = h;

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

static INLINE char * alloc_new_string(string)
char * string;
{
	mp_int length;
	char *s;
	int h;

	length = strlen(string);
	s = xalloc(1 + length + sizeof(char *) + sizeof(short));
	if (!s)
	    return s;
	h = hash_index;
	s += sizeof(char *) + sizeof(short);
	strcpy(s, string);
#if 0
	REFS(s) = 0;
#endif
	NEXT(s) = base_table[h];
	base_table[h] = s;
	num_distinct_strings++;
	bytes_distinct_strings +=
#ifdef MALLOC_smalloc
	  SMALLOC_OVERHEAD * sizeof(char *) +
#else
	  sizeof(char *) +
#endif
	    (sizeof(char *) + sizeof(short) +
	    length + 1 + (sizeof(char *)-1)) & ~(sizeof(char *)-1);
	REFS(s) = 1;
	stralloc_allocd_strings++;
	stralloc_allocd_bytes += shstr_malloced_size(s);
	return(s);
}

char * make_shared_string(str)
char * str;
{
	char * s;

	s = findstring(str);
	if (!s) {
		return alloc_new_string(str);
	} else {
	    if (REFS(s))
		REFS(s)++;
	}
	stralloc_allocd_strings++;
	stralloc_allocd_bytes += shstr_malloced_size(s);
	return(s);
}

INLINE
void decrement_string_ref(str)
char *str;
{
	stralloc_allocd_strings--;
	stralloc_allocd_bytes -= shstr_malloced_size(str);
	if (REFS(str))
	    REFS(str)--;
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

#ifndef BUG_FREE
static void checked(s, str) char * s, *str;
{
	fprintf(stderr, "%s (\"%s\")\n", s, str);
	fatal(s); /* brutal - debugging */
}
#endif

void free_string(str)
char * str;
{
#ifndef BUG_FREE
	extern int d_flag;
#endif

	char * s;

	stralloc_allocd_strings--;
	stralloc_allocd_bytes -= shstr_malloced_size(str);

#ifndef	BUG_FREE
#ifdef	dcheck	/* GNU malloc range check flag */
	{ int align;
	align = (((int)str) - sizeof(int) - sizeof(short)) & WORD_B_MASK;
	if (align)
		checked("Free string: improperly aligned string!", str);
	}
#endif /* dcheck */
#endif

#ifndef	BUG_FREE
	s = findstring(str); /* moves it to head of table if found */
	if (!s) {
	    checked("Free string: not found in string table!", str);
	    return;
	}
	if (s != str) {
	    checked("Free string: string didnt hash to the same spot!", str);
	    return;
	}

	if (REFS(s) <= 0 && d_flag) {
	    fprintf(
	      stderr,
	      "Free String: String refs zero or -ve! (\"%s\")\n",
	      str
	    );
	}
#endif	/* BUG_FREE */

	if (!REFS(str) || --REFS(str) > 0) return;

	s = findstring(str); /* moves it to head of table if found */
#ifndef BUG_FREE
	/* It will be at the head of the hash chain */
	base_table[StrHash(str)] = NEXT(str);
#else
	base_table[hash_index] = NEXT(str);
#endif
	num_distinct_strings--;
	/* We know how much overhead malloc has */
	bytes_distinct_strings -= shstr_malloced_size(str) * sizeof(char *);
	xfree(str-sizeof(short)-sizeof(char *));

	return;
}

/*
 * you think this looks bad!  and we didn't even tell them about the
 * GNU malloc overhead!  tee hee!
 */

int add_string_status(verbose)
    int verbose;
{
    mp_int net_bytes_distinct_strings, net_allocd_bytes;

    if (verbose) {
	add_message("\nShared string hash table:\n");
	add_message("-------------------------\t Strings    Bytes\n");
    }
    net_bytes_distinct_strings = (bytes_distinct_strings &
	(malloc_size_mask() * sizeof (char *))) -
      num_distinct_strings * (sizeof(char*) + sizeof(short));
    add_message("Strings malloced\t\t%8ld %8ld + %ld overhead\n",
		num_distinct_strings, net_bytes_distinct_strings, overhead_bytes());
    if (verbose) {
	char print_buff[20];

	stralloc_allocd_bytes &= malloc_size_mask();
	net_allocd_bytes = (stralloc_allocd_bytes * sizeof(char*)) -
          stralloc_allocd_strings * (sizeof(char*) + sizeof(short));
	add_message("Total asked for\t\t\t%8ld %8ld\n",
		    stralloc_allocd_strings, net_allocd_bytes );
	add_message("Space actually required/total string bytes %ld%%\n",
		    (net_bytes_distinct_strings + overhead_bytes())*100L /
		    	net_allocd_bytes );
	sprintf(print_buff, " %7.3f\n",
		    (float)search_len / (float)num_str_searches);
	add_message("Searches: %d    Average search length:%s\n",
		    num_str_searches, print_buff);
    }
    return net_bytes_distinct_strings + overhead_bytes();
}
