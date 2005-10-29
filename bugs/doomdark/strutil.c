/*
 * Added by Doomdark 11-aug-95:
 *
 * char *substitute_string(char *, vector *, vector *, int, int)
 *
 *	- Substitutes those words from 1st argument that are listed in
 *	  argument 2, with matching word from argument 3. Length of shortest
 *	  string in argument 2 is argument 4, and longest is argument 5.
 *	  arguments 4 and 5 are optional.
 *	  Used by build-in quicktyper in StickLib.
 */

#ifdef SUBST_DEBUG
#include <string.h>
#endif

#include <stdio.h>
#include <ctype.h>

#include "config.h"
#include "array.h"
#include "interpret.h"

/* When compiling from bugs/doomdark/..., use these includes:
#include "../../config.h"
#include "../../array.h"
#include "../../interpret.h"
 */

/* Routine to replace aliases in a string. Currently considers all
 * strings that do not contain spaces as words This could easily be
 * changed, though.
 */

#define	MAX_REC_DEPTH	8
#define STR_BUF_LENGTH	1000

#define	MY_ALNUM(x)	(isalnum(x) || x == '_')

static char *rec_stack[MAX_REC_DEPTH];
static char strbuf[STR_BUF_LENGTH + 1];

#if 0
#define	RETURN_STRING {\
		*dest = '\0';\
		src = dest = xalloc(dest - strbuf + 1);\
		tmp = strbuf;\
		while (*tmp) {\
			*dest = *tmp;\
			tmp++;\
			dest++;\
		}\
		return src;\
	}
#else

#define	RETURN_STRING	{\
		if (!replaced)\
			return 0;\
		*dest = '\0';\
		return strbuf;\
	}

#endif

/* Arguments:
 *	src	= String to be processed
 *	aliases	= List (LPC array) of aliases (keywords that are to be
 *		substituted) in alphabetic order. Have to be in alph. order,
 *		to allow binary search!
 *	repl_stings = List of substitutes; array of same size as aliases;
 *		each alias will get substituted by the element in same in
 *		same position in this array.
 *	min_len	= Length of shortest alias in alias list.
 *	max_len = Length of longest alias in alias list.
 *		(These last two are used for efficiency; optional in a sense)
 */

char *
substitute_string(src, aliases, repl_strings, min_len, max_len)
char *src;
struct vector *aliases;
struct vector *repl_strings;
int min_len;
int max_len;
{
int rec_count = 0, replaced = 0;
char *dest = strbuf;
char *end = &strbuf[STR_BUF_LENGTH - 1];
char *a, *b, *tmp;
char x;
int i;
int str_len, low, high, curr, bingo, a_size;

    a_size = VEC_SIZE(aliases);

    do {
#ifdef SUBST_DEBUG
fprintf(stderr, "Main loop begins; buffer left: '%s'.\n", src);
#endif
	while ((x = *src) && !(MY_ALNUM(x))) {
	/* Let's skip all non-letters */
		*dest = x;
		if (++dest >= end)
			RETURN_STRING
		src++;
	}
/* So, we are now at the end of this buffer; be it replaced alias or the
 * 'main' buffer... That depends on recursion level.
 */
	if (!x) {
#ifdef SUBST_DEBUG
fprintf(stderr, "Current string ends; returning from recoursion..\n");
#endif
		if (!rec_count)
			RETURN_STRING
		src = rec_stack[--rec_count];
		continue;
	}
	tmp = src;
	while ((x = *tmp) && MY_ALNUM(x))
		tmp++;	/* Let's search last char of the word. */
	str_len = tmp - src;
	if (str_len >= min_len && str_len <= max_len &&
	rec_count < MAX_REC_DEPTH) {
#ifdef SUBST_DEBUG
fprintf(stderr, "Alias check loop begins.\n");
#endif
		bingo = low = -1;
		high = a_size;
		while ((curr = (low + high) / 2) > low) {
			a = src;
			b = aliases -> item[curr].u.string;
			i = 1;
			while ((x = *b) && !(i = *a - x)) {
				a++;
				b++;
			}
			if (!i && a == tmp) {
				bingo = curr;
				break;
			}
			if (i < 0) {
				if (!curr) break;
				high = curr;
			} else {
				low = curr;
			}
		}
/* If it was an alias, let's substitute it; let's simulate recursion with
 * the recursion stack.
 */
		if (bingo >= 0) {
#ifdef SUBST_DEBUG
fprintf(stderr, "Alias found; doing recursion.\n");
#endif
			rec_stack[rec_count++] = tmp;
			src = repl_strings -> item[bingo].u.string;
			replaced++;
			continue;
		}
	}
/* If couldn't substitute it, let's copy it... */
	if (str_len > (end - dest + 1)) {
#ifdef SUBST_DEBUG
fprintf(stderr, "Substitution begins.\n");
#endif

/* If we would skip the end of the buffer, let's copy string and return it. */
		while (dest < end) {
			*dest = *src;
			dest++;
			src++;
		}
		RETURN_STRING;
	}
	do {
		*dest = *src;
		dest++;
	} while (++src < tmp);
    } while (1);
}
