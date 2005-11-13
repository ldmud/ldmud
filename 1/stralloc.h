#ifndef SHSTR_NEXT

#include "config.h"
#include "lint.h"

#define SHSTR_NEXT(str)	(*(char **)((char *) (str) - sizeof(unsigned short)\
						   - sizeof(char *)))
#define SHSTR_REFS(str)	(*(unsigned short *)((char *) (str)\
						   - sizeof(unsigned short)))
#define SHSTR_BLOCK(str) ((char *)(str) - sizeof(unsigned short)\
					- sizeof(char *))

#ifdef MALLOC_smalloc
#include "smalloc.h"
extern int malloc_size_mask PROT((void));
#define shstr_malloced_size(str) ( *( \
	(p_uint *)(str-sizeof(char*)-sizeof(unsigned short))\
	- SMALLOC_OVERHEAD) )
#else
#define malloc_size_mask() (~0)
#define shstr_malloced_size(str) (\
	(sizeof(char*) + sizeof(char *) + sizeof(short) +\
	strlen(str) + 1 + sizeof(char *) - 1) / sizeof(char *))
#endif

extern mp_int stralloc_allocd_strings, stralloc_allocd_bytes;

#if !defined(NO_INCREMENT_STRING_REF) && !defined(STRALLOC)
static INLINE
void increment_string_ref(str)
char *str;
{
        stralloc_allocd_strings++;
        stralloc_allocd_bytes += shstr_malloced_size(str);
        if (SHSTR_REFS(str))
            SHSTR_REFS(str)++;
}
#endif

#endif /* SHSTR_NEXT */
