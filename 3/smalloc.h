#ifndef SMALLOC_H
#define SMALLOC_H
#include "config.h"
#ifndef MALLOC_smalloc /* sigh */
#define svalue_strlen(v) (strlen((v)->u.string))
#define _svalue_strlen(v) (strlen((v)->u.string))
#define malloced_strlen(s) (strlen(s)+1)
#else

#define MASK	   0x0fffffff
#define M_REF	   0x20000000

#ifdef __GNUC__
#define SMALLOC_H_INLINE inline
#else
#define SMALLOC_H_INLINE
#endif

#ifdef SMALLOC_LPC_TRACE
#ifdef SMALLOC_TRACE
#define SMALLOC_OVERHEAD (7)
#else
#define SMALLOC_OVERHEAD (4)
#endif
#else /* !SMALLOC_LPC_TRACE */
#ifdef SMALLOC_TRACE
#define SMALLOC_OVERHEAD (4)
#else
#define SMALLOC_OVERHEAD (1)
#endif
#endif /* SMALLOC_LPC_TRACE */

#define malloced_size(ptr) ( ((p_uint *)(ptr))[-SMALLOC_OVERHEAD] & MASK )

#ifdef USES_SVALUE_STRLEN
static SMALLOC_H_INLINE int _svalue_strlen(v)
register struct svalue *v;
{
    register short type = v->x.string_type;
    register char *p = v->u.string;
    register int i;

    if (type == STRING_MALLOC) {
	i = (
		(((p_uint*)(p))[-SMALLOC_OVERHEAD] & MASK) -
		 (SMALLOC_OVERHEAD + 1)
	    ) * SIZEOF_P_INT;
	if (*(p+=i))
	    if (*++p)
		if (!*++p) return i+2;
#if SIZEOF_P_INT == 4
		else return i+3;
#else
		else return i+3+strlen(p+1);
#endif
	    else return i+1;
	return i;
    } else if (type == STRING_SHARED) {
    	if ( (i =
	       (*(p_uint*)
		 (p - sizeof(short) - (SMALLOC_OVERHEAD+1) * SIZEOF_P_INT) &
		MASK) * SIZEOF_P_INT -
	       sizeof(short) -
	       (SMALLOC_OVERHEAD+2) * SIZEOF_P_INT
	     ) >= 0)
#if SIZEOF_P_INT == 4
	{
    	    if (*(p+=i))
    		if (*++p)
	    	    if (!*++p) return i+2;
		    else return i+3;
	    	else return i+1;
	    return i;
    	}
	if (!*p) return 0;
	return 1;
#else
	    return i + strlen(p+i);
	return strlen(p);
#endif
    } else {
	return strlen(p);
    }
}

#ifdef SMALLOC_H_INLINE
static int svalue_strlen(v) struct svalue *v; { return _svalue_strlen(v); }
#else
#define svalue_strlen(v) (_svalue_strlen((v)))
#endif
#endif /* USES_SVALUE_STRLEN */

#define malloced_strlen(s) ( ( \
	(*(p_uint *)((s)-sizeof(p_int)*SMALLOC_OVERHEAD) & MASK) \
	- SMALLOC_OVERHEAD) * SIZEOF_P_INT)

#endif /* MALLOC_smalloc */
#endif /* SMALLOC_H */
