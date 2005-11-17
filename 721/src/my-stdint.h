#ifndef MY_STDINT_H__
#define MY_STDINT_H__

/*------------------------------------------------------------------
 * Portable definition of the subset of stdint.h we need.
 *------------------------------------------------------------------
 */

#include "driver.h"

#if defined(HAVE_STDINT_H)
#    include <stdint.h>
#elif defined(HAVE_INTTYPES_H)
#    include <inttypes.h>
#    define NEED_LEAST16_T
#elif defined(HAVE_INTTYPES)
#    include <sys/types.h>
#    define NEED_LEAST16_T
#else
#    define NEED_LEAST16_T

#    if SIZEOF_LONG == 4
typedef  unsigned long uint32_t;
#    elif SIZEOF_INT == 4
typedef  unsigned int uint32_t;
#    else
#        error "No suitable integer type found for uint32_t."
#    endif

#    if CHAR_BIT == 8
typedef  unsigned char uint8_t;
#    else
#        error "No suitable integer type found for uint8_t."
#    endif
#endif /* HAVE_STDINT_H */

#if defined(NEED_LEAST16_T)

#    if SIZEOF_LONG == 2
typedef  long int_least16_t;
#    elif SIZEOF_SHORT == 2
typedef  short int_least16_t;
#    else
#        error "No suitable integer type found for int_least16_t."
#    endif

#endif /* NEED_LEAST16_T */

#endif /* MY_STDINT_H__ */
