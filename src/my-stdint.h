#ifndef MY_STDINT_H__
#define MY_STDINT_H__

/*------------------------------------------------------------------
 * Portable definition of the subset of stdint.h we need.
 *------------------------------------------------------------------
 */

#include "driver.h"

#if defined(HAS_STDINT_H)
#    include <stdint.h>
#else
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

#    if SIZEOF_LONG == 2
typedef  long int_least16_t;
#    elif SIZEOF_SHORT == 2
typedef  short int_least16_t;
#    else
#        error "No suitable integer type found for int_least16_t."
#    endif

#endif /* HAS_STDINT_H */

#endif /* MY_STDINT_H__ */
