#if !defined(_PATCHFLOAT_H) && defined(_NO_OWN_FLOATS)
#define _PATCHFLOAT_H 1

#include <exec/types.h>

#if !defined(INCLUDE_VERSION) || (INCLUDE_VERSION <= 39)

#  if defined(_DCC)
#    ifdef _SP_FLOAT
       typedef double float;
#    endif
#  elif defined(__SASC)
#    ifdef _IEEE
       typedef double float;
#    endif
#  else /* default */
     typedef double float;
#  endif

#endif

#endif
