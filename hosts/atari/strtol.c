/* original from norbert schelenkar's stdio */
/* eff hacks	++jrb */
/* conversion to ansi spec -- mj */
/* 
 * Base can be anything between 2 and 36 or 0.
 * If not NULL then resulting *endptr points to the first
 * non-accepted character.
 */

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>

/* macro to avoid most frequent long muls on a lowly 68k */
#define _BASEMUL(B, SH, X) \
    ((0 != SH) ? \
       ((10 == (B)) ? ((((X) << (SH)) + (X)) << 1) : ((X) << (SH))) : \
       ((X) * (B))) 

long int strtol(nptr, endptr, base)
register const char *nptr;
char **endptr;
int base;
{
  register short c;
  long result = 0L;
  long limit;
  short negative = 0;
  short overflow = -2;	/* if this stays negative then
			  no conversion was performed */
  short digit;
  int shift;

  if (endptr != NULL)
      *endptr = (char *)nptr;

  while ((c = *nptr) && isspace(c))	/* skip leading white space */
	nptr++;
  if ((c = *nptr) == '+' || c == '-') {	/* handle signs */
	negative = (c == '-');
	nptr++;
  }
  if (base == 0) {			/* determine base if unknown */
	if (*nptr == '0') {
		base = 8;
		nptr++;
		if ((c = *nptr) == 'x' || c == 'X') {
			base += base;
			nptr++;
		}
	}
	else
		base = 10;
  }
  else
  if (base == 16 && *nptr == '0') {	/* discard 0x/0X prefix if hex */
	nptr++;
	if ((c = *nptr == 'x') || c == 'X')
		nptr++;
  }
  if (base < 2 || base > 36)
      return result;

  /* be careful with a preprocessor and signs!! */
  limit = (long)LONG_MIN/(long)base; 		/* ensure no overflow */
  shift = 0;
  switch (base) {
      case 32: shift++;
      case 16: shift++;
      case 8: shift++;
      case 4: case 10: shift++;
      case 2: shift++;
      default:;
  }
  
  nptr--;				/* convert the number */
  while (c = *++nptr) {
#if 0 /* Amylaar: non-numeric characters should not be accepted. */
	if (isdigit(c))
		digit = c - '0';
	else
		digit = c - (isupper(c) ? 'A' : 'a') + 10;
	if (digit < 0 || digit >= base)
		break;
#else
	if (isdigit(c)) {
		digit = c - '0';
		if (digit < 0 || digit >= base)
			break;
	} else {
		digit = c - (isupper(c) ? 'A' : 'a') + 10;
		if (digit < 10 || digit >= base)
			break;
	}
#endif
	if (0 == (overflow &= 1)) {	/* valid digit
					   - some conversion performed */
	    if ((result < limit) ||
		(digit >
		 ((result = _BASEMUL(base, shift, result)) - LONG_MIN))) {
		result = LONG_MIN;
		overflow = 1;
		
	    }
	    else 
		result -= digit;
	}
  }

  if (!negative) {
      if (LONG_MIN == result) {
	  result += 1;		/* this is -(LONG_MAX) */
	  overflow = 1;		/* we may get LONG_MIN without overflow */
      }
      result = 0L - result;
  }
  
  if (overflow > 0) {
	errno = ERANGE;
  }

  if ((endptr != NULL) && (overflow >= 0)) 	/* move *endptr if some */
      *endptr = (char *) nptr;                  /* digits were accepted */
  return result;
}
