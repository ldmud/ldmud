/*
 * This program is copyright Alec Muffett 1991 except for some portions of
 * code in "crack-fcrypt.c" which are copyright Robert Baldwin, Icarus
 * Sparry and Alec Muffett.  The author(s) disclaims all responsibility or
 * liability with respect to it's usage or its effect upon hardware or
 * computer systems, and maintain copyright as set out in the "LICENCE"
 * document which accompanies distributions of Crack v4.0 and upwards.
 */

#include <stdio.h>

/*
 * What bytesex is your machine ? Select one of the two below, if you have
 * some really weird machine - otherwise the program should be able to work
 * it out itself.
 */

#undef BIG_ENDIAN
#undef LITTLE_ENDIAN

/* If you haven't selected one of the above options... */
#if	!defined(BIG_ENDIAN) && !defined(LITTLE_ENDIAN)

/* Can we work out if we are little endian ? */
#if	defined(vax) || defined(ns32000) || defined(sun386) || \
	defined(i386) || defined(MIPSEL) || defined(BIT_ZERO_ON_RIGHT)
#define LITTLE_ENDIAN		/* YES */
#endif

/* Can we work out if we are bigendian ? */
#if	defined(sel) || defined(pyr) || defined(mc68000) || \
	defined(sparc) || defined(is68k) || defined(tahoe) || \
	defined(ibm032) || defined(ibm370) || defined(MIPSEB) || \
	defined(__convex__) || defined(hpux) || defined(apollo) || \
	defined (BIT_ZERO_ON_LEFT) || defined(m68k) || defined(m88k) || \
	defined(_IBMR2) || defined(AMIGA) /* yes, an Amiga A500... */
#define BIG_ENDIAN		/* YES */
#endif

/* end of trying to guess things */
#endif

/* are we schitzophrenic ? */
#if	defined(BIG_ENDIAN) && defined(LITTLE_ENDIAN)
ERROR_BAD_BIT_ORDER;		/* YES */
#endif

/* are we still ignorant ? */
#if	!defined(BIG_ENDIAN) && !defined(LITTLE_ENDIAN)
ERROR_NO_BIT_ORDER;		/* YES */
#endif


/*
 * if defined, use builtin clearing in preference to using bzero(), for 4
 * or 8 byte long ints.  This is most preferable, and a Good Thing.  If it
 * is not defined, fcrypt() will try to use bzero().
 */

#undef BUILTIN_CLEAR

/*
 * define this if you have a 4 byte "long_int" on RISC machines and want a
 * speedup - it should not hurt CISC machines either.  Do NOT define it on a
 * 8-byte int machine...
 */

#undef FDES_4BYTE

/*
 * define this if you are on a Cray or something with an 8-byte int, to
 * enable Matthew Kaufman's fcrypt fix.  I hope it works okay, cos I can't
 * test it - AEM.
 */

#undef FDES_8BYTE

/*
 * undef this if your compiler knows the fact that 6*x == x<<1 + x<<2
 */

#undef BRAINDEAD6

