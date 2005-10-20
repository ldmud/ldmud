/*
 * This program is copyright Alec Muffett 1991 except for some portions of
 * code in "crack-fcrypt.c" which are copyright Robert Baldwin, Icarus
 * Sparry and Alec Muffett.  The author(s) disclaims all responsibility or
 * liability with respect to it's usage or its effect upon hardware or
 * computer systems, and maintain copyright as set out in the "LICENCE"
 * document which accompanies distributions of Crack v4.0 and upwards.
 */

#include "conf.h"


#define reg	register
#define uns	unsigned
#define unsb	uns char
#define unsl	uns long

/*
 * Types for the different ways to represent DES bit patterns.	Bits are
 * always right justified within fields.  Bits which have lower indices in
 * the NBS spec are stored in the vax bits with less significance (e.g., Bit
 * 1 of NBS spec is stored in the bit with weight 2 ** 0 to the Vax.
 */

#define obpb1	unsb		/* One bit per byte. */
#define sbpb6	unsb		/* Six bits per byte, 6 held. */
#define sbpb6R	unsb		/* Six bits per byte Reversed order, 6 held. */
#define sbpb24	unsl		/* Six bits per byte, 24 held. */
#define ebpb24	unsl		/* Eight bits per bit, 24 held. */
#define fbpb4	unsb		/* Four bits per byte, 4 held. */
#define fbpb4R	unsb		/* Four bits per byte Reversed order, 4 held. */

/*
 * The operation (6 * x) is often better optimised as this (for really
 * braindead compilers) - AEM
 */

#ifdef BRAINDEAD6
#define SIX_TIMES(exprn)                (((exprn) << 2) + ((exprn) << 1))
#else
#define SIX_TIMES(exprn)                (6 * (exprn))
#endif				/* BRAINDEAD6 */

/* DES transformation type... */

union SDATA
{
    sbpb24 b[2];
    sbpb6 c[8];
};

#ifndef FDES_8BYTE		/* Not on a Cray */
#ifndef FDES_4BYTE		/* Thanks to Matt Bishop for this idea -AEM. */
#define SIZEFIX 	0
#define INDIRECT(a,b)   (a)[b]
#else
#define SIZEFIX 	2	/* "n" where 2^n == sizeof(sbpb24) */
#define INDIRECT(a,b)   (*((sbpb24 *)(((unsigned char *) a) + (b))))
#endif
#endif

/*
 * These used to be rather slow and frequently used functions - AEM
 */

#define TF_TO_SIXBIT(tf) \
	(sbpb24)((tf & 077L) | \
		((tf & 07700L) << 2) | \
		((tf & 0770000L) << 4) | \
		((tf & 077000000L) << 6))

#define SIXBIT_TO_TF(sb) \
	(ebpb24)((sb & 0x3fL) | \
		((sb & 0x3f00L) >> 2) | \
		((sb & 0x3f0000L) >> 4) | \
		((sb & 0x3f000000L) >> 6))
