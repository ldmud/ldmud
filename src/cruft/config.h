/*
 * some generic large primes used by various hash functions in different files
 * You can alter these if you know of a better set of numbers!  Be sure
 * they are primes...
 */

#define	P1		701	/* 3 large, different primes */
#define	P2		14009	/* There's a file of them here somewhere :-) */
#define	P3		54001

#define BITNUM(n) ( \
	 ((n)&010101010101)+\
	(((n)&020202020202)>>1)+\
	(((n)&000404040404)>>2)+\
	(((n)&001010101010)>>3)+\
	(((n)&002020202020)>>4)+\
	(((n)&004040404040)>>5)\
) %63

#define BITNUM_IS_1(n) ( !( (n) & (n)-1 ) )

