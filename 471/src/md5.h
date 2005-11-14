#ifndef MD5_H__
#define MD5_H__ 1

/*------------------------------------------------------------------
 * MD5.H - header file for MD5C.C
 * Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991.
 * All rights reserved.
 *------------------------------------------------------------------
 * License to copy and use this software is granted provided that it
 * is identified as the "RSA Data Security, Inc. MD5 Message-Digest
 * Algorithm" in all material mentioning or referencing this software
 * or this function.
 * 
 * License is also granted to make and use derivative works provided
 * that such works are identified as "derived from the RSA Data
 * Security, Inc. MD5 Message-Digest Algorithm" in all material
 * mentioning or referencing the derived work.
 * 
 * RSA Data Security, Inc. makes no representations concerning either
 * the merchantability of this software or the suitability of this
 * software for any particular purpose. It is provided "as is"
 * without express or implied warranty of any kind.
 * 
 * These notices must be retained in any copies of any part of this
 * documentation and/or software.
 *------------------------------------------------------------------
 * The sources have been edited from the original to conform to the
 * overall coding style, and to fit with the rest of the program.
 * The actual encryption code hasn't been touched.
 *------------------------------------------------------------------
 */

/* MD5 context. */
typedef struct {
    unsigned long int state[4];  /* state (ABCD) */
    unsigned long int count[2];  /* number of bits, modulo 2^64 (lsb first) */
    unsigned char buffer[64];    /* input buffer */
} MD5_CTX;

extern void MD5Init(MD5_CTX *);
extern void MD5Update(MD5_CTX *, unsigned char *, unsigned int);
extern void MD5Final(MD5_CTX *, unsigned char [16]);

#endif /* MD5_H__ */

