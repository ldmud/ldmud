#ifndef MD5_H__
#define MD5_H__ 1

/*------------------------------------------------------------------
 * md5.h - header file for md5.c
 *
 * Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991.
 * All rights reserved.
 *
 * Copyright (c) 2000 The Apache Software Foundation.  All rights
 * reserved.
 *
 * See md5.c for the full license information.
 *------------------------------------------------------------------
 */

#include <stdlib.h>

/* MD5 context. */
typedef struct {
    unsigned long int state[4];  /* state (ABCD) */
    unsigned long int count[2];  /* number of bits, modulo 2^64 (lsb first) */
    unsigned char buffer[64];    /* input buffer */
} M_MD5_CTX;

extern void MD5Init(M_MD5_CTX *);
extern void MD5Update(M_MD5_CTX *, unsigned char *, unsigned int);
extern void MD5Final(M_MD5_CTX *, unsigned char [16]);

/* The following function is from Apache */

extern void MD5Encode(unsigned char *, unsigned char *, char *, size_t);

#endif /* MD5_H__ */

