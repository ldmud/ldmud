#ifndef HASH_H__
#define HASH_H__ 1

#include <stdlib.h>

extern unsigned short whashmem (const char *s, size_t len, int maxn);
extern unsigned short whashstr (const char *s, int maxn);
extern unsigned char  chashstr (const char *s, int maxn);

#endif /* HASH_H__ */
