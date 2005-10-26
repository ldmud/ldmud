#ifndef HASH_H__
#define HASH_H__ 1

#include <stdlib.h>
#include <limits.h>

typedef unsigned short whash_t;
typedef unsigned char  chash_t;

#define MAX_WHASH (USHRT_MAX)
#define MAX_CHASH (UCHAR_MAX)

extern whash_t whashmem (const char *s, size_t len, int maxn);
extern whash_t whashmem2 (const char *s, size_t len, int maxn, whash_t initial);
extern whash_t whashstr (const char *s, int maxn);
extern chash_t chashstr (const char *s, int maxn);

#endif /* HASH_H__ */
