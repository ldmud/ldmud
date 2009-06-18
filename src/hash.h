#ifndef HASH_H__
#define HASH_H__ 1

#include <stdlib.h>
#include <limits.h>
#include "driver.h"

// seems a bit superfluous, bit I think, the semantics of the type is clearer.
typedef uint8_t   hash8_t;
typedef uint16_t  hash16_t;
typedef uint32_t  hash32_t;

#define MAX_HASH8  (UINT8_MAX)
#define MAX_HASH16 (UINT16_MAX)
#define MAX_HASH32 (UINT32_MAX)

#define INITIAL_HASH 5381

#define hashsize(n) (((uint32_t)1<<(n))-1)
#define hashmask(n) (hashsize(n) - 1)

#ifdef WORDS_BIGENDIAN
extern uint32_t hashbig( const void *key, size_t length, uint32_t initval);
#else
extern uint32_t hashlittle( const void *key, size_t length, uint32_t initval);
#endif

// Hash <len> characters into a uint32_t integer and return the result.
static INLINE hash32_t hashmem32(const char *s, size_t len)
                        __attribute__((nonnull(1))) __attribute__((pure))
                        __attribute__((flatten));

// Hash <len>characters into a hash32_t. Uses <initval> as initial hash value 
// and is used to hash several strings into one hash value.
static INLINE hash32_t hashmem32_chained(const char *s, size_t len, hash32_t initval)
                        __attribute__((nonnull(1))) __attribute__((pure))
                        __attribute__((flatten));

static INLINE hash32_t hashmem32(const char *s, size_t len)
{
#ifdef WORDS_BIGENDIAN
    return hashbig(s, len, INITIAL_HASH);
#else
    return hashlittle(s, len, INITIAL_HASH);
#endif
}

static INLINE hash32_t hashmem32_chained(const char *s, size_t len, hash32_t initval)
{
#ifdef WORDS_BIGENDIAN
    return hashbig(s, len, initval);
#else
    return hashlittle(s, len, initval);
#endif
}

#endif /* HASH_H__ */
