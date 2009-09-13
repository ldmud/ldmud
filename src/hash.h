#ifndef HASH_H__
#define HASH_H__ 1

#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include "driver.h"

// seems a bit superfluous, but I think, the semantics of the type is clearer.
typedef uint8_t   hash8_t;
typedef uint16_t  hash16_t;
typedef uint32_t  hash32_t;

#define MAX_HASH8  (UINT8_MAX)
#define MAX_HASH16 (UINT16_MAX)
#define MAX_HASH32 (UINT32_MAX)

#define INITIAL_HASH 5381

#define hashsize(n) (((uint32_t)1<<(n))-1)
#define hashmask(n) (hashsize(n) - 1)

extern uint32_t MurmurHash2 (void const * const key, size_t len, uint32_t seed);

// Hash <len> characters into a uint32_t integer and return the result.
static INLINE hash32_t hashmem32(void const * key, size_t len)
                        __attribute__((nonnull(1))) __attribute__((pure))
                        __attribute__((flatten));

// Hash <len> characters into a hash32_t. Uses <initval> as initial hash value 
// and is used to hash several strings into one hash value.
static INLINE hash32_t hashmem32_chained(void const * key, size_t len, hash32_t initval)
                        __attribute__((nonnull(1))) __attribute__((pure))
                        __attribute__((flatten));



static INLINE hash32_t hashmem32(void const * key, size_t len)
{
    return MurmurHash2(key, len, INITIAL_HASH);
}

static INLINE hash32_t hashmem32_chained(void const * key, size_t len, hash32_t initval)
{
    return MurmurHash2(key, len, initval);
}



#endif /* HASH_H__ */
