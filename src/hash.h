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

extern void MurmurHash3_x86_32 ( const void * key, int len,
                                uint32_t seed, void * out );

// Hash <len> characters into a uint32_t integer and return the result.
static INLINE hash32_t hashmem32(void const * key, size_t len)
                        __attribute__((nonnull(1))) __attribute__((pure))
                        __attribute__((flatten));

// Hash <len> characters into a hash32_t. Uses <initval> as initial hash value 
// and is used to hash several strings into one hash value.
static INLINE hash32_t hashmem32_chained(void const * key, size_t len, hash32_t initval)
                        __attribute__((nonnull(1))) __attribute__((pure))
                        __attribute__((flatten));

// Hash a pointer into a hash32_t.
static INLINE hash32_t hashpointer(void const * ptr)
                        __attribute__((nonnull)) __attribute__((pure))
                        __attribute__((flatten));

static INLINE hash32_t hashmem32(void const * key, size_t len)
{
    uint32_t result;
    MurmurHash3_x86_32(key, len, INITIAL_HASH, &result);
    return result;
}

static INLINE hash32_t hashmem32_chained(void const * key, size_t len, hash32_t initval)
{
    uint32_t result;
    MurmurHash3_x86_32(key, len, initval, &result);
    return result;
}


// This is Thomas Wang's hash32shiftmult function:
// http://www.concentric.net/~Ttwang/tech/inthash.htm
static INLINE uint32_t hash32shiftmult(uint32_t key) {
    key = (key ^ 61) ^ (key >> 16);
    key = key + (key << 3);
    key = key ^ (key >> 4);
    key = key * 0x27d4eb2d;
    key = key ^ (key >> 15);
    return key;
}

// This is Thomas Wang's 64 bit to 32 bit Hash Function:
// http://www.concentric.net/~Ttwang/tech/inthash.htm
static INLINE uint32_t hash6432shift(uint64_t key)
{
    key = (~key) + (key << 18); // key = (key << 18) - key - 1;
    key = key ^ (key >> 31);
    key = key * 21; // key = (key + (key << 2)) + (key << 4);
    key = key ^ (key >> 11);
    key = key + (key << 6);
    key = key ^ (key >> 22);
    return (uint32_t) key;
}


static INLINE hash32_t hashpointer(void const * ptr)
{
#if UINTPTR_MAX <= UINT32_MAX   // ILP32 platform (or other with small pointers)
    return hash32shiftmult((uintptr_t)ptr);
#elif UINTPTR_MAX <= UINT64_MAX // LP64 platform
    return hash6432shift((uintptr_t)ptr);
#else   // fallback if pointer sizes are neither 4 bytes nor 8 bytes.
#warning Neither ILP32 nor LP64 platform - using generic hashing for pointers.
    uint32_t result;
    MurmurHash3_x86_32(ptr, sizeof(void *), INITIAL_HASH, &result);
    return result;
#endif
}

#endif /* HASH_H__ */
