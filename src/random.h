#ifndef RANDOM_H__
#define RANDOM_H__ 1

#include "driver.h"
// SFTM expects MEXP to contain the desired period length
#ifdef RANDOM_PERIOD_LENGTH
#define MEXP RANDOM_PERIOD_LENGTH
#endif

#include "random/SFMT.h"

#define PRNG_DEFAULT_DEVICE "/dev/urandom"

// device/file to read seed for the PRNG from
extern char * prng_device_name;

/* --- Prototypes --- */
extern void seed_random_from_int(uint32_t seed);
extern void seed_random(const char *filename);
#if SIZEOF_LONG == SIZEOF_CHAR_P
uint64_t random_number(uint64_t n);
#elif SIZEOF_INT == SIZEOF_CHAR_P
uint32_t random_number(uint32_t n);
#else
#error We currently do not yes support a 128 bit integer type used as \
  svalue number type.
#endif

#endif  /* RANDOM_H__ */
