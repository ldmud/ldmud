/* Copyright 1995 Joern Rennecke */

#include "lint.h"

/* Implement a(i) = a(i-24) + a(i-55) */

#define OFFSET1 24U
#define OFFSET2 55U
#define RAND_STATE_SIZE OFFSET2
#define RAND_I_SHIFT (sizeof(int) * 4U)

static struct {
  mp_uint pred[RAND_STATE_SIZE];
  unsigned int i;
} rand_state;

mp_uint random_number(n)
    mp_uint n;
{
    unsigned int i;
    mp_uint sum;

    i = rand_state.i;
    i++;
    i &= i - RAND_STATE_SIZE >> RAND_I_SHIFT;
    rand_state.i = i;
    sum = rand_state.pred[i];
    i -= OFFSET2 - OFFSET1;
    i += RAND_STATE_SIZE & i >> RAND_I_SHIFT;
    sum = rand_state.pred[i] += sum;
#if defined(HAVE_LONG_LONG) && SIZEOF_P_INT == 4
    return (unsigned long long)sum * (unsigned long long)n >>
	sizeof(mp_uint) * 8;
#else
    return sum % n;
#endif
}

void seed_random(seed)
    int seed;
{
    int i;

    memset(rand_state.pred, seed, sizeof rand_state.pred);
    rand_state.pred[0] = seed;
    rand_state.pred[RAND_STATE_SIZE-2] = seed;
    rand_state.i = seed & RAND_STATE_SIZE - 1;
    strcpy((char *)&rand_state.pred[1],
      "The quick brown fox jumps over the lazy dog\n");
    i = RAND_STATE_SIZE * 3;
    do {
	random_number(1);
    } while (--i);
}
