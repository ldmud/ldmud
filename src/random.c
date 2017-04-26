/*------------------------------------------------------------------
 * Wrapper for the 'SIMD oriented Fast Mersenne Twister.
 *
 * SFMT was developed by Mutsuo Saito and Makoto Matsumoto, 
 *   Hiroshima University,
 *   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/index.html)
 * SFMT was integrated to LDMud by Zesstra@MorgenGrauen (http://mg.mud.de)
 *------------------------------------------------------------------
 */

#include "driver.h"
#include "random.h"
#include "backend.h"

/* This is included on purpose so that the compiler may inline some functions.
 * They are anyway never used anywhere else, all other parts of the driver use
 * only the wrapper functions in this file.
 */
#include "random/SFMT.c"

const unsigned int INIT_ARRAY_SIZE = 156U; // 4*156 == 624 bytes

// Name of the device/file to seed the PRNG from
char * prng_device_name = NULL;

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*                     Driver interface functions                          */
/*-------------------------------------------------------------------------*/

static p_uint scale(p_uint rand, p_uint n)

/* Take a random number <rand> using the full size of p_uint
 * and scale it to the range 0..<n>-1.
 */

{
#define BITS (sizeof(p_uint)*8)
#define HBITS (BITS/2)
#define HMASK ((((p_uint)1)<<HBITS) - 1)

    /* We split both rand and n into half. */
    p_uint rand_h = rand >> HBITS;
    p_uint rand_l = rand & HMASK;

    p_uint n_h = n >> HBITS;
    p_uint n_l = n & HMASK;

    /* (rand * n) >> BITS
     *  = ((rand_h << HBITS + rand_l) * (n_h << HBITS + n_l)) >> BITS
     *  = ((rand_h * n_h) << BITS + (rand_h * n_l + rand_l * n_h) << HBITS + (rand_l * n_l)) >> BITS
     *  = (rand_h * n_h) + (rand_h * n_l) >> HBITS + (rand_l * n_h) >> HBITS + (rand_l * n_l) >> BITS
     *
     * So we do each of these summands seperately, adding the overflowing bits
     * from the lower half to the upper half.
     */

    p_uint result_h = rand_h * n_h;
    p_uint result_l = rand_h * n_l;

    result_h += result_l >> HBITS;
    result_l &= HMASK;

    result_l += rand_l * n_h;
    result_h += result_l >> HBITS;
    result_l &= HMASK;

    result_l += (rand_l * n_l) >> HBITS;
    result_h += result_l >> HBITS;

    return result_h;
} /* scale() */

/* random_number() is in random.h to be inlined. */
/* Return a random number in the range 0..n-1.
 */
#if SIZEOF_LONG == SIZEOF_CHAR_P
uint64_t random_number(uint64_t n)
{
    return scale(gen_rand64(), n);
}
#elif SIZEOF_INT == SIZEOF_CHAR_P
uint32_t random_number(uint32 n)
{
    return scale(gen_rand32(), n);
}
#else
#error We currently do not yet support a 128 bit integer type used as \
  svalue number type.
#endif

void seed_random_from_int (uint32_t seed)
/* Initialize the generator */

{
    printf("%s Seeding PRNG with: 0x%lx\n"
                 , time_stamp(), (unsigned long)seed);
    debug_message("%s Seeding PRNG with: 0x%lx\n"
                 , time_stamp(), (unsigned long)seed);

    init_gen_rand(seed);
} /* seed_random_from_int() */

/*-------------------------------------------------------------------------*/

void 
seed_random(const char *filename)
    /* Opens the file given by filename and reads 156 uint32_t (624
     * bytes) from it (most often the file is probably /dev/urandom or
     * /dev/random). If successful the random number generator will be seeded
     * by an array of 624 bytes. Otherwise the driver clock will be used as
     * fallback.
     */
{
    FILE *seedsrc = NULL; // Filepointer
    
    // If we got a NULL pointer or an empty string, don't try to open some
    // device/file.
    if (filename != NULL && strlen(filename))
        seedsrc = fopen(filename,"rb");

    // if we have a file descriptor try to get a suitable amount of 32-bit
    // values from a file (right now 156 uint32_t / 624 bytes)
    if (seedsrc) {
        uint32_t seeddata[INIT_ARRAY_SIZE];
        size_t count = fread( seeddata, sizeof(uint32), INIT_ARRAY_SIZE,
                               seedsrc );
        fclose(seedsrc);
        if( count == INIT_ARRAY_SIZE ) {
            init_by_array(seeddata, INIT_ARRAY_SIZE ); // seed PRNG
            printf("%s Seeding PRNG from %s.\n", time_stamp(),
                    filename);
            debug_message("%s Seeding PRG from %s.\n", time_stamp(),
                    filename);
            return;
        } // if (count == INIT_ARRAY_SIZE)
    } // if (seedsrc)
    
    // Fall-back: driver clock
    printf("%s Seeding PRNG with current driver time\n"
                 , time_stamp());
    debug_message("%s Seeding PRNG with current driver time\n"
                 , time_stamp());
    seed_random_from_int((uint32_t)current_time);

} /* seed_random() */

/***************************************************************************/
