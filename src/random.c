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

/* random_number() is in random.h to be inlined. */
/* Return a random number in the range 0..n-1.
 *
 * The MT FAQ suggests:
 *  If the application is not sensitive to the rounding off error, then please
 *  multiply N to [0,1)-real uniform random numbers and take the integer part
 *  (this is sufficient for most applications).
 * I use the appropriate functions from the SFMT to generate random numbers on
 * the [0,1) interval and multiply with N.
 */
#if SIZEOF_LONG == SIZEOF_CHAR_P
uint64_t random_number(uint64_t n) {
    return genrand_res53() * n;
}
#elif SIZEOF_INT == SIZEOF_CHAR_P
uint32_t random_number(uint32 n) {
     return genrand_real2() * n;
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
