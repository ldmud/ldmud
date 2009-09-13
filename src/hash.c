/*---------------------------------------------------------------------------
 * String Hashing Functions
 *
 *---------------------------------------------------------------------------
 * We currently use MurmurHash2, by Austin Appleby, which he placed
 * into public domain.
 * It was modified to use the fixed width integer (u)int32_t from C99.
 * 
 *---------------------------------------------------------------------------
 */

/* Note - This code makes a few assumptions about how your machine behaves -

   1. We can read a 4-byte value from any address without crashing

   And it has a few limitations -

   1. It will not work incrementally.
   2. It will not produce the same results on little-endian and big-endian
      machines.
*/

#include <stdint.h>
#include <stdlib.h>

uint32_t MurmurHash2(void const * const key, size_t len, uint32_t seed)
{
	// 'm' and 'r' are mixing constants generated offline.
	// They're not really 'magic', they just happen to work well.
    
	const uint32_t m = 0x5bd1e995;
	const int32_t r = 24;
    
	// Initialize the hash to a 'random' value
    uint32_t h = seed ^ len;
    
	// Mix 4 bytes at a time into the hash
	const unsigned char * data = (const unsigned char *)key;
    
	while(len >= 4)
	{
		uint32_t k = *(uint32_t *)data;
        
		k *= m; 
		k ^= k >> r; 
		k *= m; 
		
		h *= m; 
		h ^= k;
        
		data += 4;
		len -= 4;
	}
	
	// Handle the last few bytes of the input array
	switch(len)
	{
        case 3: h ^= data[2] << 16;
        case 2: h ^= data[1] << 8;
        case 1: h ^= data[0];
	        h *= m;
	};
    
	// Do a few final mixes of the hash to ensure the last few
	// bytes are well-incorporated.
    
	h ^= h >> 13;
	h *= m;
	h ^= h >> 15;
    
	return h;
} 