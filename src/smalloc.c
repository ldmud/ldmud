/*---------------------------------------------------------------------------
 * SMalloc Memory Manager
 *
 * Written and put into the public domain by Sean T. "Satoria" Barrett.
 * FAST_FIT algorithm by Joern Rennecke.
 * Small block consolidation by Lars Duening.
 *---------------------------------------------------------------------------
 * Satoria's malloc intended to be optimized for lpmud. This memory manager
 * distinguishes between two sizes of blocks: small and large. It manages
 * them separately in the hopes of avoiding fragmentation between them.
 * It expects small blocks to mostly be temporaries. It expects an equal
 * number of future requests as small block deallocations.
 *
 * smalloc IS NOT THREADSAFE. And the wrong way of fixing this would be
 * to wrap all mem_alloc()/mem_free() calls into a mutex. The right way of doing
 * this would be to allocate a set of management structures for each thread
 * so that only the calls to system malloc()/sbrk() need to be guarded
 * by mutexes.
 *
 * Allocations are measured in 'word_t's which are the same size as void*.
 *
 * For MALLOC_CHECK the allocated blocks are tagged with magic words. This
 * costs a bit of time and memory, but is a good defense against the most
 * basic memory misuses.
 *
 *
 *  -- Small Blocks --
 *
 * Small blocks are allocations of up to (SMALL_BLOCK_NUM+1)*4 Bytes, currently
 * 68 Bytes. Such blocks are initially allocated from large memory blocks,
 * called "small chunks", of 16 or 32 KByte size.
 *
 * To reduce fragmentation, the initial small chunk (ok, the first small chunk
 * allocated after all arguments have been parsed) is of size
 * min_small_malloced, hopefully chosen to be a large multiple of the typical
 * small chunk size.
 *
 * When a small block is freed, it is entered in a free doubly-linked list
 * for blocks of its size, so that later allocations can use the
 * pre-allocated blocks in the free lists.  The free lists use the first
 * and last word in the "user area" for their link pointers; the small
 * chunks are themselves kept in a list and use their first word for the
 * list pointer. Small free blocks are defragmented as described below.
 *
 * If a small block can't be allocated from the appropriate free list nor
 * the small chunk, the system tries two more strategies before allocating
 * a new small chunk. First, it checks the list of oversized free small
 * blocks for a block large enough to be split.
 * If no such block exists,
 * the allocator then searches the freelists of larger block sizes for a
 * possible split.
 * If finally a new small chunk has to be allocated, the memory not used to
 * satisfy the allocation is entered as new oversized free small block into
 * the appropriate freelist.
 *
 * When a small free block is split, the allocator makes sure that the
 * remaining block is of at least SMALL_BLOCK_SPLIT_MIN words size (currently
 * 3 words -> 12 Bytes).
 * Experience has shown that the gamedriver doesn't need that many blocks of
 * less than 12 Bytes. If the allocator's splits generated those small blocks,
 * they would never be used and just tie up free space.
 *
 * To reduce memory fragmentation, the allocator implements a lazy
 * defragmentation scheme for the small blocks: if checking free lists doesn't
 * yield a suitable block, the allocator defragments the free lists and tries
 * again. Only if this second attempt fails, a new small chunk is allocated.
 * To save time, the defragmentation-on-allocation is terminated if a suitable
 * block is can be constructed (but not before the current list under
 * examination has been fully defragmented); a full defragmentation happens
 * only as part of a garbage collection.
 *
 * Obviously the defragmenter can eat up a lot of time when the free lists
 * get long, especially since most blocks can't be defragmented. So as an
 * optimization the defragmenter sets the M_DEFRAG flag in each block it
 * visited and decided as non-defragmentable. Newly freed blocks don't have
 * this flag set and are also entered at the head of their free list; this way
 * the defragmenter can stop scanning a free list as soon as it reaches the
 * first block flagged to be non-defragmentable. And while it is possible
 * that a flagged block changes to be defragmentable after all, this can
 * happen only if another block is newly freed, and the defragmenter is
 * guaranteed to find that one.
 *
 *  -- Large Blocks --
 *
 * Large blocks are allocated from the system - if large allocation is
 * too small (less than 256 KByte), the allocator allocates a 256 KByte
 * block and enters the 'unnecessary' extra memory into the freelist.
 * Large blocks are stored with boundary tags: the size field without flags
 * is replicated in the last word of the block.
 *
 * The free large blocks are stored in an AVL tree for fast retrieval
 * of best fits. The AVL structures are stored in the user area of the blocks.
 *
#ifdef USE_AVL_FREELIST
 * An AVL node is created for every distinct size of a large free block;
 * if additional blocks of the same size are freed, they are kept in
 * a double-linked list hanging of the node. Intention of this setup
 * is to decrease the size of the AVL tree (and thus increase the
 * locality large block lookups) in the case of multiple blocks of the
 * same size.
 *
 * The double-links are not really required, as only the first block on the
 * freelist or the node itself are selected for allocation, but it's
 * algorithmically simple to implement, and we have the memory for the
 * second pointer.
#ifdef MALLOC_ORDER_LARGE_FREELISTS
 * The freelist is ordered by the starting address of the blocks (with
 * the likely exception of the block holding the AVL node), in an attempt
 * to reduce high heap fragmentation.
#endif
#else
 * A new AVL node is created for every free large block.
#endif
 *
#ifdef MALLOC_SBRK && SBRK_OK && MALLOC_REPLACEABLE
 * Memory is allocated from the system with sbrk() and brk(), which puts
 * the whole heap under our control.
 *
 * In this mode, we replace the system malloc() and free() by our own 
 * functions, implementing all libc memory functions (malloc, free, calloc,
 * realloc()).
 #elif HAVE_MMAP
 * If brk/sbrk() are non-functional (e.g. on Darwin; remember: they are legacy
 * and not POSIX anymore), we check if mmap() is available. If it is, we use 
 * it to map anonymous memory pages to get memory from the VM system.
 *
 * The allocated block (modulo joints) is tagged at both ends with fake
 * "allocated" blocks of which cover the unallocated areas - large_malloc()
 * will perceive this as a fragmented heap.
 *
 #ifdef MALLOC_REPLACEABLE
 * In this mode, we replace the system malloc() and free() by our own 
 * functions, implementing all libc memory functions (malloc, free, calloc,
 * realloc()). 
 #endif
#else
 * malloc() is used to allocate a new block of memory. If this block borders
 * previous ones, the blocks are joined.
 *
 * The allocated block (modulo joints) is tagged at both ends with fake
 * "allocated" blocks of which cover the unallocated areas - large_malloc()
 * will perceive this as a fragmented heap.
#endif
 *
 *  -- Privilege Level --
 *
 * Memory is managed at three privilege levels: USER, MASTER and SYSTEM.
 * For every level, a special memory reserve is held by the backend.
 * If the system runs out of memory (or if the max_malloced limit has
 * been reached), the following steps are taken:
 *
 *  - free the user reserve, then try again.
 *  - if MASTER privilege: free the master reserve, then try again.
 *  - if SYSTEM privilege: free the system reserve, then try again.
 *  - if !SYSTEM privilege: set out_of_memory and return NULL
 *  - if SYSTEM privilege: dump the lpc backtrace and exit(2) resp. fatal().
 *
 * If any of the reserves is freed, the gc_request flag is set.
 *
 *
 *   -- Block Structure --
 *
 * The zeroth word in each large block is used to hold the size of the
 * block (incl. all overhead). This extra word is necessary as on some
 * systems large blocks can span half of the address space; however, it
 * is necessary only for large blocks.
 *
 * The first word in each allocated block is used for the 'flag&size' field
 * which holds the flags for a block, plust for small blocks the size (incl.
 * all overhead):
 *
 *   THIS_BLOCK: small: set if this block is not allocated
 *               large: set if this block is allocated
 *   PREV_BLOCK: small: set if the previous block is not allocated
 *               large: set if the previous block is allocated
 *   M_GC_FREE : set in allocated blocks if the GC may free this block if
 *               found lost
 *   M_DEFRAG  : set in small free blocks which can't be defragmented further
 *   M_REF     : set if this block is referenced (used during a GC)
 *
 * Because of the PREV_BLOCK flag, all chunk allocations (either the
 * small chunks for the small block allocator, or the esbrk() chunks for the
 * large block allocator) are initialized with a fake permanently allocatod
 * block at the end of the chunk, consisting of not much more than the
 * flag&size field with THIS_BLOCK cleared.
 *
#ifdef MALLOC_CHECK
 * The second word holds the magic word, which for small blocks is determined
 * also by the size of the block.
#endif
 *
 * When accessing these header fields, the code in general keeps a pointer
 * to the flag&size word.
 *
 * The user (aka payload) area of free blocks is used to manage the various
 * free lists.
 *
 * Small free blocks:
 *
 *   The first word of the user area holds the 'next' link of the free
 *   list, which is NULL for the last block in the list.
 *
 *   The last word in the user area holds the 'prev' link of the free
 *   list, which for the first block in the list points to the block
 *   itself.
 *
 *   For an oversized small block, the 'prev' link has the lowest bit
 *   set, and the second-to-last word in the user area holds the size
 *   of the block in words.
 *
 *   This setup allows the allocator to determine the size of a preceeding
 *   free block by simply following the 'prev' link for normally sized
 *   blocks, and by directly reading the size for an oversize block. At the
 *   same time the management structure requires just two words at minimum.
 *
 * Large free blocks:
 *
 *   The first words of the user area hold the AVL tree node structure.
 *
 *   The last word in the user area holds the size of the block in words.
 *
#ifdef HAVE_MADVISE
 * TODO: Not tested for in configure, not documented.
#endif
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#if defined(HAVE_MADVISE) || defined(HAVE_MMAP) || defined(HAVE_POSIX_MADVISE)
#    include <sys/types.h>
#    include <sys/mman.h>
#endif

#ifdef HAVE_MADVISE
#    define MADVISE(new,old)  madvise(new,old,MADV_RANDOM)
#else
#    define MADVISE(new,old)  NOOP
#endif

// for sysconf()
#include <unistd.h>

#include "smalloc.h"

#include "mstrings.h"
#include "stdstrings.h"
#include "svalue.h"
#ifdef MALLOC_EXT_STATISTICS
#include "array.h"
#include "backend.h"
#endif /* MALLOC_EXT_STATISTICS */

#include "../mudlib/sys/debug_info.h"

/* This is the granularity of memory allocations.
 * WARNING: This MUST be sizeof(word_t) at the moment. If you want to change
 * the granularity, you have to check the whole allocator first... (Or change
 * word_t.)
 * word_t is defined in xalloc.c as being p_uint, that is, it has always the
 * size of a pointer.
 */
#define GRANULARITY sizeof(word_t)

/* If configure and possible, request memory using sbrk() and replace malloc().
 * If replacing malloc is not possible, don't use sbrk().
 * If sbrk/brk() are not working, but mmap() is, then use mmap() for getting
 * memory. In this case, malloc may be replaced as well.
 * If that is also not available, use malloc().
 */
#if defined(MALLOC_SBRK)
#  if defined(SBRK_OK) && defined(MALLOC_REPLACEABLE)
#    define REPLACE_MALLOC
#  else
#    undef MALLOC_SBRK
#    undef MALLOC_REPLACEABLE
#  endif
#endif
#if defined(HAVE_MMAP) && defined(MALLOC_REPLACEABLE)
#  define REPLACE_MALLOC
#endif

#define MEM_MAIN_THREADSAFE

/* #undef NO_MEM_BLOCK_SIZE */

/* Initialiser macros for the tables */

#define INIT4  0, 0, 0, 0
#define INIT4  0, 0, 0, 0
#define INIT8  INIT4, INIT4
#define INIT12 INIT8, INIT4
#define INIT16 INIT8, INIT8
#define INIT24 INIT16, INIT8
#define INIT32 INIT16, INIT16

/*-------------------------------------------------------------------------*/

  /* A handy macro to statically determine the number of
   * elements in an array.
   */
#define NELEM(a) (sizeof (a) / sizeof (a)[0])

/* #undef DEBUG_AVL */
  /* Define this to debug the AVL tree.
   */

/* The extra smalloc header fields.
 */

#define M_LSIZE (-1)  /* (word_t) Size in word_t (large blocks only) */
#define M_SIZE  (0)   /* (word_t) Size in word_t, plus some flags */

#ifdef MALLOC_CHECK
#    define M_OVERHEAD (2)
#    define M_MAGIC  (1)  /* (word_t) The magic word */
#else
#    define M_OVERHEAD (1)
#endif /* MALLOC_CHECK */

#define ML_OVERHEAD (M_OVERHEAD+1)
   /* The overhead for large blocks. */

#define M_LINK  M_OVERHEAD
   /* Index of the 'next' link for the small free lists */

#define M_PLINK(size) ((size)-1)
   /* Index of the 'prev' link for the small free lists.
    * <size> is the block size in words.
    */

#define BLOCK_NEXT(block) ((word_t *)(block[M_LINK]))
   /* The 'next' link of free block <block>, properly typed */

#define BLOCK_PREV(block,size) ((word_t *)(block[M_PLINK(size)]))
   /* The 'prev' link of free block <block>, properly typed */


#define T_OVERHEAD (M_OVERHEAD + XM_OVERHEAD)
   /* Total overhead: it is used by smalloc to make smart decisions
    * about when to split blocks.
    */

#define TL_OVERHEAD (ML_OVERHEAD + XM_OVERHEAD)
   /* Total overhead for large blocks.
    */

#define SMALL_BLOCK_MIN (2)
   /* Minimum size of a small block in words */

#define SMALL_BLOCK_NUM (16)
   /* Number of different small block sizes.
    */

#define INIT_SMALL_BLOCK INIT16
   /* The proper initializer macro for all tables sized SMALL_BLOCK_NUM.
    */

#define SMALL_BLOCK_SPLIT_MIN (3)
   /* Minimum size of a small block created by a split, in words */

/* Derived values */

#define SMALL_BLOCK_MIN_BYTES  (SMALL_BLOCK_MIN * GRANULARITY)
   /* Minimum payload size of a small block.
    */

#define SMALL_BLOCK_MAX (SMALL_BLOCK_NUM-1 + T_OVERHEAD + SMALL_BLOCK_MIN)
   /* The maximum size (incl. overhead) of a small block in words
    */

#define SMALL_BLOCK_MAX_BYTES  ((SMALL_BLOCK_NUM-1 + SMALL_BLOCK_MIN) * GRANULARITY)
   /* Maximum payload size of a small block.
    */


/* The chunk sizes.
 *   SMALL_CHUNK_SIZE: size of a chunk from which small blocks
 *       are allocated. The actual allocation size is
 *       SMALL_CHUNK_SIZE+sizeof(word_t*) to account for the block list
 *       pointer.
 *   CHUNK_SIZE: size of a chunk from which large blocks are allocated.
 */

#if defined(MALLOC_SBRK) || defined (HAVE_MMAP)
#    define SMALL_CHUNK_SIZE    0x04000  /* 16 KByte */
#    define CHUNK_SIZE          0x40000
#else
    /* It seems to be advantagous to be just below a power of two
     * make sure that the resulting chunk sizes are GRANULARITY aligned.
     */
#    define SMALL_CHUNK_SIZE  \
        (0x8000 - (M_OVERHEAD+1)*GRANULARITY+GRANULARITY - EXTERN_MALLOC_OVERHEAD)
      /* large_malloc overhead ^ */

#    define CHUNK_SIZE    (0x40000 - GRANULARITY - EXTERN_MALLOC_OVERHEAD)
#endif


/* Bitflags for the size field:
 * TODO: Assumes a 32-Bit word_t.
 * TODO: define values for 64-bit word_t to support larger blocks.
 */
#define PREV_BLOCK  0x80000000  /* Previous block is allocated */
#define THIS_BLOCK  0x40000000  /* This block is allocated */
#define M_REF       0x20000000  /* Block is referenced */
#define M_GC_FREE   0x10000000  /* GC may free this block */
#define M_DEFRAG    (M_GC_FREE) /* Non-defragmentable small block */
#define M_MASK      0x0fffffff  /* Mask for the size, measured in word_t's */


#ifdef MALLOC_CHECK

/* The magic words for free and allocated small blocks.
 * Every small block size should get its own magic words, but
 * if there are more sizes than words, the words of lower sizes
 * are reused.
 *
 * There are enough words for 64 different sizes.
 */

static word_t sfmagic[]
  = { 0xde8f7d2d , 0xbd89a4b6 , 0xb667bbec , 0x475fae0a
    , 0x39fc5582 , 0x32041f46 , 0x624a92f0 , 0x16dd36e2
    , 0x7be9c1bd , 0x088aa102 , 0x3d38509b , 0x746b9fbe
    , 0x2d04417f , 0x775d4351 , 0x53c48d96 , 0x02b26e0b
    , 0x418fedcf , 0x19dbc19e , 0x78512adb , 0x1a1f5e2b
    , 0x307d7761 , 0x6584c1f0 , 0x24e3c36f , 0x2232310f
    , 0x2dac5ceb , 0x106e8b5a , 0x5a05a938 , 0x5e6392b6
    , 0x66b90348 , 0x75264901 , 0x4174f402 , 0x618b18a4
    , 0x3a6ab4bf , 0x3c4bc289 , 0x2657a9a9 , 0x4e68589b
    , 0x09648aa6 , 0x3fc489bb , 0x1c1b715c , 0x054e4c63
    , 0x484f2abd , 0x5953c1f8 , 0x79b9ec22 , 0x75536c3d
    , 0x50b10549 , 0x4d7e79b8 , 0x7805da48 , 0x1240f318
    , 0x675a3b56 , 0x70570523 , 0x2c605143 , 0x17d7b2b7
    , 0x55dbc713 , 0x514414b2 , 0x3a09e3c7 , 0x038823ff
    , 0x61b2a00c , 0x140f8cff , 0x61ebb6b5 , 0x486ba354
    , 0x2360aab8 , 0x29f6bbf8 , 0x43a08abf , 0x5fac6d41
    };

static word_t samagic[]
  = { 0x34706efd , 0xfc2a5830 , 0x4e62041a , 0x2713945e
    , 0xab58d8ab , 0xa372eeab , 0x71093c08 , 0xed2e6cc9
    , 0x504e65a2 , 0x1208e35b , 0x6910f7e7 , 0x1012ef5d
    , 0x2e2454b7 , 0x6e5f444b , 0x58621a1b , 0x077816af
    , 0x6819306d , 0x4db58658 , 0x58291bf8 , 0x3597aa25
    , 0x45bb60a0 , 0x6a6a0f11 , 0x1cf1e57c , 0x361265c4
    , 0x16ca6054 , 0x34c99833 , 0x0bee2cd7 , 0x680e7507
    , 0x6ed37bfa , 0x0f7650d6 , 0x49c11513 , 0x02e308f9
    , 0x7162078c , 0x122cb868 , 0x0c18defa , 0x14c2b244
    , 0x3c237460 , 0x4fb969b9 , 0x746f1f85 , 0x0c71da02
    , 0x61c24d14 , 0x5d80176d , 0x1c84c960 , 0x0fe6a1cc
    , 0x4bdf5bb8 , 0x74e6e37b , 0x175eb87b , 0x33f88c25
    , 0x429c69d3 , 0x6f87d474 , 0x6990364a , 0x0857ca73
    , 0x59f1e385 , 0x06821bc6 , 0x3e6a3037 , 0x70bc43d9
    , 0x3b4bb3fa , 0x4a585d0f , 0x58cab8e0 , 0x2a1f2ff4
    , 0x59ceade5 , 0x228bcdf4 , 0x2d0238ee , 0x4b30b571
    };

#define LFMAGIC 0xdfaff2ee  /* Magic word for free large blocks */
#define LAMAGIC 0xf460146e  /* Magic word for allocated large blocks */

#endif /* MALLOC_CHECK */

/*-------------------------------------------------------------------------*/
/* Debugging macros */

/* Define this macro to get a log of all allocation requests which can't be
 * immediately satisfied from a freelist. The log is written to
 * gcollect_outfd.
 */

/* #define DEBUG_MALLOC_ALLOCS */

#ifdef DEBUG_MALLOC_ALLOCS
#    define ulog(s) \
       write(gcollect_outfd, s, strlen(s))
#    define ulog1f(s,t) \
       dprintf1(gcollect_outfd, s, (p_int)(t))
#    define ulog2f(s,t1,t2) \
       dprintf2(gcollect_outfd, s, (p_int)(t1), (p_int)(t2))
#    define ulog3f(s,t1,t2,t3) \
       dprintf3(gcollect_outfd, s, (p_int)(t1), (p_int)(t2), (p_int)(t3))
#else
#    define ulog(s)             (void)0
#    define ulog1f(s,t)         (void)0
#    define ulog2f(s,t1,t2)     (void)0
#    define ulog3f(s,t1,t2,t3)  (void)0
#endif

/*-------------------------------------------------------------------------*/

static size_t smalloc_size;
  /* Size of the current smalloc() request.
   * It is used to print a diagnostic when the memory is running out.
   */

/* --- Small Block variables --- */

#define DEFAULT_INITIAL_SC_SIZE (3 * SMALL_CHUNK_SIZE)
  /* The default initial size of a small chunk. Currently the driver
   * uses somewhat over 2*SMALL_CHUNK_SIZE for it's own tables.
   */

static word_t small_chunk_size = DEFAULT_INITIAL_SC_SIZE;
  /* The size of a small chunk. Usually SMALL_CHUNK_SIZE, the first
   * small chunk of all can be allocated of min_small_malloced size;
   * the allocator will then reset the value to SMALL_CHUNK_SIZE.
   */

static word_t *last_small_chunk = NULL;
  /* Pointer the most recently allocated small chunk.
   * The first word of each chunk points to the previously allocated
   * one.
   */

static word_t *sfltable[SMALL_BLOCK_NUM+1] = {INIT_SMALL_BLOCK, 0};
  /* List of free small blocks of the various sizes.
   * The blocks are linked through the first non-header word_t.
   * The last list is special: it keeps the oversized free blocks.
   */

/* --- Large Block variables --- */

static word_t *heap_start = NULL;
  /* First address on the heap.
   */

static word_t *heap_end = NULL;
  /* Current end address of the heap.
   */

/* --- Statistics --- */

static t_stat small_alloc_stat = {0,0};
  /* Number and size of small block allocations (incl overhead).
   */

static t_stat small_free_stat  = {0,0};
  /* Number and size of small blocks deallocations (incl overhead).
   */

static t_stat small_chunk_stat = {0,0};
  /* Number and size of allocated small chunks.
   */

static t_stat small_chunk_wasted = {0,0};
  /* Number and size of wasted blocks in small chunks.
   * This includes the large-block overhead of the chunks themselves.
   */

static t_stat large_free_stat = {0,0};
  /* Number and size of free large blocks.
   */

static t_stat large_alloc_stat = {0,0};
  /* Number and size of allocated large blocks.
   */

static t_stat large_wasted_stat = {0,0};
  /* Number and size of unusable memory frags around the large blocks.
   */

static t_stat sbrk_stat;
  /* Number and size of allocated heap blocks.
   */

static t_stat perm_alloc_stat = {0,0};
  /* Number and size of permanent allocations functions (incl overhead). This
   * figure is a subset of {small,large}_alloc_stat.
   */

static long defrag_calls_total = 0;
  /* Total number of calls to defragment_small_lists().
   */

static long defrag_calls_req = 0;
  /* Number of calls to defragment_small_lists() with a desired size.
   */

static long defrag_req_success = 0;
  /* Number of times, a defragmentation for a desired size was successful.
   */

static long defrag_blocks_inspected = 0;
  /* Number of blocks inspected during defragmentations.
   */

static long defrag_blocks_merged = 0;
  /* Number of blocks merged during defragmentations.
   */

static long defrag_blocks_result = 0;
  /* Number of defragmented blocks (ie. merge results).
   */

static long malloc_increment_size_calls = 0;
  /* Number of calls to malloc_increment_size().
   */

static long malloc_increment_size_success = 0;
  /* Number of successfull calls to malloc_increment_size().
   */

static long malloc_increment_size_total = 0;
  /* Total memory allocated through to malloc_increment_size().
   */

#ifdef USE_AVL_FREELIST
static long num_avl_nodes = 0;
  /* Number of nodes in the AVL tree managing the large free blocks.
   */
#endif /* USE_AVL_FREELIST */

/*-------------------------------------------------------------------------*/
/* Forward declarations */

static char *esbrk(word_t size, size_t * pExtra) __attribute__((malloc,warn_unused_result));

static char *large_malloc(word_t size, Bool force_m) __attribute__((malloc,warn_unused_result));
#define large_malloc_int(size, force_m) large_malloc(size, force_m)
static void large_free(char *ptr);
static INLINE size_t mem_overhead(void) __attribute__((const));


#ifdef MALLOC_EXT_STATISTICS
/*=========================================================================*/

/*                       EXTENDED STATISTICS                               */

/* Extended statistics, giving a better overview about what is allocated
 * how often.
 */

/*-------------------------------------------------------------------------*/
/* --- struct extstat_s: Statistics for one block size
 */
typedef struct extstat_s {
    unsigned long max_alloc;   /* Max number of blocks allocated */
    unsigned long cur_alloc;   /* Current number of blocks allocated */
    unsigned long max_free;    /* Max number of free blocks */
    unsigned long cur_free;    /* Current number of free blocks */
    unsigned long num_xalloc;  /* Number of xalloc() requests since last avg */
    unsigned long num_xfree;   /* Number of xfree() requests since last avg */
    double        savg_xalloc; /* Sum of xalloc() requests/second */
    double        savg_xfree;  /* Sum of xfree() requests/second */
      /* On every call to mem_update_stats(), num_x{alloc,free} is averaged
       * into a number of requests/second and then added to savg_. The
       * total average is then later calculated using savg_ and the number
       * of mem_update_stats() calls - this way overflows are avoided.
       */
} extstat_t;

static unsigned long num_update_calls = 0;
  /* Number of mem_update_stats() calls.
   */

static mp_int last_update_time = 0;
  /* Timestamp of last call to mem_update_stats().
   */

#define SIZE_EXTSTATS (SMALL_BLOCK_NUM+2)

static extstat_t extstats[SIZE_EXTSTATS];
  /* The statistics array. [SMALL_BLOCK_NUM+1] is for the large blocks.
   */

/*-------------------------------------------------------------------------*/
static INLINE void
extstat_update_max (extstat_t * pStat)
{
    if (pStat->cur_alloc > pStat->max_alloc)
        pStat->max_alloc = pStat->cur_alloc;
    if (pStat->cur_free > pStat->max_free)
        pStat->max_free = pStat->cur_free;
} /* extstat_update_max() */

#endif /* MALLOC_EXT_STATISTICS */

/*=========================================================================*/

/*                       ASSOCIATED ROUTINES                               */

/*-------------------------------------------------------------------------*/
static INLINE size_t
mem_block_total_size (POINTER p)

/* Return the size of block <p> (including internal overhead) in bytes.
 */

{
    word_t *q = ((word_t *) p) - M_OVERHEAD;
    word_t size = q[M_SIZE] & M_MASK;
    if (size > SMALL_BLOCK_MAX)
       size = q[M_LSIZE];
    return size*GRANULARITY;
} /* mem_block_total_size() */

/*-------------------------------------------------------------------------*/
static INLINE size_t
mem_block_size (POINTER p)

/* Return the size of block <p> (sans internal overhead) in bytes.
 */

{
    word_t * q = ((word_t *)p) - M_OVERHEAD;

    word_t size = (q[M_SIZE] & M_MASK);
    if (size > SMALL_BLOCK_MAX)
        return mem_block_total_size(p) - ML_OVERHEAD*GRANULARITY;
    return mem_block_total_size(p) - mem_overhead();
} /* mem_block_size() */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_mark_permanent (POINTER p)

/* Mark the allocated block at <p> as permanent, ie. it won't be subject
 * to the GC.
 */

{
    word_t * q = (word_t *)p;
    if (q[-M_OVERHEAD] & M_GC_FREE)
    {
        q[-M_OVERHEAD] &= ~M_GC_FREE;
        count_up(&perm_alloc_stat, mem_block_total_size(q));
    }
} /* mem_mark_permanent() */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_mark_collectable (POINTER p)

/* Mark the allocated block at <p> as non-permant, ie. it is subject
 * to the GC.
 */

{
    word_t * q = (word_t *)p;
    if (!(q[-M_OVERHEAD] & M_GC_FREE))
    {
        q[-M_OVERHEAD] |= (M_REF|M_GC_FREE);
        count_back(&perm_alloc_stat, mem_block_total_size(q));
    }
} /* mem_mark_collectable() */

/*-------------------------------------------------------------------------*/
static INLINE size_t
mem_overhead (void)

/* Return the size of each block's overhead in bytes.
 */

{
   return M_OVERHEAD*GRANULARITY;
} /* mem_overhead() */

/*-------------------------------------------------------------------------*/
#ifdef MALLOC_EXT_STATISTICS
void
mem_update_stats (void)

/* Update whatever extended statistics the allocator has. Called every
 * backend cycle or so to allow for the calculation of averages over time.
 */

{
    int i;
    double time_diff;

    if (!last_update_time)
        last_update_time = boot_time;

    if (current_time <= last_update_time)
        return;

    time_diff = current_time - last_update_time;

    for (i = 0; i < SIZE_EXTSTATS; ++i)
    {
        extstats[i].savg_xalloc += (double)extstats[i].num_xalloc / time_diff;
        extstats[i].savg_xfree += (double)extstats[i].num_xfree / time_diff;
        extstats[i].num_xalloc = 0;
        extstats[i].num_xfree = 0;
    }

    num_update_calls++;
    last_update_time = current_time;
} /* mem_update_stats() */
#endif /* MALLOC_EXT_STATISTICS */


/*-------------------------------------------------------------------------*/
static INLINE p_int
mem_mem_allocated()
/* The amount of memory currently allocated from the allocator, including 
 * the overhead for the allocator.
 */
{
    return large_alloc_stat.size * GRANULARITY
          -small_free_stat.size
          -small_chunk_wasted.size;
}

/*-------------------------------------------------------------------------*/
static INLINE p_int
mem_mem_used()
/* The amount of memory currently used for driver data, excluding the 
 * overhead from the allocator.
 */
{
    return mem_mem_allocated()
          -large_alloc_stat.counter * ML_OVERHEAD * GRANULARITY
          -small_alloc_stat.counter * M_OVERHEAD * GRANULARITY
          -xalloc_stat.counter * XM_OVERHEAD_SIZE;
}


/*-------------------------------------------------------------------------*/
void
mem_dump_data (strbuf_t *sbuf)

/* For the status commands and functions: add the smalloc statistic
 * to the buffer <sbuf>.
 */

{
#ifdef REPLACE_MALLOC
    t_stat clib_st;
#endif
    t_stat sbrk_st, perm_st, xalloc_st;
    t_stat l_alloc, l_free, l_wasted;
    t_stat s_alloc, s_free, s_wasted, s_chunk;

    /* Get a snapshot of the statistics - strbuf_add() might do further
     * allocations while we're reading them.
     */

#ifdef MALLOC_EXT_STATISTICS
    mem_update_stats();
#endif /* MALLOC_EXT_STATISTICS */

    sbrk_st = sbrk_stat;
#ifdef REPLACE_MALLOC
    clib_st = clib_alloc_stat;
#endif
    xalloc_st = xalloc_stat;
    perm_st = perm_alloc_stat;
    l_alloc = large_alloc_stat; l_alloc.size *= GRANULARITY;
    l_free = large_free_stat; l_free.size *= GRANULARITY;
    l_wasted = large_wasted_stat;
    s_alloc = small_alloc_stat;
    s_free = small_free_stat;
    s_wasted = small_chunk_wasted;
    s_chunk = small_chunk_stat;

#   define dump_stat(str,stat) strbuf_addf(sbuf, str,stat.counter,stat.size)

    strbuf_add(sbuf, "Using LDMUD smalloc.\n");
    strbuf_add(sbuf, "Type                   Count      Space (bytes)\n");
    dump_stat("xallocs:           %8lu        %10lu\n\n", xalloc_st);
    dump_stat("sbrk requests:     %8lu        %10lu (a)\n",sbrk_st);
    dump_stat("large blocks:      %8lu        %10lu (b)\n",l_alloc);
    strbuf_addf(sbuf
               , "large net avail:                   %10ld\n"
               , l_alloc.size - l_alloc.counter * ML_OVERHEAD * GRANULARITY
               );
    dump_stat("large free blocks: %8lu        %10lu (c)\n",l_free);
    dump_stat("large wasted:      %8lu        %10lu (d)\n\n",l_wasted);
    dump_stat("small chunks:      %8lu        %10lu (e)\n",s_chunk);
    dump_stat("small blocks:      %8lu        %10lu (f)\n",s_alloc);
    strbuf_addf(sbuf
               , "small net avail:                   %10lu\n"
               , s_alloc.size - s_alloc.counter * M_OVERHEAD * GRANULARITY
               );
    dump_stat("small free blocks: %8lu        %10lu (g)\n",s_free);
    dump_stat("small wasted:      %8lu        %10lu (h)\n\n",s_wasted);

    dump_stat("permanent blocks:  %8lu        %10lu\n", perm_st);
#ifdef REPLACE_MALLOC
    dump_stat("clib allocations:  %8lu        %10lu\n", clib_st);
#else
    strbuf_addf(sbuf, "clib allocations:       n/a               n/a\n");
#endif
    strbuf_add(sbuf, "\n");
#ifdef USE_AVL_FREELIST
    strbuf_addf(sbuf, "AVL nodes:         %8ld                 -\n", num_avl_nodes);
    strbuf_add(sbuf, "\n");
#endif /* USE_AVL_FREELIST */

#undef dump_stat
    
    strbuf_addf(sbuf,
      "malloc_increment_size: calls %lu success %lu total %lu\n\n",
      malloc_increment_size_calls,
      malloc_increment_size_success,
      malloc_increment_size_total
    );
    strbuf_addf(sbuf
               , "Total storage:        (b+c+d)     %10lu should equal (a) %10lu\n"
               , l_alloc.size + l_free.size + l_wasted.size
               , sbrk_st.size
               );
    strbuf_addf(sbuf
               , "Total small storage:  (f+g+h)     %10lu should equal (e) %10lu\n"
               , s_alloc.size + s_free.size + s_wasted.size
               , s_chunk.size
               );
    strbuf_addf(sbuf
               , "Total storage in use: (b-g-h)     %10lu net available:   %10lu\n"
               , mem_mem_allocated()
               , mem_mem_used()
               );
    strbuf_addf(sbuf
               , "Total storage unused: (c+d+g+h)   %10lu\n\n"
               , l_free.size + l_wasted.size
                 + s_free.size + s_wasted.size
               );
    strbuf_addf(sbuf,
                "soft memory limit: %10"PRIdMPINT", hard memory limit: %10"PRIdMPINT"\n\n",
                get_memory_limit(MALLOC_SOFT_LIMIT),
                get_memory_limit(MALLOC_HARD_LIMIT)
               );

    strbuf_addf(sbuf,
      "Defragmentation: %lu calls (%lu for size: %lu successful)\n"
      "                 %lu blocks inspected: %lu merged yielding %lu blocks\n"
    , defrag_calls_total, defrag_calls_req, defrag_req_success
    , defrag_blocks_inspected, defrag_blocks_merged, defrag_blocks_result
               );
} /* mem_dump_data() */

/*-------------------------------------------------------------------------*/
void
mem_dump_extdata (strbuf_t *sbuf)

/* For the status commands and functions: add the extended smalloc statistic
 * to the buffer <sbuf>.
 */

{
#ifdef MALLOC_EXT_STATISTICS
    int i;

    strbuf_add(sbuf,
      "Detailed Block Statistics:\n\n"
              );
    for (i = 0; i < SIZE_EXTSTATS; ++i)
    {
        if (i < SMALL_BLOCK_NUM)
            strbuf_addf(sbuf, "  Size %3zu: ", (i + SMALL_BLOCK_MIN) * GRANULARITY);
        else if (i == SMALL_BLOCK_NUM)
            strbuf_addf(sbuf, "  Oversize: ");
        else
            strbuf_addf(sbuf, "  Large:    ");
        strbuf_addf(sbuf, "Alloc: %7.1lf /s - %7lu / %7lu  cur/max\n"
                        , num_update_calls ? extstats[i].savg_xalloc / num_update_calls
                                           : 0.0
                        , extstats[i].cur_alloc, extstats[i].max_alloc
                   );
        strbuf_addf(sbuf, "            "
                          "Free:  %7.1lf /s - %7lu / %7lu  cur/max\n"
                        , num_update_calls ? extstats[i].savg_xfree / num_update_calls
                                           : 0.0
                        , extstats[i].cur_free, extstats[i].max_free
                   );
    }
#else
    strbuf_add(sbuf, "No detailed blocks statistics available.\n");
#endif /* MALLOC_EXT_STATISTICS */
} /* mem_dump_extdata() */

/*-------------------------------------------------------------------------*/
void
mem_dinfo_data (svalue_t *svp, int value)

/* Fill in the data for debug_info(DINFO_DATA, DID_MEMORY) into the
 * svalue-block svp.
 */

{
#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

    if (value == -1)
        put_ref_string(svp+DID_MEM_NAME, STR_SMALLOC);
    else if (value == DID_MEM_NAME)
        put_ref_string(svp, STR_SMALLOC);

    ST_NUMBER(DID_MEM_SBRK, sbrk_stat.counter);
    ST_NUMBER(DID_MEM_SBRK_SIZE, sbrk_stat.size);
    ST_NUMBER(DID_MEM_LARGE, large_alloc_stat.counter);
    ST_NUMBER(DID_MEM_LARGE_SIZE, large_alloc_stat.size * GRANULARITY);
    ST_NUMBER(DID_MEM_LFREE, large_free_stat.counter);
    ST_NUMBER(DID_MEM_LFREE_SIZE, large_free_stat.size * GRANULARITY);
    ST_NUMBER(DID_MEM_LWASTED, large_wasted_stat.counter);
    ST_NUMBER(DID_MEM_LWASTED_SIZE, large_wasted_stat.size);
    ST_NUMBER(DID_MEM_CHUNK, small_chunk_stat.counter);
    ST_NUMBER(DID_MEM_CHUNK_SIZE, small_chunk_stat.size);
    ST_NUMBER(DID_MEM_SMALL, small_alloc_stat.counter);
    ST_NUMBER(DID_MEM_SMALL_SIZE, small_alloc_stat.size);
    ST_NUMBER(DID_MEM_SFREE, small_free_stat.counter);
    ST_NUMBER(DID_MEM_SFREE_SIZE, small_free_stat.size);
    ST_NUMBER(DID_MEM_SWASTED, small_chunk_wasted.counter);
    ST_NUMBER(DID_MEM_SWASTED_SIZE, small_chunk_wasted.size);
    ST_NUMBER(DID_MEM_MINC_CALLS, malloc_increment_size_calls);
    ST_NUMBER(DID_MEM_MINC_SUCCESS, malloc_increment_size_success);
    ST_NUMBER(DID_MEM_MINC_SIZE, malloc_increment_size_total);
#ifdef REPLACE_MALLOC
    ST_NUMBER(DID_MEM_CLIB, clib_alloc_stat.counter);
    ST_NUMBER(DID_MEM_CLIB_SIZE, clib_alloc_stat.size);
#endif
    ST_NUMBER(DID_MEM_PERM, perm_alloc_stat.counter);
    ST_NUMBER(DID_MEM_PERM_SIZE, perm_alloc_stat.size);
    ST_NUMBER(DID_MEM_OVERHEAD, T_OVERHEAD * GRANULARITY);
    ST_NUMBER(DID_MEM_ALLOCATED, mem_mem_allocated());
    ST_NUMBER(DID_MEM_USED, mem_mem_used());
    ST_NUMBER(DID_MEM_TOTAL_UNUSED, large_free_stat.size * GRANULARITY
                                    + large_wasted_stat.size
                                    + small_free_stat.size
                                    + small_chunk_wasted.size
             );
    ST_NUMBER(DID_MEM_DEFRAG_CALLS, defrag_calls_total);
    ST_NUMBER(DID_MEM_DEFRAG_CALLS_REQ, defrag_calls_req);
    ST_NUMBER(DID_MEM_DEFRAG_REQ_SUCCESS, defrag_req_success);
    ST_NUMBER(DID_MEM_DEFRAG_BLOCKS_INSPECTED, defrag_blocks_inspected);
    ST_NUMBER(DID_MEM_DEFRAG_BLOCKS_MERGED, defrag_blocks_merged);
    ST_NUMBER(DID_MEM_DEFRAG_BLOCKS_RESULT, defrag_blocks_result);
#ifdef USE_AVL_FREELIST
    ST_NUMBER(DID_MEM_AVL_NODES, num_avl_nodes);
#else
    ST_NUMBER(DID_MEM_AVL_NODES, large_free_stat.counter);
#endif /* USE_AVL_FREELIST */
#ifdef MALLOC_EXT_STATISTICS
    do {
        vector_t * top; /* Array holding the sub vectors */
        int i;
        Bool deallocate = MY_FALSE;

        mem_update_stats();
        top = allocate_array(SMALL_BLOCK_NUM+2);
        
        if (!top)
            break;

        for (i = 0; i < SIZE_EXTSTATS; ++i)
        {
            vector_t *sub = allocate_array(DID_MEM_ES_MAX);

            if (!sub)
            {
                deallocate = MY_TRUE;
                break;
            }

            put_number(&sub->item[DID_MEM_ES_MAX_ALLOC], extstats[i].max_alloc);
            put_number(&sub->item[DID_MEM_ES_CUR_ALLOC], extstats[i].cur_alloc);
            put_number(&sub->item[DID_MEM_ES_MAX_FREE], extstats[i].max_free);
            put_number(&sub->item[DID_MEM_ES_CUR_FREE], extstats[i].cur_free);
            if (num_update_calls)
            {
                put_float(&sub->item[DID_MEM_ES_AVG_XALLOC], extstats[i].savg_xalloc / num_update_calls);
                put_float(&sub->item[DID_MEM_ES_AVG_XFREE], extstats[i].savg_xfree / num_update_calls);
            }

            put_array(top->item + i, sub);
        }

        if (deallocate)
        {
            free_array(top);
            ST_NUMBER(DID_MEM_EXT_STATISTICS, 0);
        }
        else
        {
            if (value == -1)
                put_array(svp+DID_MEM_EXT_STATISTICS, top);
            else if (value == DID_MEM_EXT_STATISTICS)
                put_array(svp, top);
        }
    } while(0);
#else
    ST_NUMBER(DID_MEM_EXT_STATISTICS, 0);
#endif /* MALLOC_EXT_STATISTICS */

#undef ST_NUMBER
} /* mem_dinfo_data() */

/*=========================================================================*/

/*                            SMALL BLOCKS                                 */

/*-------------------------------------------------------------------------*/

#define SIZE_INDEX(size) \
      ((size)/GRANULARITY - T_OVERHEAD - SMALL_BLOCK_MIN)
    /* Index to the proper array entry for a small
     * block of <size> (including overhead).
     */

#define SIZE_MOD_INDEX(size, table) \
      (((size)/GRANULARITY - T_OVERHEAD - SMALL_BLOCK_MIN) % NELEM(table))
    /* Index to the proper array entry for a small
     * block of <size> (including overhead), limited to the size of <table>.
     */

/*-------------------------------------------------------------------------*/
static INLINE void
UNLINK_SMALL_FREE (word_t * block)

/* Unlink the small free <block> from its freelist
 */

{
    const word_t bsize = block[M_SIZE] & M_MASK;
    int ix;
    word_t flag;

    if (bsize <= SMALL_BLOCK_MAX)
    {
        ix = bsize - T_OVERHEAD - SMALL_BLOCK_MIN;
        flag = 0;
    }
    else
    {
        ix = SMALL_BLOCK_NUM;
        flag = 1;
    }

#ifdef MALLOC_EXT_STATISTICS
    extstats[ix].cur_free--;
#endif /* MALLOC_EXT_STATISTICS */

    if (sfltable[ix] == block)
    {
        word_t * head = BLOCK_NEXT(block);
        if (head)
            head[M_PLINK(head[M_SIZE] & M_MASK)] = (word_t)head | flag;
        sfltable[ix] = head;
    }
    else
    {
        word_t * prev = (word_t *)(block[M_PLINK(bsize)] & ~1);
        word_t * next = BLOCK_NEXT(block);
        if (next)
            next[M_PLINK(next[M_SIZE] & M_MASK)] = (word_t)prev | flag;
        prev[M_LINK] = (word_t) next;
    }
    count_back(&small_free_stat, bsize * GRANULARITY);

} /* UNLINK_SMALL_FREE() */

/*-------------------------------------------------------------------------*/
static INLINE
void MAKE_SMALL_FREE (word_t *block, word_t bsize)

/* The <bsize> words starting at <block> are a new free small block.
 * Set it up and insert it into the appropriate free list.
 */

{
    word_t * head;
    int ix;
    word_t flag;

    if (bsize <= SMALL_BLOCK_MAX)
    {
        ix = bsize - T_OVERHEAD - SMALL_BLOCK_MIN;
        flag = 0;
    }
    else
    {
        ix = SMALL_BLOCK_NUM;
        flag = 1;
    }

#ifdef MALLOC_EXT_STATISTICS
    extstats[ix].cur_free++;
    extstat_update_max(extstats + ix);
#endif /* MALLOC_EXT_STATISTICS */

    block[M_SIZE] = bsize | (THIS_BLOCK|M_REF)
                          | (block[M_SIZE] & PREV_BLOCK);
    block[bsize] |= PREV_BLOCK;

    head = sfltable[ix];
    if (head)
        head[M_PLINK(head[M_SIZE] & M_MASK)] = (word_t)block | flag;
    block[M_LINK] = (word_t) head;
    block[M_PLINK(bsize)] = (word_t)block | flag;
      /* Let the PLINK point to block itself, to satisfy sanity checks */

    if (flag)
        block[M_PLINK(bsize)-1] = bsize;

    sfltable[ix] = block;
    count_up(&small_free_stat, bsize * GRANULARITY);

#ifdef MALLOC_CHECK
    block[M_MAGIC] = sfmagic[SIZE_MOD_INDEX(bsize * GRANULARITY, sfmagic)];
#endif
} /* MAKE_SMALL_FREE() */

/*-------------------------------------------------------------------------*/
static Bool
defragment_small_lists (int req)

/* Defragment some or all of the small block free lists.
 *
 * req = 0: defragment all of the free lists; result is MY_FALSE.
 * req > 0: the routine is called from the allocator for a small block
 *          allocation for <req> words incl overhead. If the defragmentation
 *          creates a block of a suitable size, the defragmentation
 *          terminates at that point and the function returns MY_TRUE.  If
 *          all blocks are defragmented and no suitable block is found, the
 *          function returns MY_FALSE.
 */

{
    word_t split_size = 0;
      /* Minimum blocksize for a block splittable into the requested
       * size, incl. overhead in words; 0 for 'none'
       */
    int ix;  /* Freelist index */
    Bool found = MY_FALSE; /* Set to TRUE if a suitable block is found */

    defrag_calls_total++;

    if (req > 0)
    {
        split_size = req + SMALL_BLOCK_MIN + T_OVERHEAD;
        defrag_calls_req++;
    }
    else
        req = 0; /* Just to make sure */

    /* Loop over the freelists, starting at the largest.
     */
    for (ix = SMALL_BLOCK_NUM; ix >= 0 && !found; ix--)
    {
        word_t *list = NULL;
          /* Local list of the defragmented blocks, linked through [M_LINK].
           */
        word_t *block;
        word_t *pred;
          /* <block> is the block currently analysed, <pred> the previously
           * analyzed block or NULL for the list header.
           */

        /* Walk the current freelist and look for defragmentable blocks.
         * If one is found, remove it from the freelist, defragment it
         * and store the result in the local list.
         * Since the check for defragmentation occurs on both sides of
         * the free block, the defragmentation will never remove blocks from
         * this freelist which have already been visited.
         *
         * It is important that the loop is not terminated before the end
         * of the section of newly freed blocks (M_DEFRAG not set) is reached,
         * as the loop sets the M_DEFRAG flag as it goes along.
         */
        for (block = sfltable[ix], pred = NULL
            ; block != NULL && !(block[M_SIZE] & M_DEFRAG)
            ; )
        {
            Bool   merged;
            word_t bsize = block[M_SIZE] & M_MASK;

            defrag_blocks_inspected++;

            /* Can this block be defragmented? */
            if ( !((block+bsize)[M_SIZE] & THIS_BLOCK)
              && !(block[M_SIZE] & PREV_BLOCK)
               )
            {
                /* No: flag this block as non-defragmentable and step to the
                 * next block.
                 */
                block[M_SIZE] |= M_DEFRAG;
                pred = block;
                block = (word_t *)block[M_LINK];
                continue;
            }

            /* Yes: remove it from the freelist */
            UNLINK_SMALL_FREE(block);

            /* Try to merge this free block with the following ones */
            do {
                word_t *next;

                defrag_blocks_inspected++;

                merged = MY_FALSE;
                next = block + bsize;
                if (next[M_SIZE] & THIS_BLOCK)
                {
                    UNLINK_SMALL_FREE(next);
                    bsize += next[M_SIZE] & M_MASK;
                    merged = MY_TRUE;

                    defrag_blocks_merged++;
                }

                if (req > 0 && (bsize == (word_t)req || bsize >= split_size))
                    found = MY_TRUE;

            } while (merged);

            /* Try to merge the block with the ones in front of it */
            while ((block[M_SIZE] & PREV_BLOCK))
            {
                word_t *prev;

                defrag_blocks_inspected++;

                /* We use the 'prev' pointer first to get the size of previous
                 * block, and from there we can determine it's address
                 */
                prev = (word_t *)(block[-1]);
                if ((word_t)prev & 1)
                    prev = block - block[-2];
                else
                    prev = block - (prev[M_SIZE] & M_MASK);

#ifdef DEBUG
                /* TODO: Remove this test once it's been proven stable */
                if (!(prev[M_SIZE] & THIS_BLOCK))
                {
                    in_malloc = 0;
                    fatal("Block %p marks previous %p as free, but it isn't.\n"
                         , block, prev);
                }
#endif
                UNLINK_SMALL_FREE(prev);
                bsize += prev[M_SIZE] & M_MASK;

                block = prev;

                defrag_blocks_merged++;

                if (req >= 0 && (bsize == (word_t)req || bsize >= split_size))
                    found = MY_TRUE;
            } /* while() */

            /* Update the block's size and move it into the local list.
             * Be careful not to clobber the flags in the size field.
             */
            block[M_SIZE] = bsize | (block[M_SIZE] & ~M_MASK);
            block[M_LINK] = (word_t)list;
            list = block;

            defrag_blocks_result++;

            /* Step to the next block using the still-value <pred> */
            if (pred)
                block = (word_t *)pred[M_LINK];
            else
                block = sfltable[ix];
        } /* for (blocks in freelist) */

        /* Move the defragmented blocks from list back into their freelist.
         */
        while (list != NULL)
        {
            block = list;
            list = (word_t *)(list[M_LINK]);
            MAKE_SMALL_FREE(block, block[M_SIZE] & M_MASK);
              /* As we moved down the small block array, and the defragged
               * blocks already visited have been sorted into a list, setting
               * the M_DEFRAG flag would be possible here.
               * However, by not setting it we make the algorithm a bit
               * more robust, and lose only a few cycles the next time
               * around.
               */
        }
    } /* for (ix = SMALL_BLOCK_NUM..0 && !found) */

    if (found)
        defrag_req_success++;

    return found;
} /* defragment_small_lists() */

/*-------------------------------------------------------------------------*/
static INLINE void
defragment_small_block (word_t *block)

/* Try to merge the small free block <block> with any following free blocks.
 * This routine is used by mem_increment_size().
 */

{
    word_t  bsize = block[M_SIZE] & M_MASK;
    Bool merged;
    Bool defragged;

    defragged = MY_FALSE;

    UNLINK_SMALL_FREE(block);

    /* Try to merge this free block with the following ones */
    do {
        word_t *next;

        defrag_blocks_inspected++;

        merged = MY_FALSE;
        next = block + bsize;
        if (next[M_SIZE] & THIS_BLOCK)
        {
            UNLINK_SMALL_FREE(next);
            bsize += next[M_SIZE] & M_MASK;
            merged = MY_TRUE;
            defragged = MY_TRUE;

            defrag_blocks_inspected++;
        }
    } while (merged);

    if (defragged)
        defrag_blocks_result++;

    /* Reinsert the block into the freelists */
    MAKE_SMALL_FREE(block, bsize);
} /* defragment_small_block() */

/*-------------------------------------------------------------------------*/
/* Macro MAKE_SMALL_CHECK(block, size)
 * Macro MAKE_SMALL_CHECK_UNCHECKED(block, size)
 * If MALLOC_CHECK is defined, fill in the CHECK information
 * in the small block <block> of size <size> (in bytes incl overhead).
 * The _UNCHECKED macro is like the basic macro, except that it doesn't
 * check the M_MAGIC word before setting it.
 */
#ifdef MALLOC_CHECK
#  define MAKE_SMALL_CHECK(block, size) do { \
        if (block[M_MAGIC] != sfmagic[SIZE_MOD_INDEX(size, sfmagic)] ) \
        { \
            in_malloc = 0; \
            fatal("allocation from free list for %zu bytes: " \
                  "block %p (user %p) magic match failed, " \
                  "expected %08"PRIxPTR", found %08"PRIxPTR"\n" \
                 , size, block, block+T_OVERHEAD \
                 , (intptr_t)sfmagic[SIZE_MOD_INDEX(size, sfmagic)] \
                 , (intptr_t)block[M_MAGIC]); \
        } \
        block[M_MAGIC] = samagic[SIZE_MOD_INDEX(size, samagic)]; \
      } while(0)
#  define MAKE_SMALL_CHECK_UNCHECKED(block, size) do { \
        block[M_MAGIC] = samagic[SIZE_MOD_INDEX(size, samagic)]; \
      } while(0)
#else
#  define MAKE_SMALL_CHECK(block, size) (void)0
#  define MAKE_SMALL_CHECK_UNCHECKED(block, size) (void)0
#endif

/*-------------------------------------------------------------------------*/
static POINTER
mem_alloc (size_t size)

/* Allocate a memory block for <size> bytes at the source <file>:<line>.
 * Result is the pointer the memory block, or NULL when out of memory.
 */

{
    word_t *temp;
    Bool    retry;
#if defined(HAVE_MADVISE) || defined(DEBUG_MALLOC_ALLOCS)
    size_t orig_size = size;
#endif

    assert_stack_gap();

    smalloc_size = size;

    if (size == 0)
    {
        size++;
    }

    /* TODO: For the following test, see SIZET_limits in port.h */
    if (size >= ULONG_MAX - (T_OVERHEAD+SMALL_BLOCK_MIN)*GRANULARITY
     || size >= (M_MASK + 1 - (T_OVERHEAD+SMALL_BLOCK_MIN))*GRANULARITY
       )
    {
        in_malloc = 0;
        if (malloc_privilege == MALLOC_SYSTEM)
            fatal("Malloc size exceeds numerical limit.\n");
        debug_message("Malloc size exceeds numerical limit.\n");
        return NULL;
    }

    if (in_malloc++)
    {
        in_malloc = 0;
        writes(1,"Multiple threads in smalloc()\n");
        fatal("Multiple threads in smalloc()\n");
        /* NOTREACHED */
        return NULL;
    }

    if (size > SMALL_BLOCK_MAX_BYTES + XM_OVERHEAD_SIZE)
    {
        void * rc = large_malloc(size, MY_FALSE);
        in_malloc--;
        return rc;
    }

    /* --- It's a small block --- */

    /* Get the block size rounded to the next multiple of a word
     * and with the overhead.
     */
    if (size < SMALL_BLOCK_MIN_BYTES + XM_OVERHEAD_SIZE)
        size = SMALL_BLOCK_MIN_BYTES + XM_OVERHEAD_SIZE;

    size = (size+M_OVERHEAD*GRANULARITY+GRANULARITY-1) & ~(GRANULARITY-1);

    /* Update statistics */
    count_up(&small_alloc_stat,size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[SIZE_INDEX(size)].num_xalloc++;
    extstats[SIZE_INDEX(size)].cur_alloc++;
    extstat_update_max(extstats + SIZE_INDEX(size));
#endif /* MALLOC_EXT_STATISTICS */


    /* Try allocating the block from one of the free lists.
     *
     * This is done in a loop: if at the first attempt no block can be found,
     * the small block lists will be defragmented and the allocation attempt
     * is repeated. If that fails as well, the loop ends and we try to
     * get a new small chunk.
     */

    retry = MY_FALSE;
    do {
        int ix;

        /* First, check if the free list for this block size has one */

        if ( NULL != (temp = sfltable[SIZE_INDEX(size)]))
        {
            /* allocate from the free list */

            UNLINK_SMALL_FREE(temp);

            /* Fill in the header (M_SIZE is already mostly ok) */
            MAKE_SMALL_CHECK(temp, size);
            temp[M_SIZE] |= (M_GC_FREE|M_REF);
            temp[M_SIZE] &= ~THIS_BLOCK;
            temp[temp[M_SIZE] & M_MASK] &= ~PREV_BLOCK;

            temp += M_OVERHEAD;

            MADVISE(temp, orig_size);

            in_malloc--;
            return (POINTER)temp;
        }

        /* There is nothing suitable in the normal free list - next try
         * allocating from the oversized block list.
         */
        {
            word_t *this;
            word_t wsize = size / GRANULARITY; /* size incl overhead in words */

            for (this = sfltable[SMALL_BLOCK_NUM] ; this; this = BLOCK_NEXT(this))
            {
                word_t bsize = *this & M_MASK;
                word_t rsize = bsize - wsize;

                /* Make sure that the split leaves a legal block behind */
                if (bsize < wsize + T_OVERHEAD + SMALL_BLOCK_SPLIT_MIN)
                    continue;

                /* If the split leaves behind a normally sized small
                 * block, move it over to the appropriate free list.
                 * Otherwise, just update the size and magic header fields
                 * but keep this block in the oversized list.
                 */
                if (rsize <= SMALL_BLOCK_MAX)
                {
                    /* Unlink it from this list */
                    UNLINK_SMALL_FREE(this);

                    /* Put it into the real free list */
                    MAKE_SMALL_FREE(this, rsize);
                }
                else
                {
                    /* Just modify the size and move the .prev pointer
                     * and size field.
                     */
                    count_back(&small_free_stat, bsize * GRANULARITY);

                    this[M_SIZE] &= (PREV_BLOCK|M_DEFRAG);
                    this[M_SIZE] |= rsize | (THIS_BLOCK|M_REF);
                    this[M_PLINK(rsize)] = this[M_PLINK(bsize)];
                    this[M_PLINK(rsize)-1] =  rsize;

#ifdef MALLOC_CHECK
                    this[M_MAGIC] = sfmagic[SIZE_MOD_INDEX(rsize*GRANULARITY, sfmagic)];
#endif

                    count_up(&small_free_stat, rsize * GRANULARITY);
                }

                /* Split off the allocated small block from the end
                 * The front remains in the freelist.
                 */
                this += rsize;

                /* Fill in the header */
                this[M_SIZE] = wsize | (PREV_BLOCK|M_GC_FREE|M_REF);
                this[wsize] &= ~PREV_BLOCK;
                MAKE_SMALL_CHECK_UNCHECKED(this,size);

                this += M_OVERHEAD;

                MADVISE(this, orig_size);

#ifdef DEBUG_MALLOC_ALLOCS
                ulog2f("smalloc(%d / %d): Split oversized block "
                      , orig_size, size);
                dprintf2( gcollect_outfd, "(%d / %d bytes): left with block of "
                        , (p_int)(bsize - T_OVERHEAD) * GRANULARITY, (p_int)bsize * GRANULARITY);
                dprintf2( gcollect_outfd, "%d / %d bytes.\n"
                        , (p_int)(rsize - T_OVERHEAD) * GRANULARITY
                        , (p_int)rsize * GRANULARITY);
#endif

                in_malloc--;
                return (POINTER)this;
            }
        } /* allocation from oversized lists */

        /* Next, try splitting off the memory from one of the larger
         * listed free small blocks.
         * Search from the largest blocks, and stop when splits
         * would result in too small blocks.
         */
        for ( ix = SMALL_BLOCK_NUM-1
            ; ix >= (int)(SIZE_INDEX(size) + T_OVERHEAD + SMALL_BLOCK_SPLIT_MIN) 
            ; ix--
            )
        {
            word_t *pt, *split;
            size_t wsize, usize;

            if (!sfltable[ix]) /* No block available */
                continue;

            wsize = size / GRANULARITY; /* size incl. overhead in words */

            /* Remove the block from the free list */
            pt = sfltable[ix];
            UNLINK_SMALL_FREE(pt);

            /* Split off the unused part as new block */
            split = pt + wsize;
            usize = ix + T_OVERHEAD + SMALL_BLOCK_MIN - wsize;
            split[M_SIZE] = 0; /* No PREV_BLOCK */
            MAKE_SMALL_FREE(split, usize);

            /* Initialize the header of the new block */
            pt[M_SIZE] &= PREV_BLOCK;
            pt[M_SIZE] |= wsize | (M_GC_FREE|M_REF);
            MAKE_SMALL_CHECK_UNCHECKED(pt, size);

            pt += M_OVERHEAD;

            MADVISE(pt, orig_size);

#ifdef DEBUG_MALLOC_ALLOCS
            ulog2f("smalloc(%d / %d): Split block "
                  , orig_size, size);
            dprintf2( gcollect_outfd, "(%d / %d bytes): left with block of "
                    , (p_int)(ix - T_OVERHEAD) * GRANULARITY, (p_int)ix * GRANULARITY);
            dprintf2( gcollect_outfd, "%d / %d bytes.\n"
                    , (p_int)(usize - T_OVERHEAD) * GRANULARITY, (p_int)usize * GRANULARITY);
#endif

            in_malloc--;
            return (POINTER)pt;
        } /* allocation from larger blocks */

        /* At this point, there was nothing in the free lists.
         * If this is the first pass, defragment the memory and try again
         * (the defragmentation routine returns whether a retry is useful).
         * If this is the second pass, force retry to be FALSE, thus
         * terminating the loop.
         */
        if (!retry)
            retry = defragment_small_lists(size / GRANULARITY);
        else
            retry = MY_FALSE;
    } while (retry);

    /* At this point, we couldn't find a suitable block in the free lists,
     * thus allocate a new small chunk.
     * If this is the first small chunk of all, or if the small_chunk_size
     * has been modified from the default (happens for the second small
     * chunk allocation), try to get fresh memory from the system.
     *
     * Note: Changes here must be mirrored in mem_consolidate().
     */
    {
        word_t * new_chunk;
        word_t   chunk_size;

        new_chunk = (word_t*)large_malloc_int(small_chunk_size + sizeof(word_t*)
                                             ,    (last_small_chunk == NULL)
                                              || (small_chunk_size != SMALL_CHUNK_SIZE)
                                             );

        /* If this is the first small chunk allocation, it might fail because
         * the driver was configured with a too big min_small_malloced value.
         * If that happens, try again with the normal value.
         */
        if (new_chunk == NULL && small_chunk_size != SMALL_CHUNK_SIZE)
        {
            dprintf1(2, "Low on MEMORY: Failed to allocated MIN_SMALL_MALLOCED "
                       "block of %d bytes.\n"
                     , (p_int)(small_chunk_size)
                   );
            small_chunk_size = SMALL_CHUNK_SIZE;
            new_chunk = (word_t*)large_malloc_int(small_chunk_size+sizeof(word_t*)
                                                 , MY_TRUE);
        }

        if (new_chunk == NULL)
        {
            dprintf1(2, "Low on MEMORY: Failed to allocated small chunk "
                        "block of %d bytes.\n"
                      , (p_int)(small_chunk_size)
                    );
            in_malloc--;
            return NULL;
        }

        /* Enter the chunk into the chunk list and reset the small_cunk_size
         * to the ongoing value.
         */

        chunk_size = new_chunk[-ML_OVERHEAD] - ML_OVERHEAD - 2;
          /* The payload size of the new chunk in words (ie. sans
           * large block overhead, chunk link pointer and final sentinel block).
           */

        *new_chunk = (word_t)last_small_chunk;
        last_small_chunk = new_chunk++;

        count_up(&small_chunk_stat, new_chunk[-ML_OVERHEAD-1] * GRANULARITY);
        count_up(&small_chunk_wasted, GRANULARITY*(M_OVERHEAD+1));

        small_chunk_size = SMALL_CHUNK_SIZE;

        /* The last word in the chunk is a fake permanently allocated small
         * block, so that the block merging routine has something to put the
         * PREV_BLOCK flag into.
         */
        new_chunk[chunk_size] = 1 | M_REF;

        /* Use the end of the new chunk before the sentinel block to satisfy
         * the allocation and enter the rest at the front as free block into
         * the oversized free list.
         */
        chunk_size -= size / GRANULARITY;
        new_chunk[M_SIZE] = 0;
        MAKE_SMALL_FREE(new_chunk, chunk_size);

        temp = new_chunk + chunk_size;

        temp[M_SIZE] = (size / GRANULARITY) | (PREV_BLOCK|M_GC_FREE|M_REF);
        MAKE_SMALL_CHECK_UNCHECKED(temp, size);

        temp += M_OVERHEAD;

        MADVISE(temp, orig_size);
        in_malloc--;
        return (POINTER)temp;
    } /* allocate from a new small chunk */
} /* mem_alloc() */

/*-------------------------------------------------------------------------*/
static void
sfree (POINTER ptr)

/* Return the memoryblock <ptr> to the allocator.
 * This function does the actual freeing, without checking for mutexes
 * and stack gaps; it is also used internally by the allocator to free
 * the memory reserves.
 */

{
    word_t *block;
    word_t bsize, i;

    if (!ptr)
        return;

    mem_mark_collectable(ptr);

    /* Get the real block address and size */
    block = (word_t *) ptr;
    block -= M_OVERHEAD;
    bsize = i = block[M_SIZE] & M_MASK;

    if (bsize > SMALL_BLOCK_MAX)
    {
        /* It's a big block */
        large_free(ptr);
        return;
    }

    /* It's a small block: put it back into the free list */

    count_back(&small_alloc_stat, bsize * GRANULARITY);
    i -=  SMALL_BLOCK_MIN + T_OVERHEAD;

#ifdef MALLOC_EXT_STATISTICS
    extstats[i].num_xfree++;
    extstats[i].cur_alloc--;
#endif /* MALLOC_EXT_STATISTICS */

#ifdef MALLOC_CHECK
    if (block[M_MAGIC] == sfmagic[i % NELEM(sfmagic)])
    {
        in_malloc = 0;
        fatal("mem_free: block %lx size %lu (user %lx) freed twice\n"
             , (unsigned long)block, (unsigned long)(i * GRANULARITY)
             , (unsigned long)ptr);
    }
    if (block[M_MAGIC] != samagic[i % NELEM(samagic)])
    {
        in_malloc = 0;
        fatal("mem_free: block %p magic match failed: "
              "size %lu, expected %jx, found %jx\n"
             , block, (unsigned long)(i * GRANULARITY),
             (intmax_t)samagic[i], (intmax_t)block[M_MAGIC]);
    }
#endif

    MAKE_SMALL_FREE(block, bsize);
} /* sfree() */

/*-------------------------------------------------------------------------*/
static void
mem_free (POINTER ptr)

/* Return the memoryblock <ptr> to the allocator.
 * This is a wrapper for sfree() which performs checks for stack-gaps
 * and such.
 */

{
    if (!ptr)
        return;

    assert_stack_gap();

    if (in_malloc++)
    {
        in_malloc = 0;
        writes(1, "Multiple threads in mem_free()\n");
        fatal("Multiple threads in mem_free()\n");
        /* NOTREACHED */
        return;
    }

    sfree(ptr);
    in_malloc--;
} /* mem_free() */

/*-------------------------------------------------------------------------*/
static POINTER
mem_realloc (POINTER p, size_t size)

/* Reallocate block <p> to the new size of <size> and return the pointer.
 * The memory is not aligned and subject to GC.
 */

{
    POINTER t;
    size_t old_size;

    old_size = mem_block_size(p);
    if (old_size >= size)
        return p;

    t = mem_alloc(size);
    if (t == NULL)
        return NULL;

    memcpy(t, p, old_size);
    mem_free(p);
    return t;
} /* mem_realloc() */

/*=========================================================================*/

/*                            LARGE BLOCKS                                 */

/*-------------------------------------------------------------------------*/

#define l_next_block(p)  ((p) + (p)[M_LSIZE] )
#define l_prev_block(p)  ((p) - (*((p)-2)) )
  /* Address the previous resp. this block.
   */

#define l_prev_free(p)   (!(*(p) & PREV_BLOCK))
#define l_next_free(p)   (!(*l_next_block(p) & THIS_BLOCK))
  /* Check if the previous resp. the next block is free.
   */

/*-------------------------------------------------------------------------*/

/* Extra types and definitions for the AVL routines */

#if defined (sun) || defined(__linux__)
    /* there is a type signed char */
    typedef signed char balance_t;
#   define BALANCE_T_BITS 8
#else
    typedef short balance_t;
#   define BALANCE_T_BITS 16
#endif
#if defined(sparc)
    /* try to avoid multiple shifts, because these are costly */
#   define NO_BARREL_SHIFT
#endif

struct free_block
{
    word_t size;
    struct free_block *parent, *left, *right;
    balance_t balance;
#ifdef USE_AVL_FREELIST
    struct free_block * prev; /* prev free block in freelist
                               * NULL for the AVL node
                               */
    struct free_block * next; /* next free block in freelist */
#endif /* USE_AVL_FREELIST */
    short align_dummy;
};

/* Prepare two nodes for the free tree that will never be removed,
 * so that we can always assume that the tree is and remains non-empty.
 */
extern struct free_block dummy2;  /* forward */

static struct free_block dummy =
        { /* size */ 0
        , /* parent */ &dummy2, /* left */ 0, /* right */ 0, /* balance */ 0
#ifdef USE_AVL_FREELIST
        , /* prev */ 0, /* next */ 0
#endif /* USE_AVL_FREELIST */
        };

       struct free_block dummy2 =
        { /* size */ 0
        , /* parent */ 0, /* left */ &dummy, /* right */ 0, /* balance */ -1
#ifdef USE_AVL_FREELIST
        , /* prev */ 0, /* next */ 0
#endif /* USE_AVL_FREELIST */
        };

static struct free_block *free_tree = &dummy2;

#ifdef DEBUG_AVL

static Bool inconsistency = MY_FALSE;

/*-------------------------------------------------------------------------*/
static void
_check_next (struct free_block *p)

/* DEBUG_AVL: check this node <p> and both subnodes.
 */

{
    if (!p)
        return;

    {
        word_t *q;

        q = ((word_t *)p)-ML_OVERHEAD;
        q += *q;
    }
    _check_next(p->left);
    _check_next(p->right);
} /* _check_next() */

/*-------------------------------------------------------------------------*/
static void
check_next(void)

/* DEBUG_AVL: check the free_tree.
 */

{
    _check_next(free_tree);
} /* check_next() */

/*-------------------------------------------------------------------------*/
static int
check_avl (struct free_block *parent, struct free_block *p)

/* DEBUG_AVL: check the consistency of the AVL-(sub)tree in node <p>.
 * Return the size of the subtree, and set inconsistency to TRUE
 * if it is inconsistent.
 */

{
    int left, right;

    if (!p)
        return 0;

    left  = check_avl(p, p->left );
    right = check_avl(p, p->right);

    if (p->balance != right - left || p->balance < -1 || p->balance > 1)
    {
        writes  (2, "Inconsistency in avl node: invalid balance!\n");
        dprintf1(2, "  node:%x\n",(p_uint)p);
        dprintf1(2, "  size: %d\n", p->size);
        dprintf1(2, "  left node:%x\n",(p_uint)p->left);
        dprintf1(2, "  left  height: %d\n",left );
        dprintf1(2, "  right node:%x\n",(p_uint)p->right);
        dprintf1(2, "  right height: %d\n",right);
        dprintf1(2, "  alleged balance: %d\n",p->balance);
        inconsistency = MY_TRUE;
    }

    if (p->parent != parent)
    {
        writes  (2, "Inconsistency in avl node: invalid parent!\n");
        dprintf1(2, "  node:%x\n",(p_uint)p);
        dprintf1(2, "  size: %d\n", p->size);
        dprintf1(2, "  parent: %x\n", (p_uint)parent);
        dprintf1(2, "  parent size: %d\n", parent->size);
        dprintf1(2, "  alleged parent: %x\n", (p_uint)p->parent);
        dprintf1(2, "  alleged parent size: %d\n", p->parent->size);
        dprintf1(2, "  left  height: %d\n",left );
        dprintf1(2, "  right height: %d\n",right);
        dprintf1(2, "  alleged balance: %d\n",p->balance);
        inconsistency = MY_TRUE;
    }
    return left > right ? left+1 : right+1;
} /* check_avl() */

/*-------------------------------------------------------------------------*/
static int
do_check_avl(void)

/* DEBUG_AVL: Check the free_tree for consistency.
 * Return 0 on success, otherwise abort the driver.
 */

{
    check_avl(0, free_tree);
    if (inconsistency)
    {
        fflush(stderr);
        fflush(stdout);
        in_malloc = 0;
        fatal("Inconsistency could crash the driver\n");
    }
    return 0;
} /* do_check_avl() */

/*-------------------------------------------------------------------------*/
static Bool
contains (struct free_block *p, struct free_block *q)

/* DEBUG_AVL: Check if free_block <q> is contained in the sub-tree at
 * and below <p>.
 */

{
    return p == q || (p && (contains(p->left, q) || contains(p->right, q)));
} /* contains() */

/*-------------------------------------------------------------------------*/
static int
check_free_block (void *m)

/* DEBUG_AVL: Check if free memory at <m> is contained in the free_tree.
 * Return 0 on success, otherwise abort the driver.
 */

{
    word_t *p;
    int size;

    p = (word_t *)(((char *)m)-M_OVERHEAD*GRANULARITY);
    size = p[M_LSIZE];
    if (!(*(p+size) & THIS_BLOCK))
    {
        if (!contains(free_tree, (struct free_block *)(p+size+T_OVERHEAD)) )
        {
            in_malloc = 0;
            fatal("not found\n");
        }
    }
    return 0;
} /* check_free_block() */

#else /* !DEBUG_AVL */

#define do_check_avl() 0

#endif /* DEBUG_AVL */

/*-------------------------------------------------------------------------*/
static void
remove_from_free_list (word_t *ptr)

/* Remove the memory block <ptr> from the free list.
 */

{
    struct free_block *p, *q, *r, *s, *t;

#ifdef MALLOC_CHECK
    if (ptr[M_MAGIC] != LFMAGIC)
    {
        in_malloc = 0;
        fatal("remove_from_free_list: block %p, "
              "magic match failed: expected %lx, "
              "found %lx\n"
             , ptr
             , (unsigned long)LFMAGIC
             , (unsigned long)ptr[M_MAGIC]
             );
    }
#endif
    p = (struct free_block *)(ptr+M_OVERHEAD);
    count_back(&large_free_stat, p->size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[SMALL_BLOCK_NUM+1].cur_free--;
#endif /* MALLOC_EXT_STATISTICS */
#ifdef USE_AVL_FREELIST
    /* Unlink from AVL freelist */
    if (p->prev) p->prev->next = p->next;
    if (p->next) p->next->prev = p->prev;
    /* If the block is not the AVL node itself, we're done */
    if (p->prev)
        return;

    /* <p> is the AVL node itself, but if there is another block free of
     * the same size, just transfer over the node.
     */
    if (p->next)
    {
        struct free_block *next = p->next;

        if (p == free_tree)
        {
#ifdef DEBUG
            if (p->parent)
            {
                fatal("(remove_from_free_list) Node %p (size %ld) is the AVL tree root, but has a parent\n", p, (long)p->size);
            }
#endif
            free_tree = p->next;
        }
        else
        {
#ifdef DEBUG
            if (!p->parent)
            {
                fatal("(remove_from_free_list) Node %p (size %ld) has neither a parent nor is it the AVL tree root.\n", p, (long)p->size);
            }
#endif
            if (p->parent->left == p)
                p->parent->left = p->next;
            else
                p->parent->right = p->next;
        }

        /* We must not clobber p->next->next when copying the node! */
        p->next = next->next;
        *next = *p;

        /* Now adjust the parent pointer of the sub-nodes to the new
         * parent node.
         */
        if (p->left) p->left->parent = next;
        if (p->right) p->right->parent = next;

        return;
    }

    /* It's the AVL itself, and there is no other free block of the same
     * size: remove the node from the tree.
     */
    num_avl_nodes--;
#endif /* USE_AVL_FREELIST */
#ifdef DEBUG_AVL
    dprintf1(2, "node:%x\n",(p_uint)p);
    dprintf1(2, "size:%d\n",p->size);
#endif
    if (p->left) {
        if ( NULL != (q = p->right) ) {
            /* two childs */
            s = q;
            do { r = q; q = r->left; } while(q != NULL);
            if (r == s) {
                r->left = s = p->left;
                s->parent = r;
                if ( NULL != (r->parent = s = p->parent) ) {
                    if (p == s->left) {
                        s->left  = r;
                    } else {
                        s->right = r;
                    }
                } else {
                    free_tree = r;
                }
                r->balance = p->balance;
                p = r;
                goto balance_right;
            } else {
                t = r->parent;
                if ( NULL != (t->left = s = r->right) ) {
                    s->parent  = t;
                }
                r->balance = p->balance;
                r->left  = s = p->left;
                s->parent = r;
                r->right = s = p->right;
                s->parent = r;
                if ( NULL != (r->parent = s = p->parent) ) {
                    if (p == s->left) {
                        s->left  = r;
                    } else {
                        s->right = r;
                    }
                } else {
                    free_tree = r;
                }
                p = t;
                goto balance_left;
            }
        } else /* no right child, but left child */ {
            /* We set up the free list in a way so that there will remain at
               least two nodes, and the avl property ensures that the left
               child is a leaf ==> there is a parent */
            s = p;
            p = s->parent;
            r = s->left;
            r->parent = p;
            if (s == p->left) {
                p->left  = r;
                goto balance_left;
            } else {
                p->right = r;
                goto balance_right;
            }
        }
    } else /* no left child */ {
        /* We set up the free list in a way so that there is a node left
           of all used nodes, so there is a parent */
        s = p;
        p = s->parent;
        if ( NULL != (q = r = s->right) ) {
            r->parent = p;
        }
        if (s == p->left) {
            p->left  = r;
            goto balance_left;
        } else {
            p->right = r;
            goto balance_right;
        }
    }
balance_q:
    r = p;
    p = q;
    if (r == p->right) {
        balance_t b;
balance_right:
        b = p->balance;
        if (b > 0) {
            p->balance = 0;
            if (NULL != (q = p->parent)) goto balance_q;
            return;
        } else if (b < 0) {
            r = p->left;
            b = r->balance;
            if (b <= 0) {
                /* R-Rotation */
#ifdef DEBUG_AVL
                dprintf1(2, "r->balance: %d\n", r->balance);
#endif
                if ( NULL != (p->left = s = r->right) ) {
                    s->parent = p;
                }
                r->right = p;
                s = p->parent;
                p->parent = r;
                b += 1;
                r->balance = b;
                b = -b;
#ifdef DEBUG_AVL
                dprintf1(2, "node r: %x\n", (p_uint)r);
                dprintf1(2, "r->balance: %d\n", r->balance);
                dprintf1(2, "node p: %x\n", (p_uint)p);
                p->balance = b;
                dprintf1(2, "p->balance: %d\n", p->balance);
                dprintf1(2, "r-height: %d\n", check_avl(r->parent, r));
#endif
                if ( NULL != (r->parent = s) ) {
                    if ( 0 != (p->balance = b) ) {
                        if (p == s->left) {
                            s->left  = r;
                            return;
                        } else {
                            s->right = r;
                            return;
                        }
                    }
                    if (p == s->left) {
                        /* left from parent */
                        goto balance_left_s;
                    } else {
                        /* right from parent */
                        p = s;
                        p->right = r;
                        goto balance_right;
                    }
                }
                p->balance = b;
                free_tree = r;
                return;
            } else /* r->balance == +1 */ {
                /* LR-Rotation */
                balance_t b2;

                t = r->right;
                b = t->balance;
                if ( NULL != (p->left  = s = t->right) ) {
                    s->parent = p;
                }
                if ( NULL != (r->right = s = t->left) ) {
                    s->parent = r;
                }
                t->left  = r;
                t->right = p;
                r->parent = t;
                s = p->parent;
                p->parent = t;
#ifdef NO_BARREL_SHIFT
                b = -b;
                b2 = b >> 1;
                r->balance = b2;
                b -= b2;
                p->balance = b;
#else
                b2 = (unsigned char)b >> 7;
                p->balance = b2;
                b2 = -b2 -b;
                r->balance = b2;
#endif
                t->balance = 0;
#ifdef DEBUG_AVL
                dprintf1(2, "t-height: %d\n", check_avl(t->parent, t));
#endif
                if ( NULL != (t->parent = s) ) {
                    if (p == s->left) {
                        p = s;
                        s->left  = t;
                        goto balance_left;
                    } else {
                        p = s;
                        s->right = t;
                        goto balance_right;
                    }
                }
                free_tree = t;
                return;
            }
        } else /* p->balance == 0 */ {
            p->balance = -1;
            return;
        }
    } else /* r == p->left */ {
        balance_t b;

        goto balance_left;
balance_left_s:
        p = s;
        s->left  = r;
balance_left:
        b = p->balance;
        if (b < 0) {
            p->balance = 0;
            if ( NULL != (q = p->parent) ) goto balance_q;
            return;
        } else if (b > 0) {
            r = p->right;
            b = r->balance;
            if (b >= 0) {
                /* L-Rotation */
#ifdef DEBUG_AVL
                dprintf1(2, "r->balance: %d\n", r->balance);
#endif
                if ( NULL != (p->right = s = r->left) ) {
                    s->parent = p;
                }
                /* subtree relocated */
                r->left = p;
                s = p->parent;
                p->parent = r;
                b -= 1;
                r->balance = b;
                b = -b;
#ifdef DEBUG_AVL
                /* balances calculated */
                dprintf1(2, "node r: %x\n", (p_uint)r);
                dprintf1(2, "r->balance: %d\n", r->balance);
                dprintf1(2, "node p: %x\n", (p_uint)p);
                p->balance = b;
                dprintf1(2, "p->balance: %d\n", p->balance);
                dprintf1(2, "r-height: %d\n", check_avl(r->parent, r));
#endif
                if ( NULL != (r->parent = s) ) {
                    if ( 0 != (p->balance = b) ) {
                        if (p == s->left) {
                            s->left  = r;
                            return;
                        } else {
                            s->right = r;
                            return;
                        }
                    }
                    if (p == s->left) {
                        /* left from parent */
                        goto balance_left_s;
                    } else {
                        /* right from parent */
                        p = s;
                        p->right = r;
                        goto balance_right;
                    }
                }
                p->balance = b;
                free_tree = r;
                return;
            } else /* r->balance == -1 */ {
                /* RL-Rotation */
                balance_t b2;

                t = r->left;
                b = t->balance;
                if ( NULL != (p->right = s = t->left) ) {
                    s->parent = p;
                }
                if ( NULL != (r->left  = s = t->right) ) {
                    s->parent = r;
                }
                t->right = r;
                t->left  = p;
                r->parent = t;
                s = p->parent;
                p->parent = t;
#ifdef NO_BARREL_SHIFT
                b = -b;
                b2 = b >> 1;
                p->balance = b2;
                b -= b2;
                r->balance = b;
#else
                b2 = (unsigned char)b >> 7;
                r->balance = b2;
                b2 = -b2 -b;
                p->balance = b2;
#endif
                t->balance = 0;
                if ( NULL != (t->parent = s) ) {
                    if (p == s->left) {
                        p = s;
                        s->left  = t;
                        goto balance_left;
                    } else {
                        s->right = t;
                        p = s;
                        goto balance_right;
                    }
                }
                free_tree = t;
                return;
            }
        } else /* p->balance == 0 */ {
            p->balance++;
            return;
        }
    }
} /* remove_from_free_list() */

/*-------------------------------------------------------------------------*/
static void
add_to_free_list (word_t *ptr)

/* Add the memory block <ptr> to the free list.
 */

{
    word_t size;
    struct free_block *p, *q, *r;
    /* When there is a distinction between data and address registers and/or
       accesses, gcc will choose data type for q, so an assignmnt to q will
       faciliate branching
     */

#ifdef MALLOC_CHECK
    ptr[M_MAGIC] = LFMAGIC;
#endif
    size = ptr[M_LSIZE];
#ifdef DEBUG_AVL
    dprintf1(2, "size:%d\n",size);
#endif
    q = (struct free_block *)size; /* this assignment is just a hint for
                                    * register choice
                                    */
    r = (struct free_block *)(ptr+M_OVERHEAD);
    count_up(&large_free_stat, size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[SMALL_BLOCK_NUM+1].cur_free++;
    extstat_update_max(extstats+SMALL_BLOCK_NUM+1);
#endif /* MALLOC_EXT_STATISTICS */

    r->size    = size;
    r->parent  = NULL;
    r->left    = 0;
    r->right   = 0;
    r->balance = 0;
#ifdef USE_AVL_FREELIST
    r->prev = NULL;
    r->next = NULL;
#endif /* USE_AVL_FREELIST */

    q = free_tree;
    for ( ; ; /*p = q*/) {
        p = q;
#ifdef DEBUG_AVL
        dprintf1(2, "checked node size %d\n",p->size);
#endif

#ifdef USE_AVL_FREELIST
        if (size == p->size)
        {
            /* We can attach the block to an existing node */
#ifdef MALLOC_ORDER_LARGE_FREELISTS
            struct free_block * tail = p;

            /* Find the proper node after which to insert */
            if (p->next != NULL)
            {
                while (tail->next && tail->next < r)
                {
                    tail = tail->next;
                }
            }

            r->next = tail->next;
            r->prev = tail;

            if (r->next)
                r->next->prev = r;
            tail->next = r;
#else
            r->next = p->next;
            r->prev = p;

            if (r->next)
                r->next->prev = r;
            p->next = r;
#endif /* MALLOC_ORDER_LARGE_FREELISTS */

            return;
        }
#endif /* USE_AVL_FREELIST */
        if (size < p->size) {
            if ( NULL != (q = p->left) ) {
                continue;
            }
            /* add left */
            p->left = r;
            break;
        } else /* >= */ {
            if ( NULL != (q = p->right) ) {
                continue;
            }
            /* add right */
            p->right = r;
            break;
        }
    }

    /* new leaf */
    r->parent  = p;
#ifdef USE_AVL_FREELIST
    num_avl_nodes++;
#endif /* USE_AVL_FREELIST */
#ifdef DEBUG_AVL
    dprintf2(2, "p %x->balance:%d\n",p, p->balance);
#endif
    do {
        struct free_block *s;

        if (r == p->left) {
            balance_t b;

            if ( !(b = p->balance) ) {
#ifdef DEBUG_AVL
                dprintf1(2, "p: %x\n", p);
                dprintf1(2, "  p->size: %d\n", p->size);
                dprintf1(2, "  p->balance: %d := -1\n", p->balance);
                dprintf1(2, "  p->right-h: %d\n", check_avl(p, p->right));
                dprintf1(2, "  p->left -h: %d\n", check_avl(p, p->left ));
#endif
                /* growth propagation from left side */
                p->balance = -1;
            } else if (b < 0) {
#ifdef DEBUG_AVL
                dprintf2(2, "p %x->balance:%d\n",p, p->balance);
#endif
                if (r->balance < 0) {
                    /* R-Rotation */
                    if ( NULL != (p->left = s = r->right) ) {
                        s->parent = p;
                    }
                    r->right = p;
                    p->balance = 0;
                    r->balance = 0;
                    s = p->parent;
                    p->parent = r;
                    if ( NULL != (r->parent = s) ) {
                        if ( s->left == p) {
                            s->left  = r;
                        } else {
                            s->right = r;
                        }
                    } else {
                        free_tree = r;
                    }
                } else /* r->balance == +1 */ {
                    /* LR-Rotation */
                    balance_t b2;
                    struct free_block *t = r->right;

#ifdef DEBUG_AVL
                    dprintf1(2, "t = %x\n",(p_uint)t);
                    dprintf1(2, "r->balance:%d\n",r->balance);
#endif
                    if ( NULL != (p->left  = s = t->right) ) {
                        s->parent = p;
                    }
                    /* relocated right subtree */
                    t->right = p;
                    if ( NULL != (r->right = s = t->left) ) {
                        s->parent = r;
                    }
                    /* relocated left subtree */
                    t->left  = r;
                    b = t->balance;
#ifdef NO_BARREL_SHIFT
                    b = -b;
                    b2 = b >> 1;
                    r->balance = b2;
                    b -= b2;
                    p->balance = b;
#else
                    b2 = (unsigned char)b >> 7;
                    p->balance = b2;
                    b2 = -b2 -b;
                    r->balance = b2;
#endif
                    t->balance = 0;
                    /* balances calculated */
                    s = p->parent;
                    p->parent = t;
                    r->parent = t;
                    if ( NULL != (t->parent = s) ) {
                        if ( s->left == p) {
                            s->left  = t;
                        } else {
                            s->right = t;
                        }
                    } else {
                        free_tree = t;
                    }
#ifdef DEBUG_AVL
                    dprintf1(2, "p->balance:%d\n",p->balance);
                    dprintf1(2, "r->balance:%d\n",r->balance);
                    dprintf1(2, "t->balance:%d\n",t->balance);
                    /* LR-Rotation completed */
#endif
                }
                break;
            } else /* p->balance == +1 */ {
                p->balance = 0;
                /* growth of left side balanced the node */
                break;
            }
        } else /* r == p->right */ {
            balance_t b;

            if ( !(b = p->balance) )
            {
#ifdef DEBUG_AVL
                dprintf1(2, "p: %x\n", p);
                dprintf1(2, "  p->size: %d\n", p->size);
                dprintf1(2, "  p->balance: %d += 1\n", p->balance);
                dprintf1(2, "  p->right-h: %d\n", check_avl(p, p->right));
                dprintf1(2, "  p->left -h: %d\n", check_avl(p, p->left ));
#endif
                /* growth propagation from right side */
                p->balance++;
            } else if (b > 0) {
                if (r->balance > 0) {
                    /* L-Rotation */
                    if ( NULL != (p->right = s = r->left) ) {
                        s->parent = p;
                    }
                    r->left  = p;
                    p->balance = 0;
                    r->balance = 0;
                    s = p->parent;
                    p->parent = r;
                    if ( NULL != (r->parent = s) ) {
                        if ( s->left == p) {
                            s->left  = r;
                        } else {
                            s->right = r;
                        }
                    } else {
                        free_tree = r;
                    }
                } else /* r->balance == -1 */ {
                    /* RL-Rotation */
                    balance_t b2;
                    struct free_block *t = r->left;

#ifdef DEBUG_AVL
                    dprintf1(2, "t = %x\n",(p_uint)t);
                    dprintf1(2, "r->balance:%d\n",r->balance);
#endif
                    if ( NULL != (p->right = s = t->left) ) {
                        s->parent = p;
                    }
                    /* relocated left subtree */
                    t->left  = p;
                    if ( NULL != (r->left  = s = t->right) ) {
                        s->parent = r;
                    }
                    /* relocated right subtree */
                    t->right = r;
                    b = t->balance;
#ifdef NO_BARREL_SHIFT
                    b = -b;
                    b2 = b >> 1;
                    p->balance = b2;
                    b -= b2;
                    r->balance = b;
#else
                    b2 = (unsigned char)b >> 7;
                    r->balance = b2;
                    b2 = -b2 -b;
                    p->balance = b2;
#endif
                    t->balance = 0;
                    s = p->parent;
                    p->parent = t;
                    r->parent = t;
                    if ( NULL != (t->parent = s) ) {
                        if ( s->left == p) {
                            s->left  = t;
                        } else {
                            s->right = t;
                        }
                    } else {
                        free_tree = t;
                    }
                    /* RL-Rotation completed */
                }
                break;
            } else /* p->balance == -1 */ {
#ifdef DEBUG_AVL
                dprintf1(2, "p: %x\n", p);
                dprintf1(2, "  p->balance: %d\n", p->balance);
                dprintf1(2, "  p->right-h: %d\n", check_avl(p, p->right));
                dprintf1(2, "  p->left -h: %d\n", check_avl(p, p->left ));
#endif
                p->balance = 0;
                /* growth of right side balanced the node */
                break;
            }
        }
        r = p;
        p = p->parent;
    } while ( NULL != (q = p) );
} /* add_to_free_list() */

/*-------------------------------------------------------------------------*/
static void
build_block (word_t *ptr, word_t size)

/* Mark the memory block <ptr> of size <size> words(!) as unallocated.
 * Also used to initialize new memory blocks received from the system.
 */

{
    word_t tmp;

    tmp = (*ptr & PREV_BLOCK);
    ptr[M_LSIZE] = size;
    *(ptr+size-2) = size;        /* copy the size information */
    *(ptr) = tmp | M_MASK;       /* marks this block as free */
    *(ptr+size) &= ~PREV_BLOCK;  /* unset 'previous block' flag in next block */
} /* build_block() */

/*-------------------------------------------------------------------------*/
static void
mark_block (word_t *ptr)

/* Mark the memory block at <ptr> as allocated, used, and freeable
 * by the GC.
 */

{
    *l_next_block(ptr) |= PREV_BLOCK;
    *ptr |= THIS_BLOCK | M_GC_FREE | M_REF;
} /* mark_block() */

/*-------------------------------------------------------------------------*/
static word_t *
add_large_free (word_t *ptr, word_t block_size)

/* The large memory block <ptr> with size <block_size> is free:
 * add it to the free list after trying to coagulate it with adjacent
 * ones.
 * Result is the resulting pointer to the (possibly coagulated) free block.
 */

{
    /* If the next block is free, coagulate */
    if (!(*(ptr+block_size) & THIS_BLOCK))
    {
        remove_from_free_list(ptr+block_size);
        block_size += (ptr+block_size)[M_LSIZE];
    }

    /* If the previous block is free, coagulate */
    if (l_prev_free(ptr))
    {
        remove_from_free_list(l_prev_block(ptr));
        block_size += l_prev_block(ptr)[M_LSIZE];
        ptr = l_prev_block(ptr);
    }

    /* Mark the block as free and add it to the freelist */
    build_block(ptr, block_size);
    add_to_free_list(ptr);

    return ptr;
} /* add_large_free() */

/*-------------------------------------------------------------------------*/
static char *
large_malloc ( word_t size, Bool force_more)

/* Allocate a large or <size> bytes.
 *
 * If <force_more> is TRUE, the function first tries to allocate
 * more memory directly from the system. If that fails, it tries a normal
 * allocation. This feature is used to allocate small chunks for the
 * small block allocator.
 *
 * If memory from the system is allocated and <force_more>
 * is not TRUE (or it is in the retry), the allocator allocates at least
 * CHUNK_SIZE bytes. Any extra is immediately entered into the freelist.
 *
 * Return the pointer to the allocated memory block, or NULL when out
 * of memory (not when running in SYSTEM privilege).
 *
 * If the system runs out of memory, the following steps are taken:
 *
 *  - if <force_more> is TRUE, it is set to FALSE and the allocation is tried
 *    again, this time from the freelist.
 *  - free the user reserve, then try again.
 *  - if MASTER privilege: free the master reserve, then try again.
 *  - if SYSTEM privilege: free the system reserve, then try again.
 *  - if !SYSTEM privilege: set out_of_memory and return NULL
 *  - if SYSTEM privilege: dump the lpc backtrace and exit(2) resp. fatal().
 *
 * If any of the reserves is freed, the gc_request flag is set.
 */

{
    word_t real_size;
    word_t *ptr;
#if defined(HAVE_MADVISE) || defined(DEBUG) || defined(DEBUG_MALLOC_ALLOCS)
    size_t orig_size = size;
#endif

    size = (size + GRANULARITY*ML_OVERHEAD + GRANULARITY-1) / GRANULARITY; /* incl. overhead */

retry:
    ptr = NULL;
    if (!force_more)
    {

        /* Find the best fit in the AVL tree */

        struct free_block *p, *q, *r;
        word_t minsplit;
        word_t tempsize;

        ptr += M_OVERHEAD;  /* 'NULL' including overhead */
        minsplit = size + SMALL_BLOCK_MAX + 1;
          /* The split-off block must still count as 'large' */
        q = free_tree;
        for ( ; ; ) {
            p = q;
#ifdef DEBUG_AVL
            dprintf1(2, "checked node size %d\n",p->size);
#endif
            tempsize = p->size;
            if (minsplit < tempsize) {
                ptr = (word_t*)p; /* remember this fit */
                if ( NULL != (q = p->left) ) {
                    continue;
                }
                /* We don't need that much, but that's the best fit we have */
                break;
            } else if (size > tempsize) {
                if ( NULL != (q = p->right) ) {
                    continue;
                }
                break;
            } else /* size <= tempsize <= minsplit */ {
                if (size == tempsize) {
                    ptr = (word_t*)p;
                    break;
                }
                /* size < tempsize */
                if ( NULL != (q = p->left) ) {
                    r = p;
                    /* if r is used in the following loop instead of p,
                     * gcc will handle q very inefficient throughout the
                     * function large_malloc()
                     */
                    for (;;) {
                        p = q;
                        tempsize = p->size;
                        if (size < tempsize) {
                            if ( NULL != (q = p->left) ) {
                                continue;
                            }
                            break;
                        } else if (size > tempsize ) {
                            if ( NULL != (q = p->right) ) {
                                continue;
                            }
                            break;
                        } else {
                            ptr = (word_t*)p;
                            goto found_fit;
                        }
                    }
                    p = r;
                }
                tempsize = p->size;
                if (minsplit > tempsize) {
                    if ( NULL != (q = p->right) ) {
                        for (;;) {
                            p = q;
                            tempsize = p->size;
                            if (minsplit <= tempsize) {
                                ptr = (word_t*)p; /* remember this fit */
                                if ( NULL != (q = p->left) ) {
                                    continue;
                                }
                                break;
                            } else /* minsplit > tempsize */ {
                                if ( NULL != (q = p->right) ) {
                                    continue;
                                }
                                break;
                            }
                        } /* end inner for */
                        break;
                    }
                    break; /* no new fit */
                }
                /* minsplit == tempsize  ==> best non-exact fit */
                ptr = (word_t*)p;
                break;
            }
        } /* end outer for */

found_fit:
        ptr -= M_OVERHEAD;
    } /* if (!force_more) */

    if (!ptr)
    {
        /* No match, allocate more memory */

        word_t chunk_size, block_size;
        size_t extra = 0; /* extra memory allocated by esbrk() */

        block_size = size*GRANULARITY;

        /* If force_more is true (read: we have to allocate a SMALL_CHUNK)
         * or if the if the requested block would leave only a 'small'
         * block or no block in the usual CHUNK_SIZEd chunk, then allocate
         * exactly the block requested. Otherwise allocate a CHUNK_SIZEd
         * chunk, of which the unneeded part is entered into the freelist.
         */

        if (force_more
         || block_size > CHUNK_SIZE - SMALL_BLOCK_MAX_BYTES - T_OVERHEAD*GRANULARITY )
        {
            chunk_size = block_size;

        }
        else
        {
            chunk_size = CHUNK_SIZE;
        }

        /* Some systems like Darwin don't like odd sbrk()/malloc()s, therefore
         * we round up to the next multiple - 64 seems to work fine. That of
         * course might give an overhang of less than SMALL_BLOCK_MAX_BYTES,
         * so we have to add that and its own 64-Byte-fudge factor then, too.
         */
#       define ALLOC_MULTIPLE 63  /* Ok, the (multiple-1) */
        
        if ((chunk_size & ALLOC_MULTIPLE) != 0)
        {
            // this expression is constant, so the compiler will remove the if
            // (gcc actually removes it even without any optimization).
            if (SMALL_BLOCK_MAX_BYTES > ALLOC_MULTIPLE)
                chunk_size += SMALL_BLOCK_MAX_BYTES + 2 * ALLOC_MULTIPLE;
            else
                chunk_size += ALLOC_MULTIPLE;

            chunk_size &= ~ALLOC_MULTIPLE;
        }

        if (force_more)
            ulog3f("lmalloc(%d / %d): Forced allocate new chunk of %d bytes\n"
                  , orig_size, block_size, chunk_size);
        else
            ulog3f("lmalloc(%d / %d): Allocate new chunk of %d bytes\n"
                  , orig_size, block_size, chunk_size);

        /* Get <chunk_size> more bytes from the system */
        {
            if (max_malloced > 0
             && (mp_int)(sbrk_stat.size + chunk_size) > max_malloced
             && (    (mp_int)(sbrk_stat.size + (heap_start ? 0 : GRANULARITY)) >= max_malloced
                 || (chunk_size = max_malloced - sbrk_stat.size
                                               - (heap_start?0:GRANULARITY) )
                     < block_size
                )
               )
            {
                static const char mess[] = "HARD_MALLOC_LIMIT reached.\n";
                writes(2, mess);

                ptr = NULL;
            }
            else
            {
                ptr = (word_t *)esbrk(chunk_size, &extra);
            }
        }

        if (ptr == NULL)
        {
            /* Out of memory - try to recover */
            ulog2f("lmalloc(%d / %d): Didn't get the memory from the system.\n"
                  , orig_size, block_size);

            if (force_more)
            {
                /* The system is out of memory, but maybe we have some left
                 * in the freelist.
                 */
                force_more = MY_FALSE;
                goto retry;
            }

            /* Give up */
            return NULL;
        }

        /* Enough of the scary words - we got our memory block */

        chunk_size += extra;
        block_size = chunk_size / GRANULARITY;

        /* Add the block to the free memory */
        ptr = add_large_free(ptr, block_size);
    } /* end of creating a new chunk */

    /* ptr is now a pointer to a free block in the free list */

#ifdef USE_AVL_FREELIST
    /* If there is more than one free block for this size, take
     * the first block from the freelist to avoid copying around
     * the AVL node information.
     */
    {
        struct free_block *p = (struct free_block *)(ptr + M_OVERHEAD);
        if (p->next)
            ptr = ((word_t *)(p->next)) - M_OVERHEAD;
    }
#endif

    remove_from_free_list(ptr);
    real_size = ptr[M_LSIZE];

    if (real_size - size)
    {
        /* split block pointed to by ptr into two blocks */

        ptr[size] = 0; /* Init the header word */
        build_block(ptr+size, real_size-size);
#ifdef DEBUG
        if (real_size - size <= SMALL_BLOCK_MAX)
        {
            dprintf2(2,"DEBUG: lmalloc(%d / %d): "
                      , orig_size, size * GRANULARITY);
            dprintf2(2
                    , "Split off block of %d bytes, small limit is %d bytes.\n"
                    , (p_int)(real_size - size) * GRANULARITY
                    , (p_int)SMALL_BLOCK_MAX * GRANULARITY);
#ifdef DEBUG_MALLOC_ALLOCS
            if (gcollect_outfd != 2)
            {
                dprintf2(gcollect_outfd
                        ,"DEBUG: lmalloc(%d / %d): "
                        , orig_size, size * GRANULARITY);
                dprintf2(gcollect_outfd
                        , "Split off block of %d bytes, small limit is %d bytes.\n"
                        , (p_int)(real_size - size) * GRANULARITY
                        , (p_int)SMALL_BLOCK_MAX * GRANULARITY);
            }
#endif
        }
#endif

#       ifndef MALLOC_SBRK
        /* When we allocate a new chunk, it might differ slightly in size from
         * the desired size.
         */
        if (real_size - size <= SMALL_BLOCK_MAX)
        {
            mark_block(ptr+size);
            *(ptr+size) &= ~M_GC_FREE; /* Hands off, GC! */
            count_up(&large_wasted_stat, (*(ptr+size) & M_MASK) * GRANULARITY);
        }
        else
#       endif
        {
            /* At this point, it shouldn't happen that the split-off
             * block is too small to be allocated as a small block.
             */
            add_to_free_list(ptr+size);
        }
        build_block(ptr, size);
    }

    /* The block at ptr is all ours */

    mark_block(ptr);
    count_up(&large_alloc_stat, size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[SMALL_BLOCK_NUM+1].num_xalloc++;
    extstats[SMALL_BLOCK_NUM+1].cur_alloc++;
    extstat_update_max(extstats+SMALL_BLOCK_NUM+1);
#endif /* MALLOC_EXT_STATISTICS */
#ifdef MALLOC_CHECK
    ptr[M_MAGIC] = LAMAGIC;
#endif
    MADVISE(ptr+M_OVERHEAD, orig_size);
    return (char *) (ptr + M_OVERHEAD);
} /* large_malloc() */

/*-------------------------------------------------------------------------*/
static void
large_free (char *ptr)

/* Free the large memory block <ptr>. If possible, coagulate it with
 * neighbouring free blocks into one.
 */

{
    word_t size, *p;

    /* Get the real block pointer */
    p = (word_t *) ptr;
    p -= M_OVERHEAD;
    size = p[M_LSIZE];
    count_back(&large_alloc_stat, size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[SMALL_BLOCK_NUM+1].num_xfree++;
    extstats[SMALL_BLOCK_NUM+1].cur_alloc--;
#endif /* MALLOC_EXT_STATISTICS */

#ifdef MALLOC_CHECK
    if (p[M_MAGIC] == LFMAGIC)
    {
        in_malloc = 0;
        fatal("large_free: block %lx size %lu, (user %lx) freed twice\n"
             , (unsigned long)p, (unsigned long)(size * GRANULARITY)
             , (unsigned long)ptr);
    }
    if (p[M_MAGIC] != LAMAGIC)
    {
        in_malloc = 0;
        fatal("large_free(%p): block %p magic match failed: size %lu, "
              "expected %lx, found %lx\n"
             , ptr, p
             , (unsigned long)(size * GRANULARITY)
             , (unsigned long)LAMAGIC
             , (unsigned long)p[M_MAGIC]
             );
    }
#endif

    (void)add_large_free(p, size);
} /* large_free() */

/*-------------------------------------------------------------------------*/
static char *
esbrk (word_t size, size_t * pExtra)

/* Allocate a block of <size> bytes from the system and return its pointer.
 * If esbrk() allocated a larger block, the difference to <size> is
 * returned in *<pExtra>.
 *
 #ifdef MALLOC_SBRK (&& SBRK_OK && MALLOC_REPLACEABLE is implied)
 * It is system dependent how sbrk() aligns data, so we simpy use brk()
 * to ensure that we have enough.
 * At the end of the newly expanded heap we create a fake allocated
 * block of 0 bytes so that large_malloc() knows where to stop.
 #elif HAVE_MMAP
 * Use mmap() to allocate a new block of memory. If this block borders
 * to the previous one, both blocks are joined.
 * The allocated block (modulo joints) is tagged at both ends with fake
 * "allocated" blocks of which cover the unallocated areas - large_malloc()
 * will perceive this as a fragmented heap.
 #else
 * Use malloc() to allocate a new block of memory. If this block borders
 * to the previous one, both blocks are joined.
 * The allocated block (modulo joints) is tagged at both ends with fake
 * "allocated" blocks of which cover the unallocated areas - large_malloc()
 * will perceive this as a fragmented heap.
 #endif
 */

{
    mdb_log_sbrk(size);

    *pExtra = 0;
    
#ifdef MALLOC_SBRK

    if (!heap_end)
    {
        /* First call: allocate the first fake block */
        heap_start = heap_end = (word_t *)sbrk(0);
        if (!esbrk(2*GRANULARITY, pExtra))
        {
            in_malloc = 0;
            fatal("Couldn't malloc anything\n");
        }
        *heap_start = 2;
        *(heap_start+1) = PREV_BLOCK | M_MASK;
        count_up(&large_wasted_stat, 2*GRANULARITY);
        assert_stack_gap();
    }

    /* Get the new block */
    if ((int)brk((char *)heap_end + size) == -1)
        return NULL;

    count_up(&sbrk_stat, size);
    heap_end = (word_t*)((char *)heap_end + size);
    heap_end[-1] = THIS_BLOCK | M_MASK;
    heap_end[-2] = M_MASK;
    return (char *)(heap_end - 1) - size; /* overlap old memory block */

#else  /* not MALLOC_SBRK */

    char *block;
    size_t req_size = size; // the requested size of memory.
    word_t *p;    /* start of the fake block */
    const int overhead = TL_OVERHEAD;
    size_t overlap = 0;
      /* How much extra memory esbrk() could recycle from joining
       * the new allocation with the old one.
       */


    size += overhead * GRANULARITY;  /* for the extra fake "allocated" block */

    // get the new memory block
#ifdef HAVE_MMAP
    {
        static size_t pagesize = 0; // pagesize - 1 for this system.
        if (!pagesize)
            pagesize = getpagesize() - 1;
        // round to multiples of the real pagesize
        if ((size & pagesize) != 0)
        {
            size += pagesize;
            size &= ~pagesize;
        }
        // get new page(s)
        block = mmap(0, size, PROT_READ|PROT_WRITE, MAP_ANON|MAP_PRIVATE, -1, 0);
        if (block == MAP_FAILED)
            return NULL;
    }
#else
    block = malloc(size);
    if (!block)
        return NULL;
#endif
    
    assert_stack_gap();

    /* p points to the start of the fake allocated block used
     * as sentinel/bridge
     */
    p = (word_t *)(block + size) - overhead + 1;

#ifdef MALLOC_TRACE
    p[M_OVERHEAD+XM_FILE] = (word_t)"sentinel/bridge";
    p[M_OVERHEAD+XM_LINE] = 0;
#endif
#ifdef MALLOC_LPC_TRACE
    p[M_OVERHEAD+XM_OBJ] = 0;
    p[M_OVERHEAD+XM_PROG] = 0;
    p[M_OVERHEAD+XM_PC] = 0;
#endif

    if (!heap_end)
    {
        /* First call: create the inital fake block */
        heap_start = (word_t*)block;
        heap_end = (word_t*)(block + size);
        *((word_t *)block+1) = PREV_BLOCK;
        p[M_LSIZE] = overhead;
        p[M_SIZE] = THIS_BLOCK | M_MASK; /* no M_GC_FREE */
        overlap = 0;
    }
    else
    {
        /* Try to join with the existing heap */
        if (block < (char *)heap_start)
        {
            /* New block before the known heap */

            *((word_t *)block+1) = PREV_BLOCK|M_MASK; /* Lower sentinel */
            if (block + size == (char *)heap_start)
            {
                /* We can join with the existing heap */
                p[overhead] &= ~PREV_BLOCK;
                overlap = overhead * GRANULARITY;
                count_back(&large_wasted_stat, overlap);
            }
            else
            {
                /* Separate from the heap */
                p[M_LSIZE] = (heap_start - p + 1);
                p[M_SIZE] = THIS_BLOCK | M_MASK; /* no M_GC_FREE */
                overlap = 0;
            }

            heap_start = (word_t *)block;
        }
        else if (block >= (char *)heap_end)
        {
            /* New block after the known heap */

            p[M_SIZE] = THIS_BLOCK | M_MASK; /* no M_GC_FREE */
            p[M_LSIZE] =  overhead;
            if (block == (char *)heap_end)
            {
                /* We can join with the existing heap */
                heap_end = (word_t *)(block + size);
                block -= overhead * GRANULARITY;
                overlap = overhead * GRANULARITY;
                count_back(&large_wasted_stat, overlap);
            }
            else
            {
                /* Separate from the heap */
                p = (word_t *)heap_end - overhead + 1;
                p[M_SIZE] = (p[M_SIZE] & (PREV_BLOCK|THIS_BLOCK|M_GC_FREE)) | M_MASK;
                p[M_LSIZE] = (word_t *)block - p + 1;
                heap_end = (word_t *)(block + size);
                *((word_t *)block+1) = PREV_BLOCK;
                overlap = 0;
            }
        }
        else
        {
            /* We got a block within the known heap.
             * Try to find it in the fake blocks created earlier.
             * This is slow, but it shouldn't happen too often.
             */

            word_t *prev, *next;

            /* Go to the block right before the one we got */
            next = heap_start;
            do {
                prev = next;
                next = prev + *prev;
            } while (next < (word_t *)block);
            overlap = 0;

            if ((word_t *)block == prev + overhead)
            {
                /* Our block directly follows the one we found */
                block -= overhead * GRANULARITY;
                overlap += overhead * GRANULARITY;
                count_back(&large_wasted_stat, overhead * GRANULARITY);
            }
            else
            {
                /* We have to create a new bridge block */
                prev++;
                prev[M_SIZE] = (prev[M_SIZE] & (PREV_BLOCK|THIS_BLOCK|M_GC_FREE)) | M_MASK;
                prev[M_LSIZE] = (word_t*)block - prev + 1;
#ifdef MALLOC_TRACE
                prev[M_OVERHEAD+XM_FILE] = (word_t)"block";
                prev[M_OVERHEAD+XM_LINE] = 0;
#endif
#ifdef MALLOC_LPC_TRACE
                prev[M_OVERHEAD+XM_OBJ] = 0;
                prev[M_OVERHEAD+XM_PROG] = 0;
                prev[M_OVERHEAD+XM_PC] = 0;
#endif
                *((word_t *)block+1) = PREV_BLOCK | M_MASK;
            }

            if (next - p == overhead)
            {
                /* Our block directly preceedes the next one */
                *(next+1) &= ~PREV_BLOCK;
                overlap += overhead * GRANULARITY;
                count_back(&large_wasted_stat, overhead * GRANULARITY);
            }
            else
            {
                /* We have to create a new bridge block */
                p[M_SIZE] = THIS_BLOCK | M_MASK;  /* no M_GC_FREE */
                p[M_LSIZE] = next - p + 1;
            }
        }
    }

    count_up(&sbrk_stat, size);
    count_up(&large_wasted_stat, overhead * GRANULARITY);

    // amount of additional memory that was allocated
#ifdef HAVE_MMAP
    *pExtra = overlap + (size - overhead * GRANULARITY - req_size);
#else
    *pExtra = overlap;
#endif
    return block + GRANULARITY;
#endif /* !MALLOC_SBRK */
} /* esbrk() */


/*-------------------------------------------------------------------------*/
static INLINE void *
mem_increment_size (void *vp, size_t size)

/* Try to extent the allocation block for <vp> to hold <size> more bytes.
 * If this is not possible, return NULL, otherwise return a pointer
 * to the start of the block extension.
 */

{
    char *p = vp;
    word_t *start, *start2, *start3, old_size, next;
    const word_t wsize = (size + GRANULARITY - 1)/ GRANULARITY;

    malloc_increment_size_calls++;

    start = (word_t*)p - M_OVERHEAD;

    old_size = start[M_SIZE] & M_MASK;

    if (old_size <= SMALL_BLOCK_MAX)
    {
        /* Extend a small block */
        
        word_t new_size = (old_size + wsize) * GRANULARITY;
        start2 = &start[old_size];
        next = *start2;

        if (old_size + wsize > SMALL_BLOCK_MAX)
            return NULL; /* New block would be too large */

        if (!(next & THIS_BLOCK))
            return NULL; /* no following free block */

        /* Make sure the following free block is as large as possible */
        defragment_small_block(start2);

        next = start2[M_SIZE] & M_MASK; /* Might have changed */

        if (next == wsize)
        {
            /* Next block fits perfectly */

            UNLINK_SMALL_FREE(start2);
            start2[next] &= ~PREV_BLOCK;

            start[M_SIZE] += wsize;
            MAKE_SMALL_CHECK_UNCHECKED(start, new_size);

            malloc_increment_size_success++;
            malloc_increment_size_total += (start2 - start) - M_OVERHEAD;

            count_add(&small_alloc_stat, wsize * GRANULARITY);

#ifdef MALLOC_EXT_STATISTICS
            extstats[SIZE_INDEX(old_size * GRANULARITY)].cur_alloc--;
            extstats[SIZE_INDEX(new_size)].cur_alloc++;
            extstat_update_max(extstats+SIZE_INDEX(new_size));
#endif /* MALLOC_EXT_STATISTICS */

            return start2;
        }

        if (next >= wsize + SMALL_BLOCK_SPLIT_MIN + T_OVERHEAD)
        {
            /* Next block is large enough to be split */

            UNLINK_SMALL_FREE(start2);

            start3 = start2 + wsize;
            start3[M_SIZE] = 0; /* Clear PREV_BLOCK */
            MAKE_SMALL_FREE(start3, next - wsize);

            start[M_SIZE] += wsize;
            MAKE_SMALL_CHECK_UNCHECKED(start, new_size);

            malloc_increment_size_success++;
            malloc_increment_size_total += (start2 - start) - M_OVERHEAD;

            count_add(&small_alloc_stat, wsize * GRANULARITY);

#ifdef MALLOC_EXT_STATISTICS
            extstats[SIZE_INDEX(old_size * GRANULARITY)].cur_alloc--;
            extstats[SIZE_INDEX(new_size)].cur_alloc++;
            extstat_update_max(extstats+SIZE_INDEX(new_size));
#endif /* MALLOC_EXT_STATISTICS */

            return start2;
        }

        return NULL; /* No success */
    }

    /* Extend a large block */

    old_size = start[M_LSIZE];
    start2 = &start[old_size];

    if (start2[M_SIZE] & THIS_BLOCK)
        return NULL; /* no following free block */

    next = start2[M_LSIZE];

    if (next == wsize)
    {
        /* Next block fits perfectly */
        remove_from_free_list(start2);
        start2[next] |= PREV_BLOCK;
        start[M_LSIZE] += wsize;
        malloc_increment_size_success++;
        malloc_increment_size_total += (start2 - start) - M_OVERHEAD;
        count_add(&large_alloc_stat, wsize);

        return start2+M_LSIZE;
    }

    if (next > wsize + SMALL_BLOCK_MAX + 1)
    {
        /* Split the next block, it is large enough */

        remove_from_free_list(start2);
        start2[next-2] -= wsize;
        start3 = start2 + wsize;
        start3[M_SIZE] = M_MASK | PREV_BLOCK;
        start3[M_LSIZE] = (next-wsize);
        add_to_free_list(start3);
        start[M_LSIZE] += wsize;
        malloc_increment_size_success++;
        malloc_increment_size_total += (start2 - start) - M_OVERHEAD;
        count_add(&large_alloc_stat, wsize);
        return start2+M_LSIZE;
    }

    /* No success */
    return NULL;
} /* mem_increment_size() */


/*=========================================================================*/

/*                          GARBAGE COLLECTOR                              */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_clear_ref (POINTER p)

/* GC Support: Clear the 'referenced' flag for block <p>.
 */

{
    ((word_t *)(p))[-M_OVERHEAD] &= ~M_REF;
} /* mem_clear_ref() */

/*-------------------------------------------------------------------------*/
static INLINE void
mem_mark_ref (POINTER p)

/* GC Support: Set the 'referenced' flag for block <p>.
 */

{
    ((word_t *)(p))[-M_OVERHEAD] |= M_REF;
} /* mem_mark_ref() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
mem_test_ref (POINTER p)

/* GC Support: Check the memory block marker for <p>, return TRUE if _not_
 * set.
 */

{
    return !( ((word_t *)(p))[-M_OVERHEAD] & M_REF );
} /* mem_test_ref() */


/*-------------------------------------------------------------------------*/
Bool
mem_is_freed (void *p, p_uint minsize)

/* Check if block for the allocation <p> is a free block of at least
 * <minsize>. Blocks outside the heap always qualify.
 * The function might return false for joined blocks.
 */

{
    word_t *block;
    word_t i;

    block = (word_t *) p;
    block -= M_OVERHEAD;

    if (block < heap_start || block + M_OVERHEAD >= heap_end)
        return MY_TRUE;

    i = block[M_SIZE] & M_MASK;
    if (i >= SMALL_BLOCK_MAX)
       i = block[M_LSIZE];

    if (i < M_OVERHEAD + ((minsize + GRANULARITY - 1) / GRANULARITY)
     || block + i >= heap_end)
        return MY_TRUE;

    if (i >= SMALL_BLOCK_MAX)
    {
        word_t* block2;

        block2 = block + i;
        return !(block[M_SIZE] & THIS_BLOCK)
#ifdef MALLOC_CHECK
             || block[M_MAGIC] != LAMAGIC
#endif /* MALLOC_CHECK */
             || !(*block2 & PREV_BLOCK);
    }

    return (block[M_SIZE] & THIS_BLOCK) != 0;
} /* mem_is_freed() */

/*-------------------------------------------------------------------------*/
void
mem_clear_ref_flags (void)

/* Walk through all allocated blocks and clear the M_REF flag in preparation
 * for a GC.
 */

{
    word_t *p, *q, *last;
    int i;

    /* Clear the large blocks */
    last = heap_end - TL_OVERHEAD;
    for (p = heap_start; p < last; )
    {
        p[1] &= ~M_REF;
        if (p + *p > heap_end)
        {
            in_malloc = 0;
            fatal("pointer larger than brk: %p + %"PRIdPTR" = %p > %p\n"
                  , p, (intptr_t)*p, p + *p , last);
        }
        p += *p;
    }

    /* Now mark the memory used for the small chunks as ref'd,
     * then clear the small blocks.
     */
    for (p = last_small_chunk; p; p = *(word_t**)p)
    {
        word_t *end;

        mem_mark_ref(p);
        end = p - ML_OVERHEAD + p[-ML_OVERHEAD];
#ifdef DEBUG
        dprintf2(gcollect_outfd, "clearing M_REF in chunk %x, end %x\n",
          (word_t)(p - ML_OVERHEAD), (word_t)end
        );
#endif
        for (q = p+1; q < end; )
        {
            word_t size = *q;

            *q &= ~M_REF;
            q += size & M_MASK;
        }

        if (q > end)
        {
            /* pass some variables to fatal() so that the optimizer
             * won't throw away the values, making them unreadable
             * in the core.
             */
            in_malloc = 0;
            fatal("Small block error, start: %lx, %lx vs. %lx\n",
              (long)(p+1), (long)q, (long)end);
        }
    }

    /* We set M_REF for small free blocks that early for two reasons:
     * - if it's referenced anywhere else, we get a fatal.
     * - if the block gets malloced by the swapper in pass 5, it needs
     *   M_REF to be already set.
     */
    for (i=0; i < SMALL_BLOCK_NUM + 1; i++)
    {
        for (p = sfltable[i]; p; p = BLOCK_NEXT(p) ) {
            *p |= M_REF;
        }
    }
} /* mem_clear_ref_flags() */

/*-------------------------------------------------------------------------*/
void
mem_free_unrefed_memory (void)

/* The GC marked all used memory as REF'd, now recover all blocks which
 * are allocated, but haven't been marked.
 */

{
    word_t *p, *q, *last;
    mp_int success = 0;

    /* Scan the heap for lost large blocks */
    last = heap_end - TL_OVERHEAD;
    for (p = heap_start; p < last; )
    {
        word_t size, flags;

        size = *p;
        flags = p[1];
        if ( (flags & (M_REF|THIS_BLOCK|M_GC_FREE)) == (THIS_BLOCK|M_GC_FREE) )
        {
            /* Large block marked as in use (THIS_BLOCK), but is not
             * referenced (no M_REF) - recover it.
             */
            word_t size2, flags2;

            success++;
            count_back(&xalloc_stat, mem_block_size(p+ML_OVERHEAD));
#if defined(MALLOC_TRACE) || defined(MALLOC_LPC_TRACE)
            dprintf1(gcollect_outfd, "freeing large block 0x%x", (p_uint)p);
#endif
#ifdef MALLOC_TRACE
            dprintf3(gcollect_outfd, " %s %d size 0x%x\n",
              p[XM_FILE+ML_OVERHEAD], p[XM_LINE+ML_OVERHEAD], size & M_MASK
            );
#endif
#ifdef MALLOC_LPC_TRACE
            write_lpc_trace(gcollect_outfd, p + ML_OVERHEAD, MY_FALSE);
#endif
            print_block(gcollect_outfd, p + ML_OVERHEAD);
            size2 = p[size];
            flags2 = p[size + 1];
            large_free((char *)(p+ML_OVERHEAD));
            if ( !(flags2 & THIS_BLOCK) )
                size += size2;
        }
        p += size;
    }
    if (success)
    {
        dprintf1(gcollect_outfd, "%d large blocks freed\n", success);
    }

    /* Scan the small chunks for lost small blocks.
     * Remember that small blocks in the free-lists are marked as ref'd.
     */
    success = 0;
    for (p = last_small_chunk; p; p = *(word_t**)p)
    {
        word_t *end;

        end = p - ML_OVERHEAD + p[-ML_OVERHEAD];
#ifdef DEBUG
        dprintf2(gcollect_outfd, "scanning chunk %x, end %x for unref'd blocks\n",
          (word_t)(p - M_OVERHEAD), (word_t)end
        );
#endif
        for (q = p+1; q < end; )
        {
            word_t size = *q;

            if ((*q & (M_REF|M_GC_FREE)) == M_GC_FREE)
            {
                /* Unref'd small blocks are definitely lost */
                success++;
                count_back(&xalloc_stat, mem_block_size(q+M_OVERHEAD));
                dprintf2(gcollect_outfd, "freeing small block 0x%x (user 0x%x)"
                        , (p_uint)q, (p_uint)(q+M_OVERHEAD));
#ifdef MALLOC_TRACE
                dprintf2(gcollect_outfd, " %s %d"
                        , q[XM_FILE+M_OVERHEAD], q[XM_LINE+M_OVERHEAD]);
#endif
                writes(gcollect_outfd, "\n");
#ifdef MALLOC_LPC_TRACE
                write_lpc_trace(gcollect_outfd, q + M_OVERHEAD, MY_FALSE);
#endif
                print_block(gcollect_outfd, q + M_OVERHEAD);

                /* Recover the block */
                *q |= M_REF;
                sfree(q+M_OVERHEAD);
            }
            q += size & M_MASK;
        }
    }
    if (success) {
        dprintf1(gcollect_outfd, "%d small blocks freed\n", success);
    }
} /* mem_free_unrefed_memory() */

/*-------------------------------------------------------------------------*/
Bool
mem_dump_memory (int fd)

/* Print the location, size, and (if available) the TRACE information
 * of all memory blocks to file <fd>, and return TRUE.
 * If the allocator doesn't support this operation, print nothing
 * and return FALSE.
 *
 * If <fd> is -1, just return TRUE or FALSE (this is used to check if
 * the allocator supports memory dumps).
 */

{
    word_t *p, *q, *last;

    if (fd < 0)
        return MY_TRUE;

    writes(fd, "\n--- Large Blocks\n");

    /* Dump the heap blocks */
    last = heap_end - TL_OVERHEAD;
    for (p = heap_start; p < last; )
    {
        word_t size,  flags;

        size = *p;
        flags = p[1];
        if ( flags & THIS_BLOCK )
        {
            Bool isSmallChunk = MY_FALSE;

            dprintf4(fd, "%x .. %x %s size %x "
                       , (p_uint)p, (p_uint)(p + size) - 1
                       , (p_uint)((flags & M_GC_FREE) ? " " : "P")
                       , (p_uint)size * GRANULARITY
                       );
            for (q = last_small_chunk; q && !isSmallChunk; q = *(word_t**)q)
            {
                isSmallChunk = (q - ML_OVERHEAD == p);
            }

            if (isSmallChunk)
                writes(fd, ": small chunk\n");
            else
            {
#ifdef MALLOC_TRACE
                if (p[XM_FILE+ML_OVERHEAD])
                    dprintf2(fd, ": %s %d "
                               , p[XM_FILE+ML_OVERHEAD], p[XM_LINE+ML_OVERHEAD]
                    );
                else
                    writes(fd, ": - -");
#endif
#ifdef MALLOC_LPC_TRACE
                if (p[ML_OVERHEAD + XM_OBJ])
                {
                    writes(fd, ": ");
                    write_lpc_trace(fd, p + ML_OVERHEAD, MY_TRUE);
                }
                else
#endif
                    writes(fd, "\n");
            }
        }
        p += size;
    }

    /* Dump the small chunks and their small blocks.
     */
    for (p = last_small_chunk; p; p = *(word_t**)p)
    {
        word_t *end;

        end = p - ML_OVERHEAD + (p[-ML_OVERHEAD]);

        {
            word_t size, flags;

            q = p - ML_OVERHEAD;
            size = *q;
            flags = q[1];

            dprintf4(fd, "\n--- Small Chunk: %x .. %x %s size %x\n"
                       , (p_uint)q, (p_uint)(q + size) - 1
                       , (p_uint)((flags & M_GC_FREE) ? " " : "P")
                       , (p_uint)size * GRANULARITY
                       );
            /* No trace information for small chunks */
        }

        for (q = p+1; q < end; )
        {
            word_t size = *q;

            if (!(*q & THIS_BLOCK))
            {
                dprintf4(fd, "%x .. %x %s size %x "
                           , (p_uint)q, (p_uint)(q + (size&M_MASK)) - 1
                       , (p_uint)((size & M_GC_FREE) ? " " : "P")
                           , (p_uint)(size & M_MASK) * GRANULARITY
                           );

                /* The sentinel blocks in a small chunk consist of just
                 * the size byte - don't try to dump those.
                 */
                if ((size & M_MASK) > M_OVERHEAD)
                {
#ifdef MALLOC_TRACE
                    if (q[XM_FILE+M_OVERHEAD])
                        dprintf2(fd, ": %s %d "
                                   , q[XM_FILE+M_OVERHEAD], q[XM_LINE+M_OVERHEAD]
                        );
                    else
                        writes(fd, ": - - ");
#endif
#ifdef MALLOC_LPC_TRACE
                    if (q[M_OVERHEAD + XM_OBJ])
                    {
                        writes(fd, ": ");
                        write_lpc_trace(fd, q + M_OVERHEAD, MY_TRUE);
                    }
                    else
#endif
                        writes(fd, "\n");
                }
                else
                    writes(fd, "\n");
            }
            q += size & M_MASK;
        }
    }

    return MY_TRUE;
} /* mem_dump_memory() */

/*-------------------------------------------------------------------------*/
void
mem_consolidate (Bool force)

/* Consolidate the memory.
 *
 * If <force> is TRUE, the small free blocks are defragmented first.
 * Then, the function walks the list of small chunks and free all which are
 * totally unused.
 */

{
    word_t *prev, *this;

    if (force)
        defragment_small_lists(0);

    for (prev = NULL, this = last_small_chunk ; this != NULL ; )
    {
        word_t chunk_size = this[-ML_OVERHEAD];

        /* If the chunk holds only one free block, it can be freed
         * altogether.
         */
        if ((this[1+M_SIZE] & THIS_BLOCK)
         && (this[1+M_SIZE] & M_MASK) == chunk_size - M_OVERHEAD - 2
           )
        {
            word_t * next = *(word_t **) this;

            if (!prev)
                last_small_chunk = next;
            else
                *(word_t **)prev = next;

            UNLINK_SMALL_FREE(this+1);
            large_free((char *)this);

            count_back(&small_chunk_stat, chunk_size * GRANULARITY);
            count_back(&small_chunk_wasted, GRANULARITY*(M_OVERHEAD+2));

            this = next;
        }
        else
        {
            prev = this;
            this = *(word_t**)this;
        }
    } /* for (all chunks) */
} /* mem_consolidate() */


/*-------------------------------------------------------------------------*/
#if !defined(HAVE_GETPAGESIZE)
static INLINE size_t
getpagesize()
/* get the pagesize for this system */
{
#ifdef(HAVE_SYSCONF)
    return sysconf(_SC_PAGESIZE);
#else
#error Unable to find out the system pagesize. Please report this issue.
#endif
}
#endif /* HAVE_GETPAGESIZE */
/***************************************************************************/
