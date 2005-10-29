/*---------------------------------------------------------------------------
 * Slab-Alloc Memory Manager
 *
 * Written and put into the public domain by Lars Duening.
 * Based on the 'SMalloc' allocator written and put into the public domain by
 * Sean T. "Satoria" Barrett.
 *---------------------------------------------------------------------------
 * Slab-Alloc is intended to be optimized for lpmud. This memory manager
 * distinguishes between two sizes of blocks: small and large. It manages
 * them separately in the hopes of avoiding fragmentation between them.
 * It expects small blocks to mostly be temporaries.
 *
 * slaballoc IS NOT THREADSAFE. And the wrong way of fixing this would be
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
 * 68 Bytes. Such block are allocated from larger memory blocks ('slabs')
 * sized rough multiples of 4KByte. The size is determined such that the slab
 * holds at least 100 blocks.
 *
 * A slab consists of a header, followed by the blocks themselves. Free blocks
 * within a slab are linked together in a free list. When a slab is freshly
 * created, it is filled in from the end and the allocator keeps an external
 * pointer to the end of the yet unused space in the slab.
 *
 * For every possible block size, the allocator maintains three lists of
 * slabs: a list of fully used slabs (single-ended), a list of partially
 * used slabs (double-ended) and a list of fully free slabs (double-ended).
 * In addition, the allocator maintains for each block size a 'fresh slab'
 * which is only partially initialized.
#ifdef MALLOC_ORDER_SMALL_FREELISTS
 * The partially used slabs in the lists are rougly ordered by the ascending
 * number of free blocks - 'roughly' meaning that whenever the freeing of
 * a block causes a slab block to have more free blocks than the next one,
 * the order of the two is switched. This is not as effective as maintaining
 * a total order, but it can be done in constant time and is better
 * than keeping no order at all.
#else
 * The partially used slabs in the lists are managed in a FIFO fashion
 * to ensure that every partially used slab gets a chance of being
 * filled up completely. Trying a LIFO approach led to a distinctly larger
 * fragmentation.
#endif
 *
 * When allocating a new block, the allocator first tries to allocated the
 * block from the freelist of the first partly filled slab. If there is none,
 * it satisfies the request from the fresh slab (allocating one if necessary).
 *
 * When a block is freed, the allocator determines which slab it belongs to
 * and links it into the freelist. If it is the first free block for the slab,
 * the slab is also moved at the beginning of the slab list for this block
 * size. If the block was the last used block in the slab, the whole slab
 * is tagged with a timestamp and moved into a separate list.
 *
 * The backend (and the GC) in regular intervals call mem_consolidate() which
 * will scan the lists of free slabs and remove those with a timestamp older
 * than SLAB_RETENTION_TIME (currently 60 seconds). The intention is to
 * keep a load-depending cache of free slabs around to avoid thrashing
 * in the large allocator.
 *
 * In order to determine the slab for a block, the distance to the slab begin
 * is stored as number (counting in words) in the size field of the block.
 * In addition, bit 27 (0x08000000) is set to mark the block as 'small'.
 *
 * This allocation system intents to achieve these goals:
 *  - Allocations of small blocks of equal size are clustered for reduced
 *    fragmentation. This applies in particular for allocations from the
 *    free lists, which are partly decoupled from the order in which the
 *    blocks are freed.
 *  - No blocks of sizes not used by the driver are ever generated.
 *  - Unused small block memory can be returned to the overall memory pool.
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
#ifdef SBRK_OK && MALLOC_SBRK
 * Memory is allocated from the system with sbrk() and brk(), which puts
 * the whole heap under our control.
 *
 * In this mode, several functions like amalloc() are compiled as malloc(),
 * implementing all libc memory functions.
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
 * The first word in each allocated block is used for the 'size' field
 * which holds the block's size (inclusive all overhead) in words, plus
 * the following flags:
 *
 *   THIS_BLOCK: small: set if this block is not allocated
 *               large: set if this block is allocated
 *   PREV_BLOCK: large: set if the previous block is allocated
 *   M_GC_FREE : set in allocated blocks if the GC may free this block if
 *               found lost
 *   M_REF     : set if this block is referenced (used during a GC)
  *  M_SMALL   : set in small blocks
 *
 * Because of the PREV_BLOCK flag, all large allocations (either the
 * slabs for the small block allocator, or the esbrk() chunks for the
 * large block allocator) are initialized with a fake permanently allocatod
 * block at the end of the chunk, consisting of not much more than the
 * size field with THIS_BLOCK cleared.
 *
#ifdef MALLOC_CHECK
 * The second word holds the magic word, which for small blocks is determined
 * also by the size of the block.
#endif
 *
 * The user (aka payload) area of free blocks is used to manage the various
 * free lists.
 *
 * Small free blocks:
 *
 *   The M_SIZE field in fact holds the offset to the slab holding this
 *   block.
 *
 *   The first word of the user area holds the 'next' link of the free
 *   list, which is NULL for the last block in the list.
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

#ifdef HAVE_MADVISE
#    include <sys/mman.h>
#    define MADVISE(new,old)  madvise(new,old,MADV_RANDOM)
#else
#    define MADVISE(new,old)  NOOP
#endif

#include "slaballoc.h"

#include "mstrings.h"
#include "stdstrings.h"
#include "svalue.h"
#ifdef MALLOC_EXT_STATISTICS
#include "array.h"
#include "backend.h"
#endif /* MALLOC_EXT_STATISTICS */

#include "../mudlib/sys/debug_info.h"

/* Defines required by the xalloc.c wrapper */

#define MEM_ALIGN (SIZEOF_CHAR_P)

/* If possible, request memory using sbrk(). But if using pthreads, do
 * not replace malloc() with our routines, even if the system allows it,
 * as slaballoc is not threadsafe.
 */

#if defined(SBRK_OK)
#  ifdef MALLOC_SBRK
#      if !defined(USE_PTHREADS)
#          define REPLACE_MALLOC
#      else
#          undef SBRK_OK
#      endif
#  else
#      undef SBRK_OK
#  endif
#endif

#define MEM_MAIN_THREADSAFE

/* #undef NO_MEM_BLOCK_SIZE */

#define SINT (SIZEOF_CHAR_P)

/* Initialiser macros for the tables */

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

/* The extra slaballoc header fields.
 */

#define M_SIZE 0  /* (word_t) Size in word_t, plus some flags */

#ifdef MALLOC_CHECK
#    define M_OVERHEAD (2)
#    define M_MAGIC  1  /* (word_t) The magic word */
#else
#    define M_OVERHEAD (1)
#endif /* MALLOC_CHECK */

#define M_LINK  M_OVERHEAD
   /* Index of the 'next' link for the small free lists */

#define BLOCK_NEXT(block) ((word_t *)(block[M_LINK]))
   /* The 'next' link of free block <block>, properly typed.
    */

#define SET_BLOCK_NEXT(block,link) ((block)[M_LINK] = (word_t)(link))
   /* Set the 'next' link of free blocks.
    */

#define T_OVERHEAD (M_OVERHEAD + XM_OVERHEAD)
   /* Total overhead: it is used by slaballoc to make smart decisions
    * about when to split blocks.
    */

#define SLAB_RETENTION_TIME (60)
   /* Retention time of a fully free slab, in seconds.
    * If set as 0, slabs are deallocated immediately.
    */

#define SMALL_BLOCK_MIN (2)
   /* Minimum size of a small block in words */

#define SMALL_BLOCK_NUM (16)
   /* Number of different small block sizes.
    */

#define INIT_SMALL_BLOCK INIT16
   /* The proper initializer macro for all tables sized SMALL_BLOCK_NUM.
    */

/* Derived values */

#define SMALL_BLOCK_MIN_BYTES  (SMALL_BLOCK_MIN * SINT)
   /* Minimum payload size of a small block.
    */

#define SMALL_BLOCK_MAX (SMALL_BLOCK_NUM-1 + T_OVERHEAD + SMALL_BLOCK_MIN)
   /* The maximum size (incl. overhead) of a small block in words
    */

#define SMALL_BLOCK_MAX_BYTES  ((SMALL_BLOCK_NUM-1 + SMALL_BLOCK_MIN) * SINT)
   /* Maximum payload size of a small block.
    */


#define DESIRED_SLAB_SIZE 0x01000 /* 4 KByte */
   /* Desired slab size (or a multiple thereof).
    */


#ifdef SBRK_OK
#    define CHUNK_SIZE          0x40000
#else
    /* It seems to be advantagous to be just below a power of two
     * make sure that the resulting chunk sizes are SINT aligned.
     */
#    define CHUNK_SIZE    (0x40000 - SINT - EXTERN_MALLOC_OVERHEAD)
#endif


/* Bitflags for the size field:
 * TODO: Assumes a 32-Bit word_t.
 */
#define PREV_BLOCK  0x80000000  /* Previous block is allocated */
#define THIS_BLOCK  0x40000000  /* This block is allocated */
#define M_REF       0x20000000  /* Block is referenced */
#define M_GC_FREE   0x10000000  /* GC may free this block */
#define M_SMALL     0x08000000  /* Small block */
#define M_MASK      0x07ffffff  /* Mask for the size, measured in word_t's */


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
#    define ulog4f(s,t1,t2,t3,t4) \
       dprintf4(gcollect_outfd, s, (p_int)(t1), (p_int)(t2), (p_int)(t3), (p_int)t4)
#else
#    define ulog(s)                (void)0
#    define ulog1f(s,t)            (void)0
#    define ulog2f(s,t1,t2)        (void)0
#    define ulog3f(s,t1,t2,t3)     (void)0
#    define ulog4f(s,t1,t2,t3,t4)  (void)0
#endif

/*-------------------------------------------------------------------------*/

static size_t slaballoc_size;
  /* Size of the current mem_alloc() request.
   * It is used to print a diagnostic when the memory is running out.
   */

/* --- Small Block types --- */

/* --- struct slab_s: Slab header
 * This structure describes a slab, the block space follows immediately
 * afterwards.
 */

typedef struct slab_s slab_t;

struct slab_s
{
#ifdef MALLOC_CHECK
    word_t   magic;         /* Magic 'small' word based on .size. */
#endif
    size_t   size;          /* Block size (incl. overhead) for this slab in
                             * bytes. */
    slab_t * prev, * next;  /* Link pointers for slab list. */
    short    numAllocated;  /* Number of blocks allocated. */
    short    numBlocks;     /* Number of blocks available total. */
    word_t * freeList;      /* List of free blocks. */
    word_t   blocks[1];     /* First word of the allocatable memory. */
};

#define SLAB_SIZE(slab) \
    ( sizeof(struct slab_s) - SINT + (slab)->numBlocks * (slab)->size )
  /* Actual size of a given slab.
   */

/*--- struct slabentry_s: Slab table entry
 * This structure manages the slabs for a given block size.
 */

typedef struct slabentry_s slabentry_t;

struct slabentry_s
{
    slab_t * first;          /* Head of the slab list. */
    slab_t * last;           /* Last slab in the list. */
    slab_t * fullSlabs;      /* Head of the full slab list. */
    slab_t * firstFree;      /* Head of the free slab list. */
    slab_t * lastFree;       /* Last free slab in the list. */
    slab_t * fresh;          /* NULL, or the fresh slab. */
    word_t * freshMem;       /* NULL, or the end of the free mem in the fresh
                              * slab.
                              */
    unsigned long numSlabs;      /* Total Number of slabs for this size. */
    unsigned long numFullSlabs;  /* Number of full slabs in the list. */
    unsigned long numFreeSlabs;  /* Number of free slabs in the free list. */
};

#define INIT_SLABENTRY   { 0, 0, 0, 0, 0, 0, 0, 0, 0 }
#define INIT_SLABENTRY2  INIT_SLABENTRY, INIT_SLABENTRY
#define INIT_SLABENTRY4  INIT_SLABENTRY2, INIT_SLABENTRY2
#define INIT_SLABENTRY8  INIT_SLABENTRY4, INIT_SLABENTRY4
#define INIT_SLABENTRY16  INIT_SLABENTRY8, INIT_SLABENTRY8

/* --- Small Block variables --- */

static slabentry_t slabtable[SMALL_BLOCK_NUM] = { INIT_SLABENTRY16 };
  /* List of slabs of the various sizes.
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

static t_stat small_slab_stat = {0,0};
  /* Number and size of allocated slabs (incl. fully unused slabs).
   */

static t_stat small_slab_free_stat = {0,0};
  /* Number and size of allocated but completely unused slabs.
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

static char *esbrk(word_t, size_t * pExtra);

static char *large_malloc(word_t, Bool);
#define large_malloc_int(size, force_m) large_malloc(size, force_m)
static void large_free(char *);

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
#define EXTSTAT_SLABS (SMALL_BLOCK_NUM)
#define EXTSTAT_LARGE (SMALL_BLOCK_NUM+1)

static extstat_t extstats[SIZE_EXTSTATS];
  /* The statistics array.
   * [EXTSTAT_SLABS] is for the slabs combined; the semantics here are
   *   a bit different: while cur/max give the number of slabs in the
   *   used resp. free lists, the number of allocs/frees counts the
   *   actual memory (de)allocations of slabs in the large pool - not
   *   the transfers between the full and free lists.
   * [EXTSTAT_LARGE] is for the large blocks.
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
        count_up(perm_alloc_stat, (q[-M_OVERHEAD] & M_MASK)*SINT);
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
        count_back(perm_alloc_stat, (q[-M_OVERHEAD] & M_MASK)*SINT);
    }
} /* mem_mark_collectable() */

/*-------------------------------------------------------------------------*/
static INLINE size_t
mem_block_size (POINTER p)

/* Return the size of block <p> (sans internal overhead) in bytes.
 */

{
   word_t *q = ((word_t *) p) - M_OVERHEAD;

   if (q[M_SIZE] & M_SMALL)
   {
       slab_t * slab = (slab_t*)(q - (q[M_SIZE] & M_MASK));
       return slab->size - M_OVERHEAD*SINT;
   }
   return ((q[M_SIZE] & M_MASK)-M_OVERHEAD)*SINT;
} /* mem_block_size() */

/*-------------------------------------------------------------------------*/
static INLINE size_t
mem_overhead (void)

/* Return the size of each block's overhead in bytes.
 */

{
   return M_OVERHEAD*SINT;
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
void
mem_dump_data (strbuf_t *sbuf)

/* For the status commands and functions: add the slaballoc statistic
 * to the buffer <sbuf>.
 */

{
    t_stat sbrk_st, clib_st, perm_st, xalloc_st;
    t_stat l_alloc, l_free, l_wasted;
    t_stat s_alloc, s_free, s_slab, s_free_slab;
    unsigned long s_overhead;
#ifdef MALLOC_EXT_STATISTICS
    int i;
#endif /* MALLOC_EXT_STATISTICS */


    /* Get a snapshot of the statistics - strbuf_add() might do further
     * allocations while we're reading them.
     */

#ifdef MALLOC_EXT_STATISTICS
    mem_update_stats();
#endif /* MALLOC_EXT_STATISTICS */

    sbrk_st = sbrk_stat;
    clib_st = clib_alloc_stat;
    xalloc_st = xalloc_stat;
    perm_st = perm_alloc_stat;
    l_alloc = large_alloc_stat; l_alloc.size *= SINT;
    l_free = large_free_stat; l_free.size *= SINT;
    l_wasted = large_wasted_stat;
    s_alloc = small_alloc_stat;
    s_free = small_free_stat;
    s_slab = small_slab_stat; s_slab.size += s_slab.counter * M_OVERHEAD * SINT;
    s_free_slab = small_slab_free_stat;

    s_overhead = s_slab.counter * (sizeof(slab_t) - SINT + M_OVERHEAD * SINT);

#   define dump_stat(str,stat) strbuf_addf(sbuf, str,stat.counter,stat.size)

    strbuf_add(sbuf, "Type                   Count      Space (bytes)\n");
    dump_stat("xallocs:           %8u        %10lu\n\n", xalloc_st);
    dump_stat("sbrk requests:     %8u        %10lu (a)\n",sbrk_st);
    dump_stat("large blocks:      %8u        %10lu (b)\n",l_alloc);
    strbuf_addf(sbuf
               , "large net avail:                   %10ld\n"
               , l_alloc.size - l_alloc.counter * M_OVERHEAD * SINT
               );
    dump_stat("large free blocks: %8u        %10lu (c)\n",l_free);
    dump_stat("large wasted:      %8u        %10lu (d)\n\n",l_wasted);
    dump_stat("small slabs:       %8u        %10lu (e)\n",s_slab);
    dump_stat("small blocks:      %8u        %10lu (f)\n",s_alloc);
    strbuf_addf(sbuf
               , "small net avail:                   %10d\n"
               , s_alloc.size - s_alloc.counter * M_OVERHEAD * SINT
               );
    dump_stat("small free blocks: %8u        %10lu (g)\n",s_free);
    dump_stat("small free slabs:  %8u        %10lu    \n",s_free_slab);
    strbuf_addf(sbuf
               , "small overhead:                    %10lu (h)\n"
               , s_overhead
               );

    dump_stat("\npermanent blocks:  %8u        %10lu\n", perm_st);
#ifdef SBRK_OK
    dump_stat("clib allocations:  %8u        %10lu\n", clib_st);
#else
    strbuf_addf(sbuf, "clib allocations:       n/a               n/a\n");
#endif
    strbuf_add(sbuf, "\n");
#ifdef USE_AVL_FREELIST
    strbuf_addf(sbuf, "AVL nodes:         %8u                 -\n", num_avl_nodes);
    strbuf_add(sbuf, "\n");
#endif /* USE_AVL_FREELIST */

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
               , s_alloc.size + s_free.size + s_overhead
               , s_slab.size
               );
    strbuf_addf(sbuf
               , "Total storage in use: (b-g-h)     %10lu net available:   %10lu\n"
               , l_alloc.size - s_free.size - s_overhead
               , l_alloc.size - s_free.size - s_overhead
                 - l_alloc.counter * M_OVERHEAD * SINT
                 - s_alloc.counter * M_OVERHEAD * SINT
                 - xalloc_st.counter * XM_OVERHEAD_SIZE
               );
    strbuf_addf(sbuf
               , "Total storage unused: (c+d+g)     %10lu\n\n"
               , l_free.size + l_wasted.size + s_free.size
               );

#ifdef MALLOC_EXT_STATISTICS
    strbuf_add(sbuf,
      "\nDetailed Block Statistics:\n\n"
              );
    for (i = 0; i < SIZE_EXTSTATS; ++i)
    {
        if (i > 0)
            strbuf_add(sbuf, "\n");

        if (i < SMALL_BLOCK_NUM)
            strbuf_addf(sbuf, "  Size %3u: ", (i + SMALL_BLOCK_MIN) * SINT);
        else if (i == EXTSTAT_SLABS)
            strbuf_addf(sbuf, "  Slabs:    ");
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
        if (i < SMALL_BLOCK_NUM)
        {
            strbuf_addf(sbuf, "            "
                              "Slabs: %4lu partial, %4lu full, %4lu free = %4lu total\n"
                            , slabtable[i].numSlabs
                              - slabtable[i].numFullSlabs - slabtable[i].numFreeSlabs
                            , slabtable[i].numFullSlabs
                            , slabtable[i].numFreeSlabs
                            , slabtable[i].numSlabs
                       );
        }
    }
#endif /* MALLOC_EXT_STATISTICS */
} /* mem_dump_data() */

/*-------------------------------------------------------------------------*/
void
mem_dinfo_data (svalue_t *svp, int value)

/* Fill in the data for debug_info(DINFO_DATA, DID_MEMORY) into the
 * svalue-block svp.
 */

{
    unsigned long small_overhead;

#define ST_NUMBER(which,code) \
    if (value == -1) svp[which].u.number = code; \
    else if (value == which) svp->u.number = code

    if (value == -1)
        put_ref_string(svp+DID_MEM_NAME, STR_SLABALLOC);
    else if (value == DID_MEM_NAME)
        put_ref_string(svp, STR_SLABALLOC);

    small_overhead = small_slab_stat.counter
                     * (sizeof(slab_t) - SINT + M_OVERHEAD * SINT);

    ST_NUMBER(DID_MEM_SBRK, sbrk_stat.counter);
    ST_NUMBER(DID_MEM_SBRK_SIZE, sbrk_stat.size);
    ST_NUMBER(DID_MEM_LARGE, large_alloc_stat.counter);
    ST_NUMBER(DID_MEM_LARGE_SIZE, large_alloc_stat.size * SINT);
    ST_NUMBER(DID_MEM_LFREE, large_free_stat.counter);
    ST_NUMBER(DID_MEM_LFREE_SIZE, large_free_stat.size * SINT);
    ST_NUMBER(DID_MEM_LWASTED, large_wasted_stat.counter);
    ST_NUMBER(DID_MEM_LWASTED_SIZE, large_wasted_stat.size);
    ST_NUMBER(DID_MEM_SLAB, small_slab_stat.counter);
    ST_NUMBER(DID_MEM_SLAB_SIZE, small_slab_stat.size + small_slab_stat.counter * M_OVERHEAD * SINT);
    ST_NUMBER(DID_MEM_SLAB_FREE, small_slab_free_stat.counter);
    ST_NUMBER(DID_MEM_SLAB_FREE_SIZE, small_slab_free_stat.size);
    ST_NUMBER(DID_MEM_SMALL, small_alloc_stat.counter);
    ST_NUMBER(DID_MEM_SMALL_SIZE, small_alloc_stat.size);
    ST_NUMBER(DID_MEM_SFREE, small_free_stat.counter);
    ST_NUMBER(DID_MEM_SFREE_SIZE, small_free_stat.size);
    ST_NUMBER(DID_MEM_SMALL_OVERHEAD_SIZE, small_overhead);
    ST_NUMBER(DID_MEM_MINC_CALLS, malloc_increment_size_calls);
    ST_NUMBER(DID_MEM_MINC_SUCCESS, malloc_increment_size_success);
    ST_NUMBER(DID_MEM_MINC_SIZE, malloc_increment_size_total);
    ST_NUMBER(DID_MEM_CLIB, clib_alloc_stat.counter);
    ST_NUMBER(DID_MEM_CLIB_SIZE, clib_alloc_stat.size);
    ST_NUMBER(DID_MEM_PERM, perm_alloc_stat.counter);
    ST_NUMBER(DID_MEM_PERM_SIZE, perm_alloc_stat.size);
    ST_NUMBER(DID_MEM_OVERHEAD, T_OVERHEAD * SINT);
    ST_NUMBER(DID_MEM_ALLOCATED, large_alloc_stat.size * SINT
                              - small_free_stat.size
                              - small_overhead);
    ST_NUMBER(DID_MEM_USED, large_alloc_stat.size * SINT
                              - small_free_stat.size
                              - small_overhead
                              - large_alloc_stat.counter * M_OVERHEAD * SINT
                              - small_alloc_stat.counter * M_OVERHEAD * SINT
                              - xalloc_stat.counter * XM_OVERHEAD_SIZE
             );
    ST_NUMBER(DID_MEM_TOTAL_UNUSED, large_free_stat.size * SINT
                                    + large_wasted_stat.size
                                    + small_free_stat.size
             );
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
        top = allocate_array(SIZE_EXTSTATS);
        
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
            if  (i < SMALL_BLOCK_NUM)
            {
                put_number(&sub->item[DID_MEM_ES_FULL_SLABS],  slabtable[i].numFullSlabs);
                put_number(&sub->item[DID_MEM_ES_FREE_SLABS],  slabtable[i].numFreeSlabs);
                put_number(&sub->item[DID_MEM_ES_TOTAL_SLABS], slabtable[i].numSlabs);
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

/*-------------------------------------------------------------------------*/
#ifdef CHECK_MAPPING_TOTAL
mp_int
available_memory(void)

/* Return the amount of memory actually used by the driver. */

{
    return large_alloc_stat.size * SINT
           - small_free_stat.size
           - large_alloc_stat.counter * M_OVERHEAD * SINT
           - small_alloc_stat.counter * M_OVERHEAD * SINT;
} /* available_memory() */
#endif /* CHECK_MAPPING_TOTAL */

/*=========================================================================*/

/*                            SMALL BLOCKS                                 */

/*-------------------------------------------------------------------------*/

#define SIZE_INDEX(size) \
      ((size)/SINT - T_OVERHEAD - SMALL_BLOCK_MIN)
    /* Index to the proper array entry for a small
     * block of <size> (including overhead).
     */

#define SIZE_MOD_INDEX(size, table) \
      (((size)/SINT - T_OVERHEAD - SMALL_BLOCK_MIN) % NELEM(table))
    /* Index to the proper array entry for a small
     * block of <size> (including overhead), limited to the size of <table>.
     */

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
            fatal("allocation from free list for %lu bytes: " \
                  "block %p magic match failed, " \
                  "expected %08lx, found %08lx\n" \
                 , (unsigned long) size, block \
                 , sfmagic[SIZE_MOD_INDEX(size, sfmagic)] \
                 , block[M_MAGIC]); \
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
#ifdef MALLOC_ORDER_SMALL_FREELISTS

static void
keep_small_order (slab_t * slab, int ix)

/* Slab <slab> is a partially used slab in slabtable entry <ix>.
 * Check it's position and move it to a more approriate place
 * if necessary.
 */

{
    /*The threshold is if this slab has more free blocks than the next one.
     */
    if (slab->next
     && slab->numAllocated < slab->next->numAllocated
       )
    {
        /* Find new insertion position (pointing to the slab after
         * which to insert).
         * Since keeping a total order has linear complexity, we just
         * keep a local order by switch the current slab and its successor.
         * Kinda like a fuzzy incremental bubble sort :-)
         */
        slab_t * point = slab->next;

        ulog4f("slaballoc: need reorder: this %x free %d, next %x free %d\n"
              , slab, slab->numBlocks - slab->numAllocated
              , slab->next, slab->next->numBlocks - slab->next->numAllocated
              );
#if 0
#ifdef DEBUG_MALLOC_ALLOCS
        {
            slab_t * p = slabtable[ix].first;

            ulog3f("slaballoc:   [%d]: %x (%d)"
                 , ix, p, p->numBlocks - p->numAllocated
                 );
            while (p->next != NULL)
            {
                slab_t * prev = p;
                p = p->next;
                ulog2f(" -> %x (%d)"
                     , p, p->numBlocks - p->numAllocated
                     );
                if (prev != p->prev)
                    fatal("Inconsistent freelist pointer: prev %p should be %p\n"
                         , p->prev, prev
                        );
            }
            ulog("\n");
        }
#endif

        while (point->next
            && point->next->numAllocated > slab->numAllocated
              )
            point = point->next;

#ifdef DEBUG_MALLOC_ALLOCS
        if (point->next)
            ulog4f("slaballoc:   point %x free %d, point->next %x free %d\n"
                  , point, point->numBlocks - point->numAllocated
                  , point->next, point->next->numBlocks - point->next->numAllocated
                  );
        else
            ulog2f("slaballoc:   point %x free %d, no next\n"
                  , point, point->numBlocks - point->numAllocated
                  );
#endif /* DEBUG_MALLOC_ALLOCS */
#endif

        /* Unlink slab */
        if (slab->next)
            slab->next->prev = slab->prev;
        if (slab->prev)
            slab->prev->next = slab->next;
        if (slabtable[ix].first == slab)
            slabtable[ix].first = slab->next;
        if (slabtable[ix].last == slab)
            slabtable[ix].last = slab->prev;

        /* Relink slab */
        slab->next = point->next;
        slab->prev = point;
        if (point->next)
            point->next->prev = slab;
        point->next = slab;
        if (slabtable[ix].last == point)
            slabtable[ix].last = slab;

#ifdef DEBUG_MALLOC_ALLOCS
        {
            slab_t * p = slabtable[ix].first;

            ulog3f("slaballoc:   [%d]: %x (%d)"
                 , ix, p, p->numBlocks - p->numAllocated
                 );
            while (p->next != NULL)
            {
                slab_t * prev = p;
                p = p->next;
                ulog2f(" -> %x (%d)"
                     , p, p->numBlocks - p->numAllocated
                     );
                if (prev != p->prev)
                    fatal("Inconsistent freelist pointer: prev %p should be %p\n"
                         , p->prev, prev
                        );
            }
            ulog("\n");
        }
#endif
    }
#ifdef DEBUG_MALLOC_ALLOCS
    else if (slab->next)
        ulog4f("slaballoc: no reorder: this %x free %d, next %x free %d\n"
              , slab, slab->numBlocks - slab->numAllocated
              , slab->next, slab->next->numBlocks - slab->next->numAllocated
              );
    else
        ulog2f("slaballoc: no reorder: this %x free %d, no next\n"
              , slab, slab->numBlocks - slab->numAllocated
              );
#endif /* DEBUG_MALLOC_ALLOCS */
} /* keep_small_order() */

#else

#define keep_small_order(slab,ix) NOOP

#endif /* MALLOC_ORDER_SMALL_FREELISTS */

/*-------------------------------------------------------------------------*/
static void
insert_partial_slab (slab_t * slab, int ix)

/* Insert the given <slab> into the partially-used list of slabtable entry
 * <ix>.
 *
 * No statistics or unlinking are handled, this is just the insertion itself.
 */
{
    ulog4f("slaballoc: insert partial slab %x into [%d]: first %x, last %x\n"
          , slab, ix, slabtable[ix].first, slabtable[ix].last
          );
    if (NULL == slabtable[ix].first)
    {
        slabtable[ix].first = slabtable[ix].last = slab;
        slab->next = slab->prev = NULL;
    }
    else
    {
#ifdef MALLOC_ORDER_SMALL_FREELISTS
        /* Insert at front and let the ordering mechanism kick in
         * later.
         */
        slab->next = slabtable[ix].first;
        slab->prev = NULL;
        slabtable[ix].first->prev = slab;
        slabtable[ix].first = slab;

        keep_small_order(slab, ix);
#else
        /* Insert at back for FIFO handling. */
        slab->prev = slabtable[ix].last;
        slab->next = NULL;
        slabtable[ix].last->next = slab;
        slabtable[ix].last = slab;
#endif
    }
} /* insert_partial_slab() */

/*-------------------------------------------------------------------------*/
static void
free_slab (slab_t * slab, int ix)

/* Free the <slab> which belongs to slabtable entry <ix>.
 * The slab has already been unlinked, all there is left to do is
 * adjusting the generic statistics and deallocating the memory.
 */

{
    ulog2f("slaballoc: deallocate slab %x [%d]\n", slab, ix);
    slabtable[ix].numSlabs--;
    count_back(small_slab_stat, SLAB_SIZE(slab));
    count_back_n(small_free_stat, slab->numBlocks, slab->size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[SIZE_INDEX(slab->size)].cur_free -= slab->numBlocks;
    extstats[EXTSTAT_SLABS].cur_free--;
    extstats[EXTSTAT_SLABS].num_xfree++;
    extstat_update_max(extstats + EXTSTAT_SLABS);
#endif /* MALLOC_EXT_STATISTICS */
    large_free((char*)slab);
} /* free_slab() */

/*-------------------------------------------------------------------------*/
static POINTER
mem_alloc (size_t size)

/* Allocate a memory block for <size> bytes at the source <file>:<line>.
 * Result is the pointer the memory block, or NULL when out of memory.
 */

{
    word_t *block = NULL;
    int     ix;
#if defined(HAVE_MADVISE)
    size_t orig_size = size;
#endif

    assert_stack_gap();

    slaballoc_size = size;

    if (size == 0)
    {
        size++;
    }

    /* TODO: For the following test, see SIZET_limits in port.h */
    if (size >= ULONG_MAX - (T_OVERHEAD+SMALL_BLOCK_MIN)*SINT
     || size >= (M_MASK + 1 - (T_OVERHEAD+SMALL_BLOCK_MIN))*SINT
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
        writes(1,"Multiple threads in slaballoc()\n");
        fatal("Multiple threads in slaballoc()\n");
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

    ulog2f("slaballoc: alloc (%d + %d)", size, M_OVERHEAD*SINT);
    size = (size+M_OVERHEAD*SINT+SINT-1) & ~(SINT-1);
    ix = SIZE_INDEX(size);

    ulog2f(" %d bytes -> [%d]\n", size, ix);

    /* Update statistics */
    count_up(small_alloc_stat,size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[SIZE_INDEX(size)].num_xalloc++;
    extstats[SIZE_INDEX(size)].cur_alloc++;
#endif /* MALLOC_EXT_STATISTICS */

    /* Try allocating the block from the first partially used slab.
     */
    if (NULL != slabtable[ix].first)
    {
        /* Yup, we can. */

        slab_t * slab = slabtable[ix].first;

        ulog4f("slaballoc:   block %x from freelist (next %x), slab %x free %d\n"
              , slab->freeList, BLOCK_NEXT(slab->freeList), slab, slab->numBlocks - slab->numAllocated
              );

        block = slab->freeList;
        slab->freeList = BLOCK_NEXT(slab->freeList);
        count_back(small_free_stat, slab->size);

#ifdef MALLOC_EXT_STATISTICS
        extstats[ix].cur_free--;
#endif /* MALLOC_EXT_STATISTICS */

        slab->numAllocated++;

        /* If the slab is now fully used, move it into the full slab list.
         */
        if (NULL == slab->freeList)
        {
            /* Unlink from partially-used list */

            ulog3f("slaballoc:   fully used: move from partial (next %x, last %x) to full (%x)\n"
                  , slab->next, slabtable[ix].last, slabtable[ix].fullSlabs
                  );

            if (slabtable[ix].first == slabtable[ix].last)
            {
                /* Only one slab in the list */
                slabtable[ix].first = slabtable[ix].last = NULL;
            }
            else
            {
                /* At least two slabs in the list. */
                slabtable[ix].first = slab->next;
                slabtable[ix].first->prev = NULL;
            }

            /* Link into fully-used list */

            slab->prev = NULL;
            slab->next = slabtable[ix].fullSlabs;
            if (slab->next)
                slab->next->prev = slab;
            slabtable[ix].fullSlabs = slab;
            slabtable[ix].numFullSlabs++;
        }

        /* Setup the block (M_SIZE) is mostly ok. */
        MAKE_SMALL_CHECK(block, size);
        block[M_SIZE] |= (M_GC_FREE|M_REF);
        block[M_SIZE] &= ~THIS_BLOCK;

        block += M_OVERHEAD;

        MADVISE(block, orig_size);

#ifdef MALLOC_EXT_STATISTICS
        extstat_update_max(extstats + SIZE_INDEX(size));
#endif /* MALLOC_EXT_STATISTICS */

        in_malloc--;
        return (POINTER)block;
    }

    /* If there is no fresh slab, allocate one. */
    if (NULL == slabtable[ix].fresh)
    {
        slab_t * slab;

        /* If there are slabs in the fully-free list, use
         * the oldest one from there. Otherwise, allocated
         * from the large memory pool.
         */
        if (NULL != slabtable[ix].firstFree)
        {

            /* Unlink the last (oldest) slab from the free list and hold it.
             */
            slab = slabtable[ix].lastFree;

            ulog3f("slaballoc:   get fresh %x from free list (prev %x, first %x)\n"
                  , slab, slab->prev, slabtable[ix].firstFree
                  );

            if (slabtable[ix].firstFree == slabtable[ix].lastFree)
            {
                slabtable[ix].firstFree = slabtable[ix].lastFree = NULL;
            }
            else
            {
                slabtable[ix].lastFree = slab->prev;
                slabtable[ix].lastFree->next = NULL;
            }

            slabtable[ix].numFreeSlabs--;
            count_back(small_slab_free_stat, SLAB_SIZE(slab));
#ifdef MALLOC_EXT_STATISTICS
            extstats[EXTSTAT_SLABS].cur_free--;
#endif /* MALLOC_EXT_STATISTICS */
        }

        else
        {
            /* Allocate a new slab from the large memory pool. */

            short numObjects;
            size_t slabSize;

            /* Determined the number of objects such that the slab
             * is roughly a multiple of DESIRED_SLAB_SIZE large,
             * and can hold at least 100 objects.
             */
            {
#ifdef SLABALLOC_DYNAMIC_SLABS
                size_t minSlabObjectSize = DESIRED_SLAB_SIZE / 128;
                size_t sizeMultiplier = 1 + (size-1) / minSlabObjectSize;
                size_t totalSlabSize = DESIRED_SLAB_SIZE * sizeMultiplier;
                size_t effSlabSize = totalSlabSize - sizeof(slab_t) - SINT;
#else
                size_t totalSlabSize = DESIRED_SLAB_SIZE;
                size_t effSlabSize = totalSlabSize - sizeof(slab_t) - SINT;
#endif
                numObjects = (short)(effSlabSize / size);
            }

            slabSize = sizeof(struct slab_s) - SINT + numObjects * size;

            slab = (slab_t*)large_malloc_int(slabSize, MY_FALSE);

            if (slab == NULL)
            {
                dprintf1(2, "Low on MEMORY: Failed to allocated small slab "
                            "of %d bytes.\n"
                          , (p_int)(slabSize)
                        );
                in_malloc--;
                return NULL;
            }

            ulog4f("slaballoc:   allocated fresh %x size %d, objects %d size %d\n"
                  , slab, slabSize, numObjects, size
                  );

            slab->size = size;
            slab->numBlocks = numObjects;

            slabtable[ix].numSlabs++;
            count_up(small_slab_stat, slabSize);
            count_up_n(small_free_stat, numObjects, size);
#ifdef MALLOC_EXT_STATISTICS
            extstats[ix].cur_free += numObjects;
            extstats[EXTSTAT_SLABS].num_xalloc++;
#endif /* MALLOC_EXT_STATISTICS */
        }

        /* (Re)initialize the remaining slab fields. */
#ifdef MALLOC_CHECK
        slab->magic = samagic[SIZE_MOD_INDEX(slab->size, samagic)];
#endif
        slab->next = slab->prev = NULL;
        slab->freeList = NULL;
        slab->numAllocated = 0;

        slabtable[ix].fresh = slab;
        slabtable[ix].freshMem = slab->blocks + (slab->numBlocks * slab->size / SINT);

        ulog3f("slaballoc:   fresh %x mem %x..%x\n"
              , slab, slab->blocks, slabtable[ix].freshMem
              );

#ifdef MALLOC_EXT_STATISTICS
        extstats[EXTSTAT_SLABS].cur_alloc++;
        extstat_update_max(extstats + EXTSTAT_SLABS);
#endif /* MALLOC_EXT_STATISTICS */
    }

    /* Allocate a new block from the fresh slab's memory.
     */
    {
        word_t wsize = size / SINT;

        slabtable[ix].freshMem -= wsize;
        block = slabtable[ix].freshMem;
        slabtable[ix].fresh->numAllocated++;

        ulog3f("slaballoc:   freshmem %x from %x..%x\n"
              , block, slabtable[ix].fresh->blocks, ((char *)slabtable[ix].fresh->blocks) + (slabtable[ix].fresh->numBlocks * slabtable[ix].fresh->size)
              );
        block[M_SIZE] =   (word_t)(block - (word_t *)(slabtable[ix].fresh))
                        | (M_SMALL|M_GC_FREE|M_REF);
        MAKE_SMALL_CHECK_UNCHECKED(block, size);
        block += M_OVERHEAD;

        MADVISE(block, orig_size);

        count_back(small_free_stat, size);
#ifdef MALLOC_EXT_STATISTICS
        extstats[ix].cur_free--;
#endif /* MALLOC_EXT_STATISTICS */
    }

    /* We have the block allocated, now check if the fresh slab has been used up.
     * If yes, move the fresh slab into the appropriate slab: front if some
     * of the blocks have already been freed, back if the slab is full.
     */
    if (slabtable[ix].freshMem == slabtable[ix].fresh->blocks)
    {
        slab_t * slab = slabtable[ix].fresh;

        ulog2f("slaballoc:   fresh %x full, freelist %x\n"
              , slab, slab->freeList
              );
        slabtable[ix].fresh = NULL;
        slabtable[ix].freshMem = NULL;

        if (NULL != slab->freeList)
        {
            insert_partial_slab(slab, ix);
        }
        else
        {
            /* Insert in fully-used list */
            ulog2f("slaballoc:   fresh %x into full list (first %x)\n"
                  , slab, slabtable[ix].fullSlabs
                  );
            slab->next = slabtable[ix].fullSlabs;
            if (NULL != slabtable[ix].fullSlabs)
                slabtable[ix].fullSlabs->prev = slab;
            slabtable[ix].fullSlabs = slab;
            slabtable[ix].numFullSlabs++;
        }
    }

#ifdef MALLOC_EXT_STATISTICS
    extstat_update_max(extstats + ix);
#endif /* MALLOC_EXT_STATISTICS */

    /* Done! */
    in_malloc--;
    return (POINTER)block;
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
    word_t ix;
    slab_t *slab;
    Bool   isFirstFree;

    if (!ptr)
        return;

    mem_mark_collectable(ptr);

    /* Get the real block address and type */
    block = (word_t *) ptr;
    block -= M_OVERHEAD;

    ulog4f("slaballoc: free %x, %s %d, small %d\n"
          , block
          , (block[M_SIZE] & M_SMALL) ? "offset" : "size"
          , block[M_SIZE] & M_MASK
          , !!(block[M_SIZE] & M_SMALL)
          );

    if (!(block[M_SIZE] & M_SMALL))
    {
        /* It's a big block */
        large_free(ptr);
        return;
    }

    /* It's a small block: put it into the slab's free list */

    slab = (slab_t*)(block - (block[M_SIZE] & M_MASK));
    count_back(small_alloc_stat, slab->size);
    ix =  SIZE_INDEX(slab->size);

    ulog4f("slaballoc:   -> slab %x [%d], freelist %x, %d free\n"
          , slab, ix, slab->freeList, slab->numBlocks - slab->numAllocated
          );
#ifdef MALLOC_EXT_STATISTICS
    extstats[ix].num_xfree++;
    extstats[ix].cur_alloc--;
#endif /* MALLOC_EXT_STATISTICS */

#ifdef MALLOC_CHECK
    if (slab->magic == sfmagic[ix % NELEM(sfmagic)])
    {
        in_malloc = 0;
        fatal("mem_free: block %lx size %lu (user %lx) freed in free slab %lx\n"
             , (unsigned long)block, (unsigned long)(ix * SINT)
             , (unsigned long)ptr, (unsigned long) slab);
    }
    if (slab->magic != samagic[ix % NELEM(samagic)])
    {
        in_malloc = 0;
        fatal("mem_free: block %p magic match failed for slab %lx: "
              "size %lu, expected %lx, found %lx\n"
             , block, (unsigned long)slab
             , (unsigned long)(ix * SINT), samagic[ix], slab->magic);
    }
    if (block[M_MAGIC] == sfmagic[ix % NELEM(sfmagic)])
    {
        in_malloc = 0;
        fatal("mem_free: block %lx size %lu (user %lx) freed twice\n"
             , (unsigned long)block, (unsigned long)(ix * SINT)
             , (unsigned long)ptr);
    }
    if (block[M_MAGIC] != samagic[ix % NELEM(samagic)])
    {
        in_malloc = 0;
        fatal("mem_free: block %p magic match failed: "
              "size %lu, expected %lx, found %lx\n"
             , block, (unsigned long)(ix * SINT), samagic[ix], block[M_MAGIC]);
    }
#endif

    /* Insert the block into the slab's freelist */

    isFirstFree = (slab->freeList == NULL);

#ifdef MALLOC_EXT_STATISTICS
    extstats[ix].cur_free++;
    extstat_update_max(extstats + ix);
#endif /* MALLOC_EXT_STATISTICS */

    block[M_SIZE] |= (THIS_BLOCK|M_REF);
    block[M_SIZE] &= ~(M_GC_FREE);
#ifdef MALLOC_CHECK
    block[M_MAGIC] = sfmagic[SIZE_MOD_INDEX(slab->size, sfmagic)];
#endif

    SET_BLOCK_NEXT(block, slab->freeList);
    slab->freeList = block;
    slab->numAllocated--;

    count_up(small_free_stat, slab->size);

    /* If this slab is not the fresh slab, handle possible list movements.
     */
    if (slab != slabtable[ix].fresh)
    {
        if (isFirstFree)
        {
            /* First free block: move slab into the partially-used
             * list.
             */

            ulog2f("slaballoc:   first free: unlink %x from full (first %x)\n"
                  , slab, slabtable[ix].fullSlabs
                  );
            if (slabtable[ix].fullSlabs == slab)
                slabtable[ix].fullSlabs = slab->next;
            if (slab->next)
                slab->next->prev = slab->prev;
            if (slab->prev)
                slab->prev->next = slab->next;
            slabtable[ix].numFullSlabs--;

            insert_partial_slab(slab, ix);
        }
        else if (slab->numAllocated == 0)
        {
            /* Slab completely free: unlink from list and move over
             * to free list (deallocate if retention time is 0).
             */
            ulog3f("slaballoc:   all free: unlink %x from partial (first %x, last %x)\n"
                  , slab, slabtable[ix].first, slabtable[ix].last
                  );
            if (slab->next)
                slab->next->prev = slab->prev;
            if (slab->prev)
                slab->prev->next = slab->next;

            if (slabtable[ix].first == slab)
                slabtable[ix].first = slab->next;
            if (slabtable[ix].last == slab)
                slabtable[ix].last = slab->prev;

#           if SLAB_RETENTION_TIME > 0
                ulog3f("slaballoc:   current time %d, free (first %x, last %x)\n"
                      , current_time, slabtable[ix].firstFree, slabtable[ix].lastFree
                      );
#ifdef MALLOC_CHECK
                slab->magic = sfmagic[SIZE_MOD_INDEX(slab->size, sfmagic)];
#endif
                slab->blocks[0] = (mp_int)current_time;
                slab->prev = NULL;
                slab->next = slabtable[ix].firstFree;
                if (slab->next)
                    slab->next->prev = slab;
                else
                    slabtable[ix].lastFree = slab;
                slabtable[ix].firstFree = slab;

                slabtable[ix].numFreeSlabs++;
                count_up(small_slab_free_stat, SLAB_SIZE(slab));
#ifdef MALLOC_EXT_STATISTICS
                extstats[EXTSTAT_SLABS].cur_alloc--;
                extstats[EXTSTAT_SLABS].cur_free++;
                extstat_update_max(extstats + EXTSTAT_SLABS);
#endif /* MALLOC_EXT_STATISTICS */
#           else
                free_slab(slab, ix);
#           endif
        }
#ifdef MALLOC_ORDER_SMALL_FREELISTS
        else
        {
            /* Another block freed in the slab: check if its position
             * in the list needs to be updated.
             */
            keep_small_order(slab, ix);
        }
#endif /* MALLOC_ORDER_SMALL_FREELISTS */
    } /* if (not fresh slab) */
#ifdef DEBUG_MALLOC_ALLOCS
    else
        ulog("slaballoc:   is fresh slab\n");
#endif /* DEBUG_MALLOC_ALLOCS */
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

#define l_size_ptr(p)    (p)

#define l_next_ptr(p)    (*((word_t **) ((p)+M_OVERHEAD)))
#define l_prev_ptr(p)    (*((word_t **) (p+2)))
  /* Non-AVL-Freelist pointers.
   */

#define l_next_block(p)  ((p) + (M_MASK & (*(p))) )
#define l_prev_block(p)  ((p) - (M_MASK & (*((p)-1))) )
  /* Address the previous resp. this block.
   */

#define l_prev_free(p)   (!(*p & PREV_BLOCK))
#define l_next_free(p)   (!(*l_next_block(p) & THIS_BLOCK))
  /* Check if the previous resp. the next block is free.
   */

/*-------------------------------------------------------------------------*/

/* Extra types and definitions for the AVL routines */

#if defined (sun) || defined(AMIGA) || defined(__linux__) || defined(__BEOS__)
    /* there is a type signed char */
    typedef signed char balance_t;
#   define BALANCE_T_BITS 8
#else
    typedef short balance_t;
#   define BALANCE_T_BITS 16
#endif
#if defined(sparc) || defined(AMIGA)
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

        q = ((word_t *)p)-M_OVERHEAD;
        q += *q & M_MASK;
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

    p = (word_t *)(((char *)m)-M_OVERHEAD*SINT);
    size = *p & M_MASK;
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
    count_back(large_free_stat, p->size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[EXTSTAT_LARGE].cur_free--;
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
    size = *ptr & M_MASK;
#ifdef DEBUG_AVL
    dprintf1(2, "size:%d\n",size);
#endif
    q = (struct free_block *)size; /* this assignment is just a hint for
                                    * register choice
                                    */
    r = (struct free_block *)(ptr+M_OVERHEAD);
    count_up(large_free_stat, size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[EXTSTAT_LARGE].cur_free++;
    extstat_update_max(extstats+EXTSTAT_LARGE);
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

    tmp = (*ptr & PREV_BLOCK) | size;
    *(ptr+size-1) = size;        /* copy the size information */
    *(ptr) = tmp;                /* marks this block as free */
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
static char *
large_malloc ( word_t size, Bool force_more)

/* Allocate a large or <size> bytes.
 *
 * If <force_more> is TRUE, the function first tries to allocate
 * more memory directly from the system. If that fails, it tries a normal
 * allocation. This feature may be used to allocate small chunks for the
 * small block allocator (currently it is not).
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

    size = (size + SINT*M_OVERHEAD + SINT-1) / SINT; /* incl. overhead */

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

        block_size = size*SINT;

        /* If force_more is true (read: we have to allocate a small chunk)
         * or if the if the requested block would leave only a 'small'
         * block or no block in the usual CHUNK_SIZEd chunk, then allocate
         * exactly the block requested. Otherwise allocate a CHUNK_SIZEd
         * chunk, of which the unneeded part is entered into the freelist.
         */

        if (force_more
         || block_size > CHUNK_SIZE - SMALL_BLOCK_MAX_BYTES - T_OVERHEAD*SINT )
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
#           if SMALL_BLOCK_MAX_BYTES > ALLOC_MULTIPLE
                chunk_size += SMALL_BLOCK_MAX_BYTES + 2 * ALLOC_MULTIPLE;
#           else
                chunk_size += ALLOC_MULTIPLE;
#           endif

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
             && (    (mp_int)(sbrk_stat.size + (heap_start ? 0 : SINT)) >= max_malloced
                 || (chunk_size = max_malloced - sbrk_stat.size
                                               - (heap_start?0:SINT) )
                     < block_size
                )
               )
            {
                static const char mess[] = "MAX_MALLOCED limit reached.\n";
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
        block_size = chunk_size / SINT;

        /* configure header info on chunk */
        build_block(ptr, block_size);

        add_to_free_list(ptr);
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
    real_size = *ptr & M_MASK;

    if (real_size - size)
    {
        /* split block pointed to by ptr into two blocks */

        ptr[size] = 0; /* Init the header word */
        build_block(ptr+size, real_size-size);
#ifdef DEBUG
        if (real_size - size <= SMALL_BLOCK_MAX)
        {
            dprintf2(2,"DEBUG: lmalloc(%d / %d): "
                      , orig_size, size * SINT);
            dprintf2(2
                    , "Split off block of %d bytes, small limit is %d bytes.\n"
                    , (p_int)(real_size - size) * SINT
                    , (p_int)SMALL_BLOCK_MAX * SINT);
#ifdef DEBUG_MALLOC_ALLOCS
            if (gcollect_outfd != 2)
            {
                dprintf2(gcollect_outfd
                        ,"DEBUG: lmalloc(%d / %d): "
                        , orig_size, size * SINT);
                dprintf2(gcollect_outfd
                        , "Split off block of %d bytes, small limit is %d bytes.\n"
                        , (p_int)(real_size - size) * SINT
                        , (p_int)SMALL_BLOCK_MAX * SINT);
            }
#endif
        }
#endif

#       ifndef SBRK_OK
        /* When we allocate a new chunk, it might differ slightly in size from
         * the desired size.
         */
        if (real_size - size <= SMALL_BLOCK_MAX)
        {
            mark_block(ptr+size);
            *(ptr+size) &= ~M_GC_FREE; /* Hands off, GC! */
            count_up(large_wasted_stat, (*(ptr+size) & M_MASK) * SINT);
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
    count_up(large_alloc_stat, size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[EXTSTAT_LARGE].num_xalloc++;
    extstats[EXTSTAT_LARGE].cur_alloc++;
    extstat_update_max(extstats+EXTSTAT_LARGE);
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
    size = *p & M_MASK;
    count_back(large_alloc_stat, size);
#ifdef MALLOC_EXT_STATISTICS
    extstats[EXTSTAT_LARGE].num_xfree++;
    extstats[EXTSTAT_LARGE].cur_alloc--;
#endif /* MALLOC_EXT_STATISTICS */

#ifdef MALLOC_CHECK
    if (p[M_MAGIC] == LFMAGIC)
    {
        in_malloc = 0;
        fatal("large_free: block %lx size %lu, (user %lx) freed twice\n"
             , (unsigned long)p, (unsigned long)(size * SINT)
             , (unsigned long)ptr);
    }
    if (p[M_MAGIC] != LAMAGIC)
    {
        in_malloc = 0;
        fatal("large_free(%p): block %p magic match failed: size %lu, "
              "expected %lx, found %lx\n"
             , ptr, p
             , (unsigned long)(size * SINT)
             , (unsigned long)LAMAGIC
             , (unsigned long)p[M_MAGIC]
             );
    }
#endif

    /* If the next block is free, coagulate */
    if (!(*(p+size) & THIS_BLOCK))
    {
        remove_from_free_list(p+size);
        size += (*(p+size) & M_MASK);
        *p = (*p & PREV_BLOCK) | size;
    }

    /* If the previous block is free, coagulate */
    if (l_prev_free(p))
    {
        remove_from_free_list(l_prev_block(p));
        size += (*l_prev_block(p) & M_MASK);
        p = l_prev_block(p);
    }

    /* Mark the block as free and add it to the freelist */
    build_block(p, size);
    add_to_free_list(p);
} /* large_free() */

/*-------------------------------------------------------------------------*/
static char *
esbrk (word_t size, size_t * pExtra)

/* Allocate a block of <size> bytes from the system and return its pointer.
 * If esbrk() allocated a larger block, the difference to <size> is
 * returned in *<pExtra>.
 *
#ifdef SBRK_OK
 * It is system dependent how sbrk() aligns data, so we simpy use brk()
 * to insure that we have enough.
 * At the end of the newly expanded heap we create a fake allocated
 * block of 0 bytes so that large_malloc() knows where to stop.
#else
 * Use malloc() to allocate a new block of memory. If this block borders
 * to the previous one, both blocks are joined.
 * The allocated block (modulo joints) is tagged at both ends with fake
 * "allocated" blocks of which cover the unallocated areas - large_malloc()
 * will perceive this as a fragmented heap.
#endif
 */

{
#ifdef SBRK_OK
#ifdef SunOS4
    extern char *sbrk();
    extern int brk();
#endif

    mdb_log_sbrk(size);
    *pExtra = 0;
    if (!heap_end)
    {
        /* First call: allocate the first fake block */
        heap_start = heap_end = (word_t *)sbrk(0);
        if (!esbrk(SINT, pExtra))
        {
            in_malloc = 0;
            fatal("Couldn't malloc anything\n");
        }
        *heap_start = PREV_BLOCK;
        count_up(large_wasted_stat, SINT);
        assert_stack_gap();
    }

    /* Get the new block */
    if ((int)brk((char *)heap_end + size) == -1)
        return NULL;

    count_up(sbrk_stat, size);
    heap_end = (word_t*)((char *)heap_end + size);
    heap_end[-1] = THIS_BLOCK;
    return (char *)(heap_end - 1) - size; /* overlap old memory block */

#else  /* not SBRK_OK */

    char *block;
    word_t *p;    /* start of the fake block */
    const int overhead = T_OVERHEAD;
    size_t overlap = 0;
      /* How much extra memory esbrk() could recycle from joining
       * the new allocation with the old one.
       */


    mdb_log_sbrk(size);
    size += overhead * SINT;  /* for the extra fake "allocated" block */

    block = malloc(size);
    if (!block)
    {
        *pExtra = 0;
        return NULL;
    }
    assert_stack_gap();

    /* p points to the start of the fake allocated block used
     * as sentinel/bridge
     */
    p = (word_t *)(block + size) - overhead;

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
        *(word_t *)block = PREV_BLOCK;
        p[M_SIZE] = THIS_BLOCK | overhead; /* no M_GC_FREE */
        overlap = 0;
    }
    else
    {
        /* Try to join with the existing heap */
        if (block < (char *)heap_start)
        {
            /* New block before the known heap */

            *(word_t *)block = PREV_BLOCK; /* Lower sentinel */
            if (block + size == (char *)heap_start)
            {
                /* We can join with the existing heap */
                p[overhead] &= ~PREV_BLOCK;
                overlap = SINT;
                count_back(large_wasted_stat, overlap);
            }
            else
            {
                /* Separate from the heap */
                p[M_SIZE] = THIS_BLOCK | (heap_start - p); /* no M_GC_FREE */
                overlap = 0;
            }

            heap_start = (word_t *)block;
        }
        else if (block >= (char *)heap_end)
        {
            /* New block after the known heap */

            p[M_SIZE] = THIS_BLOCK | overhead; /* no M_GC_FREE */
            if (block == (char *)heap_end)
            {
                /* We can join with the existing heap */
                heap_end = (word_t *)(block + size);
                block -= overhead;
                overlap = overhead * SINT;
                count_back(large_wasted_stat, overlap);
            }
            else
            {
                /* Separate from the heap */
                p = (word_t *)heap_end - overhead;
                p[M_SIZE] = (p[M_SIZE] & (PREV_BLOCK|THIS_BLOCK|M_GC_FREE))
                            | ((word_t *)block - p);
                heap_end = (word_t *)(block + size);
                *(word_t *)block = PREV_BLOCK;
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
                next = prev + (*prev & M_MASK);
            } while (next < (word_t *)block);
            overlap = 0;

            if ((word_t *)block == prev + overhead)
            {
                /* Our block directly follows the one we found */
                block -= overhead;
                overlap += overhead * SINT;
                count_back(large_wasted_stat, overhead * SINT);
            }
            else
            {
                /* We have to create a new bridge block */
                prev[M_SIZE] = (prev[M_SIZE] & (PREV_BLOCK|THIS_BLOCK|M_GC_FREE))
                               | ((word_t*)block - prev);
#ifdef MALLOC_TRACE
                prev[M_OVERHEAD+XM_FILE] = (word_t)"block";
                prev[M_OVERHEAD+XM_LINE] = 0;
#endif
#ifdef MALLOC_LPC_TRACE
                prev[M_OVERHEAD+XM_OBJ] = 0;
                prev[M_OVERHEAD+XM_PROG] = 0;
                prev[M_OVERHEAD+XM_PC] = 0;
#endif
                *(word_t *)block = PREV_BLOCK;
            }

            if (next - p == overhead)
            {
                /* Our block directly preceedes the next one */
                *next &= ~PREV_BLOCK;
                overlap += overhead * SINT;
                count_back(large_wasted_stat, overhead * SINT);
            }
            else
            {
                /* We have to create a new bridge block */
                p[M_SIZE] = THIS_BLOCK | (next - p); /* no M_GC_FREE */
            }
        }
    }

    count_up(sbrk_stat, size);
    count_up(large_wasted_stat, overhead * SINT);

    *pExtra = overlap;
    return block;
#endif /* !SBRK_OK */
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
    const word_t wsize = (size + SINT - 1)/ SINT;

    malloc_increment_size_calls++;

    start = (word_t*)p - M_OVERHEAD;
    old_size = start[M_SIZE] & M_MASK;

    if (start[M_SIZE] & M_SMALL)
    {
        /* Can't extend small blocks. */
        return NULL;
    }

    /* Extend a large block */

    start2 = &start[old_size];
    next = *start2;

    if (next & THIS_BLOCK)
        return NULL; /* no following free block */

    next &= M_MASK;

    if (next == wsize)
    {
        /* Next block fits perfectly */
        remove_from_free_list(start2);
        start2[next] |= PREV_BLOCK;
        start[M_SIZE] += wsize;
        malloc_increment_size_success++;
        malloc_increment_size_total += (start2 - start) - M_OVERHEAD;
        count_add(large_alloc_stat, wsize);

        return start2;
    }

    if (next > wsize + SMALL_BLOCK_MAX)
    {
        /* Split the next block, it is large enough */

        remove_from_free_list(start2);
        start2[next-1] -= wsize;
        start3 = start2 + wsize;
        start3[M_SIZE] = (next-wsize) | PREV_BLOCK;
        add_to_free_list(start3);
        start[M_SIZE] += wsize;
        malloc_increment_size_success++;
        malloc_increment_size_total += (start2 - start) - M_OVERHEAD;
        count_add(large_alloc_stat, wsize);
        return start2;
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

    if (block[M_SIZE] & M_SMALL)
    {
        i = ((slab_t *)(block - (block[M_SIZE] & M_MASK)))->size / SINT;
    }
    else
    {
        i = block[M_SIZE] & M_MASK;
    }

    if (i < M_OVERHEAD + ((minsize + SINT - 1) / SINT)
     || block + i >= heap_end)
        return MY_TRUE;

    if (i >= SMALL_BLOCK_MAX)
    {
        /* Large block */

        word_t* block2;

        block2 = block + i;
        return !(block[M_SIZE] & THIS_BLOCK)
#ifdef MALLOC_CHECK
             || block[M_MAGIC] != LAMAGIC
#endif /* MALLOC_CHECK */
             || !(*block2 & PREV_BLOCK);
    }

    /* Small block */
    return (block[M_SIZE] & THIS_BLOCK) != 0;
} /* mem_is_freed() */

/*-------------------------------------------------------------------------*/
static void
mem_clear_slab_memory_flags (const char * tag,  slab_t * slab, word_t * startp)

/* Set the flags for the memory of <slab>: the slab and the contained
 * free list block are marked as referenced, the others as unreferenced.
 * If <startp> is not NULL, it denotes the starting memory address
 * for the scan, otherwise <slab>->blocks is the starting address (this is
 * used for marking the fresh slab).
 */

{
    word_t * p;

    if (NULL != startp)
        p = startp;
    else
        p = slab->blocks;

    ulog2f("slaballoc: clear in %s slab %x,", tag, slab);

    ulog3f(" mem %x/%x..%x\n"
          , slab->blocks, startp, ((char *)slab->blocks) + (slab->numBlocks * slab->size)
          );

    mem_mark_ref(slab);

    while (p < slab->blocks + slab->numBlocks * slab->size / SINT)
    {
        /* ulog1f("slaballoc:   clear block %x\n", p); */
        *p &= ~M_REF;
        p += slab->size / SINT;
    }

    for (p = slab->freeList; p != NULL; p = BLOCK_NEXT(p))
    {
        /* ulog1f("slaballoc:   mark free block %x\n", p); */
        *p |= M_REF;
    }
} /* mem_clear_slab_memory_flags() */

/*-------------------------------------------------------------------------*/
void
mem_clear_ref_flags (void)

/* Walk through all allocated blocks and clear the M_REF flag in preparation
 * for a GC.
 */

{
    word_t *p, *last;
    int i;

    /* Clear the large blocks */
    last = heap_end - M_OVERHEAD - XM_OVERHEAD;
    for (p = heap_start; p < last; )
    {
        *p &= ~M_REF;
        if (p + (*p & M_MASK) > heap_end)
        {
            in_malloc = 0;
            fatal("pointer larger than brk: %p + %lx = %p > %p\n"
                  , p, *p & M_MASK, p + (*p + M_MASK), last);
        }
        p += *p & M_MASK;
    }

    /* Now mark the memory used for the small slabs as ref'd,
     * clear all contained small blocks, then mark all blocks in the free
     * lists as ref'd.
     */
    for  (i = 0; i < SMALL_BLOCK_NUM; ++i)
    {
        slab_t * slab;

        ulog1f("slaballoc: clear_ref [%d]\n", i);

        /* Mark the fresh slab.
         */
        if (slabtable[i].fresh)
        {
            mem_clear_slab_memory_flags( "fresh",  slabtable[i].fresh
                                       , slabtable[i].freshMem);
        }

        /* Mark the partially used slabs.
         */
        for  (slab = slabtable[i].first; slab != NULL; slab = slab->next)
        {
            mem_clear_slab_memory_flags("partial", slab, NULL);
        }

        /* Mark the fully used slabs.
         */
        for  (slab = slabtable[i].fullSlabs; slab != NULL; slab = slab->next)
        {
            mem_clear_slab_memory_flags("full", slab, NULL);
        }

        /* Mark the fully free slabs.
         */
        for  (slab = slabtable[i].firstFree; slab != NULL; slab = slab->next)
        {
            ulog1f("slaballoc: clear in free slab %x\n" , slab);

            mem_mark_ref(slab);
            /* No internal freelist to scan */
        }
    }

    /* We set M_REF for small free blocks that early for two reasons:
     * - if it's referenced anywhere else, we get a fatal.
     * - if the block gets malloced by the swapper in pass 5, it needs
     *   M_REF to be already set.
     */
} /* mem_clear_ref_flags() */

/*-------------------------------------------------------------------------*/
static mp_int
mem_free_unrefed_slab_memory (const char * tag, slab_t * slab, word_t * startp)

/* Scan the memory of <slab> for unreferenced blocks and free them.
 * If <startp> is not NULL, it denotes the starting memory address
 * for the scan, otherwise <slab>->blocks is the starting address (this is
 * used for scanning the fresh slab).
 * Return the number of successfully recovered blocks.
 *
 * Note that this may move the slab into a different slab list.
 */

{
    mp_int   success = 0;
    word_t * p;

    if (NULL != startp)
        p = startp;
    else
        p = slab->blocks;

    ulog2f("slaballoc: free unref in %s slab %x,", tag, slab);

    ulog3f(" mem %x/%x..%x\n"
          , slab->blocks, startp, ((char *)slab->blocks) + (slab->numBlocks * slab->size)
          );

    while (p < slab->blocks + slab->numBlocks * slab->size / SINT)
    {
        if ((*p & (M_REF|M_GC_FREE)) == M_GC_FREE)
        {
            /* Unref'd small blocks are definitely lost */
            success++;
            count_back(xalloc_stat, slab->size - (T_OVERHEAD * SINT));
            dprintf2(gcollect_outfd, "freeing small block 0x%x (user 0x%x)"
                    , (p_uint)p, (p_uint)(p+M_OVERHEAD));
#ifdef MALLOC_TRACE
            dprintf2(gcollect_outfd, " %s %d"
                    , p[XM_FILE+M_OVERHEAD], p[XM_LINE+M_OVERHEAD]);
#endif
            writes(gcollect_outfd, "\n");
#ifdef MALLOC_LPC_TRACE
            write_lpc_trace(gcollect_outfd, p + M_OVERHEAD, MY_FALSE);
#endif
            print_block(gcollect_outfd, p + M_OVERHEAD);

            /* Recover the block */
            *p |= M_REF;
            sfree(p+M_OVERHEAD);
        }
#if 0 && defined(DEBUG_MALLOC_ALLOCS)
        else
            ulog1f("slaballoc:   block %x is ref'd\n", p);
#endif /* DEBUG_MALLOC_ALLOCS */

        p += slab->size / SINT;
    }

    return success;
} /* mem_free_unrefed_slab_memory() */

/*-------------------------------------------------------------------------*/
void
mem_free_unrefed_memory (void)

/* The GC marked all used memory as REF'd, now recover all blocks which
 * are allocated, but haven't been marked.
 */

{
    word_t *p, *last;
    int i;
    mp_int success = 0;

    /* Scan the heap for lost large blocks */
    last = heap_end - M_OVERHEAD - XM_OVERHEAD;
    for (p = heap_start; p < last; )
    {
        word_t size;

        size = *p;
        if ( (size & (M_REF|THIS_BLOCK|M_GC_FREE)) == (THIS_BLOCK|M_GC_FREE) )
        {
            /* Large block marked as in use (THIS_BLOCK), but is not
             * referenced (no M_REF) - recover it.
             */
            word_t size2;

            success++;
            count_back(xalloc_stat, mem_block_size(p+M_OVERHEAD));
#if defined(MALLOC_TRACE) || defined(MALLOC_LPC_TRACE)
            dprintf1(gcollect_outfd, "freeing large block 0x%x", (p_uint)p);
#endif
#ifdef MALLOC_TRACE
            dprintf3(gcollect_outfd, " %s %d size 0x%x\n",
              p[XM_FILE+M_OVERHEAD], p[XM_LINE+M_OVERHEAD], size & M_MASK
            );
#endif
#ifdef MALLOC_LPC_TRACE
            write_lpc_trace(gcollect_outfd, p + M_OVERHEAD, MY_FALSE);
#endif
            print_block(gcollect_outfd, p + M_OVERHEAD);
            size2 = p[size & M_MASK];
            large_free((char *)(p+M_OVERHEAD));
            if ( !(size2 & THIS_BLOCK) )
                size += size2;
        }
        p += size & M_MASK;
    }
    if (success)
    {
        dprintf1(gcollect_outfd, "%d large blocks freed\n", success);
    }

    /* Scan the small chunks for lost small blocks.
     * Remember that small blocks in the free-lists are marked as ref'd.
     */
    success = 0;
    for  (i = 0; i < SMALL_BLOCK_NUM; ++i)
    {
        slab_t * slab, *next;

        ulog1f("slaballoc: free_unrefed [%d]\n" , i);

        /* Search the fresh slab for unreferenced blocks.
         */
        if (slabtable[i].fresh)
        {
            success += mem_free_unrefed_slab_memory("fresh", slabtable[i].fresh
                                                   , slabtable[i].freshMem);
        }

        /* Search the partially used slabs for unreferenced blocks.
         * Note that this may move individual slabs into a different list.
         */
        for  (slab = slabtable[i].first; slab != NULL; slab = next)
        {
            next = slab->next;
            success += mem_free_unrefed_slab_memory("partial", slab, NULL);
        }

        /* Search the fully used slabs for unreferenced blocks.
         * Note that this may move individual slabs into a different list.
         */
        for  (slab = slabtable[i].fullSlabs; slab != NULL; slab = next)
        {
            next = slab->next;
            success += mem_free_unrefed_slab_memory("full", slab, NULL);
        }
    }
    if (success)
    {
        dprintf1(gcollect_outfd, "%d small blocks freed\n", success);
    }
} /* mem_free_unrefed_memory() */

/*-------------------------------------------------------------------------*/
#if 0
static void
mem_dump_slab_memory (int fd, const char * tag, int tableIx
                     , slab_t * slab, word_t * startp)

/* Dump the memory of <slab>.
 * If <startp> is not NULL, it denotes the starting memory address
 * for the dump, otherwise <slab>->blocks is the starting address (this is
 * used for dumping the fresh slab).
 */

{
    word_t * p;

    dprintf4(fd, "\n--- %s Slab: %x .. %x size %x"
               , (p_uint)tag
               , (p_uint)slab, (p_uint)(slab + SLAB_SIZE(slab)) - 1
               , (p_uint)SLAB_SIZE(slab)
               );
    dprintf2(fd, " (%d/%d byte blocks)\n"
               , (p_int)(tableIx + SMALL_BLOCK_MIN) * SINT
               , (p_int)(tableIx + T_OVERHEAD+SMALL_BLOCK_MIN) * SINT
            );

    /* No trace information for slabs */

    if (NULL != startp)
        p = startp;
    else
        p = slab->blocks;

    while (p <= slab->blocks + slab->numBlocks * slab->size / SINT)
    {
        word_t size = *p;
        if (!(*p & THIS_BLOCK))
        {
            dprintf4(fd, "%x .. %x %s offset %x "
                       , (p_uint)p, (p_uint)(p + (size&M_MASK)) - 1
                       , (p_uint)((size & M_GC_FREE) ? " " : "P")
                       , (p_uint)(size & M_MASK) * SINT
                       );

#ifdef MALLOC_TRACE
            if (p[XM_FILE+M_OVERHEAD])
                dprintf2(fd, ": %s %d "
                           , p[XM_FILE+M_OVERHEAD], q[XM_LINE+M_OVERHEAD]
                );
            else
                writes(fd, ": - - ");
#endif
#ifdef MALLOC_LPC_TRACE
            if (p[M_OVERHEAD + XM_OBJ])
            {
                writes(fd, ": ");
                write_lpc_trace(fd, p + M_OVERHEAD, MY_TRUE);
            }
            else
#endif
            writes(fd, "\n");
        }

        p += slab->size / SINT;
    }
} /* mem_dump_slab_memory() */
#endif

/*-------------------------------------------------------------------------*/
static Bool
mem_identify_slab (int fd, const char * tag, int tableIx
                  , slab_t * list, slab_t * slab)

/* Check if <slab> is member of the list starting at <listStart>.
 * If yes, print it's information preceeded by <tag> and return TRUE.
 * Return FALSE otherwise.
 */

{
    Bool isSlab = MY_FALSE;

    for ( NOOP
        ; !isSlab && list != NULL
        ; list = list->next
        )
    {
        if (slab == list)
        {
            dprintf4(fd, ": %s slab %x (%d/%d bytes)\n"
                       , (p_int)tag
                       , (p_int) slab
                       , (p_int)(tableIx + SMALL_BLOCK_MIN) * SINT
                       , (p_int)(tableIx + T_OVERHEAD+SMALL_BLOCK_MIN) * SINT
                       );
            isSlab = MY_TRUE;
            break;
        }
    }

    return isSlab;
} /* mem_identify_slab() */

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
    int ix;

    if (fd < 0)
        return MY_TRUE;

    writes(fd, "\n--- Large Blocks\n");

    /* Dump the heap blocks */
    last = heap_end - M_OVERHEAD - XM_OVERHEAD;
    for (p = heap_start; p < last; )
    {
        word_t size;

        size = *p;
        if ( size & THIS_BLOCK )
        {
            Bool isSlab = MY_FALSE;

            dprintf4(fd, "%x .. %x %s size %x "
                       , (p_uint)p, (p_uint)(p + (size&M_MASK)) - 1
                       , (p_uint)((size & M_GC_FREE) ? " " : "P")
                       , (p_uint)(size & M_MASK) * SINT
                       );

            q = p + M_OVERHEAD;

            for (ix = 0; !isSlab && ix < SMALL_BLOCK_NUM; ++ix)
            {
                if ((word_t *)slabtable[ix].fresh == q)
                {
                    dprintf2(fd, ": fresh slab (%d/%d bytes)\n"
                               , (p_int)(ix + SMALL_BLOCK_MIN) * SINT
                               , (p_int)(ix + T_OVERHEAD+SMALL_BLOCK_MIN) * SINT
                               );
                    isSlab = MY_TRUE;
                }

                if (!isSlab)
                    isSlab = mem_identify_slab(fd, "partial", ix
                                              , slabtable[ix].first
                                              , (slab_t*)q);
                if (!isSlab)
                    isSlab = mem_identify_slab(fd, "full", ix
                                              , slabtable[ix].fullSlabs
                                              , (slab_t*)q);
                if (!isSlab)
                    isSlab = mem_identify_slab(fd, "free", ix
                                              , slabtable[ix].firstFree
                                              , (slab_t*)q);
            }
            if (!isSlab)
            {
#ifdef MALLOC_TRACE
                if (p[XM_FILE+M_OVERHEAD])
                    dprintf2(fd, ": %s %d "
                               , p[XM_FILE+M_OVERHEAD], p[XM_LINE+M_OVERHEAD]
                    );
                else
                    writes(fd, ": - -");
#endif
#ifdef MALLOC_LPC_TRACE
                if (p[M_OVERHEAD + XM_OBJ])
                {
                    writes(fd, ": ");
                    write_lpc_trace(fd, p + M_OVERHEAD, MY_TRUE);
                }
                else
#endif
                    writes(fd, "\n");
            }
        }
        p += size & M_MASK;
    }

#if 0
    /* Dump the slabs and their small blocks.
     */
    for (ix = 0; ix < SMALL_BLOCK_NUM; ++ix)
    {
        slab_t * slab;

        if (NULL != (slab = slabtable[ix].fresh))
        {
            mem_dump_slab_memory(fd, "Fresh", ix, slab, slabtable[ix].freshMem);
        }

        for (slab = slabtable[ix].first; slab != NULL; slab = slab->next)
            mem_dump_slab_memory(fd, "Partial", ix, slab, NULL);

        for (slab = slabtable[ix].fullSlabs; slab != NULL; slab = slab->next)
            mem_dump_slab_memory(fd, "Full", ix, slab, NULL);

        for (slab = slabtable[ix].firstFree; slab != NULL; slab = slab->next)
            mem_dump_slab_memory(fd, "Free", ix, slab, NULL);
    }
#endif

    return MY_TRUE;
} /* mem_dump_memory() */

/*-------------------------------------------------------------------------*/
void
mem_consolidate (Bool force)

/* Consolidate the memory.
 *
 * If <force> is TRUE, all fully free slabs are deallocated.
 * If <force> is FALSE, only the free slabs older than SLAB_RETENTION_TIME
 * are deallocated.
 */

{
    int ix;

    ulog1f("slaballoc: consolidate (%d)\n", force);

    for (ix = 0; ix < SMALL_BLOCK_NUM; ++ix)
    {
        ulog1f("slaballoc:   consolidate [%d]\n", ix);

        if (force)
        {
            slab_t * slab;

            while (NULL != (slab = slabtable[ix].firstFree))
            {
                slabtable[ix].firstFree = slab->next;
                count_back(small_slab_free_stat, SLAB_SIZE(slab));
                free_slab(slab, ix);
            }
            slabtable[ix].numFreeSlabs = 0;
            slabtable[ix].lastFree = NULL;
        }
#if SLAB_RETENTION_TIME > 0
        else
        {
            slab_t * slab;

            if ( NULL != (slab = slabtable[ix].lastFree)
              && current_time - (mp_int)slab->blocks[0] > SLAB_RETENTION_TIME
               )
            {
                ulog3f("slaballoc:   free slab %x (prev %x) delta-t %d\n"
                      , slab, slab->prev, current_time - (mp_int)slab->blocks[0]);

                slabtable[ix].lastFree = slab->prev;
                if (slab->prev)
                    slab->prev->next = NULL;
                else
                    slabtable[ix].firstFree = NULL;
                slabtable[ix].numFreeSlabs--;
                count_back(small_slab_free_stat, SLAB_SIZE(slab));
                free_slab(slab, ix);
            }
#ifdef DEBUG_MALLOC_ALLOCS
            else if (NULL != slab)
            {
                ulog3f("slaballoc:   keep slab %x (prev %x) delta-t %d\n"
                      , slab, slab->prev, current_time - (mp_int)slab->blocks[0]);
            }
#endif /* DEBUG_MALLOC_ALLOCS */
        }
#endif /* SLAB_RETENTION_TIME > 0 */
    }
#ifdef MALLOC_EXT_STATISTICS
    extstat_update_max(extstats + EXTSTAT_SLABS);
#endif /* MALLOC_EXT_STATISTICS */
} /* mem_consolidate() */

/***************************************************************************/
