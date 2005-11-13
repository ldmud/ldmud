/* Satoria's malloc intended to be optimized for lpmud.
** this memory manager distinguishes between two sizes
** of blocks: small and large.  It manages them separately
** in the hopes of avoiding fragmentation between them.
** It expects small blocks to mostly be temporaries.
** It expects an equal number of future requests as small
** block deallocations.
**
** Written and put into the public domain by Sean T. Barrett.
*/
/* support for atari st/tt and FAST_FIT by amylaar @cs.tu-berlin.de */
#define FIT_STYLE_FAST_FIT
#define SMALLOC
#include "driver.h"

#include "smalloc.h"

#include "comm.h"
#include "backend.h"
#include "gcollect.h"
#include "main.h"
#include "simulate.h"

#ifdef SMALLOC_LPC_TRACE
#include "exec.h"
#include "object.h"
#endif

typedef p_uint u;

void dprintf1 (int, char *, p_int);
void dprintf2 (int, char *, p_int, p_int);
void dprintf3 (int, char *, p_int, p_int, p_int);


static char *esbrk PROT((u));

#define lARGE_TRACE /* case is used to switch on/off */
#define fake(s) (void)0
#ifndef fake
#define fake(s) dprintf1(2, "%s\n",(p_int)(s)),debug_message(s)
#endif

#ifdef SMALLOC_LPC_TRACE
#define M_OBJ 1
#define M_PROG 2
#define M_PC 3
#ifdef SMALLOC_TRACE
#define OVERHEAD (7)
#define M_FILE 4
#define M_LINE 5
#define M_MAGIC 6
#else
#define OVERHEAD (4)
#endif /* SMALLOC_TRACE */
#else /* !SMALLOC_LPC_TRACE */
#ifdef SMALLOC_TRACE
#define OVERHEAD (4)
#define M_FILE 1
#define M_LINE 2
#define M_MAGIC 3
#else
#define OVERHEAD (1)
#endif /* SMALLOC_TRACE */
#endif /* SMALLOC_LPC_TRACE */

#ifndef SMALLOC_TRACE
#define smalloc xalloc
#endif

#define sfree   xfree
#define srealloc realloc

#define SINT (SIZEOF_P_INT)

#define SMALL_BLOCK_MAX (8)

#define SMALL_BLOCK_MAX_BYTES        (SMALL_BLOCK_MAX*SINT)
#ifdef SBRK_OK
#define SMALL_CHUNK_SIZE        0x4000
#define CHUNK_SIZE                0x40000
#else
/* It seems to be advantagous to be just below a power of two
 * make sure that the resulting chunk sizes are SINT aligned. */
#define SMALL_CHUNK_SIZE        \
        (0x8000 - (OVERHEAD+1)*SINT+SINT - EXTERN_MALLOC_OVERHEAD)
/*   large_malloc overhead ^ */

#define CHUNK_SIZE                (0x40000 - SINT - EXTERN_MALLOC_OVERHEAD)
#endif

#define PREV_BLOCK        0x80000000
#define THIS_BLOCK        0x40000000
#define M_REF                0x20000000 /* check this in smalloc.h */
#define M_GC_FREE        0x10000000 /* check this in smalloc.h */

#define MAGIC                0x17952932

/* SMALL BLOCK info */

static u *last_small_chunk = 0;
static u *sfltable[SMALL_BLOCK_MAX]={0,0,0,0,0,0,0,0};        /* freed list */
static u *sys_sfltable[SMALL_BLOCK_MAX]={0,0,0,0,0,0,0,0};
static u *next_unused=0;
static u unused_size=0;                        /* until we need a new chunk */

#ifdef SMALLOC_TRACE
static u sfmagic[SMALL_BLOCK_MAX] =  {
        0xde8f7d2d,
        0xbd89a4b6,
        0xb667bbec,
        0x475fae0a,
        0x39fc5582,
        0x32041f46,
        0x624a92f0,
        0x16dd36e2,
};

static u samagic[SMALL_BLOCK_MAX] = {
        0x34706efd,
        0xfc2a5830,
        0x4e62041a,
        0x2713945e,
        0xab58d8ab,
        0xa372eeab,
        0x71093c08,
        0xed2e6cc9,
};

#define LFMAGIC 0xdfaff2ee
#define LAMAGIC 0xf460146e
#endif /* SMALLOC_TRACE */

/* LARGE BLOCK info */

#ifndef FIT_STYLE_FAST_FIT
static u *free_list=0;
#endif /* FIT_STYLE_FAST_FIT */

static u *heap_start=0;
static u *heap_end=0;

#ifndef SBRK_OK
static int overlap;
#endif

#define gout gcollect_outfd

/* STATISTICS */

static long small_count[SMALL_BLOCK_MAX]={0,0,0,0,0,0,0,0};
static long small_total[SMALL_BLOCK_MAX]={0,0,0,0,0,0,0,0};
static long small_max[SMALL_BLOCK_MAX]  ={0,0,0,0,0,0,0,0};
static long small_free[SMALL_BLOCK_MAX] ={0,0,0,0,0,0,0,0};
static long sys_small_missing[SMALL_BLOCK_MAX] ={0,0,0,0,0,0,0,0};

typedef struct { unsigned counter, size; } t_stat;
#define count(a,b) { a.size+=(b); if ((b)<0) --a.counter; else ++a.counter; }
#define count_up(a,b)   { a.size+=(b); ++a.counter; }
#define count_add(a,b)   { a.size+=(b); }
#define count_back(a,b) { a.size-=(b); --a.counter; }

static t_stat sbrk_stat;

int debugmalloc=0;        /* Only used when debuging malloc() */

/********************************************************/
/*  SMALL BLOCK HANDLER                                        */
/********************************************************/

#ifdef SMALLOC_TRACE
static char *_large_malloc PROT((u, int, char *, int));
#define large_malloc(size, force_m) _large_malloc(size, force_m, file, line)
#else
static char *large_malloc();
#endif
static void large_free PROT((char *));

#define s_size_ptr(p)        (p)
#define s_next_ptr(p)        ((u **) (p+OVERHEAD))

static t_stat small_alloc_stat={0,0};
static t_stat small_free_stat={0,0};
static t_stat small_chunk_stat={0,0};

#ifdef SMALLOC_TRACE
POINTER smalloc(size, file, line)
  size_t size;
  char *file; int line;
#else
POINTER smalloc(size)
  size_t size;
#endif
{
  /*int i;*/
  u *temp;

#ifdef DEBUG
  if (size <= 0)
      fatal("Malloc size <= 0.\n");
#endif
  if (size>SMALL_BLOCK_MAX_BYTES)
    return large_malloc(size,0);

  size = (size+OVERHEAD*SINT+SINT-1) & ~(SINT-1); /* block size in bytes */
#define SIZE_INDEX(u_array, size)         \
        (*(u*) ((char*)u_array-OVERHEAD*SINT-SINT+size))
#define SIZE_PNT_INDEX(u_array, size)        \
        (*(u**)((char*)u_array-OVERHEAD*SINT-SINT+size))
  /*i = (size - OVERHEAD*SINT-SINT) / SINT;*/
  count_up(small_alloc_stat,size);

  SIZE_INDEX(small_count, size) += 1;                        /* update statistics */
  SIZE_INDEX(small_total, size) += 1;
  if (SIZE_INDEX(small_count, size) > SIZE_INDEX(small_max, size))
    SIZE_INDEX(small_max, size) = SIZE_INDEX(small_count, size);

  if ( NULL != (temp = SIZE_PNT_INDEX(sfltable, size)) )
    {                                        /* allocate from the free list */
      count_back(small_free_stat, size);
#ifdef SMALLOC_LPC_TRACE
      temp[M_OBJ]  = (u)current_object;
      temp[M_PROG] = current_prog ? current_prog->id_number : 0;
      temp[M_PC]   = (u)inter_pc;
#endif
#ifdef SMALLOC_TRACE
      temp[M_FILE] = (u)file;
      temp[M_LINE] = line;
      if (temp[M_MAGIC] != SIZE_INDEX(sfmagic, size) )
        fatal("allocation from free list:  magic match failed\n");
      temp[M_MAGIC] = SIZE_INDEX(samagic, size);
#endif
      temp += OVERHEAD;
      SIZE_PNT_INDEX(sfltable, size) = * (u **) temp;
fake("From free list.");
      return (char *) temp;
    }                                        /* else allocate from the chunk */

  if (unused_size<size)                        /* no room in chunk, get another */
    {
      fake("Allocating new small chunk.");
      if (unused_size) {
        if (unused_size < SINT + OVERHEAD*SINT) {
          *s_size_ptr(next_unused) = 0;
        } else {
          *s_size_ptr(next_unused) = unused_size / SINT | (M_GC_FREE|M_REF);
          *s_next_ptr(next_unused) = SIZE_PNT_INDEX(sfltable, unused_size);
#ifdef SMALLOC_TRACE
          next_unused[M_MAGIC] = SIZE_INDEX(sfmagic, unused_size);
#endif
          SIZE_PNT_INDEX(sfltable, unused_size) = next_unused;
          count_up(small_free_stat, unused_size);
        }
      }
      next_unused = (u *) large_malloc(SMALL_CHUNK_SIZE + sizeof(u*), 1);
      if (next_unused == 0) {
        unused_size = 0;
        if (malloc_privilege < MALLOC_SYSTEM)
            return 0;
        SIZE_INDEX(sys_small_missing, size) ++;
        if ( NULL != (temp = SIZE_PNT_INDEX(sys_sfltable, size)) ) {
#ifdef SMALLOC_LPC_TRACE
            temp[M_OBJ]  = (u)current_object;
            temp[M_PROG] = current_prog ? current_prog->id_number : 0;
            temp[M_PC]   = (u)inter_pc;
#endif
#ifdef SMALLOC_TRACE
            temp[M_FILE] = (u)file;
            temp[M_LINE] = line;
            if (temp[M_MAGIC] != SIZE_INDEX(sfmagic, size) )
                fatal("allocation from free list:  magic match failed\n");
            temp[M_MAGIC] = SIZE_INDEX(samagic, size);
#endif
            temp += OVERHEAD;
            SIZE_PNT_INDEX(sys_sfltable, size) = * (u **) temp;
            return (char *)temp;
        }
        next_unused = (u *) large_malloc(SMALL_CHUNK_SIZE + sizeof(u*), 0);
      }
      *next_unused = (u)last_small_chunk;
      last_small_chunk = next_unused++;
      count_up(small_chunk_stat, SMALL_CHUNK_SIZE+SINT*OVERHEAD+sizeof(u*));
      unused_size = SMALL_CHUNK_SIZE;
    }
else fake("Allocated from chunk.");


  temp = (u *) s_next_ptr(next_unused);

  *s_size_ptr(next_unused) = size / SINT | (M_GC_FREE|M_REF);
#ifdef SMALLOC_LPC_TRACE
  next_unused[M_OBJ]  = (u)current_object;
  next_unused[M_PROG] = current_prog ? current_prog->id_number : 0;
  next_unused[M_PC]   = (u)inter_pc;
#endif
#ifdef SMALLOC_TRACE
  next_unused[M_FILE] = (u)file;
  next_unused[M_LINE] = line;
  next_unused[M_MAGIC] = SIZE_INDEX(samagic, size);
#endif
  next_unused += size / SINT;
  unused_size -= size;

fake("allocation from chunk successful\n");
  return (char *) temp;
}

#ifdef DEBUG
static char *debug_free_ptr;
#endif /* DEBUG */

void sfree(ptr)
POINTER ptr;
{
    u *block;
    u i;

#ifdef DEBUG
    debug_free_ptr = ptr;
#endif /* DEBUG */
    block = (u *) ptr;
    block -= OVERHEAD;
    i = (*s_size_ptr(block) & MASK);
    if (i > SMALL_BLOCK_MAX + OVERHEAD) {
        fake("sfree calls large_free");
        large_free(ptr);
        return;
    }

  count_back(small_alloc_stat, i * SINT);
  count_up(small_free_stat, i * SINT);
  i -=  1 + OVERHEAD;
#ifdef SMALLOC_TRACE
  if (block[M_MAGIC] != samagic[i])
    fatal("sfree: magic match failed\n");
  block[M_MAGIC] = sfmagic[i];
#endif
  *s_next_ptr(block) = sfltable[i];
  sfltable[i] = block;
  small_free[i] += 1;
fake("Freed");
  return;
}

/************************************************/
/*        LARGE BLOCK HANDLER                        */
/************************************************/

#define BEST_FIT        0
#define FIRST_FIT        1
#define HYBRID                2

#define fit_style BEST_FIT
/* if this is a constant, evaluate at compile-time.... */
#ifndef fit_style
static int fit_style =BEST_FIT;
#endif

#define l_size_ptr(p)                (p)
#define l_next_ptr(p)                (*((u **) ((p)+OVERHEAD)))
#define l_prev_ptr(p)                (*((u **) (p+2)))
#define l_next_block(p)                ((p) + (MASK & (*(p))) )
#define l_prev_block(p)         ((p) - (MASK & (*((p)-1))) )
#define l_prev_free(p)                (!(*p & PREV_BLOCK))
#define l_next_free(p)                (!(*l_next_block(p) & THIS_BLOCK))

#if 0
void show_block(ptr)
u *ptr;
{
  dprintf3(2, "[%c%d: %d]  ",(*ptr & THIS_BLOCK ? '+' : '-'),
                (int) ptr, *ptr & MASK);
}
#endif

#ifdef FIT_STYLE_FAST_FIT

#if defined(atarist) || defined (sun) || defined(AMIGA) || defined(__linux__) || defined(__BEOS__)
/* there is a type signed char */
    typedef /*signed*/ char balance_t;
#   define BALANCE_T_BITS 8
#else
    typedef short balance_t;
#   define BALANCE_T_BITS 16
#endif
#if (defined(atarist) && !defined(ATARI_TT)) || defined(sparc) || defined(AMIGA)
    /* try to avoid multiple shifts, because these are costly */
#   define NO_BARREL_SHIFT
#endif

struct free_block {
    u size;
    struct free_block *parent, *left, *right;
    balance_t balance;
    short align_dummy;
};

/* prepare two nodes for the free tree that will never be removed,
   so that we can always assume that the tree is and remains non-empty. */
/* some compilers don't understand forward declarations of static vars. */
extern struct free_block dummy2;  /* forward */
static struct free_block dummy =
        { /*size*/0, /*parent*/&dummy2, /*left*/0, /*right*/0, /*balance*/0 };
       struct free_block dummy2 =
        { /*size*/0, /*parent*/0, /*left*/&dummy, /*right*/0, /*balance*/-1 };

static struct free_block *free_tree = &dummy2;

#define WRITES(d, s) write((d), (s), strlen(s))

#ifdef DEBUG_AVL
static int inconsistency = 0;

static void _check_next(p)
struct free_block *p;
{
    if (!p) return;
    {
        u *q;

        q = ((u*)p)-OVERHEAD;
        q += *q & MASK;
    }
    _check_next(p->left);
    _check_next(p->right);
}

static void check_next() {
    _check_next(free_tree);
}

static int check_avl(parent, p)
struct free_block *parent, *p;
{
    int left, right;

    if (!p) return 0;
    left  = check_avl(p, p->left );
    right = check_avl(p, p->right);
    if (p->balance != right - left || p->balance < -1 || p->balance > 1) {
        WRITES  (2, "Inconsistency in avl node!\n");
        dprintf1(2, "node:%x\n",(p_uint)p);
        dprintf1(2, "size: %d\n", p->size);
        dprintf1(2, "left node:%x\n",(p_uint)p->left);
        dprintf1(2, "left  height: %d\n",left );
        dprintf1(2, "right node:%x\n",(p_uint)p->right);
        dprintf1(2, "right height: %d\n",right);
        dprintf1(2, "alleged balance: %d\n",p->balance);
        inconsistency = 1;
    }
    if (p->parent != parent) {
        WRITES  (2, "Inconsistency in avl node!\n");
        dprintf1(2, "node:%x\n",(p_uint)p);
        dprintf1(2, "size: %d\n", p->size);
        dprintf1(2, "parent: %x\n", (p_uint)parent);
        dprintf1(2, "parent size: %d\n", parent->size);
        dprintf1(2, "alleged parent: %x\n", (p_uint)p->parent);
        dprintf1(2, "alleged parent size: %d\n", p->parent->size);
        dprintf1(2, "left  height: %d\n",left );
        dprintf1(2, "right height: %d\n",right);
        dprintf1(2, "alleged balance: %d\n",p->balance);
        inconsistency = 1;
    }
    return left > right ? left+1 : right+1;
}

/* this function returns a value so that it can be used in ,-expressions. */
static int do_check_avl() {
    check_avl(0, free_tree);
    if (inconsistency) {
        fflush(stderr);
        fflush(stdout);
        fatal("Inconsistency could crash the driver\n");
    }
    return 0;
}

static int contains(p, q)
    struct free_block *p, *q;
{
    return p == q || (p && (contains(p->left, q) || contains(p->right, q)));
}

static int check_free_block(m)
    char *m;
{
    u *p;
    int size;

    p = (u *)(m-OVERHEAD*SINT);
    size = *p & MASK;
    if (!(*(p+size) & THIS_BLOCK)) {
        if (!contains(free_tree, (struct free_block *)(p+size+OVERHEAD)) )
            fatal("not found\n");
    }
    return 0;
}
#else /* !DEBUG_AVL */
#define do_check_avl() 0
#endif /* DEBUG_AVL */

static t_stat large_free_stat;
static void remove_from_free_list(ptr)
u *ptr;
{
    struct free_block *p, *q, *r, *s, *t;

#ifdef SMALLOC_TRACE
    if (ptr[M_MAGIC] != LFMAGIC)
        fatal("remove_from_free_list: magic match failed\n");
#endif
    fake((do_check_avl(),"remove_from_free_list called"));
    p = (struct free_block *)(ptr+OVERHEAD);
    count_back(large_free_stat, p->size);
#ifdef DEBUG_AVL
    dprintf1(2, "node:%x\n",(p_uint)p);
    dprintf1(2, "size:%d\n",p->size);
#endif
    if (p->left) {
        if ( NULL != (q = p->right) ) {
            fake("two childs");
            s = q;
            for ( ; NULL != (r = q, q = r->left); ) NOOP;
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
            fake("no right child, but left child");
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
        fake("no left child");
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
                fake("R-Rotation.");
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
                        fake("left from parent");
                        goto balance_left_s;
                    } else {
                        fake("right from parent");
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

                fake("LR-Rotation.");
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
                fake("L-Rotation.");
                dprintf1(2, "r->balance: %d\n", r->balance);
#endif
                if ( NULL != (p->right = s = r->left) ) {
                    s->parent = p;
                }
                fake("subtree relocated");
                r->left = p;
                s = p->parent;
                p->parent = r;
                b -= 1;
                r->balance = b;
                b = -b;
#ifdef DEBUG_AVL
                fake("balances calculated");
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
                        fake("left from parent");
                        goto balance_left_s;
                    } else {
                        fake("right from parent");
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

                fake("RL-Rotation.");
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
}

static void add_to_free_list(ptr)
u *ptr;
{
    u size;
    struct free_block *p, *q, *r;
    /* When there is a distinction between data and address registers and/or
       accesses, gcc will choose data type for q, so an assignmnt to q will
       faciliate branching
     */

    fake((do_check_avl(),"add_to_free_list called"));
#ifdef SMALLOC_TRACE
    ptr[M_MAGIC] = LFMAGIC;
#endif
    size = *ptr & MASK;
#ifdef DEBUG_AVL
    dprintf1(2, "size:%d\n",size);
#endif
    q = (struct free_block *)size; /* this assignment is a hint for register
                                          choice */
    r = (struct free_block *)(ptr+OVERHEAD);
    count_up(large_free_stat, size);
    q = free_tree;
    for ( ; ; /*p = q*/) {
        p = (struct free_block *)q;
#ifdef DEBUG_AVL
        dprintf1(2, "checked node size %d\n",p->size);
#endif
        if (size < p->size) {
            if ( NULL != (q = p->left) ) {
                continue;
            }
            fake("add left");
            p->left = r;
            break;
        } else /* >= */ {
            if ( NULL != (q = p->right) ) {
                continue;
            }
            fake("add right");
            p->right = r;
            break;
        }
    }
    r->size    = size;
    r->parent  = p;
    r->left    = 0;
    r->right   = 0;
    r->balance = 0;
#ifdef DEBUG_AVL
    fake("built new leaf.");
    dprintf1(2, "p->balance:%d\n",p->balance);
#endif
    do {
        struct free_block *s;

        if (r == p->left) {
            balance_t b;

            if ( !(b = p->balance) ) {
#ifdef DEBUG_AVL
                dprintf1(2, "p->size: %d\n", p->size);
                dprintf1(2, "p->balance: %d\n", p->balance);
                dprintf1(2, "p->right-h: %d\n", check_avl(p, p->right));
                dprintf1(2, "p->left -h: %d\n", check_avl(p, p->left ));
                fake("growth propagation from left side");
#endif
                p->balance = -1;
            } else if (b < 0) {
#ifdef DEBUG_AVL
                dprintf1(2, "p->balance:%d\n",p->balance);
#endif
                if (r->balance < 0) {
                    /* R-Rotation */
                    fake("R-Rotation");
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
                    fake("LR-Rotation");
                    dprintf1(2, "t = %x\n",(p_uint)t);
                    dprintf1(2, "r->balance:%d\n",r->balance);
#endif
                    if ( NULL != (p->left  = s = t->right) ) {
                        s->parent = p;
                    }
                    fake("relocated right subtree");
                    t->right = p;
                    if ( NULL != (r->right = s = t->left) ) {
                        s->parent = r;
                    }
                    fake("relocated left subtree");
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
                    fake("balances calculated");
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
                    fake((do_check_avl(),"LR-Rotation completed."));
#endif
                }
                break;
            } else /* p->balance == +1 */ {
                p->balance = 0;
                fake("growth of left side balanced the node");
                break;
            }
        } else /* r == p->right */ {
            balance_t b;

            if ( !(b = p->balance) ) {
                fake("growth propagation from right side");
                p->balance++;
            } else if (b > 0) {
                if (r->balance > 0) {
                    /* L-Rotation */
                    fake("L-Rotation");
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
                    fake("RL-Rotation");
                    dprintf1(2, "t = %x\n",(p_uint)t);
                    dprintf1(2, "r->balance:%d\n",r->balance);
#endif
                    if ( NULL != (p->right = s = t->left) ) {
                        s->parent = p;
                    }
                    fake("relocated left subtree");
                    t->left  = p;
                    if ( NULL != (r->left  = s = t->right) ) {
                        s->parent = r;
                    }
                    fake("relocated right subtree");
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
                    fake("RL-Rotation completed.");
                }
                break;
            } else /* p->balance == -1 */ {
#ifdef DEBUG_AVL
                dprintf1(2, "p->balance: %d\n", p->balance);
                dprintf1(2, "p->right-h: %d\n", check_avl(p, p->right));
                dprintf1(2, "p->left -h: %d\n", check_avl(p, p->left ));
#endif
                p->balance = 0;
                fake("growth of right side balanced the node");
                break;
            }
        }
        r = p;
        p = p->parent;
    } while ( NULL != (q = p) );
    fake((do_check_avl(),"add_to_free_list successful"));
}

#else /* FIT_STYLE_FAST_FIT */

#if 0
void show_free_list()
{
   u *p;
   p = free_list;
   while (p) {
     show_block(p);
     p = l_next_ptr(p);
   }
   WRITES("\n");
}
#endif

t_stat large_free_stat;
static void remove_from_free_list(ptr)
u *ptr;
{
#ifdef SMALLOC_TRACE
   if (ptr[M_MAGIC] != LFMAGIC)
     fatal("remove_from_free_list: magic match failed\n");
#endif
   count_back(large_free_stat, *ptr & MASK);

   if (l_prev_ptr(ptr))
     l_next_ptr(l_prev_ptr(ptr)) = l_next_ptr(ptr);
   else
     free_list = l_next_ptr(ptr);

   if (l_next_ptr(ptr))
     l_prev_ptr(l_next_ptr(ptr)) = l_prev_ptr(ptr);
}

static void add_to_free_list(ptr)
u *ptr;
{
  count_up(large_free_stat, *ptr & MASK);

#ifdef DEBUG
  if (free_list && l_prev_ptr(free_list))
    puts("Free list consistency error.");
#endif

  l_next_ptr(ptr) = free_list;
  if (free_list)
    l_prev_ptr(free_list) = ptr;
  l_prev_ptr(ptr) = 0;
  free_list = ptr;
}
#endif /* FIT_STYLE_FAST_FIT */

static void build_block(ptr, size)        /* build a properly annotated unalloc block */
u *ptr;
u size;
{
  u tmp;

  tmp = (*ptr & PREV_BLOCK) | size;
  *(ptr+size-1) = size;
  *(ptr) = tmp;                /* mark this block as free */
  *(ptr+size) &= ~PREV_BLOCK; /* unmark previous block */
}

static void mark_block(ptr)                /* mark this block as allocated */
u *ptr;
{
  *l_next_block(ptr) |= PREV_BLOCK;
  *ptr |= THIS_BLOCK | M_GC_FREE | M_REF;
}

static t_stat large_alloc_stat;
#ifdef SMALLOC_TRACE
static char *_large_malloc(size, force_more, file, line)
    u size;
    int force_more;
    char *file; int line;
#else
static char *large_malloc(size, force_more)
    u size;
    int force_more;
#endif
{
    u real_size;
    u *ptr;

fake("large_malloc called");
#ifdef LARGE_TRACE
dprintf1(2, "request:%d.",size);
#endif
    size = (size + SINT*OVERHEAD + SINT-1) / SINT; /* plus overhead */
    count_up(large_alloc_stat, size);

retry:
    ptr = 0;
    if (!force_more) {
#ifdef FIT_STYLE_FAST_FIT

        struct free_block *p, *q, *r;
        u minsplit;
        u tempsize;

        ptr += OVERHEAD;
        minsplit = size + SMALL_BLOCK_MAX + OVERHEAD;
        q = free_tree;
        for ( ; ; ) {
            p = q;
#ifdef DEBUG_AVL
            dprintf1(2, "checked node size %d\n",p->size);
#endif
            tempsize = p->size;
            if (minsplit < tempsize) {
                ptr = (u*)p; /* remember this fit */
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
                    ptr = (u*)p;
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
                            ptr = (u*)p;
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
                                ptr = (u*)p; /* remember this fit */
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
                ptr = (u*)p;
                break;
            }
        } /* end outer for */
found_fit:
        ptr -= OVERHEAD;
#else /* FIT_STYLE */
        u best_size;
        u *first, *best;
#ifdef LARGE_TRACE
        u search_length=0;
#endif

        first = best = 0;
        best_size = MASK;
        ptr = free_list;

        while (ptr) {
            u tempsize;
#ifdef LARGE_TRACE
search_length++;
#endif
                /* Perfect fit? */
            tempsize = *ptr & MASK;
            if (tempsize == size) {
                best = first = ptr;
                break;
                /* always accept perfect fit */
            }

                /* does it really even fit at all */
            if (tempsize >= size + SMALL_BLOCK_MAX + OVERHEAD)
            {
                /* try first fit */
                if (!first)
                {
                    first = ptr;
                    if (fit_style == FIRST_FIT)
                    break;
                    /* just use this one! */
                }
                /* try best fit */
                tempsize -= size;
                if (tempsize>0 && tempsize<=best_size)
                {
                    best = ptr;
                    best_size = tempsize;
                }
            }
            ptr = l_next_ptr(ptr);
        } /* end while */

#ifdef LARGE_TRACE
dprintf1(2, "search length %d\n",search_length);
#endif
        if (fit_style==BEST_FIT) ptr = best;
        else ptr = first;
        /* FIRST_FIT and HYBRID both leave it in first */

#endif /* FIT_STYLE */
    } /* end of  if (!force_more) */
    if (!ptr)                /* no match, allocate more memory */
    {
      u chunk_size, block_size;
      block_size = size*SINT;
      if (force_more ||
          block_size > CHUNK_SIZE - SMALL_BLOCK_MAX_BYTES - OVERHEAD*SINT )
      {
        chunk_size = block_size;
      } else {
        chunk_size = CHUNK_SIZE;
      }

      {
#ifdef MAX_MALLOCED
        if ((mp_int)(sbrk_stat.size + chunk_size) > max_malloced &&
            (chunk_size = max_malloced - sbrk_stat.size - (heap_start?0:SINT) )
            < block_size)
        {
          ptr = 0;
        }
        else
#endif /* MAX_MALLOCED */
        {
          ptr = (u *) esbrk(chunk_size);
        }
      }
      if (ptr == 0) {
        static int going_to_exit=0;
        static char mess1[] =
          "Temporary out of MEMORY. Freeing user reserve.\n";
        static char mess2[] =
          "Temporary out of MEMORY. Freeing master reserve.\n";
        static char mess3[] =
          "Temporary out of MEMORY. Freeing system reserve.\n";
        static char mess4[] =
          "Totally out of MEMORY.\n";

        if (going_to_exit)
          exit(3);
        if (force_more && (mp_int)small_chunk_stat.size < max_small_malloced) {
            /* Now that out of memory is a possible error, we don't want
             * to have all large blocks fragmented to smithereens.
             * Set max_small_malloced accordingly.
             */
            force_more = 0;
            goto retry;
        } else {
            garbage_collect_to_do = MY_TRUE;
            extra_jobs_to_do = MY_TRUE;
            if (reserved_user_area) {
                sfree(reserved_user_area);
                reserved_user_area = 0;
                write(2, mess1, sizeof(mess1)-1);
                goto retry;
            }
            if (malloc_privilege >= MALLOC_MASTER && reserved_master_area) {
                sfree(reserved_master_area);
                reserved_master_area = 0;
                write(2, mess2, sizeof(mess2)-1);
                goto retry;
            }
            if (malloc_privilege >= MALLOC_SYSTEM && reserved_system_area) {
                sfree(reserved_system_area);
                reserved_system_area = 0;
                write(2, mess3, sizeof(mess3)-1);
                goto retry;
            }
        }
        if (malloc_privilege < MALLOC_SYSTEM) {
            count_back(large_alloc_stat, size);
            out_of_memory = 1;
            return 0;
        }
        going_to_exit = 1;
        write(2, mess4, sizeof(mess4)-1);
        (void)dump_trace(0);
#ifdef DEBUG
        fatal("Out of memory\n");
#else
        exit(2);
#endif
      }
#ifndef SBRK_OK
      chunk_size += overlap;
#endif /* SBRK_OK */
      block_size = chunk_size / SINT;

                        /* configure header info on chunk */

      build_block(ptr,block_size);
if (force_more)
fake("Build little block");
else
fake("Built memory block description.");
      add_to_free_list(ptr);
    }    /* end of creating a new chunk */
  remove_from_free_list(ptr);
  real_size = *ptr & MASK;

  if (real_size - size) {
        /* split block pointed to by ptr into two blocks */
    build_block(ptr+size, real_size-size);
fake("Built empty block");
#ifndef SBRK_OK
    /* When we allocate a new chunk, it might differ slightly in size from
     * the desired size.
     */
    if (real_size - size < SMALL_BLOCK_MAX + OVERHEAD) {
        mark_block(ptr+size);
        *(ptr+size) &= ~M_GC_FREE;
    }
    else
#endif
    {
        add_to_free_list(ptr+size);
    }
    build_block(ptr, size);
  }

  mark_block(ptr);
fake("built allocated block");
#ifdef SMALLOC_LPC_TRACE
  ptr[M_OBJ]  = (u)current_object;
  ptr[M_PROG] = current_prog ? current_prog->id_number : 0;
  ptr[M_PC]   = (u)inter_pc;
#endif
#ifdef SMALLOC_TRACE
  ptr[M_FILE] = (u)file;
  ptr[M_LINE] = line;
  ptr[M_MAGIC] = LAMAGIC;
#endif
  return (char *) (ptr + OVERHEAD);
}

static void large_free(ptr)
char *ptr;
{
  u size, *p;
  p = (u *) ptr;
  p -= OVERHEAD;
  size = *p & MASK;
  count_back(large_alloc_stat, size);

#ifdef SMALLOC_TRACE
  if (p[M_MAGIC] != LAMAGIC)
    fatal("large_free: magic match failed\n");
#endif
  if (!(*(p+size) & THIS_BLOCK)) {
    remove_from_free_list(p+size);
    size += (*(p+size) & MASK);
    *p = (*p & PREV_BLOCK) | size;
  }

  if (l_prev_free(p)) {
    remove_from_free_list(l_prev_block(p));
    size += (*l_prev_block(p) & MASK);
    p = l_prev_block(p);
  }

  build_block(p, size);

  add_to_free_list(p);
}

POINTER amalloc(size)
size_t size;
{
    u* temp;

    temp = (u*)xalloc(size+(MALLOC_ALIGN-SINT));
    if (!temp) {
        int save_privilege = malloc_privilege;
        malloc_privilege = MALLOC_SYSTEM;
        temp = (u*)xalloc(size+(MALLOC_ALIGN-SINT));
        malloc_privilege = save_privilege;
        if (!temp)
            return 0;
    }
    temp[-OVERHEAD] &= ~M_GC_FREE;
#if MALLOC_ALIGN > SINT
    while ((u)temp & (MALLOC_ALIGN-1)) *temp++ = 0;
#endif
    return (char*)temp;
}

POINTER permanent_xalloc(size)
size_t size;
{
    u* temp;

    temp = (u*)xalloc(size);
    if (temp)
        temp[-OVERHEAD] &= ~M_GC_FREE;
    return (char*)temp;
}

PFREE_RETURN_TYPE pfree(p)
POINTER p;
{
    ((u*)p)[-OVERHEAD] |= (M_REF|M_GC_FREE);
    sfree(p);
    PFREE_RETURN;
}

#if MALLOC_ALIGN > SINT || defined(FREE_NULL_POINTER)
FREE_RETURN_TYPE afree(p)
POINTER p; /* could be void *, thus needs casting */
{
    u*q = (u*)p;
#ifdef FREE_NULL_POINTER
    if (!q) return;
#endif
    while (!*--q) NOOP;
    q[1-OVERHEAD] |= (M_REF|M_GC_FREE);
    sfree((char *)(q+1));
    FREE_RETURN
}
#endif /* MALLOC_ALIGN */

#ifdef SBRK_OK
POINTER srealloc(p, size)
POINTER p; size_t size;
{
   u *q, old_size;
   char *t;

   q = (u *) p;

#ifdef FREE_NULL_POINTER
   if (!q)                        /* FreeBSD execl() does this */
        return amalloc(size);
#endif

#if MALLOC_ALIGN > SINT
   while ( !(old_size = *--q) );
#if OVERHEAD != 1
   old_size = ((*(q + 1 - OVERHEAD) & MASK)-OVERHEAD)*SINT;
#else
   old_size = ((old_size & MASK)-1)*SINT;
#endif /* OVERHEAD */
#else /* MALLOC_ALIGN */
   q -= OVERHEAD;
   old_size = ((*q & MASK)-OVERHEAD)*SINT;
#endif /* MALLOC_ALIGN */
   if (old_size >= size)
      return p;

   t = amalloc(size);
   if (t == 0) return (char *) 0;

   memcpy(t, p, old_size);
   afree(p);
   return t;
}
#endif

POINTER rexalloc(p, size)
POINTER p; size_t size;
{
   u *q, old_size;
   char *t;

   q = (u *) p;

   q -= OVERHEAD;
   old_size = ((*q & MASK)-OVERHEAD)*SINT;
   if (old_size >= size)
      return p;

   t = xalloc(size);
   if (t == 0) return (char *) 0;

   memcpy(t, p, old_size);
   xfree(p);
   return t;
}

/*
 * It is system dependent how sbrk() aligns data, so we simpy use brk()
 * to insure that we have enough.
 */
static char *esbrk(size)
u size;
{
#ifdef SBRK_OK
#ifdef SunOS4
  extern char *sbrk();
  extern int brk();
#endif

  if (!heap_end) {
    heap_start = heap_end = (u*)sbrk(0);
    if (!esbrk(SINT))
      fatal("Couldn't malloc anything\n");
    *heap_start = PREV_BLOCK;
    fake("Allocated little fake block");
  }
  if (brk((char *)heap_end + size) == -1)
    return 0;
  count_up(sbrk_stat,size);
  heap_end = (u*)((char *)heap_end + size);
  heap_end[-1] = THIS_BLOCK;
  return (char *)(heap_end - 1) - size;        /* overlap old memory block */
#else  /* not SBRK_OK */
  char *block;
  u *p;

  size += SINT;
  block = malloc(size);
  if (!block) return 0;
  p = (u*)(block + size) - 1;
  if (!heap_end) {
    heap_start = (u*)block;
    heap_end = (u*)(block + size);
    *(u*)block = PREV_BLOCK;
    *p = THIS_BLOCK; /* no M_GC_FREE */
    overlap = 0;
  } else {
    if (block < (char *)heap_start) {
      *(u*)block = PREV_BLOCK;
      if (block + size == (char *)heap_start) {
        p[1] &= ~PREV_BLOCK;
        overlap = SINT;
      } else {
        *p = THIS_BLOCK | (heap_start - p); /* no M_GC_FREE */
        overlap = 0;
      }
      heap_start = (u*)block;
    } else if (block >= (char *)heap_end) {
      *p = THIS_BLOCK; /* no M_GC_FREE */
      if (block == (char *)heap_end) {
        heap_end = (u*)(block + size);
        block -= SINT;
        overlap = SINT;
      } else {
        p = (u*)heap_end - 1;
        *p = (*p & (PREV_BLOCK|THIS_BLOCK|M_GC_FREE)) | ((u*)block - p);
        heap_end = (u*)(block + size);
        *(u*)block = PREV_BLOCK;
        overlap = 0;
      }
    } else {
      /* This is slow, but it shouldn't happen too often */
      u *prev, *next;

      next = heap_start;
      do {
        prev = next;
        next = prev + (*prev & MASK);
      } while (next < (u*)block);
      overlap = 0;
      if ((u*)block == prev + 1) {
        block -= SINT;
        overlap += SINT;
      } else {
        *prev = (*prev & (PREV_BLOCK|THIS_BLOCK|M_GC_FREE)) | ((u*)block - prev);
        *(u*)block = PREV_BLOCK;
      }
      if (next - p == 1) {
        *next &= ~PREV_BLOCK;
        overlap += SINT;
      } else {
        *p = THIS_BLOCK | (next - p); /* no M_GC_FREE */
      }
    }
  }
  count_up(sbrk_stat,size);
  return block;
#endif /* !SBRK_OK */
}

int resort_free_list() { return 0; }

int malloc_size_mask() { return MASK; }

static long malloc_increment_size_calls = 0;
static long malloc_increment_size_success = 0;
static long malloc_increment_size_total = 0;

#define dump_stat(str,stat) add_message(str,stat.counter,stat.size)
#define dump_stat2(str,stat) add_message (str,stat.counter,stat.size * SINT)
void dump_malloc_data()
{
  add_message("Type                   Count      Space (bytes)\n");
  dump_stat("sbrk requests:     %8d        %10d (a)\n",sbrk_stat);
  dump_stat2("large blocks:      %8d        %10d (b)\n",large_alloc_stat);
  dump_stat2("large free blocks: %8d        %10d (c)\n\n",large_free_stat);
  dump_stat("small chunks:      %8d        %10d (d)\n",small_chunk_stat);
  dump_stat("small blocks:      %8d        %10d (e)\n",small_alloc_stat);
  dump_stat("small free blocks: %8d        %10d (f)\n",small_free_stat);
  add_message(
"unused from current chunk          %10ld (g)\n\n",unused_size);
  add_message(
"    Small blocks are stored in small chunks, which are allocated as\n");
  add_message(
"large blocks.  Therefore, the total large blocks allocated (b) plus\n");
  add_message(
"the large free blocks (c) should equal total storage from sbrk (a).\n");
  add_message(
"Similarly, (e) + (f) + (g) equals (d).  The total amount of storage\n");
  add_message(
"wasted is (c) + (f) + (g); the amount allocated is (b) - (f) - (g).\n");
  add_message(
    "malloc_increment_size: calls %ld success %ld total %ld\n",
    malloc_increment_size_calls,
    malloc_increment_size_success,
    malloc_increment_size_total
  );
}

/*
 * calloc() is provided because some stdio packages uses it.
 */
POINTER calloc(nelem, sizel)
    size_t nelem, sizel;
{
    char *p;

    if (nelem == 0 || sizel == 0)
        return 0;
    p = amalloc(nelem * sizel);
    if (p == 0)
        return 0;
    (void)memset(p, '\0', nelem * sizel);
    return p;
}

#ifdef SMALLOC_TRACE
static int num_dispatched_types = 0;
static struct {
    char *file;
    u line;
    void (*func)PROT((int, char *, int));
} dispatch_table[12];

void store_print_block_dispatch_info(block, func)
    char *block;
    void (*func)PROT((int, char *, int));
{
    int i;

    i = num_dispatched_types++;
    if (i >= sizeof(dispatch_table)/sizeof(dispatch_table[0])) {
        WRITES(2, "dispatch_table for print_block() to small\n");
        return;
    }
    dispatch_table[i].file = (char *)((u*)block)[M_FILE-OVERHEAD];
    dispatch_table[i].line = ((u*)block)[M_LINE-OVERHEAD];
    dispatch_table[i].func = func;
}

int is_freed(p, minsize) /* might return false for joined blocks */
    char *p;
    p_uint minsize;
{
    u *block;
    u i;

    block = (u *) p;
    block -= OVERHEAD;
    if (block < heap_start || block + OVERHEAD >= heap_end)
        return 1;
    i = (*s_size_ptr(block) & MASK);
    if (i < OVERHEAD + ((minsize + 3) / SINT) || block + i >= heap_end)
        return 1;
    if (i > SMALL_BLOCK_MAX + OVERHEAD) {
        u* block2;

        block2 = block + i;
        return
          !(*s_size_ptr(block) & THIS_BLOCK) ||
          block[M_MAGIC] != LAMAGIC ||
          !(*block2 & PREV_BLOCK);
    }
    i -= 1 + OVERHEAD;
    return block[M_MAGIC] != samagic[i];
}
#endif /* SMALLOC_TRACE */
#ifdef SMALLOC_LPC_TRACE
static void write_lpc_trace(d, p)
    int d;
    u *p;
{
    struct object *obj, *o;
    char *pc;
    struct program *prog;
    int line;
    int32 id;

    if ( (obj = (struct object *)p[M_OBJ]) ) {
        WRITES(d, "Object: ");
        for(o = obj_list; o && o != obj; o = o->next_all);
        if (o && !(o->flags & O_DESTRUCTED)) WRITES(d, o->name);
        else WRITES(d, "Doesn't exist anymore.");
    }
    WRITES(d, "\n");
    if ( (id = p[M_PROG]) ) {
        WRITES(d, "Program: ");
        for (o = obj_list;
          o && !(o->flags & O_DESTRUCTED) &&
          ((p_int)o->prog&1 || o->prog->id_number != id); )
            o = o->next_all;
        /* Unlikely, but possible: ids might have been renumbered. */
        if (o) {
            pc = (char *)p[M_PC];
            prog = o->prog;
            if (prog->program > pc || pc > PROGRAM_END(*prog))
                o = 0;
        }
        if (o) {
            char *file;

            line = get_line_number(pc, prog, &file);
            dprintf2(d, "%s line:%d\n", (p_int)file, line);
        } else {
            WRITES(d, "Not found on old address.\n");
        }
    }
}
#endif

static void print_block(d, block)
    int d;
    u *block;
{
    u size;
#ifdef SMALLOC_TRACE
    int i;
    char *file = (char *)block[M_FILE];
    u line = block[M_LINE];

    for (i = num_dispatched_types; --i >= 0; ) {
        if (dispatch_table[i].file == file &&
            dispatch_table[i].line == line)
        {
            (*dispatch_table[i].func)(d, (char *)(block+OVERHEAD), 0);
            return;
        }
    }
#endif
    size = ((*block & MASK) - OVERHEAD)*SINT;
    if (d > 70)
        return;
    write(d, (char *)(block+OVERHEAD), size);
    write(d, "\n", 1);
}

void clear_M_REF_flags() {
    u *p, *q, *last;
    int i;

    last = heap_end - 1;
    for (p = heap_start; p < last; ) {
        *p &= ~M_REF;
        if (p + (*p & MASK) > heap_end) fatal("pointer larger than break\n");
        p += *p & MASK;
    }
    for (p = last_small_chunk; p; p = *(u**)p) {
        u *end;

        note_malloced_block_ref((char *)p);
        end = p - OVERHEAD + (p[-OVERHEAD] & MASK);
#ifdef DEBUG
        dprintf2(gout, "scanning chunk %x, end %x\n",
          (u)(p - OVERHEAD), (u)end
        );
#endif
        /* Well, if we are so unlucky that write used malloc, next_unused
         * might have changed.
         */
        if (unused_size)
            *next_unused = 0;
        for (q = p+1; q < end; ) {
            u size = *q;

            if (!size) break;
            *q &= ~M_REF;
            q += size & MASK;
        }
        if (q > end) {
            /* pass some variables to fatal() so that the optimizer
             * won't throw away the values, making them unreadable
             * in the core.
             */
            fatal("Small block error, start: %lx, %lx vs. %lx\n",
              (long)(p+1), (long)q, (long)end);
        }
    }

    /* We set M_REF for small free blocks that early for two reasons:
     * - if it's referenced anywhere else, we get a fatal.
     * - if the block gets malloced by the swapper in pass 5, it needs
     *   M_REF to be already set.
     */
    for (i=0; i < SMALL_BLOCK_MAX; i++) {
        for (p = sfltable[i]; p; p = * (u **) (p + OVERHEAD) ) {
            *p |= M_REF;
        }
    }
    first_showsmallnewmalloced_call = 1;
}

#if 0
void test_small() {
    u *p, *q;
    int i;

    if (unused_size)
        *next_unused = 0;
    for (p = last_small_chunk; p; p = *(u**)p) {
        u *end;

        end = p - OVERHEAD + (p[-OVERHEAD] & MASK);
        for (q = p+1; q < end; ) {
            u size = *q;

            if (!size) break;
            *q &= ~M_REF;
            q += size & MASK;
        }
        if (q > end)
            fatal("Small block error\n");
    }
}
#endif

void free_unreferenced_memory() {
    u *p, *q, *last;
    mp_int success = 0;

    last = heap_end - 1;
    for (p = heap_start; p < last; ) {
        u size;

        size = *p;
        if ( (size & (M_REF|THIS_BLOCK|M_GC_FREE)) == (THIS_BLOCK|M_GC_FREE) ) {
            u size2;

            success++;
#if defined(SMALLOC_TRACE) || defined(SMALLOC_LPC_TRACE)
            dprintf1(gout, "large block 0x%x", (p_uint)p);
#endif
#ifdef SMALLOC_TRACE
            dprintf3(gout, " %s %d size 0x%x\n",
              p[M_FILE], p[M_LINE], size & MASK
            );
#endif
#ifdef SMALLOC_LPC_TRACE
            write_lpc_trace(gout, p);
#endif
            print_block(gout, p);
            size2 = p[size & MASK];
            large_free((char *)(p+OVERHEAD));
            if ( !(size2 & THIS_BLOCK) )
                size += size2;
        }
        p += size & MASK;
    }
    if (success) {
        dprintf1(gout, "%d large blocks freed\n", success);
    }
    success = 0;
    for (p = last_small_chunk; p; p = *(u**)p) {
        u *end;

        end = p - OVERHEAD + (p[-OVERHEAD] & MASK);
#ifdef DEBUG
        dprintf2(gout, "scanning chunk %x, end %x\n",
          (u)(p - OVERHEAD), (u)end
        );
#endif
        if (unused_size)
            *next_unused = 0;
        for (q = p+1; q < end; ) {
            u size = *q;

            if (!size) break;
            if ((*q & (M_REF|M_GC_FREE)) == M_GC_FREE) {
                success++;
                dprintf1(gout, "small block 0x%x", (p_uint)q);
#ifdef SMALLOC_TRACE
                dprintf2(gout, " %s %d", q[M_FILE], q[M_LINE]);
#endif
                WRITES(gout, "\n");
#ifdef SMALLOC_LPC_TRACE
                write_lpc_trace(gout, q);
#endif
                print_block(gout, q);
                *q |= M_REF;
                sfree((char *)(q+OVERHEAD));
            }
            q += size & MASK;
        }
    }
    if (success) {
        dprintf1(gout, "%d small blocks freed\n", success);
    }
}
char *malloc_increment_size(p, size)
    char *p;
    p_int size;
{
    p_uint *start, *start2, *start3, old_size, next;

    malloc_increment_size_calls++;
    start = (p_uint*)p - OVERHEAD;
    old_size = start[0] & MASK;
    if (old_size <= SMALL_BLOCK_MAX + OVERHEAD)
        return 0;
    start2 = &start[old_size];
    next = *start2;
    if (next & THIS_BLOCK)
        return 0;
    next &= MASK;
    if (next == (p_uint)size) {
        remove_from_free_list(start2);
        start2[next] |= PREV_BLOCK;
        start[0] += size;
        malloc_increment_size_success++;
        malloc_increment_size_total += (start2 - start) - OVERHEAD;
        count_add(large_alloc_stat, size);
        return (char*)start2;
    }
    if (next >= (p_uint)size + SMALL_BLOCK_MAX + OVERHEAD) {
        remove_from_free_list(start2);
        start2[next-1] -= size;
        start3 = start2 + size;
        start3[0] = (next-size) | PREV_BLOCK;
        add_to_free_list(start3);
        start[0] += size;
        malloc_increment_size_success++;
        malloc_increment_size_total += (start2 - start) - OVERHEAD;
        count_add(large_alloc_stat, size);
        return (char*)start2;
    }
    return 0;
}


/*
 * Functions below can be used to debug malloc.
 */

void walk_new_small_malloced(func)
    void (*func) PROT((POINTER, long));
{
    int i;
    u *p, *q;

    for (i=0; i < SMALL_BLOCK_MAX; i++) {
        for (p = sfltable[i]; p; p = * (u **) (p + OVERHEAD) ) {
            *s_size_ptr(p) &= ~M_REF;
        }
    }
    for (p = last_small_chunk; p; p = *(u**)p) {
        u *end = p - OVERHEAD + (p[-OVERHEAD] & MASK);

        dprintf2(2, "scanning chunk %x, end %x\n", (u)(p - OVERHEAD), (u)end);
        if (unused_size)
            *next_unused = 0;
        for (q = p+1; q < end; ) {
            u size = *s_size_ptr(q);

            if (!size) break;
            if (size & M_REF) {
                (*func)( (char*)s_next_ptr(q), (size & MASK) * SINT);
                *s_size_ptr(q) &= ~M_REF;
            }
            q += size & MASK;
        }
    }
    for (i=0; i < SMALL_BLOCK_MAX; i++) {
        for (p = sfltable[i]; p; p = * (u **) (p + OVERHEAD) ) {
            *s_size_ptr(p) |= M_REF;
        }
    }
}

#if 0

int debugmalloc;
/*
 * Verify that the free list is correct. The upper limit compared to
 * is very machine dependant.
 */
verify_sfltable() {
    u *p;
    int i, j;

    if (!debugmalloc)
        return;
    if (unused_size > SMALL_CHUNK_SIZE)
        apa();
    for (i=0; i < SMALL_BLOCK_MAX; i++) {
        for (j=0, p = sfltable[i]; p; p = * (u **) (p + 1), j++) {
            if (p < (u *)&end || p > (u *) 0xfffff)
                apa();
            if (*p - 1 - OVERHEAD != i)
                apa();
        }
        if (p >= next_unused && p < next_unused + unused_size/SINT)
            apa();
    }
    p = free_list;
    while (p) {
        if (p >= next_unused && p < next_unused + unused_size/SINT)
            apa();
        p = l_next_ptr(p);
    }
}

verify_free(ptr)
    u *ptr;
{
    u *p;
    int i, j;

    if (!debugmalloc)
        return;
    for (i=0; i < SMALL_BLOCK_MAX; i++) {
        for (j=0, p = sfltable[i]; p; p = * (u **) (p + 1), j++) {
            if (*p - 1 - OVERHEAD != i)
                apa();
            if (ptr >= p && ptr < p + *p)
                apa();
            if (p >= ptr && p < ptr + *ptr)
                apa();
            if (p >= next_unused && p < next_unused + unused_size/SINT)
                apa();
        }
    }

    p = free_list;
    while (p) {
        if (ptr >= p && ptr < p + (*p & MASK))
            apa();
        if (p >= ptr && p < ptr + (*ptr & MASK))
            apa();
        if (p >= next_unused && p < next_unused + unused_size/SINT)
            apa();
        p = l_next_ptr(p);
    }
    if (ptr >= next_unused && ptr < next_unused + unused_size/SINT)
        apa();
}

apa() {
    int i;
    i/0;
}

static char *ref;
test_malloc(p)
    char *p;
{
    if (p == ref)
        dprintf1(gout, "Found 0x%x\n", p);
}

#endif /* 0 (never) */

/*-------------------------------------------------------------------------*/
char *dprintf_first(fd, s, a)
    int fd;
    char *s;
    p_int a;
{
    char *p;

    do {
        if ( !(p = strchr(s, '%')) ) {
            WRITES(fd, s);
            return "";
        }
        write(fd, s, p - s);
        switch(p[1]) {
          case '%':
            write(fd, p+1, 1);
            continue;
          case 's':
            WRITES(fd, (char *)a);
            break;
          case 'd':
            writed(fd, a);
            break;
          case 'x':
            writex(fd, a);
            break;
        }
        return p+2;
    } while (1);
}

/*-------------------------------------------------------------------------*/
void dprintf1(fd, s, a)
    int fd;
    char *s;
    p_int a;
{
    s = dprintf_first(fd, s, a);
    WRITES(fd, s);
}

/*-------------------------------------------------------------------------*/
void dprintf2(fd, s, a, b)
    int fd;
    char *s;
    p_int a, b;
{
    s = dprintf_first(fd, s, a);
    dprintf1(fd, s, b);
}

/*-------------------------------------------------------------------------*/
void dprintf3(fd, s, a, b, c)
    int fd;
    char *s;
    p_int a, b, c;
{
    s = dprintf_first(fd, s, a);
    dprintf2(fd, s, b, c);
}


