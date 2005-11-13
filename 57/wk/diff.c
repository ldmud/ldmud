/*
 * diff -- computes the difference between two strings
 * (the minimal number of changes needed to make one equal the other where
 *  a change is one of the following:
 *   - replacing a character by another one
 *   - inserting a character
 *   - deleting a character
 *   - swapping two consecutive characters)
 *
 * by Alfe for TubMud 99-Mar-10
 * original idea and a first LPC implementation by Ugh
 */

#define DEBUG
#ifdef DEBUG
#  define _D_ printf("%s %d\n",__FILE__,__LINE__)
#  define _D(x) printf x
int global_debug_counter = 0;
#else
#  define _D_
#  define _D(x)
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int diff(char *,char *,int);
void cleanup_diff_pool();

void main(int argc,char *argv[]) {
  if (argc != 4) {
    printf("usage: %s <arg1> <arg2> <max>\n",argv[0]);
    exit(1);
  }
  printf("%d\n",diff(argv[1],argv[2],atoi(argv[3])));
  /*
   * cleanup_diff_pool should be called from time to time when memory
   * is needed.  to call it here is just one solution (especially in
   * combination with debug output to check that all entries are freed
   * again)
   */
  cleanup_diff_pool();
}

/*
 * an entry in the list of found positions
 * a -- position in first string
 * b -- position in second string
 * d -- difference at that point
 */
typedef struct entry {
  struct entry *next;
  int a,b,d;
} entry;

/*
 * this is a pool of disposed entries which are reused to reduce memory
 * management overhead.
 */
entry *diff_pool = NULL;

/*
 * make a new entry, fill it with the given values, return it.
 * this function could well use a pool of old disposed entries instead
 * of calling malloc() to create a new entry.
 */
entry *make_entry(int a,int b,int d) {
  entry *e;
#define USE_DISPOSED_POOL
#ifndef USE_DISPOSED_POOL
  e = malloc(sizeof(entry));
  _D(("mem %p %5d %4d %4d %3d malloc\n",e,global_debug_counter++,a,b,d));
#else
  if (diff_pool) {
    e = diff_pool;
    diff_pool = diff_pool->next;
    _D(("mem %p %5d %4d %4d %3d reuse\n",e,global_debug_counter++,a,b,d));
  }
  else {
    e = malloc(sizeof(entry));
    _D(("mem %p %5d %4d %4d %3d malloc\n",e,global_debug_counter++,a,b,d));
  }
#endif
  e->a = a;
  e->b = b;
  e->d = d;
  return e;
}

/*
 * dispose the given entry (e.g. free it); this function could well put
 * the entry into a pool of disposed entries to reduce memory management
 * overhead.
 */
void dispose_entry(entry **ep) {
#ifndef USE_DISPOSED_POOL
  _D(("mem %p %5d %4d %4d %3d free\n",(*ep),global_debug_counter++,
      (*ep)->a,(*ep)->b,(*ep)->d));
  free(*ep);
#else
  _D(("mem %p %5d %4d %4d %3d dispose\n",(*ep),global_debug_counter++,
      (*ep)->a,(*ep)->b,(*ep)->d));
  (*ep)->next = diff_pool;
  diff_pool = (*ep);
#endif
  (*ep) = NULL;
}

/*
 * this should cleanup the memory, i.e. free the pool of old disposed entries
 * if any exists.  it should be called from time to time in a running system,
 * but not too often to reduce memory management overhead.
 */
void cleanup_diff_pool() {
  int counter;
  entry *walker,*e;
  walker = diff_pool;
  for (counter=0; walker; counter++) {
    e = walker;
    walker = walker->next;
    _D(("mem %p %5d %4d %4d %3d free\n",e,global_debug_counter++,
	e->a,e->b,e->d));
    free(e);
  }
  diff_pool = NULL;
  _D(("info %d\n",counter));
}

/*
 * insert the given entry into the given list at the appropriate place.
 */
void insert_entry(entry *e,entry **startp) {
  entry *walker;
  if (!(*startp) ||  // list empty?
      e->d < (*startp)->d) {  // less than the start entry?
    e->next = (*startp);
    (*startp) = e;
    return;
  }
  walker = (*startp);
  while (walker->next && e->d >= walker->next->d) {
    if (walker->a == e->a &&
	walker->b == e->b) {  // a shorter way to this pos already in list?
      dispose_entry(&e);
      return;
    }
    walker = walker->next;
  }
  // normal case: found place to insert entry
  e->next = walker->next;
  walker->next = e;
}

/*
 * use the first entry of the given queue and produce children of it; add
 * them to the list of entries at the appropriate places.
 */
void add_new_entries(char *a,char *b,entry **startp) {
  entry *old;
  int ap,bp,d;
  ap = (*startp)->a;
  bp = (*startp)->b;
  d  = (*startp)->d;
  // remove first from list (we will process it now)
  old = (*startp);
  (*startp) = (*startp)->next;
  dispose_entry(&old);
  if (a[ap] && b[bp]) {  // both strings not at their end?
    // create usual child (both strings one further)
    if (a[ap] == b[bp]) {  // chars equal?
      insert_entry(make_entry(ap+1,bp+1,d),startp);
      return;
    }
    else {  // not equal, counts as one difference
      insert_entry(make_entry(ap+1,bp+1,d+1),startp);
      // check for swapped letters: one diff
      if (a[ap+1] && b[bp+1])  // both strings still have one more char?
        if (a[ap] == b[bp+1] &&
            b[bp] == a[ap+1])
          insert_entry(make_entry(ap+2,bp+2,d+1),startp);
    }
  }
  if (a[ap]) {  // first not at its end?
    // create difference child (assume missing letter in second: one diff)
    insert_entry(make_entry(ap+1,bp,d+1),startp);
  }
  if (b[bp]) {  // second not at its end?
    // create difference child (assume missing letter in first: one diff)
    insert_entry(make_entry(ap,bp+1,d+1),startp);
  }
}

int diff(char *a,char *b,int max) {
  char *s;
  int al,bl,h;
  entry *e,*start;
  al = strlen(a);
  bl = strlen(b);
  start = make_entry(0,0,0);  // start value: at pos (0,0) difference 0
  while (start->a < al ||
         start->b < bl) {
    if (max > 0 && start->d >= max)  // maximum reached?
      break;
    add_new_entries(a,b,&start);
  }
  h = start->d;
  while (start) {
    e = start;
    start = start->next;
    dispose_entry(&e);
  }
  return h;
}
