/*
 * Copyright (c) 2001 matti.suomalainen@iki.fi
 * Free for non-commercial use.
 *
 * All rights reserved.
 */

#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <string.h>
#include <malloc.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <stdlib.h>

#include "crc32.c"

#include "profil2.h"

#ifdef __i386__
#define USE_RDTSC
#endif



/*
 * pool alloc
 */

#define POOL_SIZE (1024*1024)

struct pool
{
  struct pool *prev;

  unsigned char pool [POOL_SIZE];
  unsigned int space_left;
};

static struct pool *pool= NULL;

static void
_add_pool ()
{
  struct pool *p;

  p= (struct pool*) calloc (1, sizeof (*p));
  p->prev= pool;
  pool= p;

  p->space_left= POOL_SIZE;
}

static char*
_pool_strdup (char *txt)
{
#if 0
  return txt;
#else
  unsigned int len= strlen (txt)+1;
  char *p;

  if (pool== NULL || pool->space_left <len)
    _add_pool ();

  p= &pool->pool [POOL_SIZE- pool->space_left];
  pool->space_left -= len;
  
  strcpy (p, txt);
  return p;
#endif
}

static void
_pool_free ()
{
  struct pool *p, *prev;

  p= pool;

  while (p)
    {
      prev= p->prev;
      free (p);
      p= prev;
    }
  pool= NULL;
}
  


/*
 * gathering data
 */

static unsigned int active_profiling;
static profil_sample_t started;
static profil_sample_t total_overhead;

static void
_profil_sample_add (profil_sample_t *to, const profil_sample_t *by)
{
  unsigned int idx;

  for (idx= 0; idx < PROFIL_MAX_COUNTER; idx++)
    {
      // assert (to->c [idx] < 0x00FFffFFff00000000);
      to->c [idx] += by->c [idx];
      // assert (to->c [idx] < 0x00FFffFFff00000000);
    }
}

static void
_profil_sample_sub (profil_sample_t *to, const profil_sample_t *by)
{
  unsigned int idx;

  for (idx= 0; idx < PROFIL_MAX_COUNTER; idx++)
    {
      // assert (to->c [idx] < 0x00FFffFFff00000000);
      to->c [idx] -= by->c [idx];
      // assert (to->c [idx] < 0x00FFffFFff00000000);
    }
}

static void
_profil_sample_mul (profil_sample_t *to, int by)
{
  unsigned int idx;

  for (idx= 0; idx < PROFIL_MAX_COUNTER; idx++)
    {
      // assert (to->c [idx] < 0x00FFffFFff00000000);
      to->c [idx] *= by;
      // assert (to->c [idx] < 0x00FFffFFff00000000);

    }
}

static void
_profil_sample_copy (profil_sample_t *to, profil_sample_t *from)
{
  memcpy (to, from, sizeof (profil_sample_t));
}

unsigned long long
_profil_get_time ()
{
#ifdef USE_RDTSC
  register unsigned long long ret;
  
  __asm__ __volatile__ ("rdtsc" : "=A"(ret));
  
  return ret;
#else
  
#if 1
  struct timeval tv;
  unsigned long long ret;
  
  gettimeofday (&tv, NULL);
  ret= (unsigned long long) tv.tv_sec*1000*1000+ tv.tv_usec;
  
  return ret;
  
#else
  struct timespec tp;
  clock_gettime (CLOCK_REALTIME, &tp);
  return tp.tv_sec*1000*1000+ tp.tv_nsec/1000;
#endif
  
#endif
}

double
profil_time_to_msec (unsigned long long proftime)
{
#ifdef USE_RDTSC
  static double clocks_to_msec= 0;
  
#define CALIBRATE_TIME_SECS 1
  
  if (clocks_to_msec== 0)
    {
      unsigned long long start, end;

      start= _profil_get_time ();
      sleep (CALIBRATE_TIME_SECS);
      end= _profil_get_time ();
      clocks_to_msec= (end-start)/(CALIBRATE_TIME_SECS* 1000);
      
      if (!clocks_to_msec)
        return 0;
    }

  return proftime/clocks_to_msec;
#else
  return proftime/1000;
#endif
}


static unsigned long long
_profil_timeval_to_longlong (struct timeval tv)
{
  return 
    (unsigned long long) tv.tv_sec*1000+ 
    (unsigned long long) tv.tv_usec/1000;
}

void
_profil_get_sample (profil_sample_t *sample)
{
  memset (sample, 0, sizeof (profil_sample_t));

  if (active_profiling >= 1)
    sample->c [PROFIL_SAMPLE_TIME]= _profil_get_time ();

  if (active_profiling >= 2)
    {
      struct rusage ru;

      getrusage (RUSAGE_SELF, &ru);
      
      sample->c [PROFIL_SAMPLE_UTIME]=_profil_timeval_to_longlong(ru.ru_utime);
      sample->c [PROFIL_SAMPLE_STIME]=_profil_timeval_to_longlong(ru.ru_stime);
      sample->c [PROFIL_SAMPLE_RSS]= ru.ru_maxrss;
      sample->c [PROFIL_SAMPLE_IXRSS]= ru.ru_ixrss;
      sample->c [PROFIL_SAMPLE_IDRSS]= ru.ru_idrss;
      sample->c [PROFIL_SAMPLE_ISRSS]= ru.ru_isrss;
      sample->c [PROFIL_SAMPLE_MINFLT]= ru.ru_minflt;
      sample->c [PROFIL_SAMPLE_MAJFLT]= ru.ru_majflt;
      sample->c [PROFIL_SAMPLE_NSWAP]= ru.ru_nswap;
      sample->c [PROFIL_SAMPLE_INBLOCK]= ru.ru_inblock;
      sample->c [PROFIL_SAMPLE_OUBLOCK]= ru.ru_oublock;
      sample->c [PROFIL_SAMPLE_MSGSND]= ru.ru_msgsnd;
      sample->c [PROFIL_SAMPLE_MSGRCV]= ru.ru_msgrcv;
      sample->c [PROFIL_SAMPLE_NSIGNALS]= ru.ru_nsignals;
      sample->c [PROFIL_SAMPLE_NVCSW]= ru.ru_nvcsw;
      sample->c [PROFIL_SAMPLE_NIVCSW]= ru.ru_nivcsw;
    }
}

void
profil_start_sample (profil_sample_t *sample)
{
  _profil_get_sample (sample);
}

void
profil_end_sample (profil_sample_t *sample)
{
  if (active_profiling)
    {
      profil_sample_t tmp;
      _profil_sample_copy (&tmp, sample);
      _profil_get_sample (sample);
      _profil_sample_sub (sample, &tmp);
    }
}

/*
 * updating and storing samples
 */


#define CACHE_SLOTS 1000000

struct profil_entry
{
  profil_sample_t self, recur;

  unsigned int owns_fun_name;
  unsigned int fun_hash;
  char *fun_name;

  unsigned int owns_class_name;
  unsigned int class_hash;
  char *class_name;
};

struct hash
{
  struct profil_entry *data;
  unsigned int len;
  unsigned int len_in_use;
};

static struct hash sample_lookup;
static struct hash class_lookup;
static struct hash fun_lookup;


void
_hash_init (struct hash *h)
{
  memset (h, 0, sizeof (*h));
}

struct profil_entry*
_hash_find (struct hash *h, char *class_name, char *fun_name)
{
  unsigned int class_hash= 
    class_name? ssh_crc32 (class_name, strlen (class_name)): 0;

  unsigned int fun_hash= 
    fun_name? ssh_crc32 (fun_name, strlen (fun_name)): 0;

  unsigned int idx;

  if (h->len== 0 || h->len_in_use== 0)
    return NULL;

  idx= class_hash;

  if (fun_name)
    idx ^= fun_hash;

  idx %= h->len;


  while (h->data [idx].class_name != 0)
    {
      if (h->data [idx].class_hash== class_hash &&
	  !strcmp (h->data [idx].class_name, class_name))
	{
	  if (!fun_name ||
	      (h->data [idx].fun_hash== fun_hash &&
	       !strcmp (h->data [idx].fun_name, fun_name)))
	    {
	      return &h->data [idx];
	    }
	}

      idx++;
      idx %= h->len;

   }
  
  return NULL;
}


struct profil_entry*
_hash_next (struct hash *h, struct profil_entry *from)
{
  if (h->len== 0)
    return NULL;

  if (from== NULL)
    from= h->data;

  from++;

  if (from== &h->data [h->len])
    return NULL;

  if (from->class_name)
    return from;

  return _hash_next (h, from);
}


void
_hash_insert (struct hash *h, struct profil_entry *entry)
{
  if (h->len_in_use >= h->len*2/3)
    {
      /* regrow */

      struct hash h2;
      struct profil_entry *iter;

      //      fprintf (stderr, "REGROW %p, len=%d, use= %d\n",  h, h->len, h->len_in_use);

      _hash_init (&h2);
      h2.len= (h->len* 2)+1024;
      h2.len_in_use= 0;
      h2.data= (struct profil_entry*) calloc (1, 
					      sizeof (struct profil_entry)*
					      h2.len);

      iter= NULL;

      while ( (iter= _hash_next (h, iter)) )
	_hash_insert (&h2, iter);
      
      free (h->data);
      *h= h2;
    }

  {
    unsigned int idx;

    entry->class_hash= 
      entry->class_name? ssh_crc32 (entry->class_name, strlen (entry->class_name)): 0;

    entry->fun_hash= entry->fun_name? ssh_crc32 (entry->fun_name, strlen (entry->fun_name)): 0;


    idx= entry->class_hash;
    
    if (entry->fun_name)
      idx ^= entry->fun_hash;

    idx %= h->len;

    while (h->data [idx].class_name != 0)
      {
	if (h->data [idx].class_hash== entry->class_hash &&
	    !strcmp (h->data [idx].class_name, entry->class_name))
	  {
	    if (!entry->fun_name ||
	      (h->data [idx].fun_hash== entry->fun_hash &&
	       !strcmp (h->data [idx].fun_name, entry->fun_name)))
	      {
		h->data [idx]= *entry;
		return;
	      }
	  }
      
	idx++;
	idx %= h->len;
	
      }

    h->len_in_use++;
    h->data [idx]= *entry;
  }
}  

void		
profil_enable (unsigned int how)
{
  active_profiling= how;
  profil_reset ();
}

unsigned int	
profil_is_enabled ()
{
  return active_profiling;
}

struct profil_entry*
_profil_find_entry (char *class_name, char *fun_name)
{
  struct profil_entry *e;
  struct profil_entry tmp;

  if ( (e= _hash_find (&sample_lookup, class_name, fun_name)))
    return e;

  memset (&tmp, 0, sizeof (tmp));

  if ( (e= _hash_find (&class_lookup, class_name, NULL)) )
    {
      tmp.class_name= e->class_name;
      tmp.owns_class_name= 0;
    }
  else
    {
      tmp.class_name= _pool_strdup (class_name);
      tmp.owns_class_name= 1;

      _hash_insert (&class_lookup, &tmp);
    }


  if ( (e= _hash_find (&fun_lookup, fun_name, NULL)) )
    {
      tmp.fun_name= e->class_name;
      tmp.owns_fun_name= 0;
    }
  else
    {
      tmp.class_name= _pool_strdup (fun_name);
      _hash_insert (&fun_lookup, &tmp);

      tmp.fun_name= tmp.class_name;
      tmp.class_name= NULL;
      tmp.owns_fun_name= 1;
    }

  _hash_insert (&sample_lookup, &tmp);

  return _profil_find_entry (class_name, fun_name);
}

void
profil_register_sample (char *class_name, 
			char *fun_name,
			profil_sample_t *self,
			 profil_sample_t *recur,
			int sample_mul)
{
  if (profil_is_enabled ())
    {
      profil_sample_t this_overhead;
      struct profil_entry *e;

      profil_start_sample (&this_overhead);

      e= _profil_find_entry (class_name, fun_name);

      if (self)
	{
	  self->c [PROFIL_SAMPLE_CALLS]++;
  
	  _profil_sample_mul (self, sample_mul);
	  _profil_sample_add (&e->self, self);
	}

      if (recur)
	{
	  _profil_sample_mul (recur, sample_mul);
	  _profil_sample_add (&e->recur, self);
	}

      profil_end_sample (&this_overhead);
      _profil_sample_add (&total_overhead, &this_overhead);
    }
}

/*
 * output & reset
 */

static unsigned int profil_sorter_arg= 0;

static int
_profil_sorter (const void *_a, const void *_b)
{
  struct profil_entry *a= (struct profil_entry*) _a;
  struct profil_entry *b= (struct profil_entry*) _b;
  
  if (a->self.c [profil_sorter_arg]== b->self.c [profil_sorter_arg])
    return 0;
  
  if (a->self.c [profil_sorter_arg] > b->self.c [profil_sorter_arg])
    return -1;
  else 
    return 1;
}

static unsigned int
_profil_output_entry (char *store_to, unsigned int max_len, 
		      unsigned long long total_time,
		      const char *class_name, const char *fun_name,
		      profil_sample_t *sample)
{
  char buf [10*1024];

  snprintf (buf, sizeof (buf), 
	    "%s::%s %.1f %.2f %.2f %.2f %.2f "
#if 1
	    "%.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f"
#endif
	    "\n", 
	    class_name, fun_name,

	   sample->c [PROFIL_SAMPLE_TIME]?
	   (double)sample->c [PROFIL_SAMPLE_TIME]*100/total_time: 0,

	   profil_time_to_msec (sample->c [PROFIL_SAMPLE_TIME]/1000.0)
	    ,
	   sample->c [PROFIL_SAMPLE_UTIME]/1000.0,
	   sample->c [PROFIL_SAMPLE_STIME]/1000.0,
	   sample->c [PROFIL_SAMPLE_RSS]/1000.0
#if 1
	    ,
	   sample->c [PROFIL_SAMPLE_IXRSS]/1000.0,
	   sample->c [PROFIL_SAMPLE_IDRSS]/1000.0,
	   sample->c [PROFIL_SAMPLE_ISRSS]/1000.0,
	   (double) sample->c [PROFIL_SAMPLE_MINFLT],
	   (double) sample->c [PROFIL_SAMPLE_MAJFLT],
	   (double) sample->c [PROFIL_SAMPLE_NSWAP],
	   (double) sample->c [PROFIL_SAMPLE_INBLOCK],
	   (double) sample->c [PROFIL_SAMPLE_OUBLOCK],
	   (double) sample->c [PROFIL_SAMPLE_MSGSND],
	   (double) sample->c [PROFIL_SAMPLE_MSGRCV],
	   (double) sample->c [PROFIL_SAMPLE_NSIGNALS],
	   (double) sample->c [PROFIL_SAMPLE_NVCSW],
	   (double) sample->c [PROFIL_SAMPLE_NIVCSW],
	    (double) sample->c [PROFIL_SAMPLE_ECOST],
	   (double) sample->c [PROFIL_SAMPLE_CALLS]
#endif
	    );
  


  if (strlen (buf) < max_len)
    strcpy (store_to, buf);
  return strlen (buf);
}


unsigned int
profil_query_results (char *store_to, unsigned int max_len,
		      unsigned int sort_order, 
		      unsigned int max_entries)
{
  struct profil_entry *entries;
  struct profil_entry *iter;
  unsigned int idx;
  unsigned long long low_water= 0;
  unsigned int total_len= 0;
  unsigned int len= 0;

  profil_sample_t total_time;

#if 0
  {
    unsigned int n;
    profil_sample_t sample;

    profil_start_sample (&sample);
    usleep (1000);
    profil_end_sample (&sample);

    for (n= 0; n < 10*1000; n++)
      profil_register_sample ("foo", "bar", &sample, &sample, 1);
  }
#endif

  entries= (struct profil_entry*) alloca (max_entries* 
					  sizeof (struct profil_entry));
  memset (entries, 0, max_entries* sizeof (struct profil_entry));

  if (sort_order >= PROFIL_MAX_COUNTER)
    return 0;

  iter= NULL;

  while ( (iter= _hash_next (&sample_lookup, iter)) )
    if (iter->self.c [sort_order] >= low_water)
      {
	
	entries [max_entries-1]= *iter;

	profil_sorter_arg= sort_order;

	qsort (entries, max_entries, sizeof (struct profil_entry), 
	       _profil_sorter);
	low_water= entries [max_entries-1].self.c [sort_order];
      }



  profil_start_sample (&total_time);
  _profil_sample_sub (&total_time, &started);

  len= _profil_output_entry (store_to, max_len, 
			     total_time.c [PROFIL_SAMPLE_TIME],
			     "total", "", &total_time);
  
    if (len <= max_len)
      {
	store_to+= len;
	max_len -= len;
      }
    total_len += len;

    len= _profil_output_entry (store_to, max_len,
			     total_time.c [PROFIL_SAMPLE_TIME],

			       "overhead", "", &total_overhead);
    
    if (len <= max_len)
      {
	store_to+= len;
	max_len -= len;
      }

    total_len += len;


    for (idx= 0; idx < max_entries; idx++)
    if (entries [idx].class_name)
      {
	len= _profil_output_entry (store_to, max_len,
			     total_time.c [PROFIL_SAMPLE_TIME],
						entries [idx].class_name,
						entries [idx].fun_name,


						&entries [idx].self);
	
    if (len <= max_len)
      {
	store_to+= len;
	max_len -= len;
      }

	total_len += len;
      }

  return total_len+1;
}

void
profil_reset ()
{

#if 0
  while ( (iter= _hash_next (&sample_lookup, iter)) )
    {
      if (iter->owns_fun_name)
	free (iter->fun_name);

      if (iter->owns_class_name)
	free (iter->class_name);
    }
#endif

  free (sample_lookup.data);
  free (class_lookup.data);
  free (fun_lookup.data);

  memset (&sample_lookup, 0, sizeof (sample_lookup));
  memset (&class_lookup, 0, sizeof (sample_lookup));
  memset (&fun_lookup, 0, sizeof (sample_lookup));

  _pool_free ();

  profil_start_sample (&started);
  memset (&total_overhead, 0, sizeof (total_overhead));
}

