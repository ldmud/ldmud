/*
 * Copyright (c) 2001 matti.suomalainen@iki.fi
 * Free for non-commercial use.
 *
 * All rights reserved.
 */

#ifndef _PROFIL_H_
#define _PROFIL_H_

#ifdef __cplusplus
extern"C" {
#endif


#define PROFIL_MAX_COUNTER 31

#define PROFIL_SAMPLE_ALL 		((unsigned) -1)
#define PROFIL_SAMPLE_TIME		 1
#define PROFIL_SAMPLE_UTIME		 2
#define PROFIL_SAMPLE_STIME		 3
#define PROFIL_SAMPLE_RSS		 4
#define PROFIL_SAMPLE_IXRSS		 5
#define PROFIL_SAMPLE_IDRSS		 6
#define PROFIL_SAMPLE_ISRSS		 7
#define PROFIL_SAMPLE_MINFLT		 8
#define PROFIL_SAMPLE_MAJFLT		 9
#define PROFIL_SAMPLE_NSWAP		10
#define PROFIL_SAMPLE_INBLOCK		11
#define PROFIL_SAMPLE_OUBLOCK		12
#define PROFIL_SAMPLE_MSGSND		13
#define PROFIL_SAMPLE_MSGRCV		14
#define PROFIL_SAMPLE_NSIGNALS		15
#define PROFIL_SAMPLE_NVCSW		16
#define PROFIL_SAMPLE_NIVCSW		17
#define PROFIL_SAMPLE_ECOST		18
#define PROFIL_SAMPLE_CALLS		19

struct profil_sample
{
  unsigned long long c[PROFIL_MAX_COUNTER+1];
};

typedef struct profil_sample profil_sample_t;

void		profil_start_sample (profil_sample_t *sample);
void		profil_end_sample (profil_sample_t *sample);


void		profil_enable (unsigned int mask);
unsigned int	profil_is_enabled ();

void		profil_register_sample ( char *class_name, 
					 char *func_name,
					profil_sample_t *self,
					profil_sample_t *recur,
					int sample_mul);

unsigned int	profil_query_results (char *store_to, unsigned int max_len,
				      unsigned int sort_order, 
				      unsigned int max_entries);
void		profil_reset ();


#ifdef __cplusplus
}
#endif


#endif
