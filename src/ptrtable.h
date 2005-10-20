#ifndef PTRTABLE_H__
#define PTRTABLE_H__ 1

#include "driver.h"

/* --- Types --- */

/* One entry in the pointer table
 */

struct pointer_record
{
    /* Private data: */
    mp_int key;            /* Casted: the pointer registered with this record */
    struct pointer_record *next;      /* Next entry in hash chain */

    /* Public data: */
    long ref_count;  /* Number of registrations of this pointer */
    long id_number;  /* User assigned ID number */
    void *data;      /* User assigned extra data */
};


/* --- Prototypes --- */

extern struct pointer_table * new_pointer_table(void);
extern void free_pointer_table(struct pointer_table *ptable);
extern struct pointer_record * find_add_pointer(struct pointer_table *ptable, void *pointer, Bool bAdd);
extern struct pointer_record * register_pointer(struct pointer_table *ptable, void *pointer);

#define lookup_pointer(ptable, p) find_add_pointer(ptable, p, MY_FALSE)

#endif /* PTRTABLE_H__ */
