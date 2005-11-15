#ifndef WIZLIST_H__
#define WIZLIST_H__ 1

#include "driver.h"
#include "svalue.h"

/* --- Types ---
 */

struct wiz_list_s
{
    wiz_list_t *next;        /* Next entry in list */
    char *name;              /* Name of this wizard (shared string),
                              * NULL for the default entry */
    size_t length;           /* strlen(.name) */
    int32  score;            /* Number of actions executed by t.w. objects */
    int32  cost;             /* Evalticks spent on this wizards objects */
    int32  heart_beats;      /* Number of heart_beat() calls */
    mp_int size_array;       /* Total size of this wizards arrays. */
    mp_int mapping_total;    /* Total size of this wizards mappings */
    svalue_t extra;          /* Extra information for this wizard */
    int32  last_call_out;    /* Time of the last call_out() */
    int32  call_out_cost;    /* Collected cost of call_outs for this time */

    /* The following values are all used to store the last error
     * message.
     */
    char *file_name;         /* Allocated: the object's filename */
    char *error_message;     /* Allocated: the error message */
    int32 line_number;       /* The line number. Bit 30 is the forget-flag */
};

/* --- Variables --- */

extern wiz_list_t *all_wiz;
extern wiz_list_t default_wizlist_entry;
extern char wizlist_name[];

/* --- Prototypes --- */

extern void name_wizlist_file (const char *name);
extern size_t wiz_list_size (void);
extern wiz_list_t *add_name(char *str);
extern void wiz_decay(void);
extern void load_wiz_file(void);
extern void remove_wiz_list(void);
extern void save_error(char *msg, char *file, int line);
extern char *get_wiz_name(char *file);
extern void check_wizlist_for_destr(void);
extern svalue_t *f_get_error_file(svalue_t *sp);
extern svalue_t *f_wizlist_info(svalue_t *sp);
extern svalue_t *f_set_extra_wizinfo(svalue_t *sp);
extern svalue_t *f_get_extra_wizinfo(svalue_t *sp);
extern svalue_t *f_set_extra_wizinfo_size(svalue_t *sp);

#ifdef GC_SUPPORT
extern void clear_ref_from_wiz_list(void);
extern void count_ref_from_wiz_list(void);
#endif /* MALLOC_smalloc */

#ifdef DEBUG
extern void count_extra_ref_from_wiz_list(void);
#endif

#endif /* WIZLIST_H__ */
