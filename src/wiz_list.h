#ifndef __WIZLIST_H__
#define __WIZLIST_H__ 1

#include "driver.h"
#include "interpret.h"  /* struct svalue */

/* --- Types --- */
struct wiz_list {
    char *name;
    int length;
    struct wiz_list *next;
    int score;
    int cost;
    int heart_beats;
    mp_int size_array;                /* Total size of this wizards arrays. */
    mp_int mapping_total;
    struct svalue extra;
    mp_int quota_allowance;
    mp_int quota_usage;
    /*
     * The following values are all used to store the last error
     * message.
     */
    char *file_name;
    char *error_message;
    int32 line_number;
    int32 last_call_out;
    int32 call_out_cost;
    char quota_state;
};

/* --- Variables --- */

extern struct wiz_list *all_wiz;
extern int wiz_info_extra_size;
extern struct wiz_list default_wizlist_entry;

/* --- Prototypes --- */

extern struct wiz_list *add_name PROT((char *str));
extern void wiz_decay PROT((void));
extern void load_wiz_file PROT((void));
extern struct svalue *f_wizlist_info PROT((struct svalue *sp));
extern struct svalue *f_set_extra_wizinfo PROT((struct svalue *sp));
extern struct svalue *f_get_extra_wizinfo PROT((struct svalue *sp));
extern void remove_wiz_list PROT((void));
extern void save_error PROT((char *msg, char *file, int line));
extern struct svalue *f_get_error_file PROT((struct svalue *sp));
extern char *get_wiz_name PROT((char *file));

#ifdef MALLOC_smalloc
extern void clear_ref_from_wiz_list PROT((void));
extern void count_ref_from_wiz_list PROT((void));
#endif /* MALLOC_SMALLOC */

#ifdef DEBUG
extern void count_extra_ref_from_wiz_list PROT((void));
#endif

#endif /* __WIZLIST_H__ */
