#include "interpret.h"

struct wiz_list {
    char *name;
    int length;
    struct wiz_list *next;
    int score;
    int cost;
    int heart_beats;
    mp_int size_array;		/* Total size of this wizards arrays. */
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

extern struct wiz_list *add_name PROT((char *));
struct value;
extern void save_wiz_file(), load_wiz_file(), wiz_decay();
