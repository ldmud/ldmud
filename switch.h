#ifndef __SWITCH_H__
#define __SWITCH_H__ 1

/* This value musn't be misinterpreted as shared string. When string
 * handling is changed, change this value appropriately.
 * This value should also not be mixed up with the null pointer, which is
 * used for a string that is not in the shared string table
 * (and thus won't be found) .
 */
#define ZERO_AS_STR_CASE_LABEL ((char *)&findstring)

#define CASE_BLOCKING_FACTOR 256 /* must be >= 3 */

struct case_list_entry {
    p_int key, addr, line;
    struct case_list_entry *next;
};

struct s_case_state {
    struct case_list_entry *free_block, *next_free, *list0, *list1, *zero;
    struct s_case_state *previous;
    p_int default_addr;
    char some_numeric_labels, no_string_labels;
};

/* --- Variables --- */
extern struct s_case_state case_state;       /* closure.c */
extern struct case_list_entry *case_blocks;

/* --- Prototypes --- */
extern struct case_list_entry *new_case_entry PROT((void)); /* closure.c */
extern void store_case_labels PROT((
    p_int total_length,
    p_int default_addr,
    int numeric,
    struct case_list_entry *zero,
    char *(*get_space)(p_int),
    void (*move_instructions)(int, p_int),
    void (*cerror)(char *),
    void (*cerrorl)(char *, char*, int, int)
));

#endif  /* __SWITCH_H__ */
