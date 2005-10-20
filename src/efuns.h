#ifndef __EFUNS_H__
#define __EFUNS_H__ 1

#include "driver.h"
#include "datatypes.h"

/* --- Prototypes --- */

extern struct svalue *f_make_shared_string(struct svalue *);
extern struct svalue *f_trim(struct svalue *, int num_arg);
extern struct svalue *f_upper_case(struct svalue *);
extern char          *e_terminal_colour (char * text, struct mapping * map
                                        , int indent, int wrap);
extern int            e_sscanf(int num_arg, struct svalue *sp);

extern struct svalue *f_copy (struct svalue *sp);
extern struct svalue *f_deep_copy (struct svalue *sp);

extern struct svalue *x_all_environment(struct svalue *, int);
extern struct svalue *f_object_info (struct svalue *sp);
extern struct svalue *f_present_clone (struct svalue *sp);
extern struct svalue *f_to_object(struct svalue *sp);
extern struct svalue *f_set_is_wizard(struct svalue *sp);
extern struct svalue *f_set_modify_command(struct svalue *sp);
extern struct svalue *f_set_prompt(struct svalue *sp);

extern struct svalue *f_debug_info(struct svalue *sp, int num_arg);

#endif /* __EFUNS_H__ */

