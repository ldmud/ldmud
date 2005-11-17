#ifndef EFUNS_H__
#define EFUNS_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "instrs.h"     /* F_TRANSFER */

/* --- Variables --- */

#ifdef USE_SET_IS_WIZARD
extern Bool is_wizard_used;
#endif

/* --- Prototypes --- */

extern svalue_t *f_copy_bits (svalue_t *sp, int num_arg);
extern svalue_t *f_make_shared_string(svalue_t *);
extern svalue_t *f_md5(svalue_t *);
extern svalue_t *f_trim(svalue_t *, int num_arg);
extern svalue_t *f_upper_case(svalue_t *);
extern char     *e_terminal_colour (char * text, mapping_t * map
                                   , svalue_t *cl
                                   , int indent, int wrap);
#ifdef USE_PROCESS_STRING
extern svalue_t *f_process_string(svalue_t *sp);
#endif /* USE_PROCESS_STRING */

extern int       e_sscanf(int num_arg, svalue_t *sp);

extern svalue_t *f_copy (svalue_t *sp);
extern svalue_t *f_deep_copy (svalue_t *sp);
extern svalue_t *f_filter (svalue_t *sp, int num_arg);
extern svalue_t *f_map (svalue_t *sp, int num_arg);
extern svalue_t *f_min (svalue_t *sp, int num_arg);
extern svalue_t *f_max (svalue_t *sp, int num_arg);
extern svalue_t *f_sgn (svalue_t *sp);

extern svalue_t *f_gmtime (svalue_t *sp);
extern svalue_t *f_localtime (svalue_t *sp);

extern svalue_t *x_all_environment(svalue_t *, int);
extern svalue_t *f_blueprint (svalue_t *sp);
extern svalue_t *f_clones (svalue_t *sp, int num_args);
extern svalue_t *f_object_info (svalue_t *sp, int num_args);
extern object_t *e_object_present(svalue_t *v, object_t *ob, Bool hasNumber, p_int num);
extern svalue_t *f_present_clone (svalue_t *sp);
extern svalue_t *f_to_object(svalue_t *sp);

#ifdef USE_SET_IS_WIZARD
extern svalue_t *f_set_is_wizard(svalue_t *sp);
#endif /* USE_SET_IS_WIZARD */

extern svalue_t *f_set_modify_command(svalue_t *sp);

#ifdef F_TRANSFER
extern svalue_t *f_transfer(svalue_t *svp);
#endif /* F_TRANSFER */
extern void e_say(svalue_t *v, vector_t *avoid);
extern void e_tell_room(object_t *room, svalue_t *v, vector_t *avoid);
extern svalue_t *tell_room(svalue_t *sp);

extern svalue_t *f_debug_info(svalue_t *sp, int num_arg);
extern svalue_t *f_shutdown(svalue_t *sp);

#endif /* EFUNS_H__ */

