#ifndef EFUNS_H__
#define EFUNS_H__ 1

#include "driver.h"
#include "typedefs.h"
#include "pkg-openssl.h"
#include "pkg-gcrypt.h"

/* --- Variables --- */

#ifdef USE_SET_IS_WIZARD
extern Bool is_wizard_used;
#endif

/* --- Prototypes --- */

extern svalue_t *f_capitalize(svalue_t *);
extern svalue_t *f_crypt(svalue_t *);
extern svalue_t *f_explode(svalue_t *);
extern svalue_t *f_implode(svalue_t *);
extern svalue_t *f_lower_case(svalue_t *);
extern svalue_t *f_make_shared_string(svalue_t *);
extern svalue_t *v_hash(svalue_t *sp, int num_arg);
extern svalue_t *f_hmac(svalue_t *sp);
extern svalue_t *v_md5(svalue_t *, int num_arg);
extern svalue_t *f_md5_crypt(svalue_t *sp);
extern svalue_t *v_sha(svalue_t *, int num_arg);
extern svalue_t *f_regexp(svalue_t *);
extern svalue_t *f_regexplode(svalue_t *sp);
extern svalue_t* f_regreplace (svalue_t *sp);
extern svalue_t* v_regmatch (svalue_t *sp, int num_arg);
extern svalue_t *f_strstr(svalue_t *);
extern svalue_t *f_strrstr(svalue_t *);
extern svalue_t *v_trim(svalue_t *, int num_arg);
extern svalue_t *f_upper_case(svalue_t *);
extern svalue_t *v_terminal_colour(svalue_t *, int num_arg);
#ifdef USE_PROCESS_STRING
extern svalue_t *f_process_string(svalue_t *sp);  /* optional */
#endif
extern int       e_sscanf(int num_arg, svalue_t *sp);

extern svalue_t *f_abs (svalue_t *sp);
extern svalue_t *f_sin (svalue_t *sp);
extern svalue_t *f_asin (svalue_t *sp);
extern svalue_t *f_cos (svalue_t *sp);
extern svalue_t *f_acos (svalue_t *sp);
extern svalue_t *f_tan (svalue_t *sp);
extern svalue_t *f_atan (svalue_t *sp);
extern svalue_t *f_atan2 (svalue_t *sp);
extern svalue_t *f_log (svalue_t *sp);
extern svalue_t *f_exp (svalue_t *sp);
extern svalue_t *f_sqrt (svalue_t *sp);
extern svalue_t *f_ceil (svalue_t *sp);
extern svalue_t *f_floor (svalue_t *sp);
extern svalue_t *f_pow (svalue_t *sp);
extern svalue_t *f_to_array (svalue_t *sp);
#ifdef USE_STRUCTS
extern svalue_t * v_to_struct (svalue_t *sp, int num_args);
#endif
extern svalue_t *f_to_int (svalue_t *sp);
extern svalue_t *f_to_float (svalue_t *sp);
extern svalue_t *f_to_string (svalue_t *sp);
extern svalue_t *f_to_object (svalue_t *sp);
extern svalue_t *f_copy (svalue_t *sp);
extern svalue_t *f_deep_copy (svalue_t *sp);
extern svalue_t *v_filter (svalue_t *sp, int num_arg);
extern svalue_t *v_get_type_info (svalue_t *sp, int num_arg);
extern svalue_t *v_map (svalue_t *sp, int num_arg);
extern svalue_t *v_member (svalue_t *sp, int num_arg);
extern svalue_t *v_rmember (svalue_t *sp, int num_arg);
extern svalue_t *v_min (svalue_t *sp, int num_arg);
extern svalue_t *v_max (svalue_t *sp, int num_arg);
extern svalue_t *f_reverse(svalue_t *sp);
extern svalue_t *f_sgn (svalue_t *sp);
extern svalue_t *f_quote (svalue_t *sp);
extern svalue_t *f_unquote (svalue_t *sp);
extern svalue_t *f_gmtime (svalue_t *sp);
extern svalue_t *f_localtime (svalue_t *sp);

extern svalue_t *f_blueprint (svalue_t *sp);
extern svalue_t *v_clones (svalue_t *sp, int num_args);
extern svalue_t *v_object_info (svalue_t *sp, int num_args);
extern svalue_t *v_present_clone (svalue_t *sp, int num_arg);
extern svalue_t *f_to_object(svalue_t *sp);
extern svalue_t *f_set_is_wizard(svalue_t *sp);  /* optional */
extern svalue_t *tell_room(svalue_t *sp);

extern svalue_t *f_ctime(svalue_t *);
extern svalue_t *v_strftime(svalue_t *, int num_arg);
extern svalue_t *v_debug_info(svalue_t *sp, int num_arg);
extern svalue_t *f_configure_driver(svalue_t *);
extern svalue_t *f_rusage(svalue_t *sp);
extern svalue_t *f_random(svalue_t *);
extern svalue_t *f_shutdown(svalue_t *sp);
extern svalue_t *f_time(svalue_t *);
extern svalue_t *f_mktime(svalue_t *);
extern svalue_t *f_utime(svalue_t *);
#ifdef GC_SUPPORT
extern void clear_ref_from_efuns(void);
extern void count_ref_from_efuns(void);
#endif /* GC_SUPPORT */

/* --- Prototypes for TLS packages --- */

#if (!defined(USE_TLS) || !defined(HAS_OPENSSL)) && !defined(USE_GCRYPT)
typedef int digest_t;
#endif

extern Bool get_digest(int algo, digest_t *md, size_t *len);
extern void calc_digest(digest_t md, void *dest, size_t destlen, void *msg, size_t msglen, void *key, size_t keylen);

#endif /* EFUNS_H__ */
