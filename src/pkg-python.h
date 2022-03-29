#ifndef PKG_PYTHON_H__
#define PKG_PYTHON_H__ 1

#include "driver.h"

#ifdef USE_PYTHON

#ifndef HAS_PYTHON3
#error "pkg-python configured even though the machine doesn't support Python3."
#endif

#include <unistd.h>
#include "typedefs.h"

/* --- Defines --- */
#define PYTHON_EFUN_TABLE_SIZE (2048UL)
  /* The number of entries in the python efun table.
   * This number is restricted by the ident_s definition,
   * which provides a short for the efun index, and by the
   * .x.closure_type entry in svalue_s, which gives us 11 bits.
   */

/* --- Enums --- */
enum python_hooks
{
    PYTHON_HOOK_ON_HEARTBEAT,
    PYTHON_HOOK_ON_OBJECT_CREATED,
    PYTHON_HOOK_ON_OBJECT_DESTRUCTED,
    PYTHON_HOOK_ON_SIGCHLD,

    PYTHON_HOOK_COUNT,
};

/* --- Variables --- */
extern char * python_startup_script;
  /* Filename to call at LDMud startup. */

extern int num_python_efun;
  /* Next available ID for python efuns. */

extern ident_t *all_python_efuns;
  /* Start of the linked list of all non-shadowing python efuns.
   * (All shadowed efuns are in the all_efuns list.)
   */

/* --- Prototypes --- */
extern void pkg_python_init(char* prog_name);

extern bool is_python_efun(ident_t *p);
extern lpctype_t* check_python_efun_args(ident_t *p, int num_arg, bool has_ellipsis, fulltype_t *args);
extern void call_python_efun(int idx, int num_arg);
extern const char* closure_python_efun_to_string(int type);

extern void python_set_fds(fd_set *readfds, fd_set *writefds, fd_set *exceptfds, int *nfds);
extern void python_handle_fds(fd_set *readfds, fd_set *writefds, fd_set *exceptfds, int nfds);

extern void python_call_hook(int hook, bool is_external);
extern void python_call_hook_object(int hook, bool is_external, object_t *ob);

extern void python_interrupt();
extern void python_handle_sigchld();
extern void python_process_pending_jobs();

extern void python_free_object(object_t *ob);
extern void python_free_replace_program_protector(replace_ob_t *r_ob);
extern void python_replace_program_adjust(replace_ob_t *r_ob);

#ifdef GC_SUPPORT
extern void python_clear_refs();
extern void python_count_refs();
#endif /* GC_SUPPORT */

#ifdef DEBUG
extern void count_python_extra_refs();
#endif /* DEBUG */

#endif /* USE_PYTHON */

#endif /* PKG_PYTHON_H__ */
