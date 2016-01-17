#ifndef PKG_PYTHON_H__
#define PKG_PYTHON_H__ 1

#include "driver.h"

#ifdef USE_PYTHON

#ifndef HAS_PYTHON3
#error "pkg-python configured even though the machine doesn't support Python3."
#endif

#include "typedefs.h"

/* --- Defines --- */
#define PYTHON_EFUN_TABLE_SIZE (2048UL)
  /* The number of entries in the python efun table.
   * This number is restricted by the ident_s definition,
   * which provides a short for the efun index, and by the
   * .x.closure_type entry in svalue_s, which gives us 11 bits.
   */

/* --- Variables --- */
extern char * python_startup_script;
  /* Filename to call at LDMud startup. */

extern int num_python_efun;
  /* Next available ID for python efuns. */

/* --- Prototypes --- */
extern void pkg_python_init (char* prog_name);
extern bool is_python_efun (ident_t *p);
extern void call_python_efun (int idx, int num_arg);
extern const char* closure_python_efun_to_string (int type);

#ifdef GC_SUPPORT
extern void python_clear_refs();
extern void python_count_refs();
#endif /* GC_SUPPORT */

#endif /* USE_PYTHON */

#endif /* PKG_PYTHON_H__ */
