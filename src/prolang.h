#ifndef __PROLANG_H__
#define __PROLANG_H__ 1

#include "driver.h"

#include "interpret.h"

/* Some functions are inlined in lang.c, and called normally
 * from everywhere else.
 */
#if defined(HAS_INLINE) && defined(LANG)
#define LANG_INLINE LOCAL_INLINE
#else
#define LANG_INLINE extern
#endif

#ifndef LANG

/* Needed in lang.h, but lang.c itself must not include
 * lang.h.
 */
struct s_lrvalue { struct s_lrvalue *fake_member; };
#include "lang.h"

#endif

/* --- Variables --- */
extern int yychar;
extern int32 current_id_number;
extern int approved_object;
extern int num_virtual_variables;
extern short hook_type_map[];

extern struct program *compiled_prog;
#ifndef INITIALIZATION_BY___INIT
extern struct svalue *prog_variable_values;
#endif

/* These are in efun_defs.c, created by make_func.y */
extern struct instr instrs[];
extern short efun_aliases[];
extern int efun_arg_types[];
extern struct svalue *(*efun_table[]) PROT((struct svalue *));
extern struct svalue *(*vefun_table[]) PROT((struct svalue *, int));

/* --- Prototypes --- */
extern void yyerrorf VARPROT((char *format, ...), printf, 1, 2);
LANG_INLINE int proxy_efun PROT((int, int));
extern void yyerror PROT((char *str));
extern void free_all_local_names PROT((void));
extern void store_line_number_info PROT((void));
extern void store_include_info PROT((char *name));
extern void store_include_end PROT((void));
extern void compile_file PROT((void));

#if defined( DEBUG ) && defined ( TRACE_CODE )
extern void set_code_window PROT((void));
extern void show_code_window PROT((void));
#endif

#ifdef MALLOC_smalloc
extern void count_compiler_refs PROT((void));
#endif

#endif /* __PROLANG_H__ */
