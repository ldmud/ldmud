#ifndef PROLANG_H__
#define PROLANG_H__ 1

#include "driver.h"
#include "typedefs.h"

#include "exec.h"  /* fulltype_t, vartype_t, fun_hdr_p */

/* --- Variables --- */
extern int yychar;
extern int32 current_id_number;
extern int approved_object;
extern int num_virtual_variables;
extern short hook_type_map[];
extern string_t *inherit_file;
extern int num_parse_error;
extern program_t *compiled_prog;
extern Bool variables_defined;

/* --- Prototypes --- */
extern int proxy_efun(int, int);
extern void yyerrorf VARPROT((const char *format, ...), printf, 1, 2);
extern void yyerror(const char *str);
extern void yywarnf VARPROT((const char *format, ...), printf, 1, 2);
extern void yywarn(const char *str);
extern void free_all_local_names(void);
extern void store_line_number_info(void);
extern void store_line_number_backward(int offset);
extern mp_uint store_include_info(char *name, char *file, char delim, int inc_depth);
extern void store_include_end(mp_uint inc_offset, int include_line);
extern void compile_file(int fd, const char * fname, Bool isMasterObj);
extern Bool is_undef_function (fun_hdr_p fun);
extern short find_inherited_function (const char * super_name, const char * real_name , unsigned short * pInherit, funflag_t *flags);
extern char *get_type_name (fulltype_t type);

#if defined( DEBUG ) && defined ( TRACE_CODE )
extern void set_code_window(void);
extern void show_code_window(void);
#endif

#ifdef GC_SUPPORT
extern void count_compiler_refs(void);
#endif

#endif /* PROLANG_H__ */
