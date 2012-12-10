#ifndef PROLANG_H__
#define PROLANG_H__ 1

#include "driver.h"
#include "typedefs.h"

#include "exec.h"  /* bytecode_p, lpctype_t via types.h */

/* --- Variables --- */

extern lpctype_t _lpctype_unknown_array, _lpctype_any_array,    _lpctype_int_float,
                 _lpctype_int_array,     _lpctype_string_array, _lpctype_object_array;
extern lpctype_t *lpctype_unknown_array, *lpctype_any_array,    *lpctype_int_float,
                 *lpctype_int_array,     *lpctype_string_array, *lpctype_object_array;

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
extern Bool is_undef_function (bytecode_p fun);
extern short find_inherited_function (const char * super_name, const char * real_name , unsigned short * pInherit, funflag_t *flags);
extern const char *get_current_function_name();
extern char *get_lpctype_name (lpctype_t *type);
extern size_t get_lpctype_name_buf (lpctype_t *type, char *buf, size_t bufsize);


#if defined( DEBUG ) && defined ( TRACE_CODE )
extern void set_code_window(void);
extern void show_code_window(void);
#endif

#ifdef GC_SUPPORT
extern void clear_compiler_refs(void);
extern void count_compiler_refs(void);
#endif

#endif /* PROLANG_H__ */
