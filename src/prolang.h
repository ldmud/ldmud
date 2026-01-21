#ifndef PROLANG_H__
#define PROLANG_H__ 1

#include "driver.h"
#include "typedefs.h"

#include "exec.h"  /* bytecode_p, lpctype_t via types.h */

/* --- Types --- */

typedef struct lvalue_block_s      lvalue_block_t;
typedef struct code_context_s      code_context_t;
typedef struct code_location_s     code_location_t;

/* --- struct lvalue_block_s: Store code for an lvalue expression ---
 *
 * This structure is used lvalue code has to be generated
 * to save its whereabouts in the A_LVALUE_CODE block.
 */

struct lvalue_block_s
{
    p_int start;
    p_int size;
};

/* --- struct statement_s: Information about a statement block ---
 *
 * Here we keep track of some control flow information, that we'll
 * use for proper diagnostics.
 */

struct statement_s
{
    bool may_return         : 1;  /* The statement may issue a return.   */
    bool may_break          : 1;  /* The statement may issue a break.    */
    bool may_continue       : 1;  /* The statement may issue a continue. */
    bool may_finish         : 1;  /* The statement may finish without
                                   * a break or return.
                                   */
    bool is_empty           : 1;  /* There is no real statement.         */
    bool warned_dead_code   : 1;  /* We already warned about dead code.  */
};

/* --- struct code_context_s: Context for string compilations ---
 *
 * Contains parameters and callbacks for string compilation.
 */
struct code_context_s
{
    int num_args;               /* The number of arguments.                */
    svalue_t *arg_names;        /* The name of the arguments as symbols.   */
    program_t *prog;            /* Program to use for lookups.             */
    svalue_t *var_lookup;       /* Mapping or closure for variable lookup. */
    svalue_t *fun_lookup;       /* Mapping or closure for function lookup. */
    svalue_t *struct_lookup;    /* Mapping or closure for struct lookup.   */

    bool use_prog_for_variables : 1;  /* Search <prog> for variables.      */
    bool use_prog_for_functions : 1;  /* Search <prog> for functions.      */
    bool use_prog_for_structs   : 1;  /* Search <prog> for struct defs.    */

    bool make_async             : 1;  /* Create a coroutine.               */
    bool detect_end             : 1;  /* End parsing on wrong tokens.      */

    char error_msg[5120];       /* Returning error message (if any).       */
    int end_position;           /* The detected end.                       */
};

/* --- struct code_location_s: Location information of source code ---
 *
 * This is the (byte) position of a token within the given string.
 * If the token comes from a file or an auto-include string, the
 * corresponding position will be -1.
 */
struct code_location_s
{
    int start;  /* Position of the first byte of the token. */
    int end;    /* Position of the byte after the token.    */
};

/* --- Variables --- */

extern lpctype_t _lpctype_unknown_array, _lpctype_any_array,    _lpctype_int_float,
                 _lpctype_int_array,     _lpctype_string_array, _lpctype_object_array,
                 _lpctype_bytes_array,   _lpctype_string_bytes, _lpctype_string_or_bytes_array,
                 _lpctype_string_object, _lpctype_string_object_lwobject,
                 _lpctype_string_object_lwobject_array,
                 _lpctype_lwobject_array,_lpctype_any_object_or_lwobject,
                 _lpctype_any_object_or_lwobject_array,
                 _lpctype_any_object_or_lwobject_array_array,
                 _lpctype_int_or_string, _lpctype_string_or_string_array,
                 _lpctype_symbol_array, _lpctype_catch_msg_arg, _lpctype_mapping_or_closure;
extern lpctype_t *lpctype_unknown_array, *lpctype_any_array,    *lpctype_int_float,
                 *lpctype_int_array,     *lpctype_string_array, *lpctype_object_array,
                 *lpctype_bytes_array,   *lpctype_string_bytes, *lpctype_string_or_bytes_array,
                 *lpctype_string_object, *lpctype_string_object_lwobject,
                 *lpctype_string_object_lwobject_array,
                 *lpctype_lwobject_array,*lpctype_any_object_or_lwobject,
                 *lpctype_any_object_or_lwobject_array,
                 *lpctype_any_object_or_lwobject_array_array,
                 *lpctype_int_or_string, *lpctype_string_or_string_array,
                 *lpctype_symbol_array, *lpctype_catch_msg_arg, *lpctype_mapping_or_closure;

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
extern lambda_t *compile_expr(string_t *expr, code_context_t *context);
extern lambda_t *compile_block(string_t *block, code_context_t *context);
extern bool is_undef_function (function_t *header, bytecode_p funstart);
extern unsigned short find_inherited_function (const char * super_name, const char * real_name , unsigned short * pInherit, funflag_t *flags);
extern const char *get_current_function_name();
extern char *get_lpctype_name (lpctype_t *type);
extern size_t get_lpctype_name_buf (lpctype_t *type, char *buf, size_t bufsize);
extern char *get_fulltype_name (fulltype_t type);
extern void init_compiler();
extern bool lookup_function(ident_t *ident, char* super, efun_override_t override);
extern int get_function_index(ident_t *ident);
extern int get_function_closure(ident_t *ident);
extern bool lookup_global_variable(ident_t *ident);
extern int get_global_variable_index(ident_t *ident, bool no_virtual);
extern int get_global_variable_lvalue(ident_t *ident);

#if defined( DEBUG ) && defined ( TRACE_CODE )
extern void set_code_window(void);
extern void show_code_window(void);
#endif

#ifdef GC_SUPPORT
extern void clear_compiler_refs(void);
extern void count_compiler_refs(void);
#endif

#endif /* PROLANG_H__ */
