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

#define PYTHON_TYPE_TABLE_SIZE (2048UL)
  /* The number of entries in the python type table.
   * This number is restricted by the ident_s and svalue_s
   * definition, which provide a unsigned short for the type
   * index.
   */

/* --- Structs --- */
typedef struct python_type_operation_s python_type_operation_t;

struct python_type_operation_s
{
    lpctype_t*  returntype;     /* NULL if the operation doesn't exist. */
    lpctype_t*  argtype;        /* Type of the second argument.         */
};

/* --- Enums --- */
enum python_operation
{
    PYTHON_OP_ADD,      // Addition, Python ob on the left
    PYTHON_OP_RADD,     // Addition, Python ob on the right
    PYTHON_OP_IADD,     // Addition and assignment
    PYTHON_OP_SUB,      // Subtraction, Python ob on the left
    PYTHON_OP_RSUB,     // Subtraction, Python ob on the right
    PYTHON_OP_ISUB,     // Subtraction and assignment
    PYTHON_OP_MUL,      // Multiplication, Python ob on the left
    PYTHON_OP_RMUL,     // Multiplication, Python ob on the right
    PYTHON_OP_IMUL,     // Multiplication and assignment
    PYTHON_OP_DIV,      // Division, Python ob on the left
    PYTHON_OP_RDIV,     // Division, Python ob on the right
    PYTHON_OP_IDIV,     // Division and assignment
    PYTHON_OP_MOD,      // Modulo, Python ob on the left
    PYTHON_OP_RMOD,     // Modulo, Python ob on the right
    PYTHON_OP_IMOD,     // Modulo and assignment
    PYTHON_OP_LSH,      // Left shift, Python ob on the left
    PYTHON_OP_RLSH,     // Left shift, Python ob on the right
    PYTHON_OP_ILSH,     // Left shift and assignment
    PYTHON_OP_RSH,      // Right shift, Python ob on the left
    PYTHON_OP_RRSH,     // Right shift, Python ob on the right
    PYTHON_OP_IRSH,     // Right shift and assignment
    PYTHON_OP_AND,      // Binary And, Python ob on the left
    PYTHON_OP_RAND,     // Binary And, Python ob on the right
    PYTHON_OP_IAND,     // Binary And and assignment
    PYTHON_OP_OR,       // Binary Or, Python ob on the left
    PYTHON_OP_ROR,      // Binary Or, Python ob on the right
    PYTHON_OP_IOR,      // Binary Or and assignment
    PYTHON_OP_XOR,      // Binary Xor, Python ob on the left
    PYTHON_OP_RXOR,     // Binary Xor, Python ob on the right
    PYTHON_OP_IXOR,     // Binary Xor and assignment
    PYTHON_OP_LT,       // Less than, Python ob on the left
    PYTHON_OP_RLT,      // Less than, Python ob on the right
    PYTHON_OP_LE,       // Less or equal, Python ob on the left
    PYTHON_OP_RLE,      // Less or equal, Python ob on the right
    PYTHON_OP_EQ,       // Equal, Python ob on the left
    PYTHON_OP_REQ,      // Equal, Python ob on the right
    PYTHON_OP_NE,       // Not equal, Python ob on the left
    PYTHON_OP_RNE,      // Not equal, Python ob on the right
    PYTHON_OP_GT,       // Greater than, Python ob on the left
    PYTHON_OP_RGT,      // Greater than, Python ob on the right
    PYTHON_OP_GE,       // Greater or equal, Python ob on the left
    PYTHON_OP_RGE,      // Greater or equal, Python ob on the right
    PYTHON_OP_NEG,      // Arithmetic negation
    PYTHON_OP_INVERT,   // Bitwise negation

    PYTHON_OPERATIONS_COUNT,
    PYTHON_OP_NONE
};

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

extern ident_t* get_python_type_name(int python_type_id);
extern lpctype_t* lookup_python_type(int python_type_id);
extern void enter_python_type(int python_type_id, lpctype_t* lpctype);
extern python_type_operation_t get_python_operation(int python_type_id, enum python_operation op);
extern lpctype_t* get_first_python_type(lpctype_t* type, void** cursor);
extern lpctype_t* get_next_python_type(void** cursor);
extern bool is_valid_arg_for_python_type_efun(lpctype_t *type, int efun, int pos, lpctype_t *argtype);
extern lpctype_t* add_result_for_python_type_efun(lpctype_t *type, int efun, lpctype_t *result);

extern bool python_ob_has_last_ref(svalue_t *pval);
extern void ref_python_ob(svalue_t *pval);
extern void free_python_ob(svalue_t *pval);
extern void copy_python_ob(svalue_t *dest, svalue_t *src);
extern bool save_python_ob(svalue_t *dest, string_t **name, svalue_t *ob);
extern bool restore_python_ob(svalue_t *dest, string_t *name, svalue_t *value);
extern string_t* python_ob_to_string(svalue_t *pval);
extern svalue_t* do_python_unary_operation(svalue_t *sp, enum python_operation op, const char* op_name);
extern svalue_t* do_python_binary_operation(svalue_t *sp, enum python_operation op, enum python_operation rop, const char* op_name);
extern svalue_t* do_python_assignment_operation(svalue_t *sp, svalue_t *dest, enum python_operation iop, enum python_operation op, enum python_operation rop, const char* op_name);
extern svalue_t* call_python_type_efun(svalue_t *sp, int efun, int num_arg);

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

extern void cleanup_python_data(cleanup_t * context);

#ifdef GC_SUPPORT
extern void python_clear_refs();
extern void python_count_refs();
#endif /* GC_SUPPORT */

#ifdef DEBUG
extern void count_python_extra_refs();
#endif /* DEBUG */

#endif /* USE_PYTHON */

#endif /* PKG_PYTHON_H__ */
