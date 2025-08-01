/*------------------------------------------------------------------
 * Python support.
 *------------------------------------------------------------------
 * This file contains the glue for the interaction between
 * python and the LPC runtime.
 *------------------------------------------------------------------
 * For a context switch from LPC to Python the following needs to
 * happen:
 *  - python_start_thread(), acquires the Python GIL. This needs
 *    to happen for every access to a Python object.
 *  - python_save_context(): Save the current object and command
 *    giver. (python_clear_context() for external calls).
 *  - set python_is_external accordingly.
 *
 * When calling LPC from Python:
 *  - python_restore_context(): Restore current object and command
 *    giver.
 *
 * When returning to LPC from Python:
 *  - If python_is_external was changed, restore the previous value.
 *  - python_finish_thread()
 *------------------------------------------------------------------
 */
#include "driver.h"
#include "machine.h"

#if defined(USE_PYTHON) && defined(HAS_PYTHON3)


#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <structmember.h>

/* structmember.h has defines for T_FLOAT, T_STRING as do we.
 * We need to rename and undef them...
 */
enum python_structmember_type
{
    PYTHON_SMT_SHORT          = T_SHORT,
    PYTHON_SMT_INT            = T_INT,
    PYTHON_SMT_LONG           = T_LONG,
    PYTHON_SMT_FLOAT          = T_FLOAT,
    PYTHON_SMT_DOUBLE         = T_DOUBLE,
    PYTHON_SMT_STRING         = T_STRING,
    PYTHON_SMT_OBJECT         = T_OBJECT,
    PYTHON_SMT_CHAR           = T_CHAR,
    PYTHON_SMT_BYTE           = T_BYTE,
    PYTHON_SMT_UBYTE          = T_UBYTE,
    PYTHON_SMT_USHORT         = T_USHORT,
    PYTHON_SMT_UINT           = T_UINT,
    PYTHON_SMT_ULONG          = T_ULONG,
    PYTHON_SMT_STRING_INPLACE = T_STRING_INPLACE,
    PYTHON_SMT_BOOL           = T_BOOL,
    PYTHON_SMT_OBJECT_EX      = T_OBJECT_EX,
    PYTHON_SMT_LONGLONG       = T_LONGLONG,
    PYTHON_SMT_ULONGLONG      = T_ULONGLONG,
    PYTHON_SMT_PYSSIZET       = T_PYSSIZET,
    PYTHON_SMT_NONE           = T_NONE
};

#undef T_SHORT
#undef T_INT
#undef T_LONG
#undef T_FLOAT
#undef T_DOUBLE
#undef T_STRING
#undef T_OBJECT
#undef T_CHAR
#undef T_BYTE
#undef T_UBYTE
#undef T_USHORT
#undef T_UINT
#undef T_ULONG
#undef T_STRING_INPLACE
#undef T_BOOL
#undef T_OBJECT_EX
#undef T_LONGLONG
#undef T_ULONGLONG
#undef T_PYSSIZET
#undef T_NONE

/* Python.h defined these also again... */
#undef _GNU_SOURCE
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE

#include <poll.h>

#include "actions.h"
#include "array.h"
#include "closure.h"
#include "coroutine.h"
#include "exec.h"
#include "gcollect.h"
#include "instrs.h"
#include "interpret.h"
#include "lex.h"
#include "lwobject.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "pkg-python.h"
#include "prolang.h"
#include "simul_efun.h"
#include "simulate.h"
#include "stdstrings.h"
#include "structs.h"
#include "swap.h"
#include "typedefs.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "i-current_object.h"

#if PY_VERSION_HEX >= 0x03070000
#define USE_PYTHON_CONTEXT
#endif

#define REAL_EFUN_COUNT (EFUN_COUNT + EFUN1_COUNT + EFUN2_COUNT + EFUN3_COUNT + EFUN4_COUNT + EFUNV_COUNT)

/* --- Type declarations --- */
typedef struct ldmud_gc_var_s ldmud_gc_var_t;
typedef struct ldmud_gc_lpctype_type_s ldmud_gc_lpctype_type_t;
typedef struct ldmud_concrete_struct_type_s ldmud_concrete_struct_type_t;
typedef void (*CClosureFun)(int,void*);
typedef struct python_efun_info_s python_efun_info_t;
typedef struct python_efun_s python_efun_t;
typedef struct python_type_entry_s python_type_entry_t;
typedef struct python_poll_fds_s python_poll_fds_t;
typedef struct python_hook_s python_hook_t;

/* --- Enum definitions --- */
enum lfun_flags
{
    LFUN_FLAG_STATIC     = 1 << 0,
    LFUN_FLAG_NOMASK     = 1 << 1,
    LFUN_FLAG_VARARGS    = 1 << 2,
    LFUN_FLAG_VIRTUAL    = 1 << 3,
    LFUN_FLAG_DEPRECATED = 1 << 4,

    LFUN_FLAG_COUNT      =      5
};

enum lfun_argument_flags
{
    LFUN_ARGUMENT_FLAG_VARARGS = 1 << 0,

    LFUN_ARGUMENT_FLAG_COUNT   =      1
};

enum variable_flags
{
    VARIABLE_FLAG_NOSAVE     = 1 << 0,
    VARIABLE_FLAG_NOMASK     = 1 << 1,
    VARIABLE_FLAG_VIRTUAL    = 1 << 2,
    VARIABLE_FLAG_DEPRECATED = 1 << 3,

    VARIABLE_FLAG_COUNT      =      4
};

enum visibility
{
    VISIBILITY_PRIVATE,
    VISIBILITY_PROTECTED,
    VISIBILITY_VISIBLE,
    VISIBILITY_PUBLIC,

    VISIBILITY_COUNT
};

enum call_frame_type
{
    CALL_FRAME_TYPE_LFUN,
    CALL_FRAME_TYPE_ALIEN_LFUN_CLOSURE,
    CALL_FRAME_TYPE_EFUN_CLOSURE,
    CALL_FRAME_TYPE_PYTHON_EFUN_CLOSURE,
    CALL_FRAME_TYPE_SIMUL_EFUN_CLOSURE,
    CALL_FRAME_TYPE_CATCH,
    CALL_FRAME_TYPE_LAMBDA,

    CALL_FRAME_TYPE_COUNT
};

/* --- Type definitions --- */
struct python_efun_info_s
{
    lpctype_t** types;          /* The return type and argument types:
                                 *   [0]:           return type
                                 *   [1 .. maxarg]: argument types
                                 *   [maxarg + 1]:  vararg type.
                                 */
    int         minarg;         /* Minimum number of arguments.        */
    int         maxarg;         /* Maximum number of arguments.        */
    bool        varargs :1;     /* Whether we have a variable number.  */
    bool        exists  :1;     /* Whether the fun actually exists
                                 * (only used in python_type_entry_s).
                                 */
};

struct python_efun_s
{
    PyObject*          callable;/* Python callable of the efun.        */
    ident_t*           name;    /* The identifier of the efun.         */
    python_efun_info_t info;    /* Additional type information.        */
};

struct python_type_entry_s
{
    PyObject*   pytype;         /* Python type object for this type.    */
    lpctype_t*  lpctype;        /* Corresponding LPC type object (ref). */
    ident_t*    name;           /* The identifier of the type.          */

    python_type_operation_t op[PYTHON_OPERATIONS_COUNT];
                                /* Operations for this type.            */
    python_efun_info_t efun[REAL_EFUN_COUNT];
                                /* Available efun overrides.            */
};

struct python_poll_fds_s
{
    int fd;                   /* file descriptor */
    short events;             /* combination of POLLIN, POLLOUT and POLLPRI
                               * (only valid when eventsfun == NULL)
                               */
    PyObject* eventsfun;      /* refcounted python callable to determine <events>. */
    PyObject* fun;            /* refcounted python callable */

    python_poll_fds_t *next;  /* singly linked list */
};

/* Just a linked list of python callables. */
struct python_hook_s
{
    PyObject* fun;
    python_hook_t *next;
};

/* A linked list of lfun and variable references. */
struct python_replace_program_protector
{
    PyObject* ref;
    struct python_replace_program_protector* next;
};

/* Arguments for ldmud_object_init_getobject(). */
struct ldmud_object_init_closure_s
{
    string_t* filename;
    object_t* ob;
};

/* --- Variables --- */
char * python_startup_script = NULL;

static volatile bool python_pending_sigchld = false;
static volatile bool python_pending_sigint = false;
static volatile bool python_pending_sigterm = false;
static volatile bool python_pending_sighup = false;
static volatile bool python_pending_sigusr1 = false;
static volatile bool python_pending_sigusr2 = false;
 /* We received a signal and need to pass it to Python.
  */

static bool python_is_external = true;
 /* Remember how python code was called,
  * false, when called from a running LPC program,
  * true otherwise (upon external events)
  */

ident_t *all_python_idents = NULL;

int num_python_efun = 0;

static python_efun_t python_efun_table[PYTHON_EFUN_TABLE_SIZE];
  /* Information about all defined efuns.
   */

int num_python_structs = 0;

static struct_type_t* python_struct_table[PYTHON_STRUCT_TABLE_SIZE];
  /* All Python-registered structs.
   */

int num_python_type = 0;

ident_t *all_python_types = NULL;

long num_lpc_python_references = 0;

long num_python_lpc_references = 0;

static long frame_counter = 0;
static long frame_current_index[MAX_TRACE];
  /* Whenever we create a call_frame object, we remember a unique number
   * (created by incrementing <frame_counter>) for the given stack level
   * in <frame_current_index> and in the call_frame object. When we leave
   * the frame this entry will marked invalid (by setting to 0). And thus
   * the call_frame object can detect, whether it represents a valid (i.e.
   * still active) frame.
   */

static python_type_entry_t* python_type_table[PYTHON_TYPE_TABLE_SIZE];
  /* Information about all defined Python types.
   *
   * Each entry is quite big, therefore we allocate it only for
   * registered types.
   */

static struct python_operation_fun_s
{
    const char* name;
    int num_arg;
} python_operation_fun[] = {
  /* Same order as the python_operation enum. */
    {"__add__",2},      {"__radd__",2},     {"__iadd__",2},
    {"__sub__",2},      {"__rsub__",2},     {"__isub__",2},
    {"__mul__",2},      {"__rmul__",2},     {"__imul__",2},
    {"__truediv__",2},  {"__rtruediv__",2}, {"__itruediv__",2},
    {"__mod__",2},      {"__rmod__",2},     {"__imod__",2},
    {"__lshift__",2},   {"__rlshift__",2},  {"__ilshift__",2},
    {"__rshift__",2},   {"__rrshift__",2},  {"__irshift__",2},
    {"__and__",2},      {"__rand__",2},     {"__iand__",2},
    {"__or__",2},       {"__ror__",2},      {"__ior__",2},
    {"__xor__",2},      {"__rxor__",2},     {"__ixor__",2},

    {"__lt__",2},       {"__rlt__",2},
    {"__le__",2},       {"__rle__",2},
    {"__eq__",2},       {"__req__",2},
    {"__ne__",2},       {"__rne__",2},
    {"__gt__",2},       {"__rgt__",2},
    {"__ge__",2},       {"__rge__",2},

    {"__neg__",1},
    {"__invert__",1},
};

static python_poll_fds_t *poll_fds = NULL;
  /* List of all via register_socket() registered file descriptors.
   */

static python_hook_t *python_hooks[PYTHON_HOOK_COUNT];
static const char* python_hook_names[] = {
    "ON_HEARTBEAT",
    "ON_OBJECT_CREATED",
    "ON_OBJECT_DESTRUCTED",
    "ON_CHILD_PROCESS_TERMINATED",
    "ON_SIGINT",
    "ON_SIGTERM",
    "ON_SIGHUP",
    "ON_SIGUSR1",
    "ON_SIGUSR2",
    "BEFORE_INSTRUCTION",
};

static const char* lfun_flag_names[] = {
    "LF_STATIC",
    "LF_NOMASK",
    "LF_VARARGS",
    "LF_VIRTUAL",
    "LF_DEPRECATED",
};

static const char* lfun_argument_flag_names[] = {
    "LA_VARARGS",
};

static const char* variable_flag_names[] = {
    "VF_NOSAVE",
    "VF_NOMASK",
    "VF_VIRTUAL",
    "VF_DEPRECATED",
};

static const char* visibility_names[] = {
    "VIS_PRIVATE",
    "VIS_PROTECTED",
    "VIS_VISIBLE",
    "VIS_PUBLIC",
};

static const char* call_frame_type_names[] = {
    "CALL_FRAME_TYPE_LFUN",
    "CALL_FRAME_TYPE_ALIEN_LFUN_CLOSURE",
    "CALL_FRAME_TYPE_EFUN_CLOSURE",
    "CALL_FRAME_TYPE_PYTHON_EFUN_CLOSURE",
    "CALL_FRAME_TYPE_SIMUL_EFUN_CLOSURE",
    "CALL_FRAME_TYPE_CATCH",
    "CALL_FRAME_TYPE_LAMBDA",
};

static ldmud_gc_var_t *gc_object_list = NULL,
                      *gc_lwobject_list = NULL,
                      *gc_program_list = NULL,
                      *gc_array_list = NULL,
                      *gc_mapping_list = NULL,
                      *gc_mapping_list_list = NULL,
                      *gc_struct_list = NULL,
                      *gc_closure_list = NULL,
                      *gc_coroutine_list = NULL,
                      *gc_symbol_list = NULL,
                      *gc_quoted_array_list = NULL,
                      *gc_lvalue_list = NULL,
                      *gc_call_frame_list = NULL,
                      *gc_call_frame_ref_list = NULL;
static ldmud_gc_lpctype_type_t *gc_lpctype_list = NULL;
static ldmud_concrete_struct_type_t *gc_struct_type_list = NULL;

static PyThreadState * thread_state = NULL;

#ifdef USE_PYTHON_CONTEXT
static PyObject * python_contextvar_current_object = NULL;
static PyObject * python_contextvar_command_giver = NULL;
  /* Context variables that store the current object
   * and command giver.
   */
#endif

/* -- Function prototypes --- */
static bool ldmud_concrete_struct_type_check(PyObject *ob);
static struct_type_t* ldmud_concrete_struct_type_get_struct_type(ldmud_concrete_struct_type_t *type);
static PyObject* ldmud_concrete_struct_type_create(struct_name_t* name);
static void ldmud_object_init_getobject(int num_arg UNUSED, struct ldmud_object_init_closure_s* data);
static bool ldmud_object_check(PyObject *ob);
static bool ldmud_efun_check(PyObject *ob);
static bool ldmud_python_efun_check(PyObject *ob);
static PyObject *ldmud_efun_create(int efun_idx);
static PyObject *ldmud_python_efun_create(int efun_idx);
static PyObject* lpctype_to_pythontype(lpctype_t *type);
static lpctype_t* pythontype_to_lpctype(PyObject* ptype);
static PyObject* adapt_pythontype(PyObject *ptype);
static string_t* python_string_to_string(PyObject* pstr);
static string_t* python_string_to_tabled_string(PyObject* pstr);
static const char* python_to_svalue(svalue_t *dest, PyObject* val);
static bool python_object_to_object(PyObject *ob, const char* argname, svalue_t* val, program_t **progp);
static PyObject* svalue_to_python(svalue_t *svp);
static PyObject* rvalue_to_python(svalue_t *svp);
static bool python_eq_svalue(PyObject* pval, svalue_t *sval);
static bool call_lpc_secure(CClosureFun fun, int num_arg, void* data);
static bool python_start_thread();
static void python_finish_thread(bool started);
static void python_save_context();
static void python_clear_context();
static void python_restore_context();

/* -- Python definitions and functions --- */

static void
update_efun_info (python_efun_info_t *info, PyObject *fun)

/* Fill out the <info> struct with information from <fun>.
 * It is assumed that <fun> is a python callable.
 */

{
    PyObject *annotations, *code, *property, *varnames, *returnname, *defaults;
    lpctype_t **types;
    long argcount, kwonlyargcount, flags;

    info->types = NULL;
    info->minarg = 0;
    info->maxarg = 0;
    info->varargs = true;
    info->exists = true;

    /* Let's check whether we have type information. */

    /* We have only enough information for real functions. */
    if (!PyFunction_Check(fun))
        return;

    /* First let's try to get the argument counts. */
    code = PyFunction_GetCode(fun);
    if (!code || !PyCode_Check(code))
        return;

    property = PyObject_GetAttrString(code, "co_argcount");
    if (!property || !PyLong_Check(property))
    {
        Py_XDECREF(property);
        PyErr_Clear();
        return;
    }
    argcount = PyLong_AsLong(property);
    Py_XDECREF(property);

    property = PyObject_GetAttrString(code, "co_kwonlyargcount");
    if (!property || !PyLong_Check(property))
    {
        Py_XDECREF(property);
        PyErr_Clear();
        return;
    }
    kwonlyargcount = PyLong_AsLong(property);
    Py_XDECREF(property);

    property = PyObject_GetAttrString(code, "co_flags");
    if (!property || !PyLong_Check(property))
    {
        Py_XDECREF(property);
        PyErr_Clear();
        return;
    }
    flags = PyLong_AsLong(property);
    Py_XDECREF(property);

    info->minarg = (int)argcount;
    info->maxarg = (int)argcount;
    info->varargs = (flags & CO_VARARGS) ? true : false;

    defaults = PyFunction_GetDefaults(fun);
    if (defaults && PySequence_Check(defaults))
        info->minarg -= (int)PySequence_Length(defaults);

    /* And now look at annotations to get the types. */
    annotations = PyFunction_GetAnnotations(fun);
    if (!annotations || !PyMapping_Check(annotations))
        return;

    varnames = PyObject_GetAttrString(code, "co_varnames");
    if (!varnames || !PySequence_Check(varnames))
    {
        Py_XDECREF(varnames);
        PyErr_Clear();
        return;
    }

    info->types = types = xalloc(sizeof(lpctype_t*) * (1 + argcount + ((flags & CO_VARARGS) ? 1 : 0)));
    if (types == NULL)
    {
        Py_XDECREF(varnames);
        return;
    }

    returnname = PyUnicode_FromString("return");
    if (returnname)
    {
        PyObject* retanno = PyObject_GetItem(annotations, returnname);
        if (retanno)
            types[0] = pythontype_to_lpctype(retanno);
        else
        {
            PyErr_Clear();
            types[0] = NULL;
        }

        Py_XDECREF(retanno);
        Py_DECREF(returnname);
    }

    for (long pos = 0; pos < argcount + ((flags & CO_VARARGS) ? 1 : 0); pos++)
    {
        PyObject* argname = PySequence_ITEM(varnames, pos == argcount ? argcount + kwonlyargcount : pos);
        PyObject* arganno;
        if (!argname || !PyUnicode_Check(argname))
        {
            PyErr_Clear();
            Py_XDECREF(argname);
            types[1 + pos] = NULL;
            continue;
        }

        arganno = PyObject_GetItem(annotations, argname);
        if (!arganno)
        {
            PyErr_Clear();
            Py_DECREF(argname);
            types[1 + pos] = NULL;
            continue;
        }

        types[1 + pos] = pythontype_to_lpctype(arganno);

        Py_DECREF(argname);
        Py_DECREF(arganno);
    }

    Py_DECREF(varnames);
} /* update_efun_info() */

/*-------------------------------------------------------------------------*/
static ident_t *
make_python_identifier (const char* name)

/* Creates a global identifier for <name>.
 * On error returns NULL and sets a Python exception accordingly.
 */

{
    ident_t *ident = make_shared_identifier(name, I_TYPE_GLOBAL, 0);
    if (!ident)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return NULL;
    }

    if (ident->type == I_TYPE_UNKNOWN)
    {
        init_global_identifier(ident, MY_FALSE);
        ident->next_all = all_python_idents;
        all_python_idents = ident;
    }
    else if (ident->type == I_TYPE_GLOBAL)
    {
        /* If this is a simul-efun, we need to remove it from
         * the all_simul_efuns list, otherwise the simul-efun
         * won't hesitate to unregister this efun.
         */
        if (ident->u.global.efun == I_GLOBAL_EFUN_OTHER &&
            ident->u.global.sim_efun != I_GLOBAL_SEFUN_OTHER)
        {
            for (ident_t** id = &all_simul_efuns; *id; id = &((*id)->next_all))
                if (*id == ident)
                {
                    /* Remove it from the list. */
                    *id = ident->next_all;

                    /* And add it to our list. */
                    ident->next_all = all_python_idents;
                    all_python_idents = ident;

                    break;
                }
        }
    }
    else
    {
        /* There is higher level identifier?
         * Should only happen during compile time, and that we forbid.
         */
        PyErr_SetString(PyExc_RuntimeError, "couldn't create efun entry");
        return NULL;
    }

    return ident;
} /* make_python_identifier() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_register_efun (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to register a python callable as an efun.
 * The callable is entered into the python_efun_table
 * and its table index is saved as an identifier in the lexer
 * (perhaps overriding an internal efun)
 */

{
    static char *kwlist[] = { "name", "function", NULL};

    char *name;
    PyObject *fun;
    ident_t *ident;
    python_efun_t* python_efun_entry;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "sO:register_efun", kwlist, &name, &fun))
        return NULL;

    if (!PyCallable_Check(fun))
    {
        PyErr_SetString(PyExc_TypeError, "function parameter must be callable");
        return NULL;
    }

    ident = make_python_identifier(name);
    if (!ident)
        return NULL;

    /* This is or once was a python efun? */
    if (ident->u.global.python_efun != I_GLOBAL_PYTHON_EFUN_OTHER)
    {
        int idx = ident->u.global.python_efun;

        python_efun_entry = python_efun_table + idx;
        Py_XDECREF(python_efun_table[idx].callable);
        xfree(python_efun_table[idx].info.types);
    }
    else if(num_python_efun == PYTHON_EFUN_TABLE_SIZE)
    {
        PyErr_SetString(PyExc_RuntimeError, "too many efuns registered");
        return NULL;
    }
    else
    {
        python_efun_entry = python_efun_table + num_python_efun;
        python_efun_entry->name = ident;
        ident->u.global.python_efun = (short)num_python_efun;

        num_python_efun++;
    }

    /* Update the efun table entry. */
    python_efun_entry->callable = fun;
    update_efun_info(&(python_efun_entry->info), fun);

    Py_XINCREF(fun);
    Py_INCREF(Py_None);
    PyErr_Clear();

    return Py_None;
} /* python_register_efun() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_unregister_efun (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to remove a efun registration.
 * We just set the entry in the python_efun_table to NULL.
 * The identifier stays, because we want to reuse its index,
 * when this efun will be re-registered.
 *
 * Already compiled code will still reference the python efun
 * and will produce errors.
 */

{
    static char *kwlist[] = { "name", NULL};

    char *name;
    ident_t *ident;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "s:unregister_efun", kwlist, &name))
        return NULL;

    ident = find_shared_identifier(name, I_TYPE_GLOBAL, 0);
    if (!ident || ident->type == I_TYPE_UNKNOWN)
    {
        /* No identifier there, we're done. */
        Py_INCREF(Py_None);
        return Py_None;
    }

    /* There is higher level identifier?
     * Should only happen during compile time, and that we forbid.
     */
    if (ident->type != I_TYPE_GLOBAL)
    {
        PyErr_SetString(PyExc_RuntimeError, "couldn't remove efun entry");
        return NULL;
    }

    /* This is a python efun? */
    if (ident->u.global.python_efun != I_GLOBAL_PYTHON_EFUN_OTHER)
    {
        int idx = ident->u.global.python_efun;

        Py_XDECREF(python_efun_table[idx].callable);
        xfree(python_efun_table[idx].info.types);
        python_efun_table[idx].callable = NULL;
        python_efun_table[idx].info.types = NULL;
    }

    Py_INCREF(Py_None);
    return Py_None;
} /* python_unregister_efun() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_register_struct (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to register a global struct definition.
 * The struct definition is entered into the python_struct_table
 * and its table index is saved as an identifier in the lexer
 * (perhaps overriding a driver definition).
 */

{
#   define STRUCT_MEMBER_CHUNK_SIZE 16
    struct struct_member_chunk
    {
        struct_member_t member[STRUCT_MEMBER_CHUNK_SIZE];
        struct struct_member_chunk *next;
    };

    static char *kwlist[] = { "name", "base", "fields", NULL};

    char *name;
    PyObject *base, *fields, *field, *iterator;
    ident_t *ident;
    struct struct_member_chunk *first = NULL, *current = NULL;
    struct_type_t *basetype = NULL, *stype;
    int num_members = 0;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "sOO:register_struct", kwlist, &name, &base, &fields))
        return NULL;

    if (base != Py_None)
    {
        if (!ldmud_concrete_struct_type_check(base))
        {
            PyErr_SetString(PyExc_TypeError, "base parameter must be a concrete ldmud.Struct");
            return NULL;
        }

        basetype = ldmud_concrete_struct_type_get_struct_type((ldmud_concrete_struct_type_t*)base);
        if (basetype == NULL)
            return NULL;
    }

    iterator = PyObject_GetIter(fields);
    if (!iterator)
        return NULL;

    while ((field = PyIter_Next(iterator)))
    {
        struct_member_t *entry;

        if ((num_members % STRUCT_MEMBER_CHUNK_SIZE) == 0)
        {
             struct struct_member_chunk *next = xalloc(sizeof(*next));
             if (next == NULL)
             {
                PyErr_SetString(PyExc_MemoryError, "out of memory");
                Py_DECREF(field);
                break;
            }

            if (first == NULL)
                first = current = next;
            else
            {
                current->next = next;
                current = next;
            }

            current->next = NULL;
        }

        if (!PyTuple_Check(field) || PyTuple_Size(field) != 2)
        {
            PyErr_SetString(PyExc_TypeError, "field entries must be 2-tuples");
            Py_DECREF(field);
            break;
        }

        entry = current->member + (num_members % STRUCT_MEMBER_CHUNK_SIZE);

        entry->name = python_string_to_tabled_string(PyTuple_GetItem(field, 0));
        entry->type = pythontype_to_lpctype(PyTuple_GetItem(field, 1));

        if (entry->name == NULL)
        {
            PyErr_Format(PyExc_TypeError, "member name must be string, not '%.200s'",
                         PyTuple_GetItem(field, 0)->ob_type->tp_name);
            if (entry->type != NULL)
                free_lpctype(entry->type);
            break;
        }

        if (entry->type == NULL)
        {
            PyErr_Format(PyExc_TypeError, "invalid member type '%R'",
                         PyTuple_GetItem(field, 1));
            free_mstring(entry->name);
            break;
        }

        num_members++;
    }

    if (!PyErr_Occurred())
    {
        stype = struct_new_type(new_unicode_tabled(name), ref_mstring(STR_PYTHON), 0,
                                basetype, num_members, NULL);
        if (stype == NULL)
            PyErr_SetString(PyExc_MemoryError, "out of memory");
        else
        {
            current = first;
            for (int pos = 0; pos < num_members; pos++)
            {
                if (pos != 0 && (pos % STRUCT_MEMBER_CHUNK_SIZE) == 0)
                    current = current->next;

                stype->member[pos] = current->member[pos % STRUCT_MEMBER_CHUNK_SIZE];
            }
            num_members = 0;
        }
    }

    current = first;
    for (int pos = 0; pos < num_members; pos++)
    {
        if (pos != 0 && (pos % STRUCT_MEMBER_CHUNK_SIZE) == 0)
            current = current->next;

        free_mstring(current->member[pos % STRUCT_MEMBER_CHUNK_SIZE].name);
        free_lpctype(current->member[pos % STRUCT_MEMBER_CHUNK_SIZE].type);
    }

    current = first;
    while (current != NULL)
    {
        struct struct_member_chunk *del = current;
        current = current->next;
        xfree(del);
    }

    if (PyErr_Occurred())
        return NULL;

    assert(stype != NULL);
    struct_publish_global_type(stype);

    ident = make_python_identifier(name);
    if (!ident)
        return NULL;

    /* This is or once was a python struct? */
    if (ident->u.global.python_struct_id != I_GLOBAL_PYTHON_STRUCT_OTHER)
    {
        int idx = ident->u.global.python_struct_id;

        if (python_struct_table[idx])
            free_struct_type(python_struct_table[idx]);
        python_struct_table[idx] = stype;
    }
    else if(num_python_structs == PYTHON_STRUCT_TABLE_SIZE)
    {
        free_struct_type(stype);

        PyErr_SetString(PyExc_RuntimeError, "too many structs registered");
        return NULL;
    }
    else
    {
        python_struct_table[num_python_structs] = stype;
        ident->u.global.python_struct_id = (short)num_python_structs;

        num_python_structs++;
    }

    return ldmud_concrete_struct_type_create(ref_struct_name(stype->name));
} /* python_register_struct() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_unregister_struct (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to remove a struct registration.
 * We just set the entry in the python_struct_table to NULL.
 * The identifier stays, because we want to reuse its index,
 * when this struct will be re-registered.
 */

{
    static char *kwlist[] = { "name", NULL};

    char *name;
    ident_t *ident;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "s:unregister_struct", kwlist, &name))
        return NULL;

    ident = find_shared_identifier(name, I_TYPE_GLOBAL, 0);
    if (!ident || ident->type == I_TYPE_UNKNOWN)
    {
        /* No identifier there, we're done. */
        Py_INCREF(Py_None);
        return Py_None;
    }

    /* There is higher level identifier?
     * Should only happen during compile time, and that we forbid.
     */
    if (ident->type != I_TYPE_GLOBAL)
    {
        PyErr_SetString(PyExc_RuntimeError, "couldn't remove struct entry");
        return NULL;
    }

    /* This is a Python-defined struct? */
    if (ident->u.global.python_struct_id != I_GLOBAL_PYTHON_STRUCT_OTHER)
    {
        int idx = ident->u.global.python_struct_id;

        if (python_struct_table[idx])
        {
            free_struct_type(python_struct_table[idx]);
            python_struct_table[idx] = NULL;
        }
    }

    Py_INCREF(Py_None);
    return Py_None;
} /* python_unregister_efun() */

/*-------------------------------------------------------------------------*/
static void
update_type_operation (python_type_entry_t* python_type_entry, enum python_operation op)

/* Update the entry for the given operation on the python type.
 *
 * Look up the operation <op> in the Python type of <python_type_entry> and
 * fill the entries for <op> accordingly.
 */

{
    PyObject *fun, *code, *property, *defaults, *annotations, *returnname;
    long argcount, kwonlyargcount, flags, min_arg, max_arg, num_arg;

    python_type_entry->op[op].returntype = NULL;
    python_type_entry->op[op].argtype = NULL;

    fun = PyObject_GetAttrString(python_type_entry->pytype, python_operation_fun[op].name);
    if (!fun || !PyCallable_Check(fun))
    {
        /* Not existing or not callable, handle it as non-existent. */
        Py_XDECREF(fun);
        PyErr_Clear();
        return;
    }
    num_arg = python_operation_fun[op].num_arg;

    /* Unless we know more specifics, we regard it as accepting
     * mixed and returning mixed.
     */
    python_type_entry->op[op].returntype = lpctype_mixed;
    if (num_arg > 1)
        python_type_entry->op[op].argtype = lpctype_mixed;

    if (!PyFunction_Check(fun))
    {
        /* This is not a regular function, so we cannot get any
         * specific information about its declaration.
         */
        Py_DECREF(fun);
        return;
    }

    code = PyFunction_GetCode(fun);
    if (!code || !PyCode_Check(code))
    {
        /* Same as above. */
        Py_DECREF(fun);
        return;
    }

    /* These properties should exist, otherwise something's amiss. */
    property = PyObject_GetAttrString(code, "co_argcount");
    if (!property || !PyLong_Check(property))
    {
        Py_XDECREF(property);
        Py_DECREF(fun);
        PyErr_Clear();
        return;
    }
    argcount = PyLong_AsLong(property);
    Py_XDECREF(property);

    property = PyObject_GetAttrString(code, "co_kwonlyargcount");
    if (!property || !PyLong_Check(property))
    {
        Py_XDECREF(property);
        Py_DECREF(fun);
        PyErr_Clear();
        return;
    }
    kwonlyargcount = PyLong_AsLong(property);
    Py_XDECREF(property);

    property = PyObject_GetAttrString(code, "co_flags");
    if (!property || !PyLong_Check(property))
    {
        Py_XDECREF(property);
        Py_DECREF(fun);
        PyErr_Clear();
        return;
    }
    flags = PyLong_AsLong(property);
    Py_XDECREF(property);

    min_arg = argcount;
    max_arg = argcount;

    defaults = PyFunction_GetDefaults(fun);
    if (defaults && PySequence_Check(defaults))
        min_arg -= PySequence_Length(defaults);
    if ((flags & CO_VARARGS) != 0)
    {
        min_arg--;
        max_arg+= num_arg;
    }

    if (num_arg < min_arg || num_arg > max_arg)
    {
        /* Calling it with <num_arg> arguments would result in an error,
         * so ignoring this function.
         */
        python_type_entry->op[op].returntype = NULL;
        python_type_entry->op[op].argtype = NULL;
        Py_DECREF(fun);
        return;
    }

    /* And now look at annotations to get the types. */
    annotations = PyFunction_GetAnnotations(fun);
    if (!annotations || !PyMapping_Check(annotations))
    {
        Py_DECREF(fun);
        return;
    }

    returnname = PyUnicode_FromString("return");
    if (returnname)
    {
        PyObject* retanno = PyObject_GetItem(annotations, returnname);
        if (!retanno)
            PyErr_Clear();
        else
        {
            python_type_entry->op[op].returntype = pythontype_to_lpctype(retanno);
            Py_DECREF(retanno);
        }
        Py_DECREF(returnname);
    }

    if (num_arg > 1)
    {
        PyObject *varnames = PyObject_GetAttrString(code, "co_varnames");
        PyObject *argname, *arganno;

        if (!varnames || !PySequence_Check(varnames))
        {
            Py_XDECREF(varnames);
            Py_DECREF(fun);
            PyErr_Clear();
            return;
        }

        argname = PySequence_ITEM(varnames, argcount == 1 ? argcount + kwonlyargcount : 1);
        if (!argname || !PyUnicode_Check(argname))
        {
            PyErr_Clear();
            Py_XDECREF(argname);
            Py_DECREF(varnames);
            Py_DECREF(fun);
            return;
        }

        arganno = PyObject_GetItem(annotations, argname);
        if (!arganno)
            PyErr_Clear();
        else
        {
            python_type_entry->op[op].argtype = pythontype_to_lpctype(arganno);
            Py_DECREF(arganno);
        }
        Py_DECREF(argname);
        Py_DECREF(varnames);
    }

    Py_XDECREF(fun);
} /* update_type_operation() */

/*-------------------------------------------------------------------------*/
static void
update_type_efun_info (python_type_entry_t* python_type_entry, int efun_idx)

/* Update the information for the given efun on the python type.
 *
 * Look up for an override of the efun <efun_idx> in the Python
 * type of <python_type_entry> and fill the entries it accordingly.
 */

{
    char funname[64];
    PyObject *fun;
    int efun_code;

    python_type_entry->efun[efun_idx].exists = false;

    if (efun_idx < EFUN_COUNT)
        efun_code = EFUN_OFFSET + efun_idx;
    else if (efun_idx < EFUN_COUNT + EFUN1_COUNT)
        efun_code = EFUN1_OFFSET + efun_idx - EFUN_COUNT;
    else if (efun_idx < EFUN_COUNT + EFUN1_COUNT + EFUN2_COUNT)
        efun_code = EFUN2_OFFSET + efun_idx - EFUN_COUNT - EFUN1_COUNT;
    else if (efun_idx < EFUN_COUNT + EFUN1_COUNT + EFUN2_COUNT + EFUN3_COUNT)
        efun_code = EFUN3_OFFSET + efun_idx - EFUN_COUNT - EFUN1_COUNT - EFUN2_COUNT;
    else if (efun_idx < EFUN_COUNT + EFUN1_COUNT + EFUN2_COUNT + EFUN3_COUNT + EFUN4_COUNT)
        efun_code = EFUN4_OFFSET + efun_idx - EFUN_COUNT - EFUN1_COUNT - EFUN2_COUNT - EFUN3_COUNT;
    else
        efun_code = EFUNV_OFFSET + efun_idx - EFUN_COUNT - EFUN1_COUNT - EFUN2_COUNT - EFUN3_COUNT - EFUN4_COUNT;

    snprintf(funname, sizeof(funname), "__efun_%s__", instrs[efun_code].name);
    fun = PyObject_GetAttrString(python_type_entry->pytype, funname);

    if (!fun || !PyCallable_Check(fun))
    {
        Py_XDECREF(fun);
        PyErr_Clear();
        return;
    }

    update_efun_info(python_type_entry->efun + efun_idx, fun);
    Py_DECREF(fun);
} /* update_type_efun_info() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_register_type (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to register a python type for use in LPC.
 *
 * The callable is entered into the python_efun_table
 * and its table index is saved as an identifier in the lexer
 * (perhaps overriding an internal efun)
 */

{
    static char *kwlist[] = { "name", "type", NULL};

    char *name;
    PyObject *class;
    ident_t *ident;
    python_type_entry_t* python_type_entry;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "sO:register_type", kwlist, &name, &class))
        return NULL;

    if (!PyType_Check(class))
    {
        PyErr_SetString(PyExc_TypeError, "type parameter must be a type");
        return NULL;
    }

    ident = make_shared_identifier(name, I_TYPE_PYTHON_TYPE, 0);
    if (!ident)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return NULL;
    }

    if (ident->type == I_TYPE_UNKNOWN)
    {
        if(num_python_type == PYTHON_TYPE_TABLE_SIZE)
        {
            free_shared_identifier(ident);
            PyErr_SetString(PyExc_RuntimeError, "too many types registered");
            return NULL;
        }
        else
        {
            ident->type = I_TYPE_PYTHON_TYPE;
            ident->u.python_type_id = (unsigned short) num_python_type;
            ident->next_all = all_python_types;
            all_python_types = ident;

            python_type_entry = python_type_table[num_python_type] = xalloc(sizeof(python_type_entry_t));
            if (python_type_entry == NULL)
            {
                free_shared_identifier(ident);
                PyErr_SetString(PyExc_MemoryError, "out of memory");
                return NULL;
            }

            python_type_entry->lpctype = NULL;
            python_type_entry->name = ident;
            num_python_type++;
        }
    }
    else if (ident->type == I_TYPE_PYTHON_TYPE)
    {
        /* Already registered? Reuse the entry. */
        python_type_entry = python_type_table[ident->u.python_type_id];
        assert(python_type_entry != NULL);

        Py_XDECREF(python_type_entry->pytype);

        for (int i = 0; i < PYTHON_OPERATIONS_COUNT; i++)
        {
            free_lpctype(python_type_entry->op[i].returntype);
            free_lpctype(python_type_entry->op[i].argtype);
        }

        for (int i = 0; i < REAL_EFUN_COUNT; i++)
            if (python_type_entry->efun[i].exists)
                xfree(python_type_entry->efun[i].types);
    }
    else
    {
        /* There is higher level identifier?
         * Must be a permanent define, which is forbidden.
         */
        PyErr_SetString(PyExc_RuntimeError, "couldn't create type entry");
        return NULL;
    }

    /* Update the type table entry. */
    Py_XINCREF(class);
    python_type_entry->pytype = class;
    for (int i = 0; i < PYTHON_OPERATIONS_COUNT; i++)
        update_type_operation(python_type_entry, i);
    for (int i = 0; i < REAL_EFUN_COUNT; i++)
        update_type_efun_info(python_type_entry, i);

    PyErr_Clear();

    Py_INCREF(Py_None);
    return Py_None;
} /* python_register_type() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_unregister_type (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to remove a type registration.
 * We just set the .pytype entry in the python_type_table to NULL.
 * The identifier stays, because we want to reuse its index,
 * when this type will be re-registered.
 *
 * Existing code and values will still work, but Python code can't return
 * this type anymore.
 */

{
    static char *kwlist[] = { "name", NULL};

    char *name;
    ident_t *ident;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "s:unregister_efun", kwlist, &name))
        return NULL;

    ident = find_shared_identifier(name, I_TYPE_PYTHON_TYPE, 0);
    if (!ident || ident->type == I_TYPE_UNKNOWN)
    {
        /* No identifier there, we're done. */
        Py_INCREF(Py_None);
        return Py_None;
    }

    /* There is higher level identifier?
     * Must be a permanent define, which is forbidden.
     */
    if (ident->type != I_TYPE_PYTHON_TYPE)
    {
        PyErr_SetString(PyExc_RuntimeError, "couldn't remove type entry");
        return NULL;
    }
    else
    {
        int idx = ident->u.python_type_id;

        Py_XDECREF(python_type_table[idx]->pytype);
        python_type_table[idx]->pytype = NULL;
    }

    Py_INCREF(Py_None);
    return Py_None;
} /* python_unregister_type() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_register_socket (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to register a callable with a socket.
 * The callable is called whenever an event on the socket occurs.
 * The socket is watched by the backend loop.
 */

{
    static char *kwlist[] = { "fd", "function", "eventmask", NULL};

    PyObject *fun, *fdob, *eventsfun = NULL;
    python_poll_fds_t *fds = poll_fds;
    int events = POLLIN | POLLPRI | POLLOUT;
    int fd;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "OO|O:register_socket", kwlist, &fdob, &fun, &eventsfun))
        return NULL;

    if (!PyCallable_Check(fun))
    {
        PyErr_SetString(PyExc_TypeError, "function parameter must be callable");
        return NULL;
    }

    if (eventsfun != NULL && !PyCallable_Check(eventsfun))
    {
        events = PyLong_AsLong(eventsfun);
        if (events == -1 && PyErr_Occurred())
            return NULL;

        eventsfun = NULL;
    }

    fd = PyObject_AsFileDescriptor(fdob);
    if (fd == -1)
        return NULL;

    if (eventsfun == NULL && !(events & (POLLIN|POLLPRI|POLLOUT)))
    {
        PyErr_SetString(PyExc_ValueError, "eventmask must be at least one of POLLIN, POLLPRI or POLLOUT");
        return NULL;
    }

    /* Let's go through our list, replace already registered functions
     * with this one or add a new entry.
     */

    for (; fds != NULL; fds = fds->next)
        if(fds->fd == fd)
            break;

    if (fds == NULL)
    {
        fds = xalloc(sizeof(python_poll_fds_t));
        if (fds == NULL)
            return PyErr_NoMemory();

        fds->next = poll_fds;
        fds->fun = NULL;
        fds->fd = fd;
        poll_fds = fds;
    }

    Py_XDECREF(fds->fun);
    Py_XINCREF(fun);
    Py_XINCREF(eventsfun);

    fds->fun = fun;
    fds->events = events;
    fds->eventsfun = eventsfun;

    Py_INCREF(Py_None);
    return Py_None;
} /* python_register_socket() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_unregister_socket (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to remove a socket from being watched.
 */

{
    static char *kwlist[] = { "fd", NULL};

    PyObject *fdob;
    int fd;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O:unregister_socket", kwlist, &fdob))
        return NULL;

    fd = PyObject_AsFileDescriptor(fdob);
    if (fd == -1)
        return NULL;

    for (python_poll_fds_t **fds = &poll_fds; *fds != NULL; fds = &((*fds)->next))
        if ((*fds)->fd == fd)
        {
            python_poll_fds_t *oldpoll = *fds;
            *fds = oldpoll->next;

            Py_XDECREF(oldpoll->fun);
            Py_XDECREF(oldpoll->eventsfun);
            xfree(oldpoll);
            break;
        }

    Py_INCREF(Py_None);
    return Py_None;
} /* python_unregister_socket() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_register_hook (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to register a callable as a hook.
 * The callable is called whenever the hook event occurs.
 */

{
    static char *kwlist[] = { "hook", "function", NULL};

    int hook;
    PyObject *fun;
    python_hook_t **hooklist;
    python_hook_t *entry;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "iO:register_hook", kwlist, &hook, &fun))
        return NULL;

    if (!PyCallable_Check(fun))
    {
        PyErr_SetString(PyExc_TypeError, "function parameter must be callable");
        return NULL;
    }

    if (hook < 0 || hook >= PYTHON_HOOK_COUNT)
    {
        PyErr_Format(PyExc_TypeError, "illegal hook '%d'", hook);
        return NULL;
    }

    entry = xalloc(sizeof(python_hook_t));
    if (entry == NULL)
        return PyErr_NoMemory();
    entry->fun = fun;
    entry->next = NULL;

    /* Append it at the end. */
    for(hooklist = &(python_hooks[hook]); *hooklist; hooklist = &(*hooklist)->next);

    *hooklist = entry;

    Py_XINCREF(fun);
    Py_INCREF(Py_None);
    return Py_None;
} /* python_register_hook() */

/*-------------------------------------------------------------------------*/
static PyObject*
python_unregister_hook (PyObject *module, PyObject *args, PyObject *kwds)

/* Python function to unregister a hook.
 */

{
    static char *kwlist[] = { "hook", "function", NULL};

    int hook;
    PyObject *fun;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "iO:unregister_hook", kwlist, &hook, &fun))
        return NULL;

    if (hook < 0 || hook >= PYTHON_HOOK_COUNT)
    {
        PyErr_Format(PyExc_TypeError, "illegal hook '%d'", hook);
        return NULL;
    }

    for(python_hook_t **hooklist = &(python_hooks[hook]); *hooklist; )
    {
        python_hook_t *entry = *hooklist;
        if (PyObject_RichCompareBool(entry->fun, fun, Py_EQ) == 1)
        {
            *hooklist = entry->next;

            Py_XDECREF(entry->fun);
            xfree(entry);
        }
        else
            hooklist = &(*hooklist)->next;
    }

    Py_INCREF(Py_None);
    return Py_None;
} /* python_unregister_hook() */

/*=========================================================================*/

/*                                 Types                                   */

/*-------------------------------------------------------------------------*/
/* Type structures */

struct ldmud_gc_var_s
{
    PyObject_HEAD;
    ldmud_gc_var_t *gcprev, *gcnext;
};

#define PyGCObject_HEAD ldmud_gc_var_t gcob_base;

typedef struct ldmud_lpctype_s ldmud_lpctype_t;
typedef lpctype_t * (*lpctype_factory)(ldmud_lpctype_t*);

struct ldmud_lpctype_s
{
    PyTypeObject type_base;
    lpctype_factory get_lpctype;
};

struct ldmud_gc_lpctype_type_s
{
    struct ldmud_lpctype_s ldmud_lpctype;
    lpctype_t *type;
    ldmud_gc_lpctype_type_t *gcprev, *gcnext;
};

struct ldmud_concrete_array_type_s
{
    struct ldmud_lpctype_s ldmud_lpctype;
    struct ldmud_lpctype_s* element_type;
};

struct ldmud_concrete_struct_type_s
{
    struct ldmud_lpctype_s ldmud_lpctype;
    struct_name_t *name;
    ldmud_concrete_struct_type_t *gcprev, *gcnext;
};

struct ldmud_array_s
{
    PyGCObject_HEAD

    vector_t *lpc_array;        /* Can never be NULL. */
};

struct ldmud_mapping_s
{
    PyGCObject_HEAD

    mapping_t *lpc_mapping;     /* Can never be NULL. */
};

struct ldmud_struct_s
{
    PyGCObject_HEAD

    struct_t *lpc_struct;       /* Can be NULL. */
};

struct ldmud_struct_and_index_s
{
    struct ldmud_struct_s struct_base;

    unsigned short index;       /* Member index. */
};

struct ldmud_object_s
{
    PyGCObject_HEAD

    object_t *lpc_object;       /* Can be NULL. */
};

struct ldmud_lwobject_s
{
    PyGCObject_HEAD

    lwobject_t *lpc_lwobject;   /* Can be NULL. */
};

struct ldmud_program_s
{
    PyGCObject_HEAD
    program_t *lpc_program;     /* Cannot be NULL, not refcounted. */
    svalue_t   lpc_object;      /* T_OBJECT or T_LWOBJECT. */
};

struct ldmud_program_and_index_s
{
    struct ldmud_program_s ob_base;
                                /* Object can never by NULL. */

    int index;                  /* Function or variable index. */
};

struct ldmud_program_lfun_argument_s
{

    PyObject_HEAD

    PyObject *type;             /* The type of the argument. */
    int position;               /* Position of the argument, starting with 1. */
    int flags;                  /* Flags of the argument, bitmask of LA_xxx constants. */
};

struct ldmud_closure_s
{
    PyGCObject_HEAD

    svalue_t lpc_closure;       /* Can be T_INVALID. */
};

struct ldmud_coroutine_s
{
    PyGCObject_HEAD

    coroutine_t *lpc_coroutine; /* Can be NULL. */
};

struct ldmud_symbol_s
{
    PyGCObject_HEAD

    svalue_t lpc_symbol;        /* Can be T_INVALID. */
};

struct ldmud_quoted_array_s
{
    PyGCObject_HEAD

    svalue_t lpc_quoted_array;  /* Can be T_INVALID. */
};

struct ldmud_lvalue_s
{
    PyGCObject_HEAD

    svalue_t lpc_lvalue;        /* Can be T_INVALID. */
};

struct ldmud_instruction_s
{
    PyObject_HEAD

    int full_instr;
};

struct ldmud_call_frame_s
{
    PyGCObject_HEAD

    svalue_t ob;
    program_t *prog;
    bytecode_t *pc;
    svalue_t *fp;
    const char* name;
    long frame_serial;
    int frame_level;
    enum call_frame_type type;
#ifdef EVAL_COST_TRACE
    int32 eval_cost;
#endif
};

struct ldmud_call_frame_ref_s
{
    PyGCObject_HEAD

    program_t *prog;
    long frame_serial;
    int frame_level;
};

struct ldmud_local_variables_s
{
    struct ldmud_call_frame_ref_s frame_ref_head;
    bytecode_t *pc;
    svalue_t *fp;
};

struct ldmud_local_variable_s
{
    struct ldmud_call_frame_ref_s frame_ref_head;
    local_variable_dbg_t *var;
    svalue_t *varp;
};

struct ldmud_efun_s
{
    PyObject_HEAD

    int      efun_idx;
};

typedef struct ldmud_lpctype_s ldmud_lpctype_t;
typedef struct ldmud_concrete_array_type_s ldmud_concrete_array_type_t;
typedef struct ldmud_array_s ldmud_array_t;
typedef struct ldmud_mapping_s ldmud_mapping_t;
typedef struct ldmud_struct_s ldmud_struct_t;
typedef struct ldmud_struct_and_index_s ldmud_struct_and_index_t;
typedef struct ldmud_program_s ldmud_program_t;
typedef struct ldmud_object_s ldmud_object_t;
typedef struct ldmud_lwobject_s ldmud_lwobject_t;
typedef struct ldmud_closure_s ldmud_closure_t;
typedef struct ldmud_coroutine_s ldmud_coroutine_t;
typedef struct ldmud_program_and_index_s ldmud_program_and_index_t;
typedef struct ldmud_program_lfun_argument_s ldmud_program_lfun_argument_t;
typedef struct ldmud_symbol_s ldmud_symbol_t;
typedef struct ldmud_quoted_array_s ldmud_quoted_array_t;
typedef struct ldmud_lvalue_s ldmud_lvalue_t;
typedef struct ldmud_instruction_s ldmud_instruction_t;
typedef struct ldmud_call_frame_s ldmud_call_frame_t;
typedef struct ldmud_call_frame_ref_s ldmud_call_frame_ref_t;
typedef struct ldmud_local_variables_s ldmud_local_variables_t;
typedef struct ldmud_local_variable_s ldmud_local_variable_t;
typedef struct ldmud_efun_s ldmud_efun_t;

/*-------------------------------------------------------------------------*/
/* GC Support */

static INLINE void
internal_add_gc_object (void** list, void* var, size_t gcprev_offset, size_t gcnext_offset)

/* Add <var> to the <list>.
 * <gcprev_offset> and <gcnext_offset> are offsets for member
 * variables gcprev/gcnext in <var>'s struct.
 */

{
    *(void**)(var + gcnext_offset) = *list;
    *(void**)(var + gcprev_offset) = NULL;

    if(*list != NULL)
    {
        assert(*(void**)(*list + gcprev_offset) == NULL);
        *(void**)(*list + gcprev_offset) = var;
    }
    *list = var;

    num_python_lpc_references++;
} /* internal_add_gc_object() */

#define ADD_GC_OBJECT(list, var) do { \
        void *l = list;               \
        internal_add_gc_object(&l, var, (char*)&(var->gcprev) - (char*)var, (char*)&(var->gcnext) - (char*)var); \
        list = l;                     \
    } while(0);
/*-------------------------------------------------------------------------*/
static INLINE void
internal_remove_gc_object (void** list, void* var, size_t gcprev_offset, size_t gcnext_offset)

/* Remove <var> from the <list>.
 */

{
    void* prev = *(void**)(var + gcprev_offset);
    void* next = *(void**)(var + gcnext_offset);

    if (prev == NULL)
    {
        /* List start */
        assert(*list == var);

        *list = next;
    }
    else
    {
        assert(*(void**)(prev + gcnext_offset) == var);
        *(void**)(prev + gcnext_offset) = next;
    }

    if(next)
    {
        assert(*(void**)(next + gcprev_offset) == var);
        *(void**)(next + gcprev_offset) = prev;
    }

    num_python_lpc_references--;
} /* internal_remove_gc_object() */

#define REMOVE_GC_OBJECT(list, var) do { \
        void *l = list;                  \
        internal_remove_gc_object(&l, var, (char*)&(var->gcprev) - (char*)var, (char*)&(var->gcnext) - (char*)var); \
        list = l;                        \
    } while(0);
/*-------------------------------------------------------------------------*/
static void
add_gc_object (ldmud_gc_var_t** list, ldmud_gc_var_t* var)

/* Add <var> to the <list>.
 */

{
    ADD_GC_OBJECT(*list, var);
} /* add_gc_object() */

/*-------------------------------------------------------------------------*/
static void
remove_gc_object (ldmud_gc_var_t** list, ldmud_gc_var_t* var)

/* Remove <var> from the <list>.
 */

{
    REMOVE_GC_OBJECT(*list, var);
} /* remove_gc_object() */

/*-------------------------------------------------------------------------*/
/* Helper functions for all types */

static PyObject *
get_class_dir (PyObject* self)

/* Return a set as a starting point for __dir__ implementations.
 */

{
    PyObject *dict, *cls;
    PyObject *attrs = NULL;

    /* Get the __dir__ for the class. */
    cls = PyObject_GetAttrString((PyObject*)self, "__class__");
    if (cls != NULL)
    {
        dict = PyObject_Dir(cls);
        Py_DECREF(cls);
    }
    else
        dict = NULL;
    if (dict == NULL)
        PyErr_Clear();
    else
    {
        attrs = PySet_New(dict);
        if (attrs == NULL)
            PyErr_Clear();
        Py_DECREF(dict);
    }

    if (attrs == NULL)
    {
        attrs = PySet_New(NULL);
        if (attrs == NULL)
            return NULL;
    }

    return attrs;
}

/*-------------------------------------------------------------------------*/
static string_t *
find_tabled_python_string (PyObject* name, const char *msg, bool *error)

/* Search a tabled string that matches the given Python string.
 * Returns NULL on error (error = true) or when not found (error = false).
 * <msg> denotes the kind of name for error messages.
 */

{
    PyObject *utf8;
    char* namebuf;
    ssize_t namelength;
    string_t* result;

    if (!PyUnicode_Check(name))
    {
        PyErr_Format(PyExc_TypeError,
                     "%s must be string, not '%.200s'",
                     msg, name->ob_type->tp_name);
        *error = true;
        return NULL;
    }

    utf8 = PyUnicode_AsEncodedString(name, "utf-8", "replace");
    if (utf8 == NULL)
    {
        PyErr_Format(PyExc_ValueError, "undecodable %s", msg);
        *error = true;
        return NULL;
    }

    PyBytes_AsStringAndSize(utf8, &namebuf, &namelength);
    result = find_tabled_str_n(namebuf, namelength, STRING_UTF8);
    Py_DECREF(utf8);

    *error = false;
    return result;
} /* find_tabled_python_string() */

/*-------------------------------------------------------------------------*/
static PyObject*
pointer_richcompare (void *arg1, void *arg2, int op)

/* Compare <arg1> against <arg2> with the compare operation <op>.
 */

{
    bool result;
    PyObject *resultval;

    /* Need to treat NULL specially as < or > comparisons between
     * NULL and non-NULL pointers are not defined.
     */
    if(arg1 == NULL && arg2 == NULL)
        result = op == Py_LE || op == Py_EQ || op == Py_GE;
    else if(arg1 == NULL)
        result = op == Py_LT || op == Py_LE || op == Py_NE;
    else if(arg2 == NULL)
        result = op == Py_GT || op == Py_GE || op == Py_NE;
    else
        Py_RETURN_RICHCOMPARE(arg1, arg2, op);

    resultval = result ? Py_True : Py_False;
    Py_INCREF(resultval);
    return resultval;
} /* pointer_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
object_mro (PyTypeObject *self)

/* This is for non-instantiable types to prevent cyclic references in the mro.
 * This function will return an tuple with the object type as the mro.
 */

{
    Py_INCREF(&PyBaseObject_Type);
    return PyTuple_Pack(1, &PyBaseObject_Type);
} /* object_mro() */

/*-------------------------------------------------------------------------*/
/* LPC type */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lpctype_or (PyObject* first, PyObject* second)

/* Create a union type of both arguments.
 */

{
    lpctype_t *t1 = pythontype_to_lpctype(first), *t2 = pythontype_to_lpctype(second);
    lpctype_t *t;
    PyObject *result;

    if (t1 == NULL || t2 == NULL)
    {
        free_lpctype(t1);
        free_lpctype(t2);

        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    t = get_union_type(t1, t2);
    free_lpctype(t1);
    free_lpctype(t2);

    result = lpctype_to_pythontype(t);
    free_lpctype(t);

    if (result == NULL)
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }
    return result;
} /* ldmud_lpctype_or() */

/*-------------------------------------------------------------------------*/
static int
ldmud_lpctype_contains (ldmud_lpctype_t *self, PyObject *val)

/* Check whether <val> is contained in <self>.
 */

{
    lpctype_t *member = pythontype_to_lpctype(val);
    int result = 0;

    if (member)
    {
        lpctype_t *con = (*self->get_lpctype)(self);

        result = lpctype_contains(member, con) ? 1 : 0;
        free_lpctype(con);
        free_lpctype(member);
    }

    return result;
} /* ldmud_lpctype_contains() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lpctype_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implmenent __new__ for ldmud_lpctype_t. This is a No-Op for any
 * argument that is already an lpc type. For other types we try to
 * convert. This is basically doing
 * lpctype_to_pythontype(pythontype_to_lpctype(arg)).
 */

{
    static char *kwlist[] = { "type", NULL};

    PyObject *value;
    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O", kwlist, &value))
        return NULL;

    return adapt_pythontype(value);
} /* ldmud_lpctype_new() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lpctype_str (ldmud_lpctype_t* self)

/* Return the LPC string for the given type.
 */

{
    lpctype_t *me = (*self->get_lpctype)(self);
    const char *str = get_lpctype_name(me);

    free_lpctype(me);

    return PyUnicode_FromString(str);
} /* ldmud_lpctype_str() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_lpctype_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.LPCType.
 */

{
    return lpctype_lpctype;
} /* ldmud_lpctype_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_lpctype_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    0,                                  /* nb_bool */
    0,                                  /* nb_invert */
    0,                                  /* nb_lshift */
    0,                                  /* nb_rshift */
    0,                                  /* nb_and */
    0,                                  /* nb_xor */
    ldmud_lpctype_or,                   /* nb_or */
};

static PySequenceMethods ldmud_lpctype_as_sequence = {
    0,                                  /* sq_length */
    0,                                  /* sq_concat */
    0,                                  /* sq_repeat */
    0,                                  /* sq_item */
    0,                                  /* sq_slice */
    0,                                  /* sq_ass_item */
    0,                                  /* sq_ass_slice */
    (objobjproc)ldmud_lpctype_contains, /* sq_contains */
    0,                                  /* sq_inplace_concat */
    0,                                  /* sq_inplace_repeat */
};

static ldmud_lpctype_t ldmud_lpctype_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.LPCType",                    /* tp_name */
    sizeof(ldmud_lpctype_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    &ldmud_lpctype_as_number,           /* tp_as_number */
    &ldmud_lpctype_as_sequence,         /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    (reprfunc)ldmud_lpctype_str,        /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC type",                         /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &PyType_Type,                       /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_lpctype_new,                  /* tp_new */
},  ldmud_lpctype_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
/* LPC union type. */

static bool ldmud_union_type_check(PyObject *ob);
static PyObject* ldmud_union_type_new(PyTypeObject *type, PyObject *args, PyObject *kwds);

/*-------------------------------------------------------------------------*/
static void
add_gc_lpctype_object (ldmud_gc_lpctype_type_t* ob)

/* Add <var> to the <list>.
 */

{
    ADD_GC_OBJECT(gc_lpctype_list, ob);
} /* add_gc_object() */

/*-------------------------------------------------------------------------*/
static void
remove_gc_lpctype_object (ldmud_gc_lpctype_type_t* ob)

/* Remove <var> from the <list>.
 */

{
    REMOVE_GC_OBJECT(gc_lpctype_list, ob);
} /* remove_gc_object() */

/*-------------------------------------------------------------------------*/
static void
ldmud_lpctype_type_dealloc (ldmud_gc_lpctype_type_t* self)

/* Destroy the ldmud_gc_lpctype_type_t object
 */

{
    free_lpctype(self->type);

    remove_gc_lpctype_object(self);

    if (Py_TYPE(self)->tp_flags & Py_TPFLAGS_HAVE_GC)
        PyObject_GC_UnTrack(self);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_base);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_dict);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_bases);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_mro);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_cache);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_subclasses);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_lpctype_type_dealloc() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_lpctype_type_hash (ldmud_gc_lpctype_type_t *type)

/* Return a hash of this lpctype.
 */

{
    return _Py_HashPointer(type->type);
} /* ldmud_lpctype_type_hash() */

/*-------------------------------------------------------------------------*/
static lpctype_t *
ldmud_lpctype_type_get_lpctype(ldmud_gc_lpctype_type_t *type)

/* Create the LPC type object for the Python type.
 */

{
    return ref_lpctype(type->type);
} /* ldmud_lpctype_type_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_union_type_repr (ldmud_gc_lpctype_type_t *type)

/* Return a string representation for this union type.
 */

{
    return PyUnicode_FromFormat("ldmud.UnionType(%s)", get_lpctype_name(type->type));
} /* ldmud_union_type_repr() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_union_type_richcompare (ldmud_gc_lpctype_type_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_union_type_check(other))
        Py_RETURN_NOTIMPLEMENTED;

    return pointer_richcompare(self->type, ((ldmud_gc_lpctype_type_t*)other)->type, op);
} /* ldmud_union_type_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_union_type_iter_next (ldmud_gc_lpctype_type_t* self)

/* Return the next value for this iterator and update the iterator.
 */

{
    if (!self->type)
        return NULL;

    if (self->type->t_class == TCLASS_UNION)
    {
        lpctype_t * old = self->type;
        PyObject * result = lpctype_to_pythontype(self->type->t_union.member);
        if (!result)
            PyErr_Format(PyExc_TypeError, "unsupported lpctype [%s]", get_lpctype_name(self->type->t_union.member));

        self->type = ref_lpctype(self->type->t_union.head);
        free_lpctype(old);

        return result;
    }
    else
    {
        PyObject * result = lpctype_to_pythontype(self->type);
        if (!result)
            PyErr_Format(PyExc_TypeError, "unsupported lpctype [%s]", get_lpctype_name(self->type));

        free_lpctype(self->type);
        self->type = NULL;

        return result;
    }
} /* ldmud_union_type_iter_next() */

/*-------------------------------------------------------------------------*/
static PyTypeObject ldmud_union_type_iter_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.UnionTypeIter",              /* tp_name */
    sizeof(ldmud_gc_lpctype_type_t),    /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_lpctype_type_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC union type iterator",          /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    PyObject_SelfIter,                  /* tp_iter */
    (iternextfunc)ldmud_union_type_iter_next, /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_union_type_iter (ldmud_gc_lpctype_type_t* self)

/* Create an iterator for the union type.
 */

{
    ldmud_gc_lpctype_type_t *result = (ldmud_gc_lpctype_type_t*)ldmud_union_type_iter_type.tp_alloc(&ldmud_union_type_iter_type, 0);
    if (result == NULL)
        return NULL;

    result->type = ref_lpctype(self->type);

    add_gc_lpctype_object(result);

    return (PyObject*)result;
} /* ldmud_union_type_iter() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_union_type_type_methods[] =
{
    {
        "mro",
        (PyCFunction) object_mro, METH_NOARGS,
        "mro() -> tuple\n\n"
        "Return the method resolution order."
    },
    {NULL}
};

static PyTypeObject ldmud_union_type_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.UnionType",                  /* tp_name */
    sizeof(ldmud_gc_lpctype_type_t),    /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_lpctype_type_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_union_type_repr,    /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_lpctype_type_hash,  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC union type",                   /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_union_type_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    (getiterfunc)ldmud_union_type_iter, /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_union_type_type_methods,      /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &ldmud_lpctype_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_union_type_check (PyObject *ob)

/* Returns true, when <ob> is a concrete LPC array type.
 */

{
    return Py_TYPE(ob) == &ldmud_union_type_type;
} /* ldmud_union_type_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_union_type_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Just prevent creating an instance of this type.
 */

{
    PyErr_SetString(PyExc_TypeError, "cannot create instances of a union type");
    return NULL;
} /* ldmud_union_type_new() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_union_type_create (lpctype_t* type)

/* Create a union type for <type>. <type> is assumed to be a union type.
 * This type class is never really used as a Python class type.
 * The reference of <type> is adopted.
 */

{
    ldmud_gc_lpctype_type_t *result;

    result = (ldmud_gc_lpctype_type_t*) ldmud_union_type_type.tp_alloc(&ldmud_union_type_type, 0);
    if (result == NULL)
    {
        free_lpctype(type);
        return NULL;
    }

    result->ldmud_lpctype.type_base.tp_name = "ldmud.Union";
    result->ldmud_lpctype.type_base.tp_doc = "LPC union";
    result->ldmud_lpctype.type_base.tp_basicsize = sizeof(PyObject);
    result->ldmud_lpctype.type_base.tp_base = &PyBaseObject_Type;
    result->ldmud_lpctype.type_base.tp_flags = Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS|Py_TPFLAGS_HEAPTYPE
#ifdef Py_TPFLAGS_DISALLOW_INSTANTIATION
                                             | Py_TPFLAGS_DISALLOW_INSTANTIATION
#endif
                                             ;
    result->ldmud_lpctype.type_base.tp_new = ldmud_union_type_new;
    result->ldmud_lpctype.get_lpctype = (lpctype_factory)ldmud_lpctype_type_get_lpctype;
    result->type = type;
    Py_INCREF(&PyBaseObject_Type);

    add_gc_lpctype_object(result);

    if (PyType_Ready(&result->ldmud_lpctype.type_base) < 0)
    {
        Py_DECREF(result);
        return NULL;
    }

#ifndef Py_TPFLAGS_DISALLOW_INSTANTIATION
    if (result->ldmud_lpctype.type_base.tp_dict != NULL)
    {
        /* Remove the __new__ function. */
        if (PyDict_DelItemString(result->ldmud_lpctype.type_base.tp_dict, "__new__"))
            PyErr_Clear();
    }
#endif

    return (PyObject*)result;
} /* ldmud_union_type_create() */

/*-------------------------------------------------------------------------*/
/* LPC array type. */

static bool ldmud_concrete_array_type_check(PyObject *ob);
static PyObject* ldmud_concrete_array_new(PyTypeObject *type, PyObject *args, PyObject *kwds);

/*-------------------------------------------------------------------------*/
static void
ldmud_concrete_array_type_dealloc (ldmud_concrete_array_type_t* self)

/* Destroy the ldmud_concrete_array_type_t object
 */

{
    if (Py_TYPE(self)->tp_flags & Py_TPFLAGS_HAVE_GC)
        PyObject_GC_UnTrack(self);
    Py_XDECREF(self->element_type);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_base);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_dict);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_bases);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_mro);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_cache);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_subclasses);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_concrete_array_type_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_array_type_repr (ldmud_concrete_array_type_t *type)

/* Return a string representation for this array type.
 */

{
    return PyUnicode_FromFormat("ldmud.Array[%R]", type->element_type);
} /* ldmud_concrete_array_type_repr() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_concrete_array_type_hash (ldmud_concrete_array_type_t *type)

/* Return a hash of this concrete array type.
 */

{
    return PyObject_Hash((PyObject*)type->element_type);
} /* ldmud_concrete_array_type_hash() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_array_type_richcompare (ldmud_concrete_array_type_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_concrete_array_type_check(other))
        Py_RETURN_NOTIMPLEMENTED;

    return PyObject_RichCompare((PyObject*)self->element_type,
                                (PyObject*)((ldmud_concrete_array_type_t*)other)->element_type, op);
} /* ldmud_concrete_array_type_richcompare() */

/*-------------------------------------------------------------------------*/
static lpctype_t *
ldmud_concrete_array_get_lpctype(ldmud_concrete_array_type_t *type)

/* Create the LPC type object for the Python type.
 */

{
    lpctype_t *elem = (*type->element_type->get_lpctype)(type->element_type);
    lpctype_t *result = get_array_type(elem);
    free_lpctype(elem);
    return result;
} /* ldmud_concrete_array_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_concrete_array_type_type_methods[] =
{
    {
        "mro",
        (PyCFunction) object_mro, METH_NOARGS,
        "mro() -> tuple\n\n"
        "Return the method resolution order."
    },
    {NULL}
};

static PyTypeObject ldmud_concrete_array_type_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.ArrayType",                  /* tp_name */
    sizeof(ldmud_concrete_array_type_t),/* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_concrete_array_type_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_concrete_array_type_repr, /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_concrete_array_type_hash, /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC array type",                   /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_concrete_array_type_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_concrete_array_type_type_methods, /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &ldmud_lpctype_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_concrete_array_type_check (PyObject *ob)

/* Returns true, when <ob> is a concrete LPC array type.
 */

{
    return Py_TYPE(ob) == &ldmud_concrete_array_type_type;
} /* ldmud_concrete_array_type_check() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_concrete_array_type_new_method =
{
    "__new__",
    (PyCFunction) ldmud_concrete_array_new,  METH_VARARGS | METH_KEYWORDS | METH_STATIC,
    "__new__(values|size) -> ldmud.Array\n\n"
    "Create an LPC array with the given values or of the given size."
};

static PyObject*
ldmud_concrete_array_type_create (ldmud_lpctype_t* element)

/* Create a type for Array[element type].
 * This type class is never really used as a Python class type.
 * It's __new__ function will create a type of ldmud.Array instead.
 */

{
    ldmud_concrete_array_type_t *result;

    result = (ldmud_concrete_array_type_t*) ldmud_concrete_array_type_type.tp_alloc(&ldmud_concrete_array_type_type, 0);
    if (result == NULL)
        return NULL;

    result->ldmud_lpctype.type_base.tp_name = "ldmud.Array";
    result->ldmud_lpctype.type_base.tp_doc = "LPC array";
    result->ldmud_lpctype.type_base.tp_basicsize = sizeof(PyObject);
    result->ldmud_lpctype.type_base.tp_base = &PyBaseObject_Type;
    result->ldmud_lpctype.type_base.tp_flags = Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS|Py_TPFLAGS_HEAPTYPE;
    result->ldmud_lpctype.type_base.tp_new = ldmud_concrete_array_new;
    result->ldmud_lpctype.get_lpctype = (lpctype_factory)ldmud_concrete_array_get_lpctype;
    result->element_type = element;
    Py_INCREF(&PyBaseObject_Type);
    Py_INCREF(element);

    if (PyType_Ready(&result->ldmud_lpctype.type_base) < 0)
    {
        Py_DECREF(result);
        return NULL;
    }

    if (result->ldmud_lpctype.type_base.tp_dict != NULL)
    {
        /* We set the __new__ function to a static method here.
         * (Using .tp_new would result in circular dependencies as this
         * method would be bound to the result object.)
         */
        PyObject *new_fun = PyCFunction_New(&ldmud_concrete_array_type_new_method, NULL);
        if (new_fun == NULL
         || PyDict_SetItemString(result->ldmud_lpctype.type_base.tp_dict, "__new__", new_fun) != 0)
            PyErr_Clear();
        Py_XDECREF(new_fun);
    }

    return (PyObject*)result;
} /* ldmud_concrete_array_type_create() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_any_array_type_subscript (PyTypeObject* array_type, PyObject *key)

/* Create a type object that represents an array of a concrete type.
 */

{
    PyObject *element = adapt_pythontype(key);
    if (element && PyObject_TypeCheck(element, &ldmud_lpctype_type.type_base))
    {
        PyObject *result = ldmud_concrete_array_type_create((ldmud_lpctype_t*)element);
        Py_DECREF(element);
        return result;
    }

    Py_XDECREF(element);

    PyErr_SetString(PyExc_TypeError, "key must be an ldmud.LPCType");
    return NULL;
} /* ldmud_any_array_type_subscript() */

/*-------------------------------------------------------------------------*/
static PyMappingMethods ldmud_any_array_type_as_mapping = {
    0,                                          /*mp_length*/
    (binaryfunc)ldmud_any_array_type_subscript, /*mp_subscript*/
    0,                                          /*mp_ass_subscript*/
};

static PyTypeObject ldmud_any_array_type_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.ArrayType",                  /* tp_name */
    sizeof(ldmud_lpctype_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    &ldmud_any_array_type_as_mapping,   /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC array type",                   /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &ldmud_lpctype_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
/* LPC struct type. */

static PyObject* ldmud_concrete_struct_new(PyTypeObject *type, PyObject *args, PyObject *kwds);

/*-------------------------------------------------------------------------*/
static void
ldmud_concrete_struct_type_dealloc (ldmud_concrete_struct_type_t* self)

/* Destroy the ldmud_concrete_struct_type_t object
 */

{
    free_struct_name(self->name);
    REMOVE_GC_OBJECT(gc_struct_type_list, self);

    if (Py_TYPE(self)->tp_flags & Py_TPFLAGS_HAVE_GC)
        PyObject_GC_UnTrack(self);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_base);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_dict);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_bases);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_mro);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_cache);
    Py_XDECREF(self->ldmud_lpctype.type_base.tp_subclasses);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_concrete_struct_type_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_struct_type_repr (ldmud_concrete_struct_type_t *type)

/* Return a string representation for this struct type.
 */

{
    return PyUnicode_FromFormat("ldmud.Struct[\"%s\",\"%s\"]", get_txt(type->name->prog_name), get_txt(type->name->name));
} /* ldmud_concrete_struct_type_repr() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_concrete_struct_type_hash (ldmud_concrete_struct_type_t *type)

/* Return a hash of this concrete struct type.
 */

{
    return (Py_hash_t)type->name->hash;
} /* ldmud_concrete_struct_type_hash() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_struct_type_richcompare (ldmud_concrete_struct_type_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    int cmp;

    if (!ldmud_concrete_struct_type_check(other))
        Py_RETURN_NOTIMPLEMENTED;

    cmp = mstrcmp(self->name->prog_name, ((ldmud_concrete_struct_type_t*)other)->name->prog_name);
    if (cmp == 0)
        cmp = mstrcmp(self->name->name, ((ldmud_concrete_struct_type_t*)other)->name->name);

    Py_RETURN_RICHCOMPARE(cmp, 0, op);
} /* ldmud_concrete_struct_type_richcompare() */

/*-------------------------------------------------------------------------*/
static lpctype_t *
ldmud_concrete_struct_get_lpctype(ldmud_concrete_struct_type_t *type)

/* Create the LPC type object for the Python type.
 */

{
    return get_struct_name_type(type->name);
} /* ldmud_concrete_struct_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_concrete_struct_type_type_methods[] =
{
    {
        "mro",
        (PyCFunction) object_mro, METH_NOARGS,
        "mro() -> tuple\n\n"
        "Return the method resolution order."
    },
    {NULL}
};

static PyTypeObject ldmud_concrete_struct_type_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.StructType",                 /* tp_name */
    sizeof(ldmud_concrete_struct_type_t),/* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_concrete_struct_type_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_concrete_struct_type_repr, /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_concrete_struct_type_hash, /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC struct type",                   /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_concrete_struct_type_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_concrete_struct_type_type_methods, /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &ldmud_lpctype_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_concrete_struct_type_check (PyObject *ob)

/* Returns true, when <ob> is a concrete LPC struct type.
 */

{
    return Py_TYPE(ob) == &ldmud_concrete_struct_type_type;
} /* ldmud_concrete_struct_type_check() */

/*-------------------------------------------------------------------------*/
static struct_type_t *
ldmud_concrete_struct_type_get_struct_type (ldmud_concrete_struct_type_t *type)

/* Returns the struct type for this instance. Loads the object if required.
 * If the object cannot be loaded or the struct definition is not found,
 * returns NULL and raises an appropriate Python exception.
 */

{
    struct_name_t *name = type->name;
    struct_type_t *stype;

    if (name->current)
        return name->current;

    /* We don't have a current struct definition.
     * So we need to load the program to get that definition.
     */
    struct ldmud_object_init_closure_s data = { name->prog_name, NULL };
    if (!call_lpc_secure((CClosureFun)ldmud_object_init_getobject, 0, &data))
        return NULL;

    if (!data.ob)
    {
        PyErr_Format(PyExc_ValueError, "could not load '%s'", get_txt(name->prog_name));
        return NULL;
    }

    stype = struct_find(name->name, data.ob->prog);
    if (!stype)
    {
        PyErr_Format(PyExc_ValueError, "unknown struct '%s'", get_txt(name->name));
        return NULL;
    }

    return stype;
} /* ldmud_concrete_struct_type_get_struct_type() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_concrete_struct_type_new_method =
{
    "__new__",
    (PyCFunction) ldmud_concrete_struct_new,  METH_VARARGS | METH_KEYWORDS | METH_STATIC,
    "__new__(values) -> ldmud.Struct\n\n"
    "Create an LPC struct with the given values."
};

static PyObject*
ldmud_concrete_struct_type_create (struct_name_t* name)

/* Create a type for Struct[<name->prog_name>, <name->name>].
 * This type class is never really used as a Python class type.
 * It's __new__ function will create a type of ldmud.Struct instead.
 * The reference to <name> is adopted.
 */

{
    ldmud_concrete_struct_type_t *result;

    result = (ldmud_concrete_struct_type_t*) ldmud_concrete_struct_type_type.tp_alloc(&ldmud_concrete_struct_type_type, 0);
    if (result == NULL)
    {
        free_struct_name(name);
        return NULL;
    }

    result->ldmud_lpctype.type_base.tp_name = "ldmud.Struct";
    result->ldmud_lpctype.type_base.tp_doc = "LPC struct";
    result->ldmud_lpctype.type_base.tp_basicsize = sizeof(PyObject);
    result->ldmud_lpctype.type_base.tp_base = &PyBaseObject_Type;
    result->ldmud_lpctype.type_base.tp_flags = Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS|Py_TPFLAGS_HEAPTYPE;
    result->ldmud_lpctype.type_base.tp_new = ldmud_concrete_struct_new;
    result->ldmud_lpctype.get_lpctype = (lpctype_factory)ldmud_concrete_struct_get_lpctype;
    result->name = name;
    Py_INCREF(&PyBaseObject_Type);
    ADD_GC_OBJECT(gc_struct_type_list, result);

    if (PyType_Ready(&result->ldmud_lpctype.type_base) < 0)
    {
        Py_DECREF(result);
        return NULL;
    }

    if (result->ldmud_lpctype.type_base.tp_dict != NULL)
    {
        /* We set the __new__ function to a static method here.
         * (Using .tp_new would result in circular dependencies as this
         * method would be bound to the result object.)
         */
        PyObject *new_fun = PyCFunction_New(&ldmud_concrete_struct_type_new_method, NULL);
        if (new_fun == NULL
         || PyDict_SetItemString(result->ldmud_lpctype.type_base.tp_dict, "__new__", new_fun) != 0)
            PyErr_Clear();
        Py_XDECREF(new_fun);
    }

    return (PyObject*)result;
} /* ldmud_concrete_struct_type_create() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_any_struct_type_subscript (PyTypeObject* struct_type, PyObject *key)

/* Create a type object that represents an struct of a concrete type.
 */

{
    PyObject *prog, *sname;
    string_t *progstr, *snamestr;
    struct_name_t *name;

    if (!PyTuple_Check(key) || PyTuple_Size(key) != 2)
    {
        PyErr_SetString(PyExc_TypeError, "key must be (program name, struct name)");
        return NULL;
    }

    prog = PyTuple_GetItem(key, 0);
    sname = PyTuple_GetItem(key, 1);

    if (ldmud_object_check(prog))
    {
        object_t *lpc_ob = ((ldmud_object_t*)prog)->lpc_object;
        if(!lpc_ob)
        {
            PyErr_SetString(PyExc_TypeError, "uninitialized lpc object");
            return NULL;
        }

        if (O_PROG_SWAPPED(lpc_ob) && load_ob_from_swap(lpc_ob) < 0)
        {
            PyErr_SetString(PyExc_MemoryError, "out of memory while unswapping");
            return NULL;
        }

        progstr = ref_mstring(lpc_ob->prog->name);
    }
    else if (PyUnicode_Check(prog))
    {
        progstr = python_string_to_string(prog);
        if (progstr == NULL)
        {
            PyErr_SetString(PyExc_ValueError, "could not decode program name");
            return NULL;
        }
    }
    else
    {
        PyErr_Format(PyExc_TypeError, "program name must be ldmud.Object or str, not %.200s", prog->ob_type->tp_name);
        return NULL;
    }

    if (PyUnicode_Check(sname))
    {
        snamestr = python_string_to_string(sname);
        if (snamestr == NULL)
        {
            free_mstring(progstr);
            PyErr_SetString(PyExc_ValueError, "could not decode struct name");
            return NULL;
        }
    }
    else
    {
        free_mstring(progstr);
        PyErr_Format(PyExc_TypeError, "struct name must be str, not %.200s", sname->ob_type->tp_name);
        return NULL;
    }

    name = struct_new_name(snamestr, progstr);
    if (name == NULL)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return NULL;
    }

    return ldmud_concrete_struct_type_create(name);
} /* ldmud_any_struct_type_subscript() */

/*-------------------------------------------------------------------------*/
static PyMappingMethods ldmud_any_struct_type_as_mapping = {
    0,                                           /*mp_length*/
    (binaryfunc)ldmud_any_struct_type_subscript, /*mp_subscript*/
    0,                                           /*mp_ass_subscript*/
};

static PyTypeObject ldmud_any_struct_type_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.StructType",                 /* tp_name */
    sizeof(ldmud_lpctype_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    &ldmud_any_struct_type_as_mapping,  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC struct type",                  /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &ldmud_lpctype_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
/* LPC object type. */

static bool ldmud_concrete_object_type_check(PyObject *ob);
static PyObject* ldmud_concrete_object_new(PyTypeObject *type, PyObject *args, PyObject *kwds);

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_object_type_repr (ldmud_gc_lpctype_type_t *type)

/* Return a string representation for this object type.
 */

{
    assert(type->type->t_class == TCLASS_OBJECT);
    return PyUnicode_FromFormat("ldmud.Object[%s]", get_txt(type->type->t_object.program_name));
} /* ldmud_concrete_object_type_repr() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_object_type_richcompare (ldmud_gc_lpctype_type_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_concrete_object_type_check(other))
        Py_RETURN_NOTIMPLEMENTED;

    return pointer_richcompare(self->type, ((ldmud_gc_lpctype_type_t*)other)->type, op);
} /* ldmud_concrete_object_type_richcompare() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_concrete_object_type_type_methods[] =
{
    {
        "mro",
        (PyCFunction) object_mro, METH_NOARGS,
        "mro() -> tuple\n\n"
        "Return the method resolution order."
    },
    {NULL}
};

static PyTypeObject ldmud_concrete_object_type_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.ObjectType",                 /* tp_name */
    sizeof(ldmud_gc_lpctype_type_t),    /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_lpctype_type_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_concrete_object_type_repr, /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_lpctype_type_hash,  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC object type",                  /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_concrete_object_type_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_concrete_object_type_type_methods, /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &ldmud_lpctype_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_concrete_object_type_check (PyObject *ob)

/* Returns true, when <ob> is a concrete LPC object type.
 */

{
    return Py_TYPE(ob) == &ldmud_concrete_object_type_type;
} /* ldmud_concrete_object_type_check() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_concrete_object_type_new_method =
{
    "__new__",
    (PyCFunction) ldmud_concrete_object_new,  METH_VARARGS | METH_KEYWORDS | METH_STATIC,
    "__new__(filename) -> ldmud.Object\n\n"
    "Create an LPC object from the filename."
};

static PyObject*
ldmud_concrete_object_type_create (lpctype_t* object_type)

/* Create a type for <object_type>, which is assumed to be a named object type.
 * The reference for <object_type> will be adopted.
 * This type class is never really used as a Python class type.
 * It's __new__ function will create a type of ldmud.Object instead.
 */

{
    ldmud_gc_lpctype_type_t *result;

    result = (ldmud_gc_lpctype_type_t*) ldmud_concrete_object_type_type.tp_alloc(&ldmud_concrete_object_type_type, 0);
    if (result == NULL)
        return NULL;

    result->ldmud_lpctype.type_base.tp_name = "ldmud.Object";
    result->ldmud_lpctype.type_base.tp_doc = "LPC object";
    result->ldmud_lpctype.type_base.tp_basicsize = sizeof(PyObject);
    result->ldmud_lpctype.type_base.tp_base = &PyBaseObject_Type;
    result->ldmud_lpctype.type_base.tp_flags = Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS|Py_TPFLAGS_HEAPTYPE;
    result->ldmud_lpctype.type_base.tp_new = ldmud_concrete_object_new;
    result->ldmud_lpctype.get_lpctype = (lpctype_factory)ldmud_lpctype_type_get_lpctype;
    result->type = object_type;
    Py_INCREF(&PyBaseObject_Type);

    add_gc_lpctype_object(result);

    if (PyType_Ready(&result->ldmud_lpctype.type_base) < 0)
    {
        Py_DECREF(result);
        return NULL;
    }

    if (result->ldmud_lpctype.type_base.tp_dict != NULL)
    {
        /* We set the __new__ function to a static method here.
         * (Using .tp_new would result in circular dependencies as this
         * method would be bound to the result object.)
         */
        PyObject *new_fun = PyCFunction_New(&ldmud_concrete_object_type_new_method, NULL);
        if (new_fun == NULL
         || PyDict_SetItemString(result->ldmud_lpctype.type_base.tp_dict, "__new__", new_fun) != 0)
            PyErr_Clear();
        Py_XDECREF(new_fun);
    }

    return (PyObject*)result;
} /* ldmud_concrete_object_type_create() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_any_object_type_subscript (PyTypeObject* object_type, PyObject *key)

/* Create a type object that represents a named object type.
 */

{
    string_t *name = python_string_to_string(key);
    if (name != NULL)
    {
        lpctype_t *obtype = get_object_type(name);

        free_mstring(name);
        if (!obtype)
        {
            PyErr_SetString(PyExc_MemoryError, "out of memory");
            return NULL;
        }

        return ldmud_concrete_object_type_create(obtype);
    }

    PyErr_SetString(PyExc_TypeError, "key must be a string");
    return NULL;
} /* ldmud_any_object_type_subscript() */

/*-------------------------------------------------------------------------*/
static PyMappingMethods ldmud_any_object_type_as_mapping = {
    0,                                           /*mp_length*/
    (binaryfunc)ldmud_any_object_type_subscript, /*mp_subscript*/
    0,                                           /*mp_ass_subscript*/
};

static PyTypeObject ldmud_any_object_type_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.ObjectType",                 /* tp_name */
    sizeof(ldmud_lpctype_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    &ldmud_any_object_type_as_mapping,  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC object type",                  /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &ldmud_lpctype_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
/* LPC lightweight object type. */

static bool ldmud_concrete_lwobject_type_check(PyObject *ob);
static PyObject* ldmud_concrete_lwobject_new(PyTypeObject *type, PyObject *args, PyObject *kwds);

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_lwobject_type_repr (ldmud_gc_lpctype_type_t *type)

/* Return a string representation for this object type.
 */

{
    assert(type->type->t_class == TCLASS_OBJECT);
    return PyUnicode_FromFormat("ldmud.LWObject[%s]", get_txt(type->type->t_object.program_name));
} /* ldmud_concrete_lwobject_type_repr() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_lwobject_type_richcompare (ldmud_gc_lpctype_type_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_concrete_lwobject_type_check(other))
        Py_RETURN_NOTIMPLEMENTED;

    return pointer_richcompare(self->type, ((ldmud_gc_lpctype_type_t*)other)->type, op);
} /* ldmud_concrete_lwobject_type_richcompare() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_concrete_lwobject_type_type_methods[] =
{
    {
        "mro",
        (PyCFunction) object_mro, METH_NOARGS,
        "mro() -> tuple\n\n"
        "Return the method resolution order."
    },
    {NULL}
};

static PyTypeObject ldmud_concrete_lwobject_type_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.LWObjectType",               /* tp_name */
    sizeof(ldmud_gc_lpctype_type_t),    /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_lpctype_type_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_concrete_lwobject_type_repr, /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_lpctype_type_hash,  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC object type",                  /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_concrete_lwobject_type_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_concrete_lwobject_type_type_methods, /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &ldmud_lpctype_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_concrete_lwobject_type_check (PyObject *ob)

/* Returns true, when <ob> is a concrete LPC object type.
 */

{
    return Py_TYPE(ob) == &ldmud_concrete_lwobject_type_type;
} /* ldmud_concrete_lwobject_type_check() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_concrete_lwobject_type_new_method =
{
    "__new__",
    (PyCFunction) ldmud_concrete_lwobject_new,  METH_VARARGS | METH_KEYWORDS | METH_STATIC,
    "__new__(filename) -> ldmud.LWObject\n\n"
    "Create an LPC lightweight object from the filename."
};

static PyObject*
ldmud_concrete_lwobject_type_create (lpctype_t* lwobject_type)

/* Create a type for <lwobject_type>, which is assumed to be a named
 * lwobject type. The reference for <lwobject_type> will be adopted.
 * This type class is never really used as a Python class type.
 * It's __new__ function will create a type of ldmud.Object instead.
 */

{
    ldmud_gc_lpctype_type_t *result;

    result = (ldmud_gc_lpctype_type_t*) ldmud_concrete_lwobject_type_type.tp_alloc(&ldmud_concrete_lwobject_type_type, 0);
    if (result == NULL)
        return NULL;

    result->ldmud_lpctype.type_base.tp_name = "ldmud.LWObject";
    result->ldmud_lpctype.type_base.tp_doc = "LPC lightweight object";
    result->ldmud_lpctype.type_base.tp_basicsize = sizeof(PyObject);
    result->ldmud_lpctype.type_base.tp_base = &PyBaseObject_Type;
    result->ldmud_lpctype.type_base.tp_flags = Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS|Py_TPFLAGS_HEAPTYPE;
    result->ldmud_lpctype.type_base.tp_new = ldmud_concrete_lwobject_new;
    result->ldmud_lpctype.get_lpctype = (lpctype_factory)ldmud_lpctype_type_get_lpctype;
    result->type = lwobject_type;
    Py_INCREF(&PyBaseObject_Type);

    add_gc_lpctype_object(result);

    if (PyType_Ready(&result->ldmud_lpctype.type_base) < 0)
    {
        Py_DECREF(result);
        return NULL;
    }

    if (result->ldmud_lpctype.type_base.tp_dict != NULL)
    {
        /* We set the __new__ function to a static method here.
         * (Using .tp_new would result in circular dependencies as this
         * method would be bound to the result object.)
         */
        PyObject *new_fun = PyCFunction_New(&ldmud_concrete_lwobject_type_new_method, NULL);
        if (new_fun == NULL
         || PyDict_SetItemString(result->ldmud_lpctype.type_base.tp_dict, "__new__", new_fun) != 0)
            PyErr_Clear();
        Py_XDECREF(new_fun);
    }

    return (PyObject*)result;
} /* ldmud_concrete_lwobject_type_create() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_any_lwobject_type_subscript (PyTypeObject* lwobject_type, PyObject *key)

/* Create a type object that represents a named lightweight object type.
 */

{
    string_t *name = python_string_to_string(key);
    if (name != NULL)
    {
        lpctype_t *obtype = get_lwobject_type(name);

        free_mstring(name);
        if (!obtype)
        {
            PyErr_SetString(PyExc_MemoryError, "out of memory");
            return NULL;
        }

        return ldmud_concrete_lwobject_type_create(obtype);
    }

    PyErr_SetString(PyExc_TypeError, "key must be a string");
    return NULL;
} /* ldmud_any_lwobject_type_subscript() */

/*-------------------------------------------------------------------------*/
static PyMappingMethods ldmud_any_lwobject_type_as_mapping = {
    0,                                             /*mp_length*/
    (binaryfunc)ldmud_any_lwobject_type_subscript, /*mp_subscript*/
    0,                                             /*mp_ass_subscript*/
};

static PyTypeObject ldmud_any_lwobject_type_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.LWObjectType",               /* tp_name */
    sizeof(ldmud_lpctype_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    &ldmud_any_lwobject_type_as_mapping,/* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC object type",                  /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &ldmud_lpctype_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
/* Integer */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_integer_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Integer.
 */

{
    return lpctype_int;
} /* ldmud_integer_get_lpctype() */

/*-------------------------------------------------------------------------*/

static ldmud_lpctype_t ldmud_integer_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.Integer",                    /* tp_name */
    0,                                  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC integer",                      /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &PyLong_Type,                       /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
},  ldmud_integer_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
/* Float */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_float_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Float.
 */

{
    return lpctype_float;
} /* ldmud_float_get_lpctype() */

/*-------------------------------------------------------------------------*/

static ldmud_lpctype_t ldmud_float_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.Float",                      /* tp_name */
    0,                                  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC float",                        /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &PyFloat_Type,                      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
},  ldmud_float_get_lpctype             /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
/* String */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_string_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.String.
 */

{
    return lpctype_string;
} /* ldmud_string_get_lpctype() */

/*-------------------------------------------------------------------------*/

static ldmud_lpctype_t ldmud_string_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.String",                     /* tp_name */
    0,                                  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC string",                       /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &PyUnicode_Type,                    /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
},  ldmud_string_get_lpctype            /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
/* Bytes */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_bytes_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Bytes.
 */

{
    return lpctype_bytes;
} /* ldmud_bytes_get_lpctype() */

/*-------------------------------------------------------------------------*/

static ldmud_lpctype_t ldmud_bytes_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.Bytes",                     /* tp_name */
    0,                                  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC bytes",                       /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    &PyBytes_Type,                      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
},  ldmud_bytes_get_lpctype            /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
/* Programs (Regular and Lightweight Objects) */

/*-------------------------------------------------------------------------*/
static bool
ldmud_program_check_available (ldmud_program_t* self)

/* Helper function to check whether the object is set and not swapped out.
 * Returns true if so, false otherwise. Sets a Python error if false.
 */

{
    switch (self->lpc_object.type)
    {
        case T_NUMBER:
            PyErr_Format(PyExc_ValueError, "empty object given");
            return false;

        case T_OBJECT:
            /* Make the program resident */
            if (!check_object(self->lpc_object.u.ob))
            {
                PyErr_Format(PyExc_ValueError, "object is destructed");
                return false;
            }
            if (O_PROG_SWAPPED(self->lpc_object.u.ob))
            {
                self->lpc_object.u.ob->time_of_ref = current_time;
                if (load_ob_from_swap(self->lpc_object.u.ob) < 0)
                {
                    PyErr_Format(PyExc_MemoryError, "out of memory when unswapping object '%s'", get_txt(self->lpc_object.u.ob->name));
                    return false;
                }
            }
            /* Update the program entry. */
            self->lpc_program = self->lpc_object.u.ob->prog;

            return true;

        case T_LWOBJECT:
            return true;
    }

    PyErr_Format(PyExc_ValueError, "unknown object type");
    return false;
} /* ldmud_program_check_available() */

/*-------------------------------------------------------------------------*/
static int
ldmud_program_bool(ldmud_program_t *self)

/* Return 0 (false) for destructed objects, 1 (true) for normal objects.
 */

{
    switch (self->lpc_object.type)
    {
        case T_OBJECT:
            return check_object(self->lpc_object.u.ob) != NULL;

        case T_LWOBJECT:
            return true;

        case T_NUMBER:
        default:
            return false;
    }
} /* ldmud_program_bool() */

/*-------------------------------------------------------------------------*/
static void
ldmud_program_dealloc (ldmud_program_t* self)

/* Destroy the ldmud_program_t object
 */

{
    free_svalue(&(self->lpc_object));
    remove_gc_object(&gc_program_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_program_dealloc() */

/*-------------------------------------------------------------------------*/
static bool
ldmud_program_register_replace_program_protector (ldmud_program_and_index_t* ref)

/* Helper function to check whether the object has replace_program()
 * in progress. If so, adds the object to the protector.
 * Returns true on success, false otherwise (sets a Python error in this case).
 */

{
    object_t* ob;

    if (ref->ob_base.lpc_object.type != T_OBJECT)
        return true;

    ob = ref->ob_base.lpc_object.u.ob;
    if (!ob)
        return true;

    if (ob->prog->flags & P_REPLACE_ACTIVE)
    {
        /* There will be a replace_program() in the backend cycle.
         * Create a protector, so we can adjust the indices afterwards.
         */
        for (replace_ob_t* r_ob = obj_list_replace; r_ob; r_ob = r_ob->next)
        {
            struct python_replace_program_protector *prpp;

            if (r_ob->ob != ob)
                continue;

            prpp = xalloc(sizeof(struct python_replace_program_protector));
            if (!prpp)
            {
                PyErr_SetString(PyExc_MemoryError, "out of memory when creating replace_program() protector");
                return false;
            }

            Py_INCREF(ref);
            prpp->ref = (PyObject*)ref;
            prpp->next = r_ob->python_rpp;
            r_ob->python_rpp = prpp;
            return true;
        }
    }

    /* Prevent replace_program(). */
    ob->flags |= O_LAMBDA_REFERENCED;

    return true;
} /* ldmud_program_register_replace_program_protector() */

/*-------------------------------------------------------------------------*/
static void
ldmud_program_lfun_argument_dealloc (ldmud_program_lfun_argument_t* self)

/* Destroy the ldmud_program_lfun_argument_t object
 */

{
    Py_XDECREF(self->type);
    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_program_lfun_argument_dealloc() */

/*-------------------------------------------------------------------------*/
static PyMemberDef ldmud_program_lfun_argument_members[] = {
    { "position", PYTHON_SMT_INT,       offsetof(ldmud_program_lfun_argument_t, position), READONLY, "Position of the argument" },
    { "type",     PYTHON_SMT_OBJECT_EX, offsetof(ldmud_program_lfun_argument_t, type),     READONLY, "Type of the argument"     },
    { "flags",    PYTHON_SMT_INT,       offsetof(ldmud_program_lfun_argument_t, flags),    READONLY, "Flags of the argument"    },
    { NULL }
};

static PyTypeObject ldmud_program_lfun_argument_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.LfunArgument",               /* tp_name */
    sizeof(ldmud_program_lfun_argument_t), /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_program_lfun_argument_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC lfun argument",                /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    ldmud_program_lfun_argument_members,/* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_program_lfun_repr (ldmud_program_and_index_t *val)

/* Return a string representation of this lfun.
 */

{
    function_t* fun;

    if (val->ob_base.lpc_object.type == T_NUMBER)
        return PyUnicode_FromString("<LPC vanished lfun>");

    if (!ldmud_program_check_available(&val->ob_base))
        return NULL;

    fun = get_function_header(val->ob_base.lpc_program, val->index);

    if (val->ob_base.lpc_object.type == T_OBJECT)
        return PyUnicode_FromFormat("<LPC lfun /%s->%s()>", get_txt(val->ob_base.lpc_object.u.ob->name), get_txt(fun->name));
    else
        return PyUnicode_FromFormat("<LPC lfun /%s->%s()>", get_txt(val->ob_base.lpc_program->name), get_txt(fun->name));
} /* ldmud_program_lfun_repr() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_program_and_index_hash (ldmud_program_and_index_t *val)

/* Return a hash of the object and index.
 */

{
    void *ptr;
    switch (val->ob_base.lpc_object.type)
    {
        case T_OBJECT:
            ptr = (void*)val->ob_base.lpc_object.u.ob;
            break;

        case T_LWOBJECT:
            ptr = (void*)val->ob_base.lpc_object.u.lwob;
            break;

        case T_NUMBER:
        default:
            ptr = NULL;
            break;
    }
    return _Py_HashPointer(ptr) ^ val->index;
} /* ldmud_program_and_index_hash() */

/*-------------------------------------------------------------------------*/
static void
ldmud_program_lfun_call_lfun (int num_arg, ldmud_program_and_index_t* lfun)

/* Helper function for ldmud_program_lfun_call().
 */

{
    switch (lfun->ob_base.lpc_object.type)
    {
        case T_OBJECT:
            call_ob_function_args(lfun->ob_base.lpc_object.u.ob, lfun->index, num_arg);
            break;

        case T_LWOBJECT:
            call_lwob_function_args(lfun->ob_base.lpc_object.u.lwob, lfun->index, num_arg);
            break;
    }
}

static PyObject*
ldmud_program_lfun_call (ldmud_program_and_index_t *lfun, PyObject *arg, PyObject *kw)

/* Call the given lfun.
 */

{
    function_t *fun;

    if(python_is_external ? (!master_ob) : (current_object.type == T_NUMBER))
    {
        PyErr_SetString(PyExc_RuntimeError, "can't call an efun without a current object");
        return NULL;
    }

    if (!ldmud_program_check_available(&lfun->ob_base))
        return NULL;

    fun = get_function_header(lfun->ob_base.lpc_program, lfun->index);
    if (kw != NULL && PyDict_Size(kw) != 0)
    {
        PyErr_Format(PyExc_TypeError, "%.200s() takes no keyword arguments",
                     get_txt(fun->name));
        return NULL;
    }
    else
    {
        svalue_t *sp = inter_sp;
        PyObject *result;
        int num_arg = (int)PyTuple_GET_SIZE(arg);

        if (!(fun->flags & TYPE_MOD_VARARGS))
        {
            if (num_arg < fun->num_arg - fun->num_opt_arg - ((fun->flags & TYPE_MOD_XVARARGS) ? 1 : 0))
            {
                PyErr_Format(PyExc_TypeError, "%.200s() missing %d required positional argument",
                             get_txt(fun->name), fun->num_arg - ((fun->flags & TYPE_MOD_XVARARGS) ? 1 : 0) - num_arg);
                return NULL;
            }
            else if (num_arg > fun->num_arg && !(fun->flags & TYPE_MOD_XVARARGS))
            {
                PyErr_Format(PyExc_TypeError, "%.200s() takes %d positional argument but %d were given",
                             get_txt(fun->name), fun->num_arg, num_arg);
                return NULL;
            }
        }

        /* Put all arguments on the stack. */
        for (int i = 0; i < num_arg; i++)
        {
            const char* err = python_to_svalue(++sp, PyTuple_GetItem(arg, i));
            if (err != NULL)
            {
                PyErr_SetString(PyExc_ValueError, err);
                pop_n_elems(i, sp-1);
                return NULL;
            }
        }

        inter_sp = sp;

        if(call_lpc_secure((CClosureFun)ldmud_program_lfun_call_lfun, num_arg, lfun))
        {
            result = svalue_to_python(inter_sp);
            pop_stack();
        }
        else
            result = NULL;

        return result;
    }

    return NULL;

} /* ldmud_program_lfun_call() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_program_and_index_richcompare (ldmud_program_and_index_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    svalue_t self_ob, other_ob;
    bool result;
    PyObject* resultval;

    if (Py_TYPE(self) != Py_TYPE(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    self_ob = self->ob_base.lpc_object;
    other_ob = ((ldmud_program_and_index_t*)other)->ob_base.lpc_object;

    if(self_ob.type < other_ob.type)
        result = op == Py_LT || op == Py_LE || op == Py_NE;
    else if (self_ob.type > other_ob.type)
        result = op == Py_GT || op == Py_GE || op == Py_NE;
    else if (self_ob.type == T_NUMBER)
        result = op == Py_LE || op == Py_EQ || op == Py_GE;
    else
    {
        string_t *self_name, *other_name;
        lwobject_t *self_lwob = NULL, *other_lwob = NULL;
        object_t *self_rob = NULL, *other_rob = NULL;

        if (self_ob.type == T_OBJECT)
        {
            self_rob = self_ob.u.ob;
            other_rob = other_ob.u.ob;
            self_name = self_rob->name;
            other_name = other_rob->name;
        }
        else
        {
            self_lwob = self_ob.u.lwob;
            other_lwob = other_ob.u.lwob;
            self_name = self_lwob->prog->name;
            other_name = other_lwob->prog->name;
        }

        switch (op)
        {
            case Py_LT:
            {
                int cmp = mstring_compare(self_name, other_name);
                if (cmp != 0)
                {
                    result = cmp < 0;
                    break;
                }

                if (self_lwob != other_lwob)
                {
                    result = self_lwob < other_lwob;
                    break;
                }

                result = (self->index < ((ldmud_program_and_index_t*)other)->index);
                break;
            }

            case Py_LE:
            {
                int cmp = mstring_compare(self_name, other_name);
                if (cmp != 0)
                {
                    result = cmp <= 0;
                    break;
                }

                if (self_lwob != other_lwob)
                {
                    result = self_lwob <= other_lwob;
                    break;
                }

                result = (self->index <= ((ldmud_program_and_index_t*)other)->index);
                break;
            }

            case Py_EQ:
                 result = (self_rob == other_rob && self_lwob == other_lwob && self->index == ((ldmud_program_and_index_t*)other)->index);
                 break;

            case Py_NE:
                 result = (self_rob != other_rob || self_lwob != other_lwob || self->index != ((ldmud_program_and_index_t*)other)->index);
                 break;

            case Py_GT:
            {
                int cmp = mstring_compare(self_name, other_name);
                if (cmp != 0)
                {
                    result = cmp > 0;
                    break;
                }

                if (self_lwob != other_lwob)
                {
                    result = self_lwob > other_lwob;
                    break;
                }

                result = (self->index > ((ldmud_program_and_index_t*)other)->index);
                break;
            }

            case Py_GE:
            {
                int cmp = mstring_compare(self_name, other_name);
                if (cmp != 0)
                {
                    result = cmp >= 0;
                    break;
                }

                if (self_lwob != other_lwob)
                {
                    result = self_lwob >= other_lwob;
                    break;
                }

                result = (self->index >= ((ldmud_program_and_index_t*)other)->index);
                break;
            }

            default:
            {
                Py_INCREF(Py_NotImplemented);
                return Py_NotImplemented;
            }
        }
    }

    resultval = result ? Py_True : Py_False;
    Py_INCREF(resultval);
    return resultval;
} /* ldmud_program_and_index_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_lfun_get_name (ldmud_program_and_index_t *lfun, void *closure)

/* Return the name for the lfun.
 */

{
    function_t* fun;

    if (!ldmud_program_check_available(&lfun->ob_base))
        return NULL;

    fun = get_function_header(lfun->ob_base.lpc_program, lfun->index);
    return PyUnicode_FromStringAndSize(get_txt(fun->name), mstrsize(fun->name));
} /* ldmud_program_lfun_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_lfun_get_program_name (ldmud_program_and_index_t *lfun, void *closure)

/* Return the program of this lfun.
 */

{
    const program_t *progp;

    if (!ldmud_program_check_available(&lfun->ob_base))
        return NULL;

    get_function_header_extended(lfun->ob_base.lpc_program, lfun->index, &progp, NULL);

    return PyUnicode_FromFormat("/%s", get_txt(progp->name));
} /* ldmud_program_lfun_get_program_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_lfun_get_file_name (ldmud_program_and_index_t *lfun, void *closure)

/* Return the file name for the lfun.
 */

{
    program_t *progp;
    int fx;
    function_t *header;
    bytecode_p funstart;
    string_t *name, *fname;

    if (!ldmud_program_check_available(&lfun->ob_base))
        return NULL;

    header = get_function_header_extended(lfun->ob_base.lpc_program, lfun->index, (const program_t**) &progp, &fx);
    funstart = progp->program + (progp->functions[fx] & FUNSTART_MASK);
    if (is_undef_function(header, funstart))
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    get_line_number(funstart, progp, &name, &fname);
    free_mstring(name);
    if (fname == NULL)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyUnicode_FromFormat("/%s", get_txt(fname));
} /* ldmud_program_lfun_get_file_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_lfun_get_line_number (ldmud_program_and_index_t *lfun, void *closure)

/* Return the starting line number for the lfun.
 */

{
    program_t *progp;
    int fx, pos;
    function_t *header;
    bytecode_p funstart;
    string_t *name;

    if (!ldmud_program_check_available(&lfun->ob_base))
        return NULL;

    header = get_function_header_extended(lfun->ob_base.lpc_program, lfun->index, (const program_t**) &progp, &fx);
    funstart = progp->program + (progp->functions[fx] & FUNSTART_MASK);
    if (is_undef_function(header, funstart))
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    pos = get_line_number(funstart, progp, &name, NULL);
    free_mstring(name);

    if (pos == 0)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyLong_FromLong(pos);
} /* ldmud_program_lfun_get_line_number() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_lfun_get_arguments (ldmud_program_and_index_t *lfun, void *closure)

/* Return the argument list for the lfun.
 */

{
    const program_t *progp;
    function_t *fun;
    int fx;
    PyObject* result;

    if (!ldmud_program_check_available(&lfun->ob_base))
        return NULL;

    fun = get_function_header_extended(lfun->ob_base.lpc_program, lfun->index, &progp, &fx);
    result = PyList_New(fun->num_arg);
    if (result == NULL)
        return result;

    for (int i = 0; i < fun->num_arg; i++)
    {
        ldmud_program_lfun_argument_t* arg = (ldmud_program_lfun_argument_t*)ldmud_program_lfun_argument_type.tp_alloc(&ldmud_program_lfun_argument_type, 0);
        if (arg == NULL)
        {
            Py_DECREF(result);
            return NULL;
        }

        arg->position = i+1;
        arg->flags = (fun->flags & TYPE_MOD_XVARARGS) ? LFUN_ARGUMENT_FLAG_VARARGS : 0;
        arg->type = NULL;

        if (progp->argument_types && progp->type_start && progp->type_start[fx] != INDEX_START_NONE)
            arg->type = lpctype_to_pythontype(progp->types[progp->argument_types[progp->type_start[fx] + i]]);

        PyList_SET_ITEM(result, i, (PyObject*)arg);
    }

    return result;

} /* ldmud_program_lfun_get_arguments() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_lfun_get_return_type (ldmud_program_and_index_t *lfun, void *closure)

/* Return the return type for the lfun.
 */

{
    function_t *fun;
    PyObject* result;

    if (!ldmud_program_check_available(&lfun->ob_base))
        return NULL;

    fun = get_function_header(lfun->ob_base.lpc_program, lfun->index);
    result = lpctype_to_pythontype(fun->type);
    if (!result)
        PyErr_Format(PyExc_AttributeError, "Function '%s' has no type information or mixed return type", fun->name);

    return result;

} /* ldmud_object_lfun_get_return_type() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_lfun_get_flags (ldmud_program_and_index_t *lfun, void *closure)

/* Return the flags for the lfun.
 */

{
    funflag_t flags;

    if (!ldmud_program_check_available(&lfun->ob_base))
        return NULL;

    flags = lfun->ob_base.lpc_program->functions[lfun->index];

    return PyLong_FromLong(
          ((flags & TYPE_MOD_STATIC)     ? LFUN_FLAG_STATIC     : 0)
        | ((flags & TYPE_MOD_NO_MASK)    ? LFUN_FLAG_NOMASK     : 0)
        | ((flags & TYPE_MOD_VARARGS)    ? LFUN_FLAG_VARARGS    : 0)
        | ((flags & TYPE_MOD_VIRTUAL)    ? LFUN_FLAG_VIRTUAL    : 0)
        | ((flags & TYPE_MOD_DEPRECATED) ? LFUN_FLAG_DEPRECATED : 0));

} /* ldmud_program_lfun_get_flags() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_lfun_get_visibility (ldmud_program_and_index_t *lfun, void *closure)

/* Return the visibility for the lfun.
 */

{
    funflag_t flags;

    if (!ldmud_program_check_available(&lfun->ob_base))
        return NULL;

    flags = lfun->ob_base.lpc_program->functions[lfun->index];

    return PyLong_FromLong(
        (flags & TYPE_MOD_PRIVATE)   ? VISIBILITY_PRIVATE :
        (flags & TYPE_MOD_PROTECTED) ? VISIBILITY_PROTECTED :
        (flags & TYPE_MOD_PUBLIC)    ? VISIBILITY_PUBLIC :
                                       VISIBILITY_VISIBLE);
} /* ldmud_program_lfun_get_visibility() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_program_lfun_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    (inquiry)ldmud_program_bool,        /* nb_bool */
};

static PyGetSetDef ldmud_program_lfun_getset [] = {
    {"name",        (getter)ldmud_program_lfun_get_name,        NULL, NULL},
    {"program_name",(getter)ldmud_program_lfun_get_program_name,NULL, NULL},
    {"file_name",   (getter)ldmud_program_lfun_get_file_name,   NULL, NULL},
    {"line_number", (getter)ldmud_program_lfun_get_line_number, NULL, NULL},
    {"arguments",   (getter)ldmud_program_lfun_get_arguments,   NULL, NULL},
    {"return_type", (getter)ldmud_program_lfun_get_return_type, NULL, NULL},
    {"flags",       (getter)ldmud_program_lfun_get_flags,       NULL, NULL},
    {"visibility",  (getter)ldmud_program_lfun_get_visibility,  NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_program_lfun_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Lfun",                       /* tp_name */
    sizeof(ldmud_program_and_index_t),  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_program_dealloc,  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_program_lfun_repr,  /* tp_repr */
    &ldmud_program_lfun_as_number,      /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_program_and_index_hash, /* tp_hash  */
    (ternaryfunc)ldmud_program_lfun_call,   /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC lfun",                         /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_program_and_index_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_program_lfun_getset,          /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_program_lfun_create (svalue_t ob, program_t *prog, unsigned short index)

/* Create an ldmud.Lfun object for the given program and function index.
 */

{
    ldmud_program_and_index_t* lfun = (ldmud_program_and_index_t*)ldmud_program_lfun_type.tp_alloc(&ldmud_program_lfun_type, 0);
    if (lfun == NULL)
        return NULL;

    lfun->ob_base.lpc_program = prog;
    lfun->ob_base.lpc_object = ob;
    if (ob.type == T_OBJECT)
        ref_object(ob.u.ob, "ldmud_program_lfun_create");
    else
        ref_lwobject(ob.u.lwob);
    lfun->index = index;

    add_gc_object(&gc_program_list, (ldmud_gc_var_t*)lfun);
    if (!ldmud_program_register_replace_program_protector(lfun))
    {
        Py_DECREF(lfun);
        return NULL;
    }
    return (PyObject*) lfun;
} /* ldmud_program_lfun_create() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_functions_iter_next (ldmud_program_and_index_t *self)

/* Returns the next lfun for this iterator and update the iterator.
 */

{
    int idx;

    if (!ldmud_program_check_available(&self->ob_base))
        return NULL;

    idx = self->index+1;
    if (idx >= self->ob_base.lpc_program->num_function_names)
        return NULL;

    self->index = idx;
    return ldmud_program_lfun_create(self->ob_base.lpc_object, self->ob_base.lpc_program, idx);
} /* ldmud_program_functions_iter_next() */

/*-------------------------------------------------------------------------*/
static PyTypeObject ldmud_program_functions_iter_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.FunctionsIter",              /* tp_name */
    sizeof(ldmud_program_and_index_t),  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_program_dealloc,  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC functions iterator",           /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    PyObject_SelfIter,                  /* tp_iter */
    (iternextfunc)ldmud_program_functions_iter_next, /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_program_functions_repr (ldmud_program_t *val)

/* Return a string representation of this object.
 */

{
    switch (val->lpc_object.type)
    {
        case T_OBJECT:
            return PyUnicode_FromFormat("<LPC functions of /%s>", get_txt(val->lpc_object.u.ob->name));

        case T_LWOBJECT:
            return PyUnicode_FromFormat("<LPC functions of /%s>", get_txt(val->lpc_object.u.lwob->prog->name));

        case T_NUMBER:
        default:
            return PyUnicode_FromString("<LPC empty function list>");
    }
} /* ldmud_program_functions_repr() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_program_functions_getattro (ldmud_program_t *val, PyObject *name)

/* Return a function of the function list.
 */

{
    PyObject *result;
    bool error;
    string_t* funname;

    /* First check real attributes... */
    result = PyObject_GenericGetAttr((PyObject *)val, name);
    if (result || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return result;

    if (!val->lpc_program)
        return NULL;

    PyErr_Clear();

    /* And now search for a function. */
    funname = find_tabled_python_string(name, "function name", &error);
    if (error)
        return NULL;

    if (!ldmud_program_check_available(val))
        return NULL;

    if (funname)
    {
        long ix = find_function(funname, val->lpc_program);
        if (ix >= 0)
            return ldmud_program_lfun_create(val->lpc_object, val->lpc_program, ix);
    }

    PyErr_Format(PyExc_AttributeError, "Function '%U' does not exist", name);
    return NULL;
} /* ldmud_program_functions_getattro() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_functions_iter (ldmud_program_t *self)

/* Creates an iterator over all functions.
 */

{
    ldmud_program_and_index_t* iter;

    if (!ldmud_program_check_available(self))
        return NULL;

    iter = (ldmud_program_and_index_t*)ldmud_program_functions_iter_type.tp_alloc(&ldmud_program_functions_iter_type, 0);
    if (iter == NULL)
        return NULL;

    iter->ob_base.lpc_program = self->lpc_program;
    iter->ob_base.lpc_object = self->lpc_object;
    if (self->lpc_object.type == T_OBJECT)
        ref_object(self->lpc_object.u.ob, "ldmud_program_functions_iter");
    else
        ref_lwobject(self->lpc_object.u.lwob);
    iter->index = -1;

    add_gc_object(&gc_program_list, (ldmud_gc_var_t*)iter);
    if (!ldmud_program_register_replace_program_protector(iter))
    {
        Py_DECREF(iter);
        return NULL;
    }
    return (PyObject*) iter;
} /* ldmud_program_functions_iter() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_functions_dir (ldmud_program_t *self)

/* Returns a list of all attributes, this includes all function names.
 */

{
    PyObject *result;
    PyObject *attrs = get_class_dir((PyObject*)self);

    if (attrs == NULL)
        return NULL;

    /* Now add all the functions. */
    if (self->lpc_program)
    {
        program_t *progp = self->lpc_program;

        if (!ldmud_program_check_available(self))
        {
            Py_DECREF(attrs);
            return NULL;
        }

        for (int idx = 0; idx < progp->num_function_names; idx++)
        {
            string_t *fun = get_function_header(progp, progp->function_names[idx])->name;
            PyObject *funname = PyUnicode_FromStringAndSize(get_txt(fun), mstrsize(fun));

            if (funname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PySet_Add(attrs, funname) < 0)
                PyErr_Clear();
            Py_DECREF(funname);
        }
    }

    /* And return the keys of our dict. */
    result = PySequence_List(attrs);
    Py_DECREF(attrs);
    return result;
} /* ldmud_program_functions_dir() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_functions_dict (ldmud_program_t *self, void *closure)

/* Returns a list of all lfuns.
 */

{
    PyObject *result, *dict = PyDict_New();
    if (!dict)
        return NULL;

    if (self->lpc_program)
    {
        program_t *progp = self->lpc_program;

        if (!ldmud_program_check_available(self))
        {
            Py_DECREF(dict);
            return NULL;
        }

        for (int idx = 0; idx < progp->num_function_names; idx++)
        {
            int fx = progp->function_names[idx];
            string_t *fun = get_function_header(progp, fx)->name;
            PyObject *funname = PyUnicode_FromStringAndSize(get_txt(fun), mstrsize(fun));
            PyObject* lfun;

            if (funname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            lfun = ldmud_program_lfun_create(self->lpc_object, self->lpc_program, fx);
            if (lfun == NULL)
            {
                PyErr_Clear();
                Py_DECREF(funname);
                continue;
            }

            if (PyDict_SetItem(dict, funname, lfun) < 0)
                PyErr_Clear();
            Py_DECREF(funname);
            Py_DECREF(lfun);
        }
    }

    result = PyDictProxy_New(dict);
    Py_DECREF(dict);
    return result;
} /* ldmud_program_functions_dict() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_program_functions_methods[] =
{
    {
        "__dir__",
        (PyCFunction)ldmud_program_functions_dir, METH_NOARGS,
        "__dir__() -> List\n\n"
        "Returns a list of all attributes."
    },

    {NULL}
};

static PyGetSetDef ldmud_program_functions_getset [] = {
    {"__dict__", (getter)ldmud_program_functions_dict, NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_program_functions_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Functions",                  /* tp_name */
    sizeof(ldmud_program_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_program_dealloc,  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_program_functions_repr, /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    (getattrofunc)ldmud_program_functions_getattro, /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC function list",                /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    (getiterfunc)ldmud_program_functions_iter, /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_program_functions_methods,    /* tp_methods */
    0,                                  /* tp_members */
    ldmud_program_functions_getset,     /* tp_getset */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_program_variable_repr (ldmud_program_and_index_t *val)

/* Return a string representation of this variable.
 */

{
    variable_t *var;

    if (val->ob_base.lpc_object.type == T_NUMBER)
        return PyUnicode_FromString("<LPC vanished variable>");

    if (!ldmud_program_check_available(&val->ob_base))
        return NULL;

    var = val->ob_base.lpc_program->variables + val->index;
    if (val->ob_base.lpc_object.type == T_OBJECT)
        return PyUnicode_FromFormat("<LPC variable /%s->%s>", get_txt(val->ob_base.lpc_object.u.ob->name), get_txt(var->name));
    else
        return PyUnicode_FromFormat("<LPC variable /%s->%s>", get_txt(val->ob_base.lpc_object.u.lwob->prog->name), get_txt(var->name));

} /* ldmud_program_variable_repr() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_variable_get_name (ldmud_program_and_index_t *varob, void *closure)

/* Return the name for the variable.
 */

{
    variable_t* var;

    if (!ldmud_program_check_available(&varob->ob_base))
        return NULL;

    var = varob->ob_base.lpc_program->variables + varob->index;
    return PyUnicode_FromStringAndSize(get_txt(var->name), mstrsize(var->name));
} /* ldmud_program_variable_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_variable_get_value (ldmud_program_and_index_t *varob, void *closure)

/* Return the value of the variable.
 */

{
    if (!ldmud_program_check_available(&varob->ob_base))
        return NULL;

    if (varob->ob_base.lpc_object.type == T_OBJECT)
    {
        if (O_VAR_SWAPPED(varob->ob_base.lpc_object.u.ob)
         && load_ob_from_swap(varob->ob_base.lpc_object.u.ob) < 0)
        {
            PyErr_Format(PyExc_MemoryError, "out of memory when unswapping object '%s'", get_txt(varob->ob_base.lpc_object.u.ob->name));
            return false;
        }

        return rvalue_to_python(varob->ob_base.lpc_object.u.ob->variables + varob->index);
    }
    else
        return rvalue_to_python(varob->ob_base.lpc_object.u.lwob->variables + varob->index);
} /* ldmud_program_variable_get_value() */

/*-------------------------------------------------------------------------*/
static int
ldmud_program_variable_set_value (ldmud_program_and_index_t *varob, PyObject *newval, void *closure)

/* Sets the value for the variable.
 * Returns 0 on success, -1 on failure.
 */

{
    const char* err;
    program_t* prog;
    svalue_t lpcval;

    if (!ldmud_program_check_available(&varob->ob_base))
        return -1;

    if (newval == NULL)
        newval = Py_None;

    err = python_to_svalue(&lpcval, newval);
    if (err)
    {
        PyErr_SetString(PyExc_ValueError, err);
        return -1;
    }

    prog = varob->ob_base.lpc_program;
    if (prog->flags & P_RTT_CHECKS)
    {
        variable_t* var = varob->ob_base.lpc_program->variables + varob->index;
        lpctype_t *type = var->type.t_type;

        if (!check_rtt_compatibility(type, &lpcval))
        {
            static char realtypebuf[512];
            lpctype_t *realtype = get_rtt_type(type, &lpcval);

            get_lpctype_name_buf(realtype, realtypebuf, sizeof(realtypebuf));
            free_lpctype(realtype);

            free_svalue(&lpcval);

            PyErr_Format(PyExc_TypeError, "bad type for variable assignment to '%s': expected %s, got %s",
                get_txt(var->name), get_lpctype_name(type), realtypebuf);
            return -1;
        }
    }

    if (varob->ob_base.lpc_object.type == T_OBJECT)
    {
        if (O_VAR_SWAPPED(varob->ob_base.lpc_object.u.ob)
         && load_ob_from_swap(varob->ob_base.lpc_object.u.ob) < 0)
        {
            PyErr_Format(PyExc_MemoryError, "out of memory when unswapping object '%s'", get_txt(varob->ob_base.lpc_object.u.ob->name));
            return false;
        }

        transfer_svalue(varob->ob_base.lpc_object.u.ob->variables + varob->index, &lpcval);
    }
    else
        transfer_svalue(varob->ob_base.lpc_object.u.lwob->variables + varob->index, &lpcval);

    return 0;
} /* ldmud_program_variable_set_value() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_variable_get_type (ldmud_program_and_index_t *varob, void *closure)

/* Return the type for the variable.
 */

{
    variable_t* var;
    PyObject* result;

    if (!ldmud_program_check_available(&varob->ob_base))
        return NULL;

    var = varob->ob_base.lpc_program->variables + varob->index;
    result = lpctype_to_pythontype(var->type.t_type);
    if (!result)
        PyErr_Format(PyExc_AttributeError, "Variable '%s' has no type information or mixed type", get_txt(var->name));

    return result;

} /* ldmud_program_variable_get_type() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_variable_get_flags (ldmud_program_and_index_t *varob, void *closure)

/* Return the flags for the variable.
 */

{
    typeflags_t flags;

    if (!ldmud_program_check_available(&varob->ob_base))
        return NULL;

    flags = varob->ob_base.lpc_program->variables[varob->index].type.t_flags;

    return PyLong_FromLong(
          ((flags & TYPE_MOD_STATIC)     ? VARIABLE_FLAG_NOSAVE     : 0)
        | ((flags & TYPE_MOD_NO_MASK)    ? VARIABLE_FLAG_NOMASK     : 0)
        | ((flags & TYPE_MOD_VIRTUAL)    ? VARIABLE_FLAG_VIRTUAL    : 0)
        | ((flags & TYPE_MOD_DEPRECATED) ? VARIABLE_FLAG_DEPRECATED : 0));

} /* ldmud_program_variable_get_flags() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_variable_get_visibility (ldmud_program_and_index_t *varob, void *closure)

/* Return the visibility for the variable.
 */

{
    typeflags_t flags;

    if (!ldmud_program_check_available(&varob->ob_base))
        return NULL;

    flags = varob->ob_base.lpc_program->variables[varob->index].type.t_flags;

    return PyLong_FromLong(
        (flags & TYPE_MOD_PRIVATE)   ? VISIBILITY_PRIVATE :
        (flags & TYPE_MOD_PROTECTED) ? VISIBILITY_PROTECTED :
        (flags & TYPE_MOD_PUBLIC)    ? VISIBILITY_PUBLIC :
                                       VISIBILITY_VISIBLE);
} /* ldmud_program_variable_get_visibility() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_program_variable_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    (inquiry)ldmud_program_bool,        /* nb_bool */
};

static PyGetSetDef ldmud_program_variable_getset [] = {
    {"name",       (getter)ldmud_program_variable_get_name,       NULL,                                     NULL},
    {"value",      (getter)ldmud_program_variable_get_value,      (setter)ldmud_program_variable_set_value, NULL},
    {"type",       (getter)ldmud_program_variable_get_type,       NULL,                                     NULL},
    {"flags",      (getter)ldmud_program_variable_get_flags,      NULL,                                     NULL},
    {"visibility", (getter)ldmud_program_variable_get_visibility, NULL,                                     NULL},
    {NULL}
};

static PyTypeObject ldmud_program_variable_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Variable",                   /* tp_name */
    sizeof(ldmud_program_and_index_t),  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_program_dealloc,  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_program_variable_repr, /* tp_repr */
    &ldmud_program_variable_as_number,  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_program_and_index_hash, /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC object variable",              /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_program_and_index_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_program_variable_getset,      /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_program_variable_create (svalue_t ob, program_t *prog, unsigned short index)

/* Create an ldmud.Variable object for the given program and variable index.
 */

{
    ldmud_program_and_index_t* var = (ldmud_program_and_index_t*)ldmud_program_variable_type.tp_alloc(&ldmud_program_variable_type, 0);
    if (var == NULL)
        return NULL;

    var->ob_base.lpc_program = prog;
    var->ob_base.lpc_object = ob;
    if (ob.type == T_OBJECT)
        ref_object(ob.u.ob, "ldmud_program_variable_create");
    else
        ref_lwobject(ob.u.lwob);
    var->index = index;

    add_gc_object(&gc_program_list, (ldmud_gc_var_t*)var);
    if (!ldmud_program_register_replace_program_protector(var))
    {
        Py_DECREF(var);
        return NULL;
    }
    return (PyObject*) var;
} /* ldmud_program_variable_create() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_variables_iter_next (ldmud_program_and_index_t *self)

/* Returns the next lfun for this iterator and update the iterator.
 */

{
    int idx;

    if (!ldmud_program_check_available(&self->ob_base))
        return NULL;

    idx = self->index+1;
    if (idx >= self->ob_base.lpc_program->num_variables)
        return NULL;

    self->index = idx;
    return ldmud_program_variable_create(self->ob_base.lpc_object, self->ob_base.lpc_program, idx);
} /* ldmud_program_variables_iter_next() */

/*-------------------------------------------------------------------------*/
static PyTypeObject ldmud_program_variables_iter_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.VariablesIter",              /* tp_name */
    sizeof(ldmud_program_and_index_t),  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_program_dealloc,  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC variables iterator",           /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    PyObject_SelfIter,                  /* tp_iter */
    (iternextfunc)ldmud_program_variables_iter_next, /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_program_variables_repr (ldmud_program_t *val)

/* Return a string representation of this object.
 */

{
    switch (val->lpc_object.type)
    {
        case T_OBJECT:
            return PyUnicode_FromFormat("<LPC variables of /%s>", get_txt(val->lpc_object.u.ob->name));

        case T_LWOBJECT:
            return PyUnicode_FromFormat("<LPC variables of /%s>", get_txt(val->lpc_object.u.lwob->prog->name));

        case T_NUMBER:
        default:
            return PyUnicode_FromString("<LPC empty variable list>");
    }

} /* ldmud_program_variables_repr() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_program_variables_getattro (ldmud_program_t *val, PyObject *name)

/* Return a variable of the variable list.
 */

{
    PyObject *result;
    bool error;
    string_t* varname;

    /* First check real attributes... */
    result = PyObject_GenericGetAttr((PyObject *)val, name);
    if (result || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return result;

    if (!val->lpc_program)
        return NULL;

    PyErr_Clear();

    /* And now search for a variable. */
    varname = find_tabled_python_string(name, "variable name", &error);
    if (error)
        return NULL;

    if (!ldmud_program_check_available(val))
        return NULL;

    if (varname)
    {
        program_t *progp = val->lpc_program;
        for (int ix = 0; ix < progp->num_variables; ix++)
            if (progp->variables[ix].name == varname)
                return ldmud_program_variable_create(val->lpc_object, val->lpc_program, ix);
    }

    PyErr_Format(PyExc_AttributeError, "Variable '%U' does not exist", name);
    return NULL;
} /* ldmud_program_variables_getattro() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_variables_iter (ldmud_program_t *self)

/* Creates an iterator over all global variables.
 */

{
    ldmud_program_and_index_t* iter;

    if (!ldmud_program_check_available(self))
        return NULL;

    iter = (ldmud_program_and_index_t*)ldmud_program_variables_iter_type.tp_alloc(&ldmud_program_variables_iter_type, 0);
    if (iter == NULL)
        return NULL;

    iter->ob_base.lpc_program = self->lpc_program;
    iter->ob_base.lpc_object = self->lpc_object;
    if (self->lpc_object.type == T_OBJECT)
        ref_object(self->lpc_object.u.ob, "ldmud_program_variables_iter");
    else
        ref_lwobject(self->lpc_object.u.lwob);
    iter->index = -1;

    add_gc_object(&gc_program_list, (ldmud_gc_var_t*)iter);
    if (!ldmud_program_register_replace_program_protector(iter))
    {
        Py_DECREF(iter);
        return NULL;
    }
    return (PyObject*) iter;
} /* ldmud_program_variables_iter() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_variables_dir (ldmud_program_t *self)

/* Returns a list of all attributes, this includes all variable names.
 */

{
    PyObject *result;
    PyObject *attrs = get_class_dir((PyObject*)self);

    if (attrs == NULL)
        return NULL;

    /* Now add all the variables. */
    if (self->lpc_program)
    {
        program_t *progp = self->lpc_program;

        if (!ldmud_program_check_available(self))
        {
            Py_DECREF(attrs);
            return NULL;
        }

        for (int ix = 0; ix < progp->num_variables; ix++)
        {
            string_t *var = progp->variables[ix].name;
            PyObject *varname = PyUnicode_FromStringAndSize(get_txt(var), mstrsize(var));

            if (varname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PySet_Add(attrs, varname) < 0)
                PyErr_Clear();
            Py_DECREF(varname);
        }
    }

    /* And return the keys of our dict. */
    result = PySequence_List(attrs);
    Py_DECREF(attrs);
    return result;
} /* ldmud_program_variables_dir() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_program_variables_dict (ldmud_program_t *self, void *closure)

/* Returns a list of all variables.
 */

{
    PyObject *result, *dict = PyDict_New();
    if (!dict)
        return NULL;

    if (self->lpc_program)
    {
        program_t *progp;

        if (!ldmud_program_check_available(self))
        {
            Py_DECREF(dict);
            return NULL;
        }

        progp = self->lpc_program;
        for (int ix = 0; ix < progp->num_variables; ix++)
        {
            string_t *var = progp->variables[ix].name;
            PyObject *varname = PyUnicode_FromStringAndSize(get_txt(var), mstrsize(var));
            PyObject* varob;

            if (varname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            varob = ldmud_program_variable_create(self->lpc_object, self->lpc_program, ix);
            if (varob == NULL)
            {
                PyErr_Clear();
                Py_DECREF(varname);
                continue;
            }

            if (PyDict_SetItem(dict, varname, varob) < 0)
                PyErr_Clear();
            Py_DECREF(varname);
            Py_DECREF(varob);
        }
    }

    result = PyDictProxy_New(dict);
    Py_DECREF(dict);
    return result;
} /* ldmud_program_variables_dict() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_program_variables_methods[] =
{
    {
        "__dir__",
        (PyCFunction)ldmud_program_variables_dir, METH_NOARGS,
        "__dir__() -> List\n\n"
        "Returns a list of all attributes."
    },

    {NULL}
};

static PyGetSetDef ldmud_program_variables_getset [] = {
    {"__dict__", (getter)ldmud_program_variables_dict, NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_program_variables_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Variables",                  /* tp_name */
    sizeof(ldmud_program_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_program_dealloc,  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_program_variables_repr, /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    (getattrofunc)ldmud_program_variables_getattro, /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC variable list",                /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    (getiterfunc)ldmud_program_variables_iter, /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_program_variables_methods,    /* tp_methods */
    0,                                  /* tp_members */
    ldmud_program_variables_getset,     /* tp_getset */
};

/*-------------------------------------------------------------------------*/
/* Objects */

/*-------------------------------------------------------------------------*/
static void
ldmud_object_dealloc (ldmud_object_t* self)

/* Destroy the ldmud_object_t object
 */

{
    if(self->lpc_object)
        free_object(self->lpc_object, "ldmud_object_dealloc");

    remove_gc_object(&gc_object_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_object_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_object_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_object_t, i.e. allocate and initialize
 * the object with null values.
 */

{
    ldmud_object_t *self;

    self = (ldmud_object_t *)type->tp_alloc(type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_object = NULL;
    add_gc_object(&gc_object_list, (ldmud_gc_var_t*)self);
    return (PyObject *)self;
} /* ldmud_object_new() */

/*-------------------------------------------------------------------------*/
static void
ldmud_object_init_getobject (int num_arg UNUSED, struct ldmud_object_init_closure_s* data)

/* Helper function for ldmud_object_init().
 */

{
    data->ob = get_object(data->filename);
} /* ldmud_object_init_getobject() */

static int
ldmud_object_init (ldmud_object_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_object_t, i.e. create a new object object
 * from the given arguments.
 */

{
    /* We expect the filename (object to load)
     */

    static char *kwlist[] = { "filename", NULL};

    Py_ssize_t length;
    const char *filename;
    struct ldmud_object_init_closure_s data = { NULL, NULL };

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "s#", kwlist, &filename, &length))
        return -1;

    data.filename = new_n_unicode_mstring(filename, length);
    if(!data.filename)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return -1;
    }
    else
    {
        /* get_object() can throw errors, we don't want to jump out of this context. */
        call_lpc_secure((CClosureFun)ldmud_object_init_getobject, 0, &data);
        free_mstring(data.filename);

        if(data.ob)
        {
            if(self->lpc_object)
                deref_object(self->lpc_object, "ldmud_object_init");
            self->lpc_object = ref_object(data.ob, "ldmud_object_init");
            return 0;
        }
        else
            return -1;
    }
} /* ldmud_object_init() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_object_repr (ldmud_object_t *val)

/* Return a string representation of this object.
 */

{
    if(!val->lpc_object)
        return PyUnicode_FromString("<LPC empty object>");
    return PyUnicode_FromFormat("<LPC object /%s>", get_txt(val->lpc_object->name));
} /* ldmud_object_repr() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_object_hash (ldmud_object_t *val)

/* Return a hash of this object.
 */

{
    return _Py_HashPointer(val->lpc_object);
} /* ldmud_object_hash() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_object_getattro (ldmud_object_t *val, PyObject *name)

/* Return an attribute of our object.
 */

{
    /* First look at our dictionary. */
    if (val->lpc_object && val->lpc_object->python_dict)
    {
        PyObject *res = PyDict_GetItem((PyObject*) val->lpc_object->python_dict, name);
        if (res != NULL)
        {
            Py_INCREF(res);
            return res;
        }
    }

    /* Forward to the generic function. */
    return PyObject_GenericGetAttr((PyObject*)val, name);

} /* ldmud_object_getattro() */

/*-------------------------------------------------------------------------*/
static int
ldmud_object_setattro (ldmud_object_t *val, PyObject *name, PyObject *value)

/* Set an attribute of our object.
 */

{
    /* First forward to the generic function. */
    int res = PyObject_GenericSetAttr((PyObject*)val, name, value);

    /* If unsuccessful, work an our dictionary. */
    if (res && PyErr_ExceptionMatches(PyExc_AttributeError) && val->lpc_object)
    {
        PyObject *extype, *exvalue, *extraceback;
        PyErr_Fetch(&extype, &exvalue, &extraceback);

        if (value == NULL)
        {
            if (val->lpc_object->python_dict)
                res = PyDict_DelItem((PyObject*)val->lpc_object->python_dict, name);
        }
        else
        {
            if (!val->lpc_object->python_dict)
                val->lpc_object->python_dict = PyDict_New();
            res = PyDict_SetItem((PyObject*)val->lpc_object->python_dict, name, value);
        }

        if (res)
        {
            PyErr_Restore(extype, exvalue, extraceback);
        }
        else
        {
            Py_XDECREF(extype);
            Py_XDECREF(exvalue);
            Py_XDECREF(extraceback);
        }
    }

    return res;

} /* ldmud_object_setattro() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_object_richcompare (ldmud_object_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_object_check(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    return pointer_richcompare(self->lpc_object, ((ldmud_object_t*)other)->lpc_object, op);
} /* ldmud_object_richcompare() */

/*-------------------------------------------------------------------------*/
static int
ldmud_object_bool(ldmud_object_t *val)

/* Return 0 (false) for destructed objects, 1 (true) for normal objects.
 */

{
    return check_object(val->lpc_object) != NULL;
} /* ldmud_object_bool() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_object_get_name (ldmud_object_t *val, void *closure)

/* Return the value for the name member.
 */

{
    if(!val->lpc_object)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyUnicode_FromFormat("/%s", get_txt(val->lpc_object->name));
} /* ldmud_object_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_object_get_program_name (ldmud_object_t *val, void *closure)

/* Return the value for the program_name member.
 */

{
    if(!val->lpc_object)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    if (!check_object(val->lpc_object))
    {
        PyErr_Format(PyExc_ValueError, "object is destructed");
        return NULL;
    }
    if (O_PROG_SWAPPED(val->lpc_object))
    {
        val->lpc_object->time_of_ref = current_time;
        if (load_ob_from_swap(val->lpc_object) < 0)
        {
            PyErr_Format(PyExc_MemoryError, "out of memory when unswapping object '%s'", get_txt(val->lpc_object->name));
            return NULL;
        }
    }

    return PyUnicode_FromFormat("/%s", get_txt(val->lpc_object->prog->name));
} /* ldmud_object_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_object_get_list (ldmud_object_t *val, PyTypeObject *type)

/* Return the value for the functions or variables member.
 */

{
    ldmud_program_t *result;

    if(!val->lpc_object)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    result = (ldmud_program_t*)type->tp_alloc(type, 0);
    if (result == NULL)
        return NULL;

    result->lpc_program = val->lpc_object->prog;
    put_ref_object(&(result->lpc_object), val->lpc_object, "ldmud_object_get_list");
    add_gc_object(&gc_program_list, (ldmud_gc_var_t*)result);

    return (PyObject *)result;
} /* ldmud_object_get_list() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_object_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Object.
 */

{
    return lpctype_any_object;
} /* ldmud_object_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_object_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    (inquiry)ldmud_object_bool,         /* nb_bool */
};

static PyMethodDef ldmud_object_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_object_getset[] =
{
    {"name",         (getter)ldmud_object_get_name, NULL, NULL, NULL},
    {"program_name", (getter)ldmud_object_get_program_name, NULL, NULL, NULL},
    {"functions",    (getter)ldmud_object_get_list, NULL, NULL, &ldmud_program_functions_type},
    {"variables",    (getter)ldmud_object_get_list, NULL, NULL, &ldmud_program_variables_type},
    {NULL}
};

static ldmud_lpctype_t ldmud_object_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_any_object_type_type, 0)
    "ldmud.Object",                     /* tp_name */
    sizeof(ldmud_object_t),             /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_object_dealloc,   /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_object_repr,        /* tp_repr */
    &ldmud_object_as_number,            /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_object_hash,        /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    (getattrofunc)ldmud_object_getattro,/* tp_getattro */
    (setattrofunc)ldmud_object_setattro,/* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC object",                       /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_object_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_object_methods,               /* tp_methods */
    0,                                  /* tp_members */
    ldmud_object_getset,                /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_object_init,        /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_object_new,                   /* tp_new */
},  ldmud_object_get_lpctype            /* get_lpctype */
};


/*-------------------------------------------------------------------------*/
static bool
ldmud_object_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC object type.
 */

{
    return Py_TYPE(ob) == &ldmud_object_type.type_base;
} /* ldmud_object_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_object_create (object_t* ob)

/* Creates a new Python object from an LPC object.
 */

{
    ldmud_object_t *self;

    self = (ldmud_object_t *)ldmud_object_type.type_base.tp_alloc(&ldmud_object_type.type_base, 0);
    if (self == NULL)
        return NULL;

    self->lpc_object = ref_object(ob, "ldmud_object_create");
    add_gc_object(&gc_object_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_object_create() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_object_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for a ldmud.Object[name].
 * We ignore the element type here and create a type for generic object instead.
 */

{
    PyObject *result = ldmud_object_new(&ldmud_object_type.type_base, args, kwds);
    if (!result)
        return NULL;

    /* When returning a different type, we need to initialize that also. */
    if (ldmud_object_init((ldmud_object_t*)result, args, kwds) < 0)
    {
        Py_DECREF(result);
        return NULL;
    }

    return result;
} /* ldmud_concrete_object_new() */

/*-------------------------------------------------------------------------*/
/* Lightweight Objects */

/*-------------------------------------------------------------------------*/
static void
ldmud_lwobject_dealloc (ldmud_lwobject_t* self)

/* Destroy the ldmud_lwobject_t object
 */

{
    if(self->lpc_lwobject)
        free_lwobject(self->lpc_lwobject);

    remove_gc_object(&gc_lwobject_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_lwobject_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lwobject_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_lwobject_t, i.e. allocate and initialize
 * the object with null values.
 */

{
    ldmud_lwobject_t *self;

    self = (ldmud_lwobject_t *)type->tp_alloc(type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_lwobject = NULL;
    add_gc_object(&gc_lwobject_list, (ldmud_gc_var_t*)self);
    return (PyObject *)self;
} /* ldmud_lwobject_new() */

/*-------------------------------------------------------------------------*/
struct ldmud_lwobject_init_closure_s
{
    string_t* filename;
    lwobject_t* lwob;
};

static void
ldmud_lwobject_init_getlwobject (int num_arg UNUSED, struct ldmud_lwobject_init_closure_s* data)

/* Helper function for ldmud_lwobject_init().
 */

{
    push_ref_string(inter_sp, data->filename);
    inter_sp = v_new_lwobject(inter_sp, 1);
    if (inter_sp->type == T_LWOBJECT)
    {
        data->lwob = inter_sp->u.lwob;
        inter_sp--;
    }
    else
        pop_stack();
} /* ldmud_lwobject_init_getlwobject() */

static int
ldmud_lwobject_init (ldmud_lwobject_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_lwobject_t, i.e. create a new lwobject object
 * from the given arguments.
 */

{
    /* We expect the filename (blueprint to use)
     */

    static char *kwlist[] = { "filename", NULL};

    Py_ssize_t length;
    const char *filename;
    struct ldmud_lwobject_init_closure_s data = { NULL, NULL };

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "s#", kwlist, &filename, &length))
        return -1;

    data.filename = new_n_unicode_mstring(filename, length);
    if(!data.filename)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return -1;
    }
    else
    {
        /* new_lwobject() can throw errors, we don't want to jump out of this context. */
        call_lpc_secure((CClosureFun)ldmud_lwobject_init_getlwobject, 0, &data);
        free_mstring(data.filename);

        if(data.lwob)
        {
            if(self->lpc_lwobject)
                free_lwobject(self->lpc_lwobject);
            self->lpc_lwobject = data.lwob;
            return 0;
        }
        else
            return -1;
    }
} /* ldmud_lwobject_init() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lwobject_repr (ldmud_lwobject_t *val)

/* Return a string representation of this lwobject.
 */

{
    if(!val->lpc_lwobject)
        return PyUnicode_FromString("<LPC empty lightweight object>");
    return PyUnicode_FromFormat("<LPC lightweight object of /%s>", get_txt(val->lpc_lwobject->prog->name));
} /* ldmud_lwobject_repr() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_lwobject_hash (ldmud_lwobject_t *val)

/* Return a hash of this lwobject.
 */

{
    return _Py_HashPointer(val->lpc_lwobject);
} /* ldmud_lwobject_hash() */

/*-------------------------------------------------------------------------*/
static bool ldmud_lwobject_check(PyObject *ob);

static PyObject*
ldmud_lwobject_richcompare (ldmud_lwobject_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_lwobject_check(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    return pointer_richcompare(self->lpc_lwobject, ((ldmud_lwobject_t*)other)->lpc_lwobject, op);
} /* ldmud_lwobject_richcompare() */

/*-------------------------------------------------------------------------*/
static int
ldmud_lwobject_bool(ldmud_lwobject_t *val)

/* Always return 1 (true), because lightweight objects are always alive.
 */

{
    return 1;
} /* ldmud_lwobject_bool() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lwobject_get_program_name (ldmud_lwobject_t *val, void *closure)

/* Return the value for the program_name member.
 */

{
    if(!val->lpc_lwobject)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyUnicode_FromFormat("/%s", get_txt(val->lpc_lwobject->prog->name));
} /* ldmud_lwobject_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lwobject_get_list (ldmud_lwobject_t *val, PyTypeObject *type)

/* Return the value for the functions or variables member.
 */

{
    ldmud_program_t *result;

    if(!val->lpc_lwobject)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    result = (ldmud_program_t*)type->tp_alloc(type, 0);
    if (result == NULL)
        return NULL;

    result->lpc_program = val->lpc_lwobject->prog;
    put_ref_lwobject(&(result->lpc_object), val->lpc_lwobject);
    add_gc_object(&gc_program_list, (ldmud_gc_var_t*)result);

    return (PyObject *)result;
} /* ldmud_lwobject_get_list() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_lwobject_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.LWObject.
 */

{
    return lpctype_any_lwobject;
} /* ldmud_lwobject_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_lwobject_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    (inquiry)ldmud_lwobject_bool,       /* nb_bool */
};

static PyMethodDef ldmud_lwobject_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_lwobject_getset[] =
{
    {"program_name", (getter)ldmud_lwobject_get_program_name, NULL, NULL, NULL},
    {"functions",    (getter)ldmud_lwobject_get_list, NULL, NULL, &ldmud_program_functions_type},
    {"variables",    (getter)ldmud_lwobject_get_list, NULL, NULL, &ldmud_program_variables_type},
    {NULL}
};

static ldmud_lpctype_t ldmud_lwobject_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_any_lwobject_type_type, 0)
    "ldmud.LWObject",                   /* tp_name */
    sizeof(ldmud_lwobject_t),           /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_lwobject_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_lwobject_repr,      /* tp_repr */
    &ldmud_lwobject_as_number,          /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_lwobject_hash,      /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC lightweight object",           /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_lwobject_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_lwobject_methods,             /* tp_methods */
    0,                                  /* tp_members */
    ldmud_lwobject_getset,              /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_lwobject_init,      /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_lwobject_new,                 /* tp_new */
},  ldmud_lwobject_get_lpctype          /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_lwobject_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC lwobject type.
 */

{
    return Py_TYPE(ob) == &ldmud_lwobject_type.type_base;
} /* ldmud_lwobject_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lwobject_create (lwobject_t* lwob)

/* Creates a new Python lwobject from an LPC lwobject.
 */

{
    ldmud_lwobject_t *self;

    self = (ldmud_lwobject_t *)ldmud_lwobject_type.type_base.tp_alloc(&ldmud_lwobject_type.type_base, 0);
    if (self == NULL)
        return NULL;

    self->lpc_lwobject = ref_lwobject(lwob);
    add_gc_object(&gc_lwobject_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_lwobject_create() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_lwobject_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for a ldmud.Object[name].
 * We ignore the element type here and create a type for generic lwobject instead.
 */

{
    PyObject *result = ldmud_lwobject_new(&ldmud_lwobject_type.type_base, args, kwds);
    if (!result)
        return NULL;

    /* When returning a different type, we need to initialize that also. */
    if (ldmud_lwobject_init((ldmud_lwobject_t*)result, args, kwds) < 0)
    {
        Py_DECREF(result);
        return NULL;
    }

    return result;
} /* ldmud_concrete_lwobject_new() */

/*-------------------------------------------------------------------------*/
/* Arrays */

static bool ldmud_array_check(PyObject *ob);
static PyObject* ldmud_array_create(vector_t *vec);

static void
ldmud_array_dealloc (ldmud_array_t* self)

/* Destroy the ldmud_array_t object
 */

{
    if(self->lpc_array)
        free_array(self->lpc_array);

    remove_gc_object(&gc_array_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_array_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_array_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_array_t, i.e. allocate and initialize
 * the array with null values.
 */

{
    ldmud_array_t *self;

    self = (ldmud_array_t *)type->tp_alloc(type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_array = ref_array(&null_vector);
    add_gc_object(&gc_array_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_array_new() */

/*-------------------------------------------------------------------------*/
static void
python_error_handler (const char *fmt, ...)

/* Error handler for allocate_array, so it won't use errorf().
 */

{
    char fixed_fmt[ERROR_FMT_LEN];
    char msg[ERROR_BUF_LEN];

    va_list va;

    va_start(va, fmt);
    limit_error_format(fixed_fmt, sizeof(fixed_fmt), fmt);
    vsnprintf(msg, sizeof(msg), fixed_fmt, va);
    va_end(va);

    PyErr_SetString(PyExc_MemoryError, msg);
} /* python_error_handler() */
/*-------------------------------------------------------------------------*/

static int
ldmud_array_init (ldmud_array_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_array_t, i.e. create a new array object
 * from the given arguments.
 */

{
    /* For storing the values, before we know their size. */
# define VBUFSIZE 256
    struct value_buffer_s
    {
        svalue_t item[VBUFSIZE];
        struct value_buffer_s* next;
    };

    /* We expect:
     *  - either an iterator for the values
     *  - or a 'size' keyword argument.
     *  - or nothing, then we're done.
     */

    static char *kwlist[] = { "values", "size", NULL};

    PyObject *values = NULL;
    int size = -1;

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "|Oi", kwlist, &values, &size))
        return -1;

    if(values == NULL || size == 0)
    {
        if(size <= 0)
        {
            free_array(self->lpc_array);
            self->lpc_array = ref_array(&null_vector);
        }
        else
        {
            void (*save_handler)(const char *, ...);
            vector_t *vec;

            save_handler = allocate_array_error_handler;
            allocate_array_error_handler = python_error_handler;
            vec = allocate_array_unlimited(size);
            allocate_array_error_handler = save_handler;

            if(vec == NULL)
            {
                PyErr_NoMemory();
                return -1;
            }

            free_array(self->lpc_array);
            self->lpc_array = vec;
        }

        return 0;
    }
    else
    {
        /* We got some values... */
        int count = 0;
        struct value_buffer_s *first = NULL, *current = NULL;

        PyObject *iterator = PyObject_GetIter(values);
        PyObject *item;

        if (!iterator)
            return -1;

        while ((item = PyIter_Next(iterator)))
        {
            const char *err;
            int idx = count % VBUFSIZE;

            if(idx == 0)
            {
                /* We need to allocate a new buffer. */
                struct value_buffer_s *next = xalloc(sizeof(struct value_buffer_s));
                if (next == NULL)
                {
                    PyErr_SetString(PyExc_MemoryError, "out of memory");
                    Py_DECREF(item);
                    break;
                }

                if(first == NULL)
                    first = current = next;
                else
                {
                    current->next = next;
                    current = next;
                }

                current->next = NULL;
            }

            err = python_to_svalue(current->item + idx, item);
            Py_DECREF(item);

            if (err != NULL)
            {
                PyErr_SetString(PyExc_ValueError, err);
                break;
            }

            count++;
            if (size >= 0 && count == size)
                break;
        }

        Py_DECREF(iterator);

        /* PyIter_Next may also set an error. */
        if (!PyErr_Occurred())
        {
            /* New create the final vector and copy all the values there. */
            void (*save_handler)(const char *, ...);
            vector_t *vec;

            save_handler = allocate_array_error_handler;
            allocate_array_error_handler = python_error_handler;
            vec = allocate_array_unlimited(count);
            allocate_array_error_handler = save_handler;

            if(vec != NULL)
            {
                current = NULL;
                for (int i = 0; i < count; i++)
                {
                    int idx = i % VBUFSIZE;
                    if (idx == 0)
                    {
                        if(current == NULL)
                            current = first;
                        else
                            current = current->next;
                    }

                    vec->item[i] = current->item[idx];
                }

                free_array(self->lpc_array);
                self->lpc_array = vec;
            }
            else
                PyErr_NoMemory();
        }

        for (current = first; current; )
        {
            struct value_buffer_s *next = current->next;
            xfree(current);
            current = next;
        }

        return PyErr_Occurred() ? -1 : 0;
    }
} /* ldmud_array_init() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_array_hash (ldmud_array_t *val)

/* Return a hash of this array.
 */

{
    return _Py_HashPointer(val->lpc_array);
} /* ldmud_array_hash() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_array_richcompare (ldmud_array_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_array_check(other))
        Py_RETURN_NOTIMPLEMENTED;

    return pointer_richcompare(self->lpc_array, ((ldmud_array_t*)other)->lpc_array, op);
} /* ldmud_array_richcompare() */

/*-------------------------------------------------------------------------*/
static Py_ssize_t
ldmud_array_length (ldmud_array_t *val)

/* Implement len() for ldmud_array_t.
 */

{
    return VEC_SIZE(val->lpc_array);
} /* ldmud_array_length() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_array_concat (ldmud_array_t *val, PyObject *second)

/* Implement addition for ldmud_array_t.
 */

{

    if (!ldmud_array_check(second))
    {
        PyErr_Format(PyExc_TypeError, "can only concatenate ldmud.Array (not \"%.200s\") to an ldmud.Array",
            Py_TYPE(second)->tp_name);
        return NULL;
    }
    else
    {
        void (*save_handler)(const char *, ...);
        ldmud_array_t *val2 = (ldmud_array_t*) second;
        ldmud_array_t *result;
        vector_t *vec;
        svalue_t *items;
        int size = VEC_SIZE(val->lpc_array) + VEC_SIZE(val2->lpc_array);

        if (size < 0)
            return PyErr_NoMemory();

        result = (ldmud_array_t *)ldmud_array_create(NULL);
        if (result == NULL)
            return NULL;

        save_handler = allocate_array_error_handler;
        allocate_array_error_handler = python_error_handler;
        vec = allocate_array_unlimited(size);
        allocate_array_error_handler = save_handler;

        if (vec == NULL)
        {
            ldmud_array_dealloc(result);
            return PyErr_NoMemory();
        }

        items = vec->item;
        for (int i = 0; i < VEC_SIZE(val->lpc_array); i++)
           assign_svalue_no_free(items + i, val->lpc_array->item + i);

        items = vec->item + VEC_SIZE(val->lpc_array);
        for (int i = 0; i < VEC_SIZE(val2->lpc_array); i++)
           assign_svalue_no_free(items + i, val2->lpc_array->item + i);

        result->lpc_array = vec;
        return (PyObject*)result;
    }
} /* ldmud_array_concat() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_array_repeat (ldmud_array_t *val, Py_ssize_t num)

/* Implement multiplication for ldmud_array_t.
 */

{
    void (*save_handler)(const char *, ...);
    ldmud_array_t *result;
    vector_t *vec;
    svalue_t *items;
    int size = VEC_SIZE(val->lpc_array) * num;

    if (size < 0)
        return PyErr_NoMemory();

    result = (ldmud_array_t *)ldmud_array_create(NULL);
    if (result == NULL)
        return NULL;

    save_handler = allocate_array_error_handler;
    allocate_array_error_handler = python_error_handler;
    vec = allocate_array_unlimited(size);
    allocate_array_error_handler = save_handler;

    if (vec == NULL)
    {
        ldmud_array_dealloc(result);
        return PyErr_NoMemory();
    }

    items = vec->item;
    for (int i = 0; i < num; i++)
    {
        for (int j = 0; j < VEC_SIZE(val->lpc_array); j++)
           assign_svalue_no_free(items++, val->lpc_array->item + j);
    }

    result->lpc_array = vec;
    return (PyObject*)result;
} /* ldmud_array_repeat() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_array_item (ldmud_array_t *val, Py_ssize_t idx)

/* Implement item access for ldmud_array_t.
 */

{
    PyObject *result;
    if (idx < 0 || idx >= VEC_SIZE(val->lpc_array))
    {
        PyErr_SetString(PyExc_IndexError, "index out of range");
        return NULL;
    }

    result = rvalue_to_python(val->lpc_array->item + idx);
    if (result == NULL)
        PyErr_SetString(PyExc_ValueError, "Unsupported data type.");

    return result;
} /* ldmud_array_item() */

/*-------------------------------------------------------------------------*/
static int
ldmud_array_ass_item (ldmud_array_t *val, Py_ssize_t idx, PyObject *v)

/* Implement item access for ldmud_array_t.
 */

{
    const char *err;
    svalue_t sv;

    if (idx < 0 || idx >= VEC_SIZE(val->lpc_array))
    {
        PyErr_SetString(PyExc_IndexError, "index out of range");
        return -1;
    }

    if (v == NULL)
    {
        /* Removal of this element. */
        free_svalue(val->lpc_array->item + idx);

        val->lpc_array->size--;
        for(; idx < VEC_SIZE(val->lpc_array); idx++)
            transfer_svalue_no_free(val->lpc_array->item + idx, val->lpc_array->item + idx + 1);
    }
    else
    {
        err = python_to_svalue(&sv, v);
        if (err != NULL)
        {
            PyErr_SetString(PyExc_ValueError, err);
            return -1;
        }

        transfer_svalue(val->lpc_array->item + idx, &sv);
    }
    return 0;
} /* ldmud_array_ass_item() */

/*-------------------------------------------------------------------------*/
static int
ldmud_array_contains (ldmud_array_t *val, PyObject *v)

/* Implement __contains__ for ldmud_array_t.
 */

{
    for (int i = 0; i < VEC_SIZE(val->lpc_array); i++)
        if (python_eq_svalue(v, val->lpc_array->item + i))
            return 1;
    return 0;
} /* ldmud_array_contains() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_array_subscript (ldmud_array_t *val, PyObject *key)

/* Implement item access for ldmud_array_t.
 */

{
    if (PyIndex_Check(key))
    {
        Py_ssize_t idx = PyNumber_AsSsize_t(key, PyExc_IndexError);
        if (idx == -1 && PyErr_Occurred())
            return NULL;
        if (idx < 0)
            idx += VEC_SIZE(val->lpc_array);

        return ldmud_array_item(val, idx);
    }
    else if (PySlice_Check(key))
    {
        Py_ssize_t start, stop, step, slicelength;

        if (PySlice_GetIndicesEx(key, VEC_SIZE(val->lpc_array),
                         &start, &stop, &step, &slicelength) < 0)
            return NULL;

        if (slicelength <= 0)
            return ldmud_array_create(&null_vector);
        else
        {
            void (*save_handler)(const char *, ...);
            PyObject *result;
            vector_t *vec;

            save_handler = allocate_array_error_handler;
            allocate_array_error_handler = python_error_handler;
            vec = allocate_array_unlimited(slicelength);
            allocate_array_error_handler = save_handler;

            if (vec == NULL)
                return PyErr_NoMemory();

            result = ldmud_array_create(vec);
            free_array(vec);

            if (result == NULL)
                return NULL;

            for (Py_ssize_t cur = start, i = 0; i < slicelength; cur += step, i++)
                assign_svalue(vec->item + i, val->lpc_array->item + cur);

            return result;
        }
    }
    else
    {
        PyErr_Format(PyExc_TypeError, "indices must be integers, not %.200s",
                    key->ob_type->tp_name);
        return NULL;
    }

} /* ldmud_array_subscript() */

/*-------------------------------------------------------------------------*/
static int
ldmud_array_ass_sub (ldmud_array_t *val, PyObject *key, PyObject *value)

/* Implement item access for ldmud_array_t.
 */

{
    if (PyIndex_Check(key))
    {
        Py_ssize_t idx = PyNumber_AsSsize_t(key, PyExc_IndexError);
        if (idx == -1 && PyErr_Occurred())
            return -1;
        if (idx < 0)
            idx += VEC_SIZE(val->lpc_array);

        return ldmud_array_ass_item(val, idx, value);
    }
    else if (PySlice_Check(key))
    {
        Py_ssize_t start, stop, step, slicelength;
        vector_t *replacement;

        if (PySlice_GetIndicesEx(key, VEC_SIZE(val->lpc_array),
                         &start, &stop, &step, &slicelength) < 0)
            return -1;

        if (value == NULL)
        {
            Py_ssize_t src, dest, length, next;

            if (step < 0)
            {
                /* The order is not relevant. */
                Py_ssize_t temp = start;
                start = stop + 1;
                stop = temp + 1;
                step = -step;
            }

            length = VEC_SIZE(val->lpc_array);
            for (src = start, dest = start, next = start; src < length; src++)
            {
                if(src == next && src < stop)
                {
                    free_svalue(val->lpc_array->item + src);
                    next += step;
                }
                else
                {
                    transfer_svalue_no_free(val->lpc_array->item + dest, val->lpc_array->item + src);
                    dest++;
                }
            }
            val->lpc_array->size -= slicelength;
            assert(VEC_SIZE(val->lpc_array) == dest);

            return 0;
        }

        if (!ldmud_array_check(value))
        {
            PyErr_Format(PyExc_TypeError, "can assign ldmud.Array (not \"%.200s\") to an ldmud.Array slice",
                Py_TYPE(value)->tp_name);
            return -1;
        }

        replacement = ((ldmud_array_t*)value)->lpc_array;
        if (VEC_SIZE(replacement) == slicelength && replacement != val->lpc_array)
        {
            /* We can replace the items in-place. */
            for (Py_ssize_t cur = start, i = 0; i < slicelength; cur += step, i++)
                assign_svalue(val->lpc_array->item + cur, replacement->item + i);

            return 0;
        }
        else if (step != 1 && step != -1)
        {
            PyErr_Format(PyExc_ValueError, "attempt to assign array of size %d to extended slice of size %d",
                VEC_SIZE(replacement), slicelength);
            return -1;
        }
        else
        {
            /* We create a new array, consisting of val[0..start-1], replacement, val[stop..<1].
             * If step == -1, then the replacement will be inserted in reverse order.
             */
            Py_ssize_t cur;
            void (*save_handler)(const char *, ...);
            vector_t *vec;

            if (step == -1)
            {
                Py_ssize_t temp = start;
                start = stop + 1;
                stop = temp + 1;
            }

            save_handler = allocate_array_error_handler;
            allocate_array_error_handler = python_error_handler;
            vec = allocate_array_unlimited(start + VEC_SIZE(replacement) + VEC_SIZE(val->lpc_array) - stop);
            allocate_array_error_handler = save_handler;

            if (vec == NULL)
            {
                PyErr_NoMemory();
                return -1;
            }

            cur = 0;
            for (Py_ssize_t i = 0; i < start; i++, cur++)
                assign_svalue_no_free(vec->item + cur, val->lpc_array->item + i);

            if (step == -1)
            {
                for (Py_ssize_t i = VEC_SIZE(replacement); i--; cur++)
                    assign_svalue_no_free(vec->item + cur, replacement->item + i);
            }
            else
            {
                for (Py_ssize_t i = 0; i < VEC_SIZE(replacement); i++, cur++)
                    assign_svalue_no_free(vec->item + cur, replacement->item + i);
            }

            for (Py_ssize_t i = stop; i < VEC_SIZE(val->lpc_array); i++, cur++)
                assign_svalue_no_free(vec->item + cur, val->lpc_array->item + i);

            assert(cur == VEC_SIZE(vec));

            free_array(val->lpc_array);
            val->lpc_array = vec;

            return 0;
        }
    }
    else
    {
        PyErr_Format(PyExc_TypeError, "indices must be integers, not %.200s",
                    key->ob_type->tp_name);
        return -1;
    }
} /* ldmud_array_ass_sub() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_array_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Array.
 */

{
    return lpctype_any_array;
} /* ldmud_array_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PySequenceMethods ldmud_array_as_sequence = {
    (lenfunc)ldmud_array_length,                /* sq_length */
    (binaryfunc)ldmud_array_concat,             /* sq_concat */
    (ssizeargfunc)ldmud_array_repeat,           /* sq_repeat */
    (ssizeargfunc)ldmud_array_item,             /* sq_item */
    0,                                          /* sq_slice */
    (ssizeobjargproc)ldmud_array_ass_item,      /* sq_ass_item */
    0,                                          /* sq_ass_slice */
    (objobjproc)ldmud_array_contains,           /* sq_contains */
    0,                                          /* sq_inplace_concat */
    0,                                          /* sq_inplace_repeat */
};


/* This is needed for slicing. */
static PyMappingMethods ldmud_array_as_mapping = {
    (lenfunc)ldmud_array_length,                /*mp_length*/
    (binaryfunc)ldmud_array_subscript,          /*mp_subscript*/
    (objobjargproc)ldmud_array_ass_sub,         /*mp_ass_subscript*/
};


static PyMethodDef ldmud_array_methods[] =
{
    {NULL}
};

static ldmud_lpctype_t ldmud_array_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_any_array_type_type, 0)
    "ldmud.Array",                      /* tp_name */
    sizeof(ldmud_array_t),              /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_array_dealloc,    /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    &ldmud_array_as_sequence,           /* tp_as_sequence */
    &ldmud_array_as_mapping,            /* tp_as_mapping */
    (hashfunc)ldmud_array_hash,         /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC array",                        /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_array_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_array_methods,                /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_array_init,         /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_array_new,                    /* tp_new */
},  ldmud_array_get_lpctype             /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_array_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC array type.
 */

{
    return Py_TYPE(ob) == &ldmud_array_type.type_base;
} /* ldmud_array_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_array_create (vector_t* vec)

/* Creates a new Python array from an LPC array.
 */

{
    ldmud_array_t *self;

    self = (ldmud_array_t *)ldmud_array_type.type_base.tp_alloc(&ldmud_array_type.type_base, 0);
    if (self == NULL)
        return NULL;

    if(vec != NULL)
        self->lpc_array = ref_array(vec);

    add_gc_object(&gc_array_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_array_create() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_array_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for a ldmud.Array[element type].
 * We ignore the element type here and create a type for generic array instead.
 */

{
    PyObject *result = ldmud_array_new(&ldmud_array_type.type_base, args, kwds);
    if (!result)
        return NULL;

    /* When returning a different type, we need to initialize that also. */
    if (ldmud_array_init((ldmud_array_t*)result, args, kwds) < 0)
    {
        Py_DECREF(result);
        return NULL;
    }

    return result;
} /* ldmud_concrete_array_new() */

/*-------------------------------------------------------------------------*/
/* Mappings */

static bool ldmud_mapping_check(PyObject *ob) __attribute__((unused));
static PyObject* ldmud_mapping_create(mapping_t *vec);

/* This is used by the list itself and the iterator class. */
enum ldmud_mapping_iterator_mode
{
    MappingIterator_Items,
    MappingIterator_Keys,
    MappingIterator_Values,
};

typedef struct ldmud_mapping_list_s ldmud_mapping_list_t;
struct ldmud_mapping_list_s
{
    PyGCObject_HEAD

    mapping_t*                       map;
    vector_t*                        indices;
    int                              pos;    /* Only used for the iterator. */
    enum ldmud_mapping_iterator_mode mode;
};

/*-------------------------------------------------------------------------*/
static void
ldmud_mapping_list_dealloc (ldmud_mapping_list_t* self)

/* Destroy the ldmud_mapping_list_t object
 */

{
    free_mapping(self->map);
    free_array(self->indices);

    remove_gc_object(&gc_mapping_list_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_mapping_list_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_iter_next (ldmud_mapping_list_t* self)

/* Return the next object for this iteration.
 * The state of the iterator is kept and updated in the ldmud_mapping_list_t
 * object.
 */

{
    PyObject *result = NULL;

    if (self->pos >= VEC_SIZE(self->indices))
        return NULL;

    switch (self->mode)
    {
        case MappingIterator_Keys:
            result = rvalue_to_python(self->indices->item + self->pos);
            break;

        case MappingIterator_Items:
        case MappingIterator_Values:
            while (true)
            {
                svalue_t *values = get_map_value(self->map, self->indices->item + self->pos);

                if (values == &const0)
                {
                    /* Key removed? Try next. */
                    self->pos++;
                    if (self->pos >= VEC_SIZE(self->indices))
                        return NULL;

                    continue;
                }

                if (self->mode == MappingIterator_Values)
                    result = rvalue_to_python(values);
                else
                {
                    PyObject *val;

                    result = PyTuple_New(self->map->num_values + 1);
                    if (result == NULL)
                        return NULL;

                    val = rvalue_to_python(self->indices->item + self->pos);
                    if (val == NULL)
                    {
                        Py_DECREF(result);
                        return NULL;
                    }

                    PyTuple_SET_ITEM(result, 0, val);
                    for (int i = 0; i < self->map->num_values; i++)
                    {
                        val = rvalue_to_python(values + i);
                        if (val == NULL)
                        {
                            Py_DECREF(result);
                            return NULL;
                        }

                        PyTuple_SET_ITEM(result, i+1, val);
                    }
                }

                break;
            }
            break;
    }

    self->pos++;
    return result;
} /* ldmud_mapping_iter_next() */

/*-------------------------------------------------------------------------*/
static PyTypeObject ldmud_mapping_iter_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.MappingIter",                /* tp_name */
    sizeof(ldmud_mapping_list_t),       /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_mapping_list_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC mapping iterator",             /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    PyObject_SelfIter,                  /* tp_iter */
    (iternextfunc)ldmud_mapping_iter_next, /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};
/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_mapping_iter_create (mapping_t *map, vector_t *indices, enum ldmud_mapping_iterator_mode mode)

/* Creates a mapping iterator.
 */

{
    ldmud_mapping_list_t *self;

    self = (ldmud_mapping_list_t *)ldmud_mapping_iter_type.tp_alloc(&ldmud_mapping_iter_type, 0);
    if (self == NULL)
        return NULL;

    self->map = ref_mapping(map);
    self->indices = ref_array(indices);
    self->pos = 0;
    self->mode = mode;

    add_gc_object(&gc_mapping_list_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_mapping_iter_create() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_list_iter (ldmud_mapping_list_t* self)

/* Creates an iterator for ourselves.
 */

{
    return ldmud_mapping_iter_create(self->map, self->indices, self->mode);
} /* ldmud_mapping_list_iter() */

/*-------------------------------------------------------------------------*/
static Py_ssize_t
ldmud_mapping_list_length (ldmud_mapping_list_t *val)

/* Implement len() for ldmud_mapping_list_t.
 */

{
    return MAP_SIZE(val->map);
} /* ldmud_mapping_list_length() */

/*-------------------------------------------------------------------------*/
struct ldmud_mapping_list_contains_info_s
{
    PyObject *v;
    bool result;
};

static void
ldmud_mapping_list_contains_walker (svalue_t *key, svalue_t *val, void *extra)

/* Called from ldmud_mapping_list_contains for each element to compare it's
 * value against extra->v.
 */

{
    struct ldmud_mapping_list_contains_info_s *info = (struct ldmud_mapping_list_contains_info_s*)extra;
    if (info->result)
        return;

    if (python_eq_svalue(info->v, val))
        info->result = true;
} /* ldmud_mapping_list_contains_walker() */

/*-------------------------------------------------------------------------*/
static int
ldmud_mapping_list_contains (ldmud_mapping_list_t *val, PyObject *v)

/* Implement __contains__ for ldmud_mapping_list_t.
 */

{

    switch (val->mode)
    {
        case MappingIterator_Items:
        {
            svalue_t sv, *values;

            /* We can only compare against tuples with the correct size. */
            if (!PyTuple_Check(v) || PyTuple_Size(v) != val->map->num_values + 1)
                return 0;

            /* If we can't convert, it's probably not in the mapping. */
            if (python_to_svalue(&sv, PyTuple_GetItem(v,0)) != NULL)
                return 0;

            values = get_map_value(val->map, &sv);
            free_svalue(&sv);

            if(values == &const0)
                return 0;

            /* Now compare the values... */
            for (int i = 0; i < val->map->num_values; i++)
                if (!python_eq_svalue(PyTuple_GetItem(v,i+1), values + i))
                    return 0;

            return 1;
        }

        case MappingIterator_Keys:
        {
            svalue_t sv;
            bool result;

            /* If we can't convert, it's probably not in the mapping. */
            if (python_to_svalue(&sv, v) != NULL)
                return 0;

            result = get_map_value(val->map, &sv) != &const0;
            free_svalue(&sv);

            return result ? 1 : 0;
        }

        case MappingIterator_Values:
        {
            /* We need to walk through the mapping... */
            struct ldmud_mapping_list_contains_info_s info;
            info.v = v;
            info.result = false;

            walk_mapping(val->map, ldmud_mapping_list_contains_walker, &info);
            return info.result ? 1 : 0;
        }
    }

    PyErr_SetString(PyExc_SystemError, "invalid state");
    return -1;

} /* ldmud_mapping_list_contains() */


/*-------------------------------------------------------------------------*/
static PySequenceMethods ldmud_mapping_list_as_sequence = {
    (lenfunc)ldmud_mapping_list_length,         /* sq_length */
    0,                                          /* sq_concat */
    0,                                          /* sq_repeat */
    0,                                          /* sq_item */
    0,                                          /* sq_slice */
    0,                                          /* sq_ass_item */
    0,                                          /* sq_ass_slice */
    (objobjproc)ldmud_mapping_list_contains,    /* sq_contains */
    0,                                          /* sq_inplace_concat */
    0,                                          /* sq_inplace_repeat */
};

static PyTypeObject ldmud_mapping_list_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.MappingList",                /* tp_name */
    sizeof(ldmud_mapping_list_t),       /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_mapping_list_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    &ldmud_mapping_list_as_sequence,    /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC mapping list",                 /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    (getiterfunc)ldmud_mapping_list_iter, /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_mapping_list_create (mapping_t *map, enum ldmud_mapping_iterator_mode mode)

/* Creates a mapping view.
 */

{
    ldmud_mapping_list_t *self;
    void (*save_handler)(const char *, ...);
    vector_t *vec;
    svalue_t *items;

    save_handler = allocate_array_error_handler;
    allocate_array_error_handler = python_error_handler;
    vec = allocate_array_unlimited(MAP_SIZE(map));
    allocate_array_error_handler = save_handler;

    if(vec == NULL)
        return PyErr_NoMemory();

    self = (ldmud_mapping_list_t *)ldmud_mapping_iter_type.tp_alloc(&ldmud_mapping_iter_type, 0);
    if (self == NULL)
    {
        free_array(vec);
        return NULL;
    }

    items = vec->item;
    walk_mapping(map, m_indices_filter, &items);

    self->map = ref_mapping(map);
    self->indices = vec;
    self->pos = 0;
    self->mode = mode;

    add_gc_object(&gc_mapping_list_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_mapping_list_create() */

/*-------------------------------------------------------------------------*/
static void
ldmud_mapping_dealloc (ldmud_mapping_t* self)

/* Destroy the ldmud_mapping_t object
 */

{
    if(self->lpc_mapping)
        free_mapping(self->lpc_mapping);

    remove_gc_object(&gc_mapping_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_mapping_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_mapping_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_mapping_t, i.e. allocate and initialize
 * the mapping as an empty one (with width 1).
 */

{
    ldmud_mapping_t *self;
    mapping_t *map;

    map = allocate_cond_mapping(&default_wizlist_entry, 0, 1);
    if (map == NULL)
        return PyErr_NoMemory();

    self = (ldmud_mapping_t *)type->tp_alloc(type, 0);
    if (self == NULL)
    {
        free_empty_mapping(map);
        return NULL;
    }

    self->lpc_mapping = map;
    add_gc_object(&gc_mapping_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_mapping_new() */

/*-------------------------------------------------------------------------*/

static int
ldmud_mapping_init (ldmud_mapping_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_mapping_t, i.e. create a new mapping object
 * from the given arguments.
 */

{
    /* We expect:
     *  - either an iterator of tuples for the values
     *  - or a 'width' keyword argument.
     *  - or nothing, then we're done.
     */

    static char *kwlist[] = { "values", "width", NULL};

    PyObject *values = NULL;
    int width = 1;

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "|Oi", kwlist, &values, &width))
        return -1;
    if (width < 0)
    {
        PyErr_Format(PyExc_ValueError, "illegal width '%d'", width);
        return -1;
    }

    if(values == NULL)
    {
        mapping_t *map;

        if (self->lpc_mapping->ref == 1 && self->lpc_mapping->num_entries == 0 && self->lpc_mapping->num_values == width)
            return 0; /* Nothing todo. */

        /* Make a new empty mapping... */
        map = allocate_cond_mapping(&default_wizlist_entry, 0, width);
        if (map == NULL)
        {
            PyErr_NoMemory();
            return -1;
        }

        free_mapping(self->lpc_mapping);
        self->lpc_mapping = map;
        return 0;
    }
    else
    {
        /* We got some values... */
        PyObject *iterator;
        PyObject *item;
        mapping_t *map;

        if (PyMapping_Check(values) && !PySequence_Check(values))
        {
            PyObject *dictitems = PyMapping_Items(values);
            if(dictitems != NULL)
            {
                iterator = PyObject_GetIter(dictitems);
                Py_DECREF(dictitems);
            }
            else
                iterator = PyObject_GetIter(values);
        }
        else
            iterator = PyObject_GetIter(values);

        if (!iterator)
            return -1;

        /* The first tuple determines the width. */
        item = PyIter_Next(iterator);
        if (PyErr_Occurred())
        {
            Py_DECREF(iterator);
            return -1;
        }

        if (!item)
        {
            Py_DECREF(iterator);

             /* Empty mapping, same as above. */
            if (self->lpc_mapping->ref == 1 && self->lpc_mapping->num_entries == 0 && self->lpc_mapping->num_values == width)
                return 0; /* Nothing todo. */

            /* Make a new empty mapping... */
            map = allocate_cond_mapping(&default_wizlist_entry, 0, width);
            if (map == NULL)
            {
                PyErr_NoMemory();
                return -1;
            }

            free_mapping(self->lpc_mapping);
            self->lpc_mapping = map;
            return 0;
        }

        if (!PyTuple_Check(item) || PyTuple_Size(item) == 0)
        {
            PyErr_SetString(PyExc_ValueError, "init iterator must return non-empty tuples");
            Py_DECREF(item);
            Py_DECREF(iterator);
            return -1;
        }

        width = PyTuple_Size(item) - 1;
        if (self->lpc_mapping->ref == 1 && self->lpc_mapping->num_entries == 0 && self->lpc_mapping->num_values == width)
            map = self->lpc_mapping;
        else
        {
            map = allocate_cond_mapping(&default_wizlist_entry, 0, width);
            if (map == NULL)
            {
                PyErr_NoMemory();
                Py_DECREF(item);
                Py_DECREF(iterator);
                return -1;
            }
        }

        do
        {
            svalue_t sv, *svalues;
            const char *err;
            PyObject *key;

            if (PyTuple_Size(item) != width + 1)
            {
                PyErr_SetString(PyExc_ValueError, "init iterator must return tuples of the same size");
                Py_DECREF(item);
                break;
            }

            key = PyTuple_GetItem(item, 0);
            if(key == NULL)
            {
                Py_DECREF(item);
                break;
            }

            err = python_to_svalue(&sv, key);
            if (err != NULL)
            {
                Py_DECREF(item);
                PyErr_SetString(PyExc_ValueError, err);
                break;
            }

            svalues = get_map_lvalue_unchecked(map, &sv);
            if (svalues == NULL)
            {
                PyErr_NoMemory();
                Py_DECREF(item);
                break;
            }
            free_svalue(&sv);

            for (int i = 0; i < width; i++)
            {
                err = python_to_svalue(svalues + i, PyTuple_GetItem(item, i + 1));
                if (err != NULL)
                {
                    PyErr_SetString(PyExc_ValueError, err);
                    break;
                }
            }

            Py_DECREF(item);

            if (PyErr_Occurred())
                break;

        } while ((item = PyIter_Next(iterator)));

        Py_DECREF(iterator);

        if (PyErr_Occurred())
        {
            if (map != self->lpc_mapping)
                free_mapping(map);
            return -1;
        }

        if (map != self->lpc_mapping)
        {
            free_mapping(self->lpc_mapping);
            self->lpc_mapping = map;
        }
        return 0;
    }
} /* ldmud_mapping_init() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_mapping_hash (ldmud_mapping_t *val)

/* Return a hash of this array.
 */

{
    return _Py_HashPointer(val->lpc_mapping);
} /* ldmud_mapping_hash() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_mapping_richcompare (ldmud_mapping_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_mapping_check(other))
        Py_RETURN_NOTIMPLEMENTED;

    return pointer_richcompare(self->lpc_mapping, ((ldmud_mapping_t*)other)->lpc_mapping, op);
} /* ldmud_mapping_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_iter (ldmud_mapping_t *self)

/* Returns an iterator over the keys.
 */

{
    PyObject* list = ldmud_mapping_list_create(self->lpc_mapping, MappingIterator_Keys);
    PyObject* iter;

    if (list == NULL)
        return NULL;

    iter = PyObject_GetIter(list);
    Py_DECREF(list);
    return iter;
} /* ldmud_mapping_iter() */

/*-------------------------------------------------------------------------*/
static int
ldmud_mapping_contains (ldmud_mapping_t *val, PyObject *v)

/* Implement __contains__ for ldmud_mapping_t.
 */

{
    svalue_t sv;
    bool result;

    /* If we can't convert, it's probably not in the mapping. */
    if (python_to_svalue(&sv, v) != NULL)
        return 0;

    result = get_map_value(val->lpc_mapping, &sv) != &const0;
    free_svalue(&sv);

    return result ? 1 : 0;
} /* ldmud_mapping_contains() */

/*-------------------------------------------------------------------------*/
static Py_ssize_t
ldmud_mapping_length (ldmud_mapping_t *val)

/* Implement len() for ldmud_mapping_t.
 */

{
    return MAP_SIZE(val->lpc_mapping);
} /* ldmud_mapping_length() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_mapping_subscript (ldmud_mapping_t *val, PyObject *key)

/* Implement item access for ldmud_mapping_t.
 */

{
    PyObject *result;
    svalue_t sv, *values;
    Py_ssize_t idx = 0;

    if (val->lpc_mapping->num_values == 0)
    {
        PyErr_SetString(PyExc_ValueError, "indexing a mapping of width 0");
        return NULL;
    }

    /* If it's a tuple, assume it's (key, index) */
    if (PyTuple_Check(key))
    {
        PyObject* second;

        if(PyTuple_Size(key) != 2)
        {
            PyErr_SetString(PyExc_IndexError, "index should be a 2-tuple");
            return NULL;
        }

        second = PyTuple_GetItem(key, 1);
        if (second==NULL)
            return NULL;
        else if (PyIndex_Check(second))
        {
            idx = PyNumber_AsSsize_t(second, PyExc_IndexError);
            if (idx == -1 && PyErr_Occurred())
                return NULL;

            if (idx < 0 || idx >= val->lpc_mapping->num_values)
            {
                PyErr_SetString(PyExc_IndexError, "index out of range");
                return NULL;
            }

            if(python_to_svalue(&sv, PyTuple_GetItem(key, 0)))
                return PyLong_FromLong(0);
        }
        else if (PySlice_Check(second))
        {
            Py_ssize_t start, stop, step, slicelength;

            if (PySlice_GetIndicesEx(second, val->lpc_mapping->num_values,
                             &start, &stop, &step, &slicelength) < 0)
                return NULL;

            if (slicelength <= 0)
                return ldmud_array_create(&null_vector);
            else
            {
                void (*save_handler)(const char *, ...);
                PyObject *arr_result;
                vector_t *vec;

                save_handler = allocate_array_error_handler;
                allocate_array_error_handler = python_error_handler;
                vec = allocate_array_unlimited(slicelength);
                allocate_array_error_handler = save_handler;

                if (vec == NULL)
                    return PyErr_NoMemory();

                arr_result = ldmud_array_create(vec);
                free_array(vec);

                if (arr_result == NULL)
                    return NULL;

                if(python_to_svalue(&sv, PyTuple_GetItem(key, 0)))
                    values = NULL;
                else
                {
                    values = get_map_value(val->lpc_mapping, &sv);
                    free_svalue(&sv);
                }

                /* The array is zero-initialized, so nothing to do,
                 * when the key isn't found.
                 */
                if (values != NULL && values != &const0)
                {
                    for (Py_ssize_t cur = start, i = 0; i < slicelength; cur += step, i++)
                        assign_svalue(vec->item + i, values + cur);
                }

                return arr_result;
            }
        }
        else
        {
            PyErr_Format(PyExc_IndexError, "second index should be an integer, not %.200s", second->ob_type->tp_name);
            return NULL;
        }
    }
    else if (python_to_svalue(&sv, key) != NULL)
        /* If we can't convert, it's probably not in the mapping. */
        return PyLong_FromLong(0);

    values = get_map_value(val->lpc_mapping, &sv);
    free_svalue(&sv);

    if (values == &const0)
        return PyLong_FromLong(0);

    result = rvalue_to_python(values + idx);
    if (result == NULL)
        PyErr_SetString(PyExc_ValueError, "Unsupported data type.");

    return result;
} /* ldmud_mapping_subscript() */

/*-------------------------------------------------------------------------*/
static int
ldmud_mapping_ass_sub (ldmud_mapping_t *val, PyObject *key, PyObject *value)

/* Implement item access for ldmud_mapping_t.
 */

{
    const char *err;
    svalue_t skey;
    Py_ssize_t idx = 0;

    if (val->lpc_mapping->num_values == 0)
    {
        PyErr_SetString(PyExc_ValueError, "indexing a mapping of width 0");
        return -1;
    }

    /* If it's a tuple, assume it's (key, index) */
    if (PyTuple_Check(key))
    {
        PyObject* second;

        if(PyTuple_Size(key) != 2)
        {
            PyErr_SetString(PyExc_IndexError, "index should be a 2-tuple");
            return -1;
        }

        if (value == NULL)
        {
            PyErr_SetString(PyExc_RuntimeError, "cannot delete sub-key");
            return -1;
        }

        second = PyTuple_GetItem(key, 1);
        if (second==NULL)
            return -1;
        else if (PyIndex_Check(second))
        {
            idx = PyNumber_AsSsize_t(second, PyExc_IndexError);
            if (idx == -1 && PyErr_Occurred())
                return -1;

            if (idx < 0 || idx >= val->lpc_mapping->num_values)
            {
                PyErr_SetString(PyExc_IndexError, "index out of range");
                return -1;
            }

            err = python_to_svalue(&skey, PyTuple_GetItem(key, 0));
        }
        else if (PySlice_Check(second))
        {
            Py_ssize_t start, stop, step, slicelength;
            vector_t *source;
            svalue_t *dest;

            if (PySlice_GetIndicesEx(second, val->lpc_mapping->num_values,
                             &start, &stop, &step, &slicelength) < 0)
                return -1;

            if (!ldmud_array_check(value))
            {
                PyErr_Format(PyExc_TypeError, "can assign ldmud.Array (not \"%.200s\") to an ldmud.Mapping slice",
                    Py_TYPE(value)->tp_name);
                return -1;
            }

            source = ((ldmud_array_t*)value)->lpc_array;
            if (VEC_SIZE(source) != slicelength)
            {
                PyErr_Format(PyExc_ValueError, "attempt to assign array of size %zd to an ldmud.Mapping slice of size %zd",
                    (Py_ssize_t)VEC_SIZE(source), slicelength);
                return -1;
            }

            err = python_to_svalue(&skey, PyTuple_GetItem(key, 0));
            if (err != NULL)
            {
                PyErr_SetString(PyExc_ValueError, err);
                return -1;
            }

            dest = get_map_lvalue_unchecked(val->lpc_mapping, &skey);
            free_svalue(&skey);

            for (Py_ssize_t cur = start, i = 0; i < slicelength; cur += step, i++)
                assign_svalue(dest + cur, source->item + i);

            return 0;
        }
        else
        {
            PyErr_Format(PyExc_IndexError, "second index should be an integer, not %.200s", second->ob_type->tp_name);
            return -1;
        }

    }
    else
        err = python_to_svalue(&skey, key);
    if (err != NULL)
    {
        PyErr_SetString(PyExc_ValueError, err);
        return -1;
    }

    if (value == NULL)
    {
        /* Deletion */
        remove_mapping(val->lpc_mapping, &skey);
        free_svalue(&skey);
    }
    else
    {
        svalue_t sval;
        svalue_t *dest;

        err = python_to_svalue(&sval, value);
        if (err != NULL)
        {
            PyErr_SetString(PyExc_ValueError, err);
            free_svalue(&skey);
            return -1;
        }

        dest = get_map_lvalue_unchecked(val->lpc_mapping, &skey);
        free_svalue(&skey);

        if (dest == NULL)
        {
            free_svalue(&sval);
            PyErr_NoMemory();
            return -1;
        }

        transfer_svalue(dest + idx, &sval);
    }
    return 0;
} /* ldmud_mapping_ass_sub() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_keys (ldmud_mapping_t *self)

/* Returns a list of the keys.
 */

{
    return ldmud_mapping_list_create(self->lpc_mapping, MappingIterator_Keys);
} /* ldmud_mapping_keys() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_items (ldmud_mapping_t *self)

/* Returns a list of items.
 */

{
    return ldmud_mapping_list_create(self->lpc_mapping, MappingIterator_Items);
} /* ldmud_mapping_items() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_values (ldmud_mapping_t *self)

/* Returns a list of values.
 */

{
    return ldmud_mapping_list_create(self->lpc_mapping, MappingIterator_Values);
} /* ldmud_mapping_values() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_get_width (ldmud_mapping_t *val, void *closure)

/* Return the value for the width member.
 */

{
    return PyLong_FromLong(val->lpc_mapping->num_values);
} /* ldmud_mapping_get_width() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_mapping_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Mapping.
 */

{
    return lpctype_mapping;
} /* ldmud_mapping_get_lpctype() */

/*-------------------------------------------------------------------------*/

/* This is needed for 'in' functionality. */
static PySequenceMethods ldmud_mapping_as_sequence = {
    0,                                          /* sq_length */
    0,                                          /* sq_concat */
    0,                                          /* sq_repeat */
    0,                                          /* sq_item */
    0,                                          /* sq_slice */
    0,                                          /* sq_ass_item */
    0,                                          /* sq_ass_slice */
    (objobjproc)ldmud_mapping_contains,         /* sq_contains */
    0,                                          /* sq_inplace_concat */
    0,                                          /* sq_inplace_repeat */
};

static PyMappingMethods ldmud_mapping_as_mapping = {
    (lenfunc)ldmud_mapping_length,              /*mp_length*/
    (binaryfunc)ldmud_mapping_subscript,        /*mp_subscript*/
    (objobjargproc)ldmud_mapping_ass_sub,       /*mp_ass_subscript*/
};


static PyMethodDef ldmud_mapping_methods[] =
{
    {
        "keys",
        (PyCFunction)ldmud_mapping_keys, METH_NOARGS,
        "keys() -> Iterable sequence\n\n"
        "Returns an iterable sequence over all the keys of this mapping."
    },

    {
        "items",
        (PyCFunction)ldmud_mapping_items, METH_NOARGS,
        "items() -> Iterable sequence\n\n"
        "Returns an iterable sequence over all the items\n"
        "(key-value-pairs) of this mapping."
    },
    {
        "values",
        (PyCFunction)ldmud_mapping_values, METH_NOARGS,
        "values() -> Iterable sequence\n\n"
        "Returns an iterable sequence over all the values of this mapping"
    },

    {NULL}
};


static PyGetSetDef ldmud_mapping_getset [] = {
    {"width", (getter)ldmud_mapping_get_width, NULL, NULL},
    {NULL}
};


static ldmud_lpctype_t ldmud_mapping_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.Mapping",                    /* tp_name */
    sizeof(ldmud_mapping_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_mapping_dealloc,  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    &ldmud_mapping_as_sequence,         /* tp_as_sequence */
    &ldmud_mapping_as_mapping,          /* tp_as_mapping */
    (hashfunc)ldmud_mapping_hash,       /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC mapping",                      /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_mapping_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    (getiterfunc)ldmud_mapping_iter,    /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_mapping_methods,              /* tp_methods */
    0,                                  /* tp_members */
    ldmud_mapping_getset,               /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_mapping_init,       /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_mapping_new,                  /* tp_new */
},  ldmud_mapping_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_mapping_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC mapping type.
 */

{
    return Py_TYPE(ob) == &ldmud_mapping_type.type_base;
} /* ldmud_mapping_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_mapping_create (mapping_t* map)

/* Creates a new Python mapping from an LPC mapping.
 */

{
    ldmud_mapping_t *self;

    self = (ldmud_mapping_t *)ldmud_mapping_type.type_base.tp_alloc(&ldmud_mapping_type.type_base, 0);
    if (self == NULL)
        return NULL;

    if (map != NULL)
        self->lpc_mapping = ref_mapping(map);

    add_gc_object(&gc_mapping_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_mapping_create() */

/*-------------------------------------------------------------------------*/
/* Structs */
static bool ldmud_struct_check(PyObject *ob);

static PyObject*
ldmud_struct_member_repr (ldmud_struct_and_index_t *self)

/* Return a string representation of this member.
 */

{
    struct_type_t *st;
    struct_member_t *mem;

    if(self->struct_base.lpc_struct == NULL)
        return PyUnicode_FromString("<LPC uninitialized struct member>");

    st = self->struct_base.lpc_struct->type;
    mem = st->member + self->index;

    return PyUnicode_FromFormat("<LPC struct member /%s::%s.%s>"
                              , get_txt(st->name->prog_name)
                              , get_txt(st->name->name)
                              , get_txt(mem->name));
} /* ldmud_struct_member_repr() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_struct_member_get_name (ldmud_struct_and_index_t *self, void *closure)

/* Return the name for the struct member.
 */

{
    struct_member_t *mem;

    if(self->struct_base.lpc_struct == NULL)
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized struct member");
        return NULL;
    }

    mem = self->struct_base.lpc_struct->type->member + self->index;
    return PyUnicode_FromStringAndSize(get_txt(mem->name), mstrsize(mem->name));
} /* ldmud_struct_member_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_struct_member_get_value (ldmud_struct_and_index_t *self, void *closure)

/* Return the value of the struct member.
 */

{
    PyObject *result;

    if(self->struct_base.lpc_struct == NULL)
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized struct member");
        return NULL;
    }

    result = rvalue_to_python(self->struct_base.lpc_struct->member + self->index);
    if (result == NULL)
        PyErr_SetString(PyExc_ValueError, "Unsupported data type.");

    return result;
} /* ldmud_struct_member_get_value() */

/*-------------------------------------------------------------------------*/
static int
ldmud_struct_member_set_value (ldmud_struct_and_index_t *self, PyObject *newval, void *closure)

/* Sets the value for the struct member.
 * Returns 0 on success, -1 on failure.
 */

{
    const char* err;
    svalue_t lpcval;

    if(self->struct_base.lpc_struct == NULL)
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized struct member");
        return -1;
    }

    if (newval == NULL)
        newval = Py_None;

    err = python_to_svalue(&lpcval, newval);
    if (err)
    {
        PyErr_SetString(PyExc_ValueError, err);
        return -1;
    }

    transfer_svalue(self->struct_base.lpc_struct->member + self->index, &lpcval);

    return 0;
} /* ldmud_struct_member_set_value() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_struct_member_get_type (ldmud_struct_and_index_t *self, void *closure)

/* Return the type for the struct member.
 */

{
    struct_member_t *mem;
    PyObject* result;

    if(self->struct_base.lpc_struct == NULL)
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized struct member");
        return NULL;
    }

    mem = self->struct_base.lpc_struct->type->member + self->index;
    result = lpctype_to_pythontype(mem->type);
    if (!result)
        PyErr_Format(PyExc_AttributeError, "Struct member '%s' has no type information or mixed type", get_txt(mem->name));

    return result;

} /* ldmud_struct_member_get_type() */

/*-------------------------------------------------------------------------*/
static void ldmud_struct_dealloc(ldmud_struct_t* self);

static PyGetSetDef ldmud_struct_member_getset [] = {
    {"name",       (getter)ldmud_struct_member_get_name,       NULL,                                    NULL},
    {"value",      (getter)ldmud_struct_member_get_value,      (setter)ldmud_struct_member_set_value, NULL},
    {"type",       (getter)ldmud_struct_member_get_type,       NULL,                                    NULL},
    {NULL}
};

static PyTypeObject ldmud_struct_member_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.StructMember",               /* tp_name */
    sizeof(ldmud_struct_and_index_t),   /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_struct_dealloc,   /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_struct_member_repr, /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC struct member",                /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_struct_member_getset,         /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_struct_member_create (struct_t* st, int ix)

/* Creates a new Python struct member object for member with index <ix> of <st>.
 */

{
    ldmud_struct_and_index_t *result = (ldmud_struct_and_index_t*)ldmud_struct_member_type.tp_alloc(&ldmud_struct_member_type, 0);
    if (result == NULL)
        return NULL;

    result->struct_base.lpc_struct = ref_struct(st);
    result->index = ix;
    add_gc_object(&gc_struct_list, (ldmud_gc_var_t*)result);

    return (PyObject*)result;
} /* ldmud_struct_member_create() */


/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_struct_members_repr (ldmud_struct_t *self)

/* Return a string representation of this object.
 */

{
    struct_type_t *st;

    if(self->lpc_struct == NULL)
        return PyUnicode_FromString("<LPC uninitialized struct member list>");

    st = self->lpc_struct->type;
    return PyUnicode_FromFormat("<LPC struct members of /%s::%s>"
                              , get_txt(st->name->prog_name)
                              , get_txt(st->name->name));
} /* ldmud_struct_members_repr() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_struct_members_getattro (ldmud_struct_t *self, PyObject *name)

/* Implement __getattr__ for ldmud_struct_t.
 * This is a direct struct member lookup.
 */

{
    PyObject *result;
    bool error;
    string_t* member_name;
    int idx;

    /* First check real attributes... */
    result = PyObject_GenericGetAttr((PyObject *)self, name);
    if (result || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return result;

    if (!self->lpc_struct)
        return NULL;

    PyErr_Clear();

    /* And now search for a member. */
    member_name = find_tabled_python_string(name, "member name", &error);
    if (error)
        return NULL;

    idx = member_name ? struct_find_member(self->lpc_struct->type, member_name) : -1;
    if (idx < 0)
    {
        PyErr_Format(PyExc_AttributeError, "Struct object has no member '%U'", name);
        return NULL;
    }
    else
    {
        ldmud_struct_and_index_t* mem = (ldmud_struct_and_index_t*)ldmud_struct_member_type.tp_alloc(&ldmud_struct_member_type, 0);
        if (mem == NULL)
            return NULL;

        mem->struct_base.lpc_struct = ref_struct(self->lpc_struct);
        mem->index = idx;

        add_gc_object(&gc_struct_list, (ldmud_gc_var_t*)mem);
        return (PyObject*) mem;
    }

} /* ldmud_struct_members_getattro() */

/*-------------------------------------------------------------------------*/
static Py_ssize_t
ldmud_struct_members_length (ldmud_struct_t *self)

/* Implement len() for struct members.
 */

{
    if (self->lpc_struct)
        return self->lpc_struct->type->num_members;
    return 0;
} /* ldmud_struct_members_length() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_struct_members_item (ldmud_struct_t *self, Py_ssize_t idx)

/* Implement item access for struct members.
 */

{
    if (!self->lpc_struct)
    {
        PyErr_SetString(PyExc_IndexError, "uninitialized struct");
        return NULL;
    }
    else
    {
        struct_type_t *st = self->lpc_struct->type;

        if (idx < 0 || idx >= st->num_members)
        {
            PyErr_SetString(PyExc_IndexError, "index out of range");
            return NULL;
        }

        return ldmud_struct_member_create(self->lpc_struct, idx);
    }
} /* ldmud_struct_members_item() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_struct_members_dir (ldmud_struct_t *self)

/* Returns a list of all attributes, this includes all member names.
 */

{
    PyObject *result;
    PyObject *attrs = get_class_dir((PyObject*)self);

    if (attrs == NULL)
        return NULL;

    /* Now add all the members. */
    if (self->lpc_struct)
    {
        struct_type_t *st = self->lpc_struct->type;
        for (int ix = 0; ix < st->num_members; ix++)
        {
            string_t *mem = st->member[ix].name;
            PyObject *memname = PyUnicode_FromStringAndSize(get_txt(mem), mstrsize(mem));

            if (memname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PySet_Add(attrs, memname) < 0)
                PyErr_Clear();
            Py_DECREF(memname);
        }
    }

    /* And return the keys of our dict. */
    result = PySequence_List(attrs);
    Py_DECREF(attrs);
    return result;
} /* ldmud_struct_members_dir() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_struct_members_dict (ldmud_struct_t *self, void *closure)

/* Returns a list of all members.
 */

{
    PyObject *result, *dict = PyDict_New();
    if (!dict)
        return NULL;

    if (self->lpc_struct)
    {
        struct_type_t *st = self->lpc_struct->type;
        for (unsigned short ix = 0; ix < st->num_members; ix++)
        {
            string_t *mem = st->member[ix].name;
            PyObject *memname = PyUnicode_FromStringAndSize(get_txt(mem), mstrsize(mem));
            PyObject* memob;

            if (memname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            memob = ldmud_struct_member_create(self->lpc_struct, ix);
            if (memob == NULL)
            {
                PyErr_Clear();
                Py_DECREF(memname);
                continue;
            }

            if (PyDict_SetItem(dict, memname, memob) < 0)
                PyErr_Clear();
            Py_DECREF(memname);
            Py_DECREF(memob);
        }
    }

    result = PyDictProxy_New(dict);
    Py_DECREF(dict);
    return result;
} /* ldmud_struct_members_dict() */

/*-------------------------------------------------------------------------*/
static PySequenceMethods ldmud_struct_members_as_sequence = {
    (lenfunc)ldmud_struct_members_length,       /* sq_length */
    0,                                          /* sq_concat */
    0,                                          /* sq_repeat */
    (ssizeargfunc)ldmud_struct_members_item,    /* sq_item */
    0,                                          /* sq_slice */
    0,                                          /* sq_ass_item */
    0,                                          /* sq_ass_slice */
    0,                                          /* sq_contains */
    0,                                          /* sq_inplace_concat */
    0,                                          /* sq_inplace_repeat */
};

static PyMethodDef ldmud_struct_members_methods[] =
{
    {
        "__dir__",
        (PyCFunction)ldmud_struct_members_dir, METH_NOARGS,
        "__dir__() -> List\n\n"
        "Returns a list of all attributes."
    },

    {NULL}
};

static PyGetSetDef ldmud_struct_members_getset [] = {
    {"__dict__", (getter)ldmud_struct_members_dict, NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_struct_members_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.StructMembers",              /* tp_name */
    sizeof(ldmud_struct_t),             /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_struct_dealloc,   /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_struct_members_repr,/* tp_repr */
    0,                                  /* tp_as_number */
    &ldmud_struct_members_as_sequence,  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    (getattrofunc)ldmud_struct_members_getattro, /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC struct member list",           /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_struct_members_methods,       /* tp_methods */
    0,                                  /* tp_members */
    ldmud_struct_members_getset,        /* tp_getset */
};

/*-------------------------------------------------------------------------*/
static void
ldmud_struct_dealloc (ldmud_struct_t* self)

/* Destroy the ldmud_struct_t object
 */

{
    if(self->lpc_struct)
        free_struct(self->lpc_struct);

    remove_gc_object(&gc_struct_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_struct_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_struct_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_struct_t, i.e. allocate and initialize
 * the struct with null values.
 */

{
    ldmud_struct_t *self;

    self = (ldmud_struct_t *)type->tp_alloc(type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_struct = NULL;
    add_gc_object(&gc_struct_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_struct_new() */

/*-------------------------------------------------------------------------*/
static bool
ldmud_struct_init_values (ldmud_struct_t *self,  struct_type_t *lpc_struct_type, PyObject *values)

/* Helper function for __init__ to initialize the struct <self> with
 * the given type and values. Returns true for success, false (with
 * a Python exception) otherwise.
 */

{
    /* Create the new struct. */
    struct_t *lpc_struct = struct_new(lpc_struct_type);
    if(!lpc_struct)
    {
        PyErr_SetString(PyExc_MemoryError, "Out of memory");
        return false;
    }

    /* Now let's have a look if there are any members to initialize. */
    if(values)
    {
        int size = struct_t_size(lpc_struct_type);

        if(PyMapping_Check(values) && !PySequence_Check(values))
        {
            /* It is some map-like object. */
            PyObject *items, *item, *iterator;

            if (PyDict_CheckExact(values))
                items = PyDict_Items(values);
            else
                items = PyObject_CallMethod(values, "items", NULL);
            if (items == NULL)
            {
                free_struct(lpc_struct);
                return false;
            }

            iterator = PyObject_GetIter(items);
            Py_DECREF(items);

            if(iterator == NULL)
            {
                free_struct(lpc_struct);
                return false;
            }

            while ((item = PyIter_Next(iterator)))
            {
                const char *err;
                PyObject *key;
                int idx;

                if (!PyTuple_Check(item) || PyTuple_Size(item) != 2)
                {
                    PyErr_SetString(PyExc_TypeError, "dict items iterator must return 2-tuples");
                    Py_DECREF(item);
                    break;
                }

                key = PyTuple_GetItem(item, 0);
                if(key == NULL)
                {
                    Py_DECREF(item);
                    break;
                }

                /* The key can be an index or a member name. */
                if (PyLong_Check(key))
                {
                    int overflow;
                    long num = PyLong_AsLongAndOverflow(key, &overflow);

                    if (overflow || num < PINT_MIN || num > PINT_MAX)
                    {
                        PyErr_SetString(PyExc_TypeError, "integer overflow");
                        Py_DECREF(item);
                        break;
                    }

                    if (num >= (long)size)
                    {
                        PyErr_Format(PyExc_ValueError, "index out of bounds: %ld", num);
                        Py_DECREF(item);
                        break;
                    }

                    idx = (int)num;
                }
                else if (PyUnicode_Check(key))
                {
                    string_t *member_name;
                    bool error;

                    member_name = find_tabled_python_string(key, "member name", &error);
                    if (error)
                    {
                        Py_DECREF(item);
                        break;
                    }

                    idx = member_name ? struct_find_member(lpc_struct_type, member_name) : -1;

                    if (idx < 0)
                    {
                        PyErr_Format(PyExc_ValueError, "member '%U' not found", key);
                        Py_DECREF(item);
                        break;
                    }
                }
                else
                {
                    PyErr_SetString(PyExc_ValueError, "illegal member index for struct initialization, neither int nor string");
                    Py_DECREF(item);
                    break;
                }

                err = python_to_svalue(lpc_struct->member + idx, PyTuple_GetItem(item, 1));
                Py_DECREF(item);

                if (err != NULL)
                {
                    PyErr_SetString(PyExc_ValueError, err);
                    break;
                }
            }

            Py_DECREF(iterator);
        }
        else
        {
            /* Hopefully a sequence. */
            PyObject *iterator = PyObject_GetIter(values);
            PyObject *item;
            int idx = 0;

            if (!iterator)
            {
                /* PyObject_GetIter will set an exception. */
                free_struct(lpc_struct);
                return false;
            }

            while ((item = PyIter_Next(iterator)))
            {
                const char *err;

                if (idx >= size)
                {
                    PyErr_SetString(PyExc_ValueError, "too many elements for struct initialization");
                    Py_DECREF(item);
                    break;
                }

                err = python_to_svalue(lpc_struct->member + (idx++), item);
                Py_DECREF(item);

                if (err != NULL)
                {
                    PyErr_SetString(PyExc_ValueError, err);
                    break;
                }
            }

            Py_DECREF(iterator);
        }

        /* PyIter_Next may set an error. */
        if (PyErr_Occurred())
        {
            free_struct(lpc_struct);
            return false;
        }
    }

    if(self->lpc_struct)
        free_struct(self->lpc_struct);
    self->lpc_struct = lpc_struct;

    return true;
} /* ldmud_struct_init_values() */

/*-------------------------------------------------------------------------*/
static int
ldmud_struct_init (ldmud_struct_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_struct_t, i.e. create a new struct object
 * from the given arguments.
 */

{
    /* We expect:
     *  - the object whose struct definition it is
     *  - the name of the struct
     *  - (optional) the values for the struct, given either as a tuple/list
     *    or map.
     */

    static char *kwlist[] = { "object", "name", "values", NULL};

    PyObject *ob, *values;
    const char* name;
    Py_ssize_t length;

    object_t *lpc_ob;
    string_t *lpc_struct_name;
    struct_type_t *lpc_struct_type;

    values = NULL;
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "O!s#|O", kwlist,
                                      &ldmud_object_type, &ob, &name, &length, &values))
        return -1;

    /* Lookup the object (we really only need the program). */
    lpc_ob = ((ldmud_object_t*)ob)->lpc_object;
    if(!lpc_ob)
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized lpc object");
        return -1;
    }

    if (O_PROG_SWAPPED(lpc_ob) && load_ob_from_swap(lpc_ob) < 0)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory while unswapping");
        return -1;
    }

    /* Lookup the struct type. */
    /* TODO: This lookup also includes NAME_HIDDEN entries, maybe only consider visible structs. */
    lpc_struct_name = find_tabled_str_n(name, length, STRING_UTF8);
    lpc_struct_type = lpc_struct_name ? struct_find(lpc_struct_name, lpc_ob->prog) : NULL;

    if(!lpc_struct_type)
    {
        PyErr_Format(PyExc_NameError, "unknown struct '%s'", name);
        return -1;
    }

    return ldmud_struct_init_values(self, lpc_struct_type, values) ? 0 : -1;
} /* ldmud_struct_init() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_struct_repr (ldmud_struct_t *self)

/* Return a string representation of this object.
 */

{
    struct_type_t *st;

    if(self->lpc_struct == NULL)
        return PyUnicode_FromString("<LPC uninitialized struct>");

    st = self->lpc_struct->type;
    return PyUnicode_FromFormat("<LPC struct /%s::%s>"
                              , get_txt(st->name->prog_name)
                              , get_txt(st->name->name));
} /* ldmud_struct_repr() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_struct_hash (ldmud_struct_t *val)

/* Return a hash of this array.
 */

{
    return _Py_HashPointer(val->lpc_struct);
} /* ldmud_struct_hash() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_struct_richcompare (ldmud_struct_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_struct_check(other))
        Py_RETURN_NOTIMPLEMENTED;

    return pointer_richcompare(self->lpc_struct, ((ldmud_struct_t*)other)->lpc_struct, op);
} /* ldmud_struct_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_struct_get_name (ldmud_struct_t *self, void *closure)

/* Return the value for the name member.
 */

{
    string_t *name;

    if(!self->lpc_struct)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    name = self->lpc_struct->type->name->name;
    return PyUnicode_FromStringAndSize(get_txt(name), mstrsize(name));
} /* ldmud_object_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_struct_get_program_name (ldmud_struct_t *self, void *closure)

/* Return the value for the program_name member.
 */

{
    if(!self->lpc_struct)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyUnicode_FromFormat("/%s", get_txt(self->lpc_struct->type->name->prog_name));
} /* ldmud_object_get_program_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_struct_get_members (ldmud_struct_t *self, void *closure)

/* Return the value for the members member.
 */

{
    ldmud_struct_t *result;

    if(!self->lpc_struct)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    result = (ldmud_struct_t*)ldmud_struct_members_type.tp_alloc(&ldmud_struct_members_type, 0);
    if (result == NULL)
        return NULL;

    result->lpc_struct = ref_struct(self->lpc_struct);
    add_gc_object(&gc_struct_list, (ldmud_gc_var_t*)result);

    return (PyObject *)result;
} /* ldmud_struct_get_members() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_struct_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Struct.
 */

{
    return lpctype_any_struct;
} /* ldmud_struct_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_struct_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_struct_getset[] =
{
    {"name",         (getter)ldmud_struct_get_name,         NULL, NULL, NULL},
    {"program_name", (getter)ldmud_struct_get_program_name, NULL, NULL, NULL},
    {"members",      (getter)ldmud_struct_get_members,      NULL, NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_struct_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_any_struct_type_type, 0)
    "ldmud.Struct",                     /* tp_name */
    sizeof(ldmud_struct_t),             /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_struct_dealloc,   /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_struct_repr,        /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_struct_hash,        /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC struct",                       /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_struct_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_struct_methods,               /* tp_methods */
    0,                                  /* tp_members */
    ldmud_struct_getset,                /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_struct_init,        /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_struct_new,                   /* tp_new */
},  ldmud_struct_get_lpctype            /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_struct_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC struct type.
 */

{
    return Py_TYPE(ob) == &ldmud_struct_type.type_base;
} /* ldmud_struct_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_struct_create (struct_t* st)

/* Creates a new Python struct from an LPC struct.
 */

{
    PyObject* val = ldmud_struct_new(&ldmud_struct_type.type_base, NULL, NULL);
    if (val != NULL)
        ((ldmud_struct_t*)val)->lpc_struct = ref_struct(st);

    return val;
} /* ldmud_struct_create() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_concrete_struct_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for a ldmud.Struct[program name, struct name].
 * We're using the information from the type to construct the struct.
 */

{
    static char *kwlist[] = { "values", NULL};
    struct_type_t *stype;
    PyObject *values = NULL;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "|O", kwlist, &values))
        return NULL;

    stype = ldmud_concrete_struct_type_get_struct_type((ldmud_concrete_struct_type_t*)type);
    if (!stype)
        return NULL;

    PyObject *result = ldmud_struct_new(&ldmud_struct_type.type_base, args, kwds);
    if (!result)
        return NULL;

    /* When returning a different type, we need to initialize that also. */
    if (!ldmud_struct_init_values((ldmud_struct_t*) result, stype, values))
    {
        Py_DECREF(result);
        return NULL;
    }

    return result;
} /* ldmud_concrete_struct_new() */

/*-------------------------------------------------------------------------*/
/* Closures */

static PyObject* ldmud_closure_create(svalue_t* cl);

static void
ldmud_closure_dealloc (ldmud_closure_t* self)

/* Destroy the ldmud_closure_t object
 */

{
    free_svalue(&self->lpc_closure);

    remove_gc_object(&gc_closure_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_closure_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_closure_plain_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_closure_t, i.e. allocate and initialize
 * the closure with null values.
 */

{
    ldmud_closure_t *self;

    self = (ldmud_closure_t *)type->tp_alloc(type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_closure.type = T_INVALID;
    add_gc_object(&gc_closure_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_closure_plain_new() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_closure_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud.Closure. This will defer initialization to
 * specific subtypes.
 */

{
    /* This function is similar to symbol_function.
     * We expect:
     *  - the object, that this closure is bound to,
     *  - the name of the (simul-)efun or lfun,
     *  - for an lfun the object of the lfun.
     */

    static char *kwlist[] = { "object", "name", "lfun_object", NULL};

    Py_ssize_t length;
    const char *name;
    PyObject *bound_ob, *lfun_ob;
    program_t *prog = NULL;
    svalue_t sv_bound_ob = const0, sv_lfun_ob = const0;
    svalue_t cl;
    PyObject *result;

    lfun_ob = NULL;
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "Os#|O", kwlist,
                                      &bound_ob,
                                      &name, &length,
                                      &lfun_ob))
        return NULL;

    if (!python_object_to_object(bound_ob, "object argument", &sv_bound_ob, NULL))
        return NULL;

    if (lfun_ob == NULL) {}
    else if (!python_object_to_object(lfun_ob, "lfun_object argument", &sv_lfun_ob, &prog))
        return NULL;

    if(lfun_ob == NULL)
    {
        /* It shall be a (simul-)efun closure. */
        svalue_t prev_ob = current_object;

        current_object = sv_bound_ob;
        symbol_efun_str(name, length, &cl, OVERRIDE_NONE, true);
        current_object = prev_ob;

        if (cl.type != T_CLOSURE)
        {
            PyErr_Format(PyExc_NameError, "unknown symbol '%s'", name);
            return NULL;
        }
    }
    else
    {
        string_t *funname = find_tabled_str_n(name, length, STRING_UTF8);
        int idx;

        if(funname)
            idx = find_function(funname, prog);
        else
            idx = -1;

        if (idx < 0)
        {
            PyErr_Format(PyExc_NameError, "unknown function '%s' in '%s'", name, get_txt(prog->name));
            return NULL;
        }

        closure_lfun(&cl, sv_lfun_ob, NULL,(unsigned short)idx, 0, MY_FALSE);
        if (cl.type != T_CLOSURE)
        {
            PyErr_SetString(PyExc_MemoryError, "out of memory");
            return NULL;
        }

        /* The closure was bound to the wrong object */
        assign_object_svalue(&(cl.u.lfun_closure->base.ob), sv_bound_ob, "ldmud_closure_init");
    }

    result = ldmud_closure_create(&cl);
    free_svalue(&cl);
    return result;
} /* ldmud_closure_init() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_closure_hash (ldmud_closure_t *val)

/* Return a hash of this closure.
 */

{
    ph_int closure_type;

    if (val->lpc_closure.type != T_CLOSURE)
        return 0;

    closure_type = val->lpc_closure.x.closure_type;

    /* Lambdas. */
    if (CLOSURE_REFERENCES_CODE(closure_type))
        return _Py_HashPointer(val->lpc_closure.u.lambda) ^ closure_type;
    /* Lfun or identifier closures. */
    if (CLOSURE_MALLOCED(closure_type))
        return (val->lpc_closure.u.closure->ob.type == T_OBJECT
                ? _Py_HashPointer(val->lpc_closure.u.closure->ob.u.ob)
                : _Py_HashPointer(val->lpc_closure.u.closure->ob.u.lwob)) ^ closure_type;
    /* Efun, simul-efun or operator closure. */
    return closure_type;
} /* ldmud_closure_hash() */

/*-------------------------------------------------------------------------*/
static bool ldmud_closure_check(PyObject *ob);

static PyObject*
ldmud_closure_richcompare (ldmud_closure_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    svalue_t *self_cl, *other_cl;
    bool result;
    PyObject* resultval;

    if (!ldmud_closure_check(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    self_cl = &self->lpc_closure;
    other_cl = &((ldmud_closure_t*)other)->lpc_closure;

    if(self_cl->type != T_CLOSURE && other_cl->type != T_CLOSURE)
        result = op == Py_LE || op == Py_EQ || op == Py_GE;
    else if(self_cl->type != T_CLOSURE)
        result = op == Py_LT || op == Py_LE || op == Py_NE;
    else if(other_cl->type != T_CLOSURE)
        result = op == Py_GT || op == Py_GE || op == Py_NE;
    else
    {
        int cmp = closure_cmp(self_cl, other_cl);
        switch (op)
        {
            case Py_LT: result = cmp < 0; break;
            case Py_LE: result = cmp <= 0; break;
            case Py_EQ: result = cmp == 0; break;
            case Py_NE: result = cmp != 0; break;
            case Py_GT: result = cmp > 0; break;
            case Py_GE: result = cmp >= 0; break;
            default:
            {
                Py_INCREF(Py_NotImplemented);
                return Py_NotImplemented;
            }
        }
    }

    resultval = result ? Py_True : Py_False;
    Py_INCREF(resultval);
    return resultval;
} /* ldmud_closure_richcompare() */

/*-------------------------------------------------------------------------*/
static int
ldmud_closure_bool(ldmud_closure_t *val)

/* Return 0 (false) for closures of destructed objects, 1 (true) for normal objects.
 */

{
    if (val->lpc_closure.type != T_CLOSURE)
        return 0;
    return !destructed_object_ref(&(val->lpc_closure));
} /* ldmud_closure_bool() */

/*-------------------------------------------------------------------------*/
static void
ldmud_closure_call_closure (int num_arg, ldmud_closure_t* closure)

/* Helper function for ldmud_closure_call().
 */

{
    call_lambda(&closure->lpc_closure, num_arg);
} /* ldmud_closure_call_closure() */


static PyObject*
ldmud_closure_call (ldmud_closure_t *cl, PyObject *arg, PyObject *kw)

/* Implement the call operator for closures.
 */

{
    if(python_is_external ? (!master_ob) : (current_object.type == T_NUMBER))
    {
        PyErr_SetString(PyExc_RuntimeError, "can't call a closure without a current object");
        return NULL;
    }

    if (kw != NULL && PyDict_Size(kw) != 0)
    {
        PyErr_SetString(PyExc_TypeError, "closure call takes no keyword arguments");
        return NULL;
    }
    else if (cl->lpc_closure.type == T_NUMBER)
    {
        /* The object was destroyed and the GC might have replaced it with a zero. */
        Py_INCREF(Py_None);
        return Py_None;
    }
    else if (cl->lpc_closure.type != T_CLOSURE)
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized lpc closure");
        return NULL;
    }
    else
    {
        svalue_t *sp = inter_sp;
        PyObject *result;
        int num_arg = (int)PyTuple_GET_SIZE(arg);

        /* Put all arguments on the stack. */
        for (int i = 0; i < num_arg; i++)
        {
            const char* err = python_to_svalue(++sp, PyTuple_GetItem(arg, i));
            if (err != NULL)
            {
                PyErr_SetString(PyExc_ValueError, err);
                pop_n_elems(i, sp-1);
                return NULL;
            }
        }

        inter_sp = sp;

        if(call_lpc_secure((CClosureFun)ldmud_closure_call_closure, num_arg, cl))
        {
            result = svalue_to_python(inter_sp);
            pop_stack();
        }
        else
            result = NULL;

        return result;
    }

    return NULL;
} /* ldmud_closure_call() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_closure_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Closure.
 */

{
    return lpctype_closure;
} /* ldmud_closure_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_closure_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    (inquiry)ldmud_closure_bool,        /* nb_bool */
};

static PyMethodDef ldmud_closure_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_closure_getset[] =
{
    {NULL}
};

static ldmud_lpctype_t ldmud_closure_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.Closure",                    /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_closure_dealloc,  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    &ldmud_closure_as_number,           /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_closure_hash,       /* tp_hash  */
    (ternaryfunc)ldmud_closure_call,    /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC closure",                      /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_closure_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_closure_methods,              /* tp_methods */
    0,                                  /* tp_members */
    ldmud_closure_getset,               /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_new,                  /* tp_new */
},  ldmud_closure_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_lfun_closure_check_available (ldmud_closure_t *self)

/* Helper function to check whether the lfun closure is initialized.
 * Returns true if so, false otherwise. Sets a Python error if false.
 */

{
    if (self->lpc_closure.type != T_CLOSURE ||
        self->lpc_closure.x.closure_type != CLOSURE_LFUN)
    {
        PyErr_Format(PyExc_ValueError, "empty object");
        return false;
    }

    return true;
} /* ldmud_lfun_closure_check_available() */

/*-------------------------------------------------------------------------*/
static int
ldmud_lfun_closure_init (ldmud_closure_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for LfunClosure, i.e. create a new lfun closure object
 * from the given arguments.
 */

{
    /* This function is similar to symbol_function.
     * We expect:
     *  - the object of the lfun,
     *  - the name of the lfun, and
     *  - optionally the object, that this closure is bound to.
     */

    static char *kwlist[] = { "object", "name", "bound_object", NULL};

    Py_ssize_t length;
    const char *name;
    PyObject *bound_ob, *lfun_ob;
    program_t *prog = NULL;
    svalue_t sv_bound_ob = const0, sv_lfun_ob = const0;
    string_t *funname;
    int idx;

    bound_ob = NULL;
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "Os#|O", kwlist,
                                      &lfun_ob,
                                      &name, &length,
                                      &bound_ob))
        return -1;

    if (!python_object_to_object(lfun_ob, "object argument", &sv_lfun_ob, &prog))
        return -1;

    if (bound_ob == NULL)
        sv_bound_ob = sv_lfun_ob;
    else if (!python_object_to_object(bound_ob, "bound_object argument", &sv_bound_ob, NULL))
        return -1;

    funname = find_tabled_str_n(name, length, STRING_UTF8);
    if(funname)
        idx = find_function(funname, prog);
    else
        idx = -1;

    if (idx < 0)
    {
        PyErr_Format(PyExc_NameError, "unknown function '%s' in '%s'", name, get_txt(prog->name));
        return -1;
    }

    free_svalue(&self->lpc_closure);
    closure_lfun(&self->lpc_closure, sv_lfun_ob, NULL,(unsigned short)idx, 0, MY_FALSE);
    if (self->lpc_closure.type != T_CLOSURE)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return -1;
    }

    /* The closure was bound to the wrong object */
    assign_object_svalue(&(self->lpc_closure.u.lfun_closure->base.ob), sv_bound_ob, "ldmud_closure_init");

    return 0;
} /* ldmud_lfun_closure_init() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lfun_closure_get_bound_object (ldmud_closure_t *self, void *closure)

/* Returns the (lw)object this closure is bound to.
 */

{
    if (!ldmud_lfun_closure_check_available(self))
        return NULL;

    switch (self->lpc_closure.u.lfun_closure->base.ob.type)
    {
        case T_OBJECT:
            return ldmud_object_create(self->lpc_closure.u.lfun_closure->base.ob.u.ob);

        case T_LWOBJECT:
            return ldmud_lwobject_create(self->lpc_closure.u.lfun_closure->base.ob.u.lwob);

        default:
            Py_INCREF(Py_None);
            return Py_None;
    }
} /* ldmud_lfun_closure_get_bound_object() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lfun_closure_get_lfun (ldmud_closure_t *self, void *closure)

/* Returns the lfun object for this lfun closure.
 */

{
    lfun_closure_t *cl;
    program_t *prog;
    int ix;

    if (!ldmud_lfun_closure_check_available(self))
        return NULL;

    cl = self->lpc_closure.u.lfun_closure;
    ix = cl->fun_index;
    switch (cl->fun_ob.type)
    {
        case T_OBJECT:
        {
            object_t *ob = cl->fun_ob.u.ob;
            if (O_PROG_SWAPPED(ob) && load_ob_from_swap(ob) < 0)
            {
                 PyErr_Format(PyExc_MemoryError, "out of memory when unswapping object '%s'", get_txt(ob->name));
                 return NULL;
            }

            prog = ob->prog;
            break;
        }

        case T_LWOBJECT:
            prog = cl->fun_ob.u.lwob->prog;
            break;

        default:
            fatal("Invalid object type for closure.\n");
    }

    return ldmud_program_lfun_create(cl->fun_ob, prog, ix);
} /* ldmud_lfun_closure_get_lfun() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lfun_closure_get_object (ldmud_closure_t *self, void *closure)

/* Returns the object of the lfun.
 */

{
    if (!ldmud_lfun_closure_check_available(self))
        return NULL;

    switch (self->lpc_closure.u.lfun_closure->fun_ob.type)
    {
        case T_OBJECT:
            return ldmud_object_create(self->lpc_closure.u.lfun_closure->fun_ob.u.ob);

        case T_LWOBJECT:
            return ldmud_lwobject_create(self->lpc_closure.u.lfun_closure->fun_ob.u.lwob);

        default:
            Py_INCREF(Py_None);
            return Py_None;
    }
} /* ldmud_lfun_closure_get_object() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lfun_closure_get_is_inherited (ldmud_closure_t *self, void *closure)

/* Returns
 */

{
    if (!ldmud_lfun_closure_check_available(self))
        return NULL;

    PyObject *result = self->lpc_closure.u.lfun_closure->inhProg ? Py_True : Py_False;
    Py_INCREF(result);
    return result;
} /* ldmud_lfun_closure_get_is_inherited() */

/*-------------------------------------------------------------------------*/
static PyGetSetDef ldmud_lfun_closure_getset[] =
{
    { "bound_object", (getter)ldmud_lfun_closure_get_bound_object, NULL, NULL},
    { "lfun",         (getter)ldmud_lfun_closure_get_lfun,         NULL, NULL},
    { "object",       (getter)ldmud_lfun_closure_get_object,       NULL, NULL},
    { "inherited",    (getter)ldmud_lfun_closure_get_is_inherited, NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_lfun_closure_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.LfunClosure",                /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC lfun closure",                 /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_lfun_closure_getset,          /* tp_getset */
    &ldmud_closure_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_lfun_closure_init,  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_plain_new,            /* tp_new */
},  ldmud_closure_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_identifier_closure_check_available (ldmud_closure_t *self)

/* Helper function to check whether the identifier closure is initialized.
 * Returns true if so, false otherwise. Sets a Python error if false.
 */

{
    if (self->lpc_closure.type != T_CLOSURE ||
        self->lpc_closure.x.closure_type != CLOSURE_IDENTIFIER)
    {
        PyErr_Format(PyExc_ValueError, "empty object");
        return false;
    }

    return true;
} /* ldmud_identifier_closure_check_available() */

/*-------------------------------------------------------------------------*/
static int
ldmud_identifier_closure_init (ldmud_closure_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for IdentifierClosure, i.e. create a new identifier
 * closure object from the given arguments.
 */

{
    /* We expect:
     *  - the object of the variable.
     *  - the name of the variable,
     */

    static char *kwlist[] = { "object", "name", NULL};

    Py_ssize_t length;
    const char *name;
    PyObject *var_ob;
    program_t *prog = NULL;
    svalue_t sv_var_ob = const0;
    string_t *varname;
    int idx;

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "Os#", kwlist,
                                      &var_ob,
                                      &name, &length))
        return -1;

    if (!python_object_to_object(var_ob, "object argument", &sv_var_ob, &prog))
        return -1;

    varname = find_tabled_str_n(name, length, STRING_UTF8);
    if(varname)
    {
        for (idx = 0; idx < prog->num_variables; idx++)
        {
            if (prog->variables[idx].name == varname)
                break;
        }
        if (idx == prog->num_variables)
            idx = -1;
    }
    else
        idx = -1;

    if (idx < 0)
    {
        PyErr_Format(PyExc_NameError, "unknown variable '%s' in '%s'", name, get_txt(prog->name));
        return -1;
    }

    free_svalue(&self->lpc_closure);
    closure_identifier(&self->lpc_closure, sv_var_ob, (unsigned short)idx, MY_FALSE);
    if (self->lpc_closure.type != T_CLOSURE)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return -1;
    }

    return 0;
} /* ldmud_identifier_closure_init() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_identifier_closure_get_object (ldmud_closure_t *self, void *closure)

/* Returns the (lw)object of this closure.
 */

{
    if (!ldmud_identifier_closure_check_available(self))
        return NULL;

    switch (self->lpc_closure.u.identifier_closure->base.ob.type)
    {
        case T_OBJECT:
            return ldmud_object_create(self->lpc_closure.u.identifier_closure->base.ob.u.ob);

        case T_LWOBJECT:
            return ldmud_lwobject_create(self->lpc_closure.u.identifier_closure->base.ob.u.lwob);

        default:
            Py_INCREF(Py_None);
            return Py_None;
    }
} /* ldmud_identifier_closure_get_object() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_identifier_closure_get_variable (ldmud_closure_t *self, void *closure)

/* Returns the variable object for this identifier closure.
 */

{
    identifier_closure_t *cl;
    program_t *prog;

    if (!ldmud_identifier_closure_check_available(self))
        return NULL;

    cl = self->lpc_closure.u.identifier_closure;
    switch (cl->base.ob.type)
    {
        case T_OBJECT:
        {
            object_t *ob = cl->base.ob.u.ob;
            if (O_PROG_SWAPPED(ob) && load_ob_from_swap(ob) < 0)
            {
                 PyErr_Format(PyExc_MemoryError, "out of memory when unswapping object '%s'", get_txt(ob->name));
                 return NULL;
            }

            prog = ob->prog;
            break;
        }

        case T_LWOBJECT:
            prog = cl->base.ob.u.lwob->prog;
            break;

        default:
            fatal("Invalid object type for closure.\n");
    }

    if (cl->var_index == VANISHED_VARCLOSURE_INDEX)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return ldmud_program_variable_create(cl->base.ob, prog, cl->var_index);
} /* ldmud_identifier_closure_get_lfun() */

/*-------------------------------------------------------------------------*/
static PyGetSetDef ldmud_identifier_closure_getset[] =
{
    /* The bound object is the same as the actual variable object. However we provide
     * the 'bound_object' function to be compatible with all other closure objects.
     */
    { "bound_object", (getter)ldmud_identifier_closure_get_object,       NULL, NULL},
    { "object",       (getter)ldmud_identifier_closure_get_object,       NULL, NULL},
    { "variable",     (getter)ldmud_identifier_closure_get_variable,     NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_identifier_closure_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.IdentifierClosure",          /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC identifier closure",           /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_identifier_closure_getset,    /* tp_getset */
    &ldmud_closure_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_identifier_closure_init,  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_plain_new,            /* tp_new */
},  ldmud_closure_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_lambda_closure_check_available (ldmud_closure_t *self)

/* Helper function to check whether the lambda closure is initialized.
 * Returns true if so, false otherwise. Sets a Python error if false.
 */

{
    if (self->lpc_closure.type != T_CLOSURE ||
        (self->lpc_closure.x.closure_type != CLOSURE_LAMBDA &&
         self->lpc_closure.x.closure_type != CLOSURE_UNBOUND_LAMBDA))
    {
        PyErr_Format(PyExc_ValueError, "empty object");
        return false;
    }

    return true;
} /* ldmud_lambda_closure_check_available() */

/*-------------------------------------------------------------------------*/
static int
ldmud_lambda_closure_init (ldmud_closure_t *self, PyObject *args, PyObject *kwds)

/* This prevents creating a lambda closure from Python.
 */

{
    PyErr_SetString(PyExc_NameError, "__init__ not supported for this type");
    return -1;
} /* ldmud_lambda_closure_init() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lambda_closure_get_object (ldmud_closure_t *self, void *closure)

/* Returns the (lw)object this closure is bound to.
 */

{
    if (!ldmud_lambda_closure_check_available(self))
        return NULL;

    switch (self->lpc_closure.u.lambda->base.ob.type)
    {
        case T_OBJECT:
            return ldmud_object_create(self->lpc_closure.u.lambda->base.ob.u.ob);

        case T_LWOBJECT:
            return ldmud_lwobject_create(self->lpc_closure.u.lambda->base.ob.u.lwob);

        default:
            Py_INCREF(Py_None);
            return Py_None;
    }
} /* ldmud_lambda_closure_get_object() */

/*-------------------------------------------------------------------------*/
static PyGetSetDef ldmud_lambda_closure_getset[] =
{
    { "bound_object", (getter)ldmud_lambda_closure_get_object, NULL, NULL},
    { "object",       (getter)ldmud_lambda_closure_get_object, NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_lambda_closure_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.LambdaClosure",              /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC lambda closure",               /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_lambda_closure_getset,        /* tp_getset */
    &ldmud_closure_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_lambda_closure_init,/* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_plain_new,            /* tp_new */
},  ldmud_closure_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
/* Unbound lambdas use the same data structure as normal lambdas,
 * therefore we are just creating another type, but re-use the
 * implementations of normal lambdas.
 */
static ldmud_lpctype_t ldmud_unbound_lambda_closure_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.UnboundLambdaClosure",       /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC unbound lambda closure",       /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_lambda_closure_getset,        /* tp_getset */
    &ldmud_closure_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_lambda_closure_init,/* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_plain_new,            /* tp_new */
},  ldmud_closure_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_bound_lambda_closure_check_available (ldmud_closure_t *self)

/* Helper function to check whether the lambda closure is initialized.
 * Returns true if so, false otherwise. Sets a Python error if false.
 */

{
    if (self->lpc_closure.type != T_CLOSURE ||
        self->lpc_closure.x.closure_type != CLOSURE_BOUND_LAMBDA)
    {
        PyErr_Format(PyExc_ValueError, "empty object");
        return false;
    }

    return true;
} /* ldmud_bound_lambda_closure_check_available() */

/*-------------------------------------------------------------------------*/
static int
ldmud_bound_lambda_closure_init (ldmud_closure_t *self, PyObject *args, PyObject *kwds)

/* This is similar to bind_lambda() and binds an unbound lambda to an object.
 */

{
    static char *kwlist[] = { "object", "lambda", NULL};

    PyObject *ob;
    ldmud_closure_t *lambda;
    bound_lambda_t *l;
    svalue_t sv_ob;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "OO!", kwlist,
                                     &ob, &ldmud_unbound_lambda_closure_type, &lambda))
        return -1;

    if (!python_object_to_object(ob, "object argument", &sv_ob, NULL))
        return -1;

    l = xalloc(sizeof(bound_lambda_t));
    if (!l)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return -1;
    }

    closure_init_base(&(l->base), sv_ob);
    l->lambda = lambda->lpc_closure.u.lambda;
    lambda->lpc_closure.u.lambda->base.ref++;

    free_svalue(&self->lpc_closure);
    self->lpc_closure.type = T_CLOSURE;
    self->lpc_closure.x.closure_type = CLOSURE_BOUND_LAMBDA;
    self->lpc_closure.u.bound_lambda = l;

    return 0;
} /* ldmud_bound_lambda_closure_init() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_bound_lambda_closure_get_object (ldmud_closure_t *self, void *closure)

/* Returns the (lw)object this closure is bound to.
 */


{
    if (!ldmud_bound_lambda_closure_check_available(self))
        return NULL;

    switch (self->lpc_closure.u.bound_lambda->base.ob.type)
    {
        case T_OBJECT:
            return ldmud_object_create(self->lpc_closure.u.bound_lambda->base.ob.u.ob);

        case T_LWOBJECT:
            return ldmud_lwobject_create(self->lpc_closure.u.bound_lambda->base.ob.u.lwob);

        default:
            Py_INCREF(Py_None);
            return Py_None;
    }
} /* ldmud_bound_lambda_closure_get_object() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_bound_lambda_closure_get_unbound_lambda (ldmud_closure_t *self, void *closure)

/* Returns the original unbound lambda for this lambda.
 */

{
    ldmud_closure_t *result;

    if (!ldmud_bound_lambda_closure_check_available(self))
        return NULL;

    result = (ldmud_closure_t *)ldmud_unbound_lambda_closure_type.type_base.tp_alloc(&ldmud_unbound_lambda_closure_type.type_base, 0);
    if (result == NULL)
        return NULL;

    result->lpc_closure.type = T_CLOSURE;
    result->lpc_closure.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
    result->lpc_closure.u.lambda = self->lpc_closure.u.bound_lambda->lambda;
    result->lpc_closure.u.lambda->base.ref++;

    add_gc_object(&gc_closure_list, (ldmud_gc_var_t*)result);

    return (PyObject *)result;

} /* ldmud_bound_lambda_closure_get_unbound_lambda() */

/*-------------------------------------------------------------------------*/
static PyGetSetDef ldmud_bound_lambda_closure_getset[] =
{
    { "bound_object",   (getter)ldmud_bound_lambda_closure_get_object,         NULL, NULL},
    { "object",         (getter)ldmud_bound_lambda_closure_get_object,         NULL, NULL},
    { "unbound_lambda", (getter)ldmud_bound_lambda_closure_get_unbound_lambda, NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_bound_lambda_closure_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.BoundLambdaClosure",         /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC bound lambda closure",         /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_bound_lambda_closure_getset,  /* tp_getset */
    &ldmud_closure_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_bound_lambda_closure_init, /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_plain_new,            /* tp_new */
},  ldmud_closure_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_efun_closure_check_available (ldmud_closure_t *self)

/* Helper function to check whether the efun closure is initialized.
 * Returns true if so, false otherwise. Sets a Python error if false.
 */

{
    int closure_type = self->lpc_closure.x.closure_type;
    if (closure_type < CLOSURE_LWO)
        closure_type -= CLOSURE_LWO;
    closure_type &= -0x0800;

    if (self->lpc_closure.type != T_CLOSURE ||
        (closure_type != CLOSURE_EFUN && closure_type != CLOSURE_PYTHON_EFUN))
    {
        PyErr_Format(PyExc_ValueError, "empty object");
        return false;
    }

    return true;
} /* ldmud_efun_closure_check_available() */

/*-------------------------------------------------------------------------*/
static int
ldmud_efun_closure_init (ldmud_closure_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for EfunClosure, i.e. create a new efun closure object
 * from the given arguments.
 */

{
    /* We expect:
     *  - the object this shall be bound to.
     *  - the name of the efun (ldmud.Efuns objects are also accepted).
     */

    static char *kwlist[] = { "object", "name", NULL};

    PyObject *ob, *name;
    svalue_t sv_ob;
    int idx;

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "OO", kwlist,
                                      &ob, &name))
        return -1;

    if (!python_object_to_object(ob, "object argument", &sv_ob, NULL))
        return -1;

    if (PyUnicode_Check(name))
    {
        bool error;
        string_t *efun_name = find_tabled_python_string(name, "efun name", &error);
        if (error)
            return -1;

        idx = 0;
        if (efun_name)
        {
            svalue_t result;

            symbol_efun_str(get_txt(efun_name), mstrsize(efun_name), &result, OVERRIDE_EFUN, true);
            if (result.type == T_CLOSURE)
            {
                idx = result.x.lvalue_type;
                free_svalue(&result);

                if (idx < CLOSURE_LWO)
                    idx -= CLOSURE_LWO;
                if ((idx&-0x0800) != CLOSURE_EFUN && (idx&-0x0800) != CLOSURE_PYTHON_EFUN)
                    idx = 0;
            }
        }

        if (!idx)
        {
            PyErr_Format(PyExc_NameError, "unknown efun '%s'", get_txt(efun_name));
            return -1;
        }
    }
    else if (ldmud_efun_check(name))
    {
        idx = CLOSURE_EFUN + ((ldmud_efun_t*)name)->efun_idx;
    }
    else if (ldmud_python_efun_check(name))
    {
        idx = CLOSURE_PYTHON_EFUN + ((ldmud_efun_t*)name)->efun_idx;
    }
    else
    {
        PyErr_Format(PyExc_TypeError, "efun name must be a string, not %.200s", name->ob_type->tp_name);
        return -1;
    }

    free_svalue(&self->lpc_closure);
    self->lpc_closure.type = T_CLOSURE;
    if (sv_ob.type == T_LWOBJECT)
    {
        self->lpc_closure.x.closure_type = idx + CLOSURE_LWO;
        self->lpc_closure.u.lwob = ref_lwobject(sv_ob.u.lwob);
    }
    else
    {
        self->lpc_closure.x.closure_type = idx;
        self->lpc_closure.u.ob = ref_object(sv_ob.u.ob, "ldmud_efun_closure_init");
    }

    return 0;
} /* ldmud_efun_closure_init() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_efun_closure_get_object (ldmud_closure_t *self, void *closure)

/* Returns the (lw)object of this closure.
 */

{
    if (!ldmud_efun_closure_check_available(self))
        return NULL;

    if (self->lpc_closure.x.closure_type < CLOSURE_LWO)
        return ldmud_lwobject_create(self->lpc_closure.u.lwob);
    else
        return ldmud_object_create(self->lpc_closure.u.ob);
} /* ldmud_efun_closure_get_object() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_efun_closure_get_efun (ldmud_closure_t *self, void *closure)

/* Returns an efun object for this efun closure.
 */

{
    int closure_type;

    if (!ldmud_efun_closure_check_available(self))
        return NULL;

    closure_type = self->lpc_closure.x.closure_type;
    if (closure_type < CLOSURE_LWO)
        closure_type -= CLOSURE_LWO;

    switch (closure_type & -0x0800)
    {
        case CLOSURE_EFUN:
            return ldmud_efun_create(closure_type - CLOSURE_EFUN);

        case CLOSURE_PYTHON_EFUN:
            return ldmud_python_efun_create(closure_type - CLOSURE_PYTHON_EFUN);

        default:
            fatal("Illegal closure type: %d\n", (int)self->lpc_closure.x.closure_type);
            return NULL;
    }
} /* ldmud_efun_closure_get_lfun() */

/*-------------------------------------------------------------------------*/
static PyGetSetDef ldmud_efun_closure_getset[] =
{
    /* The bound object is the same as the actual variable object. However we provide
     * the 'bound_object' function to be compatible with all other closure objects.
     */
    { "bound_object", (getter)ldmud_efun_closure_get_object,       NULL, NULL},
    { "object",       (getter)ldmud_efun_closure_get_object,       NULL, NULL},
    { "efun",         (getter)ldmud_efun_closure_get_efun,         NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_efun_closure_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.EfunClosure",                /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC efun closure",                 /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_efun_closure_getset,          /* tp_getset */
    &ldmud_closure_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_efun_closure_init,  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_plain_new,            /* tp_new */
},  ldmud_closure_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_simul_efun_closure_check_available (ldmud_closure_t *self)

/* Helper function to check whether the simul-efun closure is initialized.
 * Returns true if so, false otherwise. Sets a Python error if false.
 */

{
    int closure_type = self->lpc_closure.x.closure_type;
    if (closure_type < CLOSURE_LWO)
        closure_type -= CLOSURE_LWO;
    closure_type &= -0x0800;

    if (self->lpc_closure.type != T_CLOSURE || closure_type != CLOSURE_SIMUL_EFUN || simul_efun_object == NULL)
    {
        PyErr_Format(PyExc_ValueError, "empty object");
        return false;
    }

    return true;
} /* ldmud_simul_efun_closure_check_available() */

/*-------------------------------------------------------------------------*/
static int
ldmud_simul_efun_closure_init (ldmud_closure_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for SimulEfunClosure, i.e. create a new simul-efun
 * closure object from the given arguments.
 */

{
    /* We expect:
     *  - the object this shall be bound to.
     *  - the name of the simul-efun
     */

    static char *kwlist[] = { "object", "name", NULL};

    PyObject *ob;
    const char *name;
    Py_ssize_t length;
    svalue_t sv_ob;
    string_t *sefun_name;
    int idx;

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "Os#", kwlist, &ob, &name, &length))
        return -1;

    if (!python_object_to_object(ob, "object argument", &sv_ob, NULL))
        return -1;

    idx = 0;
    sefun_name = find_tabled_str_n(name, length, STRING_UTF8);
    if (sefun_name)
    {
        svalue_t result;

        symbol_efun_str(get_txt(sefun_name), mstrsize(sefun_name), &result, OVERRIDE_SEFUN, true);
        if (result.type == T_CLOSURE)
        {
            idx = result.x.lvalue_type;
            free_svalue(&result);

            if (idx < CLOSURE_LWO)
                idx -= CLOSURE_LWO;
            if ((idx&-0x0800) != CLOSURE_SIMUL_EFUN)
                idx = 0;
        }
    }

    if (!idx)
    {
        PyErr_Format(PyExc_NameError, "unknown simul-efun '%s'", get_txt(sefun_name));
        return -1;
    }

    free_svalue(&self->lpc_closure);
    self->lpc_closure.type = T_CLOSURE;
    if (sv_ob.type == T_LWOBJECT)
    {
        self->lpc_closure.x.closure_type = idx + CLOSURE_LWO;
        self->lpc_closure.u.lwob = ref_lwobject(sv_ob.u.lwob);
    }
    else
    {
        self->lpc_closure.x.closure_type = idx;
        self->lpc_closure.u.ob = ref_object(sv_ob.u.ob, "ldmud_simul_efun_closure_init");
    }

    return 0;
} /* ldmud_simul_efun_closure_init() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_simul_efun_closure_get_object (ldmud_closure_t *self, void *closure)

/* Returns the (lw)object of this closure.
 */

{
    if (!ldmud_simul_efun_closure_check_available(self))
        return NULL;

    if (self->lpc_closure.x.closure_type < CLOSURE_LWO)
        return ldmud_lwobject_create(self->lpc_closure.u.lwob);
    else
        return ldmud_object_create(self->lpc_closure.u.ob);
} /* ldmud_simul_efun_closure_get_object() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_simul_efun_closure_get_simul_efun (ldmud_closure_t *self, void *closure)

/* Returns an lfun object for this simul-efun closure.
 */

{
    int closure_type;
    simul_efun_table_t *sefun;

    if (!ldmud_simul_efun_closure_check_available(self))
        return NULL;

    closure_type = self->lpc_closure.x.closure_type;
    if (closure_type < CLOSURE_LWO)
        closure_type -= CLOSURE_LWO;

    sefun = simul_efun_table + (closure_type - CLOSURE_SIMUL_EFUN);
    return ldmud_program_lfun_create(svalue_object(simul_efun_object), sefun->program, 
        sefun->program->function_headers[FUNCTION_HEADER_INDEX(sefun->funstart)].offset.fx);
} /* ldmud_simul_efun_closure_get_lfun() */

/*-------------------------------------------------------------------------*/
static PyGetSetDef ldmud_simul_efun_closure_getset[] =
{
    { "bound_object", (getter)ldmud_simul_efun_closure_get_object,     NULL, NULL},
    { "object",       (getter)ldmud_simul_efun_closure_get_object,     NULL, NULL},
    { "simul_efun",   (getter)ldmud_simul_efun_closure_get_simul_efun, NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_simul_efun_closure_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.SimulEfunClosure",           /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC simul-efun closure",           /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_simul_efun_closure_getset,    /* tp_getset */
    &ldmud_closure_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_simul_efun_closure_init,  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_plain_new,            /* tp_new */
},  ldmud_closure_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_operator_closure_check_available (ldmud_closure_t *self)

/* Helper function to check whether the operator closure is initialized.
 * Returns true if so, false otherwise. Sets a Python error if false.
 */

{
    int closure_type = self->lpc_closure.x.closure_type;
    if (closure_type < CLOSURE_LWO)
        closure_type -= CLOSURE_LWO;
    closure_type &= -0x0800;

    if (self->lpc_closure.type != T_CLOSURE || closure_type != CLOSURE_OPERATOR)
    {
        PyErr_Format(PyExc_ValueError, "empty object");
        return false;
    }

    return true;
} /* ldmud_operator_closure_check_available() */

/*-------------------------------------------------------------------------*/
static int
ldmud_operator_closure_init (ldmud_closure_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for OperatorClosure, i.e. create a new operator closure object
 * from the given arguments.
 */

{
    /* We expect:
     *  - the object this shall be bound to.
     *  - the name of the operator
     */

    static char *kwlist[] = { "object", "name", NULL};

    PyObject *ob;
    const char *name;
    Py_ssize_t length;
    svalue_t sv_ob, cl;
    int idx;

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "Os#", kwlist, &ob, &name, &length))
        return -1;

    if (!python_object_to_object(ob, "object argument", &sv_ob, NULL))
        return -1;

    idx = 0;
    symbol_efun_str(name, length, &cl, OVERRIDE_EFUN, true);
    if (cl.type == T_CLOSURE)
    {
        idx = cl.x.lvalue_type;
        free_svalue(&cl);

        if (idx < CLOSURE_LWO)
            idx -= CLOSURE_LWO;
        if ((idx&-0x0800) != CLOSURE_OPERATOR)
            idx = 0;
    }

    if (!idx)
    {
        PyErr_Format(PyExc_NameError, "unknown operator '%s'", name);
        return -1;
    }

    free_svalue(&self->lpc_closure);
    self->lpc_closure.type = T_CLOSURE;
    if (sv_ob.type == T_LWOBJECT)
    {
        self->lpc_closure.x.closure_type = idx + CLOSURE_LWO;
        self->lpc_closure.u.lwob = ref_lwobject(sv_ob.u.lwob);
    }
    else
    {
        self->lpc_closure.x.closure_type = idx;
        self->lpc_closure.u.ob = ref_object(sv_ob.u.ob, "ldmud_operator_closure_init");
    }

    return 0;
} /* ldmud_operator_closure_init() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_operator_closure_get_object (ldmud_closure_t *self, void *closure)

/* Returns the (lw)object of this closure.
 */

{
    if (!ldmud_operator_closure_check_available(self))
        return NULL;

    if (self->lpc_closure.x.closure_type < CLOSURE_LWO)
        return ldmud_lwobject_create(self->lpc_closure.u.lwob);
    else
        return ldmud_object_create(self->lpc_closure.u.ob);
} /* ldmud_operator_closure_get_object() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_operator_closure_get_operator_name (ldmud_closure_t *self, void *closure)

/* Returns the name of the operator of this closure.
 */

{
    int closure_type;
    const char* name;

    if (!ldmud_operator_closure_check_available(self))
        return NULL;

    closure_type = self->lpc_closure.x.closure_type;
    if (closure_type < CLOSURE_LWO)
        closure_type -= CLOSURE_LWO;

    name = closure_operator_to_string(closure_type);
    if (!name)
        name = instrs[closure_type - CLOSURE_OPERATOR].name;

    return PyUnicode_FromString(name);
} /* ldmud_operator_closure_get_operator_name() */

/*-------------------------------------------------------------------------*/
static PyGetSetDef ldmud_operator_closure_getset[] =
{
    { "bound_object",  (getter)ldmud_operator_closure_get_object,        NULL, NULL},
    { "object",        (getter)ldmud_operator_closure_get_object,        NULL, NULL},
    { "operator_name", (getter)ldmud_operator_closure_get_operator_name, NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_operator_closure_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.OperatorClosure",            /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC operator closure",             /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_operator_closure_getset,      /* tp_getset */
    &ldmud_closure_type.type_base,      /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_operator_closure_init,  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_plain_new,            /* tp_new */
},  ldmud_closure_get_lpctype           /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_closure_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC closure type.
 */

{
    return PyObject_TypeCheck(ob, &ldmud_closure_type.type_base);
} /* ldmud_closure_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_closure_create (svalue_t* cl)

/* Creates a new Python closure from an LPC closure.
 */

{
    ldmud_closure_t *self;
    ldmud_lpctype_t *type;

    switch (cl->x.closure_type)
    {
        case CLOSURE_LFUN:
            type = &ldmud_lfun_closure_type;
            break;

        case CLOSURE_IDENTIFIER:
            type = &ldmud_identifier_closure_type;
            break;

        case CLOSURE_LAMBDA:
            type = &ldmud_lambda_closure_type;
            break;

        case CLOSURE_UNBOUND_LAMBDA:
            type = &ldmud_unbound_lambda_closure_type;
            break;

        case CLOSURE_BOUND_LAMBDA:
            type = &ldmud_bound_lambda_closure_type;
            break;

        default:
        {
            int closure_type = cl->x.closure_type;
            if (closure_type < CLOSURE_LWO)
                closure_type -= CLOSURE_LWO;

            switch (closure_type & -0x0800)
            {
                case CLOSURE_EFUN:
                case CLOSURE_PYTHON_EFUN:
                    type = &ldmud_efun_closure_type;
                    break;

                case CLOSURE_SIMUL_EFUN:
                    type = &ldmud_simul_efun_closure_type;
                    break;

                case CLOSURE_OPERATOR:
                    type = &ldmud_operator_closure_type;
                    break;

                default:
                    type = &ldmud_closure_type;
                    break;
            }
            break;
        }
    }

    self = (ldmud_closure_t *)type->type_base.tp_alloc(&type->type_base, 0);
    if (self == NULL)
        return NULL;

    assign_svalue_no_free(&self->lpc_closure, cl);
    add_gc_object(&gc_closure_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_closure_create() */

/*-------------------------------------------------------------------------*/
/* Coroutines */

static void
ldmud_coroutine_dealloc (ldmud_coroutine_t* self)

/* Destroy the ldmud_coroutine_t object
 */

{
    free_coroutine(self->lpc_coroutine);

    remove_gc_object(&gc_coroutine_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_coroutine_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_coroutine_variables_repr (ldmud_coroutine_t *val)

/* Return a string representation of this coroutine variables.
 */

{
    function_t *fun;
    if (val->lpc_coroutine == NULL)
        return PyUnicode_FromString("<LPC uninitialized coroutine variables>");
    if (val->lpc_coroutine->prog == NULL)
        return PyUnicode_FromString("<LPC destructed coroutine variables>");

    fun = val->lpc_coroutine->prog->function_headers + FUNCTION_HEADER_INDEX(val->lpc_coroutine->funstart);

    switch (val->lpc_coroutine->ob.type)
    {
        case T_OBJECT:
            return PyUnicode_FromFormat("<LPC coroutine variables /%s->%s()>", get_txt(val->lpc_coroutine->ob.u.ob->name), get_txt(fun->name));
        case T_LWOBJECT:
            return PyUnicode_FromFormat("<LPC coroutine variables /%s->%s()>", get_txt(val->lpc_coroutine->ob.u.lwob->prog->name), get_txt(fun->name));
        default:
            return PyUnicode_FromString("<LPC destructed coroutine variables>");
    }
} /* ldmud_coroutine_variables_repr() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_coroutine_variables_getattro (ldmud_coroutine_t *val, PyObject *name)

/* Return the value of a local variable.
 */

{
    PyObject *result = PyObject_GenericGetAttr((PyObject *)val, name);
    if (result || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return result;

    if (!val->lpc_coroutine || !val->lpc_coroutine->prog
      || (val->lpc_coroutine->state != CS_SLEEPING && val->lpc_coroutine->state != CS_AWAITING))
        return NULL;
    else
    {
        string_t* varname;
        bool error;

        PyErr_Clear();

        /* And now search for the variable. */
        varname = find_tabled_python_string(name, "variable name", &error);
        if (error)
            return NULL;

        if (varname)
        {
            int num_names = val->lpc_coroutine->num_variable_names;
            string_t **strings = val->lpc_coroutine->prog->strings;
            bytecode_t *p = val->lpc_coroutine->pc - 2*num_names;

            for (int i = 0; i < num_names; i++)
            {
                if (varname != strings[get_short(p + 2*i)])
                    continue;

                return svalue_to_python(val->lpc_coroutine->variables + i);
            }
        }

        PyErr_Format(PyExc_AttributeError, "Variable '%U' not found", name);
        return NULL;
    }
} /* ldmud_coroutine_variables_getattro() */

/*-------------------------------------------------------------------------*/
static int
ldmud_coroutine_variables_setattro (ldmud_coroutine_t *val, PyObject *name, PyObject *value)

/* Set the value of a local variable.
 */

{
    int res = PyObject_GenericSetAttr((PyObject*)val, name, value);
    if (!res || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return res;

    if (!val->lpc_coroutine || !val->lpc_coroutine->prog
      || (val->lpc_coroutine->state != CS_SLEEPING && val->lpc_coroutine->state != CS_AWAITING))
        return -1;
    else
    {
        string_t* varname;
        bool error;

        PyErr_Clear();

        /* And now search for the variable. */
        varname = find_tabled_python_string(name, "variable name", &error);
        if (error)
            return -1;

        if (varname)
        {
            int num_names = val->lpc_coroutine->num_variable_names;
            string_t **strings = val->lpc_coroutine->prog->strings;
            bytecode_t *p = val->lpc_coroutine->pc - 2*num_names;

            for (int i = 0; i < num_names; i++)
            {
                if (varname != strings[get_short(p + 2*i)])
                    continue;

                if (value == NULL)
                    assign_svalue(val->lpc_coroutine->variables + i, &const0);
                else
                {
                    svalue_t sv;
                    const char* err = python_to_svalue(&sv, value);

                    if (err != NULL)
                    {
                        PyErr_SetString(PyExc_ValueError, err);
                        return -1;
                    }

                    transfer_svalue(val->lpc_coroutine->variables + i, &sv);
                }
                return 0;
            }
        }

        PyErr_Format(PyExc_AttributeError, "Variable '%U' not found", name);
        return -1;
    }

} /* ldmud_coroutine_variables_setattro() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_coroutine_variables_dir (ldmud_coroutine_t *val)

/* Return a list of all local variables.
 */

{
    PyObject *result;
    PyObject *attrs = get_class_dir((PyObject*)val);

    if (attrs == NULL)
        return NULL;

    result = PySequence_List(attrs);
    Py_DECREF(attrs);

    if (val->lpc_coroutine && val->lpc_coroutine->prog
      && (val->lpc_coroutine->state == CS_SLEEPING || val->lpc_coroutine->state == CS_AWAITING))
    {
        int num_names = val->lpc_coroutine->num_variable_names;
        string_t **strings = val->lpc_coroutine->prog->strings;
        bytecode_t *p = val->lpc_coroutine->pc - 2*num_names;

        for (int i = 0; i < num_names; i++)
        {
            string_t *var = strings[get_short(p + 2*i)];
            PyObject *varname = PyUnicode_FromStringAndSize(get_txt(var), mstrsize(var));

            if (varname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PyList_Append(result, varname) < 0)
                PyErr_Clear();
            Py_DECREF(varname);
        }
    }

    return result;
} /* ldmud_coroutine_variables_dir() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_coroutine_variables_dict (ldmud_coroutine_t *val, void *closure)

/* Returns a list of all variables.
 */

{
    PyObject *result, *dict = PyDict_New();
    if (!dict)
        return NULL;

    if (val->lpc_coroutine && val->lpc_coroutine->prog
      && (val->lpc_coroutine->state == CS_SLEEPING || val->lpc_coroutine->state == CS_AWAITING))
    {
        int num_names = val->lpc_coroutine->num_variable_names;
        string_t **strings = val->lpc_coroutine->prog->strings;
        bytecode_t *p = val->lpc_coroutine->pc - 2*num_names;
        svalue_t *values = val->lpc_coroutine->variables;

        for (int i = 0; i < num_names; i++)
        {
            string_t *var = strings[get_short(p + 2*i)];
            PyObject *varname = PyUnicode_FromStringAndSize(get_txt(var), mstrsize(var));
            PyObject *value;

            if (varname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            value = svalue_to_python(values + i);
            if (value == NULL)
            {
                PyErr_Clear();
                Py_DECREF(varname);
                continue;
            }

            if (PyDict_SetItem(dict, varname, value) < 0)
                PyErr_Clear();
            Py_DECREF(varname);
            Py_DECREF(value);
        }
    }

    result = PyDictProxy_New(dict);
    Py_DECREF(dict);
    return result;
} /* ldmud_coroutine_variables_dict() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_coroutine_variables_methods[] =
{
    {
        "__dir__",
        (PyCFunction)ldmud_coroutine_variables_dir, METH_NOARGS,
        "__dir__() -> List\n\n"
        "Returns a list of all attributes."
    },

    {NULL}
};

static PyGetSetDef ldmud_coroutine_variables_getset[] =
{
    {"__dict__", (getter)ldmud_coroutine_variables_dict, NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_coroutine_variables_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.CoroutineVariables",         /* tp_name */
    sizeof(ldmud_coroutine_t),          /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_coroutine_dealloc,/* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_coroutine_variables_repr, /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    (getattrofunc)ldmud_coroutine_variables_getattro, /* tp_getattro */
    (setattrofunc)ldmud_coroutine_variables_setattro, /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC coroutine variables",          /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_coroutine_variables_methods,  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_coroutine_variables_getset,   /* tp_getset */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_coroutine_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_coroutine_t, i.e. allocate and initialize
 * the coroutine with null values.
 */

{
    ldmud_coroutine_t *self;

    self = (ldmud_coroutine_t *)type->tp_alloc(type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_coroutine = NULL;
    add_gc_object(&gc_coroutine_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_coroutine_new() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_coroutine_repr (ldmud_coroutine_t *val)

/* Return a string representation of this coroutine.
 */

{
    function_t *fun;
    if (val->lpc_coroutine == NULL)
        return PyUnicode_FromString("<LPC uninitialized coroutine>");
    if (val->lpc_coroutine->prog == NULL)
        return PyUnicode_FromString("<LPC destructed coroutine>");

    fun = val->lpc_coroutine->prog->function_headers + FUNCTION_HEADER_INDEX(val->lpc_coroutine->funstart);

    switch (val->lpc_coroutine->ob.type)
    {
        case T_OBJECT:
            return PyUnicode_FromFormat("<LPC coroutine /%s->%s()>", get_txt(val->lpc_coroutine->ob.u.ob->name), get_txt(fun->name));
        case T_LWOBJECT:
            return PyUnicode_FromFormat("<LPC coroutine /%s->%s()>", get_txt(val->lpc_coroutine->ob.u.lwob->prog->name), get_txt(fun->name));
        default:
            return PyUnicode_FromString("<LPC destructed coroutine>");
    }
} /* ldmud_coroutine_repr() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_coroutine_hash (ldmud_coroutine_t *val)

/* Return a hash of this coroutine.
 */

{
    return _Py_HashPointer(val->lpc_coroutine);
} /* ldmud_coroutine_hash() */

/*-------------------------------------------------------------------------*/
static bool ldmud_coroutine_check(PyObject *ob);

static PyObject*
ldmud_coroutine_richcompare (ldmud_coroutine_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    if (!ldmud_coroutine_check(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    return pointer_richcompare(self->lpc_coroutine, ((ldmud_coroutine_t*)other)->lpc_coroutine, op);
} /* ldmud_coroutine_richcompare() */

/*-------------------------------------------------------------------------*/
static int
ldmud_coroutine_bool(ldmud_coroutine_t *val)

/* Return 0 (false) for coroutines of destructed objects, 1 (true) for normal objects.
 */

{
    if (!val->lpc_coroutine)
        return 0;
    return valid_coroutine(val->lpc_coroutine);
} /* ldmud_coroutine_bool() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_coroutine_get_object (ldmud_coroutine_t *val, void *closure)

/* Return the value for the object member.
 */

{
    if(!val->lpc_coroutine)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return svalue_to_python(&(val->lpc_coroutine->ob));
} /* ldmud_coroutine_get_object() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_coroutine_get_program_name (ldmud_coroutine_t *val, void *closure)

/* Return the value for the program_name member.
 */

{
    if(!val->lpc_coroutine || !val->lpc_coroutine->prog)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyUnicode_FromFormat("/%s", get_txt(val->lpc_coroutine->prog->name));
} /* ldmud_coroutine_get_program_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_coroutine_get_function_name (ldmud_coroutine_t *val, void *closure)

/* Return the value for the function_name member.
 */

{
    string_t * name;
    if(!val->lpc_coroutine || !val->lpc_coroutine->prog)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    name = val->lpc_coroutine->prog->function_headers[FUNCTION_HEADER_INDEX(val->lpc_coroutine->funstart)].name;
    return PyUnicode_FromStringAndSize(get_txt(name), mstrsize(name));
} /* ldmud_coroutine_get_function_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_coroutine_get_file_name (ldmud_coroutine_t *val, void *closure)

/* Return the value for the file_name member.
 */

{
    string_t * name;
    string_t * fname;

    if(!val->lpc_coroutine || !val->lpc_coroutine->prog)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    get_line_number(val->lpc_coroutine->pc, val->lpc_coroutine->prog, &name, &fname);
    free_mstring(name);
    if (fname == NULL)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyUnicode_FromFormat("/%s", get_txt(fname));
} /* ldmud_coroutine_get_file_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_coroutine_get_line_number (ldmud_coroutine_t *val, void *closure)

/* Return the value for the line_number member.
 */

{
    string_t * name;
    int pos;

    if(!val->lpc_coroutine || !val->lpc_coroutine->prog)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    pos = get_line_number(val->lpc_coroutine->pc, val->lpc_coroutine->prog, &name, NULL);
    free_mstring(name);

    if (pos == 0)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyLong_FromLong(pos);
} /* ldmud_coroutine_get_line_number() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_coroutine_get_variables (ldmud_coroutine_t *val, void *closure)

/* Return the value for the variables member.
 */

{
    ldmud_coroutine_t *result;

    if (!val->lpc_coroutine || !val->lpc_coroutine->prog
      || (val->lpc_coroutine->state != CS_SLEEPING && val->lpc_coroutine->state != CS_AWAITING))
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    result = (ldmud_coroutine_t*)ldmud_coroutine_variables_type.tp_alloc(&ldmud_coroutine_variables_type, 0);
    if (result == NULL)
        return NULL;

    result->lpc_coroutine = ref_coroutine(val->lpc_coroutine);
    add_gc_object(&gc_coroutine_list, (ldmud_gc_var_t*)result);

    return (PyObject *)result;
} /* ldmud_coroutine_get_variables() */

/*-------------------------------------------------------------------------*/
static void
ldmud_coroutine_call_coroutine (int num_arg, void* data)

/* Helper function for ldmud_coroutine_call().
 */

{
    inter_sp = f_call_coroutine(inter_sp);
} /* ldmud_coroutine_call_coroutine() */


static PyObject*
ldmud_coroutine_call (ldmud_coroutine_t *cr, PyObject *arg, PyObject *kw)

/* Implement the call operator for coroutines.
 */

{
    int num_arg = (int)PyTuple_GET_SIZE(arg);
    coroutine_t * target = cr->lpc_coroutine;

    if(python_is_external ? (!master_ob) : (current_object.type == T_NUMBER))
    {
        PyErr_SetString(PyExc_RuntimeError, "can't call a coroutine without a current object");
        return NULL;
    }

    if (kw != NULL && PyDict_Size(kw) != 0)
    {
        PyErr_SetString(PyExc_TypeError, "coroutine call takes no keyword arguments");
        return NULL;
    }
    else if (num_arg > 1)
    {
        PyErr_SetString(PyExc_TypeError, "coroutine call takes only a single argument");
        return NULL;
    }
    else if (target == NULL)
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized lpc coroutine");
        return NULL;
    }
    else if (!valid_coroutine(target))
    {
        PyErr_SetString(PyExc_TypeError, "expired lpc coroutine");
        return NULL;
    }
    else if (!(target = get_resumable_coroutine(target)))
    {
        PyErr_SetString(PyExc_TypeError, "coroutine is not resumable");
        return NULL;
    }
    else
    {
        svalue_t *sp = inter_sp;
        PyObject *result;
        const char* err;

        /* Put all arguments on the stack. */
        push_ref_coroutine(sp, target);

        if (num_arg == 1)
        {
            err = python_to_svalue(++sp, PyTuple_GetItem(arg, 0));
            if (err != NULL)
            {
                free_coroutine(target);

                PyErr_SetString(PyExc_ValueError, err);
                return NULL;
            }
        }
        else
            push_number(sp, 0);

        inter_sp = sp;

        if(call_lpc_secure((CClosureFun)ldmud_coroutine_call_coroutine, 2, NULL))
        {
            result = svalue_to_python(inter_sp);
            pop_stack();
        }
        else
            result = NULL;

        return result;
    }

    return NULL;
} /* ldmud_coroutine_call() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_coroutine_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Coroutine.
 */

{
    return lpctype_coroutine;
} /* ldmud_coroutine_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_coroutine_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    (inquiry)ldmud_coroutine_bool,      /* nb_bool */
};

static PyMethodDef ldmud_coroutine_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_coroutine_getset[] =
{
    {"object",        (getter)ldmud_coroutine_get_object,        NULL, NULL, NULL},
    {"program_name",  (getter)ldmud_coroutine_get_program_name,  NULL, NULL, NULL},
    {"function_name", (getter)ldmud_coroutine_get_function_name, NULL, NULL, NULL},
    {"file_name",     (getter)ldmud_coroutine_get_file_name,     NULL, NULL, NULL},
    {"line_number",   (getter)ldmud_coroutine_get_line_number,   NULL, NULL, NULL},
    {"variables",     (getter)ldmud_coroutine_get_variables,     NULL, NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_coroutine_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.Coroutine",                  /* tp_name */
    sizeof(ldmud_coroutine_t),          /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_coroutine_dealloc,/* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_coroutine_repr,     /* tp_repr */
    &ldmud_coroutine_as_number,         /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_coroutine_hash,     /* tp_hash  */
    (ternaryfunc)ldmud_coroutine_call,  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC coroutine",                    /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_coroutine_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_coroutine_methods,            /* tp_methods */
    0,                                  /* tp_members */
    ldmud_coroutine_getset,             /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_coroutine_new,                /* tp_new */
},  ldmud_coroutine_get_lpctype         /* get_lpctype */
};


/*-------------------------------------------------------------------------*/
static bool
ldmud_coroutine_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC coroutine type.
 */

{
    return Py_TYPE(ob) == &ldmud_coroutine_type.type_base;
} /* ldmud_coroutine_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_coroutine_create (coroutine_t* cr)

/* Creates a new Python coroutine from an LPC coroutine.
 */

{
    ldmud_coroutine_t *self;

    self = (ldmud_coroutine_t *)ldmud_coroutine_type.type_base.tp_alloc(&ldmud_coroutine_type.type_base, 0);
    if (self == NULL)
        return NULL;

    self->lpc_coroutine = ref_coroutine(cr);
    add_gc_object(&gc_coroutine_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_coroutine_create() */

/*-------------------------------------------------------------------------*/
/* Symbols */
static bool ldmud_symbol_check(PyObject *ob);

static void
ldmud_symbol_dealloc (ldmud_symbol_t* self)

/* Destroy the ldmud_symbol_t object
 */

{
    free_svalue(&self->lpc_symbol);

    remove_gc_object(&gc_symbol_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_symbol_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_symbol_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_symbol_t, i.e. allocate and initialize
 * the symbol with null values.
 */

{
    ldmud_symbol_t *self;

    self = (ldmud_symbol_t *)type->tp_alloc(type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_symbol.type = T_INVALID;
    add_gc_object(&gc_symbol_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_symbol_new() */

/*-------------------------------------------------------------------------*/
static int
ldmud_symbol_init (ldmud_symbol_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_symbol_t, i.e. create a new symbol object
 * from the given arguments.
 */

{
    /* We expect:
     *  - the name of this symbol,
     *  - and optionally the number of quotes (at least 1)
     */

    static char *kwlist[] = { "name", "quotes", NULL};

    Py_ssize_t length;
    const char *name;
    int quotes = 1;
    string_t * str;

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "s#|i", kwlist,
                                      &name, &length, &quotes))
        return -1;

    if(quotes < 1)
    {
        PyErr_SetString(PyExc_ValueError, "need at least one quote");
        return -1;
    }

    str = new_n_tabled(name, length, STRING_UTF8);
    if (str == NULL)
    {
        PyErr_NoMemory();
        return -1;
    }

    free_svalue(&self->lpc_symbol);
    put_symbol(&self->lpc_symbol, str, quotes);
    return 0;
} /* ldmud_symbol_init() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_symbol_hash (ldmud_symbol_t *val)

/* Return a hash of this symbol.
 */

{
    return _Py_HashPointer(val->lpc_symbol.u.str) ^ val->lpc_symbol.x.quotes;
} /* ldmud_symbol_hash() */

/*-------------------------------------------------------------------------*/

static PyObject*
ldmud_symbol_richcompare (ldmud_symbol_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    svalue_t *self_sym, *other_sym;
    bool result;
    PyObject* resultval;

    if (!ldmud_symbol_check(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    self_sym = &self->lpc_symbol;
    other_sym = &((ldmud_symbol_t*)other)->lpc_symbol;

    if(self_sym->type != T_SYMBOL && other_sym->type != T_SYMBOL)
        result = op == Py_LE || op == Py_EQ || op == Py_GE;
    else if(self_sym->type != T_SYMBOL)
        result = op == Py_LT || op == Py_LE || op == Py_NE;
    else if(other_sym->type != T_SYMBOL)
        result = op == Py_GT || op == Py_GE || op == Py_NE;
    else
    {
        int cmp = mstrcmp(self_sym->u.str, other_sym->u.str);
        if (cmp == 0)
            cmp = self_sym->x.quotes == other_sym->x.quotes ? 0
                : self_sym->x.quotes < other_sym->x.quotes ? -1 : 1;

        switch (op)
        {
            case Py_LT: result = cmp < 0; break;
            case Py_LE: result = cmp <= 0; break;
            case Py_EQ: result = cmp == 0; break;
            case Py_NE: result = cmp != 0; break;
            case Py_GT: result = cmp > 0; break;
            case Py_GE: result = cmp >= 0; break;
            default:
            {
                Py_INCREF(Py_NotImplemented);
                return Py_NotImplemented;
            }
        }
    }

    resultval = result ? Py_True : Py_False;
    Py_INCREF(resultval);
    return resultval;
} /* ldmud_symbol_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_symbol_get_name (ldmud_symbol_t *val, void *closure)

/* Return the value for the name member.
 */

{
    if (val->lpc_symbol.type != T_SYMBOL)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyUnicode_Decode(get_txt(val->lpc_symbol.u.str), mstrsize(val->lpc_symbol.u.str), "utf-8", "replace");
} /* ldmud_symbol_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_symbol_get_quotes (ldmud_symbol_t *val, void *closure)

/* Return the value for the quotes member.
 */

{
    if (val->lpc_symbol.type != T_SYMBOL)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }
    return PyLong_FromLong(val->lpc_symbol.x.quotes);
} /* ldmud_symbol_get_quotes() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_symbol_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Symbol.
 */

{
    return lpctype_symbol;
} /* ldmud_symbol_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_symbol_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_symbol_getset[] =
{
    {"name",   (getter)ldmud_symbol_get_name,   NULL, NULL},
    {"quotes", (getter)ldmud_symbol_get_quotes, NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_symbol_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.Symbol",                     /* tp_name */
    sizeof(ldmud_symbol_t),             /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_symbol_dealloc,   /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_symbol_hash,        /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC symbol",                       /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_symbol_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_symbol_methods,               /* tp_methods */
    0,                                  /* tp_members */
    ldmud_symbol_getset,                /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_symbol_init,        /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_symbol_new,                   /* tp_new */
},  ldmud_symbol_get_lpctype            /* get_lpctype */
};


/*-------------------------------------------------------------------------*/
static bool
ldmud_symbol_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC symbol type.
 */

{
    return Py_TYPE(ob) == &ldmud_symbol_type.type_base;
} /* ldmud_symbol_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_symbol_create (svalue_t* sym)

/* Creates a new Python symbol from an LPC symbol.
 */

{
    ldmud_symbol_t *self;

    self = (ldmud_symbol_t *)ldmud_symbol_type.type_base.tp_alloc(&ldmud_symbol_type.type_base, 0);
    if (self == NULL)
        return NULL;

    assign_svalue_no_free(&self->lpc_symbol, sym);
    add_gc_object(&gc_symbol_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_symbol_create() */

/*-------------------------------------------------------------------------*/
/* Quoted Arrays */
static bool ldmud_quoted_array_check(PyObject *ob);

static void
ldmud_quoted_array_dealloc (ldmud_quoted_array_t* self)

/* Destroy the ldmud_quoted_array_t object
 */

{
    free_svalue(&self->lpc_quoted_array);

    remove_gc_object(&gc_quoted_array_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_quoted_array_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_quoted_array_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_quoted_array_t, i.e. allocate and initialize
 * the quoted_array with null values.
 */

{
    ldmud_quoted_array_t *self;

    self = (ldmud_quoted_array_t *)type->tp_alloc(type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_quoted_array.type = T_INVALID;
    add_gc_object(&gc_quoted_array_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_quoted_array_new() */

/*-------------------------------------------------------------------------*/
static int
ldmud_quoted_array_init (ldmud_quoted_array_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_quoted_array_t, i.e. create a new quoted_array object
 * from the given arguments.
 */

{
    /* We expect:
     *  - the base array (can also be an already quoted array),
     *  - and optionally the number of quotes (at least 1)
     */

    static char *kwlist[] = { "array", "quotes", NULL};

    PyObject *arr;
    int quotes = 1;
    vector_t *vec;

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "O|i", kwlist,
                                      &arr, &quotes))
        return -1;

    if(quotes < 1)
    {
        PyErr_SetString(PyExc_ValueError, "need at least one quote");
        return -1;
    }

    if (ldmud_array_check(arr))
        vec = ((ldmud_array_t*)arr)->lpc_array;
    else if(ldmud_quoted_array_check(arr))
    {
        svalue_t *sval = &((ldmud_quoted_array_t*)arr)->lpc_quoted_array;
        if (sval->type != T_QUOTED_ARRAY)
        {
            PyErr_SetString(PyExc_ValueError, "uninitialized quoted array");
            return -1;
        }

        vec = sval->u.vec;
        quotes += sval->x.quotes;
    }
    else
    {
        PyErr_Format(PyExc_TypeError, "can only quote ldmud.Array or ldmud.QuotedArray (not \"%.200s\")",
                     Py_TYPE(arr)->tp_name);
        return -1;
    }

    free_svalue(&self->lpc_quoted_array);
    put_ref_array(&self->lpc_quoted_array, vec);
    self->lpc_quoted_array.type = T_QUOTED_ARRAY;
    self->lpc_quoted_array.x.quotes = quotes;

    return 0;
} /* ldmud_quoted_array_init() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_quoted_array_hash (ldmud_quoted_array_t *val)

/* Return a hash of this array.
 */

{
    if (val->lpc_quoted_array.type != T_QUOTED_ARRAY)
        return 0;

    return _Py_HashPointer(val->lpc_quoted_array.u.vec) ^ val->lpc_quoted_array.x.quotes;
} /* ldmud_quoted_array_hash() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_quoted_array_richcompare (ldmud_quoted_array_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    vector_t *arr1, *arr2;

    if (!ldmud_quoted_array_check(other))
        Py_RETURN_NOTIMPLEMENTED;

    arr1 = (self->lpc_quoted_array.type == T_QUOTED_ARRAY) ? self->lpc_quoted_array.u.vec : NULL;
    arr2 = (((ldmud_quoted_array_t*)other)->lpc_quoted_array.type == T_QUOTED_ARRAY) ? ((ldmud_quoted_array_t*)other)->lpc_quoted_array.u.vec : NULL;

    if (arr1 != arr2 || arr1 == NULL)
        return pointer_richcompare(arr1, arr2, op);
    else
        Py_RETURN_RICHCOMPARE(self->lpc_quoted_array.x.quotes, ((ldmud_quoted_array_t*)other)->lpc_quoted_array.x.quotes, op);
} /* ldmud_quoted_array_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_quoted_array_get_array (ldmud_quoted_array_t *val, void *closure)

/* Return the value for the array member.
 */

{
    if (val->lpc_quoted_array.type != T_QUOTED_ARRAY)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return ldmud_array_create(val->lpc_quoted_array.u.vec);
} /* ldmud_quoted_array_get_array() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_quoted_array_get_quotes (ldmud_quoted_array_t *val, void *closure)

/* Return the value for the quotes member.
 */

{
    if (val->lpc_quoted_array.type != T_QUOTED_ARRAY)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }
    return PyLong_FromLong(val->lpc_quoted_array.x.quotes);
} /* ldmud_quoted_array_get_quotes() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_quoted_array_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.QuotedArray.
 */

{
    return lpctype_quoted_array;
} /* ldmud_quoted_array_get_lpctype() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_quoted_array_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_quoted_array_getset[] =
{
    {"array",  (getter)ldmud_quoted_array_get_array,  NULL, NULL},
    {"quotes", (getter)ldmud_quoted_array_get_quotes, NULL, NULL},
    {NULL}
};

static ldmud_lpctype_t ldmud_quoted_array_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.QuotedArray",                /* tp_name */
    sizeof(ldmud_quoted_array_t),       /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_quoted_array_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_quoted_array_hash,  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC quoted array",                 /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_quoted_array_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_quoted_array_methods,         /* tp_methods */
    0,                                  /* tp_members */
    ldmud_quoted_array_getset,          /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_quoted_array_init,  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_quoted_array_new,             /* tp_new */
},  ldmud_quoted_array_get_lpctype      /* get_lpctype */
};


/*-------------------------------------------------------------------------*/
static bool
ldmud_quoted_array_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC quoted_array type.
 */

{
    return Py_TYPE(ob) == &ldmud_quoted_array_type.type_base;
} /* ldmud_quoted_array_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_quoted_array_create (svalue_t* sym)

/* Creates a new Python quoted_array from an LPC quoted_array.
 */

{
    ldmud_quoted_array_t *self;

    self = (ldmud_quoted_array_t *)ldmud_quoted_array_type.type_base.tp_alloc(&ldmud_quoted_array_type.type_base, 0);
    if (self == NULL)
        return NULL;

    assign_svalue_no_free(&self->lpc_quoted_array, sym);
    add_gc_object(&gc_quoted_array_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_quoted_array_create() */

/*-------------------------------------------------------------------------*/
/* Lvalues */

static PyObject* ldmud_lvalue_create(svalue_t* lv);

static PyObject*
ldmud_lvalue_struct_members_getattro (ldmud_struct_t *self, PyObject *name)

/* Implement __getattr__ for ldmud_struct_t.
 * This is a direct struct member lookup.
 */

{
    PyObject *result;
    string_t* member_name;
    bool error;
    int idx;

    /* First check real attributes... */
    result = PyObject_GenericGetAttr((PyObject *)self, name);
    if (result || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return result;

    if (!self->lpc_struct)
        return NULL;

    PyErr_Clear();

    /* And now search for a member. */
    member_name = find_tabled_python_string(name, "member name", &error);
    if (error)
        return NULL;

    idx = member_name ? struct_find_member(self->lpc_struct->type, member_name) : -1;
    if (idx < 0)
    {
        PyErr_Format(PyExc_AttributeError, "Struct object has no member '%U'", name);
        return NULL;
    }
    else
        return ldmud_lvalue_create(self->lpc_struct->member+idx);

} /* ldmud_lvalue_struct_members_getattro() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lvalue_struct_members_dict (ldmud_struct_t *self, void *closure)

/* Returns a list of all members.
 */

{
    PyObject *result, *dict = PyDict_New();
    if (!dict)
        return NULL;

    if (self->lpc_struct)
    {
        struct_type_t *st = self->lpc_struct->type;
        for (unsigned short ix = 0; ix < st->num_members; ix++)
        {
            string_t *mem = st->member[ix].name;
            PyObject *memname = PyUnicode_FromStringAndSize(get_txt(mem), mstrsize(mem));
            PyObject *memob;

            if (memname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            memob = ldmud_lvalue_create(self->lpc_struct->member+ix);
            if (memob == NULL)
            {
                PyErr_Clear();
                Py_DECREF(memname);
                continue;
            }

            if (PyDict_SetItem(dict, memname, memob) < 0)
                PyErr_Clear();
            Py_DECREF(memname);
            Py_DECREF(memob);
        }
    }

    result = PyDictProxy_New(dict);
    Py_DECREF(dict);
    return result;
} /* ldmud_lvalue_struct_members_dict() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_lvalue_struct_members_methods[] =
{
    {
        "__dir__",
        (PyCFunction)ldmud_struct_members_dir, METH_NOARGS,
        "__dir__() -> List\n\n"
        "Returns a list of all attributes."
    },

    {NULL}
};

static PyGetSetDef ldmud_lvalue_struct_members_getset [] = {
    {"__dict__", (getter)ldmud_lvalue_struct_members_dict, NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_lvalue_struct_members_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.LvalueStructMembers",        /* tp_name */
    sizeof(ldmud_struct_t),             /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_struct_dealloc,   /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_struct_members_repr,/* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    (getattrofunc)ldmud_lvalue_struct_members_getattro, /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC lvalue struct member list",    /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_lvalue_struct_members_methods,/* tp_methods */
    0,                                  /* tp_members */
    ldmud_lvalue_struct_members_getset, /* tp_getset */
};

/*-------------------------------------------------------------------------*/
static void
ldmud_lvalue_dealloc (ldmud_lvalue_t* self)

/* Destroy the ldmud_lvalue_t object
 */

{
    free_svalue(&self->lpc_lvalue);

    remove_gc_object(&gc_lvalue_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_lvalue_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lvalue_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for ldmud_lvalue_t, i.e. allocate and initialize
 * the lvalue with null values.
 */

{
    ldmud_lvalue_t *self;

    self = (ldmud_lvalue_t *)type->tp_alloc(type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_lvalue.type = T_INVALID;
    add_gc_object(&gc_lvalue_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_lvalue_new() */

/*-------------------------------------------------------------------------*/
static int
ldmud_lvalue_init (ldmud_lvalue_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_lvalue_t, i.e. create a new lvalue.
 */

{
    /* This function will convert a Python value to an LPC value, so it
     * can be accessed as an lvalue in LPC. 
     */

    static char *kwlist[] = { "value", NULL};
    PyObject *value = Py_None;
    const char* err;
    svalue_t lpcval;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O", kwlist, &value))
        return -1;

    err = python_to_svalue(&lpcval, value);
    if (err)
    {
        PyErr_SetString(PyExc_ValueError, err);
        return -1;
    }

    free_svalue(&(self->lpc_lvalue));
    assign_protected_lvalue_no_free(&(self->lpc_lvalue), &lpcval);
    free_svalue(&lpcval);

    return 0;
} /* ldmud_lvalue_init() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_lvalue_hash (ldmud_lvalue_t *val)

/* Return a hash of this lvalue.
 */

{
    return _Py_HashPointer(val->lpc_lvalue.u.generic) ^ val->lpc_lvalue.x.lvalue_type;
} /* ldmud_lvalue_hash() */

/*-------------------------------------------------------------------------*/
static bool ldmud_lvalue_check(PyObject *ob);

static PyObject*
ldmud_lvalue_richcompare (ldmud_lvalue_t *self, PyObject *other, int op)

/* Compare <self> to <other> with the compare operation <op>.
 */

{
    svalue_t *self_lv, *other_lv;
    bool result;
    PyObject* resultval;

    if (!ldmud_lvalue_check(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    self_lv = &self->lpc_lvalue;
    other_lv = &((ldmud_lvalue_t*)other)->lpc_lvalue;

    if(self_lv->x.lvalue_type != other_lv->x.lvalue_type)
    {
        switch (op)
        {
            case Py_LT: result = self_lv->x.lvalue_type <  other_lv->x.lvalue_type; break;
            case Py_LE: result = self_lv->x.lvalue_type <= other_lv->x.lvalue_type; break;
            case Py_EQ: result = self_lv->x.lvalue_type == other_lv->x.lvalue_type; break;
            case Py_NE: result = self_lv->x.lvalue_type != other_lv->x.lvalue_type; break;
            case Py_GT: result = self_lv->x.lvalue_type >  other_lv->x.lvalue_type; break;
            case Py_GE: result = self_lv->x.lvalue_type >= other_lv->x.lvalue_type; break;
            default:
            {
                Py_INCREF(Py_NotImplemented);
                return Py_NotImplemented;
            }
        }
    }
    else
    {
        switch (op)
        {
            case Py_LT: result = self_lv->u.generic <  other_lv->u.generic; break;
            case Py_LE: result = self_lv->u.generic <= other_lv->u.generic; break;
            case Py_EQ: result = self_lv->u.generic == other_lv->u.generic; break;
            case Py_NE: result = self_lv->u.generic != other_lv->u.generic; break;
            case Py_GT: result = self_lv->u.generic >  other_lv->u.generic; break;
            case Py_GE: result = self_lv->u.generic >= other_lv->u.generic; break;
            default:
            {
                Py_INCREF(Py_NotImplemented);
                return Py_NotImplemented;
            }
        }
    }

    resultval = result ? Py_True : Py_False;
    Py_INCREF(resultval);
    return resultval;
} /* ldmud_lvalue_richcompare() */

/*-------------------------------------------------------------------------*/
static Py_ssize_t
ldmud_lvalue_length (ldmud_lvalue_t *self)

/* Implement len() for ldmud_lvalue_t.
 */

{
    struct range_iterator it;

    if (self->lpc_lvalue.type == T_INVALID)
    {
        PyErr_SetString(PyExc_ValueError, "empty lvalue given");
        return -1;
    }

    assert(self->lpc_lvalue.type == T_LVALUE);

    if (!get_iterator(&(self->lpc_lvalue), &it, false))
    {
        PyErr_SetString(PyExc_TypeError, "lvalue has no length");
        return -1;
    }

    return it.size;
} /* ldmud_lvalue_length() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lvalue_item (ldmud_lvalue_t *self, Py_ssize_t idx)

/* Implement item access for ldmud_lvalue_t.
 *
 * Here we just handle member access to bytes, strings and arrays.
 * All other (slices of the above and mappings) are handled
 * by ldmud_lvalue_subscript().
 */

{
    svalue_t *vec;

    if (self->lpc_lvalue.type == T_INVALID)
    {
        PyErr_SetString(PyExc_ValueError, "empty lvalue given");
        return NULL;
    }

    assert(self->lpc_lvalue.type == T_LVALUE);

    vec = get_rvalue_no_collapse(&(self->lpc_lvalue), NULL);
    if (vec == NULL)
    {
        /* We have a (map) range lvalue. */
        switch (self->lpc_lvalue.x.lvalue_type)
        {
            case LVALUE_PROTECTED_RANGE:
            {
                struct protected_range_lvalue *r = self->lpc_lvalue.u.protected_range_lvalue;
                Py_ssize_t len = r->index2 - r->index1;

                if (len < 0)
                    len = 0;

                switch (r->vec.type)
                {
                    case T_POINTER:
                        if (idx < 0 || idx >= len)
                        {
                            PyErr_SetString(PyExc_IndexError, "index out of range");
                            return NULL;
                        }

                        return ldmud_lvalue_create(r->vec.u.vec->item + r->index1 + idx);

                    case T_STRING:
                    case T_BYTES:
                    {
                        PyObject* ob;
                        svalue_t result;
                        string_t *p;
                        ssize_t pos, num;

                        if (r->vec.u.str->info.unicode == STRING_UTF8)
                            num = byte_to_char_index(get_txt(r->vec.u.str) + r->index1, len, NULL);
                        else
                            num = len;

                        if (idx < 0 || idx >= num)
                        {
                            PyErr_SetString(PyExc_IndexError, "index out of range");
                            return NULL;
                        }

                        p = make_mutable(r->vec.u.str);
                        if (!p)
                            return PyErr_NoMemory();

                        if (r->vec.u.str != p)
                        {
                            if (r->var != NULL && r->var->val.type == r->vec.type && r->var->val.u.str == r->vec.u.str)
                            {
                                /* Update the corresponding variable. */
                                free_mstring(r->var->val.u.str);
                                r->var->val.u.str = ref_mstring(p);
                            }
                            r->vec.u.str = p;
                        }

                        if (r->vec.u.str->info.unicode == STRING_UTF8)
                            pos = char_to_byte_index(get_txt(p) + r->index1, len, idx, NULL);
                        else
                            pos = idx;

                        assign_protected_char_lvalue_no_free(&result, r->var, p, get_txt(p) + r->index1 + pos);
                        ob = ldmud_lvalue_create(&result);
                        free_svalue(&result);

                        return ob;
                    }

                    default:
                        fatal("Illegal type for range lvalue '%s'.\n", sv_typename(&(r->vec)));
                }
            }

            case LVALUE_PROTECTED_MAP_RANGE:
            {
                struct protected_map_range_lvalue *r = self->lpc_lvalue.u.protected_map_range_lvalue;
                Py_ssize_t len = r->index2 - r->index1;
                svalue_t *value;

                if (len < 0)
                    len = 0;

                if (idx < 0 || idx >= len)
                {
                    PyErr_SetString(PyExc_IndexError, "index out of range");
                    return NULL;
                }

                value = get_map_value(r->map, &(r->key));
                if (value == &const0)
                {
                    PyObject *ob;
                    svalue_t result;

                    assign_protected_mapentry_lvalue_no_free(&result, r->map, &(r->key), r->index1 + idx);
                    ob = ldmud_lvalue_create(&result);
                    free_svalue(&result);

                    return ob;
                }
                else
                    return ldmud_lvalue_create(value + r->index1 + idx);
            }

            default:
                fatal("Illegal lvalue type %d\n",self->lpc_lvalue.x.lvalue_type);
                break;
        }
    }
    else
    {
        switch (vec->type)
        {
            case T_POINTER:
                if (idx < 0 || idx >= VEC_SIZE(vec->u.vec))
                {
                    PyErr_SetString(PyExc_IndexError, "index out of range");
                    return NULL;
                }

                return ldmud_lvalue_create(vec->u.vec->item + idx);

            case T_STRING:
            case T_BYTES:
            {
                PyObject* ob;
                svalue_t result;
                string_t *p;
                ssize_t pos, num, len = mstrsize(vec->u.str);

                if (vec->u.str->info.unicode == STRING_UTF8)
                    num = byte_to_char_index(get_txt(vec->u.str), len, NULL);
                else
                    num = len;

                if (idx < 0 || idx >= num)
                {
                    PyErr_SetString(PyExc_IndexError, "index out of range");
                    return NULL;
                }

                p = make_mutable(vec->u.str);
                if (!p)
                    return PyErr_NoMemory();

                vec->u.str = p;

                if (vec->u.str->info.unicode == STRING_UTF8)
                    pos = char_to_byte_index(get_txt(p), len, idx, NULL);
                else
                    pos = idx;

                assert(self->lpc_lvalue.x.lvalue_type == LVALUE_PROTECTED);
                assign_protected_char_lvalue_no_free(&result, self->lpc_lvalue.u.protected_lvalue, p, get_txt(p) + pos);
                ob = ldmud_lvalue_create(&result);
                free_svalue(&result);

                return ob;
            }

            default:
            {
                PyErr_SetString(PyExc_TypeError, "lvalue cannot be indexed");
                return NULL;
            }
        }
    }

    return NULL;
} /* ldmud_lvalue_item() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lvalue_subscript (ldmud_lvalue_t *self, PyObject *key)

/* Implement item access for ldmud_lvalue_t.
 *
 * Integer indices into arrays, strings and bytes are forwarded to
 * ldmud_lvalue_item(). This function handles ranges and mapping indexing.
 */

{
    svalue_t *vec;

    if (self->lpc_lvalue.type == T_INVALID)
    {
        PyErr_SetString(PyExc_ValueError, "empty lvalue given");
        return NULL;
    }

    assert(self->lpc_lvalue.type == T_LVALUE);

    vec = get_rvalue_no_collapse(&(self->lpc_lvalue), NULL);
    if (vec == NULL
      || vec->type == T_POINTER
      || vec->type == T_STRING
      || vec->type == T_BYTES)
    {
        if (PyIndex_Check(key))
        {
            Py_ssize_t idx = PyNumber_AsSsize_t(key, PyExc_IndexError);
            if (idx == -1 && PyErr_Occurred())
                return NULL;
            if (idx < 0)
                idx += ldmud_lvalue_length(self);

            return ldmud_lvalue_item(self, idx);
        }
        else if (PySlice_Check(key))
        {
            PyObject *ob;
            struct protected_lvalue *var;
            svalue_t result;
            Py_ssize_t offset, length = 0, size = 0;
            Py_ssize_t start, stop, step, slicelength;

            if (vec == NULL)
            {
                /* We have a (map) range lvalue. */
                switch (self->lpc_lvalue.x.lvalue_type)
                {
                    case LVALUE_PROTECTED_RANGE:
                    {
                        struct protected_range_lvalue *r;

                        r = self->lpc_lvalue.u.protected_range_lvalue;

                        vec = &(r->vec);
                        var = r->var;
                        offset = r->index1;
                        size = length = r->index2 - r->index1;
                        if (length <= 0)
                            length = 0;

                        if (r->vec.type == T_STRING && r->vec.u.str->info.unicode == STRING_UTF8)
                            length = (Py_ssize_t)byte_to_char_index(get_txt(r->vec.u.str) + offset, length, NULL);
                        break;
                    }

                    case LVALUE_PROTECTED_MAP_RANGE:
                    {
                        struct protected_map_range_lvalue *r = self->lpc_lvalue.u.protected_map_range_lvalue;

                        if (PySlice_GetIndicesEx(key, r->index2 - r->index1, &start, &stop, &step, &slicelength) < 0)
                            return NULL;

                        if (slicelength <= 0)
                        {
                            PyObject *arr_result;

                            put_ref_array(&result, &null_vector);
                            arr_result = ldmud_lvalue_create(&result);
                            free_svalue(&result);
                            return arr_result;
                        }

                        if (step == 1)
                        {
                            /* We generate another map range. */
                            assign_protected_map_range_lvalue_no_free(&result, r->map, &(r->key), r->index1 + start, r->index1 + stop);
                            ob = ldmud_lvalue_create(&result);
                            free_svalue(&result);

                            return ob;
                        }
                        else
                        {
                            /* We generate an array of lvalues to the selected values. */
                            void (*save_handler)(const char *, ...);
                            svalue_t *values;
                            vector_t *arr;
                            PyObject *arr_result;

                            save_handler = allocate_array_error_handler;
                            allocate_array_error_handler = python_error_handler;
                            arr = allocate_array_unlimited(slicelength);
                            allocate_array_error_handler = save_handler;

                            if (arr == NULL)
                                return PyErr_NoMemory();

                            values = get_map_value(r->map, &(r->key));
                            if (values == &const0)
                            {
                                /* Key does not exist, create this as a
                                 * list of map entry lvalues.
                                 */
                                for (Py_ssize_t cur = start, i = 0; i < slicelength; cur += step, i++)
                                    assign_protected_mapentry_lvalue_no_free(arr->item + i, r->map, &(r->key), r->index1 + cur);
                            }
                            else
                            {
                                /* Create lvalues to the entries. */
                                for (Py_ssize_t cur = start, i = 0; i < slicelength; cur += step, i++)
                                    assign_protected_lvalue_no_free(arr->item + i, values + r->index1 + cur);
                            }

                            put_array(&result, arr);
                            arr_result = ldmud_lvalue_create(&result);
                            free_svalue(&result);
                            return arr_result;
                        }
                    }

                    default:
                        fatal("Illegal lvalue type %d\n",self->lpc_lvalue.x.lvalue_type);
                        return NULL;
                }
            }
            else
            {
                assert(self->lpc_lvalue.x.lvalue_type == LVALUE_PROTECTED);

                var = self->lpc_lvalue.u.protected_lvalue;
                offset = 0;
                switch (vec->type)
                {
                    case T_POINTER:
                        size = length = VEC_SIZE(vec->u.vec);
                        break;

                    case T_STRING:
                    case T_BYTES:
                    {
                        string_t *str = make_mutable(vec->u.str);
                        if (!str)
                            return PyErr_NoMemory();

                        vec->u.str = str;
                        size = length = mstrsize(str);
                        if (str->info.unicode == STRING_UTF8)
                            length = byte_to_char_index(get_txt(str), length, NULL);
                        break;
                    }
                }
            }

            if (PySlice_GetIndicesEx(key, length, &start, &stop, &step, &slicelength) < 0)
                return NULL;

            if (step != 1)
            {
                PyErr_SetString(PyExc_ValueError, "step must be 1");
                return NULL;
            }

            if (vec->type == T_STRING && vec->u.str->info.unicode == STRING_UTF8)
            {
                start = char_to_byte_index(get_txt(vec->u.str) + offset, size, start, NULL);
                stop = char_to_byte_index(get_txt(vec->u.str) + offset, size, stop, NULL);
            }

            start += offset;
            stop += offset;

            assign_protected_range_lvalue_no_free(&result, var, vec, start, stop);
            ob = ldmud_lvalue_create(&result);
            free_svalue(&result);

            return ob;
        }
        else
        {
            PyErr_Format(PyExc_TypeError, "indices must be integers, not %.200s",
                        key->ob_type->tp_name);
            return NULL;
        }
    }
    else if (vec->type == T_MAPPING)
    {
        const char *err;
        svalue_t *dest;
        svalue_t skey;
        Py_ssize_t idx = 0;

        if (vec->u.map->num_values == 0)
        {
            PyErr_SetString(PyExc_ValueError, "indexing a mapping of width 0");
            return NULL;
        }

        /* If it's a tuple, assume it's (key, index) */
        if (PyTuple_Check(key))
        {
            PyObject* second;

            if(PyTuple_Size(key) != 2)
            {
                PyErr_SetString(PyExc_IndexError, "index should be a 2-tuple");
                return NULL;
            }

            second = PyTuple_GetItem(key, 1);
            if (second==NULL)
                return NULL;
            else if (PyIndex_Check(second))
            {
                idx = PyNumber_AsSsize_t(second, PyExc_IndexError);
                if (idx == -1 && PyErr_Occurred())
                    return NULL;

                if (idx < 0 || idx >= vec->u.map->num_values)
                {
                    PyErr_SetString(PyExc_IndexError, "index out of range");
                    return NULL;
                }

                err = python_to_svalue(&skey, PyTuple_GetItem(key, 0));
            }
            else if (PySlice_Check(second))
            {
                Py_ssize_t start, stop, step, slicelength;

                if (PySlice_GetIndicesEx(second, vec->u.map->num_values,
                                 &start, &stop, &step, &slicelength) < 0)
                    return NULL;

                err = python_to_svalue(&skey, PyTuple_GetItem(key, 0));
                if (err != NULL)
                {
                    PyErr_SetString(PyExc_ValueError, err);
                    return NULL;
                }

                if (slicelength <= 0)
                {
                    PyObject *arr_result;
                    svalue_t lv;

                    free_svalue(&skey);

                    put_ref_array(&lv, &null_vector);
                    arr_result = ldmud_lvalue_create(&lv);
                    free_svalue(&lv);
                    return arr_result;
                }

                if (step == 1)
                {
                    /* We generate a map range. */
                    svalue_t lv;
                    PyObject *ob;

                    assign_protected_map_range_lvalue_no_free(&lv, vec->u.map, &(skey), start, stop);
                    free_svalue(&skey);

                    ob = ldmud_lvalue_create(&lv);
                    free_svalue(&lv);

                    return ob;
                }
                else
                {
                    /* We generate an array of lvalues. */
                    void (*save_handler)(const char *, ...);
                    svalue_t *values;
                    vector_t *arr;
                    PyObject *arr_result;
                    svalue_t lv;

                    save_handler = allocate_array_error_handler;
                    allocate_array_error_handler = python_error_handler;
                    arr = allocate_array_unlimited(slicelength);
                    allocate_array_error_handler = save_handler;

                    if (arr == NULL)
                    {
                        free_svalue(&skey);
                        return PyErr_NoMemory();
                    }

                    values = get_map_value(vec->u.map, &(skey));
                    if (values == &const0)
                    {
                        /* Key does not exist, create this as a
                         * list of map entry lvalues.
                         */
                        for (Py_ssize_t cur = start, i = 0; i < slicelength; cur += step, i++)
                            assign_protected_mapentry_lvalue_no_free(arr->item + i, vec->u.map, &(skey), cur);
                    }
                    else
                    {
                        /* Create lvalues to the entries. */
                        for (Py_ssize_t cur = start, i = 0; i < slicelength; cur += step, i++)
                            assign_protected_lvalue_no_free(arr->item + i, values + cur);
                    }

                    free_svalue(&skey);

                    put_array(&lv, arr);
                    arr_result = ldmud_lvalue_create(&lv);
                    free_svalue(&lv);
                    return arr_result;
                }
            }
            else
            {
                PyErr_Format(PyExc_IndexError, "second index should be an integer, not %.200s", second->ob_type->tp_name);
                return NULL;
            }
        }
        else
            err = python_to_svalue(&skey, key);

        if (err != NULL)
        {
            PyErr_SetString(PyExc_ValueError, err);
            return NULL;
        }

        dest = get_map_value(vec->u.map, &skey);
        if (dest == &const0)
        {
            /* Non-existent, create a mapentry lvalue. */
            svalue_t lv;
            PyObject *ob;

            assign_protected_mapentry_lvalue_no_free(&lv, vec->u.map, &skey, idx);
            free_svalue(&skey);

            ob = ldmud_lvalue_create(&lv);
            free_svalue(&lv);

            return ob;
        }
        else
        {
            /* Ordinary lvalue to an svalue. */
            free_svalue(&skey);

            return ldmud_lvalue_create(dest + idx);
        }
    }
    else
    {
        PyErr_SetString(PyExc_TypeError, "lvalue cannot be indexed");
        return NULL;
    }

} /* ldmud_lvalue_subscript() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lvalue_get_value (ldmud_lvalue_t *self, void *closure)

/* Return the value of the lvalue.
 */

{
    svalue_t *val;

    if (self->lpc_lvalue.type == T_INVALID)
    {
        PyErr_SetString(PyExc_ValueError, "empty lvalue given");
        return NULL;
    }

    assert(self->lpc_lvalue.type == T_LVALUE);
    val = get_rvalue_no_collapse(&(self->lpc_lvalue), NULL);

    if (val != NULL)
        return svalue_to_python(val);
    else
    {
        // Range lvalue
        svalue_t vec;
        PyObject *result;

        assert(self->lpc_lvalue.x.lvalue_type == LVALUE_PROTECTED_RANGE
            || self->lpc_lvalue.x.lvalue_type == LVALUE_PROTECTED_MAP_RANGE);

        assign_rvalue_no_free_no_collapse(&vec, &(self->lpc_lvalue));
        result = svalue_to_python(&vec);
        free_svalue(&vec);

        return result;
    }
} /* ldmud_lvalue_get_value() */

/*-------------------------------------------------------------------------*/
static int
ldmud_lvalue_set_value (ldmud_lvalue_t *self, PyObject *newval, void *closure)

/* Sets the value of the lvalue.
 * Returns 0 on success, -1 on failure.
 */

{
    const char* err;
    svalue_t lpcval;

    if (self->lpc_lvalue.type == T_INVALID)
    {
        PyErr_SetString(PyExc_ValueError, "empty lvalue given");
        return -1;
    }

    assert(self->lpc_lvalue.type == T_LVALUE);

    if (newval == NULL)
        newval = Py_None;

    err = python_to_svalue(&lpcval, newval);
    if (err)
    {
        PyErr_SetString(PyExc_ValueError, err);
        return -1;
    }

    transfer_svalue(&(self->lpc_lvalue), &lpcval);

    return 0;
} /* ldmud_lvalue_set_value() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_lvalue_get_members (ldmud_lvalue_t *self, void *closure)

/* Return the value for the members member.
 * This is only valid if the lvalue contains a struct.
 */

{
    svalue_t *val;
    ldmud_struct_t *result;

    if (self->lpc_lvalue.type == T_INVALID)
    {
        PyErr_SetString(PyExc_ValueError, "empty lvalue given");
        return NULL;
    }

    assert(self->lpc_lvalue.type == T_LVALUE);

    val = get_rvalue_no_collapse(&(self->lpc_lvalue), NULL);
    if (val == NULL || val->type != T_STRUCT)
    {
        PyErr_SetString(PyExc_TypeError, "lvalue doesn't contain a struct");
        return NULL;
    }

    result = (ldmud_struct_t*)ldmud_lvalue_struct_members_type.tp_alloc(&ldmud_lvalue_struct_members_type, 0);
    if (result == NULL)
        return NULL;

    result->lpc_struct = ref_struct(val->u.strct);
    add_gc_object(&gc_struct_list, (ldmud_gc_var_t*)result);

    return (PyObject *)result;
} /* ldmud_lvalue_get_members() */

/*-------------------------------------------------------------------------*/
static PySequenceMethods ldmud_lvalue_as_sequence = {
    (lenfunc)ldmud_lvalue_length,               /* sq_length */
    0,                                          /* sq_concat */
    0,                                          /* sq_repeat */
    (ssizeargfunc)ldmud_lvalue_item,            /* sq_item */
    0,                                          /* sq_slice */
    0,                                          /* sq_ass_item */
    0,                                          /* sq_ass_slice */
    0,                                          /* sq_contains */
    0,                                          /* sq_inplace_concat */
    0,                                          /* sq_inplace_repeat */
};

/* This is needed for slicing. */
static PyMappingMethods ldmud_lvalue_as_mapping = {
    (lenfunc)ldmud_lvalue_length,               /*mp_length*/
    (binaryfunc)ldmud_lvalue_subscript,         /*mp_subscript*/
    0,                                          /*mp_ass_subscript*/
};

static PyMethodDef ldmud_lvalue_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_lvalue_getset[] =
{
    {"value",   (getter)ldmud_lvalue_get_value,   (setter)ldmud_lvalue_set_value, NULL},
    {"members", (getter)ldmud_lvalue_get_members, NULL,                           NULL},
    {NULL}
};

static PyTypeObject ldmud_lvalue_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Lvalue",                     /* tp_name */
    sizeof(ldmud_lvalue_t),             /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_lvalue_dealloc,   /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    &ldmud_lvalue_as_sequence,          /* tp_as_sequence */
    &ldmud_lvalue_as_mapping,           /* tp_as_mapping */
    (hashfunc)ldmud_lvalue_hash,        /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC lvalue",                       /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_lvalue_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_lvalue_methods,               /* tp_methods */
    0,                                  /* tp_members */
    ldmud_lvalue_getset,                /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_lvalue_init,        /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_lvalue_new,                   /* tp_new */
};


/*-------------------------------------------------------------------------*/
static bool
ldmud_lvalue_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC lvalue type.
 */

{
    return Py_TYPE(ob) == &ldmud_lvalue_type;
} /* ldmud_lvalue_check() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_lvalue_create (svalue_t* lv)

/* Creates a new Python lvalue from an LPC lvalue.
 * If the value is not an T_LVALUE, it will be made into one.
 */

{
    ldmud_lvalue_t *self;

    self = (ldmud_lvalue_t *)ldmud_lvalue_type.tp_alloc(&ldmud_lvalue_type, 0);
    if (self == NULL)
        return NULL;

    assign_protected_lvalue_no_free(&(self->lpc_lvalue), lv);
    add_gc_object(&gc_lvalue_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_lvalue_create() */

/*-------------------------------------------------------------------------*/
/* Mixed */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_mixed_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implement __new__ for the mixed type. This is a No-op for any value
 * that is already of an lpc type. For any other values we try to
 * convert. This is basically doing svalue_to_python(python_to_svalue(arg)).
 * We will never actually create an object of the mixed type.
 */

{
    static char *kwlist[] = { "value", NULL};
    ldmud_lpctype_t *internal_type;
    PyObject *internal_type_arg;

    PyObject *value;
    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O", kwlist, &value))
        return NULL;

    /* Check if <value> is of an lpctype. */
    PyTypeObject *valuetype = Py_TYPE(value);
    PyObject *mro = valuetype->tp_mro;
    if (mro && PyTuple_Check(mro))
    {
        for (Py_ssize_t i = 0, n = PyTuple_GET_SIZE(mro); i < n; i++)
        {
            if (PyObject_TypeCheck(PyTuple_GET_ITEM(mro, i), &ldmud_lpctype_type.type_base))
            {
                Py_INCREF(value);
                return value;
            }
        }
    }

    /* Is it one of the registered types? */
    for (int idx = 0; idx < PYTHON_TYPE_TABLE_SIZE; idx++)
    {
        if (python_type_table[idx] != NULL
         && python_type_table[idx]->pytype != NULL
         && PyObject_TypeCheck(value, (PyTypeObject*) python_type_table[idx]->pytype))
        {
            Py_INCREF(value);
            return value;
        }
    }

    /* Last chance: native Python types. */
    if (PyLong_Check(value))
    {
        int overflow;
        long num = PyLong_AsLongAndOverflow(value, &overflow);

        if (overflow || num < PINT_MIN || num > PINT_MAX)
        {
            PyErr_SetString(PyExc_OverflowError, "integer overflow");
            return NULL;
        }

        internal_type = &ldmud_integer_type;
        internal_type_arg = value;
        Py_INCREF(value);
    }
    else if (PyBool_Check(value))
    {
        internal_type = &ldmud_integer_type;
        internal_type_arg = PyLong_FromLong(value == Py_True ? 1 : 0);
    }
    else if (PyFloat_Check(value))
    {
        internal_type = &ldmud_float_type;
        internal_type_arg = value;
        Py_INCREF(value);
    }
    else if (PyBytes_Check(value))
    {
        internal_type = &ldmud_bytes_type;
        internal_type_arg = value;
        Py_INCREF(value);
    }
    else if (PyUnicode_Check(value))
    {
        internal_type = &ldmud_string_type;
        internal_type_arg = value;
        Py_INCREF(value);
    }
    else
        internal_type = NULL;

    if (internal_type)
    {
        PyObject *type_args = PyTuple_New(1);
        PyObject *result;

        if (type_args == NULL)
            return NULL;
        PyTuple_SET_ITEM(type_args, 0, internal_type_arg);
        result = PyObject_CallObject((PyObject *)internal_type, type_args);
        Py_DECREF(type_args);
        return result;
    }

    PyErr_Format(PyExc_TypeError, "unsupported type '%.200s'", valuetype->tp_name);
    return NULL;
} /* ldmud_mixed_new() */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_mixed_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Mixed.
 */

{
    return lpctype_mixed;
} /* ldmud_mixed_get_lpctype() */

/*-------------------------------------------------------------------------*/

static ldmud_lpctype_t ldmud_mixed_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.Mixed",                      /* tp_name */
    sizeof(PyObject),                   /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC any value",                    /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_mixed_new,                    /* tp_new */
},  ldmud_mixed_get_lpctype             /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
/* Void */

/*-------------------------------------------------------------------------*/
static lpctype_t*
ldmud_void_get_lpctype (ldmud_lpctype_t* type)

/* Return the lpctype for ldmud.Void.
 */

{
    return lpctype_void;
} /* ldmud_void_get_lpctype() */

/*-------------------------------------------------------------------------*/

static ldmud_lpctype_t ldmud_void_type =
{{
    PyVarObject_HEAD_INIT(&ldmud_lpctype_type.type_base, 0)
    "ldmud.Void",                      /* tp_name */
    sizeof(PyObject),                   /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT|Py_TPFLAGS_TYPE_SUBCLASS, /* tp_flags */
    "LPC void",                         /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
},  ldmud_void_get_lpctype             /* get_lpctype */
};

/*-------------------------------------------------------------------------*/
/* Instruction */

/*-------------------------------------------------------------------------*/

static void
ldmud_instruction_dealloc (ldmud_instruction_t* self)

/* Destroy the ldmud_instruction_t object
 */

{
    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_instruction_dealloc() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_instruction_get_name (ldmud_instruction_t *instr, void *closure)

/* Returns the name of this instruction.
 */

{
    return PyUnicode_FromString(get_f_name(instr->full_instr));
} /* ldmud_instruction_get_name() */

/*-------------------------------------------------------------------------*/
static PyGetSetDef ldmud_instruction_getset[] =
{
    {"name",             (getter)ldmud_instruction_get_name,         NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_instruction_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Instruction",                /* tp_name */
    sizeof(ldmud_instruction_t),        /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_instruction_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC instruction information",      /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_instruction_getset,           /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/

static PyObject*
ldmud_instruction_create (int instruction)

/* Create Python object that represents an instruction.
 */

{
    ldmud_instruction_t *result;

    result = (ldmud_instruction_t*) ldmud_instruction_type.tp_alloc(&ldmud_instruction_type, 0);
    if (result == NULL)
        return NULL;

    result->full_instr = instruction;

    return (PyObject*)result;
} /* ldmud_instruction_create() */

/*-------------------------------------------------------------------------*/
/* Local Variables */

/*-------------------------------------------------------------------------*/

static void
ldmud_call_frame_ref_dealloc (ldmud_call_frame_ref_t* self)

/* Destroy the ldmud_call_frame_ref_t object.
 */

{
    free_prog(self->prog, true);

    remove_gc_object(&gc_call_frame_ref_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_call_frame_ref_dealloc() */

/*-------------------------------------------------------------------------*/
static int
ldmud_call_frame_ref_bool (ldmud_call_frame_ref_t* self)

/* Return 1 (true) if this references a valid (still active) frame.
 */

{
    return self->frame_serial == frame_current_index[self->frame_level];
} /* ldmud_call_frame_ref_bool() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_local_variable_repr (ldmud_local_variable_t *varob)

/* Return a string representation of this variable.
 */

{
    return PyUnicode_FromFormat("<LPC local variable %s>", get_txt(varob->var->name));
} /* ldmud_local_variable_repr() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_local_variable_richcompare (ldmud_local_variable_t* self, PyObject *other, int op)

/* Compare <self> to <other> with the comparison operation <op>.
 */

{
    ldmud_local_variable_t *other_var;
    PyObject* resultval;
    bool result;

    if (Py_TYPE(self) != Py_TYPE(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    other_var = (ldmud_local_variable_t*) other;

// frame_level, frame_serial,  local_variable_dbg_t *var;


    if (!ldmud_call_frame_ref_bool(&self->frame_ref_head)
     || !ldmud_call_frame_ref_bool(&other_var->frame_ref_head))
        result = false;
    else if (self->frame_ref_head.frame_level < other_var->frame_ref_head.frame_level)
        result = op == Py_LT || op == Py_LE || op == Py_NE;
    else if (self->frame_ref_head.frame_level > other_var->frame_ref_head.frame_level)
        result = op == Py_GT || op == Py_GE || op == Py_NE;
    else if (self->var->idx < other_var->var->idx)
        result = op == Py_LT || op == Py_LE || op == Py_NE;
    else if (self->var->idx > other_var->var->idx)
        result = op == Py_GT || op == Py_GE || op == Py_NE;
    else
        result = op == Py_LE || op == Py_EQ || op == Py_GE;

    resultval = result ? Py_True : Py_False;
    Py_INCREF(resultval);
    return resultval;
} /* ldmud_local_variable_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_local_variable_get_name (ldmud_local_variable_t *varob, void *closure)

/* Return the name for the variable.
 */

{
    return PyUnicode_FromStringAndSize(get_txt(varob->var->name), mstrsize(varob->var->name));
} /* ldmud_local_variable_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_local_variable_get_value (ldmud_local_variable_t *varob, void *closure)

/* Return the value of the variable.
 */

{
    if (!ldmud_call_frame_ref_bool(&varob->frame_ref_head))
    {
        PyErr_Format(PyExc_ValueError, "expired local variable");
        return NULL;
    }

    return rvalue_to_python(varob->varp);
} /* ldmud_local_variable_get_value() */

/*-------------------------------------------------------------------------*/
static int
ldmud_local_variable_set_value (ldmud_local_variable_t *varob, PyObject *newval, void *closure)

/* Sets the value for the variable.
 * Returns 0 on success, -1 on failure.
 */

{
    const char* err;
    svalue_t lpcval;

    if (!ldmud_call_frame_ref_bool(&varob->frame_ref_head))
    {
        PyErr_Format(PyExc_ValueError, "expired local variable");
        return -1;
    }

    if (newval == NULL)
        newval = Py_None;

    err = python_to_svalue(&lpcval, newval);
    if (err)
    {
        PyErr_SetString(PyExc_ValueError, err);
        return -1;
    }

    if (varob->frame_ref_head.prog->flags & P_RTT_CHECKS)
    {
        if (!check_rtt_compatibility(varob->var->type, &lpcval))
        {
            static char realtypebuf[512];
            lpctype_t *realtype = get_rtt_type(varob->var->type, &lpcval);

            get_lpctype_name_buf(realtype, realtypebuf, sizeof(realtypebuf));
            free_lpctype(realtype);

            free_svalue(&lpcval);

            PyErr_Format(PyExc_TypeError, "bad type for variable assignment to '%s': expected %s, got %s",
                get_txt(varob->var->name), get_lpctype_name(varob->var->type), realtypebuf);
            return -1;
        }
    }

    transfer_svalue(varob->varp, &lpcval);

    return 0;
} /* ldmud_local_variable_set_value() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_local_variable_get_type (ldmud_local_variable_t *varob, void *closure)

/* Return the type for the variable.
 */

{
    PyObject* result = lpctype_to_pythontype(varob->var->type);
    if (!result)
        PyErr_Format(PyExc_AttributeError, "Variable '%s' has no type information or mixed type", get_txt(varob->var->name));

    return result;

} /* ldmud_local_variable_get_type() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_local_variable_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    (inquiry)ldmud_call_frame_ref_bool, /* nb_bool */
};

static PyGetSetDef ldmud_local_variable_getset [] = {
    {"name",       (getter)ldmud_local_variable_get_name,       NULL,                                     NULL},
    {"value",      (getter)ldmud_local_variable_get_value,      (setter)ldmud_local_variable_set_value, NULL},
    {"type",       (getter)ldmud_local_variable_get_type,       NULL,                                     NULL},
    {NULL}
};

static PyTypeObject ldmud_local_variable_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.LocalVariable",              /* tp_name */
    sizeof(ldmud_local_variable_t),     /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_call_frame_ref_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_local_variable_repr, /* tp_repr */
    &ldmud_local_variable_as_number,    /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC local variable",               /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_local_variable_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_local_variable_getset,        /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_local_variable_create (ldmud_local_variables_t * vars, local_variable_dbg_t* var)

/* Creates a ldmud.LocalVariable object.for <var>.
 */

{
    ldmud_local_variable_t *varob = (ldmud_local_variable_t*)ldmud_local_variable_type.tp_alloc(&ldmud_local_variable_type, 0);
    if (varob == NULL)
        return NULL;

    reference_prog(vars->frame_ref_head.prog, "ldmud_local_variable_create");

    varob->frame_ref_head.prog = vars->frame_ref_head.prog;
    varob->frame_ref_head.frame_serial = vars->frame_ref_head.frame_serial;
    varob->frame_ref_head.frame_level = vars->frame_ref_head.frame_level;
    varob->var = var;
    varob->varp = vars->fp + var->idx;

    add_gc_object(&gc_call_frame_ref_list, (ldmud_gc_var_t*)varob);

    return (PyObject*)varob;
} /* ldmud_local_variable_create() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_local_variables_getattro (ldmud_local_variables_t *vars, PyObject *name)

/* Return a variable of the variable list.
 */

{
    PyObject *result;
    bool error;
    string_t* varname;

    /* First check real attributes... */
    result = PyObject_GenericGetAttr((PyObject *)vars, name);
    if (result || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return result;

    if (!ldmud_call_frame_ref_bool(&vars->frame_ref_head))
        return NULL;

    PyErr_Clear();

    /* And now search for a local variable. */
    varname = find_tabled_python_string(name, "local variable name", &error);
    if (error)
        return NULL;

    if (varname)
    {
        for (local_variable_dbg_t *var = get_first_local_variable(vars->frame_ref_head.prog, vars->pc); var != NULL; var = get_next_local_variable(vars->frame_ref_head.prog, vars->pc, var))
            if (var->name == varname)
                return ldmud_local_variable_create(vars, var);
    }

    PyErr_Format(PyExc_AttributeError, "local variable '%U' not found", name);
    return NULL;
} /* ldmud_local_variables_getattro() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_local_variables_iter (ldmud_local_variables_t *vars)

/* Creates an iterator over all local variables.
 */

{
    PyObject *variables, *iter;

    if (ldmud_call_frame_ref_bool(&vars->frame_ref_head))
    {
        /* To avoid any further validity issues we collect all variables here
         * and return an iterator to that sequence.
         */
        local_variable_dbg_t *first = get_first_local_variable(vars->frame_ref_head.prog, vars->pc);
        int num = 0, idx = 0;

        for (local_variable_dbg_t* var = first; var != NULL; var = get_next_local_variable(vars->frame_ref_head.prog, vars->pc, var))
            num++;

        variables = PyTuple_New(num);
        if (!variables)
            return NULL;

        for (local_variable_dbg_t* var = first; var != NULL; var = get_next_local_variable(vars->frame_ref_head.prog, vars->pc, var))
        {
            PyObject *varob = ldmud_local_variable_create(vars, var);
            if (!varob)
            {
                Py_DECREF(variables);
                return NULL;
            }
            PyTuple_SET_ITEM(variables, idx++, varob);
        }
    }
    else
    {
        variables = PyTuple_New(0);
        if (!variables)
            return NULL;
    }

    iter = PyObject_GetIter(variables);
    Py_DECREF(variables);
    return iter;
} /* ldmud_local_variables_iter() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_local_variables_dir (ldmud_local_variables_t *vars)

/* Returns a list of all attributes, this includes all variable names.
 */

{
    PyObject *result;
    PyObject *attrs = get_class_dir((PyObject*)vars);

    if (attrs == NULL)
        return NULL;

    /* Now add all the variables. */
    if (ldmud_call_frame_ref_bool(&vars->frame_ref_head))
    {
        for (local_variable_dbg_t *var = get_first_local_variable(vars->frame_ref_head.prog, vars->pc); var != NULL; var = get_next_local_variable(vars->frame_ref_head.prog, vars->pc, var))
        {
            PyObject *varname = PyUnicode_FromStringAndSize(get_txt(var->name), mstrsize(var->name));

            if (varname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PySet_Add(attrs, varname) < 0)
                PyErr_Clear();
            Py_DECREF(varname);
        }
    }

    /* And return the keys of our dict. */
    result = PySequence_List(attrs);
    Py_DECREF(attrs);
    return result;
} /* ldmud_local_variables_dir() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_local_variables_dict (ldmud_local_variables_t *vars, void *closure)

/* Returns a list of all variables.
 */

{
    PyObject *result, *dict = PyDict_New();
    if (!dict)
        return NULL;

    if (ldmud_call_frame_ref_bool(&vars->frame_ref_head))
    {
        for (local_variable_dbg_t *var = get_first_local_variable(vars->frame_ref_head.prog, vars->pc); var != NULL; var = get_next_local_variable(vars->frame_ref_head.prog, vars->pc, var))
        {
            PyObject *varname = PyUnicode_FromStringAndSize(get_txt(var->name), mstrsize(var->name));
            PyObject* varob;

            if (varname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            varob = ldmud_local_variable_create(vars, var);
            if (varob == NULL)
            {
                PyErr_Clear();
                Py_DECREF(varname);
                continue;
            }

            if (PyDict_SetItem(dict, varname, (PyObject*)varob) < 0)
                PyErr_Clear();
            Py_DECREF(varname);
            Py_DECREF(varob);
        }
    }

    result = PyDictProxy_New(dict);
    Py_DECREF(dict);
    return result;
} /* ldmud_local_variables_dict() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_local_variables_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    (inquiry)ldmud_call_frame_ref_bool, /* nb_bool */
};

static PyMethodDef ldmud_local_variables_methods[] =
{
    {
        "__dir__",
        (PyCFunction)ldmud_local_variables_dir, METH_NOARGS,
        "__dir__() -> List\n\n"
        "Returns a list of all attributes."
    },

    {NULL}
};

static PyGetSetDef ldmud_local_variables_getset [] = {
    {"__dict__", (getter)ldmud_local_variables_dict, NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_local_variables_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Variables",                  /* tp_name */
    sizeof(ldmud_local_variables_t),    /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_call_frame_ref_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    &ldmud_local_variables_as_number,   /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    (getattrofunc)ldmud_local_variables_getattro, /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC local variable list",          /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    (getiterfunc)ldmud_local_variables_iter, /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_local_variables_methods,      /* tp_methods */
    0,                                  /* tp_members */
    ldmud_local_variables_getset,       /* tp_getset */
};

/*-------------------------------------------------------------------------*/
/* Call Stack Frame */

/*-------------------------------------------------------------------------*/

static void
ldmud_call_frame_dealloc (ldmud_call_frame_t* self)

/* Destroy the ldmud_call_frame_t object
 */

{
    free_svalue(&self->ob);
    free_prog(self->prog, true);

    remove_gc_object(&gc_call_frame_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_call_frame_dealloc() */

/*-------------------------------------------------------------------------*/
static int
ldmud_call_frame_bool (ldmud_call_frame_t* self)

/* Return 1 (true) if this is a valid (still active) call frame.
 */

{
    return self->frame_serial == frame_current_index[self->frame_level];
} /* ldmud_call_frame_bool() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_call_frame_get_type (ldmud_call_frame_t *frame, void *closure)

/* Returns the type of this call_frame.
 */

{
    return PyLong_FromLong(frame->type);
} /* ldmud_call_frame_get_type() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_call_frame_get_name (ldmud_call_frame_t *frame, void *closure)

/* Returns a name for this call frame. This is for
 *  - CALL_FRAME_TYPE_LFUN:                The name of the lfun
 *  - CALL_FRAME_TYPE_EFUN_CLOSURE:        The name of the efun or instruction
 *  - CALL_FRAME_TYPE_PYTHON_EFUN_CLOSURE: The name of the Python efun
 *  - CALL_FRAME_TYPE_SIMUL_EFUN_CLOSURE:  The name of the simul-efun.
 *  - CALL_FRAME_TYPE_CATCH:               "catch"
 *  - CALL_FRAME_TYPE_LAMBDA:              None
 */

{
    if (frame->name == NULL)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }
    else
        return PyUnicode_FromString(frame->name);
} /* ldmud_call_frame_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_call_frame_get_object (ldmud_call_frame_t *frame, void *closure)

/* Returns the object for this call frame.
 */

{
    return svalue_to_python(&(frame->ob));
} /* ldmud_call_frame_get_object() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_call_frame_get_program_name (ldmud_call_frame_t *frame, void *closure)

/* Returns the name of the program for this call frame.
 */

{
    return PyUnicode_FromFormat("/%s", get_txt(frame->prog->name));
} /* ldmud_call_frame_get_program_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_call_frame_get_file_name (ldmud_call_frame_t *frame, void *closure)

/* Returns the name of the source file for this call frame.
 */

{
    string_t *name, *fname;

    if (frame->pc < frame->prog->program || frame->pc >= PROGRAM_END(*frame->prog))
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    get_line_number(frame->pc+1, frame->prog, &name, &fname);
    free_mstring(name);

    if (fname == NULL)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    if (get_txt(fname)[0] == '/')
        return PyUnicode_FromStringAndSize(get_txt(fname), mstrsize(fname));
    else
        return PyUnicode_FromFormat("/%s", get_txt(fname));
} /* ldmud_call_frame_get_file_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_call_frame_get_line_number (ldmud_call_frame_t *frame, void *closure)

/* Returns the source line for this call frame.
 */

{
    string_t *name;
    int pos;

    if (frame->pc < frame->prog->program || frame->pc >= PROGRAM_END(*frame->prog))
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    /* frame->pc would yield the line number for the code *up to* frame->pc,
     * but we want the line number of the code after frame->pc, therefore
     * incrementing it.
     */
    pos = get_line_number(frame->pc+1, frame->prog, &name, NULL);
    free_mstring(name);

    if (pos == 0)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyLong_FromLong(pos);
} /* ldmud_call_frame_get_line_number() */

/*-------------------------------------------------------------------------*/
#ifdef EVAL_COST_TRACE
static PyObject *
ldmud_call_frame_get_eval_cost (ldmud_call_frame_t *frame, void *closure)

/* Returns the current evaluation cost for this call frame.
 */

{
    return PyLong_FromLong(frame->eval_cost);
} /* ldmud_call_frame_get_eval_cost() */
#endif
/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_call_frame_get_variables (ldmud_call_frame_t *frame, void *closure)

/* Returns an object that represents the local variables of this frame.
 */

{
    ldmud_local_variables_t *result;

    result = (ldmud_local_variables_t*) ldmud_local_variables_type.tp_alloc(&ldmud_local_variables_type, 0);
    if (result == NULL)
        return NULL;

    reference_prog(frame->prog, "ldmud_call_frame_get_variables");

    result->frame_ref_head.prog = frame->prog;
    result->frame_ref_head.frame_serial = frame->frame_serial;
    result->frame_ref_head.frame_level = frame->frame_level;
    result->pc = frame->pc;
    result->fp = frame->fp;

    add_gc_object(&gc_call_frame_ref_list, (ldmud_gc_var_t*)result);

    return (PyObject*)result;
} /* ldmud_call_frame_get_variables() */

/*-------------------------------------------------------------------------*/
static PyNumberMethods ldmud_call_frame_as_number =
{
    0,                                  /* nb_add */
    0,                                  /* nb_subtract */
    0,                                  /* nb_multiply */
    0,                                  /* nb_remainder */
    0,                                  /* nb_divmod */
    0,                                  /* nb_power */
    0,                                  /* nb_negative */
    0,                                  /* nb_positive */
    0,                                  /* nb_absolute */
    (inquiry)ldmud_call_frame_bool,     /* nb_bool */
};

static PyGetSetDef ldmud_call_frame_getset[] =
{
    {"type",             (getter)ldmud_call_frame_get_type,         NULL, NULL},
    {"name",             (getter)ldmud_call_frame_get_name,         NULL, NULL},
    {"object",           (getter)ldmud_call_frame_get_object,       NULL, NULL},
    {"program_name",     (getter)ldmud_call_frame_get_program_name, NULL, NULL},
    {"file_name",        (getter)ldmud_call_frame_get_file_name,    NULL, NULL},
    {"line_number",      (getter)ldmud_call_frame_get_line_number,  NULL, NULL},
#ifdef EVAL_COST_TRACE
    {"eval_cost",        (getter)ldmud_call_frame_get_eval_cost,    NULL, NULL},
#endif
    {"variables",        (getter)ldmud_call_frame_get_variables,    NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_call_frame_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.CallFrame",                  /* tp_name */
    sizeof(ldmud_call_frame_t),         /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_call_frame_dealloc, /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    &ldmud_call_frame_as_number,        /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC call stack frame",             /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_call_frame_getset,            /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/

static PyObject*
ldmud_call_frame_create (int level, enum call_frame_type type, svalue_t ob, program_t *prog, bytecode_t *pc, svalue_t *fp, const char* name
#ifdef EVAL_COST_TRACE
                        , int32 frame_eval_cost
#endif
                        )

/* Create Python object that represents an instruction.
 */

{
    ldmud_call_frame_t *result;

    result = (ldmud_call_frame_t*) ldmud_call_frame_type.tp_alloc(&ldmud_call_frame_type, 0);
    if (result == NULL)
        return NULL;

    reference_prog(prog, "ldmud_call_frame_create");
    assign_object_svalue_no_free(&(result->ob), ob, "ldmud_call_frame_create");
    result->type = type;
    result->prog = prog;
    result->pc = pc;
    result->fp = fp;
    result->name = name;
#ifdef EVAL_COST_TRACE
    result->eval_cost = frame_eval_cost;
#endif
    result->frame_level = level;
    if (!frame_current_index[level])
        frame_current_index[level] = ++frame_counter;
    result->frame_serial = frame_current_index[level];

    add_gc_object(&gc_call_frame_list, (ldmud_gc_var_t*)result);

    return (PyObject*)result;
} /* ldmud_call_frame_create() */

/*-------------------------------------------------------------------------*/
/* Interrupt exception */

static PyTypeObject ldmud_interrupt_exception_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.InterruptException",         /* tp_name */
    sizeof(PyBaseExceptionObject),      /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    0,                                  /* tp_flags */
    "Exception due to interrupt by signal",
                                        /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/

/*=========================================================================*/

/*                                 Module                                  */

/*-------------------------------------------------------------------------*/
/* The efun namespace */
/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_efun_repr (ldmud_efun_t *val)

/* Return a string representation of this efun.
 */

{
    return PyUnicode_FromFormat("<efun %s>", instrs[val->efun_idx].name);
} /* ldmud_efun_repr() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_efun_hash (ldmud_efun_t *val)

/* Return a hash of this efun.
 */

{
    return _Py_HashPointer(instrs + val->efun_idx);
} /* ldmud_efun_hash() */

/*-------------------------------------------------------------------------*/
static void
ldmud_efun_call_efun (int num_arg, ldmud_efun_t *func)

/* Helper function for ldmud_efun_call().
 */

{
    /* We construct a efun closure, so we don't need to duplicate
     * code from call_lambda.
     */
    svalue_t efun_closure = { T_CLOSURE };
    efun_closure.x.closure_type = (short)(func->efun_idx + CLOSURE_EFUN);
    switch (current_object.type)
    {
        case T_OBJECT:
            efun_closure.u.ob = current_object.u.ob;
            break;

        case T_LWOBJECT:
            efun_closure.u.lwob = current_object.u.lwob;
            efun_closure.x.closure_type += CLOSURE_LWO;
            break;

        case T_NUMBER:
        default:
            efun_closure.u.ob = master_ob;
            break;
    }

    call_lambda(&efun_closure, num_arg);
} /* ldmud_efun_call_efun() */


static PyObject*
ldmud_efun_call (ldmud_efun_t *func, PyObject *arg, PyObject *kw)

/* The call operator for efun objects.
 */

{
    if(python_is_external ? (!master_ob) : (current_object.type == T_NUMBER))
    {
        PyErr_SetString(PyExc_RuntimeError, "can't call an efun without a current object");
        return NULL;
    }

    if (kw != NULL && PyDict_Size(kw) != 0)
    {
        PyErr_Format(PyExc_TypeError, "%.200s() takes no keyword arguments",
                 instrs[func->efun_idx].name);
        return NULL;
    }
    else
    {
        svalue_t *sp = inter_sp;
        PyObject *result;
        int num_arg = (int)PyTuple_GET_SIZE(arg);

        /* Put all arguments on the stack. */
        for (int i = 0; i < num_arg; i++)
        {
            const char* err = python_to_svalue(++sp, PyTuple_GetItem(arg, i));
            if (err != NULL)
            {
                PyErr_SetString(PyExc_ValueError, err);
                pop_n_elems(i, sp-1);
                return NULL;
            }
        }

        inter_sp = sp;

        if(call_lpc_secure((CClosureFun)ldmud_efun_call_efun, num_arg, func))
        {
            result = svalue_to_python(inter_sp);
            pop_stack();
        }
        else
            result = NULL;

        return result;
    }

    return NULL;
} /* ldmud_efun_call() */

/*-------------------------------------------------------------------------*/
static bool ldmud_efun_check(PyObject *ob);

static PyObject*
ldmud_efun_richcompare (ldmud_efun_t *self, PyObject *other, int op)

/* Compare this efun with another one.
 */

{
    PyObject *result;
    bool equal;

    if ((op != Py_EQ && op != Py_NE) ||
        !ldmud_efun_check(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    equal = (self->efun_idx == ((ldmud_efun_t*)other)->efun_idx);
    result = (equal == (op == Py_EQ)) ? Py_True : Py_False;
    Py_INCREF(result);
    return result;
} /* ldmud_efun_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_efun_get_name (ldmud_efun_t *efun, void *closure)

/* Return the value for the name member.
 */

{
    return PyUnicode_FromString(instrs[efun->efun_idx].name);
} /* ldmud_efun_get_name() */

/*-------------------------------------------------------------------------*/

static PyGetSetDef ldmud_efun_getset [] = {
    {"name", (getter)ldmud_efun_get_name, NULL, NULL},
    {NULL}
};


static PyTypeObject ldmud_efun_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Efun",                       /* tp_name */
    sizeof(ldmud_efun_t),               /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_efun_repr,          /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_efun_hash,          /* tp_hash  */
    (ternaryfunc)ldmud_efun_call,       /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC efun",                         /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_efun_richcompare,/* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_efun_getset,                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_efun_check (PyObject *ob)

/* Return true, if the given object as an efun object.
 */

{
    return Py_TYPE(ob) == &ldmud_efun_type;
} /* ldmud_efun_check() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_efun_create (int efun_idx)

/* Create an efun object for <efun_idx>.
 */

{
    ldmud_efun_t *efun = (ldmud_efun_t *)ldmud_efun_type.tp_alloc(&ldmud_efun_type, 0);
    if (efun == NULL)
        return NULL;

    efun->efun_idx = efun_idx;

    return (PyObject*) efun;
} /* ldmud_efun_create() */

/*-------------------------------------------------------------------------*/
static PyTypeObject ldmud_efun_namespace =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.efuns",                      /* tp_name */
};

/*-------------------------------------------------------------------------*/
static PyObject *
create_efun_namespace ()

/* Add all the efuns to the ldmud_efun_namespace and return it.
 */

{
    /* The namespace is internal its own type with static functions. */
    if (PyType_Ready(&ldmud_efun_namespace) < 0)
        return NULL;

    if (PyType_Ready(&ldmud_efun_type) < 0)
        return NULL;

    for (int n = EFUN_OFFSET; n <= LAST_INSTRUCTION_CODE; n++)
    {
        PyObject *efun;
        PyObject *descr;

        efun = ldmud_efun_create(n);
        if (efun == NULL)
            return NULL;

        descr = PyStaticMethod_New(efun);
        Py_DECREF(efun);

        if (descr == NULL)
            return NULL;

        if (PyDict_SetItemString(ldmud_efun_namespace.tp_dict, instrs[n].name, descr) < 0)
        {
            Py_DECREF(descr);
            return NULL;
        }

        Py_DECREF(descr);
    }

    return (PyObject*)&ldmud_efun_namespace;
} /* create_efun_namespace() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_python_efun_repr (ldmud_efun_t *self)

/* Return a string representation of this efun.
 */

{
    return PyUnicode_FromFormat("<python efun %s>", get_txt(python_efun_table[self->efun_idx].name->name));
} /* ldmud_python_efun_repr() */

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_python_efun_hash (ldmud_efun_t *self)

/* Return a hash of this efun.
 */

{
    return _Py_HashPointer(python_efun_table + self->efun_idx);
} /* ldmud_python_efun_hash() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_python_efun_call (ldmud_efun_t *self, PyObject *arg, PyObject *kw)

/* The call operator for efun objects.
 */

{
    PyObject *callable = python_efun_table[self->efun_idx].callable;
    if (!callable)
    {
        PyErr_Format(PyExc_ValueError, "%s() has vanished.", get_txt(python_efun_table[self->efun_idx].name->name));
        return NULL;
    }

    return PyObject_Call(callable, arg, kw);
} /* ldmud_python_efun_call() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_python_efun_richcompare (ldmud_efun_t *self, PyObject *other, int op)

/* Compare this efun with another one.
 */

{
    PyObject *result;
    bool equal;

    if ((op != Py_EQ && op != Py_NE) || !ldmud_python_efun_check(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    equal = (self->efun_idx == ((ldmud_efun_t*)other)->efun_idx);
    result = (equal == (op == Py_EQ)) ? Py_True : Py_False;
    Py_INCREF(result);
    return result;
} /* ldmud_python_efun_richcompare() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_python_efun_get_name (ldmud_efun_t *self, void *closure)

/* Return the value for the name member.
 */

{
    string_t *name = python_efun_table[self->efun_idx].name->name;
    return PyUnicode_FromStringAndSize(get_txt(name), mstrsize(name));
} /* ldmud_python_efun_get_name() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_python_efun_get_function (ldmud_efun_t *self, void *closure)

/* Return the value for the name member.
 */

{
    PyObject *callable = python_efun_table[self->efun_idx].callable;
    if (!callable)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    Py_INCREF(callable);
    return callable;
} /* ldmud_python_efun_get_function() */

/*-------------------------------------------------------------------------*/

static PyGetSetDef ldmud_python_efun_getset [] = {
    {"name",     (getter)ldmud_python_efun_get_name,     NULL, NULL},
    {"function", (getter)ldmud_python_efun_get_function, NULL, NULL},
    {NULL}
};


static PyTypeObject ldmud_python_efun_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.PythonEfun",                 /* tp_name */
    sizeof(ldmud_efun_t),               /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_python_efun_repr,   /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_python_efun_hash,   /* tp_hash  */
    (ternaryfunc)ldmud_python_efun_call,/* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC python efun",                  /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    (richcmpfunc)ldmud_python_efun_richcompare, /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    ldmud_python_efun_getset,           /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    0,                                  /* tp_init */
    0,                                  /* tp_alloc */
    0,                                  /* tp_new */
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_python_efun_check (PyObject *ob)

/* Return true, if the given object as an efun object.
 */

{
    return Py_TYPE(ob) == &ldmud_python_efun_type;
} /* ldmud_python_efun_check() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_python_efun_create (int efun_idx)

/* Create an efun object for <efun_idx>.
 */

{
    ldmud_efun_t *efun = (ldmud_efun_t *)ldmud_python_efun_type.tp_alloc(&ldmud_python_efun_type, 0);
    if (efun == NULL)
        return NULL;

    efun->efun_idx = efun_idx;

    return (PyObject*) efun;
} /* ldmud_python_efun_create() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_registered_efuns_iter_next (ldmud_efun_t* self)

/* Return the next value for this iterator.
 */

{
    while (++self->efun_idx < num_python_efun)
    {
        if (python_efun_table[self->efun_idx].callable)
            return ldmud_python_efun_create(self->efun_idx);
    }

    return NULL;
} /* ldmud_registered_efuns_iter_next() */

/*-------------------------------------------------------------------------*/
static PyTypeObject ldmud_registered_efuns_iter_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.registered_efuns.Iter",      /* tp_name */
    sizeof(ldmud_efun_t),               /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "Registered Python efuns iterator", /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    PyObject_SelfIter,                  /* tp_iter */
    (iternextfunc)ldmud_registered_efuns_iter_next, /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
};

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_registered_efuns_iter (PyObject* self)

/* Create an iterator for registered efuns.
 */

{
    ldmud_efun_t *result = (ldmud_efun_t*)ldmud_registered_efuns_iter_type.tp_alloc(&ldmud_registered_efuns_iter_type, 0);
    if (result == NULL)
        return NULL;

    result->efun_idx = -1;

    return (PyObject*)result;
} /* ldmud_registered_efuns_iter() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_registered_efuns_getattro (PyObject *val, PyObject *name)

/* Return the callable for a registered efun.
 */

{
    PyObject *result;
    bool error;
    string_t* efunname;
    ident_t *ident;

    /* First check real attributes... */
    result = PyObject_GenericGetAttr(val, name);
    if (result || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return result;

    PyErr_Clear();

    /* And now look up registered efuns. */
    efunname = find_tabled_python_string(name, "efun name", &error);
    if (error)
        return NULL;

    if (efunname)
    {
        ident = find_shared_identifier_mstr(efunname, I_TYPE_GLOBAL, 0);
        while (ident && ident->type != I_TYPE_GLOBAL)
            ident = ident->inferior;

        if (ident && ident->type == I_TYPE_GLOBAL && ident->u.global.python_efun != I_GLOBAL_PYTHON_EFUN_OTHER)
        {
            result = python_efun_table[ident->u.global.python_efun].callable;
            if (result)
            {
                Py_INCREF(result);
                return result;
            }
        }
    }

    PyErr_Format(PyExc_AttributeError, "No such registered efun: '%U'", name);
    return NULL;
} /* ldmud_registered_efuns_getattro() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_registered_efuns_dir (PyObject *self)

/* Returns a list of all attributes, this includes all registered efun names.
 */

{
    PyObject *result;
    PyObject *attrs = get_class_dir(self);

    if (attrs == NULL)
        return NULL;

    /* Now add all registered efuns. */
    for (ident_t *ident = all_python_idents; ident; ident = ident->next_all)
    {
        if (ident->u.global.python_efun != I_GLOBAL_PYTHON_EFUN_OTHER
         && python_efun_table[ident->u.global.python_efun].callable != NULL)
        {
            PyObject *efunname = PyUnicode_FromStringAndSize(get_txt(ident->name), mstrsize(ident->name));

            if (efunname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PySet_Add(attrs, efunname) < 0)
                PyErr_Clear();
            Py_DECREF(efunname);
        }
    }

    /* And return the keys of our dict. */
    result = PySequence_List(attrs);
    Py_DECREF(attrs);
    return result;
} /* ldmud_registered_efuns_dir() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_registered_efuns_dict (ldmud_program_t *self, void *closure)

/* Returns a list of all registered efuns.
 */

{
    PyObject *result, *dict = PyDict_New();
    if (!dict)
        return NULL;

    for (ident_t *ident = all_python_idents; ident; ident = ident->next_all)
    {
        PyObject * callable;

        if (ident->u.global.python_efun == I_GLOBAL_PYTHON_EFUN_OTHER)
            continue;

        callable = python_efun_table[ident->u.global.python_efun].callable;
        if (callable != NULL)
        {
            PyObject *efunname = PyUnicode_FromStringAndSize(get_txt(ident->name), mstrsize(ident->name));

            if (efunname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PyDict_SetItem(dict, efunname, callable) < 0)
                PyErr_Clear();
            Py_DECREF(efunname);
        }
    }

    result = PyDictProxy_New(dict);
    Py_DECREF(dict);
    return result;
} /* ldmud_registered_efuns_dict() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_registered_efuns_methods[] =
{
    {
        "__dir__",
        (PyCFunction)ldmud_registered_efuns_dir, METH_NOARGS,
        "__dir__() -> List\n\n"
        "Returns a list of all attributes."
    },

    {NULL}
};

static PyGetSetDef ldmud_registered_efuns_getset [] = {
    {"__dict__", (getter)ldmud_registered_efuns_dict, NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_registered_efuns_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.registered_efuns",           /* tp_name */
    0,                                  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    ldmud_registered_efuns_getattro,    /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "Registered Python efuns",          /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    ldmud_registered_efuns_iter,        /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_registered_efuns_methods,     /* tp_methods */
    0,                                  /* tp_members */
    ldmud_registered_efuns_getset,      /* tp_getset */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_registered_structs_getattro (PyObject *val, PyObject *name)

/* Return the struct type for a defined struct.
 */

{
    PyObject *result;
    bool error;
    string_t* sname;

    /* First check real attributes... */
    result = PyObject_GenericGetAttr(val, name);
    if (result || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return result;

    PyErr_Clear();

    /* And now look up registered structs. */
    sname = find_tabled_python_string(name, "struct name", &error);
    if (error)
        return NULL;

    if (sname)
    {
        ident_t *ident = find_shared_identifier_mstr(sname, I_TYPE_GLOBAL, 0);
        while (ident && ident->type != I_TYPE_GLOBAL)
            ident = ident->inferior;

        if (ident && ident->type == I_TYPE_GLOBAL
         && ident->u.global.python_struct_id != I_GLOBAL_PYTHON_STRUCT_OTHER
         && python_struct_table[ident->u.global.python_struct_id] != NULL)
        {
            return ldmud_concrete_struct_type_create(
                    ref_struct_name(python_struct_table[ident->u.global.python_struct_id]->name));
        }
    }

    PyErr_Format(PyExc_AttributeError, "No such registered struct: '%U'", name);
    return NULL;
} /* ldmud_registered_structs_getattro() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_registered_structs_dir (PyObject *self)

/* Returns a list of all attributes, this includes all registered struct names.
 */

{
    PyObject *result;
    PyObject *attrs = get_class_dir(self);

    if (attrs == NULL)
        return NULL;

    /* Now add all registered structs. */
    for (ident_t *ident = all_python_idents; ident; ident = ident->next_all)
    {
        if (ident->u.global.python_struct_id != I_GLOBAL_PYTHON_STRUCT_OTHER
         && python_struct_table[ident->u.global.python_struct_id] != NULL)
        {
            PyObject *sname = PyUnicode_FromStringAndSize(get_txt(ident->name), mstrsize(ident->name));

            if (sname == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PySet_Add(attrs, sname) < 0)
                PyErr_Clear();
            Py_DECREF(sname);
        }
    }

    /* And return the keys of our dict. */
    result = PySequence_List(attrs);
    Py_DECREF(attrs);
    return result;
} /* ldmud_registered_structs_dir() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_registered_structs_dict (ldmud_program_t *self, void *closure)

/* Returns a list of all registered structs.
 */

{
    PyObject *result, *dict = PyDict_New();
    if (!dict)
        return NULL;

    for (ident_t *ident = all_python_idents; ident; ident = ident->next_all)
    {
        struct_type_t *stype;
        PyObject *sname;

        if (ident->u.global.python_struct_id == I_GLOBAL_PYTHON_STRUCT_OTHER)
            continue;

        stype = python_struct_table[ident->u.global.python_struct_id];
        if (stype == NULL)
            continue;

        sname = PyUnicode_FromStringAndSize(get_txt(ident->name), mstrsize(ident->name));
        if (sname == NULL)
        {
            PyErr_Clear();
            continue;
        }

        if (PyDict_SetItem(dict, sname, ldmud_concrete_struct_type_create(ref_struct_name(stype->name))) < 0)
            PyErr_Clear();
        Py_DECREF(sname);
    }

    result = PyDictProxy_New(dict);
    Py_DECREF(dict);
    return result;
} /* ldmud_registered_structs_dict() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_registered_structs_methods[] =
{
    {
        "__dir__",
        (PyCFunction)ldmud_registered_structs_dir, METH_NOARGS,
        "__dir__() -> List\n\n"
        "Returns a list of all attributes."
    },

    {NULL}
};

static PyGetSetDef ldmud_registered_structs_getset [] = {
    {"__dict__", (getter)ldmud_registered_structs_dict, NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_registered_structs_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.registered_structs",         /* tp_name */
    0,                                  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    ldmud_registered_structs_getattro,  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "Registered Python structs",        /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_registered_structs_methods,   /* tp_methods */
    0,                                  /* tp_members */
    ldmud_registered_structs_getset,    /* tp_getset */
};

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_registered_types_getattro (PyObject *val, PyObject *name)

/* Return the class for a registered type.
 */

{
    PyObject *result;
    bool error;
    string_t* typename;
    ident_t *ident;

    /* First check real attributes... */
    result = PyObject_GenericGetAttr(val, name);
    if (result || !PyErr_ExceptionMatches(PyExc_AttributeError))
        return result;

    PyErr_Clear();

    /* And now look up registered types. */
    typename = find_tabled_python_string(name, "type name", &error);
    if (error)
        return NULL;

    if (typename)
    {
        ident = find_shared_identifier_mstr(typename, I_TYPE_PYTHON_TYPE, 0);
        while (ident && ident->type != I_TYPE_PYTHON_TYPE)
            ident = ident->inferior;

        if (ident && ident->type == I_TYPE_PYTHON_TYPE)
        {
            result = python_type_table[ident->u.python_type_id]->pytype;
            if (result)
            {
                Py_INCREF(result);
                return result;
            }
        }
    }

    PyErr_Format(PyExc_AttributeError, "No such registered type: '%U'", name);
    return NULL;
} /* ldmud_registered_types_getattro() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_registered_types_dir (PyObject *self)

/* Returns a list of all attributes, this includes all registered type names.
 */

{
    PyObject *result;
    PyObject *attrs = get_class_dir(self);

    if (attrs == NULL)
        return NULL;

    /* Now add all registered types. */
    for (ident_t *ident = all_python_types; ident; ident = ident->next_all)
    {
        if (python_type_table[ident->u.python_type_id]->pytype != NULL)
        {
            PyObject *typename = PyUnicode_FromStringAndSize(get_txt(ident->name), mstrsize(ident->name));

            if (typename == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PySet_Add(attrs, typename) < 0)
                PyErr_Clear();
            Py_DECREF(typename);
        }
    }

    /* And return the keys of our dict. */
    result = PySequence_List(attrs);
    Py_DECREF(attrs);
    return result;
} /* ldmud_registered_types_dir() */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_registered_types_dict (ldmud_program_t *self, void *closure)

/* Returns a list of all registered types.
 */

{
    PyObject *result, *dict = PyDict_New();
    if (!dict)
        return NULL;

    for (ident_t *ident = all_python_types; ident; ident = ident->next_all)
    {
        PyObject *type = python_type_table[ident->u.python_type_id]->pytype;
        if (type != NULL)
        {
            PyObject *typename = PyUnicode_FromStringAndSize(get_txt(ident->name), mstrsize(ident->name));

            if (typename == NULL)
            {
                PyErr_Clear();
                continue;
            }

            if (PyDict_SetItem(dict, typename, type) < 0)
                PyErr_Clear();
            Py_DECREF(typename);
        }
    }

    result = PyDictProxy_New(dict);
    Py_DECREF(dict);
    return result;
} /* ldmud_registered_types_dict() */

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_registered_types_methods[] =
{
    {
        "__dir__",
        (PyCFunction)ldmud_registered_types_dir, METH_NOARGS,
        "__dir__() -> List\n\n"
        "Returns a list of all attributes."
    },

    {NULL}
};

static PyGetSetDef ldmud_registered_types_getset [] = {
    {"__dict__", (getter)ldmud_registered_types_dict, NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_registered_types_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.registered_types",           /* tp_name */
    0,                                  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    ldmud_registered_types_getattro,    /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "Registered Python types",          /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_registered_types_methods,     /* tp_methods */
    0,                                  /* tp_members */
    ldmud_registered_types_getset,      /* tp_getset */
};

/*-------------------------------------------------------------------------*/
static Py_ssize_t
ldmud_call_stack_length (PyObject *stack)

/* Implement len() for the stack.
 */

{
    return control_stack_depth();
} /* ldmud_call_stack_length() */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_call_stack_item (PyObject *stack, Py_ssize_t idx)

/* Implement item access for the stack.
 */

{
    struct control_stack *frame;
    bytecode_p            frame_pc;
    program_t            *frame_prog;
    svalue_t              frame_ob;
    svalue_t             *frame_fp;
    const char           *frame_name;
    enum call_frame_type  frame_type;
#ifdef EVAL_COST_TRACE
    int32                 frame_eval_cost;
#endif

    if (idx < 0 || idx >= control_stack_depth())
    {
        PyErr_SetString(PyExc_IndexError, "index out of range");
        return NULL;
    }

    frame = control_stack_start() + idx;

    /* The stack stores the previous pc and prog.
     * For current value look up the global variables.
     */
    if (frame == csp)
    {
        frame_pc = inter_pc;
        frame_prog = current_prog;
#ifdef EVAL_COST_TRACE
        frame_eval_cost = eval_cost;
#endif
        frame_ob = current_object;
        frame_fp = inter_fp;
    }
    else
    {
        struct control_stack *next = frame + 1;
        frame_pc = next->pc;
        frame_fp = next->fp;
        frame_prog = next->prog;
#ifdef EVAL_COST_TRACE
        frame_eval_cost = next->eval_cost;
#endif
        /* The current object is stored in the next extern call frame. */
        while (true)
        {
            if (next > csp)
                frame_ob = current_object;
            else if (next->extern_call)
                frame_ob = next->ob;
            else
            {
                next++;
                continue;
            }
            break;
        }
    }

    assert(frame_prog != NULL);
    assert(frame_ob.type != T_NUMBER);

    if (frame->catch_call)
    {
        frame_type = CALL_FRAME_TYPE_CATCH;
        frame_name = "catch";
    }
    else if (frame_pc == NULL)
    {
        frame_type = CALL_FRAME_TYPE_ALIEN_LFUN_CLOSURE;
        frame_name = NULL;
    }
    else  if (frame->funstart == SIMUL_EFUN_FUNSTART)
    {
        frame_type = CALL_FRAME_TYPE_SIMUL_EFUN_CLOSURE;
        frame_name = get_txt(simul_efun_table[frame->instruction].function.name);
    }
    else  if (frame->funstart == EFUN_FUNSTART)
    {
        frame_type = CALL_FRAME_TYPE_EFUN_CLOSURE;
        frame_name = instrs[frame->instruction].name;
    }
    else  if (frame->funstart == PYTHON_EFUN_FUNSTART)
    {
        frame_type = CALL_FRAME_TYPE_PYTHON_EFUN_CLOSURE;
        frame_name = closure_python_efun_to_string(frame->instruction + CLOSURE_PYTHON_EFUN);
    }
    else if (frame->funstart < frame_prog->program || frame->funstart > PROGRAM_END(*frame_prog))
    {
        frame_type = CALL_FRAME_TYPE_LAMBDA;
        frame_name = NULL;
    }
    else
    {
        frame_type = CALL_FRAME_TYPE_LFUN;
        frame_name = get_txt(frame_prog->function_headers[FUNCTION_HEADER_INDEX(frame->funstart)].name);
    }

    return ldmud_call_frame_create(idx, frame_type, frame_ob, frame_prog, frame_pc, frame_fp, frame_name
#ifdef EVAL_COST_TRACE
                                  , frame_eval_cost
#endif
                                  );
} /* ldmud_call_stack_item() */

/*-------------------------------------------------------------------------*/
static PySequenceMethods ldmud_call_stack_as_sequence = {
    (lenfunc)ldmud_call_stack_length,           /* sq_length */
    0,                                          /* sq_concat */
    0,                                          /* sq_repeat */
    (ssizeargfunc)ldmud_call_stack_item,        /* sq_item */
    0,                                          /* sq_slice */
    0,                                          /* sq_ass_item */
    0,                                          /* sq_ass_slice */
    0,                                          /* sq_contains */
    0,                                          /* sq_inplace_concat */
    0,                                          /* sq_inplace_repeat */
};

static PyTypeObject ldmud_call_stack_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.call_stack",                 /* tp_name */
    0,                                  /* tp_basicsize */
    0,                                  /* tp_itemsize */
    0,                                  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
    &ldmud_call_stack_as_sequence,      /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "The current call stack",           /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    0,                                  /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
};


/*-------------------------------------------------------------------------*/
/* Module definition for the ldmud builtin module */

static int
ldmud_traverse (PyObject *self, visitproc visit, void *arg)

/* Garbage collection support for Python.
 * We visit our global variables here.
 */
{
    for (int i = 0; i < PYTHON_EFUN_TABLE_SIZE; i++)
        Py_VISIT(python_efun_table[i].callable);
    for (int i = 0; i < PYTHON_TYPE_TABLE_SIZE; i++)
        if (python_type_table[i] != NULL)
            Py_VISIT(python_type_table[i]->pytype);
    for (python_poll_fds_t *cur = poll_fds; cur != NULL; cur = cur->next)
    {
        Py_VISIT(cur->eventsfun);
        Py_VISIT(cur->fun);
    }
    for (int i = 0; i < PYTHON_HOOK_COUNT; i++)
        for (python_hook_t* cur = python_hooks[i]; cur != NULL; cur = cur->next)
            Py_VISIT(cur->fun);
#ifdef USE_PYTHON_CONTEXT
    Py_VISIT(python_contextvar_current_object);
    Py_VISIT(python_contextvar_command_giver);
#endif

    return 0;
} /* ldmud_traverse() */

static int
ldmud_clear (PyObject *self)

/* Garbage collection support for Python.
 * We clear our global variables here.
 */
{
    for (int i = 0; i < PYTHON_EFUN_TABLE_SIZE; i++)
        Py_CLEAR(python_efun_table[i].callable);
    for (int i = 0; i < PYTHON_TYPE_TABLE_SIZE; i++)
        if (python_type_table[i] != NULL)
            Py_CLEAR(python_type_table[i]->pytype);
    for (python_poll_fds_t *cur = poll_fds; cur != NULL; cur = cur->next)
    {
        Py_CLEAR(cur->eventsfun);
        Py_CLEAR(cur->fun);
    }
    for (int i = 0; i < PYTHON_HOOK_COUNT; i++)
        for (python_hook_t* cur = python_hooks[i]; cur != NULL; cur = cur->next)
            Py_CLEAR(cur->fun);
#ifdef USE_PYTHON_CONTEXT
    Py_CLEAR(python_contextvar_current_object);
    Py_CLEAR(python_contextvar_command_giver);
#endif

    return 0;
} /* ldmud_clear() */

static PyObject*
ldmud_get_master (PyObject* self, PyObject* args UNUSED)

/* Return the master object or None.
 */

{
    if(master_ob == NULL)
        Py_RETURN_NONE;

    return ldmud_object_create(master_ob);
} /* ldmud_get_master() */

static PyObject*
ldmud_get_simul_efun (PyObject* self, PyObject* args UNUSED)

/* Return the simul_efun object or None.
 */

{
    if(simul_efun_object == NULL)
        Py_RETURN_NONE;

    return ldmud_object_create(simul_efun_object);
} /* ldmud_get_simul_efun() */

static PyMethodDef ldmud_methods[] =
{
    {
        "register_efun",
        (PyCFunction) python_register_efun, METH_VARARGS | METH_KEYWORDS,
        "register_efun(name, function) -> None\n\n"
        "Registers a new efun name. This is not allowed during\n"
        "compilation of an LPC object and must not be a permanent\n"
        "define or reserved word."
    },

    {
        "unregister_efun",
        (PyCFunction) python_unregister_efun, METH_VARARGS | METH_KEYWORDS,
        "unregister_efun(name) -> None\n\n"
        "Removes a python efun from registration. This is not allowed\n"
        "during compilation of an LPC object."
    },

    {
        "register_struct",
        (PyCFunction) python_register_struct, METH_VARARGS | METH_KEYWORDS,
        "register_struct(name, base, fields) -> None\n\n"
        "Defines a new global struct. This is not allowed during\n"
        "compilation of an LPC object and must not be a permanent\n"
        "define or reserved word."
    },

    {
        "unregister_struct",
        (PyCFunction) python_unregister_struct, METH_VARARGS | METH_KEYWORDS,
        "unregister_struct(name) -> None\n\n"
        "Removes a Python-defined struct from registration. This is not\n"
        "allowed during compilation of an LPC object."
    },

    {
        "register_type",
        (PyCFunction) python_register_type, METH_VARARGS | METH_KEYWORDS,
        "register_type(name, type) -> None\n\n"
        "Registers a new type. This is not allowed during\n"
        "compilation of an LPC object and must not be a permanent define."
    },

    {
        "unregister_type",
        (PyCFunction) python_unregister_type, METH_VARARGS | METH_KEYWORDS,
        "unregister_type(name) -> None\n\n"
        "Removes a python type from registration. This is not allowed\n"
        "during compilation of an LPC object."
    },

    {
        "register_socket",
        (PyCFunction) python_register_socket, METH_VARARGS | METH_KEYWORDS,
        "register_socket(fd, function [, eventmask]) -> None\n\n"
        "Register a file descriptor (an integer or an object with\n"
        "a fileno() method returning an int) for monitoring."
    },

    {
        "unregister_socket",
        (PyCFunction) python_unregister_socket, METH_VARARGS | METH_KEYWORDS,
        "unregister_socket(fd) -> None\n\n"
        "Remove a file descriptor from being monitored."
    },

    {
        "register_hook",
        (PyCFunction) python_register_hook, METH_VARARGS | METH_KEYWORDS,
        "register_hook(hook, function) -> None\n\n"
        "Register a hook. The python function will be called whenever\n"
        "<hook> happens. <hook> can be one of the following:\n\n"
        "  ON_HEARTBEAT\n"
        "    Called without arguments for every backend loop\n\n"
        "  ON_OBJECT_CREATED\n"
        "    Called whenever an object was created, with the object\n"
        "    as its first and only argument. This call happens before\n"
        "    any LPC code of the object ran.\n\n"
        "  ON_OBJECT_DESTRUCTED\n"
        "    Called just before an object will be destructed,\n"
        "    with the object as its first and only argument.\n\n"
        "  ON_CHILD_PROCESS_TERMINATED\n"
        "    Called without any arguments whenever a SIGCHLD signal\n"
        "    was received. This could also happen for processes\n"
        "    spawned by the driver itself (eg. erq)."
    },

    {
        "unregister_hook",
        (PyCFunction) python_unregister_hook, METH_VARARGS | METH_KEYWORDS,
        "unregister_hook(hook, function) -> None\n\n"
        "Removes a hook function."
    },

    {
        "get_master", ldmud_get_master, METH_NOARGS,
        "get_master() -> Master Object\n\n"
        "Returns the master object."
    },

    {
        "get_simul_efun", ldmud_get_simul_efun, METH_NOARGS,
        "get_simul_efun() -> Simul-Efun Object\n\n"
        "Returns the simul_efun object."
    },

    {NULL, NULL, 0, NULL}
};

static PyModuleDef ldmud_module =
{
    PyModuleDef_HEAD_INIT,
    "ldmud",                            /* m_name */
    "This module provides access to the LDMud interpreter.",
                                        /* m_doc */
    -1,                                 /* m_size */
    ldmud_methods,                      /* m_methods */
    NULL,                               /* m_slots */
    ldmud_traverse,                     /* m_traverse */
    ldmud_clear,                        /* m_clear */
    NULL                                /* m_free */
};

static bool
register_lpctype (PyObject *module, const char* name, ldmud_lpctype_t* type)

/* Register <type> in <module> as <name>.
 * Update <type> so that it inherits ldmud_lpctype_type.
 * Returns true on success.
 */

{
    assert(PyObject_TypeCheck(&type->type_base, &ldmud_lpctype_type.type_base));

    if (!type->type_base.tp_mro)
    {
        PyObject *mro = PyTuple_New(type == &ldmud_lpctype_type ? 3 : 2);
        int pos = 0;
        if (!mro)
            return false;

        PyTuple_SET_ITEM(mro, pos++, (PyObject*) type);
        Py_INCREF(type);
        if (type == &ldmud_lpctype_type)
        {
            PyTuple_SET_ITEM(mro, pos++, (PyObject*) &PyType_Type);
            Py_INCREF(&PyType_Type);
        }
        PyTuple_SET_ITEM(mro, pos++, (PyObject*) &PyBaseObject_Type);
        Py_INCREF(&PyBaseObject_Type);

        type->type_base.tp_mro = mro;
    }

    if (!type->type_base.tp_basicsize && type->type_base.tp_base)
    {
        type->type_base.tp_basicsize = type->type_base.tp_base->tp_basicsize;
        type->type_base.tp_itemsize = type->type_base.tp_base->tp_itemsize;
    }

    if (PyType_Ready(&type->type_base) < 0)
        return false;

    Py_INCREF(type);
    PyModule_AddObject(module, name, (PyObject*) type);

    return true;
} /* register_lpctype() */

static PyObject*
init_ldmud_module ()

/* Create the ldmud module.
 */

{
    PyObject *module, *efuns;

    /* Initialize module. */
    module = PyModule_Create(&ldmud_module);
    if (module == NULL)
        return module;

    /* Add types to module. */
    Py_INCREF(&PyType_Type);
    if (PyType_Type.tp_basicsize > ldmud_lpctype_type.type_base.tp_basicsize)
        ldmud_lpctype_type.type_base.tp_basicsize = PyType_Type.tp_basicsize;
    if (PyType_Type.tp_basicsize > ldmud_union_type_type.tp_basicsize)
        ldmud_union_type_type.tp_basicsize = PyType_Type.tp_basicsize;
    if (PyType_Type.tp_basicsize > ldmud_concrete_array_type_type.tp_basicsize)
        ldmud_concrete_array_type_type.tp_basicsize = PyType_Type.tp_basicsize;
    if (PyType_Type.tp_basicsize > ldmud_any_array_type_type.tp_basicsize)
        ldmud_any_array_type_type.tp_basicsize = PyType_Type.tp_basicsize;
    if (PyType_Type.tp_basicsize > ldmud_concrete_struct_type_type.tp_basicsize)
        ldmud_concrete_struct_type_type.tp_basicsize = PyType_Type.tp_basicsize;
    if (PyType_Type.tp_basicsize > ldmud_any_struct_type_type.tp_basicsize)
        ldmud_any_struct_type_type.tp_basicsize = PyType_Type.tp_basicsize;
    if (PyType_Type.tp_basicsize > ldmud_concrete_object_type_type.tp_basicsize)
        ldmud_concrete_object_type_type.tp_basicsize = PyType_Type.tp_basicsize;
    if (PyType_Type.tp_basicsize > ldmud_any_object_type_type.tp_basicsize)
        ldmud_any_object_type_type.tp_basicsize = PyType_Type.tp_basicsize;
    if (PyType_Type.tp_basicsize > ldmud_concrete_lwobject_type_type.tp_basicsize)
        ldmud_concrete_lwobject_type_type.tp_basicsize = PyType_Type.tp_basicsize;
    if (PyType_Type.tp_basicsize > ldmud_any_lwobject_type_type.tp_basicsize)
        ldmud_any_lwobject_type_type.tp_basicsize = PyType_Type.tp_basicsize;

#ifdef Py_TPFLAGS_DISALLOW_INSTANTIATION
    ldmud_void_type.type_base.tp_flags |= Py_TPFLAGS_DISALLOW_INSTANTIATION;
#endif

    if (!register_lpctype(module, "LPCType",              &ldmud_lpctype_type)
     || !register_lpctype(module, "Integer",              &ldmud_integer_type)
     || !register_lpctype(module, "Float",                &ldmud_float_type)
     || !register_lpctype(module, "String",               &ldmud_string_type)
     || !register_lpctype(module, "Bytes",                &ldmud_bytes_type)
     || !register_lpctype(module, "Object",               &ldmud_object_type)
     || !register_lpctype(module, "LWObject",             &ldmud_lwobject_type)
     || !register_lpctype(module, "Array",                &ldmud_array_type)
     || !register_lpctype(module, "Mapping",              &ldmud_mapping_type)
     || !register_lpctype(module, "Struct",               &ldmud_struct_type)
     || !register_lpctype(module, "Closure",              &ldmud_closure_type)
     || !register_lpctype(module, "LfunClosure",          &ldmud_lfun_closure_type)
     || !register_lpctype(module, "IdentifierClosure",    &ldmud_identifier_closure_type)
     || !register_lpctype(module, "LambdaClosure",        &ldmud_lambda_closure_type)
     || !register_lpctype(module, "UnboundLambdaClosure", &ldmud_unbound_lambda_closure_type)
     || !register_lpctype(module, "BoundLambdaClosure",   &ldmud_bound_lambda_closure_type)
     || !register_lpctype(module, "EfunClosure",          &ldmud_efun_closure_type)
     || !register_lpctype(module, "SimulEfunClosure",     &ldmud_simul_efun_closure_type)
     || !register_lpctype(module, "OperatorClosure",      &ldmud_operator_closure_type)
     || !register_lpctype(module, "Coroutine",            &ldmud_coroutine_type)
     || !register_lpctype(module, "Symbol",               &ldmud_symbol_type)
     || !register_lpctype(module, "QuotedArray",          &ldmud_quoted_array_type)
     || !register_lpctype(module, "Mixed",                &ldmud_mixed_type)
     || !register_lpctype(module, "Void",                 &ldmud_void_type))
        return NULL;

#ifndef Py_TPFLAGS_DISALLOW_INSTANTIATION
    if (ldmud_void_type.type_base.tp_dict != NULL)
    {
        /* Remove the __new__ function. */
        if (PyDict_DelItemString(ldmud_void_type.type_base.tp_dict, "__new__"))
            PyErr_Clear();
    }
#endif

    /* Initialize (non-lpctype) types. */
    if (PyType_Ready(&ldmud_union_type_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_union_type_iter_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_concrete_array_type_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_any_array_type_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_concrete_struct_type_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_any_struct_type_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_concrete_object_type_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_any_object_type_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_concrete_lwobject_type_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_any_lwobject_type_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_program_functions_iter_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_program_functions_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_program_lfun_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_program_lfun_argument_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_program_variables_iter_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_program_variables_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_program_variable_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_mapping_list_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_mapping_iter_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_struct_members_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_struct_member_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_coroutine_variables_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_lvalue_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_lvalue_struct_members_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_instruction_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_local_variables_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_local_variable_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_call_frame_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_registered_efuns_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_registered_structs_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_registered_types_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_python_efun_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_registered_efuns_iter_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_call_stack_type) < 0)
        return NULL;

    ldmud_interrupt_exception_type.tp_base = (PyTypeObject *) PyExc_RuntimeError;
    if (PyType_Ready(&ldmud_interrupt_exception_type) < 0)
        return NULL;

    Py_INCREF(&ldmud_lvalue_type);
    PyModule_AddObject(module, "Lvalue", (PyObject*) &ldmud_lvalue_type);
    Py_INCREF(&ldmud_interrupt_exception_type);
    PyModule_AddObject(module, "InterruptException", (PyObject*) &ldmud_interrupt_exception_type);

    /* Add constants to module. */
    assert(PYTHON_HOOK_COUNT == sizeof(python_hook_names) / sizeof(python_hook_names[0]));
    for (int i = 0; i < PYTHON_HOOK_COUNT; i++)
        PyModule_AddIntConstant(module, python_hook_names[i], i);

    assert(LFUN_FLAG_COUNT == sizeof(lfun_flag_names) / sizeof(lfun_flag_names[0]));
    for (int i = 0; i < LFUN_FLAG_COUNT; i++)
        PyModule_AddIntConstant(module, lfun_flag_names[i], 1 << i);

    assert(LFUN_ARGUMENT_FLAG_COUNT == sizeof(lfun_argument_flag_names) / sizeof(lfun_argument_flag_names[0]));
    for (int i = 0; i < LFUN_ARGUMENT_FLAG_COUNT; i++)
        PyModule_AddIntConstant(module, lfun_argument_flag_names[i], 1 << i);

    assert(VARIABLE_FLAG_COUNT == sizeof(variable_flag_names) / sizeof(variable_flag_names[0]));
    for (int i = 0; i < VARIABLE_FLAG_COUNT; i++)
        PyModule_AddIntConstant(module, variable_flag_names[i], 1 << i);

    assert(VISIBILITY_COUNT == sizeof(visibility_names) / sizeof(visibility_names[0]));
    for (int i = 0; i < VISIBILITY_COUNT; i++)
        PyModule_AddIntConstant(module, visibility_names[i], i);

    assert(CALL_FRAME_TYPE_COUNT == sizeof(call_frame_type_names) / sizeof(call_frame_type_names[0]));
    for (int i = 0; i < CALL_FRAME_TYPE_COUNT; i++)
        PyModule_AddIntConstant(module, call_frame_type_names[i], i);

    /* Add the efuns as a sub-namespace. */
    efuns = create_efun_namespace();
    if (!efuns)
        return NULL;
    PyModule_AddObject(module, "efuns", efuns);
    PyModule_AddObject(module, "registered_efuns", ldmud_registered_efuns_type.tp_alloc(&ldmud_registered_efuns_type, 0));
    PyModule_AddObject(module, "registered_structs", ldmud_registered_structs_type.tp_alloc(&ldmud_registered_structs_type, 0));
    PyModule_AddObject(module, "registered_types", ldmud_registered_types_type.tp_alloc(&ldmud_registered_types_type, 0));
    PyModule_AddObject(module, "call_stack", ldmud_call_stack_type.tp_alloc(&ldmud_call_stack_type, 0));

    return module;
} /* init_ldmud_module() */

/*=========================================================================*/

/*                          Helper functions                               */

/*-------------------------------------------------------------------------*/

static PyObject*
lpctype_to_pythontype (lpctype_t *type)

/* Creates a Python type from an lpctype_t.
 * Unsupported types will be returned as NULL.
 */

{
    if (!type)
        return NULL;

    switch (type->t_class)
    {
        case TCLASS_PRIMARY:
            switch (type->t_primary)
            {
                case TYPE_NUMBER:
                    Py_INCREF(&ldmud_integer_type);
                    return (PyObject *)&ldmud_integer_type;

                case TYPE_STRING:
                    Py_INCREF(&ldmud_string_type);
                    return (PyObject *)&ldmud_string_type;

                case TYPE_VOID:
                    Py_INCREF(&ldmud_void_type);
                    return (PyObject *)&ldmud_void_type;

                case TYPE_MAPPING:
                    Py_INCREF(&ldmud_mapping_type);
                    return (PyObject *)&ldmud_mapping_type;

                case TYPE_FLOAT:
                    Py_INCREF(&ldmud_float_type);
                    return (PyObject *)&ldmud_float_type;

                case TYPE_CLOSURE:
                    Py_INCREF(&ldmud_closure_type);
                    return (PyObject *)&ldmud_closure_type;

                case TYPE_COROUTINE:
                    Py_INCREF(&ldmud_coroutine_type);
                    return (PyObject *)&ldmud_coroutine_type;

                case TYPE_SYMBOL:
                    Py_INCREF(&ldmud_symbol_type);
                    return (PyObject *)&ldmud_symbol_type;

                case TYPE_QUOTED_ARRAY:
                    Py_INCREF(&ldmud_quoted_array_type);
                    return (PyObject *)&ldmud_quoted_array_type;

                case TYPE_BYTES:
                    Py_INCREF(&ldmud_bytes_type);
                    return (PyObject *)&ldmud_bytes_type;

                case TYPE_LPCTYPE:
                    Py_INCREF(&ldmud_lpctype_type);
                    return (PyObject *)&ldmud_lpctype_type;

                case TYPE_UNKNOWN:
                case TYPE_ANY:
                    Py_INCREF(&ldmud_mixed_type);
                    return (PyObject *)&ldmud_mixed_type;
            }
            break;

        case TCLASS_STRUCT:
        {
            struct_name_t *name = type->t_struct.name;
            if (name)
                return ldmud_concrete_struct_type_create(ref_struct_name(name));
            else
            {
                Py_INCREF(&ldmud_struct_type);
                return (PyObject *)&ldmud_struct_type;
            }
        }

        case TCLASS_OBJECT:
            switch (type->t_object.type)
            {
                case OBJECT_REGULAR:
                    if (type->t_object.program_name == NULL)
                    {
                        Py_INCREF(&ldmud_object_type);
                        return (PyObject *)&ldmud_object_type;
                    }
                    else
                        return ldmud_concrete_object_type_create(ref_lpctype(type));

                case OBJECT_LIGHTWEIGHT:
                    if (type->t_object.program_name == NULL)
                    {
                        Py_INCREF(&ldmud_lwobject_type);
                        return (PyObject *)&ldmud_lwobject_type;
                    }
                    else
                        return ldmud_concrete_lwobject_type_create(ref_lpctype(type));
            }
            break;

        case TCLASS_PYTHON:
        {
            PyObject *pytype = python_type_table[type->t_python.type_id]->pytype;
            if (pytype)
            {
                Py_INCREF(pytype);
                return pytype;
            }
            break;
        }

        case TCLASS_ARRAY:
        {
            lpctype_t *elem = type->t_array.element;
            if (elem == lpctype_mixed)
            {
                Py_INCREF(&ldmud_array_type);
                return (PyObject *)&ldmud_array_type;
            }
            else
            {
                PyObject *pelem = lpctype_to_pythontype(elem);
                PyObject *result = NULL;
                if (pelem != NULL && PyObject_TypeCheck(pelem, &ldmud_lpctype_type.type_base))
                    result = ldmud_concrete_array_type_create((ldmud_lpctype_t*)pelem);

                Py_XDECREF(pelem);
                return result;
            }
        }

        case TCLASS_UNION:
            return ldmud_union_type_create(ref_lpctype(type));
    }

    return NULL;
} /* lpctype_to_pythontype() */


/*-------------------------------------------------------------------------*/

static lpctype_t*
pythontype_to_lpctype (PyObject* ptype)

/* Creates an lpctype_t object from the given Python type.
 * <ptype> may also be None, which is translated to void.
 * Returns NULL for any non-type value or unknown type,
 * a ref-counted instance of an lpctype_t otherwise.
 */

{
    if (ptype == Py_None)
        return lpctype_void;
    else if (PyObject_TypeCheck(ptype, &ldmud_lpctype_type.type_base))
        return ((ldmud_lpctype_t*)ptype)->get_lpctype((ldmud_lpctype_t*)ptype);
    else if (PyType_Check(ptype))
    {
        for (int i = 0; i < PYTHON_TYPE_TABLE_SIZE; i++)
            if (python_type_table[i] != NULL)
            {
                PyObject * regtype = python_type_table[i]->pytype;
                if (regtype && PyObject_IsSubclass(ptype, regtype))
                    return get_python_type(i);
            }

        if (PyObject_IsSubclass(ptype, (PyObject*) &PyLong_Type))
            return lpctype_int;

        if (PyObject_IsSubclass(ptype, (PyObject*) &PyBool_Type))
            return lpctype_int;

        if (PyObject_IsSubclass(ptype, (PyObject*) &PyFloat_Type))
            return lpctype_float;

        if (PyObject_IsSubclass(ptype, (PyObject*) &PyBytes_Type))
            return lpctype_bytes;

        if (PyObject_IsSubclass(ptype, (PyObject*) &PyUnicode_Type))
            return lpctype_string;

        return NULL;
    }
    else  if (PyTuple_Check(ptype))
    {
        /* We regard tuples as LPC union types. */
        lpctype_t *result = NULL;

        for (Py_ssize_t len = PyTuple_Size(ptype); len--; )
        {
            lpctype_t *part = pythontype_to_lpctype(PyTuple_GetItem(ptype, len));
            if (!part)
                continue;

            if (!result)
                result = part;
            else
            {
                lpctype_t *old = result;

                result = get_union_type(result, part);
                free_lpctype(old);
                free_lpctype(part);
            }
        }

        return result;
    }
    else
        return NULL;

} /* pythontype_to_lpctype() */


/*-------------------------------------------------------------------------*/

static PyObject*
adapt_pythontype (PyObject *ptype)

/* Return the LPCType representation of <ptype>. If <ptype> already is
 * an ldmud_lpctype_type return a new referece to it, otherwise create
 * the corresponding ldmud_lpctype_type and return it. Returns NULL
 * and sets an exception upon an error.
 */

{
    if (ptype == Py_None
     || PyObject_TypeCheck(ptype, &ldmud_lpctype_type.type_base))
    {
        Py_INCREF(ptype);
        return ptype;
    }
    else if (PyType_Check(ptype))
    {
        for (int i = 0; i < PYTHON_TYPE_TABLE_SIZE; i++)
            if (python_type_table[i] != NULL
             && python_type_table[i]->pytype != NULL
             && PyObject_IsSubclass(ptype, python_type_table[i]->pytype))
            {
                Py_INCREF(ptype);
                return ptype;
            }

        if (PyObject_IsSubclass(ptype, (PyObject*) &PyLong_Type)
         || PyObject_IsSubclass(ptype, (PyObject*) &PyBool_Type))
        {
            Py_INCREF(&ldmud_integer_type);
            return (PyObject *)&ldmud_integer_type;
        }

        if (PyObject_IsSubclass(ptype, (PyObject*) &PyFloat_Type))
        {
            Py_INCREF(&ldmud_float_type);
            return (PyObject *)&ldmud_float_type;
        }

        if (PyObject_IsSubclass(ptype, (PyObject*) &PyBytes_Type))
        {
            Py_INCREF(&ldmud_bytes_type);
            return (PyObject *)&ldmud_bytes_type;
        }

        if (PyObject_IsSubclass(ptype, (PyObject*) &PyUnicode_Type))
        {
            Py_INCREF(&ldmud_string_type);
            return (PyObject *)&ldmud_string_type;
        }
    }
    else  if (PyTuple_Check(ptype))
    {
        lpctype_t *t = pythontype_to_lpctype(ptype);
        if (t)
        {
            PyObject *result = lpctype_to_pythontype(t);
            free_lpctype(t);
            if (result)
                return result;
        }
    }

    PyErr_Format(PyExc_TypeError, "unsupported type '%R'", ptype);
    return NULL;
} /* adapt_pythontype() */


/*-------------------------------------------------------------------------*/

static PyObject*
svalue_to_python (svalue_t *svp)

/* Creates a python value from an svalue_t.
 */

{
    switch (svp->type)
    {
        case T_NUMBER:
            return PyLong_FromLong(svp->u.number);

        case T_FLOAT:
            return PyFloat_FromDouble(READ_DOUBLE(svp));

        case T_STRING:
            return PyUnicode_Decode(get_txt(svp->u.str), mstrsize(svp->u.str), "utf-8", "replace");

        case T_BYTES:
            return PyBytes_FromStringAndSize(get_txt(svp->u.str), mstrsize(svp->u.str));

        case T_POINTER:
            return ldmud_array_create(svp->u.vec);

        case T_OBJECT:
            return ldmud_object_create(svp->u.ob);

        case T_LWOBJECT:
            return ldmud_lwobject_create(svp->u.lwob);

        case T_MAPPING:
            return ldmud_mapping_create(svp->u.map);

        case T_CLOSURE:
            return ldmud_closure_create(svp);

        case T_COROUTINE:
            return ldmud_coroutine_create(svp->u.coroutine);

        case T_SYMBOL:
            return ldmud_symbol_create(svp);

        case T_QUOTED_ARRAY:
            return ldmud_quoted_array_create(svp);

        case T_STRUCT:
            return ldmud_struct_create(svp->u.strct);

        case T_LVALUE:
            return ldmud_lvalue_create(svp);

        case T_PYTHON:
        {
            PyObject *val = (PyObject*)svp->u.generic;
            Py_INCREF(val);
            return val;
        }

        case T_LPCTYPE:
        {
            PyObject *val = lpctype_to_pythontype(svp->u.lpctype);
            if (!val)
                PyErr_Format(PyExc_TypeError, "unsupported lpctype [%s]", get_lpctype_name(svp->u.lpctype));
            return val;
        }

        default:
            PyErr_Format(PyExc_TypeError, "unsupported type %d", svp->type);
            return NULL;
    }

} /* svalue_to_python() */


/*-------------------------------------------------------------------------*/

static PyObject*
rvalue_to_python (svalue_t *svp)

/* Creates a python value from an svalue_t, but unravels any lvalues.
 */

{
    PyObject* ob = svalue_to_python(svp);
    PyObject* rv;
    if (!ob || !ldmud_lvalue_check(ob))
        return ob;

    rv = ldmud_lvalue_get_value((ldmud_lvalue_t*)ob, NULL);
    Py_DECREF(ob);
    return rv;
} /* rvalue_to_python() */

/*-------------------------------------------------------------------------*/

static string_t*
python_string_to_mstring (PyObject* pstr, bool tabled)

/* Convert a Python string to an LPC string.
 * If it's not a Python string or not decodable to UTF-8 return NULL.
 */

{
    PyObject *utf8;
    Py_ssize_t length;
    char * buf;
    string_t * str;

    if (!PyUnicode_Check(pstr))
        return NULL;

    utf8 = PyUnicode_AsEncodedString(pstr, "utf-8", "replace");
    if (utf8 == NULL)
    {
        PyErr_Clear();
        return NULL;
    }

    if (PyBytes_AsStringAndSize(utf8, &buf, &length) < 0)
    {
        PyErr_Clear();
        return NULL;
    }

    if (tabled)
        str = new_n_unicode_tabled(buf, length);
    else
        str = new_n_unicode_mstring(buf, length);
    Py_DECREF(utf8);

    return str;
} /* python_string_to_mstring() */
/*-------------------------------------------------------------------------*/

static string_t*
python_string_to_string (PyObject* pstr)

/* Convert a Python string to an untabled LPC string.
 * If it's not a Python string or not decodable to UTF-8 return NULL.
 */

{
    return python_string_to_mstring(pstr, false);
} /* python_string_to_string() */

/*-------------------------------------------------------------------------*/

static string_t*
python_string_to_tabled_string (PyObject* pstr)

/* Convert a Python string to a tabled LPC string.
 * If it's not a Python string or not decodable to UTF-8 return NULL.
 */

{
    return python_string_to_mstring(pstr, true);
} /* python_string_to_tabled_string() */

/*-------------------------------------------------------------------------*/

static const char*
python_to_svalue (svalue_t *dest, PyObject* val)

/* Convert a python value <val> back to an LPC value
 * and puts the result into <dest>. Returns NULL
 * for success, an error message otherwise.
 *
 * <dest> is assumed to be empty.
 */

{
    if (val == Py_None)
    {
        put_number(dest, 0);
        return NULL;
    }

    /* First we check our own types. */
    if (PyType_Check(val))
    {
        lpctype_t *t = pythontype_to_lpctype(val);
        if (!t)
            return "unknown type";

        put_lpctype(dest, t);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_object_type.type_base))
    {
        object_t *ob = ((ldmud_object_t*)val)->lpc_object;
        if (ob != NULL && !(ob->flags & O_DESTRUCTED))
            put_ref_object(dest, ((ldmud_object_t*)val)->lpc_object, "python_to_svalue");
        else
            put_number(dest, 0);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_lwobject_type.type_base))
    {
        lwobject_t *lwob = ((ldmud_lwobject_t*)val)->lpc_lwobject;
        if (lwob != NULL)
            put_ref_lwobject(dest, lwob);
        else
            put_number(dest, 0);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_array_type.type_base))
    {
        vector_t* arr = ((ldmud_array_t*)val)->lpc_array;
        if (arr != NULL)
            put_ref_array(dest, arr);
        else
            put_number(dest, 0);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_mapping_type.type_base))
    {
        mapping_t* m = ((ldmud_mapping_t*)val)->lpc_mapping;
        if (m != NULL)
            put_ref_mapping(dest, m);
        else
            put_number(dest, 0);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_struct_type.type_base))
    {
        struct_t* s = ((ldmud_struct_t*)val)->lpc_struct;
        if (s != NULL)
            put_ref_struct(dest, s);
        else
            put_number(dest, 0);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_closure_type.type_base))
    {
        assign_svalue_no_free(dest, &((ldmud_closure_t*)val)->lpc_closure);
        if (dest->type == T_INVALID)
            put_number(dest, 0);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_coroutine_type.type_base))
    {
        coroutine_t *cr = ((ldmud_coroutine_t*)val)->lpc_coroutine;
        if(cr != NULL)
            put_ref_coroutine(dest, cr);
        else
            put_number(dest, 0);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_symbol_type.type_base))
    {
        assign_svalue_no_free(dest, &((ldmud_symbol_t*)val)->lpc_symbol);
        if (dest->type == T_INVALID)
            put_number(dest, 0);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_quoted_array_type.type_base))
    {
        assign_svalue_no_free(dest, &((ldmud_quoted_array_t*)val)->lpc_quoted_array);
        if (dest->type == T_INVALID)
            put_number(dest, 0);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_lvalue_type))
    {
        assign_svalue_no_free(dest, &((ldmud_lvalue_t*)val)->lpc_lvalue);
        if (dest->type == T_INVALID)
            put_number(dest, 0);
        return NULL;
    }

    /* Next come the registered types. */
    for (int idx = 0; idx < PYTHON_TYPE_TABLE_SIZE; idx++)
    {
        if (python_type_table[idx] != NULL
         && python_type_table[idx]->pytype != NULL
         && PyObject_TypeCheck(val, (PyTypeObject*) python_type_table[idx]->pytype))
        {
            dest->type = T_PYTHON;
            dest->x.python_type = idx;
            dest->u.generic = val;

            Py_INCREF(val);
            num_lpc_python_references++;
            return NULL;
        }
    }

    /* And now the python ones. */

    if (PyLong_Check(val))
    {
        int overflow;
        long num = PyLong_AsLongAndOverflow(val, &overflow);

        if (overflow || num < PINT_MIN || num > PINT_MAX)
            return "integer overflow";

        put_number(dest, (p_int)num);
        return NULL;
    }

    if (PyBool_Check(val))
    {
        put_number(dest, val == Py_True ? 1 : 0);
        return NULL;
    }

    if (PyFloat_Check(val))
    {
        put_float(dest, PyFloat_AsDouble(val));
        return NULL;
    }

    if (PyBytes_Check(val))
    {
        Py_ssize_t length;
        char * buf;
        string_t * str;

        PyBytes_AsStringAndSize(val, &buf, &length);
        str = new_n_mstring(buf, length, STRING_BYTES);
        if (str == NULL)
            return "out of memory";
        put_bytes(dest, str);
        return NULL;
    }

    if (PyUnicode_Check(val))
    {
        string_t * str = python_string_to_string(val);
        if (str == NULL)
            return "undecodable unicode string";

        put_string(dest, str);
        return NULL;
    }

    return "unknown type";

} /* python_to_svalue() */

/*-------------------------------------------------------------------------*/
static bool
python_object_to_object (PyObject *ob, const char* valname, svalue_t* val, program_t **progp)

/* Converts a Python object <ob> to <val>. Returns true on success, false otherwise.
 * Sets a Python exception accordingly. <valname> is used for error messages to describe the
 * value.
 */

{
    if (PyObject_TypeCheck(ob, &ldmud_object_type.type_base))
    {
        object_t *lpc_ob = ((ldmud_object_t*)ob)->lpc_object;
        if (!lpc_ob)
        {
            PyErr_SetString(PyExc_TypeError, "uninitialized lpc object");
            return false;
        }

        if (progp)
        {
            if (O_PROG_SWAPPED(lpc_ob) && load_ob_from_swap(lpc_ob) < 0)
            {
                PyErr_SetString(PyExc_MemoryError, "out of memory while unswapping");
                return false;
            }

            *progp = lpc_ob->prog;
        }

        *val = svalue_object(lpc_ob);
    }
    else if (PyObject_TypeCheck(ob, &ldmud_lwobject_type.type_base))
    {
        lwobject_t *lwob = ((ldmud_lwobject_t*)ob)->lpc_lwobject;
        if (!lwob)
        {
            PyErr_SetString(PyExc_TypeError, "uninitialized lpc lwobject");
            return false;
        }

        if (progp)
            *progp = lwob->prog;

        *val = svalue_lwobject(lwob);
    }
    else
    {
        PyErr_Format(PyExc_TypeError, "%s must be an ldmud.Object or ldmud.LWObject", valname);
        return false;
    }

    return true;
} /* python_object_to_object() */

/*-------------------------------------------------------------------------*/


static bool
python_eq_svalue (PyObject* pval, svalue_t *sval)

/* Checks a LPC value with a Python value for equality.
 */

{
    if (pval == Py_None)
        return false;

    /* Simple case, a registered Python object. */
    if (sval->type == T_PYTHON)
        return pval == sval->u.generic;

    /* Then we check our own types. */
    if (PyType_Check(pval))
    {
        if (sval->type == T_LPCTYPE)
        {
            lpctype_t *t = pythontype_to_lpctype(pval);
            bool result;
            if (!t)
                return false;

            result = sval->u.lpctype == t;
            free_lpctype(t);
            return result;
        }
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_lpctype_type.type_base))
    {
        if (sval->type == T_OBJECT)
            return sval->u.ob == ((ldmud_object_t*)pval)->lpc_object;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_lwobject_type.type_base))
    {
        if (sval->type == T_LWOBJECT)
            return sval->u.lwob == ((ldmud_lwobject_t*)pval)->lpc_lwobject;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_array_type.type_base))
    {
        if (sval->type == T_POINTER)
            return sval->u.vec == ((ldmud_array_t*)pval)->lpc_array;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_mapping_type.type_base))
    {
        if (sval->type == T_MAPPING)
            return sval->u.map == ((ldmud_mapping_t*)pval)->lpc_mapping;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_struct_type.type_base))
    {
        if (sval->type == T_STRUCT)
            return sval->u.strct == ((ldmud_struct_t*)pval)->lpc_struct;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_closure_type.type_base))
    {
        if (sval->type == T_CLOSURE)
            return closure_cmp(sval, &((ldmud_closure_t*)pval)->lpc_closure) == 0;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_coroutine_type.type_base))
    {
        if (sval->type == T_COROUTINE)
            return sval->u.coroutine == ((ldmud_coroutine_t*)pval)->lpc_coroutine;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_symbol_type.type_base))
    {
        if (sval->type == T_SYMBOL)
        {
            svalue_t *psval = &((ldmud_symbol_t*)pval)->lpc_symbol;
            return psval->u.str == sval->u.str &&
                   psval->x.quotes == sval->x.quotes;
        }
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_quoted_array_type.type_base))
    {
        if (sval->type == T_QUOTED_ARRAY)
        {
            svalue_t *psval = &((ldmud_quoted_array_t*)pval)->lpc_quoted_array;
            return psval->u.vec == sval->u.vec &&
                   psval->x.quotes == sval->x.quotes;
        }
        else
            return false;
    }

    /* And now the python ones. */
    if (PyLong_Check(pval))
    {
        if (sval->type == T_NUMBER)
        {
            int overflow;
            long num = PyLong_AsLongAndOverflow(pval, &overflow);

            if (overflow || num < PINT_MIN || num > PINT_MAX)
                return false;
            return sval->u.number == num;
        }
        else if (sval->type == T_FLOAT)
        {
            return READ_DOUBLE(sval) == PyLong_AsDouble(pval);
        }
        else
            return false;
    }

    if (PyBool_Check(pval))
    {
        if (sval->type == T_NUMBER)
            return sval->u.number == (pval == Py_True ? 1 : 0);
        else
            return false;
    }

    if (PyFloat_Check(pval))
    {
        double num = PyFloat_AsDouble(pval);

        if (sval->type == T_NUMBER)
            return sval->u.number == num;
        else if (sval->type == T_FLOAT)
            return READ_DOUBLE(sval) == num;
        else
            return false;
    }

    if (PyBytes_Check(pval))
    {
        if (sval->type == T_BYTES)
        {
            Py_ssize_t length;
            char * buf;

            PyBytes_AsStringAndSize(pval, &buf, &length);
            if (mstrsize(sval->u.str) != length)
                return false;
            return memcmp(get_txt(sval->u.str), buf, length) == 0;
        }
        else
            return false;
    }

    if (PyUnicode_Check(pval))
    {
        if (sval->type == T_STRING)
        {
            PyObject *utf8;
            Py_ssize_t length;
            char * buf;
            bool result;

            utf8 = PyUnicode_AsEncodedString(pval, "utf-8", "replace");
            if (utf8 == NULL)
                return false;

            PyBytes_AsStringAndSize(utf8, &buf, &length);

            result = (mstrsize(sval->u.str) == length && memcmp(get_txt(sval->u.str), buf, length) == 0);
            Py_DECREF(utf8);
            return result;
        }
        else
            return false;
    }

    return false;

} /* python_eq_svalue() */

/*-------------------------------------------------------------------------*/
static bool
call_lpc_secure (CClosureFun fun, int num_arg, void* data)

/* Call <fun>(<data>), but guard it against LPC errors.
 * There are <num_arg> arguments on the stack, that will be
 * removed automatically upon any errors.
 * Returns false if there happened an LPC error, true otherwise.
 * The error string will be set as a python exception.
 */

{
    struct error_recovery_info error_recovery_info;
    struct control_stack *save_csp;
    svalue_t *save_sp;
    bool result = false;

    error_recovery_info.rt.last = rt_context;
    error_recovery_info.rt.type = ERROR_RECOVERY_APPLY;
    rt_context = (rt_context_t *)&error_recovery_info;

    save_sp = inter_sp - num_arg;
    save_csp = csp;

    if (setjmp(error_recovery_info.con.text))
    {
        /* Although we propagate the error as a Python exception to the
         * caller (which will result in another LPC error if the caller
         * is LPC code), we also call secure_apply_error() here,
         * so the error is logged with the full call trace.
         *
         * secure_apply_error will free current_error, so saving
         * a reference here.
         */
        string_t *error = ref_mstring(current_error);
        secure_apply_error(save_sp, save_csp, python_is_external);

        PyErr_SetString(PyExc_RuntimeError, get_txt(current_error));
        free_mstring(error);
    }
    else
    {
        svalue_t save_ob = current_object;

        if(python_is_external)
        {
            /* We do externally called python code
             * in the context of the master ob.
             */
            python_restore_context();
            if (current_object.type == T_NUMBER)
                set_current_object(master_ob);
            mark_start_evaluation();
        }

        (*fun)(num_arg, data);
        result = true;

        if(python_is_external)
        {
            mark_end_evaluation();
            python_save_context();
            current_object = save_ob;
        }
    }

    rt_context = error_recovery_info.rt.last;

    return result;
} /* call_lpc_secure() */

/*-------------------------------------------------------------------------*/
static void
raise_python_error (const char* prefix, bool thread_started)

/* Raise an LPC error upon a Python exception.
 * <thread_started> is true, if we need to finish a Python thread.
 * This function will not return.
 */

{
    /* Exception occurred. */
    char buf[1024];
    char *msg;

    PyObject *exc_type, *exc_value, *exc_tb, *exc_str;
    PyErr_Fetch(&exc_type, &exc_value, &exc_tb);

    /* Make that exception into a string. */
    exc_str = PyObject_Str(exc_value);
    if (exc_str == NULL)
        msg = "unknown exception";
    else
    {
        /* And convert it to UTF-8 (for now). */
        PyObject *exc_utf8;

        exc_utf8 = PyUnicode_AsEncodedString(exc_str, "utf-8", "replace");
        if (exc_utf8 == NULL)
            msg = "undecodable exception";
        else
        {
            /* Copy the message, because it may not
             * exist after Py_DECREF.
             */
            strncpy(buf, PyBytes_AS_STRING(exc_utf8), sizeof(buf) - 1);
            buf[sizeof(buf)-1] = 0;

            msg = buf;

            Py_DECREF(exc_utf8);
        }

        Py_DECREF(exc_str);
    }

    /* And print it to stdout. */
    PyErr_Restore(exc_type, exc_value, exc_tb);
    PyErr_Print();
    PyErr_Clear();

    python_finish_thread(thread_started);
    errorf("%s: %s\n", prefix, msg);
} /* raise_python_error() */

/*-------------------------------------------------------------------------*/
static bool
python_start_thread ()

/* Do anything necessary to execute Python code or access Python objects.
 * This basically means acquiring the global interpreter lock. Returns
 * true, if this function indeed started the thread (false if already
 * in Python mode).
 */

{
    if (!thread_state)
        return false;

    PyEval_RestoreThread(thread_state);
    thread_state = NULL;
    return true;
} /* python_start_thread() */

/*-------------------------------------------------------------------------*/
static void
python_finish_thread (bool started)

/* Revert python_start_thread(), basically releasing the global interpreter
 * lock. <started> is the return value of python_start_thread().
 */

{
    if (!started)
        return;

    thread_state = PyEval_SaveThread();
} /* python_finish_thread() */

/*-------------------------------------------------------------------------*/
#ifdef USE_PYTHON_CONTEXT
static void
python_save_contextvar_object (PyObject** contextvar, const char* name, object_t* object)

/* Save the given object into a the corresponding Python context variable.
 */

{
    PyObject *tok, *ob;

    if (!*contextvar)
    {
        if (!object)
            return;

        *contextvar = PyContextVar_New(name, NULL);
    }

    if (object)
        ob = ldmud_object_create(object);
    else
    {
        Py_INCREF(Py_None);
        ob = Py_None;
    }
    if (!ob)
        PyErr_Clear();
    else
    {
        tok = PyContextVar_Set(*contextvar, ob);
        if (!tok)
            PyErr_Clear();
        else
            Py_DECREF(tok);
        Py_DECREF(ob);
    }
} /* python_save_contextvar_object() */

/*-------------------------------------------------------------------------*/
static void
python_save_contextvar_value (PyObject** contextvar, const char* name, svalue_t object)

/* Save the given LPC value into a the corresponding Python context variable.
 */

{
    PyObject *tok, *ob;

    if (!*contextvar)
    {
        if (object.type == T_NUMBER)
            return;

        *contextvar = PyContextVar_New(name, NULL);
    }

    ob = svalue_to_python(&object);
    if (!ob)
        PyErr_Clear();
    else
    {
        tok = PyContextVar_Set(*contextvar, ob);
        if (!tok)
            PyErr_Clear();
        else
            Py_DECREF(tok);
        Py_DECREF(ob);
    }
} /* python_save_contextvar_value() */
#endif

/*-------------------------------------------------------------------------*/
static void
python_save_context ()

/* Save the current context (current object, current command giver) into
 * the corresponding Python context variables.
 */

{
#ifdef USE_PYTHON_CONTEXT
    python_save_contextvar_value(&python_contextvar_current_object, "ldmud.current_object", current_object);
    python_save_contextvar_object(&python_contextvar_command_giver, "ldmud.command_giver", command_giver);
#endif
} /* python_save_context() */

/*-------------------------------------------------------------------------*/
static void
python_clear_context ()

/* Clear the current context in the Python context variables.
 */

{
#ifdef USE_PYTHON_CONTEXT
    python_save_contextvar_object(&python_contextvar_current_object, "ldmud.current_object", NULL);
    python_save_contextvar_object(&python_contextvar_command_giver, "ldmud.command_giver", NULL);
#endif
} /* python_clear_context() */

/*-------------------------------------------------------------------------*/
#ifdef USE_PYTHON_CONTEXT
static void
python_restore_contextvar_object (PyObject* contextvar, object_t** object)

/* Restore an object from the corresponding Python context variable.
 */

{
    PyObject *val;

    if (!contextvar)
        return;

    if (PyContextVar_Get(contextvar, NULL, &val) < 0)
    {
        PyErr_Clear();
        *object = NULL;
    }
    else if (val == NULL)
        *object = NULL;
    else if (val == Py_None || !ldmud_object_check(val))
    {
        *object = NULL;
        Py_DECREF(val);
    }
    else
    {
        *object = ((ldmud_object_t*)val)->lpc_object;
        Py_DECREF(val);
    }
} /* python_restore_contextvar_object() */

/*-------------------------------------------------------------------------*/
static void
python_restore_contextvar_value (PyObject* contextvar, svalue_t* dest)

/* Restore an LPC value from the corresponding Python context variable.
 */

{
    PyObject *val;

    if (!contextvar)
        return;

    if (PyContextVar_Get(contextvar, NULL, &val) < 0)
    {
        PyErr_Clear();
        *dest = const0;
    }
    else if (val == NULL)
        *dest = const0;
    else
    {
        if (python_to_svalue(dest, val) != NULL)
            *dest = const0;
        Py_DECREF(val);
    }
} /* python_restore_contextvar_object() */
#endif

/*-------------------------------------------------------------------------*/
static void
python_restore_context ()

/* Restore the current context (current object, current command giver) from
 * the corresponding Python context variables. Should only be called for
 * external calls.
 */

{
#ifdef USE_PYTHON_CONTEXT
    python_restore_contextvar_value(python_contextvar_current_object, &current_object);
    python_restore_contextvar_object(python_contextvar_command_giver, &command_giver);
#else
    clear_current_object();
    command_giver = NULL;
#endif
} /* python_restore_context() */

/*=========================================================================*/

/*                          Global functions                               */

/*-------------------------------------------------------------------------*/


void
pkg_python_init (char* prog_name)

/* Called at LDMud startup. <prog_name> is the path name of the
 * LDMud executable. This function will called any configured
 * python script for initialization.
 */

{
    PyObject *name, *importer = NULL;

    /** Python3 requires now wchar_t?!
     * Py_SetProgramName(prog_name);
     */

    PyImport_AppendInittab("ldmud", &init_ldmud_module);
    Py_Initialize();

    /* Check, whether the startup script is a module
     * (eg. directory or archive).
     */
    name = PyUnicode_DecodeFSDefault(python_startup_script);
    if (name != NULL)
    {
        importer = PyImport_GetImporter(name);
        if (importer == Py_None)
        {
            Py_DECREF(importer);
            importer = NULL;
        }
        else if (importer == NULL)
            PyErr_Clear();
    }
    else
        PyErr_Clear();

    if (importer != NULL)
    {
        /* The file can be loaded as a module.
         * So first put it into sys.path at the beginning.
         */
        PyObject *path = PySys_GetObject("path");
        if (path != NULL)
        {
            if (PyList_Insert(path, 0, name))
            {
                PyErr_Print();
                PyErr_Clear();
            }
        }

        /* Remove the default __main__ module. */
        PyObject *main_name = PyUnicode_FromString("__main__");
        if (main_name == NULL)
        {
            PyErr_Print();
            PyErr_Clear();
            Py_DECREF(importer);
            return;
        }
        PyObject *modules = PySys_GetObject("modules");
        if (modules != NULL)
        {
            if (PyMapping_DelItem(modules, main_name) < 0)
            {
                PyErr_Print();
                PyErr_Clear();
            }
        }

        /* And load __main__ hopefully from the first module in sys.path.
         */
        PyObject *main_mod = PyImport_Import(main_name);
        if (main_mod != NULL)
            Py_DECREF(main_mod);
        else
        {
            PyErr_Print();
            PyErr_Clear();
        }
        Py_DECREF(main_name);
        Py_DECREF(importer);
    }
    else
    {
        /* Not a module, then just execute the file. */
        FILE *script_file = fopen(python_startup_script, "rt");
        if(script_file != NULL)
        {
            PyCompilerFlags flags;
            flags.cf_flags = 0;
            PyRun_SimpleFileExFlags(script_file, python_startup_script, 1, &flags);
        }

        Py_XDECREF(name);
    }

    assert(sizeof(python_operation_fun) / sizeof(python_operation_fun[0]) == PYTHON_OPERATIONS_COUNT);

    python_finish_thread(true);
} /* pkg_python_init() */


/*-------------------------------------------------------------------------*/


bool
is_python_efun (ident_t *p)

/* Returns true, if <p> is still a registered python efun.
 * When a python efun is unregistered, we'll leave the python_efun
 * index in the identifier intact, so re-registering the same efun
 * can reuse the same index.
 */

{
    return p->u.global.python_efun != I_GLOBAL_PYTHON_EFUN_OTHER && python_efun_table[p->u.global.python_efun].callable != NULL;
} /* is_python_efun() */


/*-------------------------------------------------------------------------*/

lpctype_t *
check_python_efun_args (ident_t *p, int num_arg, bool has_ellipsis, fulltype_t *args)

/* Check whether the given argument types match the ones
 * the Python efun expects. Any errors will be printed using yyerrorf().
 * This function returns the return type of the efun.
 */

{
    python_efun_t * entry = python_efun_table + p->u.global.python_efun;

    if (num_arg < entry->info.minarg && !has_ellipsis)
        yyerrorf("Too few arguments to %s", get_txt(p->name));
    else if(!entry->info.varargs && num_arg > entry->info.maxarg)
    {
        yyerrorf("Too many arguments to %s", get_txt(p->name));
        num_arg = entry->info.maxarg;
    }

    if (!entry->info.types)
        return lpctype_mixed;

    for (int pos = 0; pos < num_arg; pos++)
    {
        lpctype_t* expected;

        if (pos >= entry->info.maxarg)
            expected = entry->info.types[1 + entry->info.maxarg];
        else
            expected = entry->info.types[1 + pos];

        if (!expected)
            continue;

        if (!has_common_type(expected, args[pos].t_type))
        {
            yyerrorf("Bad arg %d type to %s(): got %s, expected %s"
                    , 1 + pos, get_txt(p->name)
                    , get_fulltype_name(args[pos])
                    , get_lpctype_name(expected));
        }
    }

    if (entry->info.types[0])
        return entry->info.types[0];
    return lpctype_mixed;

} /* check_python_efun_args() */

/*-------------------------------------------------------------------------*/
static PyObject*
build_python_efun_args (python_efun_info_t *efun_info, svalue_t *argp, int num_arg, const char* name, bool skip_first, bool thread_started)

/* Check the <num_arg> arguments starting at <argp> against the type
 * annotations in <efun_info> and raise any type error.
 * On success build an argument tuple and return it.
 * If <skip_first> is true, don't check and add the first argument.
 */

{
    PyObject *args;

    if (!num_arg || (skip_first && num_arg == 1))
        return NULL;

    args = PyTuple_New(num_arg - (skip_first ? 1 : 0));

    for (int pos = 0; pos < num_arg; pos++,argp++)
    {
        PyObject *arg;

        if (skip_first && !pos)
            continue;

        if (efun_info->types)
        {
            lpctype_t *expected;
            if (pos < efun_info->maxarg)
                expected = efun_info->types[1 + pos];
            else if (efun_info->varargs)
                expected = efun_info->types[1 + efun_info->maxarg];
            else
                expected = NULL;

            if (expected && !check_rtt_compatibility(expected, argp))
            {
                static char buff[512];
                lpctype_t *realtype = get_rtt_type(expected, argp);
                get_lpctype_name_buf(realtype, buff, sizeof(buff));
                free_lpctype(realtype);

                Py_DECREF(args);
                python_finish_thread(thread_started);

                errorf("Bad arg %d to %s(): got '%s', expected '%s'.\n"
                      , pos + 1, name
                      , buff, get_lpctype_name(expected));
            }
        }

        arg = svalue_to_python(argp);
        if (arg == NULL)
        {
            PyErr_Clear();
            Py_DECREF(args);
            python_finish_thread(thread_started);

            errorf("Bad argument %d to %s().\n", pos+1, name);
        }
        PyTuple_SET_ITEM(args, pos - (skip_first ? 1 : 0), arg);
    }

    return args;
} /* check_python_efun_args() */

/*-------------------------------------------------------------------------*/
void
call_python_efun (int idx, int num_arg)

/* Call the python-defined efun <idx> with <num_arg> arguments on the stack.
 * This function removes the arguments from the stack and leaves the result
 * there.
 */

{
    PyObject *result, *args;
    bool was_external = python_is_external;
    bool started;
    python_efun_t* python_efun_entry = python_efun_table + idx;

    /* Efun still registered?
     * (F_PYTHON_EFUN opcodes may still be floating around.)
     */
    if (python_efun_entry->callable == NULL)
        errorf("Python-defined efun vanished: %s\n"
             , get_txt(python_efun_entry->name->name));

    started = python_start_thread();

    /* We leave the argument count check to Python.
     * But we check the types if there are annotations about it.
     */
    args = build_python_efun_args(&(python_efun_entry->info), inter_sp - num_arg + 1, num_arg, get_txt(python_efun_entry->name->name), false, started);
    inter_sp = pop_n_elems(num_arg, inter_sp);

    python_is_external = false;
    python_save_context();
    result = PyObject_CallObject(python_efun_entry->callable, args);
    python_clear_context();
    python_is_external = was_external;
    Py_XDECREF(args);

    if (result == NULL)
    {
        /* Exception occurred. */
        raise_python_error(get_txt(python_efun_entry->name->name), started);
    }
    else
    {
        const char *err = python_to_svalue(inter_sp + 1, result);
        Py_DECREF(result);
        python_finish_thread(started);

        if (err != NULL)
        {
            errorf("Bad return value from %s(): %s\n"
                  , get_txt(python_efun_entry->name->name)
                  , err);
        }

        inter_sp++;
    }
} /* call_python_efun() */


/*-------------------------------------------------------------------------*/
struct_type_t*
get_python_struct_type (int idx)

/* Returns the Python-defined struct with this index or NULL if it doesn't
 * exist.
 */

{
    return (idx < PYTHON_STRUCT_TABLE_SIZE) ? python_struct_table[idx] : NULL;
} /* get_python_struct_type() */

/*-------------------------------------------------------------------------*/
void
python_set_fds (fd_set *readfds, fd_set *writefds, fd_set *exceptfds, int *nfds)

/* Set all file descriptors that some python routines are waiting for.
 */

{
    bool started = python_start_thread();

    python_is_external = true;
    interrupt_execution = false;
    python_clear_context();

    for (python_poll_fds_t *fds = poll_fds; fds != NULL; fds = fds->next)
    {
        short events = fds->events;

        if (fds->eventsfun)
        {
            PyObject *result;

            result = PyObject_CallObject(fds->eventsfun, NULL);
            if (result == NULL)
            {
                /* Exception occurred. */
                if (PyErr_Occurred())
                    PyErr_Print();
                continue;
            }

            events = PyLong_AsLong(result);
            if (events == -1 && PyErr_Occurred())
            {
                PyErr_Print();
                Py_DECREF(result);
                continue;
            }
            Py_DECREF(result);
        }

        if(events & POLLIN)
            FD_SET(fds->fd, readfds);

        if(events & POLLOUT)
            FD_SET(fds->fd, writefds);

        if(events & POLLPRI)
            FD_SET(fds->fd, exceptfds);

        if((events & (POLLIN|POLLOUT|POLLPRI)) && *nfds <= fds->fd)
            (*nfds) = fds->fd + 1;
    }
    python_finish_thread(started);
} /* python_set_fds() */

/*-------------------------------------------------------------------------*/
void
python_handle_fds (fd_set *readfds, fd_set *writefds, fd_set *exceptfds, int nfds)

/* File descriptors in the given sets have events.
 * Check whether a callable is waiting for it, then call it.
 */

{
    int num_cbs = 0, pos_cbs = 0;
    bool started;
    PyObject **cb_funs, **cb_args;

    /* First count the number of calls we need to make. */
    for (python_poll_fds_t *fds = poll_fds; fds != NULL; fds = fds->next)
        if (FD_ISSET(fds->fd, readfds) || FD_ISSET(fds->fd, writefds) || FD_ISSET(fds->fd, exceptfds))
            num_cbs++;

    /* Nothign to do? */
    if (!num_cbs)
        return;

    /* Then we collect the callbacks. We can't do them while we are
     * iterating through the structure, because they may alter the
     * structure in the meanwhile.
     */
    cb_funs = xalloc(sizeof(PyObject*)*num_cbs);
    cb_args = xalloc(sizeof(PyObject*)*num_cbs);

    /* If we don't have any memory, we skip it now.
     * They will be checked the next time around, too.
     */
    if (!cb_funs || !cb_args)
        return;

    started = python_start_thread();

    for (python_poll_fds_t *fds = poll_fds; fds != NULL; fds = fds->next)
    {
        int events = 0;

        if (FD_ISSET(fds->fd, readfds))
            events |= POLLIN;

        if (FD_ISSET(fds->fd, writefds))
            events |= POLLOUT;

        if (FD_ISSET(fds->fd, exceptfds))
            events |= POLLPRI;

        if (events != 0)
        {
            PyObject *args, *arg;

            args = PyTuple_New(1);
            if (args == NULL)
            {
                PyErr_Clear();
                continue; /* Out of memory, what shall we do... */
            }

            arg = PyLong_FromLong(events);
            if (arg == NULL)
            {
                PyErr_Clear();
                Py_DECREF(args);
                continue;
            }

            PyTuple_SET_ITEM(args, 0, arg);

            Py_INCREF(fds->fun);
            cb_funs[pos_cbs] = fds->fun;
            cb_args[pos_cbs] = args;
            pos_cbs++;
        }
    }

    assert(pos_cbs == num_cbs);

    /* And now call them. */
    python_is_external = true;
    interrupt_execution = false;
    python_clear_context();

    for (int i = 0; i < pos_cbs; i++)
    {
        PyObject *result = PyObject_CallObject(cb_funs[i], cb_args[i]);
        Py_DECREF(cb_funs[i]);
        Py_DECREF(cb_args[i]);

        if (result == NULL)
        {
            /* Exception occurred. */
            if (PyErr_Occurred())
                PyErr_Print();
        }
        else
            Py_DECREF(result);
    }

    xfree(cb_funs);
    xfree(cb_args);
    python_finish_thread(started);
} /* python_handle_fds() */

/*-------------------------------------------------------------------------*/
void
python_call_hook (int hook, bool is_external)

/* Call the python <hook> without any arguments.
 */

{
    bool started = python_start_thread();
    bool was_external = python_is_external;
    python_is_external = is_external;
    if (is_external)
    {
        python_clear_context();
        interrupt_execution = false;
    }
    else
        python_save_context();

    for(python_hook_t *entry = python_hooks[hook]; entry; entry = entry->next)
    {
        PyObject *result;

        result = PyObject_CallObject(entry->fun, NULL);
        if (result == NULL)
        {
            /* Exception occurred. */
            if (PyErr_Occurred())
                PyErr_Print();
            continue;
        }
        Py_DECREF(result);
    }

    python_clear_context();
    python_is_external = was_external;
    python_finish_thread(started);
} /* python_call_hook() */

/*-------------------------------------------------------------------------*/
void
python_call_hook_object (int hook, bool is_external, object_t *ob)

/* Call the python <hook> with an object as its only argument.
 */

{
    bool started;
    bool was_external = python_is_external;
    PyObject *args, *arg;
    if (python_hooks[hook] == NULL)
        return;

    started = python_start_thread();
    if (is_external)
    {
        python_clear_context();
        interrupt_execution = false;
    }
    else
        python_save_context();

    args = PyTuple_New(1);
    if (args == NULL)
    {
        PyErr_Clear();
        python_finish_thread(started);
        return;
    }

    arg = ldmud_object_create(ob);
    if (arg == NULL)
    {
        PyErr_Clear();
        Py_DECREF(args);
        python_finish_thread(started);
        return;
    }

    PyTuple_SET_ITEM(args, 0, arg);

    python_is_external = is_external;

    for(python_hook_t *entry = python_hooks[hook]; entry; entry = entry->next)
    {
        PyObject *result;

        result = PyObject_CallObject(entry->fun, args);
        if (result == NULL)
        {
            /* Exception occurred. */
            if (PyErr_Occurred())
                PyErr_Print();
            continue;
        }
        Py_DECREF(result);
    }

    Py_DECREF(args);

    python_clear_context();
    python_is_external = was_external;
    python_finish_thread(started);
} /* python_call_hook_object() */

/*-------------------------------------------------------------------------*/

void
python_call_instruction_hook (int instruction)

/* Call the BEFORE_INSTRUCTION hook. We'll pass the LPC (lw)object and a Python
 * object that contains information about the instruction.
 */

{
    bool started;
    bool was_external = python_is_external;
    PyObject *args, *arg;
    python_hook_t *hooks;
    int stack_depth = control_stack_depth();

    if (stack_depth >= 0)
        frame_current_index[stack_depth] = 0;

    hooks = python_hooks[PYTHON_HOOK_BEFORE_INSTRUCTION];
    if (hooks == NULL)
        return;

    started = python_start_thread();
    python_save_context();

    args = PyTuple_New(2);
    if (args == NULL)
    {
        PyErr_Clear();
        python_finish_thread(started);
        return;
    }

    arg = svalue_to_python(&current_object);
    if (arg == NULL)
    {
        PyErr_Clear();
        Py_DECREF(args);
        python_finish_thread(started);
        return;
    }
    PyTuple_SET_ITEM(args, 0, arg);

    arg = ldmud_instruction_create(instruction);
    if (arg == NULL)
    {
        PyErr_Clear();
        Py_DECREF(args);
        python_finish_thread(started);
        return;
    }
    PyTuple_SET_ITEM(args, 1, arg);

    python_is_external = false;

    for(python_hook_t *entry = hooks; entry; entry = entry->next)
    {
        PyObject *result;

        result = PyObject_CallObject(entry->fun, args);
        if (result == NULL)
        {
            /* Exception occurred. */
            if (PyErr_Occurred())
                PyErr_Print();
            continue;
        }
        Py_DECREF(result);
    }

    Py_DECREF(args);

    python_clear_context();
    python_is_external = was_external;
    python_finish_thread(started);
} /* python_call_instruction_hook() */

/*-------------------------------------------------------------------------*/
int
python_process_interrupt (void* arg UNUSED)

/* Called from within the Python interpreter
 * to cause an interrupt in the current running python efun.
 */

{
    /* Was the interrupt request handled in the meanwhile? */
    if (!interrupt_execution)
        return 0;

    interrupt_execution = false;

    PyErr_SetString((PyObject *) &ldmud_interrupt_exception_type, "signal caught");
    return -1;
} /* python_process_interrupt() */

/*-------------------------------------------------------------------------*/
void
python_interrupt ()

/* Called from the signal handler (!)
 * to cause an interrupt in the current running python efun.
 */

{
    Py_AddPendingCall(&python_process_interrupt, NULL);
} /* python_interrupt() */

/*-------------------------------------------------------------------------*/
void
python_handle_sigchld ()

/* Called from the SIGCHLD signal handler.
 * We will remember that and handle it in the backend loop.
 */

{
    comm_return_to_backend = true;
    python_pending_sigchld = true;
} /* python_handle_sigchld() */

/*-------------------------------------------------------------------------*/
void
python_handle_signal (int sig)

/* Called from the general signal handler for <sig>.
 * We will remember that and handle it in the backend loop.
 */

{
    comm_return_to_backend = true;
    switch (sig)
    {
        case SIGINT:
            python_pending_sigint = true;
            break;

        case SIGTERM:
            python_pending_sigterm = true;
            break;

        case SIGHUP:
            python_pending_sighup = true;
            break;

        case SIGUSR1:
            python_pending_sigusr1 = true;
            break;

        case SIGUSR2:
            python_pending_sigusr2 = true;
            break;
    }
} /* python_handle_signal() */

/*-------------------------------------------------------------------------*/
void
python_process_pending_signal (volatile bool *pending_flag, int hook)

/* Process some pending signal if <*pending_flag> is true
 * by calling <hook>.
 */

{
    if (*pending_flag)
    {
        bool started = python_start_thread();
        *pending_flag = false;
        python_call_hook(hook, true);
        python_finish_thread(started);
    }
} /* python_process_pending_signal() */

/*-------------------------------------------------------------------------*/
void
python_process_pending_jobs ()

/* Called from the backend to do some pending jobs,
 * to call some hooks.
 */

{
#if PY_VERSION_HEX >= 0x03040000
    if (PyGILState_Check())
    {
        debug_message("%s Warning: Python GIL was held during backend loop.\n", time_stamp());
        python_finish_thread(true);
    }
#endif

    python_process_pending_signal(&python_pending_sigchld, PYTHON_HOOK_ON_SIGCHLD);
    python_process_pending_signal(&python_pending_sigint,  PYTHON_HOOK_ON_SIGINT);
    python_process_pending_signal(&python_pending_sigterm, PYTHON_HOOK_ON_SIGTERM);
    python_process_pending_signal(&python_pending_sighup,  PYTHON_HOOK_ON_SIGHUP);
    python_process_pending_signal(&python_pending_sigusr1, PYTHON_HOOK_ON_SIGUSR1);
    python_process_pending_signal(&python_pending_sigusr2, PYTHON_HOOK_ON_SIGUSR2);
} /* python_process_pending_jobs() */

/*-------------------------------------------------------------------------*/
const char*
closure_python_efun_to_string (int type)

/* <type> is the code for a closure python-efun (the caller has to make
 * sure of that). Returns the name of that python-defined efun.
 */

{
    return get_txt(python_efun_table[type - CLOSURE_PYTHON_EFUN].name->name);
} /* closure_python_efun_to_string() */

/*-------------------------------------------------------------------------*/
ident_t*
get_python_type_name (int python_type_id)

/* Return the identifier that belongs to a registered python type.
 */

{
    assert(python_type_table[python_type_id] != NULL);
    return python_type_table[python_type_id]->name;
} /* get_python_type_name() */

/*-------------------------------------------------------------------------*/
lpctype_t*
lookup_python_type (int python_type_id)

/* Called by the type functions to check whether our table has
 * a LPC type pointer stored. This is done, so the lpctype_t object
 * stays unique for each Python type.
 */

{
    assert(python_type_table[python_type_id] != NULL);
    return python_type_table[python_type_id]->lpctype;
} /* lookup_python_type() */

/*-------------------------------------------------------------------------*/
void
enter_python_type (int python_type_id, lpctype_t* lpctype)

/* An LPC type object was created for <python_type_id>, enter it into
 * our type table so we can reuse this type object later. The reference
 * to the type is adopted.
 */

{
    assert(python_type_table[python_type_id] != NULL);
    assert(python_type_table[python_type_id]->lpctype == NULL);
    python_type_table[python_type_id]->lpctype = lpctype;
} /* enter_python_type() */

/*-------------------------------------------------------------------------*/
python_type_operation_t
get_python_operation (int python_type_id, enum python_operation op)

/* If the Python type <python_type_id> supports the Python operation <op>,
 * return a structure with the return and operand type (borrowed references).
 * Return a structure with NULL entries for unsupported operations.
 */

{
    assert(python_type_table[python_type_id] != NULL);
    return python_type_table[python_type_id]->op[op];
} /* get_python_operation() */

/*-------------------------------------------------------------------------*/
lpctype_t*
get_first_python_type(lpctype_t* type, void** cursor)

/* Return the first Python type within <type> or NULL if there is none.
 * <cursor> is a pointer for passing data to subsequent calls to
 * get_next_python_type().
 */

{
    *cursor = type;

    return get_next_python_type(cursor);
} /* get_first_python_type() */

/*-------------------------------------------------------------------------*/
lpctype_t*
get_next_python_type(void** cursor)

/* Return the next Python type after a call to get_first_python_type().
 *
 * <cursor> is a pointer to an lpctype_t*, where we need to continue
 * looking.
 */

{
    lpctype_t *curtype = *(lpctype_t**)cursor;
    while (curtype != NULL)
    {
        lpctype_t *curbase;
        if (curtype->t_class == TCLASS_UNION)
        {
            curbase = curtype->t_union.member;
            curtype = curtype->t_union.head;
        }
        else
        {
            curbase = curtype;
            curtype = NULL;
        }

        if (curbase->t_class == TCLASS_PYTHON)
        {
            *cursor = curtype;
            return curbase;
        }
    }

    *cursor = NULL;
    return NULL;
}

/*-------------------------------------------------------------------------*/
static python_efun_info_t*
get_python_type_efun_info (int python_type_id, int efun)

/* Returns the efun override information for the Python type <pytype>.
 */

{
    int efun_idx;
    python_efun_info_t *result;

    assert(python_type_table[python_type_id] != NULL);

    if (EFUN_OFFSET <= efun && efun < EFUN_OFFSET + EFUN_COUNT)
        efun_idx = efun - EFUN_OFFSET;
    else if (EFUN1_OFFSET <= efun && efun < EFUN1_OFFSET + EFUN1_COUNT)
        efun_idx = EFUN_COUNT + efun - EFUN1_OFFSET;
    else if (EFUN2_OFFSET <= efun && efun < EFUN2_OFFSET + EFUN2_COUNT)
        efun_idx = EFUN_COUNT + EFUN1_COUNT + efun - EFUN2_OFFSET;
    else if (EFUN3_OFFSET <= efun && efun < EFUN3_OFFSET + EFUN3_COUNT)
        efun_idx = EFUN_COUNT + EFUN1_COUNT + EFUN2_COUNT + efun - EFUN3_OFFSET;
    else if (EFUN4_OFFSET <= efun && efun < EFUN4_OFFSET + EFUN4_COUNT)
        efun_idx = EFUN_COUNT + EFUN1_COUNT + EFUN2_COUNT + EFUN3_COUNT + efun - EFUN4_OFFSET;
    else if (EFUNV_OFFSET <= efun && efun < EFUNV_OFFSET + EFUNV_COUNT)
        efun_idx = EFUN_COUNT + EFUN1_COUNT + EFUN2_COUNT + EFUN3_COUNT + EFUN4_COUNT + efun - EFUNV_OFFSET;
    else
        return NULL;

    result = python_type_table[python_type_id]->efun + efun_idx;
    if (!result->exists)
        return NULL;

    return result;
} /* get_python_type_efun_info() */

/*-------------------------------------------------------------------------*/
bool
is_valid_arg_for_python_type_efun (lpctype_t *type, int efun, int pos, lpctype_t *argtype)

/* Check the type <argtype> for the argument <pos> when calling the efun
 * override for one of the Python types within <type>.
 */

{
    void* cur;

    for (lpctype_t *pytype = get_first_python_type(type, &cur);
         pytype != NULL;
         pytype = get_next_python_type(&cur))
    {
        assert(pytype->t_class == TCLASS_PYTHON);

        python_efun_info_t* efun_info = get_python_type_efun_info(pytype->t_python.type_id, efun);
        lpctype_t* expected;

        /* No efun override. */
        if (efun_info == NULL)
            continue;

        /* No type information, everything is allowed. */
        if (efun_info->types == NULL)
            return true;

        /* First argument is <type> itself, so it's okay. */
        if (pos == 1)
            return true;

        if (pos > efun_info->maxarg)
            expected = efun_info->types[1 + efun_info->maxarg];
        else
            expected = efun_info->types[pos];

        if (!expected)
            return true;

        if (has_common_type(expected, argtype))
            return true;
    }

    return false;

} /* is_valid_arg_for_python_type_efun() */

/*-------------------------------------------------------------------------*/
lpctype_t*
add_result_for_python_type_efun (lpctype_t *type, int efun, lpctype_t *result)

/* Add any result types for efun overrides of Python types within <type>
 * to <result> and return it. The reference to <result> is adopted.
 */

{
    void* cur;

    if (result == lpctype_mixed)
        return result;

    for (lpctype_t *pytype = get_first_python_type(type, &cur);
         pytype != NULL;
         pytype = get_next_python_type(&cur))
    {
        assert(pytype->t_class == TCLASS_PYTHON);

        python_efun_info_t* efun_info = get_python_type_efun_info(pytype->t_python.type_id, efun);
        lpctype_t *oldresult = result;

        if (efun_info == NULL)
            continue;

        if (efun_info->types == NULL || efun_info->types[0] == NULL)
        {
            free_lpctype(result);
            return lpctype_mixed;
        }

        result = get_union_type(result, efun_info->types[0]);
        free_lpctype(oldresult);
    }

    return result;
} /* add_result_for_python_type_efun() */

/*-------------------------------------------------------------------------*/
bool
python_ob_has_last_ref (svalue_t *pval)

/* Return true, if this is the last reference to the Python object.
 */

{
    bool started = python_start_thread();
    bool result = Py_REFCNT((PyObject*)pval->u.generic) == 1;
    python_finish_thread(started);
    return result;
} /* python_ob_has_last_ref() */

/*-------------------------------------------------------------------------*/
void
ref_python_ob (svalue_t *pval)

/* Increment the reference count to the given Python object.
 */

{
    bool started = python_start_thread();
    Py_INCREF((PyObject*)pval->u.generic);
    num_lpc_python_references++;
    python_finish_thread(started);
} /* ref_python_ob() */

/*-------------------------------------------------------------------------*/
void
free_python_ob (svalue_t *pval)

/* Free a reference to the given Python object.
 */

{
    bool started = python_start_thread();
    Py_DECREF((PyObject*)pval->u.generic);
    num_lpc_python_references--;
    python_finish_thread(started);
} /* free_python_ob() */

/*-------------------------------------------------------------------------*/
void
copy_python_ob (svalue_t *dest, svalue_t *src)

/* Put a copy of the python object <src> into dest <dest>.
 * If the object is not copyable, a reference of <src> is put into <dest>.
 */

{
    PyObject *fun;
    bool started = python_start_thread();

    assert(src->type == T_PYTHON);
    assert(python_type_table[src->x.python_type] != NULL);

    fun = PyObject_GetAttrString((PyObject*)src->u.generic, "__copy__");
    if (fun == NULL)
    {
        PyErr_Clear();
    }
    else
    {
        PyObject* result;
        bool was_external = python_is_external;

        python_is_external = false;
        python_save_context();

        result = PyObject_CallObject(fun, NULL);

        python_clear_context();
        python_is_external = was_external;
        Py_DECREF(fun);

        if (result == NULL)
            raise_python_error("copy", started);
        else if (result == Py_NotImplemented)
            Py_DECREF(result);
        else
        {
            const char* err = python_to_svalue(dest, result);
            Py_DECREF(result);

            python_finish_thread(started);
            if (err != NULL)
                errorf("Bad return value from copy: %s\n", err);

            return;
        }
    }

    /* If we come here, the copy was not successful. */
    *dest = *src;
    ref_python_ob(src);

    python_finish_thread(started);
} /* copy_python_ob() */

/*-------------------------------------------------------------------------*/
bool
save_python_ob (svalue_t *dest, string_t **name, svalue_t *ob)

/* If the Python object <ob> is saveable, then return the actual value to
 * save into <dest> (new reference) and the corresponding type name (borrowed
 * reference) into <name> and return true. Return false if the object cannot
 * be saved (then <dest> and <name> stay undefined).
 */

{
    PyObject *fun;
    bool started = python_start_thread();

    assert(ob->type == T_PYTHON);
    assert(python_type_table[ob->x.python_type] != NULL);

    fun = PyObject_GetAttrString((PyObject*)ob->u.generic, "__save__");
    if (fun == NULL)
    {
        PyErr_Clear();
        python_finish_thread(started);
        return false;
    }
    else
    {
        PyObject* result;
        bool was_external = python_is_external;

        python_is_external = false;
        python_save_context();

        result = PyObject_CallObject(fun, NULL);

        python_clear_context();
        python_is_external = was_external;
        Py_DECREF(fun);

        if (result == NULL)
            raise_python_error("save", started);
        else if (result == Py_NotImplemented)
        {
            Py_DECREF(result);
            python_finish_thread(started);
            return false;
        }
        else
        {
            const char* err = python_to_svalue(dest, result);
            Py_DECREF(result);
            python_finish_thread(started);

            if (err != NULL)
            {
                return false;
            }
            if (dest->type == T_PYTHON)
            {
                free_svalue(dest);
                *dest = const0;
                return false;
            }

            *name = python_type_table[ob->x.python_type]->name->name;
            return true;
        }
    }

    return false; /* NOTREACHED */
} /* save_python_ob() */

/*-------------------------------------------------------------------------*/
bool
restore_python_ob (svalue_t *dest, string_t *name, svalue_t *value)

/* Restore a Python object of type <name> from a save file/string with value
 * <value>. The references of both values are adopted/freed, even in case
 * of an error. Return true for success.
 */

{
    bool was_external = python_is_external;
    ident_t *ident;
    PyObject *type, *fun, *arg, *args, *ob;
    const char* err;
    bool started;

    /* Get the type for the type name. */
    ident = find_shared_identifier_mstr(name, I_TYPE_PYTHON_TYPE, 0);
    free_mstring(name);

    if (!ident || ident->type != I_TYPE_PYTHON_TYPE)
    {
        free_svalue(value);
        return false;
    }

    assert(python_type_table[ident->u.python_type_id] != NULL);

    type = python_type_table[ident->u.python_type_id]->pytype;
    if (type == NULL)
    {
        /* Registration for that type was removed. */
        free_svalue(value);
        return false;
    }

    /* Call __restore__ on the type object. We haven't checked whether the
     * type actually has a __restore__(), we assume for now (as it has been
     * saved somehow) that it has and check it later.
     */

    started = python_start_thread();
    python_is_external = false;
    python_save_context();

    fun = PyObject_GetAttrString(type, "__restore__");
    if (fun == NULL)
    {
        PyErr_Clear();
        free_svalue(value);
        python_is_external = was_external;
        python_finish_thread(started);
        return false;
    }

    args = PyTuple_New(1);
    if (args == NULL)
    {
        PyErr_Clear();
        Py_DECREF(fun);
        free_svalue(value);
        python_is_external = was_external;
        python_finish_thread(started);
        return false;
    }

    arg = svalue_to_python(value);
    free_svalue(value);

    if (arg == NULL)
    {
        PyErr_Clear();
        Py_DECREF(fun);
        Py_DECREF(args);
        python_is_external = was_external;
        python_finish_thread(started);
        return false;
    }

    PyTuple_SET_ITEM(args, 0, arg);
    ob = PyObject_CallObject(fun, args);

    python_clear_context();
    python_is_external = was_external;
    Py_DECREF(fun);
    Py_DECREF(args);

    if (ob == NULL)
    {
        PyErr_Clear();
        python_finish_thread(started);
        return false;
    }
    else if (ob == Py_NotImplemented)
    {
        Py_DECREF(ob);
        python_finish_thread(started);
        return false;
    }

    /* And finally return the result. */
    err = python_to_svalue(dest, ob);
    Py_DECREF(ob);
    python_finish_thread(started);

    if (err != NULL)
        return false;

    return true;
} /* restore_python_ob() */

/*-------------------------------------------------------------------------*/
bool
convert_python_ob (svalue_t *dest, svalue_t *ob,  lpctype_t *type, struct_t *opts)

/* Convert a Python object to <type> of possible. Return true on success.
 */

{
    PyObject *fun, *pyob;
    bool started = python_start_thread();

    assert(ob->type == T_PYTHON);
    assert(python_type_table[ob->x.python_type] != NULL);

    pyob = (PyObject*)ob->u.generic;
    fun = PyObject_GetAttrString(pyob, "__convert__");
    if (fun != NULL)
    {
        PyObject *args = PyTuple_New(2);
        PyObject *result, *pytype, *pyopts;
        bool was_external = python_is_external;

        if (args == NULL)
        {
            Py_DECREF(fun);
            raise_python_error("to_type", started);
        }

        pytype = lpctype_to_pythontype(type);
        if (pytype == NULL)
        {
            Py_DECREF(fun);
            Py_DECREF(args);
            raise_python_error("to_type", started);
        }
        PyTuple_SET_ITEM(args, 0, pytype);

        if (opts)
            pyopts = ldmud_struct_create(opts);
        else
        {
            Py_INCREF(Py_None);
            pyopts = Py_None;
        }
        if (pyopts == NULL)
        {
            Py_DECREF(fun);
            Py_DECREF(args);
            raise_python_error("to_type", started);
        }
        PyTuple_SET_ITEM(args, 1, pyopts);

        python_is_external = false;
        python_save_context();

        result = PyObject_CallObject(fun, args);

        python_clear_context();
        python_is_external = was_external;
        Py_DECREF(fun);
        Py_DECREF(args);

        if (result == NULL)
            raise_python_error("to_type", started);
        else if (result == Py_NotImplemented)
        {
            Py_DECREF(result);
            python_finish_thread(started);
            return false;
        }
        else
        {
            const char* err = python_to_svalue(dest, result);
            Py_DECREF(result);
            python_finish_thread(started);

            if (err != NULL)
                errorf("Bad return value from __convert__: %s\n", err);

            return true;
        }
    }
    PyErr_Clear();

    /* Convert doesn't exist. Let's try native magic functions. */
    if (lpctype_contains(lpctype_int, type))
    {
        PyObject* result = PyNumber_Long(pyob);
        if (result != NULL)
        {
            const char* err = python_to_svalue(dest, result);
            Py_DECREF(result);
            python_finish_thread(started);

            if (err != NULL)
                errorf("Bad return value from __int__: %s\n", err);

            return true;
        }
        PyErr_Clear();
    }

    if (lpctype_contains(lpctype_float, type))
    {
        PyObject* result = PyNumber_Float(pyob);
        if (result != NULL)
        {
            const char* err = python_to_svalue(dest, result);
            Py_DECREF(result);
            python_finish_thread(started);

            if (err != NULL)
                errorf("Bad return value from __float__: %s\n", err);

            return true;
        }
        PyErr_Clear();
    }

    if (lpctype_contains(lpctype_string, type))
    {
        PyObject* result = PyObject_Str(pyob);
        if (result != NULL)
        {
            const char* err = python_to_svalue(dest, result);
            Py_DECREF(result);
            python_finish_thread(started);

            if (err != NULL)
                errorf("Bad return value from __str__: %s\n", err);

            return true;
        }
        PyErr_Clear();
    }

    if (lpctype_contains(lpctype_bytes, type))
    {
        PyObject* result = PyObject_Bytes(pyob);
        if (result != NULL)
        {
            const char* err = python_to_svalue(dest, result);
            Py_DECREF(result);
            python_finish_thread(started);

            if (err != NULL)
                errorf("Bad return value from __bytes__: %s\n", err);

            return true;
        }
        PyErr_Clear();
    }

    return false;
} /* convert_python_ob() */

/*-------------------------------------------------------------------------*/
string_t*
python_ob_to_string (svalue_t *pval)

/* Generate a printable string for the Python object.
 */

{
    bool started = python_start_thread();
    PyObject *repr = PyObject_Repr((PyObject*) pval->u.generic);

    if (!repr)
    {
        PyErr_Print();
        PyErr_Clear();
    }
    else if (PyUnicode_Check(repr))
    {
        string_t *str = python_string_to_string(repr);

        if (str != NULL)
        {
            Py_DECREF(repr);
            python_finish_thread(started);
            return str;
        }
    }

    Py_XDECREF(repr);
    python_finish_thread(started);

    return ref_mstring(get_python_type_name(pval->x.python_type)->name);
} /* python_ob_to_string() */

/*-------------------------------------------------------------------------*/
static svalue_t*
do_single_python_operation (svalue_t *sp, svalue_t *arg1, svalue_t *arg2, bool reverse, enum python_operation op, const char* op_name)

/* Execute the operation <op> on <arg1> and <arg2> (which are the topmost
 * values on <sp>). Pop elements from the stack and push the result on it,
 * returning the new stack pointer. Throw errors upon any (also type) errors.
 * Return NULL when no matching operation available.
 */

{
    python_type_operation_t *py_op;

    if (arg1->type != T_PYTHON)
        return NULL;

    assert(python_type_table[arg1->x.python_type] != NULL);
    py_op = python_type_table[arg1->x.python_type]->op + op;
    if (py_op->returntype == NULL)
        return NULL;

    if (check_rtt_compatibility(py_op->argtype, arg2))
    {
        bool started = python_start_thread();
        PyObject *arg = svalue_to_python(arg2);
        PyObject *fun, *args, *result;

        if (arg == NULL)
        {
            PyErr_Clear();
            python_finish_thread(started);

            inter_sp = sp;
            errorf("Bad %s argument to %s.\n", reverse ? "left" : "right", op_name);
        }

        fun = PyObject_GetAttrString((PyObject*)arg1->u.generic, python_operation_fun[op].name);
        args = PyTuple_New(1);

        if (fun && args)
        {
            bool was_external = python_is_external;

            PyTuple_SET_ITEM(args, 0, arg);

            python_is_external = false;
            python_save_context();
            result = PyObject_CallObject(fun, args);
            python_clear_context();
            python_is_external = was_external;

            if (result == NULL)
            {
                inter_sp = sp;
                raise_python_error(python_operation_fun[op].name, started);
            }
            else if (result == Py_NotImplemented)
            {
                Py_DECREF(result);
                result = NULL;
            }
        }
        else
        {
            result = NULL;
            Py_XDECREF(arg);
            PyErr_Clear();
        }

        Py_XDECREF(args);
        Py_XDECREF(fun);

        if (result != NULL)
        {
            const char* err;

            sp = pop_n_elems(2, sp);
            err = python_to_svalue(sp+1, result);
            Py_DECREF(result);
            python_finish_thread(started);

            if (err != NULL)
            {
                inter_sp = sp;
                errorf("Bad return value from %s: %s\n", op_name, err);
            }

            return sp+1;
        }

        python_finish_thread(started);
    }

    return NULL;
} /* do_single_python_operation() */

/*-------------------------------------------------------------------------*/
static void
raise_single_python_op_arg_error (svalue_t *sp, svalue_t *arg1, svalue_t *arg2, bool reverse, enum python_operation op, const char* op_name)

/* Throw an argument error for Python operation <op>.
 */

{
    python_type_operation_t *py_op = python_type_table[arg1->x.python_type]->op + op;

    inter_sp = sp;

    if (py_op->returntype == NULL)
    {
        errorf("Unsupported operation %s for %s.\n",
            op_name, get_txt(python_type_table[arg1->x.python_type]->name->name));
    }
    else
    {
        static char buff[512];

        lpctype_t *realtype = get_rtt_type(py_op->argtype, arg2);
        get_lpctype_name_buf(realtype, buff, sizeof(buff));
        free_lpctype(realtype);

        errorf("Bad %s argument to %s: got '%s', expected '%s'.\n",
            reverse ? "left" : "right", op_name,
            buff, get_lpctype_name(py_op->argtype));
    }
} /* raise_single_python_op_arg_error() */

/*-------------------------------------------------------------------------*/
svalue_t*
do_python_unary_operation (svalue_t *sp, enum python_operation op, const char* op_name)

/* Execute the operation <op> on sp[0]. Pop it from the stack and
 * push the result on it. Throw errors upon any errors.
 */

{
    python_type_operation_t *py_op = python_type_table[sp->x.python_type]->op + op;

    if (py_op->returntype != NULL)
    {
        bool started = python_start_thread();
        PyObject *fun = PyObject_GetAttrString((PyObject*)sp->u.generic, python_operation_fun[op].name);
        if (fun)
        {
            bool was_external = python_is_external;
            PyObject *result;

            python_is_external = false;
            python_save_context();
            result = PyObject_CallObject(fun, NULL);
            python_clear_context();
            python_is_external = was_external;

            Py_DECREF(fun);

            if (result == NULL)
            {
                inter_sp = sp;
                raise_python_error(python_operation_fun[op].name, started);
            }
            else if (result == Py_NotImplemented)
            {
                Py_DECREF(result);
            }
            else
            {
                const char* err;

                free_svalue(sp);
                err = python_to_svalue(sp, result);
                Py_DECREF(result);
                python_finish_thread(started);

                if (err != NULL)
                {
                    inter_sp = sp-1;
                    errorf("Bad return value from %s: %s\n", op_name, err);
                }

                return sp;
            }
        }
        else
            PyErr_Clear();
        python_finish_thread(started);
    }

    errorf("Unsupported operation %s for %s.\n",
        op_name, get_txt(python_type_table[sp->x.python_type]->name->name));

    return NULL; /* NOTREACHED */

} /* do_python_unary_operation() */

/*-------------------------------------------------------------------------*/
svalue_t*
do_python_binary_operation (svalue_t *sp, enum python_operation op, enum python_operation rop, const char* op_name)

/* Execute the operation <op> or reverse operation <rop> on sp[-1] and sp[0].
 * Pop elements from the stack and push the result on it, returning the new
 * stack pointer. Throw errors upon any (also type) errors.
 *
 * If both values are python objects and provide a corresponding operation,
 * we might try both if the first one reports NotImplemented.
 */

{
    svalue_t *result;

    result = do_single_python_operation(sp, sp-1, sp, false, op, op_name);
    if (result)
        return result;

    result = do_single_python_operation(sp, sp, sp-1, true, rop, op_name);
    if (result)
        return result;

    /* Now generate some error message. */
    if (sp[-1].type == T_PYTHON)
        raise_single_python_op_arg_error(sp, sp-1, sp, false, op, op_name);
    else
    {
        assert(sp[0].type == T_PYTHON);
        raise_single_python_op_arg_error(sp, sp, sp-1, true, rop, op_name);
    }

    return NULL; /* NOTREACHED */
} /* do_python_binary_operation() */

/*-------------------------------------------------------------------------*/
svalue_t *
do_python_assignment_operation (svalue_t *sp, svalue_t *dest, enum python_operation iop, enum python_operation op, enum python_operation rop, const char* op_name)

/* Execute an assignment operation <iop> on the Python object <dest>
 * with sp[-1]. If not successful, try normal operations <op> and <rop>.
 * Remove the arguments from the stack and return the result on the
 * stack (or throw an error).
 */

{
    svalue_t * result;

    result = do_single_python_operation(sp, dest, sp-1, false, iop, op_name);
    if (result != NULL)
    {
        assign_svalue(dest, result);
        return result;
    }

    result = do_single_python_operation(sp, dest, sp-1, false, op, op_name);
    if (result)
    {
        assign_svalue(dest, result);
        return result;
    }

    result = do_single_python_operation(sp, sp-1, dest, true, rop, op_name);
    if (result)
    {
        assign_svalue(dest, result);
        return result;
    }

    /* Now generate some error message. */
    if (dest->type == T_PYTHON)
    {
        if (python_type_table[dest->x.python_type]->op[iop].returntype != NULL)
            raise_single_python_op_arg_error(sp, dest, sp-1, false, iop, op_name);
        else
            raise_single_python_op_arg_error(sp, dest, sp-1, false, op, op_name);
    }
    else
    {
        assert(sp[-1].type == T_PYTHON);
        raise_single_python_op_arg_error(sp, sp-1, dest, true, rop, op_name);
    }

    return NULL; /* NOTREACHED */
} /* do_python_assignment_operation() */

/*-------------------------------------------------------------------------*/
svalue_t*
call_python_type_efun(svalue_t *sp, int efun, int num_arg)

/* Check whether the first argument on the stack <sp> is a Python object,
 * if so call any efun override for it. Return the new stack pointer
 * on success, NULL if no override was found.
 * If the original efun returns lpctype_void, then also this function
 * will not leave a result on the stack.
 */

{
    svalue_t *arg = sp - num_arg + 1;
    python_efun_info_t* efun_info;
    char funname[64];
    PyObject *fun, *args, *result;
    bool started;
    bool was_external = python_is_external;

    if (num_arg == 0)
        return NULL;

    if (arg->type != T_PYTHON)
        return NULL;

    assert(python_type_table[arg->x.python_type] != NULL);

    efun_info = get_python_type_efun_info(arg->x.python_type, efun);
    if (efun_info == NULL)
        return NULL;

    started = python_start_thread();

    /* Check the arguments. */
    inter_sp = sp;
    args = build_python_efun_args(efun_info, arg, num_arg, instrs[efun].name, true, started);

    /* Object (still) does have this fun? */
    snprintf(funname, sizeof(funname), "__efun_%s__", instrs[efun].name);
    fun = PyObject_GetAttrString((PyObject*)arg->u.generic, funname);

    if (!fun || !PyCallable_Check(fun))
    {
        Py_XDECREF(fun);
        Py_XDECREF(args);
        PyErr_Clear();
        python_finish_thread(started);
        return NULL;
    }

    /* Do the call. */
    inter_sp = pop_n_elems(num_arg, inter_sp);

    python_is_external = false;
    python_save_context();
    result = PyObject_CallObject(fun, args);
    python_clear_context();
    python_is_external = was_external;
    Py_XDECREF(fun);
    Py_XDECREF(args);

    if (result == NULL)
    {
        /* Exception occurred. */
        raise_python_error(instrs[efun].name, started);
    }
    else if (instrs[efun].ret_type != lpctype_void)
    {
        const char *err = python_to_svalue(inter_sp + 1, result);
        Py_DECREF(result);

        if (err != NULL)
        {
            python_finish_thread(started);
            errorf("Bad return value from %s(): %s\n"
                  , instrs[efun].name
                  , err);
        }

        inter_sp++;
    }
    else
        Py_DECREF(result);

    python_finish_thread(started);
    return inter_sp;
} /* call_python_type_efun() */

/*-------------------------------------------------------------------------*/
void
python_free_object (object_t *ob)

/* Free the dictionary from <ob>.
 */

{

    if (ob->python_dict != NULL)
    {
        bool started = python_start_thread();
        Py_DECREF((PyObject*) ob->python_dict);
        python_finish_thread(started);
    }
} /* python_free_object() */

/*-------------------------------------------------------------------------*/
void
python_free_replace_program_protector (replace_ob_t *r_ob)

/* Free the python replace_program() protector and all references
 * it holds.
 */

{
    struct python_replace_program_protector *prpp = r_ob->python_rpp;
    while (prpp)
    {
        struct python_replace_program_protector *next = prpp->next;
        bool started = python_start_thread();

        Py_DECREF(prpp->ref);
        python_finish_thread(started);

        xfree(prpp);

        prpp = next;
    }
} /* python_free_replace_program_protector() */

/*-------------------------------------------------------------------------*/
static void
python_replace_program_adjust_single_ref (ldmud_program_and_index_t* ref, replace_ob_t *r_ob, int (*convert_idx)(replace_ob_t*, int))

/* Update a single reference after a replace_program().
 */

{
    int index;

    if (ref->index < 0) // Start of an iterator.
        return;

    index = (*convert_idx)(r_ob, ref->index);
    if (index < 0)
    {
        /* We set the object to NULL. */
        free_svalue(&(ref->ob_base.lpc_object));
        ref->ob_base.lpc_object = const0;
        ref->ob_base.lpc_program = NULL;
        ref->index = 0;
    }
    else
        ref->index = index;
} /* python_replace_program_adjust_single_ref() */

/*-------------------------------------------------------------------------*/
void
python_replace_program_adjust (replace_ob_t *r_ob)

/* Walk through the references and adjust the indices of lfuns and variables.
 * Also free the protectors.
 */

{
    struct python_replace_program_protector *prpp = r_ob->python_rpp;
    while (prpp)
    {
        struct python_replace_program_protector *next = prpp->next;
        bool started = python_start_thread();

        /* At least one more reference, otherwise we don't need to care. */
        if (prpp->ref->ob_refcnt > 1)
        {
            if (Py_TYPE(prpp->ref) == &ldmud_program_lfun_type ||
                Py_TYPE(prpp->ref) == &ldmud_program_functions_iter_type)
                python_replace_program_adjust_single_ref((ldmud_program_and_index_t*)prpp->ref
                                                       , r_ob
                                                       , replace_program_function_adjust);
            else if (Py_TYPE(prpp->ref) == &ldmud_program_variable_type ||
                Py_TYPE(prpp->ref) == &ldmud_program_variables_iter_type)
                python_replace_program_adjust_single_ref((ldmud_program_and_index_t*)prpp->ref
                                                       , r_ob
                                                       , replace_program_variable_adjust);
        }

        Py_DECREF(prpp->ref);
        python_finish_thread(started);

        xfree(prpp);

        prpp = next;
    }
} /* python_replace_program_adjust() */

/*-------------------------------------------------------------------------*/
void
cleanup_python_data (cleanup_t * context)

/* Cleanup any Python-held LPC values. */

{
    bool started = python_start_thread();
    for(ldmud_gc_var_t* var = gc_object_list; var != NULL; var = var->gcnext)
    {
        object_t* ob = ((ldmud_object_t*)var)->lpc_object;
        if(ob != NULL && (ob->flags & O_DESTRUCTED))
        {
            free_object(ob, "cleanup_python_data");
            ((ldmud_object_t*)var)->lpc_object = NULL;
        }
    }

    for(ldmud_gc_var_t* var = gc_lwobject_list; var != NULL; var = var->gcnext)
    {
        lwobject_t* lwob = ((ldmud_lwobject_t*)var)->lpc_lwobject;
        if(lwob != NULL)
            cleanup_vector(lwob->variables, lwob->prog->num_variables, context);
    }

    for(ldmud_gc_var_t* var = gc_program_list; var != NULL; var = var->gcnext)
    {
        ldmud_program_t* self = ((ldmud_program_t*)var);
        cleanup_vector(&(self->lpc_object), 1, context);
    }

    for(ldmud_gc_var_t* var = gc_array_list; var != NULL; var = var->gcnext)
    {
        vector_t* arr = ((ldmud_array_t*)var)->lpc_array;
        if (arr != NULL)
            cleanup_vector(arr->item, VEC_SIZE(arr), context);
    }

    for(ldmud_gc_var_t* var = gc_mapping_list; var != NULL; var = var->gcnext)
    {
        mapping_t* m = ((ldmud_mapping_t*)var)->lpc_mapping;
        if (m != NULL)
        {
            /* Let cleanup_vector do that. */
            svalue_t mapping = svalue_mapping(m);
            cleanup_vector(&mapping, 1, context);
        }
    }

    for(ldmud_gc_var_t* var = gc_mapping_list_list; var != NULL; var = var->gcnext)
    {
        /* Let cleanup_vector do that. */
        svalue_t values[2] = { svalue_mapping(((ldmud_mapping_list_t*)var)->map),
                               svalue_array(((ldmud_mapping_list_t*)var)->indices) };
        cleanup_vector(values, 2, context);
    }

    for(ldmud_gc_var_t* var = gc_struct_list; var != NULL; var = var->gcnext)
    {
        struct_t* s = ((ldmud_struct_t*)var)->lpc_struct;
        if(s != NULL)
            cleanup_vector(s->member, struct_size(s), context);
    }

    for(ldmud_gc_var_t* var = gc_closure_list; var != NULL; var = var->gcnext)
    {
        cleanup_vector(&((ldmud_closure_t*)var)->lpc_closure, 1, context);
    }

    for(ldmud_gc_var_t* var = gc_coroutine_list; var != NULL; var = var->gcnext)
    {
        coroutine_t *cr = ((ldmud_coroutine_t*)var)->lpc_coroutine;
        if(cr != NULL)
        {
            svalue_t coroutine = svalue_coroutine(cr);
            cleanup_vector(&coroutine, 1, context);

            if (coroutine.type != T_COROUTINE)
                ((ldmud_coroutine_t*)var)->lpc_coroutine = NULL;
        }
    }

    for(ldmud_gc_var_t* var = gc_symbol_list; var != NULL; var = var->gcnext)
    {
        cleanup_vector(&((ldmud_symbol_t*)var)->lpc_symbol, 1, context);
    }

    for(ldmud_gc_var_t* var = gc_quoted_array_list; var != NULL; var = var->gcnext)
    {
        cleanup_vector(&((ldmud_quoted_array_t*)var)->lpc_quoted_array, 1, context);
    }

    for(ldmud_gc_var_t* var = gc_lvalue_list; var != NULL; var = var->gcnext)
    {
        cleanup_vector(&((ldmud_lvalue_t*)var)->lpc_lvalue, 1, context);
    }

    python_finish_thread(started);
} /* cleanup_python_data() */

#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
python_clear_refs ()

/* GC Support: Clear all reference counts.
 */

{
    bool started = python_start_thread();

    for (int idx = 0; idx < PYTHON_EFUN_TABLE_SIZE; idx++)
    {
        if (python_efun_table[idx].info.types)
            for (int pos = 0; pos < 1 + python_efun_table[idx].info.maxarg + (python_efun_table[idx].info.varargs ? 1 : 0); pos++)
                clear_lpctype_ref(python_efun_table[idx].info.types[pos]);
    }

    for (int idx = 0; idx < num_python_structs; idx++)
    {
        if (python_struct_table[idx])
            clear_struct_type_ref(python_struct_table[idx]);
    }

    for (int idx = 0; idx < PYTHON_TYPE_TABLE_SIZE; idx++)
    {
        if (python_type_table[idx] != NULL)
        {
            clear_lpctype_ref(python_type_table[idx]->lpctype);

            for (int op = 0; op < PYTHON_OPERATIONS_COUNT; op++)
            {
                clear_lpctype_ref(python_type_table[idx]->op[op].returntype);
                clear_lpctype_ref(python_type_table[idx]->op[op].argtype);
            }

            for (int efun = 0; efun < REAL_EFUN_COUNT; efun++)
                if (python_type_table[idx]->efun[efun].exists
                 && python_type_table[idx]->efun[efun].types)
                    for (int pos = 0; pos < 1 + python_type_table[idx]->efun[efun].maxarg + (python_type_table[idx]->efun[efun].varargs ? 1 : 0); pos++)
                        clear_lpctype_ref(python_type_table[idx]->efun[efun].types[pos]);
        }
    }

    for(ldmud_gc_var_t* var = gc_object_list; var != NULL; var = var->gcnext)
    {
        object_t* ob = ((ldmud_object_t*)var)->lpc_object;
        if(ob != NULL)
            clear_object_ref(ob);
    }

    for(ldmud_gc_var_t* var = gc_lwobject_list; var != NULL; var = var->gcnext)
    {
        lwobject_t* lwob = ((ldmud_lwobject_t*)var)->lpc_lwobject;
        if(lwob != NULL)
            clear_lwobject_ref(lwob);
    }

    for(ldmud_gc_var_t* var = gc_program_list; var != NULL; var = var->gcnext)
    {
        ldmud_program_t* self = ((ldmud_program_t*)var);

        clear_ref_in_vector(&(self->lpc_object), 1);
        self->lpc_program = NULL; /* Will be restored by python_count_refs(). */
    }

    for(ldmud_gc_var_t* var = gc_array_list; var != NULL; var = var->gcnext)
    {
        vector_t* arr = ((ldmud_array_t*)var)->lpc_array;
        if (arr->ref)
        {
            arr->ref = 0;
            clear_ref_in_vector(arr->item, VEC_SIZE(arr));
        }
    }

    for(ldmud_gc_var_t* var = gc_mapping_list; var != NULL; var = var->gcnext)
    {
        /* Let clear_ref_in_vector do that. */
        svalue_t mapping = { T_MAPPING };
        mapping.u.map = ((ldmud_mapping_t*)var)->lpc_mapping;
        clear_ref_in_vector(&mapping, 1);
    }

    for(ldmud_gc_var_t* var = gc_mapping_list_list; var != NULL; var = var->gcnext)
    {
        /* Let clear_ref_in_vector do that. */
        svalue_t values[2] = { { T_MAPPING }, { T_POINTER } };
        values[0].u.map = ((ldmud_mapping_list_t*)var)->map;
        values[1].u.vec = ((ldmud_mapping_list_t*)var)->indices;
        clear_ref_in_vector(values, 2);
    }

    for(ldmud_gc_var_t* var = gc_struct_list; var != NULL; var = var->gcnext)
    {
        struct_t* s = ((ldmud_struct_t*)var)->lpc_struct;
        if(s != NULL)
            clear_struct_ref(s);
    }

    for(ldmud_gc_var_t* var = gc_closure_list; var != NULL; var = var->gcnext)
    {
        clear_ref_in_vector(&((ldmud_closure_t*)var)->lpc_closure, 1);
    }

    for(ldmud_gc_var_t* var = gc_coroutine_list; var != NULL; var = var->gcnext)
    {
        coroutine_t *cr = ((ldmud_coroutine_t*)var)->lpc_coroutine;
        if(cr != NULL)
            clear_coroutine_ref(cr);
    }

    for(ldmud_gc_var_t* var = gc_symbol_list; var != NULL; var = var->gcnext)
    {
        clear_ref_in_vector(&((ldmud_symbol_t*)var)->lpc_symbol, 1);
    }

    for(ldmud_gc_var_t* var = gc_quoted_array_list; var != NULL; var = var->gcnext)
    {
        clear_ref_in_vector(&((ldmud_quoted_array_t*)var)->lpc_quoted_array, 1);
    }

    for(ldmud_gc_var_t* var = gc_lvalue_list; var != NULL; var = var->gcnext)
    {
        clear_ref_in_vector(&((ldmud_lvalue_t*)var)->lpc_lvalue, 1);
    }

    for(ldmud_gc_var_t* var = gc_call_frame_list; var != NULL; var = var->gcnext)
    {
        clear_ref_in_vector(&((ldmud_call_frame_t*)var)->ob, 1);
        clear_program_ref(((ldmud_call_frame_t*)var)->prog, true);
    }

    for(ldmud_gc_var_t* var = gc_call_frame_ref_list; var != NULL; var = var->gcnext)
    {
        clear_program_ref(((ldmud_call_frame_ref_t*)var)->prog, true);
    }

    for(ldmud_gc_lpctype_type_t* var = gc_lpctype_list; var != NULL; var = var->gcnext)
    {
        clear_lpctype_ref(var->type);
    }

    for(ldmud_concrete_struct_type_t* var = gc_struct_type_list; var != NULL; var = var->gcnext)
    {
        clear_struct_name_ref(var->name);
    }

    python_finish_thread(started);
} /* python_clear_refs() */

/*-------------------------------------------------------------------------*/
void
python_count_refs ()

/* GC Support: Mark all references to xalloc'ed memory.
 */

{
    bool started = python_start_thread();

    /* The efun table */
    for (int idx = 0; idx < PYTHON_EFUN_TABLE_SIZE; idx++)
    {
        if (python_efun_table[idx].info.types)
        {
            note_malloced_block_ref(python_efun_table[idx].info.types);

            for (int pos = 0; pos < 1 + python_efun_table[idx].info.maxarg + (python_efun_table[idx].info.varargs ? 1 : 0); pos++)
                count_lpctype_ref(python_efun_table[idx].info.types[pos]);
        }
    }

    /* The struct table */
    for (int idx = 0; idx < num_python_structs; idx++)
    {
        if (python_struct_table[idx])
            count_struct_type_ref(python_struct_table[idx]);
    }

    /* The type table */
    for (int idx = 0; idx < PYTHON_TYPE_TABLE_SIZE; idx++)
    {
        if (python_type_table[idx] != NULL)
        {
            note_malloced_block_ref(python_type_table[idx]);
            count_lpctype_ref(python_type_table[idx]->lpctype);
            for (int op = 0; op < PYTHON_OPERATIONS_COUNT; op++)
            {
                count_lpctype_ref(python_type_table[idx]->op[op].returntype);
                count_lpctype_ref(python_type_table[idx]->op[op].argtype);
            }

            for (int efun = 0; efun < REAL_EFUN_COUNT; efun++)
                if (python_type_table[idx]->efun[efun].exists
                 && python_type_table[idx]->efun[efun].types)
                {
                    note_malloced_block_ref(python_type_table[idx]->efun[efun].types);

                    for (int pos = 0; pos < 1 + python_type_table[idx]->efun[efun].maxarg + (python_type_table[idx]->efun[efun].varargs ? 1 : 0); pos++)
                        count_lpctype_ref(python_type_table[idx]->efun[efun].types[pos]);
                }
        }
    }

    /* Our polling list */
    for (python_poll_fds_t *fds = poll_fds; fds != NULL; fds = fds->next)
        note_malloced_block_ref(fds);

    /* The python hooks */
    for (int i = 0; i < PYTHON_HOOK_COUNT; i++)
        for(python_hook_t *entry = python_hooks[i]; entry; entry = entry->next)
            note_malloced_block_ref(entry);

    /* All Python objects that reference LPC data */
    for(ldmud_gc_var_t* var = gc_object_list; var != NULL; var = var->gcnext)
    {
        object_t* ob = ((ldmud_object_t*)var)->lpc_object;
        if(ob != NULL)
        {
            if (ob->flags & O_DESTRUCTED)
            {
                ((ldmud_object_t*)var)->lpc_object = NULL;
                reference_destructed_object(ob);
            }
            else
            {
                ob->ref++;
            }
        }
    }

    for(ldmud_gc_var_t* var = gc_lwobject_list; var != NULL; var = var->gcnext)
    {
        lwobject_t* lwob = ((ldmud_lwobject_t*)var)->lpc_lwobject;
        if(lwob != NULL)
            count_lwobject_ref(lwob);
    }

    for(ldmud_gc_var_t* var = gc_program_list; var != NULL; var = var->gcnext)
    {
        ldmud_program_t* self = ((ldmud_program_t*)var);

        count_ref_in_vector(&(self->lpc_object), 1);
        switch (self->lpc_object.type)
        {
            case T_OBJECT:
                self->lpc_program = self->lpc_object.u.ob->prog;
                break;

            case T_LWOBJECT:
                self->lpc_program = self->lpc_object.u.lwob->prog;
                break;

            case T_NUMBER:
            default:
                self->lpc_program = NULL;
        }
    }

    for(ldmud_gc_var_t* var = gc_array_list; var != NULL; var = var->gcnext)
    {
        vector_t* arr = ((ldmud_array_t*)var)->lpc_array;

        if (arr != &null_vector && x_test_ref(arr))
        {
            x_mark_ref(arr);
            count_array_size(arr);
            count_ref_in_vector(arr->item, VEC_SIZE(arr));
        }
        arr->ref++;
    }

    for(ldmud_gc_var_t* var = gc_mapping_list; var != NULL; var = var->gcnext)
    {
        mapping_t* map = ((ldmud_mapping_t*)var)->lpc_mapping;

        if (x_test_ref(map))
        {
            x_mark_ref(map);
            count_ref_in_mapping(map);
            count_mapping_size(map);
        }
        map->ref++;
    }

    for(ldmud_gc_var_t* var = gc_mapping_list_list; var != NULL; var = var->gcnext)
    {
        /* Let count_ref_in_vector do that. */
        svalue_t values[2] = { { T_MAPPING }, { T_POINTER } };
        values[0].u.map = ((ldmud_mapping_list_t*)var)->map;
        values[1].u.vec = ((ldmud_mapping_list_t*)var)->indices;
        count_ref_in_vector(values, 2);
    }

    for(ldmud_gc_var_t* var = gc_struct_list; var != NULL; var = var->gcnext)
    {
        struct_t* s = ((ldmud_struct_t*)var)->lpc_struct;
        if(s != NULL)
            count_struct_ref(s);
    }

    for(ldmud_gc_var_t* var = gc_closure_list; var != NULL; var = var->gcnext)
    {
        count_ref_in_vector(&((ldmud_closure_t*)var)->lpc_closure, 1);
    }

    for(ldmud_gc_var_t* var = gc_coroutine_list; var != NULL; var = var->gcnext)
    {
        coroutine_t *cr = ((ldmud_coroutine_t*)var)->lpc_coroutine;
        if(cr != NULL)
            count_coroutine_ref(cr);
    }

    for(ldmud_gc_var_t* var = gc_symbol_list; var != NULL; var = var->gcnext)
    {
        count_ref_in_vector(&((ldmud_symbol_t*)var)->lpc_symbol, 1);
    }

    for(ldmud_gc_var_t* var = gc_quoted_array_list; var != NULL; var = var->gcnext)
    {
        count_ref_in_vector(&((ldmud_quoted_array_t*)var)->lpc_quoted_array, 1);
    }

    for(ldmud_gc_var_t* var = gc_lvalue_list; var != NULL; var = var->gcnext)
    {
        count_ref_in_vector(&((ldmud_lvalue_t*)var)->lpc_lvalue, 1);
    }

    for(ldmud_gc_var_t* var = gc_call_frame_list; var != NULL; var = var->gcnext)
    {
        count_ref_in_vector(&((ldmud_call_frame_t*)var)->ob, 1);
        mark_program_ref(((ldmud_call_frame_t*)var)->prog);
    }

    for(ldmud_gc_var_t* var = gc_call_frame_ref_list; var != NULL; var = var->gcnext)
    {
        mark_program_ref(((ldmud_call_frame_ref_t*)var)->prog);
    }

    for(ldmud_gc_lpctype_type_t* var = gc_lpctype_list; var != NULL; var = var->gcnext)
    {
        count_lpctype_ref(var->type);
    }

    for(ldmud_concrete_struct_type_t* var = gc_struct_type_list; var != NULL; var = var->gcnext)
    {
        count_struct_name_ref(var->name);
    }

    python_finish_thread(started);
} /* python_count_refs() */

#endif /* GC_SUPPORT */

#ifdef DEBUG
void
count_python_extra_refs ()

/* Refcount Debugging: Count the refcounts in objects and arrays.
 */

{
    bool started = python_start_thread();

    /* All Python objects that reference LPC data. */
    for(ldmud_gc_var_t* var = gc_object_list; var != NULL; var = var->gcnext)
    {
        object_t* ob = ((ldmud_object_t*)var)->lpc_object;
        if(ob != NULL)
            count_extra_ref_in_object(ob);
    }

    for(ldmud_gc_var_t* var = gc_lwobject_list; var != NULL; var = var->gcnext)
    {
        lwobject_t* lwob = ((ldmud_lwobject_t*)var)->lpc_lwobject;
        if(lwob != NULL)
        {
            svalue_t sv = { T_LWOBJECT, {}, { .lwob = lwob } };
            count_extra_ref_in_vector(&sv, 1);
        }
    }

    for(ldmud_gc_var_t* var = gc_program_list; var != NULL; var = var->gcnext)
    {
        count_extra_ref_in_vector(&(((ldmud_program_t*)var)->lpc_object), 1);
    }

    for(ldmud_gc_var_t* var = gc_array_list; var != NULL; var = var->gcnext)
    {
        svalue_t vec = { T_POINTER };
        vec.u.vec = ((ldmud_array_t*)var)->lpc_array;

        count_extra_ref_in_vector(&vec, 1);
    }

    for(ldmud_gc_var_t* var = gc_mapping_list; var != NULL; var = var->gcnext)
    {
        svalue_t map = { T_MAPPING };
        map.u.map = ((ldmud_mapping_t*)var)->lpc_mapping;

        count_extra_ref_in_vector(&map, 1);
    }

    for(ldmud_gc_var_t* var = gc_mapping_list_list; var != NULL; var = var->gcnext)
    {
        /* Let count_ref_in_vector do that. */
        svalue_t values[2] = { { T_MAPPING }, { T_POINTER } };
        values[0].u.map = ((ldmud_mapping_list_t*)var)->map;
        values[1].u.vec = ((ldmud_mapping_list_t*)var)->indices;
        count_extra_ref_in_vector(values, 2);
    }

    for(ldmud_gc_var_t* var = gc_struct_list; var != NULL; var = var->gcnext)
    {
        svalue_t str = { T_STRUCT };
        str.u.strct = ((ldmud_struct_t*)var)->lpc_struct;

        count_extra_ref_in_vector(&str, 1);
    }

    for(ldmud_gc_var_t* var = gc_closure_list; var != NULL; var = var->gcnext)
    {
        count_extra_ref_in_vector(&((ldmud_closure_t*)var)->lpc_closure, 1);
    }

    for(ldmud_gc_var_t* var = gc_coroutine_list; var != NULL; var = var->gcnext)
    {
        coroutine_t* cr = ((ldmud_coroutine_t*)var)->lpc_coroutine;
        if(cr != NULL)
        {
            svalue_t sv = { T_COROUTINE, {}, { .coroutine = cr } };
            count_extra_ref_in_vector(&sv, 1);
        }
    }

    for(ldmud_gc_var_t* var = gc_symbol_list; var != NULL; var = var->gcnext)
    {
        count_extra_ref_in_vector(&((ldmud_symbol_t*)var)->lpc_symbol, 1);
    }

    for(ldmud_gc_var_t* var = gc_quoted_array_list; var != NULL; var = var->gcnext)
    {
        count_extra_ref_in_vector(&((ldmud_quoted_array_t*)var)->lpc_quoted_array, 1);
    }

    for(ldmud_gc_var_t* var = gc_lvalue_list; var != NULL; var = var->gcnext)
    {
        count_extra_ref_in_vector(&((ldmud_lvalue_t*)var)->lpc_lvalue, 1);
    }

    python_finish_thread(started);
} /* count_python_extra_refs() */

#endif /* DEBUG */

#endif /* USE_PYTHON && HAS_PYTHON3 */
