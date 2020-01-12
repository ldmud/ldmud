/*------------------------------------------------------------------
 * Python support.
 *------------------------------------------------------------------
 * This file contains the glue for the interaction between
 * python and the LPC runtime.
 *------------------------------------------------------------------
 */
#include "driver.h"
#include "machine.h"

#if defined(USE_PYTHON) && defined(HAS_PYTHON3)

#include <poll.h>

#include "actions.h"
#include "array.h"
#include "closure.h"
#include "exec.h"
#include "gcollect.h"
#include "instrs.h"
#include "interpret.h"
#include "lex.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "pkg-python.h"
#include "prolang.h"
#include "simul_efun.h"
#include "simulate.h"
#include "structs.h"
#include "swap.h"
#include "typedefs.h"
#include "wiz_list.h"
#include "xalloc.h"

/* Python.h defines these again... */
#undef _GNU_SOURCE
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#if PY_VERSION_HEX >= 0x03070000
#define USE_PYTHON_CONTEXT
#endif

/* --- Type declarations --- */
typedef struct ldmud_gc_var_s ldmud_gc_var_t;
typedef void (*CClosureFun)(void*);
typedef struct python_efun_s python_efun_t;
typedef struct python_poll_fds_s python_poll_fds_t;
typedef struct python_hook_s python_hook_t;

/* --- Type definitions --- */
struct python_efun_s
{
    PyObject*   callable;       /* Python callable of the efun.        */
    ident_t*    name;           /* The identifier of the efun.         */
    lpctype_t** types;          /* The return type and argument types:
                                 *   [0]:           return type
                                 *   [1 .. maxarg]: argument types
                                 *   [maxarg + 1]:  vararg type.
                                 */
    int         minarg;         /* Minimum number of arguments.        */
    int         maxarg;         /* Maximum number of arguments.        */
    bool        varargs;        /* Whether we have a variable number.  */
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

/* --- Variables --- */
char * python_startup_script = NULL;

static volatile bool python_pending_sigchld = false;
 /* We received a SIGCHLD and need to pass it to Python.
  */

static bool python_is_external = true;
 /* Remember how python code was called,
  * false, when called from a running LPC program,
  * true otherwise (upon external events)
  */

int num_python_efun = 0;

ident_t *all_python_efuns = NULL;

static python_efun_t python_efun_table[PYTHON_EFUN_TABLE_SIZE];
  /* Information about all defined efuns.
   */

static python_poll_fds_t *poll_fds = NULL;
  /* List of all via register_socket() registered file descriptors.
   */

static python_hook_t *python_hooks[PYTHON_HOOK_COUNT];
static const char* python_hook_names[] = {
    "ON_HEARTBEAT",
    "ON_OBJECT_CREATED",
    "ON_OBJECT_DESTRUCTED",
    "ON_CHILD_PROCESS_TERMINATED",
};

static ldmud_gc_var_t *gc_object_list = NULL,
                      *gc_array_list = NULL,
                      *gc_mapping_list = NULL,
                      *gc_mapping_list_list = NULL,
                      *gc_struct_list = NULL,
                      *gc_closure_list = NULL,
                      *gc_symbol_list = NULL,
                      *gc_quoted_array_list = NULL;

#ifdef USE_PYTHON_CONTEXT
static PyObject * python_contextvar_current_object = NULL;
static PyObject * python_contextvar_command_giver = NULL;
  /* Context variables that store the current object
   * and command giver.
   */
#endif

/* -- Function prototypes --- */
static lpctype_t* pythontype_to_lpctype(PyObject* ptype);
static const char* python_to_svalue(svalue_t *dest, PyObject* val);
static PyObject* svalue_to_python (svalue_t *svp);
static bool python_eq_svalue(PyObject* pval, svalue_t *sval);
static bool call_lpc_secure(CClosureFun fun, void* data);
static void python_save_context();
static void python_clear_context();
static void python_restore_context();

/* -- Python definitions and functions --- */

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

    ident = make_shared_identifier(name, I_TYPE_GLOBAL, 0);
    if (!ident)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return NULL;
    }

    if (ident->type == I_TYPE_UNKNOWN)
    {
        init_global_identifier(ident, MY_FALSE);
        ident->next_all = all_python_efuns;
        all_python_efuns = ident;
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
                    ident->next_all = all_python_efuns;
                    all_python_efuns = ident;

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

    /* This is or once was a python efun? */
    if (ident->u.global.python_efun != I_GLOBAL_PYTHON_EFUN_OTHER)
    {
        int idx = ident->u.global.python_efun;

        python_efun_entry = python_efun_table + idx;
        Py_XDECREF(python_efun_table[idx].callable);
        xfree(python_efun_table[idx].types);
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
    python_efun_entry->types = NULL;
    python_efun_entry->minarg = 0;
    python_efun_entry->maxarg = 0;
    python_efun_entry->varargs = true;

    /* Let's check whether we have type information. */
    do  /* A loop, so we can exit this block easily. */
    {
        PyObject *annotations, *code, *property, *varnames, *returnname, *defaults;
        lpctype_t **types;
        long argcount, kwonlyargcount, flags;

        /* We have only enough information for real functions. */
        if (!PyFunction_Check(fun))
            break;

        /* First let's try to get the argument counts. */
        code = PyFunction_GetCode(fun);
        if (!code || !PyCode_Check(code))
            break;

        property = PyObject_GetAttrString(code, "co_argcount");
        if (!property || !PyLong_Check(property))
        {
            Py_XDECREF(property);
            break;
        }
        argcount = PyLong_AsLong(property);
        Py_XDECREF(property);

        property = PyObject_GetAttrString(code, "co_kwonlyargcount");
        if (!property || !PyLong_Check(property))
        {
            Py_XDECREF(property);
            break;
        }
        kwonlyargcount = PyLong_AsLong(property);
        Py_XDECREF(property);

        property = PyObject_GetAttrString(code, "co_flags");
        if (!property || !PyLong_Check(property))
        {
            Py_XDECREF(property);
            break;
        }
        flags = PyLong_AsLong(property);
        Py_XDECREF(property);

        python_efun_entry->minarg = (int)argcount;
        python_efun_entry->maxarg = (int)argcount;
        python_efun_entry->varargs = (flags & CO_VARARGS) ? true : false;

        defaults = PyFunction_GetDefaults(fun);
        if (defaults && PySequence_Check(defaults))
            python_efun_entry->minarg -= (int)PySequence_Length(defaults);

        /* And now look at annotations to get the types. */
        annotations = PyFunction_GetAnnotations(fun);
        if (!annotations || !PyMapping_Check(annotations))
            break;

        varnames = PyObject_GetAttrString(code, "co_varnames");
        if (!varnames || !PySequence_Check(varnames))
        {
            Py_XDECREF(varnames);
            break;
        }

        python_efun_entry->types = types = xalloc(sizeof(lpctype_t*) * (1 + argcount + ((flags & CO_VARARGS) ? 1 : 0)));

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
    } while (false);

    Py_XINCREF(fun);
    Py_INCREF(Py_None);
    PyErr_Clear();

    return Py_None;
} /* python_register_efun */

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
        xfree(python_efun_table[idx].types);
        python_efun_table[idx].callable = NULL;
        python_efun_table[idx].types = NULL;
    }

    Py_INCREF(Py_None);
    return Py_None;
} /* python_register_efun */

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
} /* python_register_socket */

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
} /* python_unregister_socket */

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
} /* python_register_hook */

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
} /* python_unregister_hook */

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

struct ldmud_object_s
{
    PyGCObject_HEAD

    object_t *lpc_object;       /* Can be NULL. */
};

struct ldmud_closure_s
{
    PyGCObject_HEAD

    svalue_t lpc_closure;       /* Can be T_INVALID. */
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

typedef struct ldmud_array_s ldmud_array_t;
typedef struct ldmud_mapping_s ldmud_mapping_t;
typedef struct ldmud_struct_s ldmud_struct_t;
typedef struct ldmud_object_s ldmud_object_t;
typedef struct ldmud_closure_s ldmud_closure_t;
typedef struct ldmud_symbol_s ldmud_symbol_t;
typedef struct ldmud_quoted_array_s ldmud_quoted_array_t;

/*-------------------------------------------------------------------------*/
/* GC Support */

static void
add_gc_object (ldmud_gc_var_t** list, ldmud_gc_var_t* var)

/* Add <var> to the <list>.
 */

{
    var->gcnext = *list;
    var->gcprev = NULL;
    if(*list != NULL)
    {
        assert((*list)->gcprev == NULL);
        (*list)->gcprev = var;
    }
    *list = var;
} /* add_gc_object */

/*-------------------------------------------------------------------------*/
static void
remove_gc_object (ldmud_gc_var_t** list, ldmud_gc_var_t* var)

/* Remove <var> from the <list>.
 */

{
    if (var->gcprev == NULL)
    {
        /* List start */
        assert(*list == var);

        *list = var->gcnext;
        if(*list)
            (*list)->gcprev = NULL;
    }
    else
    {
        assert(var->gcprev->gcnext == var);
        var->gcprev->gcnext = var->gcnext;
        if(var->gcnext)
            var->gcnext->gcprev = var->gcprev;
    }
} /* remove_gc_object */

/*-------------------------------------------------------------------------*/
/* Objects */

static void
ldmud_object_dealloc (ldmud_object_t* self)

/* Destroy the ldmud_object_t object
 */

{
    if(self->lpc_object)
        free_object(self->lpc_object, "ldmud_object_dealloc");

    remove_gc_object(&gc_object_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_object_dealloc */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_object_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implmenent __new__ for ldmud_object_t, i.e. allocate and initialize
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
} /* ldmud_object_new */

/*-------------------------------------------------------------------------*/
struct ldmud_object_init_closure_s
{
    string_t* filename;
    object_t* ob;
};

static void
ldmud_object_init_getobject(struct ldmud_object_init_closure_s* data)

/* Helper function for ldmud_object_init().
 */

{
    data->ob = get_object(data->filename);
} /* ldmud_object_init_getobject */

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
        call_lpc_secure((CClosureFun)ldmud_object_init_getobject, &data);
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
}

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_object_repr (ldmud_object_t *val)

/**
 * Return a string representation of this object.
 */

{
    if(!val->lpc_object)
        return PyUnicode_FromString("<LPC empty object>");
    return PyUnicode_FromFormat("<LPC object /%s>", get_txt(val->lpc_object->name));
}

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_object_hash (ldmud_object_t *val)

/**
 * Return a hash of this object.
 */

{
    return _Py_HashPointer(val->lpc_object);
}

/*-------------------------------------------------------------------------*/
static bool ldmud_object_check(PyObject *ob);

static PyObject*
ldmud_object_richcompare (ldmud_object_t *self, PyObject *other, int op)

/**
 * Compare <self> to <other> with the compare operation <op>.
 */

{
    object_t *self_ob, *other_ob;
    bool result;
    PyObject* resultval;

    if (!ldmud_object_check(other))
    {
        Py_INCREF(Py_NotImplemented);
        return Py_NotImplemented;
    }

    self_ob = self->lpc_object;
    other_ob = ((ldmud_object_t*)other)->lpc_object;

    if(self_ob == NULL && other_ob == NULL)
        result = op == Py_LE || op == Py_EQ || op == Py_GE;
    else if(self_ob == NULL)
        result = op == Py_LT || op == Py_LE || op == Py_NE;
    else if(other_ob == NULL)
        result = op == Py_GT || op == Py_GE || op == Py_NE;
    else
    {
        switch (op)
        {
            case Py_LT: result = mstring_compare(self_ob->name, other_ob->name) < 0; break;
            case Py_LE: result = mstring_compare(self_ob->name, other_ob->name) <= 0; break;
            case Py_EQ: result = self_ob == other_ob; break;
            case Py_NE: result = self_ob != other_ob; break;
            case Py_GT: result = mstring_compare(self_ob->name, other_ob->name) > 0; break;
            case Py_GE: result = mstring_compare(self_ob->name, other_ob->name) >= 0; break;
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
}

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_object_get_name(ldmud_object_t *val, void *closure)

/**
 * Return the value for the name member.
 */

{
    if(!val->lpc_object)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyUnicode_FromFormat("/%s", get_txt(val->lpc_object->name));
}

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_object_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_object_getset[] =
{
    {"name", (getter)ldmud_object_get_name,   NULL, NULL},
    {NULL}
};

static PyTypeObject ldmud_object_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Object",                     /* tp_name */
    sizeof(ldmud_object_t),             /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_object_dealloc,   /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    (reprfunc)ldmud_object_repr,        /* tp_repr */
    0,                                  /* tp_as_number */
    0,                                  /* tp_as_sequence */
    0,                                  /* tp_as_mapping */
    (hashfunc)ldmud_object_hash,        /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
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
};


/*-------------------------------------------------------------------------*/
static bool
ldmud_object_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC object type.
 */

{
    return Py_TYPE(ob) == &ldmud_object_type;
}

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_object_create (object_t* ob)

/* Creates a new Python object from an LPC object.
 */

{
    ldmud_object_t *self;

    self = (ldmud_object_t *)ldmud_object_type.tp_alloc(&ldmud_object_type, 0);
    if (self == NULL)
        return NULL;

    self->lpc_object = ref_object(ob, "ldmud_object_create");
    add_gc_object(&gc_object_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_object_create */

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
} /* ldmud_array_dealloc */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_array_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implmenent __new__ for ldmud_array_t, i.e. allocate and initialize
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
} /* ldmud_array_new */

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
} /* python_error_handler */
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
} /* ldmud_array_init */

/*-------------------------------------------------------------------------*/
static Py_ssize_t
ldmud_array_length (ldmud_array_t *val)

/* Implement len() for ldmud_array_t.
 */

{
    return VEC_SIZE(val->lpc_array);
} /* ldmud_array_length */

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
} /* ldmud_array_concat */

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
} /* ldmud_array_repeat */

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

    result = svalue_to_python(val->lpc_array->item + idx);
    if (result == NULL)
        PyErr_SetString(PyExc_ValueError, "Unsupported data type.");

    return result;
} /* ldmud_array_item */

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
} /* ldmud_array_ass_item */

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
} /* ldmud_array_contains */

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

} /* ldmud_array_subscript */

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
} /* ldmud_array_ass_item */

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



static PyTypeObject ldmud_array_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
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
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC array",                        /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
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
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_array_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC array type.
 */

{
    return Py_TYPE(ob) == &ldmud_array_type;
}

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_array_create (vector_t* vec)

/* Creates a new Python array from an LPC array.
 */

{
    ldmud_array_t *self;

    self = (ldmud_array_t *)ldmud_array_type.tp_alloc(&ldmud_array_type, 0);
    if (self == NULL)
        return NULL;

    if(vec != NULL)
        self->lpc_array = ref_array(vec);

    add_gc_object(&gc_array_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_object_create */

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
} /* ldmud_mapping_dealloc */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_iter_next (ldmud_mapping_list_t* self)
{
    PyObject *result = NULL;

    if (self->pos >= VEC_SIZE(self->indices))
        return NULL;

    switch (self->mode)
    {
        case MappingIterator_Keys:
            result = svalue_to_python(self->indices->item + self->pos);
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
                    result = svalue_to_python(values);
                else
                {
                    PyObject *val;

                    result = PyTuple_New(self->map->num_values + 1);
                    if (result == NULL)
                        return NULL;

                    val = svalue_to_python(self->indices->item + self->pos);
                    if (val == NULL)
                    {
                        Py_DECREF(result);
                        return NULL;
                    }

                    PyTuple_SET_ITEM(result, 0, val);
                    for (int i = 0; i < self->map->num_values; i++)
                    {
                        val = svalue_to_python(values + i);
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
} /* ldmud_mapping_iter_next */

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
} /* ldmud_object_create */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_list_iter (ldmud_mapping_list_t* self)

/* Creates an iterator for ourselves.
 */
{
    return ldmud_mapping_iter_create(self->map, self->indices, self->mode);
}

/*-------------------------------------------------------------------------*/
static Py_ssize_t
ldmud_mapping_list_length (ldmud_mapping_list_t *val)

/* Implement len() for ldmud_mapping_list_t.
 */

{
    return MAP_SIZE(val->map);
} /* ldmud_mapping_list_length */

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
} /* ldmud_mapping_list_contains_walker */

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

} /* ldmud_mapping_list_contains */


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
} /* ldmud_object_create */

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
} /* ldmud_mapping_dealloc */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_mapping_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implmenent __new__ for ldmud_mapping_t, i.e. allocate and initialize
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
} /* ldmud_mapping_new */

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
                    Py_DECREF(item);
                    PyErr_SetString(PyExc_ValueError, err);
                    break;
                }
            }

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
} /* ldmud_mapping_init */

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
} /* ldmud_mapping_iter */

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
} /* ldmud_mapping_contains */

/*-------------------------------------------------------------------------*/
static Py_ssize_t
ldmud_mapping_length (ldmud_mapping_t *val)

/* Implement len() for ldmud_mapping_t.
 */

{
    return MAP_SIZE(val->lpc_mapping);
} /* ldmud_mapping_length */

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
        if (second==NULL || !PyIndex_Check(second))
        {
            PyErr_Format(PyExc_IndexError, "second index should be an integer, not %.200s", second->ob_type->tp_name);
            return NULL;
        }

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
    else if (python_to_svalue(&sv, key) != NULL)
        /* If we can't convert, it's probably not in the mapping. */
        return PyLong_FromLong(0);

    values = get_map_value(val->lpc_mapping, &sv);
    free_svalue(&sv);

    if (values == &const0)
        return PyLong_FromLong(0);

    result = svalue_to_python(values + idx);
    if (result == NULL)
        PyErr_SetString(PyExc_ValueError, "Unsupported data type.");

    return result;
} /* ldmud_mapping_subscript */

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

        second = PyTuple_GetItem(key, 1);
        if (second==NULL || !PyIndex_Check(second))
        {
            PyErr_Format(PyExc_IndexError, "second index should be an integer, not %.200s", second->ob_type->tp_name);
            return -1;
        }

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
} /* ldmud_mapping_ass_item */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_keys (ldmud_mapping_t *self)

/* Returns a list of the keys.
 */

{
    return ldmud_mapping_list_create(self->lpc_mapping, MappingIterator_Keys);
} /* ldmud_mapping_keys */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_items (ldmud_mapping_t *self)

/* Returns a list of items.
 */

{
    return ldmud_mapping_list_create(self->lpc_mapping, MappingIterator_Items);
} /* ldmud_mapping_items */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_values (ldmud_mapping_t *self)

/* Returns a list of values.
 */

{
    return ldmud_mapping_list_create(self->lpc_mapping, MappingIterator_Values);
} /* ldmud_mapping_values */

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_mapping_get_width (ldmud_mapping_t *val, void *closure)

/**
 * Return the value for the width member.
 */

{
    return PyLong_FromLong(val->lpc_mapping->num_values);
} /* ldmud_mapping_get_width */
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


static PyTypeObject ldmud_mapping_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
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
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC mapping",                      /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
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
};

/*-------------------------------------------------------------------------*/
static bool
ldmud_mapping_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC mapping type.
 */

{
    return Py_TYPE(ob) == &ldmud_mapping_type;
}

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_mapping_create (mapping_t* map)

/* Creates a new Python mapping from an LPC mapping.
 */

{
    ldmud_mapping_t *self;

    self = (ldmud_mapping_t *)ldmud_mapping_type.tp_alloc(&ldmud_mapping_type, 0);
    if (self == NULL)
        return NULL;

    if (map != NULL)
        self->lpc_mapping = ref_mapping(map);

    add_gc_object(&gc_mapping_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_object_create */

/*-------------------------------------------------------------------------*/
/* Structs */

static void
ldmud_struct_dealloc (ldmud_struct_t* self)

/* Destroy the ldmud_struct_t object
 */

{
    if(self->lpc_struct)
        free_struct(self->lpc_struct);

    remove_gc_object(&gc_struct_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_struct_dealloc */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_struct_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implmenent __new__ for ldmud_struct_t, i.e. allocate and initialize
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
} /* ldmud_struct_new */

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
    struct_t *lpc_struct;
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

    /* Create the new struct. */
    lpc_struct = struct_new(lpc_struct_type);
    if(!lpc_struct)
    {
        PyErr_SetString(PyExc_MemoryError, "Out of memory");
        return -1;
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
                return -1;
            }

            iterator = PyObject_GetIter(items);
            Py_DECREF(items);

            if(iterator == NULL)
            {
                free_struct(lpc_struct);
                return -1;
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
                    PyObject *utf8;
                    Py_ssize_t member_length;
                    char * buf;
                    string_t *member_name;

                    utf8 = PyUnicode_AsEncodedString(key, "utf-8", "replace");
                    if (utf8 == NULL)
                    {
                        PyErr_SetString(PyExc_ValueError, "undecodable member name");
                        Py_DECREF(item);
                        break;
                    }

                    PyBytes_AsStringAndSize(utf8, &buf, &member_length);
                    member_name = find_tabled_str_n(buf, member_length, STRING_UTF8);
                    idx = member_name ? struct_find_member(lpc_struct_type, member_name) : -1;

                    if (idx < 0)
                    {
                        PyErr_Format(PyExc_ValueError, "member '%s' not found", buf);
                        Py_DECREF(utf8);
                        Py_DECREF(item);
                        break;
                    }

                    Py_DECREF(utf8);
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
                return -1;
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
            return -1;
        }
    }

    if(self->lpc_struct)
        free_struct(self->lpc_struct);
    self->lpc_struct = lpc_struct;

    return 0;
}

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_struct_getattr (ldmud_struct_t *obj, char *name)

/* Implement __getattr__ for ldmud_struct_t.
 * This is a direct struct member lookup.
 */

{
    string_t *member_name;
    int idx;
    PyObject *result;

    if(obj->lpc_struct == NULL)
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized struct object");
        return NULL;
    }

    member_name = find_tabled_str(name, STRING_UTF8);
    idx = member_name ? struct_find_member(obj->lpc_struct->type, member_name) : -1;

    if (idx < 0)
    {
        PyErr_Format(PyExc_AttributeError, "Struct object has no attribute '%.400s'", name);
        return NULL;
    }

    result = svalue_to_python(obj->lpc_struct->member + idx);
    if (result == NULL)
        PyErr_SetString(PyExc_ValueError, "Unsupported data type.");

    return result;
}

/*-------------------------------------------------------------------------*/
static int
ldmud_struct_setattr (ldmud_struct_t *obj, char *name, PyObject *value)

/* Implement __setattr__ for ldmud_struct_t.
 * This is a direct struct member lookup.
 */

{
    string_t *member_name;
    int idx;
    const char *err;

    if(obj->lpc_struct == NULL)
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized struct object");
        return -1;
    }

    member_name = find_tabled_str(name, STRING_UTF8);
    idx = struct_find_member(obj->lpc_struct->type, member_name);

    if (idx < 0)
    {
        PyErr_Format(PyExc_AttributeError, "Struct object has no attribute '%.400s'", name);
        return -1;
    }

    err = python_to_svalue(obj->lpc_struct->member + idx, value);
    if (err != NULL)
    {
        PyErr_SetString(PyExc_ValueError, err);
        return -1;
    }

    return 0;
}

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_struct_methods[] =
{
    {NULL}
};



static PyTypeObject ldmud_struct_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Struct",                     /* tp_name */
    sizeof(ldmud_struct_t),             /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_struct_dealloc,   /* tp_dealloc */
    0,                                  /* tp_print */
    (getattrfunc)ldmud_struct_getattr,  /* tp_getattr */
    (setattrfunc)ldmud_struct_setattr,  /* tp_setattr */
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
    "LPC struct",                       /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
    0,                                  /* tp_weaklistoffset */
    0,                                  /* tp_iter */
    0,                                  /* tp_iternext */
    ldmud_struct_methods,               /* tp_methods */
    0,                                  /* tp_members */
    0,                                  /* tp_getset */
    0,                                  /* tp_base */
    0,                                  /* tp_dict */
    0,                                  /* tp_descr_get */
    0,                                  /* tp_descr_set */
    0,                                  /* tp_dictoffset */
    (initproc)ldmud_struct_init,        /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_struct_new,                   /* tp_new */
};

/*-------------------------------------------------------------------------*/
/* Closures */

static void
ldmud_closure_dealloc (ldmud_closure_t* self)

/* Destroy the ldmud_closure_t object
 */

{
    free_svalue(&self->lpc_closure);

    remove_gc_object(&gc_closure_list, (ldmud_gc_var_t*)self);

    Py_TYPE(self)->tp_free((PyObject*)self);
} /* ldmud_closure_dealloc */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_closure_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implmenent __new__ for ldmud_closure_t, i.e. allocate and initialize
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
} /* ldmud_closure_new */

/*-------------------------------------------------------------------------*/
static int
ldmud_closure_init (ldmud_closure_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_closure_t, i.e. create a new closure object
 * from the given arguments.
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
    ldmud_object_t *bound_ob, *lfun_ob;

    lfun_ob = NULL;
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "O!s#|O!", kwlist,
                                      &ldmud_object_type, &bound_ob,
                                      &name, &length,
                                      &ldmud_object_type, &lfun_ob))
        return -1;

    if(bound_ob->lpc_object == NULL || (lfun_ob != NULL && lfun_ob->lpc_object == NULL))
    {
        PyErr_SetString(PyExc_TypeError, "uninitialized lpc object");
        return -1;
    }

    if(lfun_ob == NULL)
    {
        /* It shall be a (simul-)efun closure. */
        symbol_efun_str(name, length, &self->lpc_closure, OVERRIDE_NONE, true);
        if (self->lpc_closure.type != T_CLOSURE)
        {
            PyErr_Format(PyExc_NameError, "unknown symbol '%s'", name);
            return -1;
        }

        return 0;
    }
    else
    {
        object_t *prog_ob = lfun_ob->lpc_object;
        string_t *funname;
        int idx;

        if (O_PROG_SWAPPED(prog_ob) && load_ob_from_swap(prog_ob) < 0)
        {
            PyErr_SetString(PyExc_MemoryError, "out of memory while unswapping");
            return -1;
        }

        funname = find_tabled_str_n(name, length, STRING_UTF8);
        if(funname)
            idx = find_function(funname, prog_ob->prog);
        else
            idx = -1;

        if (idx < 0)
        {
            PyErr_Format(PyExc_NameError, "unknown function '%s' in '%s'", name, get_txt(prog_ob->prog->name));
            return -1;
        }

        closure_lfun(&self->lpc_closure, lfun_ob->lpc_object, NULL,(unsigned short)idx, 0, MY_FALSE);
        if (self->lpc_closure.type != T_CLOSURE)
        {
            PyErr_SetString(PyExc_MemoryError, "out of memory");
            return -1;
        }

        /* The lambda was bound to the wrong object */
        free_object(self->lpc_closure.u.lambda->ob, "ldmud_closure_init");
        self->lpc_closure.u.lambda->ob = ref_object(bound_ob->lpc_object, "ldmud_closure_init");

        return 0;
    }
}

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_closure_hash (ldmud_closure_t *val)

/**
 * Return a hash of this closure.
 */

{
    return _Py_HashPointer(val->lpc_closure.u.ob) ^ val->lpc_closure.x.closure_type;
}

/*-------------------------------------------------------------------------*/
static bool ldmud_closure_check(PyObject *ob);

static PyObject*
ldmud_closure_richcompare (ldmud_closure_t *self, PyObject *other, int op)

/**
 * Compare <self> to <other> with the compare operation <op>.
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
}

/*-------------------------------------------------------------------------*/

struct ldmud_closure_call_closure_s
{
    ldmud_closure_t *closure;
    int num_arg;
};

static void
ldmud_closure_call_closure(struct ldmud_closure_call_closure_s* data)

/* Helper function for ldmud_closure_call().
 */

{
    call_lambda(&data->closure->lpc_closure, data->num_arg);
} /* ldmud_closure_call_getobject */


static PyObject*
ldmud_closure_call(ldmud_closure_t *cl, PyObject *arg, PyObject *kw)
{
    if(!(python_is_external ? master_ob : current_object))
    {
        PyErr_SetString(PyExc_RuntimeError, "can't call a closure without a current object");
        return NULL;
    }

    if (kw != NULL && PyDict_Size(kw) != 0)
    {
        PyErr_SetString(PyExc_TypeError, "closure call takes no keyword arguments");
        return NULL;
    }
    else
    {
        svalue_t *sp = inter_sp;
        PyObject *result;
        struct ldmud_closure_call_closure_s data = { cl, (int)PyTuple_GET_SIZE(arg)};

        /* Put all arguments on the stack. */
        for (int i = 0; i < data.num_arg; i++)
        {
            const char* err = python_to_svalue(++sp, PyTuple_GetItem(arg, i));
            if (err != NULL)
            {
                PyErr_SetString(PyExc_ValueError, err);
                pop_n_elems(i, sp);
                return NULL;
            }
        }

        inter_sp = sp;

        if(call_lpc_secure((CClosureFun)ldmud_closure_call_closure, &data))
        {
            result = svalue_to_python(inter_sp);
            pop_stack();
        }
        else
            result = NULL;

        return result;
    }

    return NULL;
}

/*-------------------------------------------------------------------------*/
static PyMethodDef ldmud_closure_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_closure_getset[] =
{
    {NULL}
};

static PyTypeObject ldmud_closure_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.Closure",                    /* tp_name */
    sizeof(ldmud_closure_t),            /* tp_basicsize */
    0,                                  /* tp_itemsize */
    (destructor)ldmud_closure_dealloc,  /* tp_dealloc */
    0,                                  /* tp_print */
    0,                                  /* tp_getattr */
    0,                                  /* tp_setattr */
    0,                                  /* tp_reserved */
    0,                                  /* tp_repr */
    0,                                  /* tp_as_number */
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
    (initproc)ldmud_closure_init,       /* tp_init */
    0,                                  /* tp_alloc */
    ldmud_closure_new,                  /* tp_new */
};


/*-------------------------------------------------------------------------*/
static bool
ldmud_closure_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC closure type.
 */

{
    return Py_TYPE(ob) == &ldmud_closure_type;
}

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_closure_create (svalue_t* cl)

/* Creates a new Python closure from an LPC closure.
 */

{
    ldmud_closure_t *self;

    self = (ldmud_closure_t *)ldmud_closure_type.tp_alloc(&ldmud_closure_type, 0);
    if (self == NULL)
        return NULL;

    assign_svalue_no_free(&self->lpc_closure, cl);
    add_gc_object(&gc_closure_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_closure_create */

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
} /* ldmud_symbol_dealloc */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_symbol_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implmenent __new__ for ldmud_symbol_t, i.e. allocate and initialize
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
} /* ldmud_symbol_new */

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

    put_symbol(&self->lpc_symbol, str, quotes);
    return 0;
}

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_symbol_hash (ldmud_symbol_t *val)

/**
 * Return a hash of this symbol.
 */

{
    return _Py_HashPointer(val->lpc_symbol.u.str) ^ val->lpc_symbol.x.quotes;
}

/*-------------------------------------------------------------------------*/

static PyObject*
ldmud_symbol_richcompare (ldmud_symbol_t *self, PyObject *other, int op)

/**
 * Compare <self> to <other> with the compare operation <op>.
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
}

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_symbol_get_name(ldmud_symbol_t *val, void *closure)

/**
 * Return the value for the name member.
 */

{
    if (val->lpc_symbol.type != T_SYMBOL)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return PyUnicode_Decode(get_txt(val->lpc_symbol.u.str), mstrsize(val->lpc_symbol.u.str), "utf-8", "replace");
}

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_symbol_get_quotes(ldmud_symbol_t *val, void *closure)

/**
 * Return the value for the quotes member.
 */

{
    if (val->lpc_symbol.type != T_SYMBOL)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }
    return PyLong_FromLong(val->lpc_symbol.x.quotes);
}

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

static PyTypeObject ldmud_symbol_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
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
};


/*-------------------------------------------------------------------------*/
static bool
ldmud_symbol_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC symbol type.
 */

{
    return Py_TYPE(ob) == &ldmud_symbol_type;
}

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_symbol_create (svalue_t* sym)

/* Creates a new Python symbol from an LPC symbol.
 */

{
    ldmud_symbol_t *self;

    self = (ldmud_symbol_t *)ldmud_symbol_type.tp_alloc(&ldmud_symbol_type, 0);
    if (self == NULL)
        return NULL;

    assign_svalue_no_free(&self->lpc_symbol, sym);
    add_gc_object(&gc_symbol_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_symbol_create */

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
} /* ldmud_quoted_array_dealloc */

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_quoted_array_new (PyTypeObject *type, PyObject *args, PyObject *kwds)

/* Implmenent __new__ for ldmud_quoted_array_t, i.e. allocate and initialize
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
} /* ldmud_quoted_array_new */

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

    put_ref_array(&self->lpc_quoted_array, vec);
    self->lpc_quoted_array.type = T_QUOTED_ARRAY;
    self->lpc_quoted_array.x.quotes = quotes;

    return 0;
}

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_quoted_array_get_array(ldmud_quoted_array_t *val, void *closure)

/**
 * Return the value for the array member.
 */

{
    if (val->lpc_quoted_array.type != T_QUOTED_ARRAY)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }

    return ldmud_array_create(val->lpc_quoted_array.u.vec);
}

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_quoted_array_get_quotes(ldmud_quoted_array_t *val, void *closure)

/**
 * Return the value for the quotes member.
 */

{
    if (val->lpc_quoted_array.type != T_QUOTED_ARRAY)
    {
        Py_INCREF(Py_None);
        return Py_None;
    }
    return PyLong_FromLong(val->lpc_quoted_array.x.quotes);
}

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

static PyTypeObject ldmud_quoted_array_type =
{
    PyVarObject_HEAD_INIT(NULL, 0)
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
    0,                                  /* tp_hash  */
    0,                                  /* tp_call */
    0,                                  /* tp_str */
    0,                                  /* tp_getattro */
    0,                                  /* tp_setattro */
    0,                                  /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,                 /* tp_flags */
    "LPC quoted array",                 /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
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
};


/*-------------------------------------------------------------------------*/
static bool
ldmud_quoted_array_check (PyObject *ob)

/* Returns true, when <ob> is of the LPC quoted_array type.
 */

{
    return Py_TYPE(ob) == &ldmud_quoted_array_type;
}

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_quoted_array_create (svalue_t* sym)

/* Creates a new Python quoted_array from an LPC quoted_array.
 */

{
    ldmud_quoted_array_t *self;

    self = (ldmud_quoted_array_t *)ldmud_quoted_array_type.tp_alloc(&ldmud_quoted_array_type, 0);
    if (self == NULL)
        return NULL;

    assign_svalue_no_free(&self->lpc_quoted_array, sym);
    add_gc_object(&gc_quoted_array_list, (ldmud_gc_var_t*)self);

    return (PyObject *)self;
} /* ldmud_quoted_array_create */

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

struct ldmud_efun_s
{
    PyObject_HEAD

    int      efun_idx;
};

typedef struct ldmud_efun_s ldmud_efun_t;

/*-------------------------------------------------------------------------*/
static PyObject*
ldmud_efun_repr (ldmud_efun_t *val)

/**
 * Return a string representation of this efun.
 */

{
    return PyUnicode_FromFormat("<efun %s>", instrs[val->efun_idx].name);
}

/*-------------------------------------------------------------------------*/
static Py_hash_t
ldmud_efun_hash (ldmud_efun_t *val)

/**
 * Return a hash of this efun.
 */

{
    return _Py_HashPointer(instrs + val->efun_idx);
}

/*-------------------------------------------------------------------------*/

struct ldmud_efun_call_closure_s
{
    svalue_t efun_closure;
    int num_arg;
};

static void
ldmud_efun_call_efun(struct ldmud_efun_call_closure_s* data)

/* Helper function for ldmud_efun_call().
 */

{
    call_lambda(&data->efun_closure, data->num_arg);
} /* ldmud_efun_call_getobject */


static PyObject*
ldmud_efun_call(ldmud_efun_t *func, PyObject *arg, PyObject *kw)
{
    if(!(python_is_external ? master_ob : current_object))
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

        /* We construct a efun closure, so we don't need to duplicate
         * code from call_lambda.
         */
        struct ldmud_efun_call_closure_s data = { { T_CLOSURE}, (int)PyTuple_GET_SIZE(arg)};

        data.efun_closure.x.closure_type = (short)(func->efun_idx + CLOSURE_EFUN);
        if (python_is_external)
            python_restore_context();
        data.efun_closure.u.ob = current_object == NULL ? master_ob : current_object;

        /* Put all arguments on the stack. */
        for (int i = 0; i < data.num_arg; i++)
        {
            const char* err = python_to_svalue(++sp, PyTuple_GetItem(arg, i));
            if (err != NULL)
            {
                PyErr_SetString(PyExc_ValueError, err);
                pop_n_elems(i, sp);
                return NULL;
            }
        }

        inter_sp = sp;

        if(call_lpc_secure((CClosureFun)ldmud_efun_call_efun, &data))
        {
            result = svalue_to_python(inter_sp);
            pop_stack();
        }
        else
            result = NULL;

        return result;
    }

    return NULL;
}

/*-------------------------------------------------------------------------*/
static bool ldmud_efun_check(PyObject *ob);

static PyObject*
ldmud_efun_richcompare (ldmud_efun_t *self, PyObject *other, int op)

/**
 * Return a hash of this efun.
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
}

/*-------------------------------------------------------------------------*/
static PyObject *
ldmud_efun_get_name(ldmud_efun_t *efun, void *closure)

/**
 * Return the value for the name member.
 */

{
    return PyUnicode_FromString(instrs[efun->efun_idx].name);
}

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

static bool ldmud_efun_check(PyObject *ob)
{
    return Py_TYPE(ob) == &ldmud_efun_type;
}


static PyTypeObject ldmud_efun_namespace =
{
    PyVarObject_HEAD_INIT(NULL, 0)
    "ldmud.efuns",                      /* tp_name */
};

/*-------------------------------------------------------------------------*/
static PyObject *
create_efun_namespace()

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
        ldmud_efun_t *efun;
        PyObject *descr;

        efun = (ldmud_efun_t *)ldmud_efun_type.tp_alloc(&ldmud_efun_type, 0);
        if (efun == NULL)
            return NULL;

        efun->efun_idx = n;

        descr = PyStaticMethod_New((PyObject*)efun);
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
}


/*-------------------------------------------------------------------------*/
/* Module definition for the ldmud builtin module */

static PyObject*
ldmud_get_master (PyObject* self, PyObject* args UNUSED)

/* Return the master object or None.
 */

{
    if(master_ob == NULL)
        Py_RETURN_NONE;

    return ldmud_object_create(master_ob);
}

static PyObject*
ldmud_get_simul_efun (PyObject* self, PyObject* args UNUSED)

/* Return the simul_efun object or None.
 */

{
    if(simul_efun_object == NULL)
        Py_RETURN_NONE;

    return ldmud_object_create(simul_efun_object);
}

static PyMethodDef ldmud_methods[] =
{
    {
        "register_efun",
        (PyCFunction) python_register_efun, METH_VARARGS | METH_KEYWORDS,
        "register_efun(name, function) -> None\n\n"
        "Registers a new efun name. This is not allowed during\n"
        "compilation of an LPC object."
    },

    {
        "unregister_efun",
        (PyCFunction) python_unregister_efun, METH_VARARGS | METH_KEYWORDS,
        "unregister_efun(name) -> None\n\n"
        "Removes a python efun from registration. This is not allowed\n"
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
    PyModuleDef_HEAD_INIT, "ldmud",
    "This module provides access to the LDMud interpreter.",
    -1, ldmud_methods,
    NULL, NULL, NULL, NULL
};

static PyObject* init_ldmud_module()
{
    PyObject *module, *efuns;

    /* Initialize types. */
    if (PyType_Ready(&ldmud_object_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_array_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_mapping_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_mapping_list_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_mapping_iter_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_struct_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_closure_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_symbol_type) < 0)
        return NULL;
    if (PyType_Ready(&ldmud_quoted_array_type) < 0)
        return NULL;

    ldmud_interrupt_exception_type.tp_base = (PyTypeObject *) PyExc_RuntimeError;
    if (PyType_Ready(&ldmud_interrupt_exception_type) < 0)
        return NULL;

    /* Initialize module. */
    module = PyModule_Create(&ldmud_module);
    if (module == NULL)
        return module;

    /* Add types to module. */
    Py_INCREF(&ldmud_object_type);
    Py_INCREF(&ldmud_array_type);
    Py_INCREF(&ldmud_mapping_type);
    Py_INCREF(&ldmud_struct_type);
    Py_INCREF(&ldmud_closure_type);
    Py_INCREF(&ldmud_symbol_type);
    Py_INCREF(&ldmud_quoted_array_type);
    Py_INCREF(&ldmud_interrupt_exception_type);
    PyModule_AddObject(module, "Object", (PyObject*) &ldmud_object_type);
    PyModule_AddObject(module, "Array", (PyObject*) &ldmud_array_type);
    PyModule_AddObject(module, "Mapping", (PyObject*) &ldmud_mapping_type);
    PyModule_AddObject(module, "Struct", (PyObject*) &ldmud_struct_type);
    PyModule_AddObject(module, "Closure", (PyObject*) &ldmud_closure_type);
    PyModule_AddObject(module, "Symbol", (PyObject*) &ldmud_symbol_type);
    PyModule_AddObject(module, "QuotedArray", (PyObject*) &ldmud_quoted_array_type);
    PyModule_AddObject(module, "InterruptException", (PyObject*) &ldmud_interrupt_exception_type);

    assert(PYTHON_HOOK_COUNT == sizeof(python_hook_names) / sizeof(python_hook_names[0]));
    for (int i = 0; i < PYTHON_HOOK_COUNT; i++)
        PyModule_AddIntConstant(module, python_hook_names[i], i);

    /* Add the efuns as a sub-namespace. */
    efuns = create_efun_namespace();
    if (!efuns)
        return NULL;
    PyModule_AddObject(module, "efuns", efuns);

    return module;
}

/*=========================================================================*/

/*                          Helper functions                               */

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
    else if (PyType_Check(ptype))
    {
        /* Most of them are static type objects, so we don't
         * need to ref-count them.
         */
        if (PyObject_IsSubclass(ptype, (PyObject*) &ldmud_object_type))
            return lpctype_object;

        if (PyObject_IsSubclass(ptype, (PyObject*) &ldmud_array_type))
            return get_array_type(lpctype_mixed);

        if (PyObject_IsSubclass(ptype, (PyObject*) &ldmud_mapping_type))
            return lpctype_mapping;

        if (PyObject_IsSubclass(ptype, (PyObject*) &ldmud_struct_type))
            return lpctype_any_struct;

        if (PyObject_IsSubclass(ptype, (PyObject*) &ldmud_closure_type))
            return lpctype_closure;

        if (PyObject_IsSubclass(ptype, (PyObject*) &ldmud_symbol_type))
            return lpctype_symbol;

        if (PyObject_IsSubclass(ptype, (PyObject*) &ldmud_quoted_array_type))
            return lpctype_quoted_array;

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

} /* pythontype_to_lpctype */


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

        case T_MAPPING:
            return ldmud_mapping_create(svp->u.map);

        case T_CLOSURE:
            return ldmud_closure_create(svp);

        case T_SYMBOL:
            return ldmud_symbol_create(svp);

        case T_QUOTED_ARRAY:
            return ldmud_quoted_array_create(svp);

        case T_STRUCT:
        {
            PyObject* val = ldmud_struct_new(&ldmud_struct_type, NULL, NULL);
            if (val != NULL)
                ((ldmud_struct_t*)val)->lpc_struct = ref_struct(svp->u.strct);

            return val;
        }

        case T_LVALUE:
        {
            svalue_t* rval = get_rvalue(svp, NULL);

            if (rval != NULL)
                return svalue_to_python(svp);

            PyErr_SetString(PyExc_NotImplementedError, "lvalue ranges are not supported");
            return NULL;
        }

        default:
            PyErr_Format(PyExc_TypeError, "unsupported type %d", svp->type);
            return NULL;
    }

} /* svalue_to_python */


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
    if (PyObject_TypeCheck(val, &ldmud_object_type))
    {
        put_ref_object(dest, ((ldmud_object_t*)val)->lpc_object, "python_to_svalue");
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_array_type))
    {
        put_ref_array(dest, ((ldmud_array_t*)val)->lpc_array);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_mapping_type))
    {
        put_ref_mapping(dest, ((ldmud_mapping_t*)val)->lpc_mapping);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_struct_type))
    {
        put_ref_struct(dest, ((ldmud_struct_t*)val)->lpc_struct);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_closure_type))
    {
        assign_svalue_no_free(dest, &((ldmud_closure_t*)val)->lpc_closure);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_symbol_type))
    {
        assign_svalue_no_free(dest, &((ldmud_symbol_t*)val)->lpc_symbol);
        return NULL;
    }

    if (PyObject_TypeCheck(val, &ldmud_quoted_array_type))
    {
        assign_svalue_no_free(dest, &((ldmud_quoted_array_t*)val)->lpc_quoted_array);
        return NULL;
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
        PyObject *utf8;
        Py_ssize_t length;
        char * buf;
        string_t * str;

        utf8 = PyUnicode_AsEncodedString(val, "utf-8", "replace");
        if (utf8 == NULL)
            return "undecodable unicode string";

        PyBytes_AsStringAndSize(utf8, &buf, &length);
        str = new_n_unicode_mstring(buf, length);
        Py_DECREF(utf8);

        if (str == NULL)
            return "out of memory";
        put_string(dest, str);
        return NULL;
    }

    return "unknown type";

} /* python_to_svalue */

/*-------------------------------------------------------------------------*/


static bool
python_eq_svalue (PyObject* pval, svalue_t *sval)

/* Checks a LPC value with a Python value for equality.
 */

{
    if (pval == Py_None)
        return false;

    /* First we check our own types. */
    if (PyObject_TypeCheck(pval, &ldmud_object_type))
    {
        if (sval->type == T_OBJECT)
            return sval->u.ob == ((ldmud_object_t*)pval)->lpc_object;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_array_type))
    {
        if (sval->type == T_POINTER)
            return sval->u.vec == ((ldmud_array_t*)pval)->lpc_array;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_mapping_type))
    {
        if (sval->type == T_MAPPING)
            return sval->u.map == ((ldmud_mapping_t*)pval)->lpc_mapping;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_struct_type))
    {
        if (sval->type == T_STRUCT)
            return sval->u.strct == ((ldmud_struct_t*)pval)->lpc_struct;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_closure_type))
    {
        if (sval->type == T_CLOSURE)
            return closure_cmp(sval, &((ldmud_closure_t*)pval)->lpc_closure) == 0;
        else
            return false;
    }

    if (PyObject_TypeCheck(pval, &ldmud_symbol_type))
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

    if (PyObject_TypeCheck(pval, &ldmud_quoted_array_type))
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

} /* python_eq_svalue */

/*-------------------------------------------------------------------------*/
static bool
call_lpc_secure (CClosureFun fun, void* data)

/* Call <fun>(<data>), but guard it against LPC errors.
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

    save_sp = inter_sp;
    save_csp = csp;

    if (setjmp(error_recovery_info.con.text))
    {
        PyErr_SetString(PyExc_RuntimeError, get_txt(current_error));
        secure_apply_error(save_sp, save_csp, python_is_external);
    }
    else
    {
        object_t *save_ob = current_object;

        if(python_is_external)
        {
            /* We do externally called python code
             * in the context of the master ob.
             */
            python_restore_context();
            if (!current_object)
                current_object = master_ob;
            mark_start_evaluation();
        }

        (*fun)(data);
        result = true;

        if(python_is_external)
        {
            mark_end_evaluation();
            current_object = save_ob;
        }
    }

    rt_context = error_recovery_info.rt.last;

    return result;
} /* call_lpc_secure */

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
#endif

/*-------------------------------------------------------------------------*/
static void
python_save_context ()

/* Save the current context (current object, current command giver) into
 * the corresponding Python context variables.
 */

{
#ifdef USE_PYTHON_CONTEXT
    python_save_contextvar_object(&python_contextvar_current_object, "ldmud.current_object", current_object);
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
    python_restore_contextvar_object(python_contextvar_current_object, &current_object);
    python_restore_contextvar_object(python_contextvar_command_giver, &command_giver);
#else
    current_object = NULL;
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
    FILE *script_file;

    /** Python3 requires now wchar_t?!
     * Py_SetProgramName(prog_name);
     */

    PyImport_AppendInittab("ldmud", &init_ldmud_module);
    Py_Initialize();

    script_file = fopen(python_startup_script, "rt");
    if(script_file != NULL)
    {
        PyCompilerFlags flags;
        flags.cf_flags = 0;
        PyRun_SimpleFileExFlags(script_file, python_startup_script, 1, &flags);
    }
} /* pkg_python_init */


/*-------------------------------------------------------------------------*/


bool
is_python_efun (ident_t *p)

/* Returns true, if <p> is still a registered python efun.
 * When a python efun is unregistered, we'll leave the python_efun
 * index in the identifier intact, so re-registering the same efun
 * can reuse the same index.
 */

{
    return p->u.global.python_efun >= 0 && python_efun_table[p->u.global.python_efun].callable != NULL;
} /* is_python_efun */


/*-------------------------------------------------------------------------*/

lpctype_t *
check_python_efun_args (ident_t *p, int num_arg, fulltype_t *args)

/* Check whether the given argument types match the ones
 * the Python efun expects. Any errors will be printed using yyerrorf().
 * This function returns the return type of the efun.
 */

{
    python_efun_t * entry = python_efun_table + p->u.global.python_efun;

    if (num_arg < entry->minarg)
        yyerrorf("Too few arguments to %s", get_txt(p->name));
    else if(!entry->varargs && num_arg > entry->maxarg)
    {
        yyerrorf("Too many arguments to %s", get_txt(p->name));
        num_arg = entry->maxarg;
    }

    if (!entry->types)
        return lpctype_mixed;

    for (int pos = 0; pos < num_arg; pos++)
    {
        lpctype_t* expected;

        if (pos >= entry->maxarg)
            expected = entry->types[1 + entry->maxarg];
        else
            expected = entry->types[1 + pos];

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

    if (entry->types[0])
        return entry->types[0];
    return lpctype_mixed;

} /* check_python_efun_args */

/*-------------------------------------------------------------------------*/


void
call_python_efun (int idx, int num_arg)

/* Call the python-defined efun <idx> with <num_arg> arguments on the stack.
 * This function removes the arguments from the stack and leaves the result
 * there.
 */

{
    PyObject *result, *args;
    int pos;
    svalue_t *argp;
    bool was_external = python_is_external;
    python_efun_t* python_efun_entry = python_efun_table + idx;

    /* Efun still registered?
     * (F_PYTHON_EFUN opcodes may still be floating around.)
     */
    if (python_efun_entry->callable == NULL)
        errorf("Python-defined efun vanished: %s\n"
             , get_txt(python_efun_entry->name->name));

    /* We leave the argument count check to Python.
     * But we check the types if there are annotations about it.
     */
    args = num_arg ? PyTuple_New(num_arg) : NULL;
    argp = inter_sp - num_arg + 1;
    for (pos = 0; pos < num_arg; pos++,argp++)
    {
        PyObject *arg;

        if (python_efun_entry->types)
        {
            lpctype_t *expected;
            if (pos < python_efun_entry->maxarg)
                expected = python_efun_entry->types[1 + pos];
            else if (python_efun_entry->varargs)
                expected = python_efun_entry->types[1 + python_efun_entry->maxarg];
            else
                expected = NULL;

            if (expected && !check_rtt_compatibility(expected, argp))
            {
                static char buff[512];
                lpctype_t *realtype = get_rtt_type(expected, argp);
                get_lpctype_name_buf(realtype, buff, sizeof(buff));
                free_lpctype(realtype);

                Py_DECREF(args);

                errorf("Bad arg %d to %s(): got '%s', expected '%s'.\n"
                      , pos + 1, get_txt(python_efun_entry->name->name)
                      , buff, get_lpctype_name(expected));
            }
        }

        arg = svalue_to_python(argp);
        if (arg == NULL)
        {
            Py_DECREF(args);

            errorf("Bad argument %d to %s().\n"
                 , pos+1
                 , get_txt(python_efun_entry->name->name));
        }
        PyTuple_SET_ITEM(args, pos, arg);
    }
    inter_sp = pop_n_elems(num_arg, inter_sp);

    python_is_external = false;
    python_save_context();
    result = PyObject_CallObject(python_efun_entry->callable, args);
    python_is_external = was_external;
    Py_XDECREF(args);

    if (result == NULL)
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

        errorf("%s: %s\n"
             , get_txt(python_efun_entry->name->name)
             , msg);
    }
    else
    {
        const char *err = python_to_svalue(inter_sp + 1, result);
        Py_DECREF(result);

        if (err != NULL)
        {
            errorf("Bad return value from %s(): %s\n"
                  , get_txt(python_efun_entry->name->name)
                  , err);
        }

        inter_sp++;
    }
} /* call_python_efun */


/*-------------------------------------------------------------------------*/
void
python_set_fds (fd_set *readfds, fd_set *writefds, fd_set *exceptfds, int *nfds)

/* Set all file descriptors that some python routines are waiting for.
 */

{
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
} /* python_set_fds */

/*-------------------------------------------------------------------------*/
void
python_handle_fds (fd_set *readfds, fd_set *writefds, fd_set *exceptfds, int nfds)

/* File descriptors in the given sets have events.
 * Check whether a callable is waiting for it, then call it.
 */

{
    int num_cbs = 0, pos_cbs = 0;
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
} /* python_handle_fds */

/*-------------------------------------------------------------------------*/
void
python_call_hook (int hook, bool is_external)

/* Call the python <hook> without any arguments.
 */

{
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

    python_is_external = was_external;
} /* python_call_hook */

/*-------------------------------------------------------------------------*/
void
python_call_hook_object (int hook, bool is_external, object_t *ob)

/* Call the python <hook> with an object as its only argument.
 */

{
    bool was_external = python_is_external;
    PyObject *args, *arg;
    if (python_hooks[hook] == NULL)
        return;

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
        return;
    }

    arg = ldmud_object_create(ob);
    if (arg == NULL)
    {
        PyErr_Clear();
        Py_DECREF(args);
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

    python_is_external = was_external;
} /* python_call_hook_object */

/*-------------------------------------------------------------------------*/
int python_process_interrupt (void* arg UNUSED)

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
} /* python_process_interrupt */

/*-------------------------------------------------------------------------*/
void python_interrupt ()

/* Called from the signal handler (!)
 * to cause an interrupt in the current running python efun.
 */

{
    Py_AddPendingCall(&python_process_interrupt, NULL);
} /* python_interrupt */

/*-------------------------------------------------------------------------*/
void
python_handle_sigchld ()

/* Called from the SIGCHLD signal handler.
 * We will remember that and handle it in the backend loop.
 */

{
    comm_return_to_backend = true;
    python_pending_sigchld = true;
} /* python_handle_sigchld */

/*-------------------------------------------------------------------------*/
void
python_process_pending_jobs ()

{
    if (python_pending_sigchld)
    {
        python_pending_sigchld = false;
        python_call_hook(PYTHON_HOOK_ON_SIGCHLD, true);
    }
} /* python_handle_sigchld */

/*-------------------------------------------------------------------------*/
const char*
closure_python_efun_to_string (int type)

/* <type> is the code for a closure python-efun (the caller has to make
 * sure of that). Returns the name of that python-defined efun.
 */

{
    return get_txt(python_efun_table[type - CLOSURE_PYTHON_EFUN].name->name);
}

#ifdef GC_SUPPORT

/*-------------------------------------------------------------------------*/
void
python_clear_refs ()

/* GC Support: Clear all reference counts.
 */

{
    for (int idx = 0; idx < PYTHON_EFUN_TABLE_SIZE; idx++)
    {
        if (python_efun_table[idx].types)
            for (int pos = 0; pos < 1 + python_efun_table[idx].maxarg + (python_efun_table[idx].varargs ? 1 : 0); pos++)
                clear_lpctype_ref(python_efun_table[idx].types[pos]);
    }

    for(ldmud_gc_var_t* var = gc_object_list; var != NULL; var = var->gcnext)
    {
        object_t* ob = ((ldmud_object_t*)var)->lpc_object;
        if(ob != NULL)
            clear_object_ref(ob);
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

    for(ldmud_gc_var_t* var = gc_symbol_list; var != NULL; var = var->gcnext)
    {
        clear_ref_in_vector(&((ldmud_symbol_t*)var)->lpc_symbol, 1);
    }

    for(ldmud_gc_var_t* var = gc_quoted_array_list; var != NULL; var = var->gcnext)
    {
        clear_ref_in_vector(&((ldmud_quoted_array_t*)var)->lpc_quoted_array, 1);
    }
} /* python_clear_refs() */

/*-------------------------------------------------------------------------*/
void
python_count_refs ()

/* GC Support: Mark all references to xalloc'ed memory.
 */

{
    /* The efun table */
    for (int idx = 0; idx < PYTHON_EFUN_TABLE_SIZE; idx++)
    {
        if (python_efun_table[idx].types)
        {
            note_malloced_block_ref(python_efun_table[idx].types);

            for (int pos = 0; pos < 1 + python_efun_table[idx].maxarg + (python_efun_table[idx].varargs ? 1 : 0); pos++)
                count_lpctype_ref(python_efun_table[idx].types[pos]);
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

    for(ldmud_gc_var_t* var = gc_symbol_list; var != NULL; var = var->gcnext)
    {
        count_ref_in_vector(&((ldmud_symbol_t*)var)->lpc_symbol, 1);
    }

    for(ldmud_gc_var_t* var = gc_quoted_array_list; var != NULL; var = var->gcnext)
    {
        count_ref_in_vector(&((ldmud_quoted_array_t*)var)->lpc_quoted_array, 1);
    }
} /* python_count_refs() */

#endif /* GC_SUPPORT */

#ifdef DEBUG
void
count_python_extra_refs ()

/* Refcount Debugging: Count the refcounts in objects and arrays.
 */

{
    /* All Python objects that reference LPC data. */
    for(ldmud_gc_var_t* var = gc_object_list; var != NULL; var = var->gcnext)
    {
        object_t* ob = ((ldmud_object_t*)var)->lpc_object;
        if(ob != NULL)
            count_extra_ref_in_object(ob);
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

    for(ldmud_gc_var_t* var = gc_symbol_list; var != NULL; var = var->gcnext)
    {
        count_extra_ref_in_vector(&((ldmud_symbol_t*)var)->lpc_symbol, 1);
    }

    for(ldmud_gc_var_t* var = gc_quoted_array_list; var != NULL; var = var->gcnext)
    {
        count_extra_ref_in_vector(&((ldmud_quoted_array_t*)var)->lpc_quoted_array, 1);
    }
} /* count_python_extra_refs() */

#endif /* DEBUG */

#endif /* USE_PYTHON && HAS_PYTHON3 */
