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

#include "exec.h"
#include "instrs.h"
#include "interpret.h"
#include "lex.h"
#include "mstrings.h"
#include "object.h"
#include "pkg-python.h"
#include "prolang.h"
#include "simulate.h"
#include "structs.h"
#include "typedefs.h"

/* Python.h defines these again... */
#undef _GNU_SOURCE
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE

#define PY_SSIZE_T_CLEAN
#include "Python.h"

/* --- Variables --- */
char * python_startup_script = NULL;

int num_python_efun = 0;

static PyObject* python_efun_table[PYTHON_EFUN_TABLE_SIZE];
  /* Python callables for all defined efuns. 
   */

static ident_t*  python_efun_names[PYTHON_EFUN_TABLE_SIZE];
  /* For each python-defined efun store its identifier
   * here, so we can reverse lookup python efun indices.
   */

/* -- Function prototypes --- */
static const char* python_to_svalue(svalue_t *dest, PyObject* val);
static PyObject* svalue_to_python (svalue_t *svp);

/* -- Python definitions and functions --- */

static PyObject* python_register_efun(PyObject *module, PyObject *args)
{
    char *name;
    PyObject *fun;
    ident_t *ident;

    if (!PyArg_ParseTuple(args, "sO:register_efun", &name, &fun))
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
    }

    /* There is higher level identifier?
     * Should only happen during compile time, and that we forbid.
     */
    if (ident->type != I_TYPE_GLOBAL)
    {
        PyErr_SetString(PyExc_RuntimeError, "couldn't create efun entry");
        return NULL;
    }

    /* This is or once was a python efun? */
    if (ident->u.global.python_efun != I_GLOBAL_PYTHON_EFUN_OTHER)
    {
        int idx = ident->u.global.python_efun;

        Py_XDECREF(python_efun_table[idx]);
        python_efun_table[idx] = fun;
        /* No need for python_efun_names, because the ident_t didn't change. */
    }
    else if(num_python_efun == PYTHON_EFUN_TABLE_SIZE)
    {
        PyErr_SetString(PyExc_RuntimeError, "too many efuns registered");
        return NULL;
    }
    else
    {
        ident->u.global.python_efun = (short)num_python_efun;
        python_efun_table[num_python_efun] = fun;
        python_efun_names[num_python_efun] = ident;
        num_python_efun++;
    }

    Py_XINCREF(fun);
    Py_INCREF(Py_None);
    return Py_None;
}

/*=========================================================================*/

/*                                 Types                                   */

/*-------------------------------------------------------------------------*/
/* Type structures */

struct ldmud_struct_s
{
    PyObject_HEAD

    struct_t *lpc_struct;
};

struct ldmud_object_s
{
    PyObject_HEAD

    object_t *lpc_object;
};

typedef struct ldmud_struct_s ldmud_struct_t;
typedef struct ldmud_object_s ldmud_object_t;

/*-------------------------------------------------------------------------*/
/* Objects */

static void
ldmud_object_dealloc (ldmud_object_t* self)

/* Destroy the ldmud_object_t object
 */

{
    if(self->lpc_object)
        free_object(self->lpc_object, "ldmud_object_dealloc");

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
    if (self != NULL)
        self->lpc_object = NULL;

    return (PyObject *)self;
} /* ldmud_object_new */

/*-------------------------------------------------------------------------*/
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

    string_t *filename_str;

    if (! PyArg_ParseTupleAndKeywords(args, kwds, "s#", kwlist, &filename, &length))
        return -1;

    filename_str = new_n_mstring(filename, length);
    if(!filename_str)
    {
        PyErr_SetString(PyExc_MemoryError, "out of memory");
        return -1;
    }
    else
    {
        /* get_object() can throw errors, we don't want to jump out of this context. */

        /* TODO: We need to detect whether this is an external or internal call,
         * TODO:: so we can set the runtime limits accordingly, also secure_apply_error
         * TODO:: needs to know for the same reason.
         */
        struct error_recovery_info error_recovery_info;
        struct control_stack *save_csp;
        svalue_t *save_sp;
        object_t *ob;

        error_recovery_info.rt.last = rt_context;
        error_recovery_info.rt.type = ERROR_RECOVERY_APPLY;
        rt_context = (rt_context_t *)&error_recovery_info;

        save_sp = inter_sp;
        save_csp = csp;

        if (setjmp(error_recovery_info.con.text))
        {
            PyErr_SetString(PyExc_RuntimeError, get_txt(current_error));
            secure_apply_error(save_sp, save_csp, MY_FALSE);

            ob = NULL;
        }
        else
        {
            ob = get_object(filename_str);
        }

        rt_context = error_recovery_info.rt.last;
        free_mstring(filename_str);

        if(ob)
        {
            if(self->lpc_object)
                deref_object(self->lpc_object, "ldmud_object_init");
            self->lpc_object = ref_object(ob, "ldmud_object_init");
            return 0;
        }
        else
            return -1;
    }
}

static PyMethodDef ldmud_object_methods[] =
{
    {NULL}
};

static PyGetSetDef ldmud_object_getset[] =
{
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
    "LPC object",                       /* tp_doc */
    0,                                  /* tp_traverse */
    0,                                  /* tp_clear */
    0,                                  /* tp_richcompare */
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
/* Structs */

static void
ldmud_struct_dealloc (ldmud_struct_t* self)

/* Destroy the ldmud_struct_t object
 */

{
    if(self->lpc_struct)
        free_struct(self->lpc_struct);

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
    if (self != NULL)
        self->lpc_struct = NULL;

    return (PyObject *)self;
} /* ldmud_struct_new */

/*-------------------------------------------------------------------------*/
static int
ldmud_struct_init (ldmud_struct_t *self, PyObject *args, PyObject *kwds)

/* Implement __init__ for ldmud_stuct_t, i.e. create a new struct object
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

    /* Lookup the struct type. */
    /* TODO: This lookup also includes NAME_HIDDEN entries, maybe only consider visible structs. */
    lpc_struct_name = new_n_mstring(name, length);
    lpc_struct_type = struct_find(lpc_struct_name, lpc_ob->prog);
    free_mstring(lpc_struct_name);

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
                    member_name = new_n_mstring(buf, member_length);
                    idx = struct_find_member(lpc_struct_type, member_name);
                    free_mstring(member_name);

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

    member_name = new_mstring(name);
    idx = struct_find_member(obj->lpc_struct->type, member_name);
    free_mstring(member_name);

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

    member_name = new_mstring(name);
    idx = struct_find_member(obj->lpc_struct->type, member_name);
    free_mstring(member_name);

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
extern svalue_t *inter_fp;
#ifdef USE_NEW_INLINES
extern svalue_t *inter_context;
#endif

static PyObject*
ldmud_efun_call(ldmud_efun_t *func, PyObject *arg, PyObject *kw)
{
    if(!current_object)
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
        /* So we need to construct some LPC bytecode. */
        bytecode_t code[9];
        bytecode_p p = code;
        svalue_t *sp = inter_sp;
        int efun_idx = func->efun_idx;
        instr_t *def = instrs + func->efun_idx;

        int min_arg = def->min_arg;
        int max_arg = def->max_arg;
        int num_arg = (int)PyTuple_GET_SIZE(arg);

        if (max_arg == -1)
            max_arg = 0xff;

        if (num_arg < min_arg)
        {
            int def_argidx = def->Default;
            if (num_arg == min_arg - 1 && def_argidx >= 0)
            {
                /* We lack one argument for which a default
                 * is provided.
                 */
                if (instrs[def_argidx].prefix)
                    *p++ = instrs[def_argidx].prefix;
                *p++ = instrs[def_argidx].opcode;
            }
            else
            {
                int replacement = proxy_efun(func->efun_idx, num_arg);
                if (replacement >= 0)
                {
                    efun_idx = replacement;
                    def = instrs + replacement;
                }
                else
                {
                    PyErr_Format(PyExc_TypeError, "%.200s() takes at least %d arguments (%d given)",
                        def->name, min_arg, num_arg);
                    return NULL;
                }
            }
        }
        else if(num_arg > max_arg)
        {
            PyErr_Format(PyExc_TypeError, "%.200s() takes at most %d arguments (%d given)",
                def->name, max_arg, num_arg);
            return NULL;
        }

        /* Store the instruction code */
        if (def->prefix)
            *p++ = def->prefix;
        *p++ = def->opcode;

        /* And finally the return instruction */
        if ( def->ret_type == lpctype_void )
            *p++ = F_RETURN0;
        else
            *p++ = F_RETURN;

        /* We have a go, put all arguments on the stack. */
        for (int i = 0; i < num_arg; i++)
        {
            const char* err = python_to_svalue(++sp, PyTuple_GetItem(arg, i));
            if (err != NULL)
            {
                PyErr_SetString(PyExc_ValueError, err);
                pop_n_elems(i, sp);
                return NULL;
            }
        }

        /* Now we have to prepare everything for that return to work. */
        {
            struct error_recovery_info error_recovery_info;
            struct control_stack *save_csp = csp;
            svalue_t *save_sp = inter_sp;
            PyObject* result;

            error_recovery_info.rt.last = rt_context;
            error_recovery_info.rt.type = ERROR_RECOVERY_APPLY;
            rt_context = (rt_context_t *)&error_recovery_info;

#ifdef USE_NEW_INLINES
            push_control_stack(sp, inter_pc, inter_fp, inter_context);
#else
            push_control_stack(sp, inter_pc, inter_fp);
#endif /* USE_NEW_INLINES */

            csp->ob = current_object;
            csp->prev_ob = previous_ob;
            csp->instruction = efun_idx;
            csp->funstart = EFUN_FUNSTART;
            csp->num_local_variables = 0;
            previous_ob = current_object;
            inter_fp = sp - num_arg + 1;
#ifdef USE_NEW_INLINES
            inter_context = NULL;
#endif /* USE_NEW_INLINES */
            tracedepth++;

            if (setjmp(error_recovery_info.con.text))
            {
                PyErr_SetString(PyExc_RuntimeError, get_txt(current_error));
                secure_apply_error(save_sp, save_csp, MY_FALSE);
                result = NULL;
            }
            else
            {
                eval_instruction(code, sp);

                /* The result is on the stack (inter_sp) */
                result = svalue_to_python(inter_sp);
                pop_stack();
            }
            rt_context = error_recovery_info.rt.last;
            return result;
        }
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
ldmud_efun_get__name__(ldmud_efun_t *efun, void *closure)

/**
 * Return the value for the __name__ member.
 */

{
    return PyUnicode_FromString(instrs[efun->efun_idx].name);
}

/*-------------------------------------------------------------------------*/

static PyGetSetDef ldmud_efun_getset [] = {
    {"__name__", (getter)ldmud_efun_get__name__, NULL, NULL},
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
static PyMethodDef ldmud_methods[] =
{
    {
        "register_efun", python_register_efun, METH_VARARGS,
        "register_efun(name, function) -> None\n\n"
        "Registers a new efun name. This is not allowed during\n"
        "compilation of an LPC object."
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
    if (PyType_Ready(&ldmud_struct_type) < 0)
        return NULL;

    /* Initialize module. */
    module = PyModule_Create(&ldmud_module);
    if (module == NULL)
        return module;

    /* Add types to module. */
    Py_INCREF(&ldmud_object_type);
    Py_INCREF(&ldmud_struct_type);
    PyModule_AddObject(module, "Object", (PyObject*) &ldmud_object_type);
    PyModule_AddObject(module, "Struct", (PyObject*) &ldmud_struct_type);

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

static PyObject*
svalue_to_python (svalue_t *svp)

/* Creates a python value from an svalue_t.
 */

{
    switch (svp->type)
    {
        case T_LVALUE:
            // TODO
            return NULL;

        case T_NUMBER:
            return PyLong_FromLong(svp->u.number);

        case T_FLOAT:
            return PyFloat_FromDouble(READ_DOUBLE(svp));

        case T_STRING:
            return PyUnicode_Decode(get_txt(svp->u.str), mstrsize(svp->u.str), "utf-8", "replace");

        case T_POINTER:
            // TODO
            return NULL;

        case T_OBJECT:
        {
            PyObject* val = ldmud_object_new(&ldmud_object_type, NULL, NULL);
            if (val != NULL)
                ((ldmud_object_t*)val)->lpc_object = ref_object(svp->u.ob, "svalue_to_python");

            return val;
        }

        case T_MAPPING:
            // TODO
            return NULL;

        case T_CLOSURE:
            // TODO
            return NULL;

        case T_SYMBOL:
            // TODO
            return NULL;

        case T_QUOTED_ARRAY:
            // TODO
            return NULL;

        case T_STRUCT:
        {
            PyObject* val = ldmud_struct_new(&ldmud_struct_type, NULL, NULL);
            if (val != NULL)
                ((ldmud_struct_t*)val)->lpc_struct = ref_struct(svp->u.strct);

            return val;
        }

        case T_CHAR_LVALUE:
            // TODO
            return NULL;

        default:
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

    if (PyObject_TypeCheck(val, &ldmud_struct_type))
    {
        put_ref_struct(dest, ((ldmud_struct_t*)val)->lpc_struct);
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

        PyBytes_AsStringAndSize(val, &buf, &length);
        put_c_n_string(dest, buf, length);
        return NULL;
    }

    if (PyUnicode_Check(val))
    {
        PyObject *utf8;
        Py_ssize_t length;
        char * buf;

        utf8 = PyUnicode_AsEncodedString(val, "utf-8", "replace");
        if (utf8 == NULL)
            return "undecodable unicode string";

        PyBytes_AsStringAndSize(utf8, &buf, &length);
        put_c_n_string(dest, buf, length);
        Py_DECREF(utf8);
        return NULL;
    }

    return "unknown type";

} /* python_to_svalue */


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
    PyCompilerFlags flags;

    /** Python3 requires now wchar_t?!
     * Py_SetProgramName(prog_name);
     */

    PyImport_AppendInittab("ldmud", &init_ldmud_module);
    Py_Initialize();

    script_file = fopen(python_startup_script, "rt");
    flags.cf_flags = 0;
    PyRun_SimpleFileExFlags(script_file, python_startup_script, 1, &flags);
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
    return p->u.global.python_efun >= 0 && python_efun_table[p->u.global.python_efun] != NULL;
} /* is_python_efun */


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

    /* Efun still registered?
     * (F_PYTHON_EFUN opcodes may still be floating around.)
     */
    if (python_efun_table[idx] == NULL)
        errorf("Python-defined efun vanished: %s\n"
             , get_txt(python_efun_names[idx]->name));

    args = num_arg ? PyTuple_New(num_arg) : NULL;
    argp = inter_sp - num_arg + 1;
    for (pos = 0; pos < num_arg; pos++,argp++)
    {
        PyObject *arg = svalue_to_python(argp);
        if (arg == NULL)
        {
            Py_DECREF(args);

            errorf("Bad argument %d to %s().\n"
                 , pos+1
                 , get_txt(python_efun_names[idx]->name));
        }
        PyTuple_SET_ITEM(args, pos, arg);
    }
    inter_sp = pop_n_elems(num_arg, inter_sp);

    result = PyObject_CallObject(python_efun_table[idx], args);
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
             , get_txt(python_efun_names[idx]->name)
             , msg);
    }
    else
    {
        const char *err = python_to_svalue(inter_sp + 1, result);
        Py_DECREF(result);

        if (err != NULL)
        {
            errorf("Bad return value from %s(): %s\n"
                  , get_txt(python_efun_names[idx]->name)
                  , err);
        }

        inter_sp++;
    }
} /* call_python_efun */


/*-------------------------------------------------------------------------*/


const char*
closure_python_efun_to_string (int type)

/* <type> is the code for a closure python-efun (the caller has to make
 * sure of that). Returns the name of that python-defined efun.
 */

{
    return get_txt(python_efun_names[type - CLOSURE_PYTHON_EFUN]->name);
}

// TODO: Add support for garbage collection.
// TODO:: We need to keep a root set.

// TODO: Add master and simul-efun objects as members to the ldmud module

#endif /* USE_PYTHON && HAS_PYTHON3 */
