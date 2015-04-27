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

#include "interpret.h"
#include "lex.h"
#include "mstrings.h"
#include "pkg-python.h"
#include "simulate.h"
#include "typedefs.h"

/* Python.h defines these again... */
#undef _GNU_SOURCE
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE
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
        PyErr_SetString(PyExc_MemoryError, "Out of memory");
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
        PyErr_SetString(PyExc_RuntimeError, "Couldn't create efun entry.");
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
        PyErr_SetString(PyExc_RuntimeError, "Too many efuns registered.");
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

/* Module definition fot the ldmud builtin module */
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
    return PyModule_Create(&ldmud_module);
}


/*-------------------------------------------------------------------------*/
/* --- Helper functions --- */


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

        default:
            return NULL;
    }

} /* svalue_to_python */


/*-------------------------------------------------------------------------*/


static bool
python_to_svalue (svalue_t *dest, PyObject* val)

/* Convert a python value <val> back to an LPC value
 * and puts the result into <dest>. Returns true
 * for success, false for an unknown type.
 *
 * <dest> is assumed to be empty.
 */

{
    if (val == Py_None)
    {
        put_number(dest, 0);
        return true;
    }

    if (PyLong_Check(val))
    {
        int overflow;
        long num = PyLong_AsLongAndOverflow(val, &overflow);

        if (overflow)
            return false;

        if (num < PINT_MIN || num > PINT_MAX)
            return false;

        put_number(dest, (p_int)num);
        return true;
    }

    if (PyBool_Check(val))
    {
        put_number(dest, val == Py_True ? 1 : 0);
        return true;
    }

    if (PyFloat_Check(val))
    {
        put_float(dest, PyFloat_AsDouble(val));
        return true;
    }

    if (PyBytes_Check(val))
    {
        Py_ssize_t length;
        char * buf;

        PyBytes_AsStringAndSize(val, &buf, &length);
        put_c_n_string(dest, buf, length);
        return true;
    }

    if (PyUnicode_Check(val))
    {
        PyObject *utf8;
        Py_ssize_t length;
        char * buf;

        utf8 = PyUnicode_AsEncodedString(val, "utf-8", "replace");
        if (utf8 == NULL)
            return false;

        PyBytes_AsStringAndSize(utf8, &buf, &length);
        put_c_n_string(dest, buf, length);
        Py_DECREF(utf8);
        return true;
    }

    return false;

} /* svalue_to_python */


/*-------------------------------------------------------------------------*/
/* --- Global functions --- */


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
            msg = "Unknown exception.";
        else
        {
            /* And convert it to UTF-8 (for now). */
            PyObject *exc_utf8;

            exc_utf8 = PyUnicode_AsEncodedString(exc_str, "utf-8", "replace");
            if (exc_utf8 == NULL)
                msg = "Undecodable exception.";
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
        if (!python_to_svalue(inter_sp + 1, result))
        {
            Py_DECREF(result);
            errorf("Bad return value from %s().\n"
                  , get_txt(python_efun_names[idx]->name));
        }

        inter_sp++;
        Py_DECREF(result);
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

#endif /* USE_PYTHON && HAS_PYTHON3 */
