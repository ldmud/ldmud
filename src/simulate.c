#include "driver.h"

#include "my-alloca.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <setjmp.h>
#include <stdio.h>
#include <sys/stat.h>
#ifdef __STDC__
#include <stdarg.h>
#endif
#ifdef AMIGA
#include "hosts/amiga/nsignal.h"
#else
#include <signal.h>
#endif
#if defined(HAVE_DIRENT_H) || defined(_POSIX_VERSION)
#include <dirent.h>
#define generic_dirent dirent
#define DIRENT_NLENGTH(dirent) (strlen((dirent)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#define generic_dirent direct
#define DIRENT_NLENGTH(dirent) ((dirent)->d_namlen)
#ifdef HAVE_SYS_NDIR_H
#include <sys/ndir.h>
#endif /* SYSNDIR */
#ifdef HAVE_SYS_DIR_H
#include <sys/dir.h>
#endif /* SYSDIR */
#ifdef HAVE_NDIR_H
#include <ndir.h>
#endif /* NDIR */
#endif /* not (HAVE_DIRENT_H or _POSIX_VERSION) */
#if defined(__CYGWIN__)
extern int lstat PROT((const char *, struct stat *));
#endif

#include "simulate.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "dumpstat.h"
#include "ed.h"
#include "exec.h"
#include "filestat.h"
#include "heartbeat.h"
#include "interpret.h"
#include "instrs.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "object.h"
#include "otable.h"
#include "prolang.h"
#include "rxcache.h"
#include "sent.h"
#include "simul_efun.h"
#include "stralloc.h"
#include "strfuns.h"
#include "swap.h"
#include "wiz_list.h"

#include "../mudlib/sys/rtlimits.h"

#ifdef atarist
#define CONST const
#else
#define CONST
#endif

#ifndef S_ISDIR
#define S_ISDIR(m)        (((m)&S_IFMT) == S_IFDIR)
#endif

#ifndef S_ISREG
#define S_ISREG(m)        (((m)&S_IFMT) == S_IFREG)
#endif


#ifdef SunOS4
#if ! defined (__GNUC__) || __GNUC__ < 2 || __GNUC__ == 2 && __GNUC_MINOR__ < 7
extern int lstat PROT((CONST char *, struct stat *));
#endif
extern int fchmod PROT((int, int));
#endif
#if defined(OS2)
#define lstat stat
#endif

/* --- Runtime limits --- */

/* Each of these limits comes as pair: one def_... value which holds the
 * limit set at startup or with the set_limits() efun, and the max_... 
 * value which holds the limit currently in effect. Before every execution,
 * max_... are initialised from def_... with the RESET_LIMITS macro.
 * 
 * A limit of 0 usually means 'no limit'.
 */

size_t def_array_size = MAX_ARRAY_SIZE;
size_t max_array_size = MAX_ARRAY_SIZE;
  /* If != 0: the max. number of elements in an array.
   */

size_t def_mapping_size = MAX_MAPPING_SIZE;
size_t max_mapping_size = MAX_MAPPING_SIZE;
  /* If != 0: the max. number of elements in a mapping.
   */

int32 def_eval_cost = MAX_COST;
int32 max_eval_cost = MAX_COST;
  /* The max eval cost available for one execution thread. Stored as negative
   * value for easier initialisation (see eval_cost).
   * CLEAR_EVAL_COST uses this value to re-initialize (assigned_)eval_cost.
   */

int32 def_byte_xfer = MAX_BYTE_TRANSFER;
int32 max_byte_xfer = MAX_BYTE_TRANSFER;
  /* Maximum number of bytes to read/write in one read/write_bytes() call.
   * If 0, it is unlimited.
   */

int32 def_file_xfer = READ_FILE_MAX_SIZE;
int32 max_file_xfer = READ_FILE_MAX_SIZE;
  /* Maximum number of bytes to read/write in one read/write_file() call.
   */

/* --- struct limits_context_s: last runtime limits context ---
 *
 * This structure saves the runtime limits on the runtime context stack.
 * It is also used as a temporary when parsing limit specifications.
 */

struct limits_context_s
{
    rt_context_t rt;     /* the rt_context superclass */
    size_t max_array;    /* max array size */
    size_t max_mapping;  /* max mapping size */
    int32  max_eval;     /* max eval cost */
    int32  max_byte;     /* max byte xfer */
    int32  max_file;     /* max file xfer */
    int32  eval_cost;    /* the then-current eval costs used */
};

char *inherit_file;
/*
 * 'inherit_file' is used as a flag. If it is set to a string
 * after yyparse(), this string should be loaded as an object,
 * and the original object must be loaded again.
 */
#ifdef F_SET_IS_WIZARD
int is_wizard_used = 0;
  /* TODO: This flag can go when the special commands are gone. */
#endif

struct object *obj_list = NULL;
  /* Head of the list of all objects
   */
struct object *obj_list_end = NULL;
  /* Last object in obj_list. This object also has its .next_all member
   * cleared.
   */

struct object *destructed_objs = NULL;
  /* List holding objects destructed in this execution thread.
   * They are no longer part of the obj_list.
   */

struct object *master_ob = 0;
p_int new_destructed = 0;  /* Number of destructed objects in object list */

struct object *current_object;      /* The object interpreting a function. */
struct object *current_interactive; /* The user who caused this execution */
struct object *previous_ob;

int num_parse_error;                /* Number of errors in the parser. */

struct svalue closure_hook[NUM_CLOSURE_HOOKS];

/*-------------------------------------------------------------------------*/
static INLINE void
save_limits_context (struct limits_context_s * context)

/* Save the current limits context into <context> (but don't put it
 * onto the context stack).
 */

{
    context->rt.type = LIMITS_CONTEXT;
    context->max_array = max_array_size;
    context->max_mapping = max_mapping_size;
    context->max_eval = max_eval_cost;
    context->eval_cost = eval_cost;
    context->max_byte = max_byte_xfer;
    context->max_file = max_file_xfer;
} /* save_limits_context() */

/*-------------------------------------------------------------------------*/
static INLINE void
restore_limits_context (struct limits_context_s * context)

/* Restore the last runtime limits from <context>.
 *
 * Restoring max_eval_cost is a bit tricky since eval_cost
 * itself might be a bit too high for the restored limit, but
 * avoiding a 'eval-cost too high' was the point of the exercise
 * in the first place. Therefore, if we ran under a less limited
 * eval-cost limit, we fake an effective cost of 10 ticks.
 */

{
    assign_eval_cost();
    if (!max_eval_cost || max_eval_cost > context->max_eval)
    {
        assigned_eval_cost = eval_cost = context->eval_cost+10;
    }
    max_array_size = context->max_array;
    max_mapping_size = context->max_mapping;
    max_eval_cost = context->max_eval;
    max_byte_xfer = context->max_byte;
    max_file_xfer = context->max_file;
} /* restore_limits_context() */

/*-------------------------------------------------------------------------*/
static void
unroll_context_stack (void)

/* Remove entries from the rt_context stack until the last entry
 * is an ERROR_RECOVERY context.
 */

{
    while (!ERROR_RECOVERY_CONTEXT(rt_context->type))
    {
        rt_context_t * context = rt_context;

        rt_context = rt_context->last;
        switch(context->type)
        {
        case COMMAND_CONTEXT:
            restore_command_context(context);
            break;

        case LIMITS_CONTEXT:
            restore_limits_context((struct limits_context_s *)context);
            break;

        default:
            fatal("Unimplemented context type %d.\n", context->type);
            /* NOTREACHED */
        }
    }
} /* unroll_context_stack() */

/*-------------------------------------------------------------------------*/
struct give_uid_error_context {
    struct svalue head;
    struct object *new_object;
};

static void give_uid_error_handler(arg)
    struct svalue *arg;
{
    struct give_uid_error_context *ecp;
    struct object *ob;

    ecp = (struct give_uid_error_context *)arg;
    ob = ecp->new_object;
    xfree((char *)ecp);
    if (ob)
    {
        emergency_destruct(ob);
    }
}

static void push_give_uid_error_context(ob)
    struct object *ob;
{
    struct give_uid_error_context *ecp;

    ecp = (struct give_uid_error_context *)xalloc(sizeof *ecp);
    if (!ecp) {
        emergency_destruct(ob);
        error("Out of memory\n");
    }
    ecp->head.type = T_ERROR_HANDLER;
    ecp->head.u.error_handler = give_uid_error_handler;
    ecp->new_object = ob;
    inter_sp++;
    inter_sp->type = T_LVALUE;
    inter_sp->u.lvalue = &ecp->head;
}

/*
 * Give the correct uid and euid to a created object.
 */
static int give_uid_to_object(ob, x, n)
    struct object *ob;
    int x, n;
{
    struct lambda *l;
    char *err;
    struct svalue arg, *ret;

    ob->user = &default_wizlist_entry;
    if ( NULL != (l = closure_hook[x].u.lambda) ) {
        if (closure_hook[x].x.closure_type == CLOSURE_LAMBDA)
            l->ob = ob;
        call_lambda(&closure_hook[x], n);
        ret = inter_sp;
        xfree((char *)ret[-1].u.lvalue); /* free error context */
        if (ret->type == T_STRING) {
            ob->user = add_name(ret->u.string);
            ob->eff_user = ob->user;
            pop_stack();        /* deallocate result */
            inter_sp--;         /* skip error context */
            return 1;
        } else if (ret->type == T_POINTER && VEC_SIZE(ret->u.vec) == 2 &&
                   ( ret->u.vec->item[0].type == T_STRING
                     || (!strict_euids && ret->u.vec->item[0].u.number)

        ) ) {
            ret = ret->u.vec->item;
            ob->user =
                ret[0].type != T_STRING ?
                  &default_wizlist_entry :
                  add_name(ret[0].u.string);
            ob->eff_user =
                ret[1].type != T_STRING ? 0 : add_name(ret[1].u.string);
            pop_stack();
            inter_sp--;
            return 1;
        } else if (!strict_euids && ret->type == T_NUMBER && ret->u.number) {
            ob->user = &default_wizlist_entry;
            ob->eff_user = NULL;
            pop_stack();
            inter_sp--;
            return 1;
        } else {
            pop_stack();        /* deallocate result */
            err = "Illegal object to load.\n";
        }
    } else {
        do pop_stack(); while (--n); /* deallocate arguments */
        xfree((char *)inter_sp->u.lvalue);
        err = "closure to set uid not initialized!\n";
    }
    inter_sp--;                        /* skip error context */
    if (master_ob == 0) {
        /* Only for the master object. */
        ob->user = add_name("NONAME");
        ob->eff_user = NULL;
        return 1;
    }
    ob->user = add_name("NONAME");
    ob->eff_user = ob->user;
    put_object(&arg, ob);
    destruct_object(&arg);
    error(err);
    /* NOTREACHED */
    return 0;
}

/* Make a given object name sane.
 *
 * The function removes leading '/' (if addSlash is true, all but one leading
 * '/' are removed), a trailing '.c', and folds consecutive
 * '/' into just one '/'. The '.c' removal does not work when given
 * clone object names (i.e. names ending in '#<number>').
 *
 * The function returns a pointer to a static(!) buffer with the cleant
 * up name, or NULL if the given name already was sane.
 */

const char *
make_name_sane (const char *pName, Bool addSlash)
{
#   define BUFLEN 500
    static char buf[BUFLEN];
    const char *from = pName;
    char *to;
    short bDiffers = MY_FALSE;

    to = buf;

    /* Skip leading '/' */
    if (!addSlash)
    {
        while (*from == '/') {
            bDiffers = MY_TRUE;
            from++;
        }
    }
    else
    {
        *to++ = '/';
        if (*from != '/')
            bDiffers = MY_TRUE;
        else
        {
            from++;
            while (*from == '/') {
                bDiffers = MY_TRUE;
                from++;
            }
        }
    }
    /* addSlash or not: from now points to the first non-'/' */

    /* Copy the name into buf, doing the other operations */
    for (; '\0' != *from && (to - buf) < BUFLEN
         ; from++, to++)
    {
        if ('/' == *from)
        {
            *to = '/';
            while ('/' == *from) {
                from++;
                bDiffers = MY_TRUE;
            }

            from--;
        }
        else if ('.' == *from && 'c' == *(from+1) && '\0' == *(from+2))
        {
            bDiffers = MY_TRUE;
            break;
        }
        else
            *to = *from;
    }
    *to = '\0';

    if (!bDiffers)
        return NULL;

    return (const char *)buf;
#   undef BUFLEN
} /* make_name_sane() */

/*
 * Load an object definition from file. If the object wants to inherit
 * from an object that is not loaded, discard all, load the inherited object,
 * and reload again.
 *
 * The passed object name must be sane, but can be a clone object name.
 *
 * In mudlib3.0 when loading inherited objects, their reset() is not called.
 *
 * Save the command_giver, because reset() in the new object might change
 * it.
 */
static struct object *
load_object (const char *lname, int dont_reset, int depth)
{
    int fd;

    struct object *ob, *save_command_giver = command_giver;
    int i;
    struct stat c_st;
    int name_length;
    char *name; /* Copy of <lname> */
    char *fname; /* Filename for <name> */
    struct program *prog;

#ifdef DEBUG
    if ('/' == lname[0])
        fatal("Improper filename '%s' passed to load_object()\n", lname);
#endif

    /* It could be that the passed filename is one of an already loaded
     * object. In that case, simply return that object.
     */
    ob = lookup_object_hash((char *)lname);
    if (ob)
        return ob;

    /* We need two copies of <lname>: one to construct the filename in,
     * the second because lname might be a buffer which is deleted
     * during the compilation process.
     */
    name_length = strlen(lname);
    name = alloca(name_length+2);
    fname = alloca(name_length+4);
    if (!name || !fname)
        fatal("Stack overflow in load_object()");
#ifndef COMPAT_MODE
    *name++ = '/';  /* Add and hide a leading '/' */
#endif
    strcpy(name, lname);
    strcpy(fname, lname);

    if (strict_euids && current_object && current_object->eff_user == 0
        && current_object->name)
        error("Can't load objects when no effective user.\n");

    if (master_ob && master_ob->flags & O_DESTRUCTED) {
        /* The master has been destructed, and it has not been noticed yet.
         * Reload it, because it can't be done inside of yyparse.
         * assert_master_ob_loaded() will clear master_ob while reloading is
         * in progress, thus preventing a fatal recursion.
         */
        assert_master_ob_loaded();
        /* has the object been loaded by assert_master_ob_loaded ? */
        if ( NULL != (ob = find_object(name)) ) {
            if (ob->flags & O_SWAPPED && load_ob_from_swap(ob) < 0)
                    /* The master has swapped this object and used up most
                     * memory... strange, but thinkable */
                    error("Out of memory\n");
            return ob;
        }
    }
    {
        char c;
        char *p;

        i = name_length;
        p = name+name_length;
        while (--i > 0) {
            /* isdigit would need to check isascii first... */
            if ( (c = *--p) < '0' || c > '9' ) {
                if (c == '#' && name_length - i > 1) {
                    fprintf(stderr, "Illegal file to load: %s\n", name);
                    error("Illegal file to load.\n");
                }
                break;
            }
        }
    }
    /*
     * First check that the c-file exists.
     */
    (void)strcpy(fname+name_length, ".c");
    if (ixstat(fname, &c_st) == -1) {
        struct svalue *svp;

        push_volatile_string(fname);
        svp = apply_master_ob(STR_COMP_OBJ, 1);
        if (svp && svp->type == T_OBJECT) {
            if ( NULL != (ob = lookup_object_hash(name)) ) {
                if (ob == svp->u.ob)
                    return ob;
            } else if (ob != master_ob) {
                ob = svp->u.ob;
                remove_object_hash(ob);
                xfree(ob->name);
                ob->name = string_copy(name);
                enter_object_hash(ob);
                return ob;
            }
            fname[name_length] = '.';
        }
        fprintf(stderr, "Could not load descr for '%s'\n", name);
        error("Failed to load file '%s'.\n", name);
        return 0;
    }
    /*
     * Check if it's a legal name.
     */
    if (!legal_path(fname)) {
        fprintf(stderr, "Illegal pathname: '%s'\n", fname);
        error("Illegal path name '%s'.\n", fname);
        return 0;
    }

    /* The compilation loop. It will run until either <name> is loaded
     * or an error occurs. If the compilation is aborted because an
     * inherited object was not found, that object is loaded in a
     * recursive call, then the loop will try again on the original
     * object.
     */

    while (MY_TRUE) {
        /* This can happen after loading an inherited object: */
        ob = lookup_object_hash((char *)name);
        if (ob)
        {
            return ob;
        }

        if (comp_flag)
            fprintf(stderr, " compiling %s ...", fname);
        if (current_file)
        {
            error("Compiler is busy.\n");
        }
        fd = ixopen(fname, O_RDONLY | O_BINARY);
        if (fd <= 0) {
            perror(fname);
            error("Could not read the file.\n");
        }
        FCOUNT_COMP(fname);

        current_file = fname;
        /* The file name is needed before start_new_file(), in case there is
         * an initial line too long error.
         */
        start_new_file(fd);
        compile_file();
        end_new_file();
        if (comp_flag)
        {
            if (NULL == inherit_file)
                fprintf(stderr, " done\n");
            else
                fprintf(stderr, " needs inherit\n");
        }
        update_compile_av(total_lines);
        total_lines = 0;
        (void)close(fd);
        current_file = NULL;

        /* If there is no inherited file to compile, this compilation
         * succeeded. We can end the loop here.
         */
        if (NULL == inherit_file)
            break;

        /* This object wants to inherit an unloaded object. We discard
         * current object, load the object to be inherited and reload
         * the current object again. The global variable "inherit_file"
         * will be set by lang.y to point to a file name.
         */
        {
            char * pInherited;
            const char * tmp;

            tmp = make_name_sane(inherit_file, MY_FALSE);
            if (!tmp)
            {
                pInherited = inherit_file;
            }
            else
            {
                pInherited = alloca(strlen(tmp)+1);
                strcpy(pInherited, tmp);
            }

            push_referenced_shared_string(inherit_file);
            inherit_file = NULL;
            if (num_parse_error > 0) {
                error("Error in loading object '%s'\n", name);
            }
            if (strcmp(pInherited, name) == 0) {
                error("Illegal to inherit self.\n");
            }
            if (!depth)
            {
                error("Too deep inheritance nesting.\n");
            }
            ob = load_object(pInherited, 1, depth-1);
            free_string(inter_sp->u.string);
            inter_sp--;
            if (!ob || ob->flags & O_DESTRUCTED)
            {
                error("Error in loading object '%s' "
                      "(inheritance failed)\n", name);
            }
        } /* handling of inherit_file */
    } /* while() - compilation loop */

    if (num_parse_error > 0) {
        error("Error in loading object '%s'\n", name);
    }

    prog = compiled_prog;
#ifdef INITIALIZATION_BY___INIT
    ob = get_empty_object(prog->num_variables);
#else
    ob = get_empty_object( prog->num_variables, prog->variable_names
                         , prog_variable_values);
    /* TODO: The initializers should be stored in the program.
     * TODO:: See clone_object() for the reason.
     */
    for (i = prog->num_variables; --i >= 0; )
        free_svalue(&prog_variable_values[i]);
    xfree((char *)prog_variable_values);
#endif
    if (!ob)
        error("Out of memory\n");
    ob->name = string_copy(name);        /* Shared string is no good here */
#ifndef COMPAT_MODE
    name--;  /* Make the leading '/' visible again */
#endif
    ob->load_name = make_shared_string(name);  /* but here it is */
    ob->prog = prog;
    ob->ticks = ob->gigaticks = 0;
    ob->next_all = obj_list;
    ob->prev_all = NULL;
    if (obj_list)
        obj_list->prev_all = ob;
    obj_list = ob;
    if (!obj_list_end)
        obj_list_end = ob;
    num_listed_objs++;
    enter_object_hash(ob);        /* add name to fast object lookup table */

    push_give_uid_error_context(ob);
    push_string_shared(ob->name);
    if (give_uid_to_object(ob, H_LOAD_UIDS, 1)) {
        struct svalue *svp;
        int j;
        struct object *save_current;

        save_current = current_object;
        current_object = ob; /* for lambda_ref_replace_program */
        svp = ob->variables;
        for (j = ob->prog->num_variables;  --j >= 0; svp++) {
            if (svp->type == T_NUMBER)
                continue;
            set_svalue_user(svp, ob);
        }
        if (save_current == &dummy_current_object_for_loads) {
            /* The master object is loaded with no current object */
            current_object = 0;
            reset_object(ob, H_CREATE_OB - dont_reset);
            /* If the master inherits anything -Ugh- we have to have
               some object to attribute initialized variables to again */
            current_object = save_current;
        } else {
            current_object = save_current;
            reset_object(ob, H_CREATE_OB - dont_reset);
        }
    }
    if ( !(ob->flags & O_DESTRUCTED) && function_exists("clean_up",ob) )
        ob->flags |= O_WILL_CLEAN_UP;
    command_giver = check_object(save_command_giver);
    if (d_flag > 1 && ob)
        debug_message("--%s loaded\n", ob->name);
    return ob;
}

void set_svalue_user(svp, owner)
    struct svalue *svp;
    struct object *owner;
{
    switch(svp->type) {
      case T_POINTER:
      case T_QUOTED_ARRAY:
        set_vector_user(svp->u.vec, owner);
        break;
      case T_MAPPING:
      {
        set_mapping_user(svp->u.map, owner);
        break;
      }
      case T_CLOSURE:
      {
        set_closure_user(svp, owner);
      }
    }
}

static char *make_new_name(str)
    char *str;
{
    static long i = 0;
    static int test_conflict = 0;

    char *p;
    int l;
    char buff[12];

    if ('/' == *str)
        str++;

    for (;;) {
        (void)sprintf(buff, "#%ld", i);
        l = strlen(str);
        p = xalloc(l + strlen(buff) + 1);
        strcpy(p, str);
        strcpy(p+l, buff);
        i++;
        if (i <= 0)
            test_conflict = 1;
        if (!test_conflict || !find_object(p))
            return p;
        xfree(p);
    }
}


/*
 * Save the command_giver, because reset() in the new object might change
 * it.
 */
struct object *clone_object(str1)
    char *str1;
{
    struct object *ob, *new_ob;
    struct object *save_command_giver = command_giver;
    char *name;

    if (strict_euids && current_object && current_object->eff_user == 0)
        error("Illegal to call clone_object() with effective user 0\n");
    ob = get_object(str1);

    /*
     * If the object self-destructed...
     */
    if (ob == 0)
        return 0;

    /* If ob is a clone, try finding the blueprint via the load_name */
    if (ob->flags & O_CLONE)
    {
        struct object *bp;

        bp = get_object(ob->load_name);
        if (bp)
            ob = bp;
    }

    if (ob->super)
        error("Cloning a bad object: '%s' is contained in '%s'.\n"
             , ob->name, ob->super->name);

    name = ob->name;

    /* If the ob is a clone, we have to test if its name is something
     * illegal like 'foobar#34'. In that case, we have to use the
     * load_name as template.
     */
    if (ob->flags & O_CLONE)
    {
        char c;
        char *p;
        mp_int name_length, i;

        name_length = strlen(name);
        i = name_length;
        p = ob->name+name_length;
        while (--i > 0) {
            /* isdigit would need to check isascii first... */
            if ( (c = *--p) < '0' || c > '9' )
            {
                if (c == '#' && name_length - i > 1)
                {
                    /* Well, unusable name format - use the load_name */
                    name = ob->load_name;
                }
                break;
            }
        }
    }

    if ((ob->flags & O_SWAPPED) && load_ob_from_swap(ob) < 0)
        error("Out of memory\n");
    /* TODO: Good question: if both blueprint and clone are swapped out,
     * TODO:: and the clone is swapped back in - what happens to the entries
     * TODO:: of the blueprint? Answer: cloned programs are never swapped?
     */

    if (ob->prog->flags & P_NO_CLONE)
        error("Cloning a bad object: '%s' sets '#pragma no_clone'.\n"
             , ob->name);

    ob->time_of_ref = current_time;

    /* We do not want the heart beat to be running for unused copied objects */

    if (!(ob->flags & O_CLONE) && ob->flags & O_HEART_BEAT)
        set_heart_beat(ob, 0);
    new_ob = get_empty_object(ob->prog->num_variables
#ifndef INITIALIZATION_BY___INIT
                             , ob->prog->variable_names
                             , ob->variables
#endif
    );
    /* TODO: Yeech: the new objects variables are initialised from the
     * TODO:: template object variables. These values should be stored
     * TODO:: in the program.
     */
    if (!new_ob)
        error("Out of memory\n");
    new_ob->name = make_new_name(name);
    new_ob->load_name = ref_string(ob->load_name);
    new_ob->flags |= O_CLONE | (ob->flags & O_WILL_CLEAN_UP ) ;
    new_ob->prog = ob->prog;
    reference_prog (ob->prog, "clone_object");
    new_ob->ticks = new_ob->gigaticks = 0;
#ifdef DEBUG
    if (!current_object)
        fatal("clone_object() from no current_object !\n");
#endif
    new_ob->next_all = obj_list;
    new_ob->prev_all = NULL;
    if (obj_list)
        obj_list->prev_all = new_ob;
    obj_list = new_ob;
    if (!obj_list_end)
        obj_list_end = new_ob;
    num_listed_objs++;
    enter_object_hash(new_ob);        /* Add name to fast object lookup table */
    push_give_uid_error_context(new_ob);
    push_object(ob);
    push_volatile_string(new_ob->name);
    give_uid_to_object(new_ob, H_CLONE_UIDS, 2);
    reset_object(new_ob, H_CREATE_CLONE);
    command_giver = check_object(save_command_giver);
    /* Never know what can happen ! :-( */
    if (new_ob->flags & O_DESTRUCTED)
        return 0;
    return new_ob;
}

struct svalue *f_rename_object(sp)
    struct svalue *sp;
{
    struct object *ob;
    char *name;
    mp_int length;

    inter_sp = sp; /* this is needed for assert_master_ob_loaded(), and for
                    * the possible errors before.
                    */
    if (sp[-1].type != T_OBJECT)
        bad_xefun_arg(1, sp);
    if (sp[0].type != T_STRING)
        bad_xefun_arg(2, sp);
    ob = sp[-1].u.ob;
    name = sp[0].u.string;
    /* Remove leading '/' if any. */
    while(name[0] == '/')
        name++;
    /* Truncate possible .c in the object name. */
    length = strlen(name);
    if (name[length-2] == '.' && name[length-1] == 'c') {
        /* A new writeable copy of the name is needed. */
        char *p;
        p = (char *)alloca(length+1);
        strcpy(p, name);
        name = p;
        name[length -= 2] = '\0';
    }
    {
        char c;
        char *p;
        mp_int i;

        i = length;
        p = name + length;
        while (--i > 0) {
            /* isdigit would need to check isascii first... */
            if ( (c = *--p) < '0' || c > '9' ) {
                if (c == '#' && length - i > 1) {
                    error("Illegal name to rename_object: '%s'.\n", name);
                }
                break;
            }
        }
    }
    if (lookup_object_hash(name)) {
        error("Attempt to rename to object '%s'\n", name);
    }
    assert_master_ob_loaded();
    if (master_ob == ob)
        error("Attempt to rename the master object\n");
    if (privilege_violation4("rename_object", ob, name, 0, sp)) {
        remove_object_hash(ob);
        xfree(ob->name);
        ob->name = string_copy(name);
        enter_object_hash(ob);
    }
    free_svalue(sp--);
    free_svalue(sp--);
    return sp;
}

struct object *environment(arg)
    struct svalue *arg;
{
    struct object *ob = current_object;

    if (arg && arg->type == T_OBJECT)
        ob = arg->u.ob;
    else if (arg && arg->type == T_STRING)
        ob = find_object(arg->u.string);
    if (ob == 0 || ob->super == 0 || (ob->flags & O_DESTRUCTED))
        return 0;
    if (ob->flags & O_DESTRUCTED)
        error("environment() of destructed object.\n");
    return ob->super;
}


/*
 * To find if an object is present, we have to look in two inventory
 * lists. The first list is the inventory of the current object.
 * The second list is all things that have the same ->super as
 * current_object.
 * Also test the environment.
 * If the second argument 'ob' is non zero, only search in the
 * inventory of 'ob'. The argument 'ob' will be mandatory, later.
 * TODO: Make this a nice efuns.c-Efun and also implement
 * TODO:: deep_present() and present_clone() (see bugs/f-something)
 */

static struct object *object_present2 PROT((char *, struct object *));

struct object *object_present(v, ob)
    struct svalue *v;
    struct object *ob;
{
    struct svalue *ret;
    struct object *ret_ob;
    int specific = 0;

    if (ob == 0)
        ob = current_object;
    else
        specific = 1;
    if (ob->flags & O_DESTRUCTED)
        return 0;
    if (v->type == T_OBJECT) {
        if (specific) {
            if (v->u.ob->super == ob)
                return v->u.ob;
            else
                return 0;
        }
        if (v->u.ob->super == ob ||
            (v->u.ob->super == ob->super && ob->super != 0))
            return v->u.ob->super;
        return 0;
    }
    ret_ob = object_present2(v->u.string, ob->contains);
    if (ret_ob)
        return ret_ob;
    if (specific)
        return 0;
    if (ob->super) {
        push_volatile_string(v->u.string);
        ret = sapply(STR_ID, ob->super, 1);
        if (ob->super->flags & O_DESTRUCTED)
            return 0;
        if (ret && !(ret->type == T_NUMBER && ret->u.number == 0))
            return ob->super;
        return object_present2(v->u.string, ob->super->contains);
    }
    return 0;
}

static struct object *object_present2(str, ob)
    char *str;
    struct object *ob;
{
    struct svalue *ret;
    char *p;
    int count = 0, length;
    char *item;

    length = strlen(str);
    item = xalloc(length + 1);
    if (!item)
        error("Out of memory\n");
    strcpy(item, str);
    push_malloced_string(item); /* free on error */
    p = item + length - 1;
    if (*p >= '0' && *p <= '9') {
        while(p > item && *p >= '0' && *p <= '9')
            p--;
        if (p > item && *p == ' ') {
            count = atoi(p+1) - 1;
            *p = '\0';
        /*  length = p - item;        This is never used again ! */
        }
    }
    for (; ob; ob = ob->next_inv) {
        push_volatile_string(item);
        ret = sapply(STR_ID, ob, 1);
        if (ob->flags & O_DESTRUCTED) {
            xfree(item);
            inter_sp--;
            return 0;
        }
        if (ret == 0 || (ret->type == T_NUMBER && ret->u.number == 0))
            continue;
        if (count-- > 0)
            continue;
        xfree(item);
        inter_sp--;
        return ob;
    }
    xfree(item);
    inter_sp--;
    return 0;
}

/*
 * Remove an object. It is first moved into the destruct list, and
 * not really destructed until later. (see destruct2()).
 */
void destruct_object(v)
    struct svalue *v;
{
    struct object *ob;
    struct svalue *result;

    if (v->type == T_OBJECT)
        ob = v->u.ob;
    else {
        ob = find_object(v->u.string);
        if (ob == 0)
            error("destruct_object: Could not find %s\n", v->u.string);
    }
    if (ob->flags & O_DESTRUCTED)
        return;
    if (ob->flags & O_SWAPPED)
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory\n");

    if (d_flag)
    {
        debug_message("Destruct object %s (ref %ld)\n", ob->name, ob->ref);
    }

    push_object(ob);
    result = apply_master_ob(STR_PREP_DEST, 1);
    if (!result) error("No prepare_destruct\n");
    if (result->type == T_STRING) error(result->u.string);
    if (result->type != T_NUMBER || result->u.number != 0) return;
    if (ob->contains) {
        error("Master failed to clean inventory in prepare_destruct\n");
    }
    if (ob->flags & O_SHADOW) {
        struct interactive *ip;
        struct object *save=command_giver;

        command_giver=ob;
        ip = O_GET_INTERACTIVE(ob);
        if (ip->sent.type == SENT_INTERACTIVE)
            trace_level |= ip->trace_level;
        if (ip->sent.ed_buffer) {
            save_ed_buffer();
        }
        command_giver=save;
    }
    emergency_destruct(ob);
}

/*
 * Remove an object while the master is out of order. It is first just marked
 * as destructed and moved into the destructed_objs list, and not really
 * destructed until later. (see destruct2()).
 * This function is also used by destruct() to do the low-level operation.
 */
void emergency_destruct(ob)
    struct object *ob;
{
    struct object **pp, *item, *next;

    if (ob->flags & O_DESTRUCTED)
        return;

    ob->time_reset = 0;

    if (ob->flags & O_SWAPPED) {
        int save_privilege;

        save_privilege = malloc_privilege;
        malloc_privilege = MALLOC_SYSTEM;
        load_ob_from_swap(ob);
        malloc_privilege = save_privilege;
    }

    if (ob->flags & O_SHADOW)
    {
        struct shadow_sentence *shadow_sent;
        struct object *shadowing, *shadowed_by;

        shadow_sent = O_GET_SHADOW(ob);

        if (shadow_sent->ed_buffer) {
            struct object *save=command_giver;

            command_giver = ob;
            free_ed_buffer();
            command_giver=save;
        }
        /*
         * The chain of shadows is a double linked list. Take care to update
         * it correctly.
         */
        if ( NULL != (shadowing = shadow_sent->shadowing) ) {
            struct shadow_sentence *shadowing_sent;

            shadowing_sent = O_GET_SHADOW(shadowing);
            shadow_sent->shadowing = 0;
            if (!(shadowing_sent->shadowed_by = shadow_sent->shadowed_by) &&
                !shadowing_sent->shadowing && !shadowing_sent->ed_buffer &&
                shadowing_sent->type == SENT_SHADOW)
            {
                shadowing->flags &= ~O_SHADOW;
                shadowing->sent = shadowing_sent->next;
                free_shadow_sent(shadowing_sent);
            }
        }
        if ( NULL != (shadowed_by = shadow_sent->shadowed_by) ) {
            struct shadow_sentence *shadowed_by_sent;

            shadowed_by_sent = O_GET_SHADOW(shadowed_by);
            shadow_sent->shadowed_by = 0;
            if (!(shadowed_by_sent->shadowing = shadowing) &&
                !shadowed_by_sent->shadowed_by &&
                !shadowed_by_sent->ed_buffer &&
                shadowed_by_sent->type == SENT_SHADOW)
            {
                shadowed_by->flags &= ~O_SHADOW;
                shadowed_by->sent = shadowed_by_sent->next;
                free_shadow_sent(shadowed_by_sent);
            }
        }
        if (shadow_sent->type == SENT_SHADOW) {
            ob->sent = shadow_sent->next;
            free_shadow_sent(shadow_sent);
            ob->flags &= ~O_SHADOW;
        }
    }

    for (item = ob->contains; item; item = next) {
        remove_sent(ob, item);
        item->super = 0;
        next = item->next_inv;
        item->next_inv = 0;
    }
    remove_object_from_stack(ob);
    if (ob == simul_efun_object)
        simul_efun_object = 0;
    set_heart_beat(ob, 0);
    /*
     * Remove us out of this current room (if any).
     * Remove all sentences defined by this object from all objects here.
     */
    if (ob->super) {
        if (ob->super->sent)
            remove_sent(ob, ob->super);
#       ifdef F_SET_LIGHT
            add_light(ob->super, - ob->total_light);
#       endif
        for (pp = &ob->super->contains; *pp;) {
            if ((*pp)->sent)
                remove_sent(ob, *pp);
            if (*pp != ob)
                pp = &(*pp)->next_inv;
            else
                *pp = (*pp)->next_inv;
        }
    }
    /*
     * Now remove us out of the list of all objects.
     * This must be done last, because an error in the above code would
     * halt execution.
     */
    remove_object_hash(ob);
    if (ob->prev_all)
        ob->prev_all->next_all = ob->next_all;
    if (ob->next_all)
        ob->next_all->prev_all = ob->prev_all;
    if (ob == obj_list)
        obj_list = ob->next_all;
    if (ob == obj_list_end)
        obj_list_end = ob->prev_all;
    num_listed_objs--;
    ob->super = 0;
    ob->next_inv = 0;
    ob->contains = 0;
    ob->flags &= ~O_ENABLE_COMMANDS;
    ob->flags |= O_DESTRUCTED;  /* should come last! */
    new_destructed++;
    if (command_giver == ob) command_giver = 0;

    /* Put the object into the list of destructed objects */
    ob->prev_all = NULL;
    ob->next_all = destructed_objs;
    destructed_objs = ob;
}

/*
 * This one is called when no program is executing from the main loop.
 * The object must have been unlinked from the object list already.
 */
void destruct2(ob)
    struct object *ob;
{
    struct sentence *sent;

    if (d_flag > 1) {
        debug_message("Destruct-2 object %s (ref %ld)\n", ob->name, ob->ref);
    }
    if (O_GET_INTERACTIVE(ob) &&
        O_GET_INTERACTIVE(ob)->sent.type == SENT_INTERACTIVE)
    {
        remove_interactive(ob);
    }
    /*
     * We must deallocate variables here, not in 'free_object()'.
     * That is because one of the local variables may point to this object,
     * and deallocation of this pointer will also decrease the reference
     * count of this object. Otherwise, an object with a variable pointing
     * to itself, would never be freed.
     * Just in case the program in this object would continue to
     * execute, change string and object variables into the number 0.
     */
    if (ob->prog->num_variables > 0) {
        /*
         * Deallocate variables in this object.
         */
        int i;
        for (i=0; i<ob->prog->num_variables; i++) {
            free_svalue(&ob->variables[i]);
            put_number(ob->variables+i, 0);
        }
        xfree((char *)ob->variables);
    }

    /* This should be here to avoid using up memory as long as the object
     * isn't released. It must be here because gcollect doesn't expect
     * sentences in destructed objects.
     */
    if ( NULL != (sent = ob->sent) ) {
        struct sentence *next;
        do {

            next = sent->next;
            free_sentence(sent);
        } while ( NULL != (sent = next) );
        ob->sent = NULL;
    }

    free_object(ob, "destruct_object");
}

/*-------------------------------------------------------------------------*/
void
remove_destructed_objects (void)

/* Remove all destructed objects which are kept pending for deallocation
 * in the destructed_objs list.
 */

{
#ifdef DEBUG_RESET
printf("DEBUG: remove_destructed: %d: list %p '%s', end %p '%s'\n", new_destructed, obj_list, obj_list ? obj_list->name: "<null>", obj_list_end, obj_list_end ? obj_list_end->name: "<null>" );
#endif
    while (destructed_objs)
    {
        struct object *ob = destructed_objs;

        destructed_objs = ob->next_all;
#ifdef DEBUG
        if (!(ob->flags & O_DESTRUCTED))
            fatal("Non-destructed object %p '%s' in list of destructed objects.\n"
                 , ob, ob->name ? ob->name : "<null>"
                 );
#endif
        destruct2(ob);
        new_destructed--;
    }
#ifdef DEBUG
    if (new_destructed)
        fatal("new_destructed is %ld instead of 0.\n", new_destructed);
#endif
}  /* remove_destructed_objects() */


/*
 * A message from an object will reach
 * all objects in the inventory,
 * all objects in the same environment and
 * the surrounding object.
 * Non interactive objects gets no messages.
 *
 * There are two cases to take care of. If this routine is called from
 * a player (indirectly), then the message goes to all in his
 * environment. Otherwise, the message goes to all in the current_object's
 * environment (as the case when called from a heart_beat()).
 *
 * Do not send the message to members of the array avoid.
 */

void say(v, avoid)
    struct svalue *v;
    struct vector *avoid;
{
    static struct svalue ltmp = { T_POINTER };
    static struct svalue stmp = { T_OBJECT };
    struct object *ob, *save_command_giver = command_giver;
    struct object *origin;
    char buff[256], *message;
#define INITIAL_MAX_RECIPIENTS 48
    int max_recipients = INITIAL_MAX_RECIPIENTS;
    struct object *first_recipients[INITIAL_MAX_RECIPIENTS];
    struct object **recipients = first_recipients;
    struct object **curr_recipient = first_recipients;
    struct object **last_recipients =
        &first_recipients[INITIAL_MAX_RECIPIENTS-1];

    struct object *save_again;

    if (current_object->flags & O_ENABLE_COMMANDS) {
        command_giver = current_object;
    } else if (current_object->flags & O_SHADOW &&
             O_GET_SHADOW(current_object)->shadowing)
    {
        command_giver = O_GET_SHADOW(current_object)->shadowing;
    }
    if (command_giver) {
        struct interactive *ip;

        if (NULL != (ip = O_GET_INTERACTIVE(command_giver)) &&
            ip->sent.type == SENT_INTERACTIVE)
        {
            trace_level |= ip->trace_level;
        }
        origin = command_giver;
        if (avoid->item[0].type == T_NUMBER) {
            put_ref_object(avoid->item, command_giver, "say");
        }
    } else
        origin = current_object;
    ltmp.u.vec = avoid;
    avoid = order_alist(&ltmp, 1, 1);
    push_referenced_vector(avoid);
    avoid = avoid->item[0].u.vec;
    if ( NULL != (ob = origin->super) ) {
        struct interactive *ip;

        if (ob->flags & O_ENABLE_COMMANDS ||
            (NULL != (ip = O_GET_INTERACTIVE(ob)) && ip->sent.type == SENT_INTERACTIVE))
        {
            *curr_recipient++ = ob;
        }
        for (ob = ob->contains; ob; ob = ob->next_inv) {
            if (ob->flags & O_ENABLE_COMMANDS ||
                ( NULL != (ip = O_GET_INTERACTIVE(ob)) &&
                   ip->sent.type == SENT_INTERACTIVE) )
        {
                if (curr_recipient >= last_recipients) {
                    max_recipients <<= 1;
                    curr_recipient = (struct object **)
                      alloca(max_recipients * sizeof(struct object *));
                    memcpy((char*)curr_recipient, (char*)recipients,
                      max_recipients * sizeof(struct object *)>>1);
                    recipients = curr_recipient;
                    last_recipients = &recipients[max_recipients-1];
                    curr_recipient += (max_recipients>>1) - 1;
                }
                *curr_recipient++ = ob;
            }
        }
    }
    for (ob = origin->contains; ob; ob = ob->next_inv) {
        struct interactive *ip;

        if (ob->flags & O_ENABLE_COMMANDS ||
            ( NULL != (ip = O_GET_INTERACTIVE(ob)) &&
               ip->sent.type == SENT_INTERACTIVE) )
        {
            if (curr_recipient >= last_recipients) {
                max_recipients <<= 1;
                curr_recipient = (struct object **)
                  alloca(max_recipients * sizeof(struct object *));
                memcpy((char*)curr_recipient, (char*)recipients,
                  max_recipients * sizeof(struct object *)>>1);
                recipients = curr_recipient;
                last_recipients = &recipients[max_recipients-1];
                curr_recipient += (max_recipients>>1) - 1;
            }
            *curr_recipient++ = ob;
        }
    }
    *curr_recipient = (struct object *)0;
    switch(v->type) {
    case T_STRING:
        message = v->u.string;
        break;
    case T_OBJECT:
        strncpy(buff, v->u.ob->name, sizeof buff);
        buff[sizeof buff - 1] = '\0';
        message = buff;
        break;
    case T_NUMBER:
        sprintf(buff, "%ld", v->u.number);
        message = buff;
        break;
    case T_POINTER:
        for (curr_recipient = recipients; NULL != (ob = *curr_recipient++) ; ) {
            if (ob->flags & O_DESTRUCTED) continue;
            stmp.u.ob = ob;
            if (assoc(&stmp, avoid) >= 0) continue;
            push_vector(v->u.vec);
            push_object(origin);
            sapply(STR_CATCH_MSG, ob, 2);
        }
        pop_stack(); /* free avoid alist */
        command_giver = check_object(save_command_giver);
        return;
    default:
        error("Invalid argument %d to say()\n", v->type);
    }
    for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); ) {
        struct interactive *ip;

        if (ob->flags & O_DESTRUCTED) continue;
        stmp.u.ob = ob;
        if (assoc(&stmp, avoid) >= 0) continue;
        if (  !(ip = O_GET_INTERACTIVE(ob)) ||
                ip->sent.type != SENT_INTERACTIVE)
        {
            tell_npc(ob, message);
            continue;
        }
        save_again = command_giver;
        command_giver = ob;
        add_message("%s", message);
        command_giver = save_again;
    }
    pop_stack(); /* free avoid alist */
    command_giver = check_object(save_command_giver);
}

/*
 * Send a message to all objects inside an object.
 * Non interactive objects get the messages too.
 * Compare with say().
 */

void tell_room(room, v, avoid)
    struct object *room;
    struct svalue *v;
    struct vector *avoid; /* has to be in alist order */
{
    int assoc PROT((struct svalue *key, struct vector *));
    struct object *ob, *save_command_giver;
    int num_recipients = 0;
    struct object *some_recipients[20], **recipients, **curr_recipient;
    char buff[256], *message;
    static struct svalue stmp = { T_OBJECT, } ;


    for (ob = room->contains; ob; ob = ob->next_inv) {
        struct interactive *ip;

        if ( ob->flags & O_ENABLE_COMMANDS ||
             ( NULL != (ip = O_GET_INTERACTIVE(ob)) &&
                ip->sent.type == SENT_INTERACTIVE) )
        {
            num_recipients++;
        }
    }
    if (num_recipients < 20)
        recipients = some_recipients;
    else
        recipients = (struct object **)
          alloca( (num_recipients+1) * sizeof(struct object *) );
    curr_recipient = recipients;
    for (ob = room->contains; ob; ob = ob->next_inv) {
        struct interactive *ip;

        if ( ob->flags & O_ENABLE_COMMANDS ||
             ( NULL != (ip = O_GET_INTERACTIVE(ob)) &&
                ip->sent.type == SENT_INTERACTIVE) )
        {
            *curr_recipient++ = ob;
        }
    }
    *curr_recipient = (struct object *)0;
    switch(v->type) {
      case T_STRING:
        message = v->u.string;
        break;
      case T_OBJECT:
        strncpy(buff, v->u.ob->name, sizeof buff);
        buff[sizeof buff - 1] = '\0';
        message = buff;
        break;
      case T_NUMBER:
        sprintf(buff, "%ld", v->u.number);
        message = buff;
        break;
      case T_POINTER:
      {
        struct object *origin = command_giver;
        if (!origin)
            origin = current_object;
        for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); ) {
            if (ob->flags & O_DESTRUCTED) continue;
            stmp.u.ob = ob;
            if (assoc(&stmp, avoid) >= 0) continue;
            push_vector(v->u.vec);
            push_object(origin);
            sapply(STR_CATCH_MSG, ob, 2);
        }
        return;
      }
      default:
        error("Invalid argument %d to tell_room()\n", v->type);
    }
    for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); ) {
        struct interactive *ip;

        if (ob->flags & O_DESTRUCTED) continue;
        stmp.u.ob = ob;
        if (assoc(&stmp, avoid) >= 0) continue;
        if (!(ip = O_GET_INTERACTIVE(ob)) || ip->sent.type != SENT_INTERACTIVE)
        {
            tell_npc(ob, message);
            continue;
        }
        save_command_giver = command_giver;
        command_giver = ob;
        add_message("%s", message);
        command_giver = save_command_giver;
    }
}

struct object *first_inventory(arg)
    struct svalue *arg;
{
    struct object *ob;

    if (arg->type == T_STRING)
        ob = get_object(arg->u.string);
    else
        ob = arg->u.ob;
    if (ob == 0)
        error("No object to first_inventory()");
    if (ob->contains == 0)
        return 0;
    return ob->contains;
}

#define MAX_LINES 50

/*
 * This one is used by qsort in get_dir().
 */
static int pstrcmp(p1, p2)
    struct svalue *p1, *p2;
{
    return strcmp(p1->u.string, p2->u.string);
}

#ifdef atarist
/* this code is provided to speed up ls() on an Atari ST/TT . */

#include <support.h>
#include <limits.h>
#include <osbind.h>

extern long _unixtime PROT((unsigned, unsigned));

struct xdirect {
        /* inode and position in directory aren't usable in a portable way,
           so why support them anyway?
         */
        short d_namlen;
        char  d_name[16];
        int   size;
        int   time;
};

typedef struct
{
    _DTA dta;
    char *dirname;
    long status;
} xdir;
#define XDIR xdir

static long olddta;

static XDIR *xopendir(path)
char *path;
{
    char pattern[MAXPATHLEN];
    XDIR *d;
    long status;

    d = (XDIR *)xalloc(sizeof(XDIR));
    _unx2dos(path, pattern);
    strcat(pattern, "\\*.*");
    olddta = Fgetdta();
    Fsetdta(&d->dta);
    d->status = status = Fsfirst(pattern, 0xff);
    if (status && status != -ENOENT) {
        xfree(d);
        return 0;
    }
    d->dirname = string_copy(pattern);
    return d;
}

#define XOPENDIR(dest, path) ((dest) = xopendir(path))

static struct xdirect *xreaddir(d)
XDIR *d;
{
    static struct xdirect xde;

    if (d->status)
        return 0;
    _dos2unx(d->dta.dta_name, xde.d_name);
    xde.d_namlen = strlen(xde.d_name);
    if (FA_DIR & d->dta.dta_attribute)
        xde.size = -2;
    else
        xde.size = d->dta.dta_size;
    xde.time = _unixtime(d->dta.dta_time, d->dta.dta_date);
    d->status = Fsnext();
    return &xde;
}

static void xclosedir(d)
XDIR *d;
{
    Fsetdta(olddta);
    xfree(d->dirname);
    xfree(d);
}
static void xrewinddir(d)
XDIR *d;
{
    long status;

    Fsetdta(&d->dta);
    d->status = status = Fsfirst(d->dirname, 0xff);
}

#endif /* atarist */

#ifndef XDIR

struct xdirect {
        /* inode and position in directory aren't usable in a portable way,
           so why support them anyway?
         */
        short d_namlen;
        char  *d_name;
        int   size;
        int   time;
};

#define XOPENDIR(dest, path) (\
    (!chdir(path) &&\
    NULL != ((dest) = opendir("."))) ||\
        (chdir(mud_lib),MY_FALSE)\
)

#define xclosedir( dir_ptr) (chdir(mud_lib),closedir(dir_ptr))
#define xrewinddir(dir_ptr)  rewinddir(dir_ptr)
#define XDIR DIR

static struct xdirect *xreaddir(dir_ptr, mask)
XDIR *dir_ptr;
int mask;
{
    static struct xdirect xde;
    struct generic_dirent *de;
    int namelen;
    struct stat st;

    de = readdir(dir_ptr);
    if (!de) return 0;
    namelen = DIRENT_NLENGTH(de);
    xde.d_namlen = namelen;
    xde.d_name   = de->d_name;
    if (mask & (2|4) ) {
        if (ixstat(xde.d_name, &st) == -1) { /* who knows... */
            xde.size = -1;
            xde.time = 0;
        } else {
            if (S_IFDIR & st.st_mode)
                xde.size = -2;
            else
                xde.size = st.st_size;
            xde.time = st.st_mtime;
        }
    }
    return &xde;
}
#endif /* XDIR */

struct get_dir_error_context {
    struct svalue head;
    XDIR *dirp;
    struct vector *v;
};

static void get_dir_error_handler(arg)
    struct svalue *arg;
{
    struct get_dir_error_context *ecp;

    ecp = (struct get_dir_error_context *)arg;
    xclosedir(ecp->dirp);
    if (ecp->v)
        free_array(ecp->v);
}

/*
 * List files in directory. This function do same as standard list_files did,
 * but instead writing files right away to player this returns an array
 * containing those files. Actually most of code is copied from list_files()
 * function.
 * Differences with list_files:
 *
 *   - file_list("/w"); returns ({ "w" })
 *
 *   - file_list("/w/"); and file_list("/w/."); return contents of directory
 *     "/w"
 *
 *   - file_list("/");, file_list("."); and file_list("/."); return contents
 *     of directory "/"
 *
 * The mask flag values have to match mudlib/sys/files.h.
 */
struct vector *get_dir(path, mask)
    char *path;
    int mask;
{
    struct vector *v, *w;
    int i,j, count = 0;
    XDIR *dirp;
    int namelen, do_match = 0;
    struct xdirect *de;
    struct stat st;
    char *temppath;
    char *p;
    char *regexpr = 0;
    int nqueries;
    static struct get_dir_error_context ec;

    if (!path)
        return 0;

    path = check_valid_path(path, current_object, "get_dir", MY_FALSE);

    if (path == 0)
        return 0;

    /*
     * We need to modify the returned path, and thus to make a
     * writeable copy.
     * The path "" needs 2 bytes to store ".\0".
     */
    temppath = (char *)alloca(strlen(path)+2);
    if (strlen(path)<2) {
        temppath[0]=path[0]?path[0]:'.';
        temppath[1]='\000';
        p = temppath;
    } else {
        strcpy(temppath, path);
        /*
         * If path ends with '/' or "/." remove it
         */
        if ((p = strrchr(temppath, '/')) == 0)
            p = temppath;
        if ((p[0] == '/' && p[1] == '.' && p[2] == '\0') ||
            (p[0] == '/' && p[1] == '\0') )
            *p = '\0';
    }

    nqueries = (mask&1) + (mask>>1 &1) + (mask>>2 &1);
    if (strchr(p, '*') || ixstat(temppath, &st) < 0) {
        if (*p == '\0')
            return 0;
        regexpr = (char *)alloca(strlen(p)+2);
        if (p != temppath) {
            strcpy(regexpr, p + 1);
            *p = '\0';
        } else {
            strcpy(regexpr, p);
            strcpy(temppath, ".");
        }
        do_match = 1;
    } else if (*p != '\0' && strcmp(temppath, ".")) {
        struct svalue *stmp;

        if (*p == '/' && *(p + 1) != '\0')
            p++;
        v = allocate_array(nqueries);
        stmp = v->item;
        if (mask&1) {
            put_malloced_string(stmp, string_copy(p));
            stmp++;
        }
        if (mask&2) {
            put_number(stmp, (S_IFDIR & st.st_mode) ? -2 : st.st_size);
            stmp++;
        }
        if (mask&4) {
            put_number(stmp, st.st_mtime);
            stmp++;
        }
        return v;
    }

    if ( XOPENDIR(dirp, temppath) == 0)
        return 0;

    /* If an error occurs, clean things up.
     * When an error is caught, the lp stack gets popped after the machine
     * stack. For this reason, there must be no local variables holding
     * important svalue information. This is why ec is static.
     * get_dir() is not and need not be reentrant anyways.
     */
    ec.head.type = T_ERROR_HANDLER;
    ec.head.u.error_handler = get_dir_error_handler;
    ec.dirp = dirp;
    ec.v = 0;
    inter_sp++;
    inter_sp->type = T_LVALUE;
    inter_sp->u.lvalue = &ec.head;

    /*
     *  Count files
     */
    for (de = xreaddir(dirp, 1); de; de = xreaddir(dirp, 1)) {
        namelen = de->d_namlen;
        if (do_match) {
            if ( !match_string(regexpr, de->d_name, namelen) )
                continue;
        } else {
            if (namelen <= 2 && *de->d_name == '.' &&
                (namelen == 1 || de->d_name[1] == '.' ) )
                continue;
        }
        count += nqueries;
        if (max_array_size && count >= max_array_size)
            break;
    }
    if (nqueries)
        count /= nqueries;
    /*
     * Make array and put files on it.
     */
    v = allocate_array(count * nqueries);
    if (count == 0) {
        /* This is the easy case :-) */
        inter_sp--;
        xclosedir(dirp);
        return v;
    }
    ec.v = v;
    xrewinddir(dirp);
    w = v;
    j = 0;
    /* Taken into account that files might be added/deleted from outside. */
    for(i = 0, de = xreaddir(dirp,mask); de; de = xreaddir(dirp,mask)) {

        namelen = de->d_namlen;
        if (do_match) {
            if ( !match_string(regexpr, de->d_name, namelen) )
                continue;
        } else {
            if (namelen <= 2 && *de->d_name == '.' &&
                (namelen == 1 || de->d_name[1] == '.' ) )
                continue;
        }
        if (i >= count) {
            struct vector *tmp, *new;
            /* New file. Don't need efficience here, but consistence. */
            count++;
            tmp = allocate_array(nqueries);
            new = add_array(v, tmp);
            free_array(v);
            free_array(tmp);
            ec.v = v = new;
            w = v;
        }
        if (mask & 1) {
            char *name;

            if ( !(name = xalloc(namelen+1)) ) {
                error("Out of memory\n");
            }
            if (namelen)
                memcpy(name, de->d_name, namelen);
            name[namelen] = '\0';
            put_malloced_string(w->item+j, name);
            j++;
        }
        if (mask & 2) {
            put_number(w->item + j, de->size);
            j++;
        }
        if (mask & 4) {
            put_number(w->item + j, de->time);
            j++;
        }
        i++;
    }
    xclosedir(dirp);
    inter_sp--;
    if ( !((mask ^ 1) & 0x21) ) {
        /* Sort by names. */
        qsort((char *)v->item, i, sizeof v->item[0] * nqueries, pstrcmp);
    }
    return v;
}

int tail(path)
    char *path;
{
    char buff[1000];
    FILE *f;
    struct stat st;
    int offset;

    path = check_valid_path(path, current_object, "tail", MY_FALSE);

    if (path == 0)
        return 0;
    f = fopen(path, "r");
    if (f == 0)
        return 0;
    FCOUNT_READ(path);
    if (fstat(fileno(f), &st) == -1)
        fatal("Could not stat an open file.\n");
    if ( !S_ISREG(st.st_mode) ) {
        fclose(f);
        return 0;
    }
    offset = st.st_size - 54 * 20;
    if (offset < 0)
        offset = 0;
    if (fseek(f, offset, 0) == -1)
        fatal("Could not seek.\n");
    /* Throw away the first incomplete line. */
    if (offset > 0)
        (void)fgets(buff, sizeof buff, f);
    while(fgets(buff, sizeof buff, f)) {
        add_message("%s", buff);
    }
    fclose(f);
    return 1;
}

int print_file(path, start, len)
    char *path;
    int start, len;
{
    char buff[1000];
    FILE *f;
    int i;

    if (len < 0)
        return 0;

    path = check_valid_path(path, current_object, "print_file", MY_FALSE);

    if (path == 0)
        return 0;
    if (start < 0)
        return 0;
    f = fopen(path, "r");
    if (f == 0)
        return 0;
    FCOUNT_READ(path);

    if (len == 0)
        len = MAX_LINES;
    if (len > MAX_LINES)
        len = MAX_LINES;
    if (start == 0)
        start = 1;
    for (i=1; i < start + len; i++) {
        if (fgets(buff, sizeof buff, f) == 0)
            break;
        if (i >= start)
            add_message("%s", buff);
    }
    fclose(f);
    if (i <= start)
        return 0;
    if (i == MAX_LINES + start)
        add_message("*****TRUNCATED****\n");
    return i-start;
}

int remove_file(path)
    char *path;
{
    path = check_valid_path(path, current_object, "remove_file", MY_TRUE);

    if (path == 0)
        return 0;
    if (unlink(path) == -1)
        return 0;
    FCOUNT_DEL(path);
    return 1;
}

void
print_svalue(arg)
    struct svalue *arg;
{
    if (arg == 0) {
        add_message("<NULL>");
    } else if (arg->type == T_STRING) {
        struct interactive *ip;

        /* Strings sent to monsters are now delivered */
        if (command_giver && (command_giver->flags & O_ENABLE_COMMANDS) &&
            !( NULL != (ip = O_GET_INTERACTIVE(command_giver)) &&
                ip->sent.type == SENT_INTERACTIVE ) )
        {
            tell_npc(command_giver, arg->u.string);
        } else {
            add_message("%s", arg->u.string);
        }
    } else if (arg->type == T_OBJECT)
        add_message("OBJ(%s)", arg->u.ob->name);
    else if (arg->type == T_NUMBER)
        add_message("%ld", arg->u.number);
    else if (arg->type == T_FLOAT) {
        char buff[40];

        sprintf(buff, "%g", READ_DOUBLE( arg ) );
        add_message(buff);
    } else if (arg->type == T_POINTER)
        add_message("<ARRAY>");
    else
        add_message("<UNKNOWN>");
}

void do_write(arg)
    struct svalue *arg;
{
    struct object *save_command_giver = command_giver;
    if (command_giver == 0 &&
        current_object->flags & O_SHADOW &&
        O_GET_SHADOW(current_object)->shadowing)
    {
        command_giver = current_object;
    }
    if (command_giver) {
        /* Send the message to the first object in the shadow list */
        if (command_giver->flags & O_SHADOW)
            while( O_GET_SHADOW(command_giver)->shadowing )
                command_giver = O_GET_SHADOW(command_giver)->shadowing;
    }
    print_svalue(arg);
    command_giver = check_object(save_command_giver);
}

/* Find an object. If not loaded, load it !
 * The object may selfdestruct, which is the only case when 0 will be
 * returned.
 */

struct object *
lookfor_object(char * str, Bool bLoad)

/* Look for a named object <str>, optionally loading it (<bLoad> is true).
 * Return a pointer to the object structure, or NULL.
 *
 * If <bLoad> is true, the function tries to load the object if it is
 * not already loaded.
 * If <bLoad> is false, the function just checks if the object is loaded.
 *
 * The object is not swapped in.
 *
 * For easier usage, the macros find_object() and get_object() expand
 * to the no-load- resp. load-call of this function.
 *
 * TODO: It would be nice if all loading uses of lookfor would go through
 * TODO:: the efun load_object() or a driver hook so that the mudlib
 * TODO:: has a chance to interfere with it. Dito for clone_object(), so
 * TODO:: that the euid-check can be done there?
 */
{
    struct object *ob;
    const char * pName;

    /* TODO: It would be more useful to check all callers of lookfor()
     * TODO:: and move the make_name_sane() into those where it can
     * TODO:: be dirty.
     */
    pName = make_name_sane(str, MY_FALSE);
    if (!pName)
        pName = str;

    ob = lookup_object_hash((char *)pName);
    if (!bLoad)
        return ob;

    if (!ob)
        ob = load_object(pName, 0, 60);
    if (!ob || ob->flags & O_DESTRUCTED)
        return NULL;
    return ob;
}

#if 0

void apply_command(com)
    char *com;
{
    struct value *ret;

    if (command_giver == 0)
        error("command_giver == 0 !\n");
    ret = apply(com, command_giver->super, 0);
    if (ret != 0) {
        add_message("Result:");
        if (ret->type == T_STRING)
            add_message("%s\n", ret->u.string);
        if (ret->type == T_NUMBER)
            add_message("%d\n", ret->u.number);
    } else {
        add_message("Error apply_command: function %s not found.\n", com);
    }
}
#endif /* 0 */


void move_object()
/* Expects inter_sp[-1] being item and inter_sp[0] being dest
 */
{
    struct lambda *l;
    struct object *save_command = command_giver;

    if (NULL != ( l = closure_hook[H_MOVE_OBJECT1].u.lambda) ) {
        l->ob = inter_sp[-1].u.ob;
        call_lambda(&closure_hook[H_MOVE_OBJECT1], 2);
    } else if (NULL != ( l = closure_hook[H_MOVE_OBJECT0].u.lambda) ) {
        l->ob = current_object;
        call_lambda(&closure_hook[H_MOVE_OBJECT0], 2);
    }
    else
        error("Don't know how to move objects.\n");
    command_giver = check_object(save_command);
}

/*
 * Transfer an object.
 * The object has to be taken from one inventory list and added to another.
 */

struct svalue *f_set_environment(sp)
    struct svalue *sp;
{
    struct object *item, *dest;
    struct object **pp, *ob;
    struct object *save_cmd = command_giver;

    if (sp[-1].type != T_OBJECT)
        bad_xefun_arg(1, sp);
    item = sp[-1].u.ob;
    if (item->flags & O_SHADOW && O_GET_SHADOW(item)->shadowing)
        error("Can't move an object that is shadowing.\n");
    if (sp->type != T_OBJECT) {
        if (sp->type != T_NUMBER || sp->u.number)
            bad_xefun_arg(2, sp);
        dest = 0;
    } else {
        dest = sp->u.ob;
        /* Recursive moves are not allowed. */
        for (ob = dest; ob; ob = ob->super)
            if (ob == item)
                error("Can't move object inside itself.\n");

#       ifdef F_SET_LIGHT
            add_light(dest, item->total_light);
#       endif
        dest->flags &= ~O_RESET_STATE;
    }
    item->flags &= ~O_RESET_STATE;
    if (item->super) {
        int okey = 0;

        if (item->sent) {
            remove_environment_sent(item);
        }
        if (item->super->sent)
            remove_sent(item, item->super);
#       ifdef F_SET_LIGHT
            add_light(item->super, - item->total_light);
#       endif
        for (pp = &item->super->contains; *pp;) {
            if (*pp != item) {
                if ((*pp)->sent)
                    remove_sent(item, *pp);
                pp = &(*pp)->next_inv;
                continue;
            }
            *pp = item->next_inv;
            okey = 1;
        }
        if (!okey)
            fatal("Failed to find object %s in super list of %s.\n",
                  item->name, item->super->name);
    }
    item->super = dest;
    if (!dest) {
        item->next_inv = 0;
    } else {
        item->next_inv = dest->contains;
        dest->contains = item;
    }
    command_giver = check_object(save_cmd);
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    return sp - 1;
}

#ifdef F_SET_LIGHT
/*
 * Every object as a count of number of light sources it contains.
 * Update this.
 */

void add_light(p, n)
    struct object *p;
    int n;
{
    if (n == 0)
        return;
    do {
        p->total_light += n;
    } while ( NULL != (p = p->super) );
}
#endif

static struct sentence *sent_free = 0;
static long tot_alloc_sentence;

struct sentence *alloc_sentence() {
    struct sentence *p;

    if (sent_free == 0) {
        p = (struct sentence *)xalloc(sizeof *p);
        if (!p)
            error("Out of memory\n");
        tot_alloc_sentence++;
    } else {
        p = sent_free;
        sent_free = sent_free->next;
    }
    p->verb = NULL;
    p->function = NULL;
    return p;
}

void free_all_sent() {
    struct sentence *p;
    for (;sent_free; sent_free = p) {
        p = sent_free->next;
        xfree((char *)sent_free);
        tot_alloc_sentence--;
    }
}

void free_sentence(p)
    struct sentence *p;
{
    if (p->function)
        free_string(p->function);
    if (p->verb)
        free_string(p->verb);
    p->next = sent_free;
    sent_free = p;
}

void free_shadow_sent(p)
    struct shadow_sentence *p;
{
    p->next = sent_free;
    sent_free = (struct sentence *)p;
}

Bool status_parse(strbuf_t * sbuf, char * buff)
{
    if (sbuf) strbuf_zero(sbuf);
    if (!buff || *buff == 0 || strcmp(buff, "tables") == 0)
    {
        size_t tot, res;
        Bool verbose = MY_FALSE;

        if (strcmp(buff, "tables") == 0)
            verbose = MY_TRUE;
        res = 0;
        if (reserved_user_area)
            res = reserved_user_size;
        if (reserved_master_area)
            res += reserved_master_size;
        if (reserved_system_area)
            res += reserved_system_size;
        if (!verbose)
        {
            strbuf_addf(sbuf, "Sentences:\t\t\t%8ld %8ld\n"
                            , tot_alloc_sentence
                            , tot_alloc_sentence * sizeof (struct sentence));
            strbuf_addf(sbuf, "Objects:\t\t\t%8d %8d (%ld swapped, %ld Kbytes)\n"
                            , tot_alloc_object, tot_alloc_object_size
                            , num_vb_swapped, total_vb_bytes_swapped / 1024);
            strbuf_addf(sbuf, "Arrays:\t\t\t\t%8ld %8ld\n"
                            , (long)num_arrays, total_array_size() );
            strbuf_addf(sbuf, "Mappings:\t\t\t%8ld %8ld\n"
                             , num_mappings, total_mapping_size() );
            strbuf_addf(sbuf, "Prog blocks:\t\t\t%8ld %8ld (%ld swapped, %ld Kbytes)\n"
                            , total_num_prog_blocks + num_swapped - num_unswapped
                            , total_prog_block_size + total_bytes_swapped
                                                    - total_bytes_unswapped
                            , num_swapped - num_unswapped
                            , (total_bytes_swapped - total_bytes_unswapped) / 1024);
            strbuf_addf(sbuf, "Memory reserved:\t\t\t %8d\n", res);
        }
        if (verbose) {
#ifdef COMM_STAT
            strbuf_addf(sbuf
                       , "Calls to add_message: %d   Packets: %d   "
                         "Average packet size: %.2f\n\n"
                       , add_message_calls
                       , inet_packets
                       , inet_packets ? (float)inet_volume/(float)inet_packets : 0.0
            );
#endif
#ifdef APPLY_CACHE_STAT
            strbuf_addf(sbuf
                       , "Calls to apply_low: %ld    "
                         "Cache hits: %ld (%.2f%%)\n\n"
                       , (long)(apply_cache_hit+apply_cache_miss)
                       , (long)apply_cache_hit
                       , 100.*(float)apply_cache_hit/
                         (float)(apply_cache_hit+apply_cache_miss) );
#endif
        }
        tot =  tot_alloc_sentence * sizeof (struct sentence);
        tot += total_prog_block_size;
        tot += total_array_size();
        tot += tot_alloc_object_size;
        if (verbose)
        {
            strbuf_add(sbuf, "\nObject status:\n");
            strbuf_add(sbuf, "--------------\n");
            strbuf_addf(sbuf, "Objects total:\t\t\t %8ld\n"
                             , (long)tot_alloc_object);
            strbuf_addf(sbuf, "Objects in list:\t\t %8ld\n"
                             , (long)num_listed_objs);
            strbuf_addf(sbuf, "Objects processed in last cycle: "
                               "%8ld (%5.1f%% - avg. %5.1f%%)\n"
                       , (long)num_last_processed
                       , (float)num_last_processed / (float)num_listed_objs * 100.0
                       , (avg_in_list || avg_last_processed > avg_in_list)
                         ? 100.0
                         : 100.0 * (float)avg_last_processed / avg_in_list
                       );
        }
        tot += show_otable_status(sbuf, verbose);
        tot += heart_beat_status(sbuf, verbose);
        tot += add_string_status(sbuf, verbose);
        tot += print_call_out_usage(sbuf, verbose);
        tot += total_mapping_size();
#ifdef RXCACHE_TABLE
        tot += rxcache_status(sbuf, verbose);
#endif
        tot += res;

        if (!verbose) {
            strbuf_add(sbuf, "\t\t\t\t\t --------\n");
            strbuf_addf(sbuf, "Total:\t\t\t\t\t %8d\n", tot);
        }
        return MY_TRUE;
    }

    if (strcmp(buff, "swap") == 0) {

        /* maximum seen so far: 10664 var blocks swapped,    5246112 bytes */
        strbuf_addf(sbuf, "%6ld prog blocks swapped,%10ld bytes\n"
                          "%6ld prog blocks unswapped,%8ld bytes\n"
                          "%6ld var blocks swapped,%11ld bytes\n"
                          "%6ld free blocks in swap,%10ld bytes\n"
                          "Swapfile size:%23ld bytes\n"
                    , num_swapped - num_unswapped
                    , total_bytes_swapped - total_bytes_unswapped
                    , num_unswapped, total_bytes_unswapped
                    , num_vb_swapped, total_vb_bytes_swapped
                    , num_swapfree, total_bytes_swapfree
                    , swapfile_size
        );
        strbuf_addf(sbuf, "Total reused space:%18ld bytes\n\n"
                        , total_swap_reused);
        strbuf_addf(sbuf
                   , "Swap: searches: %5ld average search length: %3.1f\n"
                     "Free: searches: %5ld average search length: %3.1f\n"
                   , swap_num_searches
                   , (double)swap_total_searchlength /
                     ( swap_num_searches ? swap_num_searches : 1 )
                   , swap_free_searches
                   , (double)swap_free_searchlength /
                     ( swap_free_searches ? swap_free_searches : 1 )
        );
        return MY_TRUE;
    }

    if (strcmp(buff, "malloc") == 0) {
#if defined(MALLOC_malloc) || defined(MALLOC_smalloc)
        dump_malloc_data(sbuf);
#endif
#ifdef MALLOC_sysmalloc
        strbuf_add(sbuf, "Using system standard malloc.\n");
#endif
        return MY_TRUE;
    }

    return MY_FALSE;
}

#ifdef __STDC__
void fatal(char *fmt, ...)
#else
/*VARARGS1*/
void fatal(fmt, a, b, c, d, e, f, g, h)
    char *fmt;
    int a, b, c, d, e, f, g, h;
#endif
{
#ifdef __STDC__
    va_list va;
#endif
    static int in_fatal = 0;
    /* Prevent double fatal. */
    if (in_fatal)
        abort();
#ifdef __STDC__
    va_start(va, fmt);
#endif
    in_fatal = 1;
    fflush(stdout);
#ifdef __STDC__
    (void)vfprintf(stderr, fmt, va);
#else
    (void)fprintf(stderr, fmt, a, b, c, d, e, f, g, h);
#endif
    fflush(stderr);
    if (current_object)
        (void)fprintf(stderr, "Current object was %s\n",
                      current_object->name);
#ifdef __STDC__
    debug_message(fmt, va);
#else
    debug_message(fmt, a, b, c, d, e, f, g, h);
#endif
    if (current_object)
        debug_message("Current object was %s\n", current_object->name);
    debug_message("Dump of variables:\n");
    (void)dump_trace(MY_TRUE);
    fflush(stdout);
#if !defined(AMIGA) || !defined(__SASC)
    sleep(1); /* let stdout settle down... abort can ignore the buffer... */
#else
    Delay(50);        /* Call Dos.library to wait... */
#endif
#ifdef __STDC__
    va_end(va);
#endif
#if !defined(AMIGA) && !defined(__BEOS__)
    /* we want a core dump, and abort() seems to fail for linux and sun */
    (void)signal(SIGFPE, SIG_DFL);
    *((char*)0) = 0/0;
#endif
    abort();
}

int num_error = 0;
char *current_error, *current_error_file, *current_error_object_name;
mp_int current_error_line_number;

/*
 * This is here because throw constructs its own return value; we dont
 * want to replace it with the system's error string.
 */

void throw_error() {
    unroll_context_stack();
    if (rt_context->type >= ERROR_RECOVERY_CATCH) {
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
        fatal("Throw_error failed!");
    }
    free_svalue(&catch_value);
    catch_value.type = T_INVALID;
    error("Throw with no catch.\n");
}

char *limit_error_format(fixed_fmt, fmt)
    char *fixed_fmt, *fmt;
{
    char *ffptr;

    ffptr = fixed_fmt;
    while (*fmt)
    {
      if ((*ffptr++=*fmt++)=='%')
      {
        if (*fmt == 's')
        {
          *ffptr++ = '.';
          *ffptr++ = '2';
          *ffptr++ = '0';
          *ffptr++ = '0';
        }
      }
    }
    *ffptr = 0;
    return fixed_fmt;
}

static char emsg_buf[2000];

#ifdef __STDC__
void error(char *fmt, ...)
#else
#if SIZEOF_CHAR_P != 4
You need an Ansi C compiler to get this right.
#endif
/*VARARGS1*/
void error(fmt, a, b, c, d, e, f, g, h)
    char *fmt;
    int a, b, c, d, e, f, g, h;
#endif
{
    char *object_name;
    struct object *save_cmd;
    struct svalue *svp;
    int do_save_error;
    char *file, *malloced_error, *malloced_file = 0, *malloced_name = 0;
    char fixed_fmt[200];
    mp_int line_number = 0;
    rt_context_t *rt;
#ifdef __STDC__
    int a;
    va_list va;
#endif

    /* Find the last error recovery context, but do not yet unroll
     * the stack: the current command context might be needed
     * in the runtime error apply.
     */
    for ( rt = rt_context
        ; !ERROR_RECOVERY_CONTEXT(rt->type)
        ; rt = rt->last) NOOP;

    fmt = limit_error_format(fixed_fmt, fmt);
#ifdef __STDC__
    va_start(va, fmt);
#endif
    if (current_object)
        assign_eval_cost();
    if (num_error && rt->type <= ERROR_RECOVERY_APPLY) {
        static char *times_word[] = {
          "",
          "Double",
          "Triple",
          "Quadruple",
        };
        debug_message(
          "%s fault, last error was: %s",
          times_word[num_error],
          emsg_buf + 1
        );
    }
#ifdef __STDC__
    vsprintf(emsg_buf+1, fmt, va);
    va_end(va);
#else
    sprintf(emsg_buf+1, fmt, a, b, c, d, e, f, g, h);
#endif
    emsg_buf[0] = '*';  /* all system errors get a * at the start */
    if (rt->type >= ERROR_RECOVERY_CATCH) {
        /* user catches this error */
        put_malloced_string(&catch_value, string_copy(emsg_buf));
          /* always reallocate */
        debug_message("Caught error: %s", emsg_buf + 1);
        printf("Caught error: %s", emsg_buf + 1);
        dump_trace(MY_FALSE);
        debug_message("... execution continues.\n");
        printf("... execution continues.\n");

        unroll_context_stack();
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
        fatal("Catch() longjump failed");
    }
    num_error++;
    if (num_error > 3)
        fatal("Too many simultaneous errors.\n");
    debug_message("%s", emsg_buf+1);
    do_save_error = 0;
    if ( NULL != (malloced_error = xalloc(strlen(emsg_buf)/* -1 for *, +1 for \0 */)) ) {
        strcpy(malloced_error, emsg_buf+1);
    }
    if (current_object) {
        line_number = get_line_number_if_any(&file);
        debug_message("program: %s, object: %s line %ld\n",
                    file,
                    current_object->name,
                    line_number);
        if (current_prog && num_error < 3) {
            do_save_error = 1;
        }
        if ( NULL != (malloced_file = xalloc(strlen(file) + 1)) ) {
            strcpy(malloced_file, file);
        }
        if ( NULL != (malloced_name = xalloc(strlen(current_object->name) + 1)) ) {
            strcpy(malloced_name, current_object->name);
        }
    }
    object_name = dump_trace(num_error == 3);
    fflush(stdout);
    if (rt->type == ERROR_RECOVERY_APPLY) {
        printf("error in function call: %s", emsg_buf+1);
        if (current_object) {
            printf("program: %s, object: %s line %ld\n",
              file,
              current_object->name,
              line_number
            );
        }
        current_error = malloced_error;
        current_error_file = malloced_file;
        current_error_object_name = malloced_name;
        current_error_line_number = line_number;
        if (out_of_memory) {
            if (malloced_error)
                xfree(malloced_error);
            if (malloced_file)
                xfree(malloced_file);
            if (malloced_name)
                xfree(malloced_name);
        }
        unroll_context_stack();
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
    }
    /*
     * The stack must be brought in a usable state. After the
     * call to reset_machine(), all arguments to error() are invalid,
     * and may not be used any more. The reason is that some strings
     * may have been on the stack machine stack, and have been deallocated.
     */
    reset_machine(MY_FALSE);
    if (do_save_error) {
        save_error(emsg_buf, file, line_number);
    }
    if (object_name) {
        struct object *ob;
        ob = find_object(object_name);
        if (!ob) {
            if (command_giver && num_error < 2)
                add_message("error when executing program in destroyed object %s\n",
                            object_name);
            debug_message("error when executing program in destroyed object %s\n",
                        object_name);
        }
    }
    if (num_error == 3) {
        debug_message("Master failure: %s", emsg_buf+1);
    } else if (!out_of_memory) {
        CLEAR_EVAL_COST;
        RESET_LIMITS;
        push_volatile_string(malloced_error);
        a = 1;
        if (current_object) {
            push_volatile_string(malloced_file);
            push_volatile_string(malloced_name);
            push_number(line_number);
            a += 3;
        }
        save_cmd = command_giver;
        apply_master_ob(STR_RUNTIME, a);
        command_giver = save_cmd;
        if (current_heart_beat) {
            struct object *culprit;

            culprit = current_heart_beat;
            current_heart_beat = 0;
            set_heart_beat(culprit, 0);
            debug_message("Heart beat in %s turned off.\n",
                          culprit->name);
            push_valid_ob(culprit);
            push_volatile_string(malloced_error);
            a = 2;
            if (current_object) {
                push_volatile_string(malloced_file);
                push_volatile_string(malloced_name);
                push_number(line_number);
                a += 3;
            }
            svp = apply_master_ob(STR_HEART_ERROR, a);
            command_giver = save_cmd;
            if (svp && (svp->type != T_NUMBER || svp->u.number) ) {
                set_heart_beat(culprit, 1);
            }
        }
        assigned_eval_cost = eval_cost += MASTER_RESERVED_COST;
    }
    if (malloced_error)
        xfree(malloced_error);
    if (malloced_file)
        xfree(malloced_file);
    if (malloced_name)
        xfree(malloced_name);
    num_error--;
    if (current_interactive) {
        struct interactive *i;

        i = O_GET_INTERACTIVE(current_interactive);
        if (i && i->sent.type == SENT_INTERACTIVE && i->noecho & NOECHO_STALE) {
            set_noecho(i, 0);
        }
    }
    unroll_context_stack();
    if (rt_context->type != ERROR_RECOVERY_NONE)
        longjmp(((struct error_recovery_info *)rt_context)->con.text, 1);
    abort();
}

/* Check that there are not '/../' in the path.
 * TODO: This should go into a 'files' module.
 */
Bool check_no_parentdirs (char *path)
{
    char *p;

    if (path == NULL)
        return MY_FALSE;

    for (p = strchr(path, '.'); p; p = strchr(p+1, '.'))
    {
        if (p[1] != '.')
            continue;
        if ((p[2] == '\0' || p[2] == '/')
         && (p == path    || p[-1] == '/')
           )
            return MY_FALSE;

        /* Skip the next '.' as it's safe to do so */
        p++;
    }
    return MY_TRUE;
}

/*
 * Check that it is a legal relative path. This means no spaces
 * and no '/../' are allowed.
 * TODO: This should go into a 'files' module.
 */
int legal_path(path)
    char *path;
{
    if (path == NULL || strchr(path, ' '))
        return 0;
    if (path[0] == '/')
        return 0;
#ifdef MSDOS_FS
    {
        char *name;

        if (strchr(path,'\\'))
            return 0; /* better save than sorry ... */
        if (strchr(path,':'))
            return 0; /* \B: is okay for DOS .. *sigh* */
        name = strrchr(path,'/');
        if (NULL != name)
            name++;
        else
            name = path;
        if (!strcasecmp(name,"NUL")
         || !strcasecmp(name,"CON")
         || !strcasecmp(name,"PRN")
         || !strcasecmp(name,"AUX")
         || !strcasecmp(name,"COM1")
         || !strcasecmp(name,"COM2")
         || !strcasecmp(name,"COM3")
         || !strcasecmp(name,"COM4")
         || !strcasecmp(name,"LPT1")
         || !strcasecmp(name,"LPT2")
         || !strcasecmp(name,"LPT3")
         || !strcasecmp(name,"LPT4")
           )
            return 0;
    }
#endif

    return check_no_parentdirs(path);
}

/*
 * There is an error in a specific file. Ask the game driver to log the
 * message somewhere.
 */
void smart_log(error_file, line, what, context)
     char *error_file, *what, *context;
     int line;
{
    char buff[500];

    if (error_file == 0)
        return;
    if (strlen(what) + strlen(error_file) > sizeof buff - 100)
        what = "...[too long error message]...";
    if (strlen(what) + strlen(error_file) > sizeof buff - 100)
        error_file = "...[too long filename]...";
    sprintf(buff, "%s line %d %s:%s\n", error_file, line, context, what);
    /* Amylaar: don't reload the master object from yyparse! */
    if (master_ob && !(master_ob->flags & O_DESTRUCTED) ) {
        push_volatile_string(error_file);
        push_volatile_string(buff);
        apply_master_ob(STR_LOG_ERROR, 2);
    }
}

/*
 * Check that a file name is valid for read or write.
 * Also change the name as if the current directory was at the players
 * own directory.
 * This is done by functions in the player object.
 *
 * The master object (master.c) always have permissions to access
 * any file in the mudlib hierarchy, but only inside the mudlib.
 *
 * WARNING: The string returned will (mostly) be deallocated at next
 * call of apply().
 */

/*
 * Check that a path to a file is valid for read or write.
 * This is done by functions in the master object.
 * The path is returned without a leading '/'.
 * If the path was '/', then '.' is returned.
 * The returned string may or may not be residing inside the argument 'path',
 * so don't deallocate arg 'path' until the returned result is used no longer.
 * Otherwise, the returned path is temporarily allocated by apply(), which
 * means it will be dealocated at next apply().
 */
char *check_valid_path(path, caller, call_fun, writeflg)
    char *path;
    struct object *caller;
    char *call_fun;
    Bool writeflg;
{
    struct svalue *v;

    if (path)
        push_string_malloced(path);
    else
        push_number(0);
    {
    struct wiz_list *eff_user;
    if ( NULL != (eff_user = caller->eff_user) )
        push_shared_string(eff_user->name);
    else
        push_number(0);
    }
    push_volatile_string(call_fun);
    push_valid_ob(caller);
    if (writeflg)
        v = apply_master_ob(STR_VALID_WRITE, 4);
    else
        v = apply_master_ob(STR_VALID_READ, 4);
    if (!v || (v->type == T_NUMBER && v->u.number == 0))
        return 0;
    if (v->type != T_STRING) {
        if (!path) {
            debug_message("master returned bogus error file\n");
            return 0;
        }
    } else {
        path = v->u.string;
    }
    if (path[0] == '/')
        path++;
    /* The string "/" will be converted to "." */
    if (path[0] == '\0')
        path = ".";
    if (legal_path(path))
        return path;
    error("Illegal path: %s\n", path);
    return 0;
}

/*
 * This one is called from HUP.
 */
int game_is_being_shut_down = 0;
int master_will_be_updated = 0;

struct svalue *f_shutdown(sp)
    struct svalue *sp;
{
    extra_jobs_to_do = MY_TRUE;
    game_is_being_shut_down = 1;
    return sp;
}

/* this will be activated by SIGUSR1 */
void startmasterupdate() {
    extra_jobs_to_do = MY_TRUE;
    master_will_be_updated = 1;
    eval_cost += max_eval_cost >> 3;
    (void)signal(SIGUSR1, (RETSIGTYPE(*)PROT((int)))startmasterupdate);
}

/*
 * This one is called from the command "shutdown".
 * We don't call it directly from HUP, because it is dangerous when being
 * in an interrupt.
 */
void shutdowngame() {
    apply_master_ob(STR_SHUTDOWN, 0);
    ipc_remove();
    remove_all_players();
    remove_destructed_objects(); /*Will perform the remove_interactive calls*/
    unlink_swap_file();
#ifdef DEALLOCATE_MEMORY_AT_SHUTDOWN
    remove_all_objects();
    free_all_sent();
    remove_wiz_list();
#if defined(MALLOC_malloc) || defined(MALLOC_smalloc)
    dump_malloc_data();
#endif
    find_alloced_data();
#endif
#if defined(AMIGA)
    amiga_end();
#endif
    exit(0);
}

/*
 * Transfer an object from an object to an object.
 * Call add_weight(), drop(), get(), prevent_insert(), add_weight(),
 * and can_put_and_get() where needed.
 * Return 0 on success, and special code on failure:
 *
 * 1: To heavy for destination.
 * 2: Can't be dropped.
 * 3: Can't take it out of it's container.
 * 4: The object can't be inserted into bags etc.
 * 5: The destination doesn't allow insertions of objects.
 * 6: The object can't be picked up.
 */
#ifdef F_TRANSFER
int transfer_object(svp)
    struct svalue *svp;
{
    struct object *ob, *to;
    struct svalue *v_weight, *ret;
    int weight;
    struct object *from;

    ob = svp[0].u.ob;
    to = svp[1].u.ob;
    from = ob->super;
    /*
     * Get the weight of the object
     */
    weight = 0;
    v_weight = sapply(STR_QUERY_WEIGHT, ob, 0);
    if (v_weight && v_weight->type == T_NUMBER)
        weight = v_weight->u.number;
    if (ob->flags & O_DESTRUCTED)
        return 3;
    /*
     * If the original place of the object is a living object,
     * then we must call drop() to check that the object can be dropped.
     */
    if (from && (from->flags & O_ENABLE_COMMANDS)) {
        ret = sapply(STR_DROP, ob, 0);
        if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
            return 2;
        /* This should not happen, but we can not trust LPC hackers. :-) */
        if (ob->flags & O_DESTRUCTED)
            return 2;
    }
    /*
     * If 'from' is not a room and not a player, check that we may
     * remove things out of it.
     */
    if (from && from->super && !(from->flags & O_ENABLE_COMMANDS)) {
        ret = sapply(STR_CANPUTGET, from, 0);
        if (!ret || (ret->type != T_NUMBER && ret->u.number != 1) ||
          (from->flags & O_DESTRUCTED))
            return 3;
    }
    /*
     * If the destination is not a room, and not a player,
     * Then we must test 'prevent_insert', and 'can_put_and_get'.
     */
    if (to->super && !(to->flags & O_ENABLE_COMMANDS)) {
        ret = sapply(STR_PREVENT_INSERT, ob, 0);
        if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
            return 4;
        ret = sapply(STR_CANPUTGET, to, 0);
        if (!ret || (ret->type != T_NUMBER && ret->type != 0) ||
          (to->flags & O_DESTRUCTED) || (ob->flags & O_DESTRUCTED))
            return 5;
    }
    /*
     * If the destination is a player, check that he can pick it up.
     */
    if (to->flags & O_ENABLE_COMMANDS) {
        ret = sapply(STR_GET, ob, 0);
        if (!ret || (ret->type == T_NUMBER && ret->u.number == 0) ||
          (ob->flags & O_DESTRUCTED))
            return 6;
    }
    /*
     * If it is not a room, correct the total weight in the destination.
     */
    if (to->super && weight) {
        /*
         * Check if the destination can carry that much.
         */
        push_number(weight);
        ret = sapply(STR_ADD_WEIGHT, to, 1);
        if (ret && ret->type == T_NUMBER && ret->u.number == 0)
            return 1;
        if (to->flags & O_DESTRUCTED)
            return 1;
    }
    /*
     * If it is not a room, correct the weight in the 'from' object.
     */
    if (from && from->super && weight) {
        push_number(-weight);
        (void)sapply(STR_ADD_WEIGHT, from, 1);
    }
    move_object();
    return 0;
}
#endif /* F_TRANSFER */

/*
 * Call this one when there is only little memory left. It will start
 * Armageddon.
 */
void slow_shut_down(minutes)
    int minutes;
{
    char buf[90];
    sprintf(buf, "slow_shut_down(%d)\n", minutes);
    write(1, buf, strlen(buf));
    push_number(minutes);
    apply_master_ob(STR_SLOW_SHUT, 1);
}

int match_string(match, str, len)
    char *match, *str;
    mp_int len;
{
    for (;;) {
        switch(*match) {
          case '?':
            if (--len < 0)
                return 0;
            str++;
            match++;
            continue;
          case '*':
          {
            char *str2;
            mp_int matchlen;

            for (;;) {
                switch (*++match) {
                  case '\0':
                    return len >= 0;
                  case '?':
                    --len;
                    str++;
                  case '*':
                    continue;
                  case '\\':
                    match++;
                  default:
                    break;
                }
                break;
            }
            if (len <= 0)
                return 0;
            str2 = strpbrk(match + 1, "?*\\");
            if (!str2) {
                if ( (matchlen = strlen(match)) > len)
                    return 0;
                return strncmp(match, str + len - matchlen, matchlen) == 0;
            } else {
                matchlen = str2 - match;
            }
            /* matchlen >= 1 */
            if ((len -= matchlen) >= 0) do {
                if ( !(str2 = memmem(match, matchlen, str, len + matchlen)) )
                    return 0;
                len -= str2 - str;
                if (match_string(match + matchlen, str2 + matchlen, len))
                    return 1;
                str = str2 + 1;
            } while (--len >= 0);
            return 0;
          }
          case '\0':
            return len == 0;
          case '\\':
            match++;
            if (*match == '\0')
                return 0;
            /* Fall through ! */
          default:
            if (--len >= 0 && *match == *str) {
                match++;
                str++;
                continue;
            }
            return 0;
        }
    }
}

/*
 * Credits for some of the code below goes to Free Software Foundation
 * Copyright (C) 1990 Free Software Foundation, Inc.
 * See the GNU General Public License for more details.
 */

static int
isdir (path)
     char *path;
{
  struct stat stats;

  return ixstat (path, &stats) == 0 && S_ISDIR (stats.st_mode);
}

static void
strip_trailing_slashes (path)
     char *path;
{
  int last;

  last = strlen (path) - 1;
  while (last > 0 && path[last] == '/')
    path[last--] = '\0';
}

static struct stat to_stats, from_stats;

static int
copy (from, to)
     char *from, *to;
{
  int ifd;
  int ofd;
  char buf[1024 * 8];
  int len;                        /* Number of bytes read into `buf'. */

  if (!S_ISREG (from_stats.st_mode))
    {
      error ("cannot move `%s' across filesystems: Not a regular file\n", from);
      return 1;
    }

  if (unlink (to) && errno != ENOENT)
    {
      error ("cannot remove `%s'\n", to);
      return 1;
    }

  ifd = ixopen3 (from, O_RDONLY | O_BINARY, 0);
  if (ifd < 0)
    {
      error ("%s: open failed\n", from);
      return errno;
    }
  ofd = ixopen3 (to, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0600);
  if (ofd < 0)
    {
      error ("%s: open failed\n", to);
      close (ifd);
      return 1;
    }
#ifdef HAVE_FCHMOD
  if (fchmod (ofd, from_stats.st_mode & 0777))
    {
      error ("%s: fchmod failed\n", to);
      close (ifd);
      close (ofd);
      unlink (to);
      return 1;
    }
#endif
  FCOUNT_READ(from);
  FCOUNT_WRITE(to);

  while ((len = read (ifd, buf, sizeof (buf))) > 0)
    {
      int wrote = 0;
      char *bp = buf;

      do
        {
          wrote = write (ofd, bp, len);
          if (wrote < 0)
            {
              error ("%s: write failed\n", to);
              close (ifd);
              close (ofd);
              unlink (to);
              return 1;
            }
          bp += wrote;
          len -= wrote;
        } while (len > 0);
    }
  if (len < 0)
    {
      error ("%s: read failed\n", from);
      close (ifd);
      close (ofd);
      unlink (to);
      return 1;
    }

  if (close (ifd) < 0)
    {
      error ("%s: close failed", from);
      close (ofd);
      return 1;
    }
  if (close (ofd) < 0)
    {
      error ("%s: close failed", to);
      return 1;
    }

#ifndef HAVE_FCHMOD
  if (chmod (to, from_stats.st_mode & 0777))
    {
      error ("%s: chmod failed\n", to);
      return 1;
    }
#endif

  return 0;
}

/* Move FROM onto TO.  Handles cross-filesystem moves.
   If TO is a directory, FROM must be also.
   Return 0 if successful, 1 if an error occurred.  */

static int
do_move (from, to)
     char *from;
     char *to;
{
  if (lstat (from, &from_stats) != 0)
    {
      error ("%s: lstat failed\n", from);
      return 1;
    }

  if (lstat (to, &to_stats) == 0)
    {
      if (from_stats.st_dev == to_stats.st_dev
          && from_stats.st_ino == to_stats.st_ino)
        {
          error ("`%s' and `%s' are the same file", from, to);
          return 1;
        }

      if (S_ISDIR (to_stats.st_mode))
        {
          error ("%s: cannot overwrite directory", to);
          return 1;
        }

    }
  else if (errno != ENOENT)
    {
      perror("do_move");
      error ("%s: unknown error\n", to);
      return 1;
    }
#ifndef RENAME_HANDLES_DIRECTORIES
  /* old SYSV */
  if (isdir(from)) {
      char cmd_buf[100];

      if (strchr(from, '\'') || strchr(to, '\''))
        return 0;
      sprintf(cmd_buf, "/usr/lib/mv_dir '%s' '%s'", from, to);
      return system(cmd_buf);
  } else
#endif /* RENAME_HANDLES_DIRECTORIES */
  if (rename (from, to) == 0)
    {
      FCOUNT_DEL(from);
      return 0;
    }

#if !defined(AMIGA)
  if (errno != EXDEV)
    {
      error ("cannot move `%s' to `%s'", from, to);
      return 1;
    }
#endif

  /* rename failed on cross-filesystem link.  Copy the file instead. */

  if (copy (from, to))
      return 1;

  if (unlink (from))
    {
      error ("cannot remove `%s'", from);
      return 1;
    }
  FCOUNT_DEL(from);

  return 0;

}

/*
 * do_rename is used by the efun rename. It is basically a combination
 * of the unix system call rename and the unix command mv. Please shoot
 * the people at ATT who made Sys V.
 */

#ifdef F_RENAME
int
do_rename(fr, t)
    char *fr, *t;
{
    char *from, *to;
    int i;

    from = check_valid_path(fr, current_object, "do_rename", MY_TRUE);
    if(!from)
        return 1;
    push_apply_value();
    to = check_valid_path(t, current_object, "do_rename", MY_TRUE);
    if(!to) {
        pop_apply_value();
        return 1;
    }
    if(!strlen(to) && !strcmp(t, "/")) {
        to = (char *)alloca(3);
        sprintf(to, "./");
    }
    strip_trailing_slashes (from);
    if (isdir (to))
        {
            /* Target is a directory; build full target filename. */
            char *cp;
            char *newto;

            cp = strrchr (from, '/');
            if (cp)
                cp++;
            else
                cp = from;

            newto = (char *) alloca (strlen (to) + 1 + strlen (cp) + 1);
            sprintf (newto, "%s/%s", to, cp);
            pop_apply_value();
            return do_move (from, newto);
        }
    else
        i = do_move (from, to);
        pop_apply_value();
        return i;
}
#endif /* F_RENAME */

struct svalue *f_set_driver_hook(sp)
    struct svalue *sp;
{
    p_int n;
    struct svalue old;

    if (sp[-1].type != T_NUMBER ||
        (n = sp[-1].u.number) < 0 || n > NUM_CLOSURE_HOOKS)
    {
        bad_xefun_arg(1, sp);
    }
    if (!_privilege_violation("set_driver_hook", sp-1, sp)) {
        free_svalue(sp);
        return sp - 2;
    }
    old = closure_hook[n];
    switch(sp->type) {
      case T_NUMBER:
        if (sp->u.number != 0)
            goto bad_arg_2;
        put_number(closure_hook + n, 0);
        break;
      case T_STRING:
      {
        char *str;

        if ( !((1 << T_STRING) & hook_type_map[n]) )
            goto bad_arg_2;
        if ( NULL != (str = make_shared_string(sp->u.string)) ) {
            put_string(closure_hook + n, str);
            if (n == H_NOECHO)
                mudlib_telopts();
        } else {
            error("Out of memory\n");
        }
        break;
      }
      case T_MAPPING:
        if (!sp->u.map->num_values ||
            sp->u.map->ref != 1 /* add_to_mapping() could zero num_values */)
        {
            goto bad_arg_2;
        }
        goto default_test;
      case T_POINTER:
      {
        struct vector *v = sp->u.vec;

        if (v->ref > 1) {
            deref_array(v);
            sp->u.vec = v = slice_array(v, 0, VEC_SIZE(v)-1);
        }
        if (n == H_INCLUDE_DIRS) {
            inter_sp = sp;
            set_inc_list(v);
        }
        goto default_test;
      }
      case T_CLOSURE:
        if (n == H_NOECHO) {
            mudlib_telopts();
        }
        if (sp->x.closure_type == CLOSURE_UNBOUND_LAMBDA &&
            sp->u.lambda->ref == 1)
        {
            closure_hook[n] = *sp;
            closure_hook[n].x.closure_type = CLOSURE_LAMBDA;
            closure_hook[n].u.lambda->ob = master_ob;
            break;
        } else if (!CLOSURE_IS_LFUN(sp->x.closure_type)) {
            goto bad_arg_2;
        } /* else fall through */
      default:
default_test:
        if ( !((1 << sp->type) & hook_type_map[n]) ) {
bad_arg_2:
            bad_xefun_arg(2, sp);
            break; /* flow control hint */
        }
        closure_hook[n] = *sp;
        break;
    }
    if (old.type != T_NUMBER)
        free_closure_hooks(&old, 1);
    return sp - 2;
}

void init_closure_hooks() {
    int i;

    for (i = NUM_CLOSURE_HOOKS; --i >= 0; ) {
        put_number(closure_hook + i, 0);
    }
}

/*
 * May current_object shadow object 'ob' ? We rely heavily on the fact that
 * function names are pointers to shared strings, which means that equality
 * can be tested simply through pointer comparison.
 */
static int validate_shadowing(ob)
    struct object *ob;
{
    int i, j;
    struct object *cob;
    struct program *shadow, *victim;
    struct svalue *ret;

    cob = current_object;
    shadow = cob->prog;
    if (cob->flags & O_DESTRUCTED)
        return 0;
    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory\n");
    victim = ob->prog;
    if (cob->flags & O_SHADOW) {
        struct shadow_sentence *shadow_sent = O_GET_SHADOW(cob);

        if (shadow_sent->shadowing)
            error("shadow: Already shadowing.\n");
        if (shadow_sent->shadowed_by)
            error("shadow: Can't shadow when shadowed.\n");
    }
    if (cob->super)
        error("The shadow must not reside inside another object.\n");
    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing)
        error("Can't shadow a shadow.\n");
    if (ob == cob)
        error("Can't shadow self.\n");
    for (i = shadow->num_function_names; --i >= 0; ) {
        uint32 flags;
        char *name;
        struct program *progp;

        j = shadow->function_names[i];
        flags = shadow->functions[j];
        progp = shadow;
        while (flags & NAME_INHERITED) {
            struct inherit *inheritp;

            inheritp = &progp->inherit[flags & INHERIT_MASK];
            j -= inheritp->function_index_offset;
            progp = inheritp->prog;
            flags = progp->functions[j];
        }
        memcpy(
          (char *)&name,
          progp->program + (flags & FUNSTART_MASK) - 1 - sizeof name,
          sizeof name
        );
        if ( (j = find_function(name, victim)) >= 0 &&
             victim->functions[j] & TYPE_MOD_NO_MASK )
        {
            error("Illegal to shadow 'nomask' function \"%s\".\n", name);
        }
    }
    push_object(ob);
    ret = apply_master_ob(STR_QUERY_SHADOW, 1);
    if (out_of_memory)
        error("Out of memory\n");
    if (!((ob->flags|cob->flags) & O_DESTRUCTED) &&
        ret && !(ret->type == T_NUMBER && ret->u.number == 0))
    {
        return 1;
    }
    return 0;
}

struct svalue *f_shadow(sp)
    struct svalue *sp;
{
    struct object *ob;

    if (sp[-1].type != T_OBJECT)
        bad_xefun_arg(1, sp);
    if (sp->type != T_NUMBER)
        bad_xefun_arg(2, sp);
    sp--;
    ob = sp->u.ob;
    deref_object(ob, "shadow");
    if (sp[1].u.number == 0) {
        ob = ob->flags & O_SHADOW ? O_GET_SHADOW(ob)->shadowed_by : 0;
        if (ob) {
            sp->u.ob = ref_object(ob, "shadow");
        } else {
            put_number(sp, 0);
        }
        return sp;
    }
    sp->type = T_NUMBER; /* validate_shadowing might destruct ob */
    assign_eval_cost();
    inter_sp = sp;
    if (validate_shadowing(ob)) {
        struct shadow_sentence *shadow_sent, *co_shadow_sent;

        /*
         * The shadow is entered first in the chain.
         */
        if ( !(ob->flags & O_SHADOW) ) {
            ob->flags |= O_SHADOW;
            shadow_sent = (struct shadow_sentence *)alloc_sentence();
            shadow_sent->type = SENT_SHADOW;
            shadow_sent->shadowing = 0;
            shadow_sent->shadowed_by = 0; /* in case of later error */
            shadow_sent->ed_buffer = 0;
            shadow_sent->next = ob->sent;
            ob->sent = (struct sentence *)shadow_sent;
        } else {
            shadow_sent = O_GET_SHADOW(ob);
            if (shadow_sent->type == SENT_INTERACTIVE)
            ((struct interactive*)shadow_sent)->catch_tell_activ = MY_TRUE;
        }
        while (shadow_sent->shadowed_by) {
            ob = shadow_sent->shadowed_by;
            shadow_sent = O_GET_SHADOW(ob);
        }
        if ( !(current_object->flags & O_SHADOW) ) {
            current_object->flags |= O_SHADOW;
            /* alloc_sentence can cause out of memory error */
            co_shadow_sent = (struct shadow_sentence *)alloc_sentence();
            co_shadow_sent->type = SENT_SHADOW;
            co_shadow_sent->shadowed_by = 0;
            co_shadow_sent->ed_buffer = 0;
            co_shadow_sent->next = current_object->sent;
            current_object->sent = (struct sentence *)co_shadow_sent;
        } else {
            co_shadow_sent = O_GET_SHADOW(current_object);
        }
        co_shadow_sent->shadowing = ob;
        shadow_sent->shadowed_by = current_object;
        put_ref_object(sp, ob, "shadow");
        return sp;
    }
    put_number(sp, 0);
    return sp;
}

struct svalue *f_query_shadowing(sp)
    struct svalue *sp;
{
    struct object *ob;

    if (sp->type != T_OBJECT)
        bad_xefun_arg(1, sp);
    ob = sp->u.ob;
    deref_object(ob, "shadow");
    ob = ob->flags & O_SHADOW ? O_GET_SHADOW(ob)->shadowing : 0;
    if (ob) {
        sp->u.ob = ref_object(ob, "shadow");
    } else {
        put_number(sp, 0);
    }
    return sp;
}

struct svalue *f_unshadow(sp)
    struct svalue *sp;
{
    struct shadow_sentence *shadow_sent, *shadowing_sent;
    struct object *shadowing, *shadowed_by;

    if (current_object->flags & O_SHADOW &&
        NULL != (shadowing = (shadow_sent = O_GET_SHADOW(current_object))->shadowing) )
    {
        shadowing_sent = O_GET_SHADOW(shadowing);
        shadowed_by = shadow_sent->shadowed_by;
        if ( NULL != (shadowing_sent->shadowed_by = shadowed_by) ) {
            O_GET_SHADOW(shadowed_by)->shadowing = shadow_sent->shadowing;
            shadow_sent->shadowed_by = 0;
        } else {
            if (!shadowing_sent->shadowing && !shadowing_sent->ed_buffer &&
                 shadowing_sent->type == SENT_SHADOW )
            {
                shadowing->sent = shadowing_sent->next;
                shadowing->flags &= ~O_SHADOW;
                free_shadow_sent(shadowing_sent);
            }
        }
        shadow_sent->shadowing = 0;
        if (shadow_sent->type == SENT_SHADOW && !shadow_sent->ed_buffer) {
            current_object->sent = shadow_sent->next;
            current_object->flags &= ~O_SHADOW;
            free_shadow_sent(shadow_sent);
        }
    }
    return sp;
}

/*-------------------------------------------------------------------------*/
static void
extract_limits ( struct limits_context_s * result
               , struct svalue *svp
               , int  num
               , Bool tagged
               )

/* Extract the user-given runtime limits from <svp>...
 * and store them into <result>. If <tagged> is FALSE, <svp> points to an array
 * with the <num> values stored at the proper indices, otherwise <svp> points
 * to a series of <num>/2 (tag, value) pairs.
 *
 * If the function encounters illegal limit tags or values, it throws
 * an error.
 */

{
    char * limitnames[] = { "LIMIT_EVAL", "LIMIT_ARRAY", "LIMIT_MAPPING"
                          , "LIMIT_BYTE", "LIMIT_FILE" };

    /* Set the defaults (unchanged) limits */
    result->max_eval = max_eval_cost;
    result->max_array = max_array_size;
    result->max_mapping = max_mapping_size;
    result->max_byte = max_byte_xfer;
    result->max_file = max_file_xfer;

    if (!tagged)
    {
        p_int val;
        int limit;

        for (limit = 0; limit < LIMIT_MAX && limit < num; limit++)
        {
        
            if (svp[limit].type != T_NUMBER)
                error("Illegal %s value: not a number\n", limitnames[limit]);
                /* TODO: Give type and value */
            val = svp[limit].u.number;
            if (val >= 0)
            {
                switch(limit)
                {
                case LIMIT_EVAL:    result->max_eval = val;    break;
                case LIMIT_ARRAY:   result->max_array = val;   break;
                case LIMIT_MAPPING: result->max_mapping = val; break;
                case LIMIT_BYTE:    result->max_byte = val;    break;
                case LIMIT_FILE:    result->max_file = val;    break;
                default: fatal("Unimplemented limit #%d\n", limit);
                }
            }
            else if (val == LIMIT_DEFAULT)
            {
                switch(limit)
                {
                case LIMIT_EVAL:    result->max_eval = def_eval_cost;
                                    break;
                case LIMIT_ARRAY:   result->max_array = def_array_size;
                                    break;
                case LIMIT_MAPPING: result->max_mapping = def_mapping_size;
                                    break;
                case LIMIT_BYTE:    result->max_byte = def_byte_xfer;
                                    break;
                case LIMIT_FILE:    result->max_file = def_file_xfer;
                                    break;
                default: fatal("Unimplemented limit #%d\n", limit);
                }
            }
            else if (val != LIMIT_KEEP)
                error("Illegal %s value: %ld\n", limitnames[limit], val);
        }
    }
    else
    {
        int i;

        for (i = 0; i < num - 1; i += 2)
        {
            p_int val;
            int limit;

            if (svp[i].type != T_NUMBER)
                error("Illegal limit tag: not a number.\n");
                /* TODO: Give type and value */
            limit = (int)svp[i].u.number;
            if (limit < 0 || limit >= LIMIT_MAX)
                error("Illegal limit tag: %ld\n", (long)limit);

            if (svp[i+1].type != T_NUMBER)
                error("Illegal %s value: not a number\n", limitnames[limit]);
                /* TODO: Give type and value */
            val = svp[i+1].u.number;
            if (val >= 0)
            {
                switch(limit)
                {
                case LIMIT_EVAL:    result->max_eval = val;    break;
                case LIMIT_ARRAY:   result->max_array = val;   break;
                case LIMIT_MAPPING: result->max_mapping = val; break;
                case LIMIT_BYTE:    result->max_byte = val;    break;
                case LIMIT_FILE:    result->max_file = val;    break;
                default: fatal("Unimplemented limit #%d\n", limit);
                }
            }
            else if (val == LIMIT_DEFAULT)
            {
                switch(limit)
                {
                case LIMIT_EVAL:    result->max_eval = def_eval_cost;
                                    break;
                case LIMIT_ARRAY:   result->max_array = def_array_size;
                                    break;
                case LIMIT_MAPPING: result->max_mapping = def_mapping_size;
                                    break;
                case LIMIT_BYTE:    result->max_byte = def_byte_xfer;
                                    break;
                case LIMIT_FILE:    result->max_file = def_file_xfer;
                                    break;
                default: fatal("Unimplemented limit #%d\n", limit);
                }
            }
            else if (val != LIMIT_KEEP)
                error("Illegal %s value: %ld\n", limitnames[limit], val);
        }
    }
} /* extract_limits() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_limited (struct svalue * sp, int num_arg)

/* VEFUN limited()
 *
 *   mixed limited(closure fun)
 *   mixed limited(closure fun, int tag, int value, ...)
 *   mixed limited(closure fun, int * limits [, mixed args...] )
 *
 * Call the function <fun> and execute it with the given runtime limits.
 * After the function exits, the currently active limits are restored.
 * Result of the efun is the result of the closure call.
 *
 * The arguments can be given in two ways: as an array (like the one
 * returned from query_limits(), or as a list of tagged values.
 * If the efun is used without any limit specification, all limits
 * are supposed to be 'unlimited'.
 *
 * The limit settings recognize two special values:
 *     LIMIT_UNLIMITED: the limit is deactivated
 *     LIMIT_KEEP:      the former setting is kept
 *     LIMIT_DEFAULT:   the 'global' default setting is used.
 *
 * The efun causes a privilege violation ("limited", current_object, closure).
 */

{
    struct svalue *argp;
    struct limits_context_s limits;
    int cl_args;

    if (!num_arg)
        error("No arguments given.\n");

    argp = sp - num_arg + 1;
    cl_args = 0;

    if (argp->type != T_CLOSURE)
        bad_xefun_vararg(1, sp);
    
    /* Get the limits */
    if (num_arg == 1)
    {
        limits.max_eval = 0;
        limits.max_array = 0;
        limits.max_mapping = 0;
        limits.max_byte = 0;
        limits.max_file = 0;
    }
    else if (argp[1].type == T_POINTER)
    {
        extract_limits(&limits, argp[1].u.vec->item
                      , (int)VEC_SIZE(argp[1].u.vec)
                      , MY_FALSE);
        cl_args = num_arg - 2;
    }
    else if (num_arg % 2 == 1)
    {
        extract_limits(&limits, argp+1, num_arg-1, MY_TRUE);
        cl_args = 0;
    }
    else
        bad_xefun_vararg(num_arg, sp);

    /* If this object is destructed, no extern calls may be done */
    if (current_object->flags & O_DESTRUCTED
     || !_privilege_violation("limited", argp, sp)
        )
    {
        sp = pop_n_elems(num_arg, sp);
        sp++;
        put_number(sp, 0);
    }
    else
    {
        struct limits_context_s context;
        
        /* Save the current runtime limits and set the new ones */
        save_limits_context(&context);
        context.rt.last = rt_context;
        rt_context = (rt_context_t *)&context;

        max_eval_cost = limits.max_eval;
        max_array_size = limits.max_array;
        max_mapping_size = limits.max_mapping;
        max_byte_xfer = limits.max_byte;
        max_file_xfer = limits.max_file;

        assign_eval_cost();
        inter_sp = sp;
        call_lambda(argp, cl_args);
        sp = inter_sp;
        
        /* Over write the closure with the result */
        free_closure(argp);
        *argp = *sp;
        sp--;

        /* Free the remaining arguments from the efun call */
        sp = pop_n_elems(num_arg - cl_args - 1, sp);

        /* Restore the old limits */
        rt_context = context.rt.last;
        restore_limits_context(&context);
    }

    /* Stack is clean and sp points to the result */
    return sp;
} /* f_limited() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_set_limits (struct svalue * sp, int num_arg)

/* VEFUN set_limits()
 *
 *   void set_limits(int tag, int value, ...)
 *   void set_limits(int * limits)
 *
 * Set the default runtime limits from the given arguments. The new limits
 * will be in effect for the next execution thread.
 *
 * The arguments can be given in two ways: as an array (like the one
 * returned from query_limits(), or as a list of tagged values.
 * The limit settings recognize two special values:
 *     LIMIT_UNLIMITED: the limit is deactivated
 *     LIMIT_KEEP:      the former setting is kept
 *
 * The efun causes a privilege violation ("set_limits", current_object, first
 * arg).
 */

{
    struct svalue *argp;
    struct limits_context_s limits;

    if (!num_arg)
        error("No arguments given.\n");

    argp = sp - num_arg + 1;

    if (num_arg == 1 && argp->type == T_POINTER)
        extract_limits(&limits, argp->u.vec->item, (int)VEC_SIZE(argp->u.vec)
                      , MY_FALSE);
    else if (num_arg % 2 == 0)
        extract_limits(&limits, argp, num_arg, MY_TRUE);
    else
        bad_xefun_vararg(num_arg, sp);

    if (_privilege_violation("set_limits", argp, sp))
    {
        /* Now store the parsed limits into the variables */
        def_eval_cost = limits.max_eval;
        def_array_size = limits.max_array;
        def_mapping_size = limits.max_mapping;
        def_byte_xfer = limits.max_byte;
        def_file_xfer = limits.max_file;
    }

    sp = pop_n_elems(num_arg, sp);
    return sp;
} /* f_set_limits() */

/*-------------------------------------------------------------------------*/
struct svalue *
f_query_limits (struct svalue * sp)

/* TEFUN query_limits()
 *
 *   int * query_limits(int defaults)
 *
 * Return an array with the current runtime limits, resp. if defaults
 * is true, the default runtime limits. The entries in the returned
 * array are:
 *
 *   int[LIMIT_EVAL]:    the max number of eval costs
 *   int[LIMIT_ARRAY]:   the max number of array entries
 *   int[LIMIT_MAPPING]: the max number of mapping entries
 *   int[LIMIT_BYTE]:    the max number of bytes for one read/write_bytes()
 *   int[LIMIT_FILE]:    the max number of bytes for one read/write_file()
 *
 * A limit of '0' means 'no limit'.
 */

{
    struct vector *vec;
    Bool def;
    
    if (sp->type != T_NUMBER)
        bad_xefun_arg(1, sp);
    def = sp->u.number != 0;
    
    vec = allocate_uninit_array(LIMIT_MAX);
    if (!vec)
        error("Out of memory.\n");
    
    put_number(vec->item+LIMIT_EVAL,    def ? def_eval_cost : max_eval_cost);
    put_number(vec->item+LIMIT_ARRAY,   def ? def_array_size : max_array_size);
    put_number(vec->item+LIMIT_MAPPING, def ? def_mapping_size : max_mapping_size);
    put_number(vec->item+LIMIT_BYTE,    def ? def_byte_xfer : max_byte_xfer);
    put_number(vec->item+LIMIT_FILE,    def ? def_file_xfer : max_file_xfer);

    /* No free_svalue: sp is a number */
    put_array(sp, vec);
    return sp;
} /* f_query_limits() */

/***************************************************************************/

