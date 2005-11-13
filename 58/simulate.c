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
#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
#define generic_dirent dirent
#define DIRENT_NLENGTH(dirent) (strlen((dirent)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#define generic_dirent direct
#define DIRENT_NLENGTH(dirent) ((dirent)->d_namlen)
#ifdef SYSNDIR
#include <sys/ndir.h>
#endif /* SYSNDIR */
#ifdef SYSDIR
#include <sys/dir.h>
#endif /* SYSDIR */
#ifdef NDIR
#include <ndir.h>
#endif /* NDIR */
#endif /* not (DIRENT or _POSIX_VERSION) */
#if defined(__CYGWIN__)
extern int lstat PROT((char *, struct stat *));
#endif

#include "simulate.h"

#include "array.h"
#include "backend.h"
#include "call_out.h"
#include "closure.h"
#include "comm.h"
#include "dumpstat.h"
#include "ed.h"
#include "exec.h"
#include "filestat.h"
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
#include "swap.h"
#include "wiz_list.h"

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

#define COMMAND_FOR_OBJECT_BUFSIZE 1000

static void free_sentence PROT((struct sentence *p)); /* forward */
static void remove_sent PROT((struct object *ob, struct object *player));
static void remove_environment_sent PROT((struct object *));
static int special_parse PROT((char *buff));


char *last_verb = 0;
char *inherit_file;
/*
 * 'inherit_file' is used as a flag. If it is set to a string
 * after yyparse(), this string should be loaded as an object,
 * and the original object must be loaded again.
 */
int is_wizard_used = 0;

struct object *obj_list, *master_ob = 0;
p_int new_destructed = 0;

struct object *current_object;      /* The object interpreting a function. */
struct object *command_giver;       /* Where the current command came from. */
struct object *current_interactive; /* The user who caused this execution */
struct object *previous_ob;

int num_parse_error;                /* Number of errors in the parser. */

struct svalue closure_hook[NUM_CLOSURE_HOOKS];

#if 0
struct variable *find_status(str, must_find)
    char *str;
    int must_find;
{
    int i;

    for (i=0; i < current_object->prog->num_variables; i++) {
        if (strcmp(current_object->prog->variable_names[i].name, str) == 0)
            return &current_object->prog->variable_names[i];
    }
    if (!must_find)
        return 0;
    error("--Status %s not found in prog for %s\n", str,
           current_object->name);
    return 0;
}
#endif

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
#ifdef EUIDS
            ob->eff_user = ob->user;
#endif
            pop_stack();        /* deallocate result */
            inter_sp--;         /* skip error context */
            return 1;
#ifdef EUIDS
        } else if (ret->type == T_POINTER && VEC_SIZE(ret->u.vec) == 2 &&
                   ( ret->u.vec->item[0].type == T_STRING
#ifndef NATIVE_MODE
                     || ret->u.vec->item[0].u.number
#endif

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
#endif /* EUIDS */
#ifndef NATIVE_MODE
        } else if (ret->type == T_NUMBER && ret->u.number) {
            ob->user = &default_wizlist_entry;
#ifdef EUIDS
            ob->eff_user = 0;
#endif
            pop_stack();
            inter_sp--;
            return 1;
#endif
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
#ifdef EUIDS
        ob->eff_user = 0;
#endif
        return 1;
    }
    ob->user = add_name("NONAME");
#ifdef EUIDS
    ob->eff_user = ob->user;
#endif
    arg.type = T_OBJECT;
    arg.u.ob = ob;
    destruct_object(&arg);
    error(err);
    /* NOTREACHED */
    return 0;
}

/* Make a given object name sane.
 *
 * The function removes leading '/', a trailing '.c', and folds consecutive
 * '/' into just one '/'. The '.c' removal does not work when given
 * clone object names (i.e. names ending in '#<number>').
 *
 * The function returns a pointer to a static(!) buffer with the cleant
 * up name, or NULL if the given name already was sane.
 */

static const char *
make_name_sane (const char *pName)
{
#   define BUFLEN 500
    static char buf[BUFLEN];
    const char *from = pName;
    char *to;
    short bDiffers = MY_FALSE;

    /* Skip leading '/' */
    while(*from == '/') {
        bDiffers = MY_TRUE;
        from++;
    }

    /* Copy the name into buf, doing the other operations */
    for (to = buf
        ; '\0' != *from && (to - buf) < BUFLEN
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

#ifdef NATIVE_MODE
    if (current_object && current_object->eff_user == 0
        && current_object->name)
        error("Can't load objects when no effective user.\n");
#endif

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
            return ob;

        if (comp_flag)
            fprintf(stderr, " compiling %s ...", fname);
        if (current_file)
            error("Compiler is busy.\n");
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

            tmp = make_name_sane(inherit_file);
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
                error("Error in loading object\n");
            }
            if (strcmp(pInherited, name) == 0) {
                error("Illegal to inherit self.\n");
            }
            if (!depth)
                error("Too deep inheritance nesting.\n");
            ob = load_object(pInherited, 1, depth-1);
            free_string(inter_sp->u.string);
            inter_sp--;
            if (!ob || ob->flags & O_DESTRUCTED)
                error("Inheritance failed\n");
        } /* handling of inherit_file */
    } /* while() - compilation loop */

    if (num_parse_error > 0) {
        error("Error in loading object\n");
    }

    prog = compiled_prog;
#ifdef INITIALIZATION_BY___INIT
    ob = get_empty_object(prog->num_variables);
#else
    ob = get_empty_object( prog->num_variables, prog->variable_names
                         , prog_variable_values);
    for (i = prog->num_variables; --i >= 0; )
        free_svalue(&prog_variable_values[i]);
    xfree((char *)prog_variable_values);
#endif
    if (!ob)
        error("Out of memory\n");
    /*
     * Can we approve of this object ?
     * TODO: Does anybody use this anyway?
     */
    if (approved_object || strcmp(prog->name, "std/object.c") == 0)
        ob->flags |= O_APPROVED;
    ob->name = string_copy(name);        /* Shared string is no good here */
#ifndef COMPAT_MODE
    name--;  /* Make the leading '/' visible again */
#endif
    ob->load_name = make_shared_string(name);  /* but here it is */
    ob->prog = prog;
    ob->ticks = ob->gigaticks = 0;
    ob->next_all = obj_list;
    obj_list = ob;
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

#ifdef NATIVE_MODE
    if (current_object && current_object->eff_user == 0)
        error("Illegal to call clone_object() with effective user 0\n");
#endif
    ob = get_object(str1);

    /*
     * If the object self-destructed...
     */
    if (ob == 0)
        return 0;
    if (ob->super)
        error("Cloning a bad object !\n");
    if (ob->flags & O_CLONE) {
        char c;
        char *p;
        mp_int name_length, i;

        name_length = strlen(ob->name);
        i = name_length;
        p = ob->name+name_length;
        while (--i > 0) {
            /* isdigit would need to check isascii first... */
            if ( (c = *--p) < '0' || c > '9' ) {
                if (c == '#' && name_length - i > 1) {
                    /* would need to use program name to allow it */
                    error("Cloning a bad object !\n");
                }
                break;
            }
        }
    }

    /* We do not want the heart beat to be running for unused copied objects */

    if (ob->flags & O_HEART_BEAT)
        (void)set_heart_beat(ob, 0);
    new_ob = get_empty_object(ob->prog->num_variables
#ifndef INITIALIZATION_BY___INIT
                             , ob->prog->variable_names
                             , ob->variables
#endif
    );
    if (!new_ob)
        error("Out of memory\n");
    new_ob->name = make_new_name(ob->name);
    increment_string_ref(new_ob->load_name = ob->load_name);
    new_ob->flags |= O_CLONE | (ob->flags & ( O_APPROVED | O_WILL_CLEAN_UP )) ;
    new_ob->prog = ob->prog;
    reference_prog (ob->prog, "clone_object");
    new_ob->ticks = new_ob->gigaticks = 0;
#ifdef DEBUG
    if (!current_object)
        fatal("clone_object() from no current_object !\n");
#endif
    new_ob->next_all = obj_list;
    obj_list = new_ob;
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
 * Execute a command for an object. Copy the command into a
 * new buffer, because 'parse_command()' can modify the command.
 * If the object is not current object, static functions will not
 * be executed. This will prevent forcing players to do illegal things.
 *
 * Return cost of the command executed if success (> 0).
 * When failure, return 0.
 */
int command_for_object(str, ob)
    char *str;
    struct object *ob;
{
    char buff[COMMAND_FOR_OBJECT_BUFSIZE];
    int save_eval_cost = eval_cost - 1000;
    struct interactive *ip;

    if (strlen(str) > sizeof(buff) - 1)
        error("Too long command.\n");
    if (ob == 0)
        ob = current_object;
    if (current_object->flags & O_DESTRUCTED || ob->flags & O_DESTRUCTED)
        return 0;
    strncpy(buff, str, sizeof buff);
    buff[sizeof buff - 1] = '\0';
    if (NULL != (ip = O_GET_INTERACTIVE(ob)) && ip->sent.type == SENT_INTERACTIVE)
        trace_level |= ip->trace_level;
    if (parse_command(buff, ob))
        return eval_cost - save_eval_cost;
    else
        return 0;
}

/*
 * To find if an object is present, we have to look in two inventory
 * lists. The first list is the inventory of the current object.
 * The second list is all things that have the same ->super as
 * current_object.
 * Also test the environment.
 * If the second argument 'ob' is non zero, only search in the
 * inventory of 'ob'. The argument 'ob' will be mandatory, later.
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
 * as destructed while still staying in the obj_list, and not really
 * destructed until later. (see destruct2()).
 * This function is also used by destruct() to do the low-level operation.
 */
void emergency_destruct(ob)
    struct object *ob;
{
    struct object **pp, *item, *next;

    if (ob->flags & O_DESTRUCTED)
        return;
    if (ob->flags & O_SWAPPED) {
        int save_privilege;

        save_privilege = malloc_privilege;
        malloc_privilege = MALLOC_SYSTEM;
        load_ob_from_swap(ob);
        malloc_privilege = save_privilege;
    }
    if (ob->flags & O_SHADOW) {
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
        add_light(ob->super, - ob->total_light);
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
    ob->super = 0;
    ob->next_inv = 0;
    ob->contains = 0;
    ob->flags &= ~O_ENABLE_COMMANDS;
    ob->flags |= O_DESTRUCTED;
    new_destructed++;
    if (command_giver == ob) command_giver = 0;
}

/*
 * This one is called when no program is executing from the main loop.
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
            ob->variables[i].type = T_NUMBER;
            ob->variables[i].u.number = 0;
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
        ob->sent = 0;
    }

    free_object(ob, "destruct_object");
}

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
            avoid->item[0].type = T_OBJECT;
            avoid->item[0].u.ob = command_giver;
            add_ref(command_giver, "ass to var");
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

/*
 * This will enable an object to use commands normally only
 * accessible by interactive players.
 * Also check if the player is a wizard. Wizards must not affect the
 * value of the wizlist ranking.
 */

void enable_commands(num)
    int num;
{
    if (current_object->flags & O_DESTRUCTED)
        return;
    if (d_flag > 1) {
        debug_message("Enable commands %s (ref %ld)\n",
            current_object->name, current_object->ref);
    }
    if (num) {
        struct interactive *ip;

        current_object->flags |= O_ENABLE_COMMANDS;
        command_giver = current_object;
        if (NULL != (ip = O_GET_INTERACTIVE(command_giver)) &&
            ip->sent.type == SENT_INTERACTIVE)
        {
            trace_level |= ip->trace_level;
        }
    } else {
        current_object->flags &= ~O_ENABLE_COMMANDS;
        command_giver = 0;
    }
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
    char pattern[PATH_MAX];
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
        free_vector(ecp->v);
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

    path = check_valid_path(path, current_object, "get_dir", 0);

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
            stmp->type = T_STRING;
            stmp->x.string_type = STRING_MALLOC;
            stmp->u.string = string_copy(p);
            stmp++;
        }
        if (mask&2) {
            stmp->type = T_NUMBER;
            stmp->u.number =  (S_IFDIR & st.st_mode) ? -2 : st.st_size;
            stmp++;
        }
        if (mask&4) {
            stmp->type = T_NUMBER;
            stmp->u.number = st.st_mtime;
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
        if ( count >= MAX_ARRAY_SIZE)
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
            free_vector(v);
            free_vector(tmp);
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
            w->item[j].type = T_STRING;
            w->item[j].x.string_type = STRING_MALLOC;
            w->item[j].u.string = name;
            j++;
        }
        if (mask & 2) {
            w->item[j].type = T_NUMBER;
            w->item[j].u.number = de->size;
            j++;
        }
        if (mask & 4) {
            w->item[j].type = T_NUMBER;
            w->item[j].u.number = de->time;
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

    path = check_valid_path(path, current_object, "tail", 0);

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

    path = check_valid_path(path, current_object, "print_file", 0);

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
    path = check_valid_path(path, current_object, "remove_file", 1);

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
lookfor_object(char * str, /* TODO: BOOL */ int bLoad)

/* Look for a named object <str>, optionally loading it (<bLoad> is true).
 * Return a pointer to the object structure, or NULL.
 *
 * If <bLoad> is true, the function tries to load the object if it is
 * not already loaded, or alternatively makes sure it is swapped in.
 * If <bLoad> is false, the function just checks if the object is loaded,
 * but makes to attempt to swap it in (which may cause out-of-memory
 * conditions) or to load it.
 *
 * For easier usage, the macros find_object() and get_object() expand
 * to the no-load- resp. load-call of this function.
 */
{
    struct object *ob;
    const char * pName;

    pName = make_name_sane(str);
    if (!pName)
        pName = str;

    ob = lookup_object_hash((char *)pName);
    if (!bLoad)
        return ob;

    if (!ob)
        ob = load_object(pName, 0, 60);
    if (!ob || ob->flags & O_DESTRUCTED)
        return NULL;
    if (ob->flags & O_SWAPPED)
    {
        if (load_ob_from_swap(ob) < 0)
            error("Out of memory\n");
    }
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

        add_light(dest, item->total_light);
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
        add_light(item->super, - item->total_light);
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

struct svalue *f_set_this_player(sp)
    struct svalue *sp;
{
    struct object *ob;
    struct interactive *ip;

    if (sp->type != T_OBJECT)
        bad_xefun_arg(1, sp);
    ob = sp->u.ob;
    command_giver = ob;
    if (NULL != (ip = O_GET_INTERACTIVE(ob)) &&
        ip->sent.type == SENT_INTERACTIVE)
    {
        trace_level |= ip->trace_level;
    }
    free_object(ob, "set_this_player");
    return sp - 1;
}

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

static struct sentence *sent_free = 0;
static int tot_alloc_sentence;

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
    p->verb = 0;
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

static void free_sentence(p)
    struct sentence *p;
{
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

static struct marked_command_giver {

    struct object *object;
    struct error_recovery_info *erp;
    struct sentence *marker;           /* when at the end of the sentence
                                        * chain, the marker is referenced here.
                                        */
    struct marked_command_giver *next;
} *last_marked = 0;

static void pop_marked_command_giver()
{
    struct marked_command_giver *tmp;

    tmp = last_marked;
    last_marked = tmp->next;
    xfree( (char *)tmp);
}

/*
 * Find the sentence for a command from the player.
 * Return success status.
 */
int player_parser(buff)
    char *buff;
{
    struct sentence *s;
    char *p;
    int length;
    struct object *save_current_object = current_object;
    struct object *save_command_giver  = command_giver;
    char *shared_verb;
    struct sentence *mark_sentence;

#ifdef DEBUG
    if (d_flag > 1)
        debug_message("cmd [%s]: %s\n", command_giver->name, buff);
#endif
    /* strip trailing spaces. */
    for (p = buff + strlen(buff) - 1; p >= buff; p--) {
        if (*p != ' ')
            break;
        *p = '\0';
    }
    if (buff[0] == '\0')
        return 0;
    clear_notify();
    if (special_parse(buff))
        return 1;
    mark_sentence = alloc_sentence();
    length = (int)(p_int)p;
    p = strchr(buff, ' ');
    if (p == 0) {
        length += 1 - (int)(p_int)buff;
        shared_verb = make_shared_string(buff);
    } else {
        *p = '\0';
        shared_verb = make_shared_string(buff);
        *p = ' ';
        length = p - buff;
    }
    {
        /* mark the command_giver as having a sentence of type SENT_MARKER
         * in the current error recovery context.
         */

        struct marked_command_giver *new_marked;

        new_marked = (struct marked_command_giver *)xalloc(sizeof *new_marked);
        new_marked->object = command_giver;
        new_marked->erp = error_recovery_pointer;
        new_marked->marker = 0;
        new_marked->next = last_marked;
        last_marked = new_marked;
    }
    for (s = command_giver->sent; s; s = s->next) {
        struct svalue *ret;
        struct object *command_object;
        unsigned char type;
        struct sentence *next; /* used only as flag */

        if ((type = s->type) == SENT_PLAIN) {
            if (s->verb != shared_verb)
                continue;
        } else if (type == SENT_SHORT_VERB) {
            int len;
            len = strlen(s->verb);
            if (strncmp(s->verb, buff, len) != 0)
                continue;
        } else if (type == SENT_NO_SPACE) {
            int len;
            len = strlen(s->verb);
            if(strncmp(buff, s->verb,len) != 0)
                continue;
        } else if (type == SENT_NO_VERB) {
            /* Give an error only the first time we scan this sentence */
            if (s->short_verb)
                continue;
            s->short_verb++;
            error("An 'action' had an undefined verb.\n");
        } else {
            /* SENT_MARKER ... due to recursion. Or another SENT_IS_INTERNAL */
            continue;
        }
        /*
         * Now we have found a special sentence !
         */
#ifdef DEBUG
        if (d_flag > 1)
            debug_message("Local command %s on %s\n", s->function, s->ob->name);
#endif
        last_verb = shared_verb;
        /*
         * If the function is static and not defined by current object,
         * then it will fail. If this is called directly from player input,
         * then we set current_object so that static functions are allowed.
         * current_object is reset just after the call to apply().
         */
        if (current_object == 0)
            current_object = s->ob;
        /*
         * Remember the object, to update score.
         */
        command_object = s->ob;

        /* if we get an error, we want the verb to be freed */
        mark_sentence->function = shared_verb;
        mark_sentence->verb = 0;
        if ( !(next = s->next) ) {
            mark_sentence->next = 0;
            last_marked->marker = mark_sentence;
            /* Since new commands are always added at the start, the end
             * will remain the end. So there's no need to clear
             * last_marked->marker again.
             */
        } else {
            /* Place the marker, so we can continue the search, no matter what
             * the object does. But beware, if the command_giver is destructed,
             * the marker will be freed.
             * Take care not to alter marker addresses.
             */
            if (next->type == SENT_MARKER) {
                struct sentence *insert;

                do {
                    insert = next;
                    next = next->next;
                    if (!next) {
                        last_marked->marker = mark_sentence;
                        break;
                    }
                } while (next->type == SENT_MARKER);
                if (next)
                    insert->next = mark_sentence;
            } else {
                s->next = mark_sentence;
            }
            mark_sentence->ob = (struct object *)error_recovery_pointer;
            mark_sentence->next = next;
            mark_sentence->type = SENT_MARKER;
        }

        if(s->type == SENT_NO_SPACE) {
            push_volatile_string(&buff[strlen(s->verb)]);
            ret = sapply(s->function, s->ob, 1);
        } else if (buff[length] == ' ') {
            push_volatile_string(&buff[length+1]);
            ret = sapply(s->function, s->ob, 1);
        } else {
            ret = sapply(s->function, s->ob, 0);
        }
        if (ret == 0) {
            error("function %s not found.\n", s->function);
        }
        current_object = save_current_object;
        command_giver  = save_command_giver;
        /* s might be a dangling pointer right now. */
        if (command_giver->flags & O_DESTRUCTED) {
            /* the marker has been freed by destruct_object unless... */
            if (!next) {
                free_sentence(mark_sentence);
            }
            pop_marked_command_giver();
            command_giver = 0;
            last_verb = 0;
            return 1;
        }

        /* remove the marker from the sentence chain, and make s->next valid */
        if ( NULL != (s = mark_sentence->next) && s->type != SENT_MARKER) {
            *mark_sentence = *s;
            s->next = mark_sentence;
            mark_sentence = s;
        } else {
            if (next) {
                /* !s : there have been trailing sentences before, but all
                 * have been removed.
                 * s->type == SENT_MARKER : There was a delimiter sentence
                 * between the two markers, which has been removed.
                 */
                struct sentence **pp;

                for (pp = &command_giver->sent; (s = *pp) != mark_sentence; )
                    pp = &s->next;
                *pp = s->next;
            }
            s = mark_sentence;
        }
        /* If we get fail from the call, it was wrong second argument. */
        if (ret->type == T_NUMBER && ret->u.number == 0) {
            continue;
        }
        if (O_GET_INTERACTIVE(command_giver) &&
            O_GET_INTERACTIVE(command_giver)->sent.type == SENT_INTERACTIVE &&
            !(command_giver->flags & O_IS_WIZARD))
        {
            command_object->user->score++;
        }
        break;
    }
    last_verb = 0;
    /* free marker and verb */
    mark_sentence->verb = 0;
    mark_sentence->function = shared_verb;
    free_sentence(mark_sentence);
    pop_marked_command_giver();
    if (s == 0) {
        notify_no_command(buff);
        return 0;
    }
    return 1;
}

/*
 * Associate a command with function in this object.
 * The optional second argument is the command name. If the command name
 * is not given here, it should be given with add_verb().
 *
 * The optinal third argument is a flag that will state that the verb should
 * only match against leading characters.
 *
 * The object must be near the command giver, so that we ensure that the
 * sentence is removed when the command giver leaves.
 *
 * If the call is from a shadow, make it look like it is really from
 * the shadowed object.
 */
int add_action(func, cmd, flag)
    struct svalue *func, *cmd;
    int flag;
{
    struct sentence *p;
    struct object *ob;
    char *str;
    short string_type;

    if (current_object->flags & O_DESTRUCTED)
        return -1;
    ob = current_object;
    if (ob->flags & O_SHADOW && O_GET_SHADOW(ob)->shadowing) {
        str = findstring(func->u.string);
        do {
            ob = O_GET_SHADOW(ob)->shadowing;
            if (find_function(str, ob->prog) >= 0) {
                if ( !privilege_violation4(
                    "shadow_add_action", ob, str, 0, inter_sp)
                )
                    return -1;
            }
        } while(O_GET_SHADOW(ob)->shadowing);
    }
    if (command_giver == 0 || (command_giver->flags & O_DESTRUCTED))
        return -1;
    if (ob != command_giver && ob->super != command_giver &&
        ob->super != command_giver->super && ob != command_giver->super)
      error("add_action from object that was not present.\n");
#ifdef DEBUG
    if (d_flag > 1)
        debug_message("--Add action %s\n", func->u.string);
#endif
    if (*func->u.string == ':')
        error("Illegal function name: %s\n", func->u.string);
#ifdef COMPAT_MODE
    str = func->u.string;
    if (*str++=='e' && *str++=='x' && *str++=='i' && *str++=='t' && !*str)
        error("Illegal to define a command to the exit() function.\n");
#endif
    p = alloc_sentence();
    str = func->u.string;
    if ((string_type = func->x.string_type) != STRING_SHARED) {
        char *str2;
        str = make_shared_string(str2 = str);
        if (string_type == STRING_MALLOC) {
            xfree(str2);
        }
    }
    p->function = str;
    p->ob = ob;
    if (cmd) {
        str = cmd->u.string;
        if ((string_type = cmd->x.string_type) != STRING_SHARED) {
            char *str2;
            str = make_shared_string(str2 = str);
            if (string_type == STRING_MALLOC) {
                xfree(str2);
            }
        }
        p->verb = str;
        p->type = SENT_PLAIN;
        if (flag) {
            p->type = SENT_SHORT_VERB;
            p->short_verb = flag;
            if (flag == 2)
                p->type = SENT_NO_SPACE;
        }
    } else {
        p->short_verb = 0;
        p->verb = 0;
        p->type = SENT_NO_VERB;
    }
    if (command_giver->flags & O_SHADOW) {
        struct sentence *previous = command_giver->sent;

        p->next = previous->next;
        previous->next = p;
    } else {
        p->next = command_giver->sent;
        command_giver->sent = p;
    }
    return 0;
}

static struct svalue *add_verb(sp, type)
    struct svalue *sp;
    int type;
{
    if (sp->type != T_STRING)
        bad_xefun_arg(1, sp);
    if (command_giver  && !(command_giver->flags & O_DESTRUCTED)) {
        struct sentence *sent;

        sent = command_giver->sent;
        if (command_giver->flags & O_SHADOW)
            sent = sent->next;
        if (!sent)
            error("No add_action().\n");
        if (sent->verb != 0)
            error("Tried to set verb again.\n");
        sent->verb = make_shared_string(sp->u.string);
        sent->type = type;
        if (d_flag > 1)
            debug_message("--Adding verb %s to action %s\n", sp->u.string,
                command_giver->sent->function);
    }
    free_svalue(sp--);
    return sp;
}

struct svalue *f_add_verb(sp)
    struct svalue *sp;
{
    return add_verb(sp, SENT_PLAIN);
}

struct svalue *f_add_xverb(sp)
    struct svalue *sp;
{
    return add_verb(sp, SENT_NO_SPACE);
}

struct svalue *f_remove_action(sp)
    struct svalue *sp;
{
    struct object *ob;
    char *verb;
    struct sentence **sentp, *s;

    if (sp[-1].type != T_STRING)
        bad_xefun_arg(1, sp);
    if (sp->type != T_OBJECT)
        bad_xefun_arg(2, sp);
    ob = sp->u.ob;
    verb = sp[-1].u.string;
    if (sp[-1].x.string_type != STRING_SHARED)
        if ( !(verb = findstring(verb)) )
            verb = (char *)f_remove_action; /* won't be found */
    sentp = &ob->sent;
    ob = current_object;
    while ( NULL != (s = *sentp) ) {
        if (s->ob == ob && s->verb == verb) {
            *sentp = s->next;
            free_sentence(s);
            break;
        }
        sentp = &s->next;
    }
    free_object_svalue(sp);
    sp--;
    free_string_svalue(sp);
    sp->type = T_NUMBER;
    sp->u.number = s != 0;
    return sp;
}

/*
 * Remove all commands (sentences) defined by object 'ob' in object
 * 'player'
 */
static void remove_sent(ob, player)
    struct object *ob, *player;
{
    struct sentence **s;

    for (s= &player->sent; *s;) {
        struct sentence *tmp;
        if ((*s)->ob == ob) {
#ifdef DEBUG
            if (d_flag > 1)
                debug_message("--Unlinking sentence %s\n", (*s)->function);
#endif
            tmp = *s;
            *s = tmp->next;
            free_sentence(tmp);
        } else
            s = &((*s)->next);
    }
}

/*
 * Remove all commands (sentences) defined by objects that have the same
 * environment as object 'player' in object 'player'
 */
static void remove_environment_sent(player)
    struct object *player;
{
    struct sentence **p, *s;
    struct object *super, *ob;

    super = player->super;
    p= &player->sent;
    if ( NULL != (s = *p) ) for(;;) {
        ob = s->ob;
        if (!SENT_IS_INTERNAL(s->type) &&
            ((ob->super == super && ob != player) || ob == super ) )
        {
            do {
                struct sentence *tmp;

#ifdef DEBUG
                if (d_flag > 1)
                    debug_message("--Unlinking sentence %s\n", s->function);
#endif
                tmp = s;
                s = s->next;
                free_sentence(tmp);
                if (!s) {
                    *p = 0;
                    return;
                }
            } while (s->ob == ob);
            *p = s;
        } else {
            do {
                p = &s->next;
                if (!(s = *p)) return;
            } while (s->ob == ob);
        }
    }
}

static void no_op(p, size)
    char *p UNUSED;
    long size UNUSED;
{
#ifdef __MWERKS__
#    pragma unused(p,size)
#endif
}

static void show_memory_block(p, size)
    char *p;
    long size;
{
    add_message(
      "adress: 0x%lx size: 0x%lx '%.*s'\n", (long)p, size, (int)size, p
    );
}

int status_parse(buff)
    char *buff;
{
    if (*buff == 0 || strcmp(buff, "tables") == 0) {
        int tot, res;
        int /* TODO: BOOL */ verbose = MY_FALSE;
#if defined( COMM_STAT ) || defined( APPLY_CACHE_STAT )
        /* passing floats/doubles to add_message is not portable */

        char print_buff[90];
#endif

        if (strcmp(buff, "tables") == 0)
            verbose = MY_TRUE;
        res = 0;
        if (reserved_user_area)
            res = reserved_user_size;
        if (reserved_master_area)
            res += reserved_master_size;
        if (reserved_system_area)
            res += reserved_system_size;
        if (!verbose) {
            add_message("Sentences:\t\t\t%8d %8d\n", tot_alloc_sentence,
                        tot_alloc_sentence * sizeof (struct sentence));
            add_message("Objects:\t\t\t%8d %8d (%ld swapped, %ld Kbytes)\n",
                        tot_alloc_object, tot_alloc_object_size,
                        num_vb_swapped, total_vb_bytes_swapped / 1024);
            add_message("Arrays:\t\t\t\t%8ld %8ld\n", (long)num_arrays,
                        total_array_size() );
            add_message("Mappings:\t\t\t%8ld %8ld\n", num_mappings,
                        total_mapping_size() );
            add_message("Prog blocks:\t\t\t%8ld %8ld (%ld swapped, %ld Kbytes)\n",
                        total_num_prog_blocks, total_prog_block_size,
                        num_swapped - num_unswapped,
                        (total_bytes_swapped - total_bytes_unswapped) / 1024);
            add_message("Memory reserved:\t\t\t %8d\n", res);
        }
        if (verbose) {
#ifdef COMM_STAT
            sprintf(print_buff,
              "Calls to add_message: %d   Packets: %d   Average packet size: %.2f\n\n",
              add_message_calls,
              inet_packets,
              inet_packets ? (float)inet_volume/(float)inet_packets : 0.0
            );
            add_message(print_buff);
#endif
#ifdef APPLY_CACHE_STAT
            sprintf(print_buff,
              "Calls to apply_low: %d Cache hits: %d (%.2f%%)\n\n",
              apply_cache_hit+apply_cache_miss, apply_cache_hit,
              100.*(float)apply_cache_hit/
                (float)(apply_cache_hit+apply_cache_miss) );
            add_message("%s", print_buff);
#endif
        }
        tot =  tot_alloc_sentence * sizeof (struct sentence);
        tot += total_prog_block_size;
        tot += total_array_size();
        tot += tot_alloc_object_size;
        tot += show_otable_status(verbose);
        tot += heart_beat_status(verbose);
        tot += add_string_status(verbose);
        tot += print_call_out_usage(verbose);
        tot += total_mapping_size();
#ifdef RXCACHE_TABLE
        tot += rxcache_status(verbose);
#endif
        tot += res;

        if (!verbose) {
            add_message("\t\t\t\t\t --------\n");
            add_message("Total:\t\t\t\t\t %8d\n", tot);
        }
        return 1;
    }
    if (strcmp(buff, "swap") == 0) {
        char print_buff[128];

        /* maximum seen so far: 10664 var blocks swapped,    5246112 bytes */
        add_message("\
%6ld prog blocks swapped,%10ld bytes\n\
%6ld prog blocks unswapped,%8ld bytes\n\
%6ld var blocks swapped,%11ld bytes\n\
%6ld free blocks in swap,%10ld bytes\n\
Swapfile size:%23ld bytes\n",
          num_swapped - num_unswapped,
          total_bytes_swapped - total_bytes_unswapped,
          num_unswapped, total_bytes_unswapped,
          num_vb_swapped, total_vb_bytes_swapped,
          num_swapfree, total_bytes_swapfree,
          swapfile_size
        );
        sprintf(print_buff, "\
Swap: searches: %5ld average search length: %3.1f\n\
Free: searches: %5ld average search length: %3.1f\n",
          swap_num_searches,
            (double)swap_total_searchlength /
              ( swap_num_searches ? swap_num_searches : 1 ),
          swap_free_searches,
            (double)swap_free_searchlength /
              ( swap_free_searches ? swap_free_searches : 1 )
        );
        add_message("\
Total reused space:%18ld bytes\n\
\n%s",
          total_swap_reused, print_buff
        );
        return 1;
    }
    return 0;
}

/*
 * Hard coded commands, that will be available to all players. They can not
 * be redefined, so the command name should be something obscure, not likely
 * to be used in the game.
 */

#ifdef DEBUG
static char debug_parse_buff[50]; /* Used for debugging */
#endif

int first_showsmallnewmalloced_call = 1;

static int special_parse(buff)
    char *buff;
{
    struct interactive *ip;
    struct svalue *svp;

#ifdef DEBUG
    strncpy(debug_parse_buff, buff, sizeof debug_parse_buff);
    debug_parse_buff[sizeof debug_parse_buff - 1] = '\0';
#endif
    if (strcmp(buff, "malloc") == 0) {
#if defined(MALLOC_malloc) || defined(MALLOC_smalloc)
        dump_malloc_data();
#endif
#ifdef MALLOC_gmalloc
        add_message("Using Gnu malloc.\n");
#endif
#ifdef MALLOC_sysmalloc
        add_message("Using system standard malloc.\n");
#endif
        return 1;
    }
    if (!is_wizard_used || command_giver->flags & O_IS_WIZARD) {
        if (strcmp(buff, "dumpallobj") == 0) {
            dumpstat();
            return 1;
        }
#ifdef OPCPROF /* amylaar */
        if (strcmp(buff,  "opcdump") == 0) {
            opcdump();
            return 1;
        }
#endif
#if defined(MALLOC_malloc) || defined(MALLOC_smalloc)
        if (strcmp(buff,  "showsmallnewmalloced") == 0) {

#if !defined(DEBUG) || defined(SHOWSMALLNEWMALLOCED_RESTRICTED)
            struct svalue *arg;
            push_constant_string("inspect memory");
            arg = apply_master_ob(STR_PLAYER_LEVEL, 1);
            if (arg && (arg->type != T_NUMBER || arg->u.number != 0))
#endif
            {
                if (first_showsmallnewmalloced_call) {
                    add_message("No previous call. please redo.\n");
                    walk_new_small_malloced(no_op);
                    first_showsmallnewmalloced_call = 0;
                } else {
                    walk_new_small_malloced(show_memory_block);
                }
            }
            return 1;
        }
        if (strcmp(buff, "debugmalloc") == 0) {
            debugmalloc = !debugmalloc;
            if (debugmalloc)
                add_message("On.\n");
            else
                add_message("Off.\n");
            return 1;
        }
#endif /* MALLOC_(s)malloc */
        if (strncmp(buff, "status", 6) == 0)
            return status_parse(buff+6+(buff[6]==' '));
    } /* end of wizard-only special parse commands */
    if (NULL != (ip = O_GET_INTERACTIVE(command_giver)) &&
        ip->sent.type == SENT_INTERACTIVE &&
        ip->modify_command )
    {
        struct object *ob = ip->modify_command;

        if (ob->flags & O_DESTRUCTED) {
            ip->modify_command = 0;
            free_object(ob, "modify_command");
            return 0;
        }
        if (closure_hook[H_MODIFY_COMMAND_FNAME].type != T_STRING)
            return 0;
        push_volatile_string(buff);
        svp = sapply(closure_hook[H_MODIFY_COMMAND_FNAME].u.string, ob, 1);
        /* !command_giver means that the command_giver has been destructed. */
        if (!svp) return 0;
        if (!command_giver) return 1;
    } else {
        if (closure_hook[H_MODIFY_COMMAND].type == T_CLOSURE) {
            struct lambda *l;

            l = closure_hook[H_MODIFY_COMMAND].u.lambda;
            if (closure_hook[H_MODIFY_COMMAND].x.closure_type == CLOSURE_LAMBDA)
                l->ob = command_giver;
            push_volatile_string(buff);
            push_object(command_giver);
            call_lambda(&closure_hook[H_MODIFY_COMMAND], 2);
            transfer_svalue(svp = &apply_return_value, inter_sp--);
            if (!command_giver) return 1;
        } else if (closure_hook[H_MODIFY_COMMAND].type == T_STRING) {
            if (command_giver->flags & O_DESTRUCTED)  /* just in case... */
                return 0;
            push_volatile_string(buff);
            svp =
              sapply(closure_hook[H_MODIFY_COMMAND].u.string, command_giver, 1);
            if (!svp) return 0;
            if (!command_giver) return 1;
        } else if (closure_hook[H_MODIFY_COMMAND].type == T_MAPPING) {
            struct svalue sv;

            if ( NULL != (sv.u.string = findstring(buff)) ) {
                sv.type = T_STRING;
                sv.x.string_type = STRING_SHARED;
                svp =
                  get_map_lvalue(closure_hook[H_MODIFY_COMMAND].u.map, &sv, 0);
                if (svp->type == T_CLOSURE) {
                    push_shared_string(sv.u.string);
                    push_object(command_giver);
                    call_lambda(svp, 2);
                    transfer_svalue(svp = &apply_return_value, inter_sp--);
                    if (!command_giver) return 1;
                }
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }
    if (svp->type == T_STRING) {
        strncpy(buff, svp->u.string, COMMAND_FOR_OBJECT_BUFSIZE-1);
        buff[COMMAND_FOR_OBJECT_BUFSIZE-1] = '\0';
    } else if (svp->type == T_NUMBER) {
        return svp->u.number;
    }
    return 0;
}

struct vector *get_action(ob, verb)
    struct object *ob;
    char *verb;
{
    struct vector *v;
    struct sentence *s;
    struct svalue *p;

    if ( !(verb = findstring(verb)) ) return NULL;
    for (s = ob->sent; s; s = s->next) {
        if (verb != s->verb) continue;
        /* verb will be 0 for SENT_MARKER */

        v = allocate_array(4);
        p = v->item;

        p->u.number = s->type;
        p++;
        p->u.number = s->type != SENT_PLAIN ? s->short_verb : 0;
        p++;
        p->type = T_OBJECT;
        add_ref((p->u.ob = s->ob), "get_action");
        p++;
        p->type = T_STRING;
        p->x.string_type = STRING_SHARED;
        increment_string_ref(p->u.string = s->function);

        return v;
    }
    /* not found */
    return NULL;
}

struct vector *get_all_actions(ob, mask)
    struct object *ob;
    int mask;
{
    struct vector *v;
    struct sentence *s;
    int num;
    struct svalue *p;
    int nqueries;

    nqueries = ((mask>>1) & 0x55) + (mask & 0x55);
    nqueries = ((nqueries>>2) & 0x33) + (nqueries & 0x33);
    nqueries = ((nqueries>>4) & 0x0f) + (nqueries & 0x0f);
    num = 0;
    for (s = ob->sent; s; s = s->next) {
        if (SENT_IS_INTERNAL(s->type))
            continue;
        num += nqueries;
    }

    v = allocate_array(num);
    p = v->item;
    for (s = ob->sent; s; s = s->next)
    {
        if (SENT_IS_INTERNAL(s->type))
            continue;
        if (mask & 1) {
            if ( NULL != (p->u.string = s->verb) ) {
                increment_string_ref(p->u.string);
                p->type = T_STRING;
                p->x.string_type = STRING_SHARED;
            }
            p++;
        }
        if (mask & 2) {
            p->u.number = s->type;
            p++;
        }
        if (mask & 4) {
            p->u.number = s->short_verb;
            p++;
        }
        if (mask & 8) {
            p->type = T_OBJECT;
            add_ref((p->u.ob = s->ob), "get_action");
            p++;
        }
        if (mask & 16) {
            p->type = T_STRING;
            p->x.string_type = STRING_SHARED;
            increment_string_ref(p->u.string = s->function);
            p++;
        }
    }

    return v;
}

struct vector *get_object_actions(ob1, ob2)
    struct object *ob1;
    struct object *ob2;
{
    struct vector *v;
    struct sentence *s;
    int num;
    struct svalue *p;

    num = 0;
    for (s = ob1->sent; s; s = s->next)
        if (s->ob == ob2) num += 2;

    v = allocate_array(num);
    p = v->item;
    for (s = ob1->sent; s; s = s->next)
    {
        if (s->ob == ob2) {
            increment_string_ref(p->u.string = s->verb);
            p->type = T_STRING;
            p->x.string_type = STRING_SHARED;
            p++;

            p->type = T_STRING;
            p->x.string_type = STRING_SHARED;
            increment_string_ref(p->u.string = s->function);
            p++;
        }
    }
    return v;
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
    (void)dump_trace(1);
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
 * Error() has been "fixed" so that users can catch and throw them.
 * To catch them nicely, we really have to provide decent error information.
 * Hence, all errors that are to be caught
 * (error_recovery_pointer->type == ERROR_RECOVERY_CATCH)
 * construct a string containing the error message, which is returned as the
 * thrown value.  Users can throw their own error values however they choose.
 */

static void remove_command_giver_markers()
{
    struct marked_command_giver *m;

    while ( NULL != (m = last_marked) && m->erp == error_recovery_pointer) {
        if (m->marker) {
            free_sentence(m->marker);
        } else {
            remove_sent( (struct object *)error_recovery_pointer, m->object);
        }
        last_marked = m->next;
        xfree( (char *)m);
        /* We have freed the shared string pointed to by last_verb above,
         * thus it is invalid. Besides, this function is called when we
         * longjmp out of player_parser(), thus last_verb should be cleared.
         */
        last_verb = 0;
    }
}

/*
 * This is here because throw constructs its own return value; we dont
 * want to replace it with the system's error string.
 */

void throw_error() {
    if (error_recovery_pointer->type >= ERROR_RECOVERY_CATCH) {
        remove_command_giver_markers();
        longjmp(error_recovery_pointer->con.text, 1);
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
#if SIZEOF_P_INT != 4
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
#ifdef __STDC__
    int a;
    va_list va;
#endif

    fmt = limit_error_format(fixed_fmt, fmt);
#ifdef __STDC__
    va_start(va, fmt);
#endif
    if (current_object)
        assign_eval_cost();
    remove_command_giver_markers();
    if (num_error && error_recovery_pointer->type <= ERROR_RECOVERY_APPLY) {
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
    if (error_recovery_pointer->type >= ERROR_RECOVERY_CATCH) {
        /* user catches this error */
        catch_value.type = T_STRING;
        catch_value.x.string_type = STRING_MALLOC;        /* Always reallocate */
        catch_value.u.string = string_copy(emsg_buf);
        longjmp(error_recovery_pointer->con.text, 1);
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
    if (error_recovery_pointer->type == ERROR_RECOVERY_APPLY) {
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
        longjmp(error_recovery_pointer->con.text, 1);
    }
    /*
     * The stack must be brought in a usable state. After the
     * call to reset_machine(), all arguments to error() are invalid,
     * and may not be used any more. The reason is that some strings
     * may have been on the stack machine stack, and have been deallocated.
     */
    reset_machine (0);
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
        assigned_eval_cost = eval_cost -= MASTER_RESERVED_COST;
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
    if (error_recovery_pointer->type != ERROR_RECOVERY_NONE)
        longjmp(error_recovery_pointer->con.text, 1);
    abort();
}

/* Check that there are not '/../' in the path.
 * TODO: This should go into a 'files' module.
 */
/* TODO: BOOL */ int check_no_parentdirs (char *path)
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
        if (!strncasecmp(name,"NUL", 3)
         || !strncasecmp(name,"CON", 3)
         || !strncasecmp(name,"PRN", 3)
         || !strncasecmp(name,"AUX", 3)
         || !strncasecmp(name,"COM1", 4)
         || !strncasecmp(name,"COM2", 4)
         || !strncasecmp(name,"COM3", 4)
         || !strncasecmp(name,"COM4", 4)
         || !strncasecmp(name,"LPT1", 4)
         || !strncasecmp(name,"LPT2", 4)
         || !strncasecmp(name,"LPT3", 4)
         || !strncasecmp(name,"LPT4", 4)
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
    int writeflg;
{
    struct svalue *v;

    if (path)
        push_string_malloced(path);
    else
        push_number(0);
    {
#ifdef EUIDS
    struct wiz_list *eff_user;
    if ( NULL != (eff_user = caller->eff_user) )
        push_shared_string(eff_user->name);
    else
#endif
        push_number(0);
    }
    push_constant_string(call_fun);
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
    add_eval_cost(-initial_eval_cost >> 3);
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
#ifdef OPCPROF
    opcdump();
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

    from = check_valid_path(fr, current_object, "do_rename", 1);
    if(!from)
        return 1;
    push_apply_value();
    to = check_valid_path(t, current_object, "do_rename", 1);
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
    if (_privilege_violation("set_driver_hook", sp-1, sp) <= 0) {
        free_svalue(sp);
        return sp - 2;
    }
    old = closure_hook[n];
    switch(sp->type) {
      case T_NUMBER:
        if (sp->u.number != 0)
            goto bad_arg_2;
        closure_hook[n].type = T_NUMBER;
        closure_hook[n].u.lambda = 0;
        break;
      case T_STRING:
      {
        char *str;

        if ( !((1 << T_STRING) & hook_type_map[n]) )
            goto bad_arg_2;
        if ( NULL != (str = make_shared_string(sp->u.string)) ) {
            closure_hook[n].u.string = str;
            closure_hook[n].type = T_STRING;
            closure_hook[n].x.string_type = STRING_SHARED;
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
            v->ref--;
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
    if (old.type == T_CLOSURE && old.x.closure_type == CLOSURE_LAMBDA)
        old.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
    free_svalue(&old);
    return sp - 2;
}

void init_closure_hooks() {
    int i;

    for (i = NUM_CLOSURE_HOOKS; --i >= 0; ) {
        closure_hook[i].type = T_NUMBER;
        closure_hook[i].u.lambda = 0;
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
    decr_object_ref(ob, "shadow");
    if (sp[1].u.number == 0) {
        ob = ob->flags & O_SHADOW ? O_GET_SHADOW(ob)->shadowed_by : 0;
        if (ob) {
            add_ref(ob, "shadow");
            sp->u.ob = ob;
        } else {
            sp->type = T_NUMBER;
            sp->u.number = 0;
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
        sp->type = T_OBJECT;
        sp->u.ob = ob;
        add_ref(ob, "shadow");
        return sp;
    }
    sp->u.number = 0;
    return sp;
}

struct svalue *f_query_shadowing(sp)
    struct svalue *sp;
{
    struct object *ob;

    if (sp->type != T_OBJECT)
        bad_xefun_arg(1, sp);
    ob = sp->u.ob;
    decr_object_ref(ob, "shadow");
    ob = ob->flags & O_SHADOW ? O_GET_SHADOW(ob)->shadowing : 0;
    if (ob) {
        add_ref(ob, "shadow");
        sp->u.ob = ob;
    } else {
        sp->type = T_NUMBER;
        sp->u.number = 0;
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
