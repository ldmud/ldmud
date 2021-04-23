/*---------------------------------------------------------------------------
 * Object handling.
 *
 *---------------------------------------------------------------------------
 * Objects are the core of LPMud: everything a players sees, handles and
 * is, is an object. Unfortunately this also means that a lot of object
 * related functions are scattered all over the driver, so this file is
 * actually quite short.
 *
 * The first part deals with the creation and deallocation of objects
 * and programs. The second parts implements a pointer table which can
 * be used to sequentialize the set of arrays and mappings, detecting
 * cycles and shared usages while doing this. The last and largest part
 * are the functions to save an object into a save file, and restoring
 * it from there.
 *
 * -- object_t --
 *
 *   object_t {
 *       unsigned short  flags;
 *       p_int           ref;
 *       mp_int          time_reset;
 *       mp_int          time_of_ref;
 *       mp_int          time_cleanup;
 *       mp_int          load_time;
 *       p_int           load_id;
 *       p_int           extra_ref;            (ifdef DEBUG)
 *       program_t     * prog;
 *       string_t      * name;
 *       string_t      * load_name;
 *       object_t      * next_all;
 *       object_t      * prev_all;
 *       object_t      * next_hash;
 *       object_t      * next_inv;
 *       object_t      * contains;
 *       object_t      * super;
 * TODO: The environment members (plus light) could be put into a special
 * TODO:: sentence and thus concentrated in a separated source file.
 *       sentence_t    * sent;
 *       wiz_list_t    * user;
 *       wiz_list_t    * eff_user;
 *       Bool            open_sqlite_db (ifdef USE_SQLITE)
 *       int             extra_num_variables;  (ifdef DEBUG)
 *       svalue_t      * variables;
 *       unsigned long   ticks, gigaticks;
 *   }
 *
 * The .flags collect some vital information about the object:
 *     O_HEART_BEAT       : the object has a heartbeat
 *     O_ENABLE_COMMANDS  : can execute commands ("is a living")
 *     O_CLONE            : is a clone, or uses a replaced program
 *     O_DESTRUCTED       : has actually been destructed
 *     O_SWAPPED          : program and/or variables have been swapped out
 *     O_ONCE_INTERACTIVE : is or was interactive
 *     O_RESET_STATE      : is in a virgin resetted state
 *     O_WILL_CLEAN_UP    : call clean_up() when time is due
 *     O_LAMBDA_REFERENCED: a reference to a lambda was taken; this may
 *                          inhibit a replace_program().
 *     O_SHADOW           : object is shadowed
 *     O_REPLACED         : program was replaced.
 *
 * .ref counts the number of references to this object: it is this count
 * which can keep a destructed object around. Destructed objects are
 * stripped of everything but the basic object_t, but this one
 * is kept until the last reference is gone.
 *
 * .time_of_ref is the time() of the last apply on this object. The swapper
 * uses this timestamp to decide whether to swap the object or not.
 *
 * .time_cleanup is the time() when the next variable cleanup is due.
 *
 * Similar, .time_reset is the time() when the object should be reset
 * again. A time of 0 means: never.
 * The timing is not strict: any time after the given time is
 * sufficient. A reset object has its O_RESET_STATE flag set, which is
 * reset in an apply. If the time of reset is reached, but the object
 * is still in a reset state, or it is swapped out, the backend simply
 * sets a new .time_reset time, but does not do any real action.
 * To reduce the lag caused by the reset calls, all objects are kept
 * in the reset_table sorted by their time_reset. The .next_reset pointer
 * is used to built the table.
 *
 * .load_time simply is the time when the object was created. .load_id
 * serves to determine the creation order of objects created at the
 * same .load_time - this is used mostly for efuns like clones().
 *
 * .prog is a pointer to the program_tm, the bunch of bytecode for
 * this object. The program is shared between the master object (the
 * blueprint) and its clones. It is possible to replace the program
 * of a single object with a different one, but special care has
 * to be taken if lambda closures have been created.
 *
 * .variables is the block of variables of this object. Obviously they
 * can't be shared between master and clones. The number of variables
 * implicitly known by the program.
 *
 * .name and .load_name are the two names of the object. .name (an untabled
 * string) is the objects 'real' name: something like "std/thing" for
 * a blueprint, and "std/thing#45" for a clone. This name never has
 * a leading '/'. However, this name can be changed with the efun
 * rename_object().
 *
 * The .load_name (a tabled string) is the name of the file from which
 * the object was created. It is identical in both blueprint and clones
 * (in our example "/std/thing") and can't be changed. In compat mode,
 * this name has no leading '/'. However, for virtual objects .load_name
 * is the virtual name - the real program name is .prog->name.
 *
 * Both .name and .load_name never contain a '\0' as part of the name.
 *
 * .sent is the list of annotations to the object. Primary use is to
 * hold the list of commands ("sentences") defined by this object. Just
 * the first entry in this list has a special role: if the object is
 * shadowed, interactive, or using the editor, it is a "shadow_t"
 * and keeps the list of shadows resp. the other information.
 *
 * .user points to the wizlist entry of the wizard who 'owns' this
 * object. The entry is used to collect several stats for this user.
 * .eff_user describes the rights of this object. .eff_user can be
 * NULL, while .user can't.
 *
 * .ticks and .gigaticks count how much time the interpreter spent
 * in this particular object. The number is kept in two variables
 * to prevent overflows, it can be computed as gigaticks * 1E9 + ticks.
 *
 * .next_all, .prev_all and .next_hash are used to store the object.
 * .next_all and .prev_all are the link pointers in the list of all
 * objects, .next_hash is the link pointer in the object table (see otable.c).
 *
 * The gamedriver implements an environment/inventory system. .super
 * points to an object's surrounding object (and can be NULL), .contains
 * is the head of the list of contained objects. This inventory list
 * is linked by the .next_inv pointer.
 *
 * .extra_ref and .extra_num_variables are used by check_a_lot_of_refcounts().
 *
 *
 * A word about swapping: when an object is not in use for longer time,
 * the driver swaps out the program and/or the objects variables, and the
 * swapper assigns an even 'swapnum' each. If an object is swapped, the
 * O_SWAPPED flag is set and the affected pointer (.prog or .variables)
 * is replaced by the assigned swap_num _with the lowest bit set_. Since
 * pointers are assumed to always be even, this allows to distinguish
 * swapped programs/variables from unswapped ones.
 *
 * The exact structure of programs and variables is explained in exec.h .
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>
#include <assert.h>

#include "object.h"

#include "actions.h"
#include "array.h"
#include "backend.h"
#include "closure.h"
#include "comm.h"
#include "filestat.h"
#include "interpret.h"
#include "instrs.h"
#include "lex.h"
#include "main.h"
#include "mapping.h"
#include "mempools.h"
#include "mstrings.h"
#include "otable.h"
#include "prolang.h"
#include "ptrtable.h"
#include "random.h"
#include "sent.h"
#include "simulate.h"
#include "simul_efun.h"
#include "stdstrings.h"
#include "strfuns.h"
#include "structs.h"
#include "swap.h"
#include "svalue.h"
#include "wiz_list.h"
#include "xalloc.h"

#include "pkg-python.h"

#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/functionlist.h"
#include "../mudlib/sys/include_list.h"
#include "../mudlib/sys/inherit_list.h"

/*-------------------------------------------------------------------------*/

replace_ob_t *obj_list_replace = NULL;
  /* List of scheduled program replacements.
   */

long tot_alloc_object = 0;
long tot_alloc_object_size = 0;
  /* Total number of allocated object, and the sum of memory they use.
   */

object_t NULL_object = { 0 };
  /* static null object for initialisations. memset() is not sufficient
   * because some machines (e.g. Bull) have a (char*)0 which is not
   * binary zero. Structure assignment otoh works.
   */

Bool dest_last_ref_gone = MY_FALSE;
  /* This flag is set to TRUE if the second-to-last reference to
   * a destructed is removed, as this usually means that it can be removed
   * from the list of destructed objects and deallocated altogether.
   * The flag is used to avoid unnecessary scans of the list of destructed
   * objects by the backend in the meantime.
   */

/*-------------------------------------------------------------------------*/
#ifndef CHECK_OBJECT_REF
void
dealloc_object (object_t *ob)
#else
void
dealloc_object ( object_t *ob, const char * file, int line)
#endif

/* Deallocate/dereference all memory and structures held by <ob>.
 * At the time of call, the object must be have at no refcount left,
 * must be destructed and removed from the object table and lists.
 */

{
#ifdef DEBUG

    /* Check the reference count */

    if (ob->ref > 0)
        fatal("Object with %"PRIdPINT" refs passed to _free_object()\n", 
              ob->ref);

#if 0 && defined(CHECK_OBJECT_REF)
    if (strchr(get_txt(ob->name), '#') == NULL)
        printf("DEBUG: (%s:%d) free_object(%p '%s') ref %"PRIdPINT" flags %x\n"
              , file, line, ob, get_txt(ob->name), ob->ref, ob->flags);
#elif defined(CHECK_OBJECT_REF)
#   ifdef __MWERKS__
#       pragma unused(file)
#       pragma unused(line)
#   endif
#endif
    if (d_flag)
        printf("%s free_object: %s.\n", time_stamp(), get_txt(ob->name));

    /* Freeing a non-destructed object should never happen */

    if (!(ob->flags & O_DESTRUCTED)) {
        fatal("Object %p %s ref count 0, but not destructed.\n"
             , ob, get_txt(ob->name));
    }

#endif /* DEBUG */

    if (ob->sent)
        fatal("free_object: Object '%s' (ref %"PRIdPINT", flags %08x) "
              "still has sentences.\n"
             , get_txt(ob->name), ob->ref, ob->flags);

#ifdef USE_PYTHON
    if (ob->python_dict != NULL)
        python_free_object(ob);
#endif

    /* If the program is freed, then we can also free the variable
     * declarations.
     */
    if (ob->prog)
    {
        program_t *prog = ob->prog;
#ifdef CHECK_OBJECT_STAT
        if (check_object_stat)
        {
            fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) free( %p '%s') with %hu vars : %"PRIuPINT" -> (%ld:%ld)\n"
                          , tot_alloc_object, tot_alloc_object_size, ob, ob->name ? get_txt(ob->name) : "<null>"
                          , prog->num_variables
                          , (p_uint)(prog->num_variables * sizeof (svalue_t) + sizeof (object_t))
                          , tot_alloc_object
                          , (tot_alloc_object_size - (prog->num_variables * sizeof (svalue_t) + sizeof (object_t)))
                          );
        }
#endif
        tot_alloc_object_size -=
            prog->num_variables * sizeof (svalue_t) +
            sizeof (object_t);
        free_prog(prog, MY_TRUE);
        ob->prog = NULL;
    }
#ifdef CHECK_OBJECT_STAT
    else
    {
        if (check_object_stat)
        {
            fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) free( %p '%s') has no program\n"
                          , tot_alloc_object, tot_alloc_object_size, ob, ob->name ? get_txt(ob->name) : "<null>");
        }
    }
#endif

    /* Deallocate the name */
    if (ob->name)
    {
        if (d_flag > 1)
            debug_message("%s Free object %s\n", time_stamp(), get_txt(ob->name));
        if (lookup_object_hash(ob->name) == ob)
            fatal("Freeing object %s but name still in name table\n"
                 , get_txt(ob->name));
#ifdef CHECK_OBJECT_STAT
        if (check_object_stat)
        {
            fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) free( %p '%s') with name : %zu -> (%ld:%ld)\n"
                          , tot_alloc_object, tot_alloc_object_size, ob, get_txt(ob->name)
                          , mstrsize(ob->name)
                          , tot_alloc_object-1
                          , tot_alloc_object_size - (mstrsize(ob->name)+1)
                          );
        }
#endif
        tot_alloc_object_size -= mstrsize(ob->name);
        free_mstring(ob->name);
        ob->name = NULL;
    }
#ifdef CHECK_OBJECT_STAT
    else
    {
        if (check_object_stat)
        {
            fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) free( %p ) has no name -> (%ld:%ld)\n"
                          , tot_alloc_object, tot_alloc_object_size, ob
                          , tot_alloc_object-1
                          , tot_alloc_object_size);
        }
    }
#endif

    /* Dereference the load_name */
    if (ob->load_name)
    {
        free_mstring(ob->load_name);
        ob->load_name = NULL;
    }

    /* Free the object structure */
    tot_alloc_object--;
    xfree(ob);
} /* dealloc_object() */

/*-------------------------------------------------------------------------*/
object_t *
get_empty_object (int num_var)

/* Allocate a new, empty object with <numvar> variables (set to 0)
 * and return it.
 * Return NULL when out of memory.
 */

{
static mp_int last_time = 0;
static mp_int last_id = 0;

    object_t *ob;
    size_t size = sizeof (object_t);
    size_t size2 = num_var * sizeof (svalue_t);
    int i;
    svalue_t *ob_vars;

    /* Allocate the object structure */

    if ( !(ob = xalloc(size)) )
        return NULL;

    ob_vars = NULL;

    /* Allocated the variable block */

    if (size2 && !(ob_vars = xalloc(size2)) )
    {
        xfree(ob);
        return NULL;
    }

#ifdef CHECK_OBJECT_STAT
    if (check_object_stat)
    {
        fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) new( %p ) with %d vars : %zu -> (%ld:%"PRIuPINT")\n"
                      , tot_alloc_object, tot_alloc_object_size, ob
                      , num_var
                      , (size2+size)
                      , tot_alloc_object+1
                      , (p_uint)tot_alloc_object_size + size + size2
                      );
    }
#endif
    tot_alloc_object++;
    tot_alloc_object_size += size + size2;

    /* Clear and initialise the object (no memset!) */

    *ob = NULL_object;
    ob->ref = 1;
    ob->load_time = current_time;
    if (last_time == current_time)
        ob->load_id = ++last_id;
    else
    {
        ob->load_id = last_id = 0;
        last_time = current_time;
    }
#ifdef DEBUG
    ob->extra_num_variables = num_var;
#endif
#ifdef USE_SQLITE
    ob->open_sqlite_db = MY_FALSE;
#endif
#ifdef USE_PYTHON
    ob->python_dict = NULL;
#endif

    ob->variables = ob_vars;

    ob->time_cleanup = current_time + time_to_data_cleanup;

    /* Initialize the variables */

    for (i = num_var; --i >= 0; )
    {
        ob_vars[i] = const0;
    }

    /* That's it. */
    return ob;
}  /* get_empty_object() */

/*-------------------------------------------------------------------------*/
void
init_object_variables (object_t *ob, object_t *templ)

/* The variables of object <ob> are initialized.
 *
 * First, if <ob> is a clone, all variables marked as !VAR_INITIALIZED are
 * copied over from the <templ>, if given. <templ> MUST be the blueprint
 * object.
 * 
 * Then, for all <ob>, __INIT() is called in <ob> which initializes all
 * the variables marked as VAR_INITIALIZED in clones, and all variables
 * in blueprints.
 */

{
    /* For clones, copy the shared variable values */
    if ((ob->flags & O_CLONE))
    {
        int i;
        variable_t *p_vars;
        svalue_t *ob_vars, *templ_vars;

        if (!templ || templ->flags & O_DESTRUCTED)
            templ_vars = NULL;
        else
            templ_vars = templ->variables;

        ob_vars = ob->variables;
        p_vars = ob->prog->variables;

        for (i = ob->prog->num_variables; --i >= 0; )
        {
            if (p_vars[i].type.t_flags & VAR_INITIALIZED)
                continue;
            if (!templ_vars)
                errorf("Can't initialize object '%s': no blueprint given.\n"
                     , get_txt(ob->name));
            assign_svalue_no_free(&ob_vars[i], &templ_vars[i]);
        }
    }

#ifdef USE_PYTHON
    python_call_hook_object(PYTHON_HOOK_ON_OBJECT_CREATED, false, ob);
#endif /* USE_PYTHON */

    /* Initialized all other variables programmatically */
    sapply_ign_prot(STR_VARINIT, ob, 0);
} /* init_object_variables() */

/*-------------------------------------------------------------------------*/
#ifdef DEALLOCATE_MEMORY_AT_SHUTDOWN

void
remove_all_objects (void)

/* Call destruct_object() for every object on the object list, then
 * call remove_destructed_objects() to actually remove them.
 *
 * This function is called from simulate.c when the game is shut down.
 */

{
    object_t *ob;
    svalue_t v;

    v.type = T_OBJECT;
    for (ob = obj_list; ob; ob = ob->next_all)
    {
#ifdef DEBUG
        if (ob->flags & O_DESTRUCTED) /* TODO: Can't happen */
            continue;
#endif
        v.u.ob = ob;
        destruct_object(&v);
        if ( !(ob->flags & O_DESTRUCTED) )
            break;
    }
    remove_destructed_objects(MY_TRUE);
}

#endif

/*-------------------------------------------------------------------------*/
void
reference_prog (program_t *progp, char *from)

/* Increment the refcount of program <progp>, called from location <from>.
 */

{
    progp->ref++;
    if (d_flag)
        printf("%s reference_prog: %s ref %"PRIdPINT" (%s)\n"
              , time_stamp(), get_txt(progp->name), progp->ref, from);
}

/*-------------------------------------------------------------------------*/
void
do_free_sub_strings (int num_strings,   string_t **strings
                    ,int num_variables, variable_t *variables
                    ,int num_includes,  include_t *includes
                    ,int num_structs,  struct_def_t *struct_defs
                    )

/* Free a bunch of shared strings used in connection with an object:
 * the <num_strings> strings in the array <strings>,
 * the <num_variables> names and type objects of the vars in array <variables>, 
 * the <num_includes> names of the includes in array <includes>,
 * the <num_structs> names of the struct defs in array <struct_defs>, and.
 *
 * The function is called from free_prog() and from the compiler epilog().
 */

{
    int i;

    /* Free all strings */
    for (i = 0; i < num_strings; i++)
        free_mstring(strings[i]);

    /* Free all variable names and types */
    for (i = num_variables; --i >= 0; )
    {
        free_mstring(variables[i].name);
        free_fulltype(variables[i].type);
    }

    /* Free all include names */
    for (i = num_includes; --i >= 0; )
    {
        free_mstring(includes[i].name);
        free_mstring(includes[i].filename);
    }

    /* Free all struct names */
    for  (i = num_structs; --i >= 0; )
    {
        free_struct_type(struct_defs[i].type);
    }
}

/*-------------------------------------------------------------------------*/
#ifndef CHECK_OBJECT_REF
void
free_prog (program_t *progp, Bool free_all)
#else
void
_free_prog (program_t *progp, Bool free_all, const char * file, int line
           )
#endif

/* Decrement the refcount for program <progp>. If it reaches 0, the program
 * is freed.
 *
 * If free_all is TRUE, all object strings and the blueprint reference are
 * freed, and free_prog() is called for all inherited programs.
 *
 * The only case when free_all is not true, is, when the swapper
 * swapped out the program and now attempts to free the memory.
 * This means that the string data is kept in memory all the time.
 * TODO: Swapping the strings is tricky, as they are all shared.
 * TODO:: Maybe swap them together with the variables - this is costly
 * TODO:: enough to make the lookup time needed when swapping in the
 * TODO:: strings look small.
 */

{
    /* Decrement the refcount */

    progp->ref--;
    if (progp->ref > 0)
        return;

#if 0 && defined(CHECK_OBJECT_REF)
    if (strchr(get_txt(progp->name), '#') == NULL)
        printf("DEBUG: (%s:%d) free_prog(%p '%s') ref %"PRIdPINT"\n"
              , file, line, progp, get_txt(progp->name), progp->ref);
#endif
    if (d_flag)
        printf("%s free_prog: %s\n", time_stamp(), get_txt(progp->name));
    if (progp->ref < 0)
        fatal("Negative ref count (%"PRIdPINT") for prog ref "
              "(program %p '%s').\n",
              progp->ref, progp, get_txt(progp->name));

    if (free_all && progp->blueprint)
    {
        object_t * blueprint = progp->blueprint;
        progp->blueprint = NULL;
        remove_prog_swap(progp, MY_TRUE);
#if 0 && defined(CHECK_OBJECT_REF)
    if (strchr(get_txt(blueprint->name), '#') == NULL)
        printf("DEBUG: (%s:%d) free_prog(%p '%s') ref %"PRIdPINT" : "
               "blueprint (%p '%s') ref %"PRIdPINT", flags %hx\n"
              , file, line, progp, get_txt(progp->name), progp->ref
              , blueprint, get_txt(blueprint->name), blueprint->ref, blueprint->flags);
#elif defined(CHECK_OBJECT_REF)
#   ifdef __MWERKS__
#       pragma unused(file)
#       pragma unused(line)
#   endif
#endif
        free_object(blueprint, "free_prog");
    }

    /* Update the statistics */
    total_prog_block_size -= progp->total_size;
    total_num_prog_blocks -= 1;

    /* Free the line numbers.
     *
     * This has to be done before the program is removed from the
     * swapper, else the following test would fail.
     */
    if (progp->line_numbers)
    {
        total_prog_block_size -= progp->line_numbers->size;
        xfree(progp->line_numbers);
        progp->line_numbers = NULL;
    }

    /* Is it a 'real' free? Then dereference all the
     * things held by the program, too.
     */
    if (free_all)
    {
        int i;

        /* Remove the swap entry */
        remove_prog_swap(progp, MY_FALSE);

        /* Free all function names. */
        for (i = progp->num_function_headers; --i >= 0; )
        {
            free_mstring(progp->function_headers[i].name);
            free_lpctype(progp->function_headers[i].type);
        }

        /* Free all argument types. */
        for (i = progp->num_argument_types; --i >= 0; )
            free_lpctype(progp->argument_types[i]);

        /* Free the strings, variable names and include filenames. */
        do_free_sub_strings( progp->num_strings, progp->strings
                           , progp->num_variables, progp->variables
                           , progp->num_includes, progp->includes
                           , progp->num_structs, progp->struct_defs
                           );

        /* Free all inherited objects */
        for (i = 0; i < progp->num_inherited; i++)
            free_prog(progp->inherit[i].prog, MY_TRUE);

        /* Free the program name */
        total_prog_block_size -= mstrsize(progp->name);
        free_mstring(progp->name);
    }

    /* Remove the program structure */
    xfree(progp);
} /* free_prog() */

/*-------------------------------------------------------------------------*/
static string_t *
function_exists (string_t *fun, object_t *ob, Bool show_hidden
                , string_t ** prog_name, uint32 * prog_line
                , int * num_arg,  uint32 * fun_flags
                , lpctype_t ** fun_type
                )

/* Search for the function <fun> in the object <ob>. If existing, return
 * the name of the program (without added reference), if not return NULL.
 *
 * If <prog_name> and <prog_line> are both non-NULL, they are set to
 * the name of the program _file_ and the line where the function is found.
 * The program file name will have one reference added.
 *
 * *<num_arg>, *<fun_flags>,  *<fun_type> are set to the number of
 * arguments, the function flags and the function return type respectively.
 *
 * Visibility rules apply: static and protected functions can't be
 * found from the outside unless <show_hidden> is true.
 */

{
    string_t *shared_name;
    bytecode_p funstart;
    program_t *progp;
    function_t  *header;
    int ix;
    funflag_t flags;

#ifdef DEBUG
    if (ob->flags & O_DESTRUCTED)
        fatal("function_exists() on destructed object\n");
#endif

    memset(fun_type, 0, sizeof(*fun_type));
    *num_arg = 0;
    *fun_flags = 0;

    if (prog_name)
        *prog_name = NULL;

    /* Make the program resident */
    if (O_PROG_SWAPPED(ob))
    {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
            errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));
    }

    shared_name = find_tabled(fun);
    progp = ob->prog;

    /* Check if the function exists at all */
    if ( (ix = find_function(shared_name, progp)) < 0)
        return NULL;

    /* Is it visible for the caller? */
    flags = progp->functions[ix];
    *fun_flags = (flags & ~INHERIT_MASK);

    if (!show_hidden
     && (   flags & TYPE_MOD_PRIVATE
         || (flags & TYPE_MOD_STATIC && current_object != ob))
       )
        return NULL;

    /* Resolve inheritance */
    while (flags & NAME_INHERITED)
    {
        inherit_t *inheritp;

        inheritp = &progp->inherit[flags & INHERIT_MASK];
        ix -= inheritp->function_index_offset;
        progp = inheritp->prog;
        flags = progp->functions[ix];
    }

    funstart = progp->program  + (flags & FUNSTART_MASK);
    header = progp->function_headers + FUNCTION_HEADER_INDEX(funstart);

    /* Set the additional information */
    *num_arg = header->num_arg;
    *fun_type = header->type;

    /* And after all this, the function may be undefined */
    if (is_undef_function(funstart))
    {
        *fun_flags |= NAME_UNDEFINED;
        return NULL;
    }

    if(prog_line && prog_name)
      *prog_line = get_line_number(funstart, progp, prog_name);

    /* We got it. */
    return progp->name;
} /* function_exists() */

/*-------------------------------------------------------------------------*/
void
reset_object (object_t *ob, int arg)

/* Depending on <arg>, call one of the initialisation functions in <ob>.
 * The actual function is given in <arg> through its hook index.
 * Accepted values are: H_RESET, H_CREATE_SUPER, H_CREATE_OB,
 *   H_CREATE_CLONE.
 *
 * The value of the hooks can be function names (strings) or closures.
 *
 * For strings, the name is the function called in <ob>. It gets passed
 * one argument: 0 for H_CREATE_*, 1 for H_RESET. If on a H_RESET call
 * the function can not be found, the object will never be reset again.
 *
 * For closures, the code distinguishes closures which take no arguments
 * (only for H_CREATE_* calls) from those which take at least one argument.
 * In the former case, the closure is bound to <ob> and called; in the
 * latter case, the closure is bound to the current object and gets <ob>
 * passed as argument. If the closure returns a numeric result, it is
 * used as the time delay before the next reset.
 *
 * If the delay to the next (resp. first) reset is not determined by
 * the called function, it is set to a random value between time_to_reset/2
 * and time_to_reset. Upon time of call, the object must not be
 * in the reset table; this function will enter it there.
 */

{
    /* Be sure to update time first ! */
    if (time_to_reset > 0)
        ob->time_reset = current_time + time_to_reset/2
                         + (mp_int)random_number((uint32)time_to_reset/2);

    if (driver_hook[arg].type == T_CLOSURE)
    {
        lambda_t *l;

        if (arg == H_RESET)
            previous_ob = current_object = ob;

        l = driver_hook[arg].u.lambda;
        free_object(l->ob, "reset_object");
        if (l->function.code.num_arg && arg != H_RESET)
        {
            /* closure accepts arguments, presumably one, so
             * give it the target object and bind to the current
             * object.
             */
            l->ob = ref_object(current_object, "reset_object");
            push_ref_object(inter_sp, ob, "reset");
            call_lambda(&driver_hook[arg], 1);
        }
        else
        {
            /* no arguments, just bind to target */
            l->ob = ref_object(ob, "reset_object");
            call_lambda(&driver_hook[arg], 0);
        }

        /* If the call returned a non-zero number, use it as the current
         * reset interval, overwriting the default set above.
         */
        if (inter_sp->type == T_NUMBER && inter_sp->u.number)
            ob->time_reset = (inter_sp->u.number > 0)
                             ? current_time + inter_sp->u.number
                             : 0;

        pop_stack();
    }
    else if (driver_hook[arg].type == T_STRING)
    {
        if (arg == H_RESET)
            previous_ob = current_object = ob;

        push_number(inter_sp, arg == H_RESET);
        if (!sapply_ign_prot(driver_hook[arg].u.str, ob, 1)
         && arg == H_RESET)
            ob->time_reset = 0;
    }

    /* Object is reset now */
    ob->flags |= O_RESET_STATE;
} /* reset_object() */

/*-------------------------------------------------------------------------*/
void
logon_object (object_t *ob, p_int flag)

/* Call the logon() lfun in the object <ob> with the argument <flag>.
 *
 * current_object is temporarily set to <ob> in order to allow logon()
 * to be static (security measure). Doing so is harmless as there is no
 * previous_object to consider.
 */

{
    svalue_t *ret;
    object_t *save = current_object;

    current_object = ob;
    mark_start_evaluation();

    /* Only pass the flag, when != 0. */
    if (flag)
        push_number(inter_sp, flag);
    ret = apply(STR_LOGON, ob, flag ? 1 : 0);
    if (ret == 0)
    {
        errorf("Could not find %s() on the player %s\n", get_txt(STR_LOGON), 
               get_txt(ob->name));
        /* NOTREACHED */
    }
    mark_end_evaluation();
    current_object = save;
} /* logon_object() */

/*-------------------------------------------------------------------------*/
void
replace_programs (void)

/* Called from the backend loop, this function
 * performs all pending program replacements listed in obj_list_replace.
 *
 * If the function runs out of memory, the processing ends at that point
 * and will be retried in the next call.
 *
 * Sideeffects of this action are: the objects are marked as 'replaced',
 * and current shadows are removed.
 */

{
    replace_ob_t *r_ob, *r_next;  /* List pointers */
    svalue_t *svp;

#ifdef DEBUG
    if (d_flag)
        debug_message("%s start of replace_programs\n", time_stamp());
#endif

    for (r_ob = obj_list_replace; r_ob; r_ob = r_next)
    {
        program_t *old_prog;
        int newinhcount;

        /* Don't bother with destructed objects. */
        if (r_ob->ob->flags & O_DESTRUCTED)
        {
            r_next = r_ob->next;
            if (r_ob->lambda_rpp)
                free_replace_program_protector(r_ob);
#ifdef USE_PYTHON
            if (r_ob->python_rpp)
                python_free_replace_program_protector(r_ob);
#endif
            xfree(r_ob);
            continue;
        }

        /* Swap in the program. This can't fail when called during
         * a garbage collection because then the malloc privilege
         * is MALLOC_SYSTEM.
         */
        if (r_ob->ob->flags & O_SWAPPED && load_ob_from_swap(r_ob->ob) < 0)
        {
            obj_list_replace = r_ob;
            return; /* Hope for more memory next time... */
        }

        /* If the new program has variables, we need to create a new
         * variable block. Even if the number doesn't change, the
         * virtual variable blocks may have a different order.
         */
        if (r_ob->new_prog->num_variables)
        {
            svalue_t *new_vars;
            program_t *oldprog = r_ob->ob->prog;

            /* Get the memory */
            new_vars = xalloc(r_ob->new_prog->num_variables * sizeof *new_vars);
            if (!new_vars)
            {
                obj_list_replace = r_ob;
                return; /* Hope for more memory next time... */
            }

            svp = r_ob->ob->variables; /* the old variables */

            /* First we are going to copy the virtual variables by inherit,
             * then en-bloc all remaining non-virtual variables.
             */
            newinhcount = r_ob->new_prog->num_inherited;
            for (inherit_t *newinh = r_ob->new_prog->inherit; newinhcount > 0; newinh++, newinhcount--)
            {
                inherit_t *oldinh;

                if (newinh->inherit_type == INHERIT_TYPE_NORMAL)
                    continue; /* Non-virtual come later. */

                if (newinh->inherit_type & INHERIT_TYPE_DUPLICATE)
                    continue; /* We already had that one. */

                /* Search for the corresponding inherit in the old program. */
                for (oldinh = oldprog->inherit;
                     oldinh->prog != newinh->prog || oldinh->inherit_type == INHERIT_TYPE_NORMAL;
                     oldinh++);

                if (newinh->inherit_type & INHERIT_TYPE_MAPPED)
                {
                    /* An obsolete inherit. It also be an obsoleted inherit
                     * in the to-be-replaced program. So just copy any
                     * additional variables.
                     */

                    assert(oldinh->inherit_type & INHERIT_TYPE_MAPPED);

                    for (int varidx = 0; varidx < oldinh->num_additional_variables; varidx++)
                    {
                        new_vars[newinh->variable_index_offset + varidx] = svp[oldinh->variable_index_offset + varidx];
                        svp[oldinh->variable_index_offset + varidx].type = T_INVALID;
                    }
                }
                else
                {
                    /* A regular virtual inherit. */
                    if (oldinh->inherit_type & INHERIT_TYPE_MAPPED)
                    {
                        /* That virtual inherit is obsolete in the program we want to replace.
                         * We have to reconstruct the variable block using the variable map.
                         */
                        inherit_t *update = oldprog->inherit + oldinh->updated_inherit;
                        int updcount = update->prog->num_variables - update->prog->num_virtual_variables;

                        for (int varidx = newinh->prog->num_variables - newinh->prog->num_virtual_variables; varidx-- > 0;)
                        {
                            int updidx = oldprog->update_index_map[varidx + oldinh->variable_map_offset];
                            if (updidx >= updcount)
                            {
                                new_vars[newinh->variable_index_offset + varidx] = svp[oldinh->variable_index_offset + updidx - updcount];
                                svp[oldinh->variable_index_offset + updidx - updcount].type = T_INVALID;
                            }
                            else
                            {
                                new_vars[newinh->variable_index_offset + varidx] = svp[update->variable_index_offset + updidx];
                                svp[update->variable_index_offset + updidx].type = T_INVALID;
                            }
                        }
                    }
                    else
                    {
                        for (int varidx = newinh->prog->num_variables - newinh->prog->num_virtual_variables; varidx-- > 0;)
                        {
                            new_vars[newinh->variable_index_offset + varidx] = svp[oldinh->variable_index_offset + varidx];
                            svp[oldinh->variable_index_offset + varidx].type = T_INVALID;
                        }
                    }
                }
            }

            /* Now the non-virtual variables. */
            for (int varidx = r_ob->new_prog->num_variables - r_ob->new_prog->num_virtual_variables; varidx-- > 0;)
            {
                new_vars[r_ob->new_prog->num_virtual_variables + varidx] = svp[r_ob->var_offset + varidx];
                svp[r_ob->var_offset + varidx].type = T_INVALID;
            }

            /* Deref those variables of ob which we haven't copied. */
            for (int varidx = oldprog->num_variables; varidx-- > 0; svp++)
            {
                free_svalue(svp);
            }

            /* Free the old variable block and set the new one */
            xfree(r_ob->ob->variables);
            r_ob->ob->variables = new_vars;
        }
        else
        {
            /* No variables. */
            xfree(r_ob->ob->variables);
            r_ob->ob->variables = NULL;
        }

        if (r_ob->ob->prog->num_variables != r_ob->new_prog->num_variables)
        {
            int diff = r_ob->ob->prog->num_variables - r_ob->new_prog->num_variables;

#ifdef DEBUG
            if (d_flag)
                debug_message("%s %d less variables\n", time_stamp(), diff);
            r_ob->ob->extra_num_variables = r_ob->new_prog->num_variables;
#endif

            /* Adjust the statistics */
#ifdef CHECK_OBJECT_STAT
            if (check_object_stat)
            {
                fprintf(stderr, "DEBUG: OSTAT: (%ld:%ld) rprog( %p '%s') sub %d vars : %"PRIuPINT" -> (%ld:%ld)\n"
                              , tot_alloc_object, tot_alloc_object_size, r_ob, r_ob->ob->name ? get_txt(r_ob->ob->name) : "<null>"
                              , diff
                              , (p_uint)(diff * sizeof(svalue_t))
                              , tot_alloc_object, tot_alloc_object_size - (diff * sizeof(svalue_t))
                              );
            }
#endif
            tot_alloc_object_size -= diff * sizeof(svalue_t);
        }

        /* Handle a possible closure adjustment */
        if (r_ob->lambda_rpp)
            replace_program_lfun_closure_adjust(r_ob);

#ifdef USE_PYTHON
        if (r_ob->python_rpp)
            python_replace_program_adjust(r_ob);
#endif

        /* If the object modified is a blueprint object, NULL out the pointer
         * in its program, because after the replacement the blueprint nature
         * will be lost.
         */
        if (r_ob->ob->prog->blueprint == r_ob->ob)
        {
            r_ob->ob->prog->blueprint = NULL;
            remove_prog_swap(r_ob->ob->prog, MY_TRUE);
            free_object(r_ob->ob, "replace_programs: blueprint reference");
        }

        /* Replace the old program with the new one */
        old_prog = r_ob->ob->prog;
        r_ob->new_prog->ref++;
        r_ob->ob->prog = r_ob->new_prog;
        r_ob->ob->flags |= O_REPLACED;

        r_next = r_ob->next;  /* remove it from the list */

        /* Handle a possible lambda adjustment */
        if (r_ob->lambda_rpp)
        {
            obj_list_replace = r_next;
            replace_program_lambda_adjust(r_ob);
        }

        /* Remove current shadows */

        if (r_ob->ob->flags & O_SHADOW)
        {
            shadow_t *shadow_sent;

            if ((shadow_sent = O_GET_SHADOW(r_ob->ob))->shadowing)
            {
                /* The master couldn't decide if it's a legal shadowing
                 * before the program was actually replaced. It is possible
                 * that the blueprint to the replacing program is already
                 * destructed, and it's source changed.
                 * On the other hand, if we called the master now, all kind
                 * of volatile data structures could result, even new entries
                 * for obj_list_replace. This would eventually require to
                 * reference it, and all the lrpp's , in check_a_lot_ref_counts()
                 * and garbage_collection() . Being able to use replace_program()
                 * in shadows is hardly worth this effort. Thus, we simply
                 * stop the shadowing.
                 */
                O_GET_SHADOW(shadow_sent->shadowing)->shadowed_by =
                  shadow_sent->shadowed_by;
                if (shadow_sent->shadowed_by)
                {
                    O_GET_SHADOW(shadow_sent->shadowed_by)->shadowing =
                      shadow_sent->shadowing;
                    shadow_sent->shadowed_by = NULL;
                }
                shadow_sent->shadowing = NULL;
            }
        }
        xfree(r_ob);

        /* Free the old program, finally */
        free_prog(old_prog, MY_TRUE);

#ifdef DEBUG
        if (d_flag)
            debug_message("%s program freed.\n", time_stamp());
#endif
    }

    /* Done with the list */
    obj_list_replace = NULL;

#ifdef DEBUG
    if (d_flag)
        debug_message("%s end of replace_programs\n", time_stamp());
#endif

}  /* replace_programs() */

/*-------------------------------------------------------------------------*/
static replace_ob_t *
retrieve_replace_program_entry (void)

/* Auxiliary function to efun replace_program(): test if a program
 * replacement is already scheduled for the current object. If yes,
 * return the pointer to the replace_ob struct, else return NULL.
 */

{
    replace_ob_t *r_ob;

    for (r_ob = obj_list_replace; r_ob; r_ob = r_ob->next)
    {
        if (r_ob->ob == current_object)
            return r_ob;
    }
    return NULL;
} /* retrieve_replace_program_entry() */

/*-------------------------------------------------------------------------*/
static program_t *
search_inherited (string_t *str, program_t *prg, int *offpnt)

/* Auxiliary function to efun replace_program(): check if program <str>
 * is inherited by <prg>. If yes, return the originating program and
 * store the (accumulated) variable and function offsets in offpnt[0]
 * and offpnt[1] resp. (offpnt[2] will indicate, if it's a virtual inherit.)
 *
 * If the program is not found, return NULL.
 *
 * Nested inherits are handled in a depth search, the function recurses
 * for this.
 */

{
    program_t *tmp;
    int i;
#ifdef DEBUG
    char *ts;
#endif

#ifdef DEBUG
    ts = NULL;
    if (d_flag)
    {
        ts = time_stamp();
        debug_message("%s search_inherited started\n", ts);
        debug_message("%s searching for PRG(%s) in PRG(%s)\n"
                     , ts, get_txt(str), get_txt(prg->name));
        debug_message("%s num_inherited=%hu\n", ts, prg->num_inherited);
    }
#endif

    /* Loop through all inherited programs, returning directly when
     * the name program was found.
     */
    for ( i = 0; i < prg->num_inherited; i++)
    {
#ifdef DEBUG
        if (d_flag)
        {
            debug_message("%s index %d:\n", ts, i);
            debug_message("%s checking PRG(%s)\n"
                         , ts, get_txt(prg->inherit[i].prog->name));
        }
#endif
        /* Duplicate virtual inherits don't count */
        if ( prg->inherit[i].inherit_type & INHERIT_TYPE_DUPLICATE )
            continue;

        if (mstreq(str, prg->inherit[i].prog->name ))
        {
#ifdef DEBUG
            if (d_flag)
                debug_message("%s match found\n", ts);
#endif
            if (prg->inherit[i].inherit_type & INHERIT_TYPE_MAPPED)
                i = prg->inherit[i].updated_inherit;

            offpnt[0] = prg->inherit[i].variable_index_offset;
            offpnt[1] = prg->inherit[i].function_index_offset;
            offpnt[2] = prg->inherit[i].inherit_type == INHERIT_TYPE_NORMAL;
            return prg->inherit[i].prog;
        }
    }

    /* Now let's do that again for a deep search.
     */
    for ( i = 0; i < prg->num_inherited; i++)
    {
        /* We don't search in duplicate or obsolete virtual inherits. */
        if ( prg->inherit[i].inherit_type & (INHERIT_TYPE_DUPLICATE|INHERIT_TYPE_MAPPED) )
            continue;

        if ( NULL != (tmp = search_inherited(str, prg->inherit[i].prog,offpnt)) )
        {
            assert(offpnt[2]); /* We should have found virtual inherits as direct matches. */
#ifdef DEBUG
            if (d_flag)
                debug_message("%s deferred match found\n", ts);
#endif
            offpnt[0] += prg->inherit[i].variable_index_offset;
            offpnt[1] += prg->inherit[i].function_index_offset;
            offpnt[2]  = prg->inherit[i].inherit_type == INHERIT_TYPE_NORMAL;
            return tmp;
        }
    }

#ifdef DEBUG
    if (d_flag)
        debug_message("%s search_inherited failed\n", ts);
#endif

    return NULL;
} /* search_inherited() */

/*-------------------------------------------------------------------------*/
void
tell_npc (object_t *ob, string_t *str)

/* Call the lfun 'catch_tell()' in object <ob> with <str> as argument.
 *
 * This function is used to talk to non-interactive commandgivers
 * (aka NPCs).
 */

{
    push_ref_string(inter_sp, str);
    (void)sapply(STR_CATCH_TELL, ob, 1);
}

/*-------------------------------------------------------------------------*/
void
tell_npc_str (object_t *ob, const char *str)

/* Call the lfun 'catch_tell()' in object <ob> with <str> as argument.
 *
 * This function is used to talk to non-interactive commandgivers
 * (aka NPCs).
 */

{
    push_c_string(inter_sp, str);
    (void)sapply(STR_CATCH_TELL, ob, 1);
}

/*-------------------------------------------------------------------------*/
void
tell_object (object_t *ob, string_t *str)

/* Send message <str> to object <ob>. If <ob> is an interactive player,
 * it will go to his screen (unless a shadow catches it - see shadow_catch_
 * message() ). If <ob> is not interactive, the message will go
 * to the lfun 'catch_tell()' via a call to tell_npc().
 */

{
    object_t *save_command_giver;
    interactive_t *ip;

    if (ob->flags & O_DESTRUCTED)
        return;

    if (O_SET_INTERACTIVE(ip, ob))
    {
        save_command_giver = command_giver;
        command_giver = ob;
        add_message_str(str);
        command_giver = save_command_giver;
        return;
    }
    tell_npc(ob, str);
}

/*-------------------------------------------------------------------------*/
Bool
shadow_catch_message (object_t *ob, const char *str)

/* Called by comm:add_message() to handle the case that messages <str> sent
 * to interactive objects <ob> are to be delivered to shadows of the
 * OR to a function in the interactive itself.
 *
 * This function checks all shadows of <ob> if they contain the lfun
 * catch_tell(), and calls the lfun in the first shadow where it exists
 * with message <str> as argument.
 *
 * Result is true if there is such a function, and false if not. In
 * the latter case, the flag ob->ip.catch_tell_activ is cleared to
 * speed up later calls.
 *
 * The function returns immediately with false, if ob->ip.catch_tell_activ
 * is cleared, or if ob is the current_object.
 *
 * Beware that one of the shadows may be the originator of the message,
 * which means that we must not send the message to that shadow, or any
 * shadows in the linked list before that shadow.
 *
 * If the interactive user itself contains the lfun catch_tell(), the
 * messages counts as caught, too.
 */

{
    interactive_t *ip;

    ip = O_GET_INTERACTIVE(ob);

    if (!ip || !ip->catch_tell_activ || ob == current_object)
        return MY_FALSE;

    trace_level |= ip->trace_level;
    push_c_string(inter_sp, str);
    if (sapply(STR_CATCH_TELL, ob, 1))
        return MY_TRUE;

    /* The call failed, thus, current_object wasn't changed
     * (e.g. destructed and set to 0 ) .
     * !current_object is true when a prompt is given.
     */
    if (!current_object
     || !(current_object->flags & O_SHADOW)
     || !O_GET_SHADOW(current_object)->shadowing)
    {
        ip->catch_tell_activ = MY_FALSE;
    }
    return MY_FALSE;
} /* shadow_catch_message() */

/*-------------------------------------------------------------------------*/
static void
clear_program_id (program_t *p)

/* Clear the id_number of program <p> and all inherited programs.
 */

{
    int i;

    if (!p->id_number)
        return;

    p->id_number = 0;
    for (i = 0; i< p->num_inherited; i++)
    {
        clear_program_id(p->inherit[i].prog);
    }
}

/*-------------------------------------------------------------------------*/
static void
renumber_program (program_t *p)

/* Renumber program <p> and all inherited programs.
 * Assumes that all id_numbers have been cleared before.
 */

{
    int i;

    if (p->id_number)
        return;
    p->id_number = ++current_id_number;
    for (i=0; i< p->num_inherited; i++) {
        renumber_program(p->inherit[i].prog);
    }
}

/*-------------------------------------------------------------------------*/
int32
renumber_programs (void)

/* Renumber all programs in the game, recycling number from old
 * objects. Return the first free new id_number and modifies
 * the global current_id_number.
 *
 * The function is called by swap.c and lang.c when current_id_number
 * overflows.
 */

{
    object_t *ob;

    current_id_number = 0;
    for (ob = obj_list; ob; ob = ob->next_all)
    {
#ifdef DEBUG
        if (ob->flags & O_DESTRUCTED) /* TODO: Can't happen */
            continue;
#endif
        if ( !O_PROG_SWAPPED(ob) )
            clear_program_id(ob->prog);
    }

    /* We'll start here at the list's end, because there are
     * the older objects. So we try to preserve the notion
     * that newer programs have greater ids. (Structs use
     * this characteristic in their .prog_id field.)
     */
    for (ob = obj_list_end; ob; ob = ob->prev_all)
    {
#ifdef DEBUG
        if (ob->flags & O_DESTRUCTED)  /* TODO: Can't happen */
            continue;
#endif
        if ( !O_PROG_SWAPPED(ob) )
            renumber_program(ob->prog);
    }
    invalidate_apply_low_cache();
    return ++current_id_number;
}

/*=========================================================================*/
/*                                EFUNS                                    */

/*-------------------------------------------------------------------------*/
svalue_t *
v_function_exists (svalue_t *sp, int num_arg)

/* EXEC function_exists()
 *
 *   mixed function_exists (string str [, int flags])
 *   mixed function_exists (string str , object ob, [, int flags])
 *
 * Look up a function <str> in the current object, respectively
 * in the object <ob>. Depending on the value of <flags>, one
 * of the following informations is returned:
 *
 * <flags> == FEXISTS_PROGNAME (0, default):
 *   Return the name of the program the function is defined in.
 *   This can be either object_name(ob), or the name of an inherited
 *   program. If !compat mode, the returned name always begins
 *   with a '/'.
 *
 * <flags> == FEXISTS_FILENAME (1):
 *   Return the name of the file the function is defined in (this
 *   may be an include file). If !compat mode, the returned name
 *   always begins with a '/'.
 *
 * <flags> == FEXISTS_LINENO (2):
 *   Return the line number within the source file.
 *
 * <flags> == FEXISTS_ALL (3):
 *   Return an array with all the above information, plus information
 *   about the function type/flags/number of arguments.
 *
 *   The returned array contains this information:
 *     string [FEXISTS_PROGNAME]: the program name
 *     string [FEXISTS_FILENAME]: the filename
 *     int    [FEXISTS_LINENO]:   the linenumber
 *     int    [FEXISTS_NUMARG]:   the number of arguments to the function
 *     int    [FEXISTS_TYPE]:     the return type of the function
 *     int    [FEXISTS_FLAGS]:    the function flags
 *
 * The <flags> value can be or-ed to NAME_HIDDEN to return
 * information about static and protected functions in other objects.
 * It is not possible to return information about private functions.
 *
 * If the function cannot be found (because it doesn't exist or
 * it is not visible to the caller), the result is 0.
 */

{
    string_t *str, *prog_name;
    uint32 prog_line = 0;
    p_int flags;
    svalue_t *argp;
    object_t *ob;

    uint32    fun_flags;
    int       fun_num_arg;
    lpctype_t *fun_type;

    /* Evaluate arguments */
    argp = sp - num_arg + 1;

    ob = NULL;
    flags = 0;

    if (num_arg < 2)
    {
        ob = current_object;
        flags = 0;
    }

    if (num_arg >= 2)
    {
        if (argp[1].type == T_NUMBER)
        {
            ob = current_object;
            flags = argp[1].u.number;

            if ((flags & ~NAME_HIDDEN) < 0
             || (flags & ~NAME_HIDDEN) > FEXISTS_ALL
               )
            {
                errorf("Bad argument 2 to function_exists(): value %"PRIdPINT
                       " (%"PRIdPINT" sans NAME_HIDDEN) out of range %d..%d .\n"
                     , flags, (flags & ~NAME_HIDDEN)
                     , FEXISTS_ALL, FEXISTS_LINENO);
                /* NOTREACHED */
                return sp;
            }
        }
        else if (argp[1].type == T_OBJECT)
        {
            ob = argp[1].u.ob;
            flags = 0;
        }
    }

    if (num_arg >= 3)
    {
        /* The last argument must be a number. On the other
         * side, we can't have two numbers at once.
         */
        if (argp[1].type != T_OBJECT)
        {
            errorf("Bad argument 2 to function_exists(): got %s, expected object.\n", typename(argp[1].type));
            /* NOTREACHED */
            return sp;
        }

        flags = argp[2].u.number;

        if (((flags & ~NAME_HIDDEN) < 0)
         || ((flags & ~NAME_HIDDEN) > FEXISTS_ALL)
           )
        {
            errorf("Bad argument 3 to function_exists(): eff. value %"PRIdPINT" (sans NAME_HIDDEN) out of range %d..%d .\n"
                 , (flags & ~NAME_HIDDEN)
                 , FEXISTS_PROGNAME, FEXISTS_ALL);
            /* NOTREACHED */
            return sp;
        }
    }

    if (ob->flags & O_DESTRUCTED)
    {
        errorf("Bad argument to function_exists(): Object is destructed.\n");
        /* NOTREACHED */
        return sp;
    }

    /* Get the information */
    prog_name = NULL;
    str = function_exists(argp->u.str, ob, (flags & NAME_HIDDEN)
                         , &prog_name, &prog_line
                         , &fun_num_arg, &fun_flags, &fun_type);
    sp = pop_n_elems(num_arg, sp);

    if (str)
    {
        switch (flags & ~NAME_HIDDEN)
        {
        case FEXISTS_ALL:
          {
            string_t *res;
            vector_t *vec;

            res = cvt_progname(str);
            if (!res)
            {
                errorf("Out of memory\n");
            }
            vec = allocate_uninit_array(FEXISTS_FLAGS+1);
            put_string(vec->item+FEXISTS_PROGNAME, res);
            if (prog_name)
            {
                res = add_slash(prog_name);
                if (!res)
                {
                    errorf("Out of memory\n");
                }
                put_string(vec->item+FEXISTS_FILENAME, res);
            }
            else
                put_number(vec->item+FEXISTS_FILENAME, 0);
            put_number(vec->item+FEXISTS_LINENO, prog_line);

            put_number(vec->item+FEXISTS_NUMARG, fun_num_arg);
            put_number(vec->item+FEXISTS_TYPE, get_type_compat_int(fun_type));
            put_number(vec->item+FEXISTS_FLAGS, (p_int)fun_flags);

            push_array(sp, vec);
            break;
          }
        case FEXISTS_PROGNAME:
          {
            string_t *res;

            res = cvt_progname(str);
            if (!res)
            {
                errorf("Out of memory\n");
            }
            push_string(sp, res);
            break;
          }

        case FEXISTS_FILENAME:
            if (prog_name)
            {
                string_t *res;

                res = add_slash(prog_name);
                if (!res)
                {
                    errorf("Out of memory\n");
                }
                push_string(sp, res);
            }
            else
                push_number(sp, 0);
            break;

        case FEXISTS_LINENO:
            push_number(sp, prog_line);
            break;

        default:
            fatal("function_exists(): flags value %"PRIdPINT" (from %"PRIdPINT") not implemented.\n"
                 , (flags & ~NAME_HIDDEN), flags);
            /* NOTREACHED */
        }
    }
    else
    {
        push_number(sp, 0);
    }

    /* Clean up */
    if (prog_name)
        free_mstring(prog_name);
    /* str had no ref on its own */

    return sp;
} /* v_function_exists() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_functionlist (svalue_t *sp)

/* EFUN functionlist()
 *
 *   mixed *functionlist (object ob, int flags = RETURN_FUNCTION_NAME)
 *
 * Return an array with information about <ob>s lfunctions. For every
 * function, 1 to 4 values (depending on <flags>) are stored in
 * the result array conveying in this order:
 *   - the name of the function
 *   - the function flags (see below)
 *   - the return type (listed in mudlib/sys/lpctypes.h)
 *   - the number of accepted argumens
 *
 * <ob> may be given as true object or as a filename. In the latter
 * case, the efun does not try to load the object before proceeding.
 *
 * <flags> determines both which information is returned for every
 * function, and which functions should be considered at all.
 * Its value is created by bin-or'ing together following flags from
 * mudlib/sys/functionlist.h:
 *
 *   Control of returned information:
 *     RETURN_FUNCTION_NAME    include the function name
 *     RETURN_FUNCTION_FLAGS   include the function flags
 *     RETURN_FUNCTION_TYPE    include the return type
 *     RETURN_FUNCTION_NUMARG  include the number of arguments.
 *
 *     The name RETURN_FUNCTION_ARGTYPE is defined but not implemented.
 *
 *   Control of listed functions:
 *     NAME_INHERITED      don't list if defined by inheritance
 *     TYPE_MOD_STATIC     don't list if static function
 *     TYPE_MOD_PRIVATE    don't list if private
 *     TYPE_MOD_PROTECTED  don't list if protected
 *     NAME_HIDDEN         don't list if not visible through inheritance
 *
 * The 'flags' information consists of the bin-or of the list control
 * flags given above, plus the following:
 *
 *     TYPE_MOD_VARARGS    function takes varargs
 *     NAME_UNDEFINED      function not defined yet, but referenced.
 *     NAME_CROSS_DEFINED  function is defined to be in a different program
 *     TYPE_MOD_NO_MASK    function is nomask
 *     TYPE_MOD_PUBLIC     function is public
 *
 * All these flags are defined in mudlib/sys/functionlist.h, which
 * should be copied into an accessible place in the mudlib. The
 * return types are defined in mudlib/sys/lpctypes.h which also
 * should be copied into the mudlib.
 *
 * TODO: All these defs are in mudlib/sys/functionlist.h and mudlib/sys/lpctypes.h
 * TODO:: as well as in exec.h and this file. This should be centralized.
 * TODO:: Maybe write the files on mud startup?
 * TODO:: Include mudlib/sys/functionlist.h doesn't help because then
 * TODO:: mkdepend stumbles over the embedded include <sys/lpctypes.h>.
 */

{
    object_t *ob;         /* <ob> argument to list */
    mp_int mode_flags;    /* <flags> argument */
    program_t *prog;      /* <ob>'s program */
    unsigned short num_functions;  /* Number of functions to list */
    char *vis_tags;
      /* Bitflag array describing the visibility of every function in prog
       * in relation to the passed <flags>: */
#define VISTAG_INVIS '\0'  /* Function should not be listed */
#define VISTAG_VIS   '\1'  /* Function matches the <flags> list criterium */
#define VISTAG_ALL   '\2'  /* Function should be listed, no list restrictions */
#define VISTAG_NAMED '\4'  /* Function is neither hidden nor private */

    vector_t *list;       /* Result vector */
    svalue_t *svp;        /* Last element in list which was filled in. */
    uint32 *fun;          /* Current function under examination */
    program_t *defprog;   /* Program which actually defines *fun */
    uint32 flags;
    unsigned short *ixp;
    long i, j;

#define FILTERFLAGS (NAME_HIDDEN|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|NAME_INHERITED)


    inter_sp = sp; /* In case of errors leave a clean stack */

    /* Extract the arguments from the vm stack.
     */
    if (sp[-1].type != T_OBJECT)
    {
        if (!(ob = find_object(sp[-1].u.str)))
            errorf("Object '%s' not found.\n", get_txt(sp[-1].u.str));
    }
    else
        ob = sp[-1].u.ob;

    mode_flags = sp->u.number;

    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0)
        {
            errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));
            /* NOTREACHED */
            return NULL;
        }

    prog = ob->prog;

    /* Initialize the vistag[] flag array.
     */
    num_functions = prog->num_functions;
    vis_tags = alloca(num_functions);
    if (!vis_tags)
    {
        errorf("Stack overflow in functionlist()");
        /* NOTREACHED */
        return NULL;
    }

    /* Preset the visibility. By default, if there is any listing
     * modifier, the functions are not visible. If there is none, the functions
     * are visible.
     */
    memset( vis_tags, (mode_flags & FILTERFLAGS) ? VISTAG_INVIS : VISTAG_ALL
          , num_functions);

    /* Count how many named functions need to be listed in the result.
     * Flag every function to list in vistag[].
     */
    num_functions = 0;

    /* First, check all functions for which we have a name */
    flags = mode_flags & (FILTERFLAGS ^ NAME_HIDDEN);
    fun = prog->functions;
    j = prog->num_function_names;
    for (ixp = prog->function_names + j; --j >= 0; ) {
        i = *--ixp;
        if (!(fun[i] & flags) )
        {
            vis_tags[i] = VISTAG_NAMED|VISTAG_VIS;
            num_functions++;
        }
        else
        {
            vis_tags[i] |= VISTAG_NAMED;
        }
    }

    /* If the user wants to see the hidden or private functions, we loop
     * through the full function table and check all functions not yet
     * touched by the previous 'named' scan.
     * TODO: Due to the dedicated 'find hidden name' loop, this shouldn't
     * TODO:: be necessary, nor the VISTAG_ALL at all.
     */
    if ((mode_flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) == 0)
    {
        fun = prog->functions;
        for (i = prog->num_functions; --i >= 0; )
        {
            if (!(vis_tags[i] & VISTAG_NAMED)
             && !(fun[i] & flags)
               )
            {
                vis_tags[i] = VISTAG_VIS;
                num_functions++;
            }
        }
    }

    /* If <flags> accepts all functions, use the total number of functions
     * instead of the count computed above.
     */
    if ( !(mode_flags & FILTERFLAGS))
    {
        num_functions = prog->num_functions;
    }

    /* Compute the size of the result vector to
     *  2**(number of RETURN_FUNCTION_ bits set)
     */
    for (i = mode_flags & RETURN_FUNCTION_MASK, j = 0; i; i >>= 1) {
        if (i & 1)
            j += num_functions;
    }

    /* Allocate the result vector and set svp to its end
     */
    list = allocate_array(j);
    svp = list->item + j;

    /* Loop backwards through all functions, check their flags if
     * they are to be listed and store the requested data in
     * the result vector.
     */

    for (i = prog->num_functions, fun = prog->functions + i; --i >= 0; )
    {
        bytecode_p funstart; /* Pointer to function in the executable */
        uint32 active_flags;  /* A functions definition status flags */
        function_t *header;

        fun--;

        if ((vis_tags[i] & (VISTAG_ALL|VISTAG_VIS)) == VISTAG_INVIS)
            continue; /* Don't list this one */

        flags = *fun;

        active_flags = (flags & ~INHERIT_MASK);
        if (!(vis_tags[i] & VISTAG_NAMED))
            active_flags |= NAME_HIDDEN;

        defprog = prog;

        /* If its a cross-defined function, get the flags from
         * real definition and let j point to it.
         */
        if ( !~(flags | ~(NAME_INHERITED|NAME_CROSS_DEFINED) ) ) {
            active_flags |= NAME_CROSS_DEFINED;
            j = (long)CROSSDEF_NAME_OFFSET(flags);
            flags = fun[j];
            j += i;
        } else {
            j = i;
        }

        /* If the function is inherited, find the original definition.
         */
        while (flags & NAME_INHERITED) {
            inherit_t *ip = &defprog->inherit[flags & INHERIT_MASK];

            defprog = ip->prog;
            j -= ip->function_index_offset;
            flags = defprog->functions[j];
        }

        /* defprog now points to the program which really defines
         * the function fun.
         */

        funstart = defprog->program + (flags & FUNSTART_MASK);
        header = defprog->function_headers + FUNCTION_HEADER_INDEX(funstart);

        /* Add the data to the result vector as <flags> determines.
         */

        if (mode_flags & RETURN_FUNCTION_NUMARG) {
            svp--;
            svp->u.number = header->num_arg;
        }

        if (mode_flags & RETURN_FUNCTION_TYPE) {
            svp--;
            svp->u.number = get_type_compat_int(header->type);
        }

        if (mode_flags & RETURN_FUNCTION_FLAGS) {

            /* If the function starts with the bytecodes F_UNDEF,
             * it referenced but undefined. But you know that.
             */
            if (is_undef_function(funstart))
            {
                active_flags |= NAME_UNDEFINED;
            }
            svp--;
            svp->u.number = (p_int)active_flags;
        }

        if (mode_flags & RETURN_FUNCTION_NAME) {
            svp--;
            svp->type = T_STRING;
            svp->u.str = header->name;
            (void)ref_mstring(svp->u.str);
        }
    } /* for() */

    /* Cleanup and return */
    free_svalue(sp);
    sp--;
    free_svalue(sp);

    put_array(sp, list);
    return sp;

#undef VISTAG_INVIS
#undef VISTAG_VIS
#undef VISTAG_ALL
#undef VISTAG_NAMED

#undef FILTERFLAGS
} /* f_functionlist() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_variable_exists (svalue_t *sp, int num_arg)

/* EXEC variable_exists()
 *
 *   string variable_exists (string str [, int flags])
 *   string variable_exists (string str , object ob, [, int flags])
 *
 * Look up a variable <str> in the current object, respectively
 * in the object <ob>.
 *
 * The result is the name of the program the variable is defined in. This can
 * be either object_name(ob), or the name of an inherited program. If !compat
 * mode, the returned name always begins with a '/'.
 *
 * If <flags> can be passed as NAME_HIDDEN to return information about static
 * and protected variables in other objects. It is not possible to return
 * information about private variables.
 *
 * If the variable cannot be found (because it doesn't exist or
 * it is not visible to the caller), the result is 0.
 */

{
    string_t *str;
    svalue_t *argp;
    object_t *ob;
    p_int mode_flags;

    /* Evaluate arguments */
    argp = sp - num_arg + 1;

    ob = NULL;
    mode_flags = 0;

    if (num_arg < 2)
    {
        ob = current_object;
        mode_flags = 0;
    }

    if (num_arg >= 2)
    {
        if (argp[1].type == T_NUMBER)
        {
            ob = current_object;
            mode_flags = argp[1].u.number;

            if (mode_flags != 0 && mode_flags != NAME_HIDDEN)
            {
                errorf("Bad argument 2 to variable_exists(): "
                      "value %"PRIdPINT", expected 0 or %d (NAME_HIDDEN).\n"
                     , mode_flags, NAME_HIDDEN
                    );
                /* NOTREACHED */
                return sp;
            }
        }
        else if (argp[1].type == T_OBJECT)
        {
            ob = argp[1].u.ob;
            mode_flags = 0;
        }
    }

    if (num_arg >= 3)
    {
        /* The last argument must be a number. On the other
         * side, we can't have two numbers at once.
         */
        if (argp[1].type != T_OBJECT)
        {
            errorf("Bad argument 2 to variable_exists(): "
                  "got %s, expected object.\n", typename(argp[1].type));
            /* NOTREACHED */
            return sp;
        }

        mode_flags = argp[2].u.number;

        if (mode_flags != 0 && mode_flags != NAME_HIDDEN)
        {
            errorf("Bad argument 3 to variable_exists(): "
                  "value %"PRIdPINT", expected 0 or %d (NAME_HIDDEN).\n"
                 , mode_flags, NAME_HIDDEN
                );
            /* NOTREACHED */
            return sp;
        }
    }


    if (ob->flags & O_DESTRUCTED)
    {
        errorf("Bad argument to variable_exists(): Object is destructed.\n");
        /* NOTREACHED */
        return sp;
    }
 
    /* Make the program resident */
    if (O_PROG_SWAPPED(ob))
    {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
            errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));
    }

    /* Get the information */
    str = NULL;

    do
    {
        string_t *shared_name;
        program_t *progp;
        int ix;
        typeflags_t flags;
        bool virtualvar;

        shared_name = find_tabled(argp->u.str);
        if (!shared_name)
            break;

        progp = ob->prog;

        /* Check if the variable exists at all */
        for (ix = 0; ix < progp->num_variables; ix++)
        {
            if (mstreq(shared_name, progp->variables[ix].name))
                break;
        }

        if (ix >= progp->num_variables)
            break;

        virtualvar = (ix < progp->num_virtual_variables);
        if (!virtualvar)
            ix -= progp->num_virtual_variables;

        /* Is it visible for the caller? */
        flags = progp->variables[ix].type.t_flags;

        if (!(mode_flags & NAME_HIDDEN)
         && (   (flags & TYPE_MOD_PRIVATE)
             || ((flags & TYPE_MOD_PROTECTED) && current_object != ob))
           )
            break;

        /* Resolve inheritance */
        while (flags & NAME_INHERITED)
        {
            int ic;

            for (ic = 0; ic < progp->num_inherited; ic++)
            {
                inherit_t *ip = &progp->inherit[ic];

                if ((ip->inherit_type == INHERIT_TYPE_NORMAL) == virtualvar)
                    continue;

                if (ix >= ip->variable_index_offset + ip->prog->num_variables
                 || ix < ip->variable_index_offset
                   )
                    continue;
                ix -= ip->variable_index_offset;
                progp = ip->prog;
                flags = progp->variables[ix].type.t_flags;
                virtualvar = false;
            }
        }

        /* progp now points to the program which really defines
         * the variable var.
         */

        /* We got it. */
        str = progp->name;
    } while(0);

    /* Put the result onto the stack */
    sp = pop_n_elems(num_arg, sp);

    if (str)
    {
        string_t *res;

        res = cvt_progname(str);
        push_string(sp, res);
    }
    else
    {
        push_number(sp, 0);
    }

    /* str had no ref on its own */

    return sp;
} /* v_variable_exists() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_variable_list (svalue_t *sp)

/* EFUN variable_list()
 *
 *   mixed *variable_list (object ob, int flags = RETURN_FUNCTION_NAME)
 *
 * Return an array with information about <ob>s variables. For every
 * variable, 1 to 4 values (depending on <flags>) are stored in
 * the result array conveying in this order:
 *   - the name of the variable
 *   - the variable flags (see below)
 *   - the return type (listed in mudlib/sys/lpctypes.h)
 *   - the value of the variable
 *
 * <ob> may be given as true object or as a filename. In the latter
 * case, the efun does not try to load the object before proceeding.
 *
 * If <ob> is not the current object and the value of the variable is
 * requested, a privilege_violation ("variable_list", <ob>) occurs.
 *
 * <flags> determines both which information is returned for every
 * variable, and which variables should be considered at all.
 * Its value is created by bin-or'ing together following flags from
 * mudlib/sys/functionlist.h:
 *
 *   Control of returned information:
 *     RETURN_FUNCTION_NAME    include the variable name
 *     RETURN_FUNCTION_FLAGS   include the variable flags
 *     RETURN_FUNCTION_TYPE    include the return type
 *     RETURN_VARIABLE_VALUE   include the variable value
 *
 *   Control of listed variables:
 *     NAME_INHERITED      don't list if defined by inheritance
 *     TYPE_MOD_NOSAVE ==
 *     TYPE_MOD_STATIC     don't list if nosave ('static') variable
 *     TYPE_MOD_PRIVATE    don't list if private
 *     TYPE_MOD_PROTECTED  don't list if protected
 *     NAME_HIDDEN         don't list if not visible through inheritance
 *
 * The 'flags' information consists of the bin-or of the list control
 * flags given above, plus the following:
 *
 *     TYPE_MOD_VIRTUAL    variable is inherited virtually
 *     TYPE_MOD_NO_MASK    variable is nomask
 *     TYPE_MOD_PUBLIC     variable is public
 *
 * All these flags are defined in mudlib/sys/functionlist.h, which
 * should be copied into an accessible place in the mudlib. The
 * return types are defined in mudlib/sys/lpctypes.h which also
 * should be copied into the mudlib.
 *
 * TODO: All these defs are in mudlib/sys/functionlist.h and mudlib/sys/lpctypes.h
 * TODO:: as well as in exec.h and this file. This should be centralized.
 * TODO:: Maybe write the files on mud startup?
 * TODO:: Include mudlib/sys/functionlist.h doesn't help because then
 * TODO:: mkdepend stumbles over the embedded include <sys/lpctypes.h>.
 */

{
    object_t *ob;         /* <ob> argument to list */
    mp_int mode_flags;    /* <flags> argument */
    program_t *prog;      /* <ob>'s program */
    unsigned short num_variables;  /* Number of variables to list */
    char *vis_tags;
      /* Bitflag array describing the visibility of every variable in prog
       * in relation to the passed <flags>: */
#define VISTAG_INVIS '\0'  /* Variable should not be listed */
#define VISTAG_VIS   '\1'  /* Variable matches the <flags> list criterium */
#define VISTAG_ALL   '\2'  /* Variable should be listed, no list restrictions */
#define VISTAG_NAMED '\4'  /* Variable is neither hidden nor private */

#define FILTERFLAGS (NAME_HIDDEN|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|NAME_INHERITED)

    vector_t *list;       /* Result vector */
    svalue_t *svp;        /* Last element in list which was filled in. */
    variable_t *var;      /* Current variable under examination */
    uint32 flags;
    long i, j;

    inter_sp = sp; /* In case of errors leave a clean stack */

    /* Extract the arguments from the vm stack.
     */
    if (sp[-1].type != T_OBJECT)
    {
        if (!(ob = find_object(sp[-1].u.str)))
            errorf("Object '%s' not found.\n", get_txt(sp[-1].u.str));
    }
    else
        ob = sp[-1].u.ob;

    mode_flags = sp->u.number;

    if (ob != current_object && (mode_flags & RETURN_VARIABLE_VALUE))
    {
        assert_master_ob_loaded();
        if (privilege_violation(STR_VARIABLE_LIST, sp-1, sp) <= 0)
        {
            free_svalue(sp);
            sp--;
            free_svalue(sp);
            sp->type = T_NUMBER;
            sp->u.number = 0;
            return sp;
        }
    }

    // We need the program and if the values of variables are requested we additionally
    // need the variables block.
    if (O_PROG_SWAPPED(ob)
     || ((mode_flags & RETURN_VARIABLE_VALUE) && O_VAR_SWAPPED(ob))
       )
    {
        if (load_ob_from_swap(ob) < 0)
        {
            errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));
            /* NOTREACHED */
            return NULL;
        }
    }
    prog = ob->prog;

    /* Initialize the vistag[] flag array.
     */
    num_variables = prog->num_variables;
    vis_tags = alloca(num_variables);
    if (!vis_tags)
    {
        errorf("Stack overflow in variable_list()");
        /* NOTREACHED */
        return NULL;
    }

    /* Preset the visibility. By default, if there is any listing
     * modifier, the variables are not visible. If there is none, the
     * variables are visible.
     */
    memset( vis_tags, (mode_flags & FILTERFLAGS) ? VISTAG_INVIS : VISTAG_ALL
          , num_variables);

    /* Count how many named variables need to be listed in the result.
     * Flag every variable to list in vistag[].
     */
    num_variables = 0;

    /* First, check all variables for which we have a name */
    flags = mode_flags & (FILTERFLAGS ^ NAME_HIDDEN);
    var = prog->variables;
    i = prog->num_variables;
    while ( --i >= 0 ) {
        if (!(var[i].type.t_flags & flags) )
        {
            vis_tags[i] = VISTAG_NAMED|VISTAG_VIS;
            num_variables++;
        }
        else
        {
            vis_tags[i] |= VISTAG_NAMED;
        }
    }

    /* If the user wants to see the hidden or private variables, we loop
     * through the full variable table and check all variables not yet
     * touched by the previous 'named' scan.
     * TODO: Due to the dedicated 'find hidden name' loop, this shouldn't
     * TODO:: be necessary, nor the VISTAG_ALL at all.
     */
    if ((mode_flags & (TYPE_MOD_PRIVATE|NAME_HIDDEN)) == 0)
    {
        var = prog->variables;
        for (i = prog->num_variables; --i >= 0; )
        {
            if (!(vis_tags[i] & VISTAG_NAMED)
             && !(var[i].type.t_flags & flags)
               )
            {
                vis_tags[i] = VISTAG_VIS;
                num_variables++;
            }
        }
    }

    /* If <flags> accepts all variables, use the total number of variables
     * instead of the count computed above.
     */
    if ( !(mode_flags & FILTERFLAGS))
    {
        num_variables = prog->num_variables;
    }

    /* Compute the size of the result vector to
     *  2**(number of RETURN_FUNCTION_ bits set)
     */
    for (i = mode_flags & RETURN_VARIABLE_MASK, j = 0; i; i >>= 1) {
        if (i & 1)
            j += num_variables;
    }

    /* Allocate the result vector and set svp to its end
     */
    list = allocate_array(j);
    svp = list->item + j;

    /* Loop backwards through all variables, check their flags if
     * they are to be listed and store the requested data in
     * the result vector.
     */

    for (i = prog->num_variables, var = prog->variables + i; --i >= 0; )
    {
        uint32 active_flags;  /* A variable's definition status flags */
        var--;

        if ((vis_tags[i] & (VISTAG_ALL|VISTAG_VIS)) == VISTAG_INVIS)
            continue; /* Don't list this one */

        flags = var->type.t_flags;

        active_flags = (flags & ~INHERIT_MASK);
        if (!(vis_tags[i] & VISTAG_NAMED))
            active_flags |= NAME_HIDDEN;

        /* Add the data to the result vector as <flags> determines.
         */

        if (mode_flags & RETURN_VARIABLE_VALUE)
        {
            svp--;
            assign_svalue_no_free(svp, &ob->variables[i]);
        }

        if (mode_flags & RETURN_FUNCTION_TYPE)
        {
            svp--;
            svp->u.number = get_type_compat_int(var->type.t_type);
        }

        if (mode_flags & RETURN_FUNCTION_FLAGS)
        {
            svp--;
            svp->u.number = (p_int)active_flags;
        }

        if (mode_flags & RETURN_FUNCTION_NAME) {
            svp--;
            put_ref_string(svp, var->name);
        }
    } /* for() */

    /* Cleanup and return */
    free_svalue(sp);
    sp--;
    free_svalue(sp);

    put_array(sp, list);
    return sp;

#undef VISTAG_INVIS
#undef VISTAG_VIS
#undef VISTAG_ALL
#undef VISTAG_NAMED

#undef FILTERFLAGS
} /* f_variable_list() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_include_list (svalue_t *sp, int num_arg)

/* EFUN include_list()
 *
 *   string* include_list ()
 *   string* include_list (object ob)
 *   string* include_list (object ob, int flags)
 *
 * Return a list with the names of all files included by the program
 * of object <ob>, including <ob>'s program file itself.
 */

{
    object_t  *ob;          /* Analyzed object */
    vector_t  *vec;         /* Result vector */
    int        count;       /* Total number of includes */
    svalue_t  *argp;        /* Arguments */
    include_t *includes;    /* Pointer to the include information */
    p_int     flags;

    /* Get the arguments */
    argp = sp - num_arg + 1;

    if (num_arg >= 1)
        ob = argp[0].u.ob;
    else
        ob = current_object;

    if (num_arg >= 2)
        flags = argp[1].u.number;
    else
        flags = 0;

    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0)
        {
            errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));
            /* NOTREACHED */
            return NULL;
        }

    /* Create the result.
     * Depending on the flags value, this can be a flat list or a tree.
     */

    if (!(flags & INCLIST_TREE))
    {
        svalue_t *svp;

        /* Get the result array */
        vec = allocate_array((ob->prog->num_includes+1) * 3);
        svp = vec->item;

        /* Walk the includes information and copy it into the result vector
         */
        for (  svp = vec->item+3
             , count = ob->prog->num_includes
             , includes = ob->prog->includes
            ; count > 0
            ; count--, includes++, svp += 3
            )
        {
            int depth;

            put_ref_string(svp, includes->name);
            put_ref_string(svp+1, includes->filename);
            depth = includes->depth;
            if (depth > 0)
                put_number(svp+2, depth);
            else
                put_number(svp+2, -depth);
        }
    }
    else  /* Tree-type result */
    {
        /* Local structure to hold the found programs */
        struct iinfo {
            struct iinfo * next;     /* Next structure in flat list */
            int            depth;    /* Include depth */
            include_t    * inc;      /* The include information */
              /* The following members are used to recreate the inherit tree */
            int            count;    /* Number of direct includes */
            struct iinfo * parent;   /* Parent include, or NULL */
            struct iinfo * child;    /* First child include */
            struct iinfo * sibling;  /* Next include on same level */
              /* These members are used to create the result tree */
            size_t         index;    /* # of this include file in the parent
                                      * vector */
            vector_t     * vec;      /* Result vector for this include */
        } *begin, *end;         /* Flat list of all found includes */

        struct iinfo * last;    /* Last include found on this depth */
        struct iinfo * next;    /* Next include to work */
        Mempool   pool;         /* The memory pool to allocate from */

        /* Get the memory pool */
        memsafe(pool = new_mempool(size_mempool(sizeof(*begin)))
               , size_mempool(sizeof(*begin)), "memory pool");

        /* Walk the list of included files and build the tree from it.
         */

        begin = mempool_alloc(pool, sizeof(*begin));
        if (NULL == begin)
        {
            mempool_delete(pool);
            outofmem(sizeof(*begin), "allocation from mempool");
        }

        /* Root node for the object's program itself */
        begin->next = NULL;
        begin->child = NULL;
        begin->sibling = NULL;
        begin->inc = NULL;
        begin->depth = 0;
        begin->count = 0;
        begin->parent = NULL;
        begin->vec = NULL;
        begin->index = 0;

        end = begin;
        last = begin;

        includes = ob->prog->includes;
        count = ob->prog->num_includes;

        for ( ; count > 0; count--, includes++)
        {
            /* Get new node and put it into the flat list */
            end->next = mempool_alloc(pool, sizeof(*end));
            if (NULL == end->next)
            {
                mempool_delete(pool);
                outofmem(sizeof(*end), "allocation from mempool");
            }
            end = end->next;
            end->next = NULL;
            end->inc = includes;
            end->depth = includes->depth > 0 ? includes->depth : - includes->depth;

            /* Handle the tree-based information */
            end->child = NULL;
            end->sibling = NULL;

            if (last->depth > end->depth)
            {
                /* We reached a leaf with <last> - this new was included from
                 * some parent above.
                 */
                while (last->depth > end->depth)
                    last = last->parent;

                /* Got back up to the right sibling level, no go to the end
                 * of the sibling list (just in case - we should already
                 * be there).
                 */
                while (last->sibling)
                    last = last->sibling;
            }
            /* Now the new file is either a sibling or a child of <last> */

            if (last->depth == end->depth)
            {
                /* Sibling to <last> */
                last->sibling = end;
                end->parent = last->parent;
                last = end;
                end->parent->count++;
            }
            else /* last->depth < end->depth */
            {
                /* Included from <last> */
                last->child = end;
                last->count++;
                end->parent = last;
                last = end;
            }

            /* Init the rest */
            end->count = 0;
            end->index = end->parent->count;
            end->vec = NULL;
        }

        /* Get the top result array and keep a reference to it on the
         * stack so that it will be deallocated on an error.
         */
        vec = allocate_array((begin->count+1) * 3);
        begin->vec = vec;
        push_array(sp, vec); inter_sp = sp;

        /* Loop through all the include infos and copy them into
         * their result vector. We create the subvectors when
         * we encounter them.
         * Invariant: <next> points to the next iinfo to work.
         */
        for (next = begin->child; next != NULL; )
        {
            /* If this child has no includes, we just copy the
             * name into its proper place in the parent vector.
             *
             * Otherwise we create a vector for this include
             * and store the names in there.
             */
            if (next->child == NULL)
            {
                svalue_t *svp;

                svp = &next->parent->vec->item[next->index*3];
                put_ref_string(svp, next->inc->name);
                put_ref_string(svp+1, next->inc->filename);
                put_number(svp+2, next->depth);

                /* If we are in the last sibling, roll back up to
                 * the parents.
                 */
                while (next->sibling == NULL && next->parent != NULL)
                    next = next->parent;

                /* Advance to the next sibling. If by  */
                next = next->sibling;
            }
            else
            {
                svalue_t *svp;

                next->vec = allocate_array((next->count+1)*3);

                svp = &next->parent->vec->item[next->index*3];
                put_array(svp, next->vec);
                  /* svp[1] and svp[2] are already 0 */

                svp = next->vec->item;
                put_ref_string(svp, next->inc->name);
                put_ref_string(svp+1, next->inc->filename);
                put_number(svp+2, next->depth);

                /* Descend into the first child */
                next = next->child;
            }
        }

        mempool_delete(pool);
        sp--; /* Remove the temporary storage of vec on the stack */
    }

    /* Copy the information about the program file itself. */

    {
        string_t *str;
        size_t slen;  /* Also used for error reporting */

        slen = mstrsize(ob->prog->name);

        if (compat_mode)
            str = ref_mstring(ob->prog->name);
        else
            str = add_slash(ob->prog->name);

        if (!str)
        {
            free_array(vec);
            errorf("(include_list) Out of memory: (%zu bytes) for filename\n"
                 , slen);
        }

        put_string(vec->item, str);
        /* vec->item[1] and vec->item[2] are already 0 */
    }

    /* Done */

    sp = pop_n_elems(num_arg, sp);

    sp++;
    put_array(sp, vec);
    return sp;
} /* v_include_list() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_inherit_list (svalue_t *sp, int num_arg)

/* EFUN inherit_list()
 *
 *   string* inherit_list ()
 *   string* inherit_list (object ob)
 *   string* inherit_list (object ob, int flags)
 *
 * Return a list with the filenames of all programs inherited by <ob>, include
 * <ob>'s program itself.
 */

{
    /* Local structure to hold the found programs */
    struct iinfo {
        struct iinfo * next;     /* Next structure in flat list */
        SBool          virtual;  /* TRUE: Virtual inherit */
        program_t    * prog;     /* Program found */
          /* The following members are used to recreate the inherit tree */
        int            count;    /* Number of direct inherits */
        struct iinfo * parent;   /* Parent program, or NULL */
          /* These members are used to create the result tree */
        size_t         index;    /* # of this inherited program */
        vector_t     * vec;      /* Result vector for this program */
    } *begin, *end;         /* Flat list of all found inherits */
    struct iinfo * next;    /* Next program to analyze */

    Mempool   pool;         /* The memory pool to allocate from */
    object_t *ob;           /* Analyzed object */
    vector_t *vec;          /* Result vector */
    svalue_t *svp;          /* Pointer to next vec entry to fill in */
    int       count;        /* Total number of inherits found */
    svalue_t *argp;         /* Arguments */
    p_int     flags;

    /* Get the arguments */
    argp = sp - num_arg + 1;

    if (num_arg >= 1)
        ob = argp[0].u.ob;
    else
        ob = current_object;

    if (num_arg >= 2)
        flags = argp[1].u.number;
    else
        flags = 0;

    if (O_PROG_SWAPPED(ob))
        if (load_ob_from_swap(ob) < 0) {
            errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));
            /* NOTREACHED */
            return NULL;
        }

    /* Get the memory pool */
    memsafe(pool = new_mempool(size_mempool(sizeof(*begin)))
           , size_mempool(sizeof(*begin)), "memory pool");

    /* Perform a breadth search on ob's inherit tree and append the
     * found programs to the iinfo list while counting them.
     */

    begin = mempool_alloc(pool, sizeof(*begin));
    if (NULL == begin)
    {
        mempool_delete(pool);
        outofmem(sizeof(*begin), "allocation from mempool");
    }

    /* Root node for the object's program itself */
    begin->next = NULL;
    begin->prog = ob->prog;
    begin->virtual = MY_FALSE;
    begin->count = 0;
    begin->parent = NULL;
    begin->vec = NULL;
    begin->index = 0;

    end = begin;

    count = 1;

    for (next = begin; next != NULL; next = next->next)
    {
        int cnt;
        inherit_t *inheritp;

        cnt = next->prog->num_inherited;

        /* Store the inherited programs in the list.
         */
        for (inheritp = &next->prog->inherit[0]; cnt--; inheritp++)
        {
            if (inheritp->inherit_type == INHERIT_TYPE_NORMAL
             || inheritp->inherit_type == INHERIT_TYPE_VIRTUAL
               )
            {
                count++;
                next->count++;

                end->next = mempool_alloc(pool, sizeof(*end));
                if (NULL == end->next)
                {
                    mempool_delete(pool);
                    outofmem(sizeof(*end), "allocation from mempool");
                }
                end = end->next;
                end->next = NULL;
                end->prog = inheritp->prog;
                end->virtual = (inheritp->inherit_type == INHERIT_TYPE_VIRTUAL);

                /* Handle the tree-based information */
                end->parent = next;
                end->count = 0;
                end->index = next->count;
                end->vec = NULL;
            }
        }
    }

    /* Create the result.
     * Depending on the flags value, this can be a flat list or a tree.
     */

    if (!(flags & INHLIST_TREE))
    {
        /* Get the result array */
        vec = allocate_array(count);

        /* Take the filenames of the programs and copy them into
         * the result vector.
         */
        for (svp = vec->item, next = begin; next != NULL; svp++, next = next->next)
        {
            string_t *str;
            size_t slen;  /* Also used for error reporting */

            slen = mstrsize(next->prog->name);

            if (compat_mode)
                str = ref_mstring(next->prog->name);
            else
                str = add_slash(next->prog->name);

            if (str && (flags & INHLIST_TAG_VIRTUAL))
            {
                string_t * str2;

                slen = mstrsize(str) + 2;

                if (next->virtual)
                    str2 = mstr_add_to_txt("v ", 2, str);
                else
                    str2 = mstr_add_to_txt("  ", 2, str);

                free_mstring(str);
                str = str2;
            }

            if (!str)
            {
                free_array(vec);
                mempool_delete(pool);
                errorf("(inherit_list) Out of memory: (%zu bytes) for filename\n"
                     , slen);
            }
            put_string(svp, str);
        }
    }
    else
    {
        /* Get the top result array and keep a reference to it on the
         * stack so that it will be deallocated on an error.
         */
        vec = allocate_array(begin->count+1);
        begin->vec = vec;
        push_array(sp, vec); inter_sp = sp;

        /* Loop through all filenames and copy them into their result
         * vector. Since the list in breadth-order, we can create the
         * sub-vectors when we encounter them.
         */
        for (next = begin; next != NULL; next = next->next)
        {
            string_t *str;
            size_t slen;  /* Also used for error reporting */

            slen = mstrsize(next->prog->name);

            if (compat_mode)
                str = ref_mstring(next->prog->name);
            else
                str = add_slash(next->prog->name);

            if (str && (flags & INHLIST_TAG_VIRTUAL))
            {
                string_t * str2;

                slen = mstrsize(str) + 2;

                if (next->virtual)
                    str2 = mstr_add_to_txt("v ", 2, str);
                else
                    str2 = mstr_add_to_txt("  ", 2, str);

                free_mstring(str);
                str = str2;
            }

            if (!str)
            {
                mempool_delete(pool);
                errorf("(inherit_list) Out of memory: (%zu bytes) for filename\n"
                     , slen);
            }

            /* If this child has no inherits, we just copy the
             * name into its proper place in the parent vector.
             * Same for the name of the top program.
             *
             * Otherwise we create a vector for this program
             * and store the name in there.
             */
            if (begin == next)
            {
                put_string(next->vec->item, str);
            }
            else if (next->count == 0)
            {
                put_string(&next->parent->vec->item[next->index], str);
            }
            else
            {
                next->vec = allocate_array(next->count+1);
                put_array(&next->parent->vec->item[next->index], next->vec);
                put_string(next->vec->item, str);
            }
        }

        sp--; /* Remove the temporary storage of vec on the stack */
    }

    mempool_delete(pool);

    sp = pop_n_elems(num_arg, sp);

    push_array(sp, vec);
    return sp;
} /* v_inherit_list() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_load_name (svalue_t *sp)

/* EFUN load_name()
 *
 *   string load_name()
 *   string load_name(object obj)
 *   string load_name(string obj)
 *
 * Return the load name for the object <obj> which may be given
 * directly or by its name.
 *
 * If <obj> is a clone, return the load_name() of <obj>'s blueprint.
 * If <obj> is a blueprint, return the filename from which the
 * blueprint was compiled.
 *
 * If <obj> is given by name but not/no longer existing, the
 * function synthesizes the load name as it should be and returns
 * that. If the given name is illegal, the function returns 0.
 *
 * For virtual objects this efun of course returns the virtual
 * filename.  If <obj> is omitted, the name for the current object is
 * returned.
 *
 * As a special case, if <obj> is 0, the function returns 0.
 *
 * In contrast to the object_name(), the load name can not be changed
 * by with rename_object(). However, if an object uses
 * replace_program() the load name no longer reflects the actual
 * behaviour of an object.
 *
 * The returned name starts with a '/', unless the driver is running
 * in COMPAT mode.
 */

{
    string_t *s;   /* String argument */
    char *name;    /* Result string, maybe 's' itself */
    char *hash;    /* Position of the hash in the name */
    char *mem;     /* Allocated memory blocks */
    object_t *ob;

    /* If the argument is 0, just return 0. */
    if (sp->type == T_NUMBER)
    {
        return sp;
    }

    /* If the argument is an object, we just need to read the name */
    if (sp->type == T_OBJECT)
    {
        s = sp->u.ob->load_name;
        free_object_svalue(sp);
        put_ref_string(sp, s);
        return sp;
    }

    /* Argument is a string: try to find the object for it */
    s = sp->u.str;
    ob = find_object(s);
    if (ob)
    {
        /* Got it */
        s = ob->load_name;
        free_string_svalue(sp);
        put_ref_string(sp, s);
        return sp;
    }

    /* There is no object for the string argument: just normalize
     * the string. First check if it ends in #<number>.
     */
    mem = NULL;
    hash = strchr(get_txt(s), '#');
    if (!hash)
    {
        /* No '#' at all: make the name sane directly */
        name = (char *)make_name_sane(get_txt(s), !compat_mode);
        if (!name)
            name = get_txt(s);
    }
    else
    {
        char *p;
        size_t len;

        /* All characters after the '#' must be digits */
        for (p = hash+1; '\0' != *p; p++)
            if (*p < '0' || *p > '9')
                /* Illegal name: break to return svalue 0 */
                break;

        if ('\0' != *p)
        {
            /* Illegal name: break to return svalue 0 */
            free_string_svalue(sp);
            put_number(sp, 0);
            return sp;
        }

        /* Good, we can slash off the '#<number>' */
        len = (size_t)(hash - get_txt(s));
        p = mem = xalloc(len+1);
        if (!p)
            errorf("(load_name) Out of memory (%zu bytes) for filename."
                 , len+1);
        strncpy(p, get_txt(s), len);
        p[len] = '\0';

        /* Now make the name sane */
        name = (char *)make_name_sane(p, !compat_mode);
        if (!name)
            name = p;
    }

    /* name now points to the synthesized load_name and
     * may be the argument (== s), allocated (== mem), or
     * points to a static buffer otherwise.
     */

    /* '/.c' is a legal object name, so make sure that
     * the result will be '/' (in compat mode).
     */
    if (compat_mode && '\0' == *name)
        name = "/";

    /* Now return the result */
    if (get_txt(s) != name)
    {
        free_string_svalue(sp);
        put_c_string(sp, name);
    }

    if (mem)
        xfree(mem);

    return sp;
} /* f_load_name() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_object_name (svalue_t *sp)

/* EFUN object_name()
 *
 *   string object_name()
 *   string object_name(object ob)
 *
 * Get the name of an object <ob> or, if no argument is given, of
 * the current object.
 *
 * As a special case, if <obj> is 0, the function returns 0.
 *
 * This name is the name under which the object is stored in the
 * muds object table. It is initialised at the creation of the
 * object such that blueprints are named after the file they are
 * compiled from (without the trailing '.c'), and clones receive
 * the name of their blueprint, extended by '#' followed by
 * a unique non-negative number. These rules also apply to
 * virtual objects - the real name/type of virtual objects
 * is ignored.
 *
 * The name of an object can be changed with rename_object(), and
 * object_name() will reflect any of these changes.
 *
 * The returned name always begins with '/' (absolute path),
 * except when the parser runs in COMPAT mode.
 */

{
    string_t *name, *res;

    /* If the argument is 0, just return 0. */
    if (sp->type == T_NUMBER)
    {
        return sp;
    }

    name = sp->u.ob->name;
    if (compat_mode)
        res = ref_mstring(name);
    else
        res = add_slash(name);
    if (!res)
        errorf("Out of memory\n");
    free_object_svalue(sp);
    put_string(sp, res);

    return sp;
} /* f_object_name() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_object_time (svalue_t *sp)

/* EFUN object_time()
 *
 *   int object_time()
 *   int object_time(object ob)
 *
 * Returns the creation time of the object.
 * Default is this_object(), if no arg is given.
 */

{
    mp_int load_time;

    load_time = sp->u.ob->load_time;

    free_object_svalue(sp);
    put_number(sp, load_time);

    return sp;
} /* f_object_time() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_program_name (svalue_t *sp)

/* EFUN program_name()
 *
 *   string program_name()
 *   string program_name(object obj)
 *
 * Returns the name of the program of <obj>, resp. the name of the
 * program of the current object if <obj> is omitted.
 *
 * The returned name is usually the name from which the blueprint
 * of <obj> was compiled (the 'load name'), but changes if an
 * object replaces its programs with the efun replace_program().
 *
 * As a special case, if <ob> is 0, the function returns 0.
 *
 * The name always ends in '.c'. It starts with a '/' unless the
 * driver is running in COMPAT mode.
 */

{
    string_t *name, *res;
    object_t *ob;

    /* If the argument is 0, just return 0. */
    if (sp->type == T_NUMBER)
    {
        return sp;
    }

    ob = sp->u.ob;
    if (O_PROG_SWAPPED(ob))
    {
        ob->time_of_ref = current_time;
        if (load_ob_from_swap(ob) < 0)
        {
            errorf("Out of memory: unswap object '%s'\n", get_txt(ob->name));
        }
    }
    name = ob->prog->name;
    if (compat_mode)
        res = ref_mstring(name);
    else
        res = add_slash(name);
    if (!res)
        errorf("Out of memory\n");
    free_object_svalue(sp);
    put_string(sp, res);

    return sp;
} /* f_program_name() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_program_time (svalue_t *sp)

/* EFUN program_time()
 *
 *   int program_time()
 *   int program_time(object ob)
 *
 * Returns the creation (compilation) time of the object's
 * program. Default is this_object(), if no arg is given.
 */

{
    mp_int load_time;

    if (O_PROG_SWAPPED(sp->u.ob))
    {
        sp->u.ob->time_of_ref = current_time;
        if (load_ob_from_swap(sp->u.ob) < 0)
        {
            sp--;
            errorf("Out of memory: unswap object '%s'\n", get_txt(sp->u.ob->name));
        }
    }
    load_time = sp->u.ob->prog->load_time;

    free_object_svalue(sp);
    put_number(sp, load_time);

    return sp;
} /* f_program_time() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_rename_object (svalue_t *sp)

/* EFUN rename_object()
 *
 *   void rename_object (object ob, string new_name);
 *
 * Give the object <ob> a new object name <new_name>. Causes a privilege
 * violation. The new name must not contain a # character, except
 * at the end, to avoid confusion with clone numbers.
 *
 * Raises a privilege violation ("rename_object", this_object(), ob, name).
 */

{
    object_t *ob;      /* object to be renamed */
    char     *name;    /* new name as c-string */
    string_t *m_name;  /* new name */
    size_t   length;   /* length of new name */
    Bool     freenamebuffer = MY_FALSE;  /* free name when not needed */

    inter_sp = sp; /* this is needed for assert_master_ob_loaded(), and for
                    * the possible errors before.
                    */
    ob = sp[-1].u.ob;
    name = get_txt(sp[0].u.str);

    /* Remove leading '/' if any. */
    while(name[0] == '/')
        name++;

    /* Truncate possible .c in the object name. */
    length = strlen(name);
    if (name[length-2] == '.' && name[length-1] == 'c') {
        /* A new writeable copy of the name is needed. */
        char *p;
        p = xalloc(length+1);
        if (!p)
            errorf("Out of memory for %zu bytes in rename_object().\n",
                length+1);

        strcpy(p, name);
        name = p;
        name[length -= 2] = '\0';
        freenamebuffer = MY_TRUE;
    }

    /* check for any #xxx at the end. */
    {
        char c;
        char *p;
        mp_int i;

        i = length;
        p = name + length;
        while (--i > 0)
        {
            /* isdigit would need to check isascii first... */
            if ( (c = *--p) < '0' || c > '9' )
            {
                if (c == '#' && length - i > 1) {
                    if (freenamebuffer)
                        xfree(name);
                    errorf("Illegal name to rename_object: '%s'.\n", name);
                }
                break;
            }
        }
    }

    m_name = new_mstring(name, sp[0].u.str->info.unicode);
    if (!m_name)
    {
        if (freenamebuffer)
            xfree(name);
        errorf("Out of memory for object name (%zu bytes)\n", strlen(name));
    }
    /* in case of errors (e.g. in privilege_violation()), push string on the
     * stack. */
    push_string(sp, m_name);
    inter_sp = sp;

    /* name is not needed anymore. Free it, if it was allocated here. */
    if (freenamebuffer)
    {
        xfree(name);
        /* just to be sure it crashes if somebody uses the pointer by
         * accident. */
        name = NULL;
    }
    
    assert_master_ob_loaded();
    if (master_ob == ob)
    {
        errorf("Attempt to rename the master object\n");
    }

    
    if (lookup_object_hash(m_name))
    {
        errorf("Attempt to rename to existing object '%s'\n", 
               get_txt(m_name));
    }

    if (privilege_violation4(STR_RENAME_OBJECT, ob, m_name, 0, sp)
     && check_object(ob)
       )
    {
        remove_object_hash(ob);
        free_mstring(ob->name);
        ob->name = m_name;
        // m_name needs another reference (one from the stack, one from
        // object->name).
        ref_mstring(m_name);
        enter_object_hash(ob);
    }

    /* free the string m_name (on the top of the stack) and the 2 arguments */
    sp = pop_n_elems(3, sp);

    return sp;
} /* f_rename_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_replace_program (svalue_t *sp, int num_arg)

/* EFUN replace_program()
 *
 *   void replace_program()
 *   void replace_program(string program)
 *
 * Substitutes a program with the inherited program <program>. If the object
 * inherits only one program, the argument may be omitted and the efun will
 * automatically select the one inherited program.
 *
 * This efun is useful if you consider the performance and memory consumption
 * of the driver. A program which doesn't need any additional variables and
 * functions (except during creation) can call replace_program() to increase
 * the function-cache hit-rate of the driver which decreases with the number
 * of programs in the system. Any object can call replace_program() but looses
 * all extra variables and functions which are not defined by the inherited
 * program.
 *
 * When replace_program() takes effect, shadowing is stopped on the object
 * since 3.2@166.
 *
 * It is not possible to replace the program of an object after (lambda)
 * closures have been bound to it. It is of course possible to first replace
 * the program and then bind lambda closures to it.
 *
 * The program replacement does not take place with the call to the efun, but
 * is merely scheduled to be carried out at the end of the backend cycle. This
 * may cause closures to have references to then vanished lfuns of the object.
 * This poses no problem as long as these references are never executed after
 * they became invalid.
 */

{
    replace_ob_t *tmp;
    program_t *new_prog;  /* the replacing program */
    program_t *curprog;   /* the current program */
    int offsets[3];       /* the offsets of the replacing prog */

    if (!current_object)
        errorf("replace_program called with no current object\n");
    if (current_object == simul_efun_object)
        errorf("replace_program on simul_efun object\n");
    
    if (current_object->flags & O_LAMBDA_REFERENCED)
    {
        inter_sp = sp;
        warnf("Object '%s', program '%s': Cannot schedule "
              "replace_program() after binding lambda closures.\n"
             , get_txt(current_object->name)
             , get_txt(current_prog->name)
             );
        sp = pop_n_elems(num_arg, sp);
        return sp;
    }

    curprog = current_object->prog;

    if (num_arg < 1)
    {
        /* Just take the first inherited program */
        size_t replace_index;  /* Inherit index of the replacing program */

        /* Just take the first normal inherited program */
        if (curprog->num_inherited < 1)
            errorf("replace_program called with no inherited program\n");
        replace_index = 0;
        if (curprog->num_inherited > 1)
        {
            /* The object might have extra inherits caused by virtual
             * variables. Since they preceed the associated 'real'
             * inherit, search forward in the inherit list for the real
             * one.
             */
            for ( ; replace_index < curprog->num_inherited
                  ; replace_index++)
            {
                if (!(curprog->inherit[replace_index].inherit_type
                      & INHERIT_TYPE_EXTRA))
                    break;
            }
            /* replace_index must now be the last inherit for the
             * auto-replace_program to work.
             */
            if (replace_index + 1 != curprog->num_inherited)
            {
                errorf("replace_program() requires argument for object "
                      "with more than one inherit\n");
                /* NOTREACHED */
            }
        }

        new_prog = curprog->inherit[replace_index].prog;
        offsets[0] = curprog->inherit[replace_index].variable_index_offset;
        offsets[1] = curprog->inherit[replace_index].function_index_offset;
        offsets[2] = curprog->inherit[replace_index].inherit_type == INHERIT_TYPE_NORMAL;
    }
    else
    {
        string_t *sname;
        { /* block for limiting the scope of name until the xfree. */
            size_t name_len;
            char *name;

            /* Create the full program name with a trailing '.c' and without
             * a leading '/' to match the internal name representation.
             */
            name_len = mstrsize(sp->u.str);
            name = xalloc(name_len+3);
            if (!name)
            {
                errorf("Out of memory (%zu bytes) for temporary name buffer in "
                       "replace_program.\n", name_len);
            }
            strcpy(name, get_txt(sp->u.str));
            if (name_len < 2 || name[name_len-2] != '.' || name[name_len-1] != 'c')
                strcat(name,".c");
            if (*name == '/')
                sname = new_mstring(name+1, sp->u.str->info.unicode);
            else
                sname = new_mstring(name, sp->u.str->info.unicode);
            /* name is not needed anymore, free it first, before throwing any
             * runtime errors. */
            xfree(name);
    
            /* now check if we got a string from new_mstring(). */
            if (!sname)
            {
                errorf("Out of memory (%zu bytes) for temporary name in "
                       "replace_program().\n", name_len+3);
            }
        } /* scope of name ends here */
        
        new_prog = search_inherited(sname, current_object->prog, offsets);
        if (!new_prog)
        {
            /* Given program not inherited, maybe it's the current already.
             */
            if (mstreq(sname, curprog->name ))
            {
                new_prog = curprog;
                offsets[0] = offsets[1] = 0;
                offsets[2] = 1;
            }
            else
            {
                free_mstring(sname);
                errorf("replacement program '%s' needs to be inherited\n"
                     , get_txt(sp->u.str));
            }
        }

        free_mstring(sname);

        free_svalue(sp);
        sp--;

    } /* if (num_arg) */

    /* The offsets of non-virtual inherits are relative to
     * the non-virtual variables auf the inheriting program.
     */
    if (offsets[2])
        offsets[0] += curprog->num_virtual_variables;

    /* Program ok, now create a new replace program entry, or
     * change an existing one.
     */
    if (!(curprog->flags & P_REPLACE_ACTIVE)
     || !(tmp = retrieve_replace_program_entry()) )
    {
        tmp = xalloc(sizeof *tmp);
        tmp->lambda_rpp = NULL;
#ifdef USE_PYTHON
        tmp->python_rpp = NULL;
#endif
        tmp->ob = current_object;
        tmp->next = obj_list_replace;
        obj_list_replace = tmp;
        curprog->flags |= P_REPLACE_ACTIVE;
    }

    tmp->new_prog = new_prog;
    tmp->var_offset = offsets[0];
    tmp->fun_offset = offsets[1];

    return sp;
} /* v_replace_program() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_next_reset (svalue_t *sp)

/* EFUN set_next_reset()
 *
 *   int set_next_reset (int delay)
 *
 * Instruct the gamedriver to reset this object not earlier than in
 * <delay> seconds. If a negative value is given as delay, the object
 * will never reset (useful for blueprints). If 0 is given, the
 * object's reset time is not changed.
 *
 * Result is the former delay to the objects next reset (which can be
 * negative if the reset was overdue).
 */

{
    int new_time;

    new_time = sp->u.number;
    if (current_object->flags & O_DESTRUCTED)
            sp->u.number = 0;
    else
    {
        sp->u.number = current_object->time_reset - current_time;
        if (new_time < 0)
            current_object->time_reset = 0;
        else if (new_time > 0)
            current_object->time_reset = new_time + current_time;
    }
    return sp;
} /* f_set_next_reset() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tell_object (svalue_t *sp)

/* EFUN tell_object()
 *
 *   void tell_object(object|string ob, string str)
 *   void tell_object(object|string ob, mixed msg)
 *
 * Send a message str to object ob. If it is an interactive
 * object (a user), then the message will go to him (her?),
 * otherwise the lfun catch_tell() of the living will be called
 * with the message as argument.
 * If the object is given as its filename, the driver
 * looks up the object under that name, loading it if necessary.
 *
 * If the second arg is an array, catch_msg() will be called in
 * the receiving living.
 */

{
    object_t * ob = NULL;
    svalue_t *arg = sp - 1;

    /* Get the arguments */
    if (arg[0].type == T_OBJECT)
        ob = arg[0].u.ob;
    else if (arg[0].type == T_STRING)
    {
        ob = get_object(arg[0].u.str);
        if (!ob)
            errorf("Object not found: %s.\n", get_txt(arg[0].u.str));
    }

    if (arg[1].type == T_STRING)
    {
        tell_object(ob, sp->u.str);
        free_svalue(sp);
    }
    else
    {
        apply(STR_CATCH_MSG, ob, 1);
          /* Will pop the <msg> at sp from the stack. */
    }

    sp--;
    free_svalue(sp);
    sp--;

    return sp;
} /* f_tell_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_geteuid (svalue_t *sp)

/* EFUN geteuid()
 *
 *   string geteuid(object ob)
 *
 * Get the effective user-id of the object (mostly a wizard or
 * domain name). Standard objects cloned by this object will get
 * that userid. The effective userid is also used for checking access
 * permissions. If ob is omitted, is this_object() as default.
 */

{
    object_t *ob;

     ob = sp->u.ob;

    if (ob->eff_user && ob->eff_user->name)
    {
        string_t *tmp;
        tmp = ref_mstring(ob->eff_user->name);
        free_svalue(sp);
        put_string(sp, tmp);
    }
    else
    {
        free_svalue(sp);
        put_number(sp, 0);
    }

    return sp;
} /* f_geteuid() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_getuid (svalue_t *sp)

/* EFUN getuid()
 *
 *   string getuid(object ob)
 *   string creator(object ob)
 *
 * User-ids are not used in compat mode, instead the uid is
 * then called 'creator'.
 * Get user-id of the object, i.e. the name of the wizard or
 * domain that is responsible for the object. This name is also
 * the name used in the wizlist. If no arg is given, use
 * this_object() as default.
 */

{
    object_t *ob;
    string_t *name;

    ob = sp->u.ob;
    deref_object(ob, "getuid");
    if ( NULL != (name = ob->user->name) )
        put_ref_string(sp, name);
    else
        put_number(sp, 0);

    return sp;
} /* f_getuid() == f_creator() */


/*=========================================================================*/
/*                             INVENTORIES                                 */

/*-------------------------------------------------------------------------*/
static void
move_object (void)

/* Move the object inter_sp[-1] into object inter_sp[0]; both objects
 * are removed from the stack.
 *
 * The actual move performed by the hooks H_MOVE_OBJECT0/1, this
 * function is called to implement the efuns move_object() and transfer().
 */

{
    lambda_t *l;
    object_t *save_command = command_giver;

    if (NULL != ( l = driver_hook[H_MOVE_OBJECT1].u.lambda) )
    {
        free_object(l->ob, "move_object");
        l->ob = ref_object(inter_sp[-1].u.ob, "move_object");
        call_lambda(&driver_hook[H_MOVE_OBJECT1], 2);
    }
    else if (NULL != ( l = driver_hook[H_MOVE_OBJECT0].u.lambda) )
    {
        free_object(l->ob, "move_object");
        l->ob = ref_object(current_object, "move_object");
        call_lambda(&driver_hook[H_MOVE_OBJECT0], 2);
    }
    else
        errorf("Don't know how to move objects.\n");
    command_giver = check_object(save_command);
} /* move_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_all_environment (svalue_t *sp, int num_arg)

/* EFUN all_environment()
 *
 *   object *all_environment()
 *   object *all_environment(object o)
 *
 * Returns an array with all environments object <o> is in. If <o>
 * is omitted, the environments of the current object is returned.
 *
 * If <o> has no environment, or if <o> is destructed, 0 is
 * returned.
 */

{
    object_t *o;

    /* Get the arg from the stack, if any */
    if (num_arg)
    {
        if (sp->type == T_NUMBER) /* destructed object */
            o = NULL;
        else
        {
            o = ref_object(sp->u.ob, "all_environment");
            free_object_svalue(sp);
        }
    }
    else
    {
        o = current_object;
        sp++;
    }


    /* Default return value: 0 */
    put_number(sp, 0);

    if (o != NULL && !(o->flags & O_DESTRUCTED))
    {
        mp_int num;
        object_t *env;
        vector_t *v;
        svalue_t *svp;

        /* Count the number of environments */
        for ( num = 0, env = o->super
            ; NULL != env
            ; num++, env = env->super)
            NOOP;

        if (num)
        {
            /* Get the array and fill it */
            v = allocate_uninit_array(num);
            for ( svp = v->item, env = o->super
                ; NULL != env
                ; svp++, env = env->super)
            {
                put_ref_object(svp, env, "all_environment");
            }

            /* Put the result on the stack and return */
            put_array(sp, v);
        }
    }

    if (num_arg && o != NULL)
        free_object(o, "all_environment");

    return sp;
} /* v_all_environment() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_all_inventory (svalue_t *sp)

/* EFUN all_inventory()
 *
 *     object *all_inventory(object ob = this_object())
 *
 * Returns an array of the objects contained in the inventory
 * of ob.
 */

{
    vector_t *vec;
    object_t *ob;
    object_t *cur;  /* Current inventory object */
    int cnt, res;

    ob = sp->u.ob;

    /* Count how many inventory objects there are. */
    cnt = 0;
    for (cur = ob->contains; cur; cur = cur->next_inv)
        cnt++;

    if (!cnt)
        vec = allocate_array(0);
    else
    {
        vec = allocate_array(cnt);

        /* Copy the object references */
        cur = ob->contains;
        for (res = 0; res < cnt; res++) {
            put_ref_object(vec->item+res, cur, "all_inventory");
            cur = cur->next_inv;
        }
    }

    free_object_svalue(sp);

    if (vec == NULL)
        put_number(sp, 0);
    else
        put_array(sp, vec);

    return sp;
} /* f_all_inventory() */

/*-------------------------------------------------------------------------*/
static int
deep_inventory_size (object_t *first, p_int level, p_int depth)

/* Helper function for deep_inventory()
 *
 * <level> is the current inventory level this function is called for,
 * <depth> is the desired inventory depth (see v_deep_inventory()).
 *
 * Count the size of <ob>'s inventory by counting the contained objects,
 * invoking this function for every object and then returning the sum
 * of all numbers.
 */

{
    int n;
    object_t *ob;

    n = 0;

    /* Return immediately if the level exceeds the depth */
    if (depth != 0 && level > (depth > 0 ? depth : -depth))
        return 0;

    /* Add the objects of this level if depth allows */
    if (depth >= 0 || level == -depth)
    {
        for (ob = first; ob; ob = ob->next_inv)
            n++;
    }

    /* Recurse into the next level if required */
    if (depth == 0 || (depth > 0 && level < depth) || level < -depth)
    {
        for (ob = first; ob; ob = ob->next_inv)
            if (ob->contains)
                n += deep_inventory_size(ob->contains, level+1, depth);
    }

    return n;
} /* deep_inventory_size() */

/*-------------------------------------------------------------------------*/
static svalue_t *
write_deep_inventory (object_t *first, svalue_t *svp, p_int level, p_int depth)

/* Helper function for deep_inventory()
 *
 * Copy into <svp> and following a reference to all objects in the
 * inventory chain starting with <first>; then invoke this function
 * for every inventory chain in the found objects.
 *
 * <svp> has to point into a suitably big area of svalue elements, like
 * a vector.
 *
 * <level> is the current inventory level this function is called for,
 * <depth> is the desired inventory depth (see v_deep_inventory()).
 *
 * Result is the updated <svp>, pointing to the next free svalue element
 * in the storage area.
 */

{
    object_t *ob;

    /* Return immediately if the level exceeds the depth */
    if (depth != 0 && level > (depth > 0 ? depth : -depth))
        return svp;

    /* Add the objects of this level if depth allows */
    if (depth >= 0 || level == -depth)
    {
        ob = first;
        do {
            put_ref_object(svp, ob, "deep_inventory");
            svp++;
        } while ( NULL != (ob = ob->next_inv) );
    }

    /* Recurse into the next level if required */
    if (depth == 0 || (depth > 0 && level < depth) || level < -depth)
    {
        ob = first;
        do {
            if (ob->contains)
                svp = write_deep_inventory(ob->contains, svp, level+1, depth);
        } while ( NULL != (ob = ob->next_inv) );
    }

    return svp;
} /* write_deep_inventory() */

/*-------------------------------------------------------------------------*/
#if !defined(USE_PARSE_COMMAND)
static
#endif
       vector_t *
deep_inventory (object_t *ob, Bool take_top, p_int depth)

/* Return a vector with the full inventory of <ob>, i.e. all objects contained
 * by <ob> and all deep inventories of those objects, too. The resulting
 * vector is created by a recursive breadth search.
 *
 * If <take_top> is true (and <depth> not negative), <ob> itself is included
 * as first element in the result vector.
 *
 * If <depth> is not 0, it determines the depth up to which the inventory
 * is searched (see v_deep_inventory()).
 *
 * The function is used for the efuns deep_inventory() and parse_command().
 */

{
    vector_t *dinv;  /* The resulting inventory vector */
    svalue_t *svp;   /* Next element to fill in dinv */
    int n;                /* Number of elements in dinv */

    /* Count the contained objects */
    n = (take_top && depth >= 0) ? 1 : 0;
    if (ob->contains) {
        n += deep_inventory_size(ob->contains, 1, depth);
    }

    /* Get the array */
    dinv = allocate_array(n);
    svp = dinv->item;

    /* Fill in <ob> if desired */
    if (take_top && depth >= 0) {
        put_ref_object(svp, ob, "deep_inventory");
        svp++;
    }

    /* Fill in the deep inventory */
    if (ob->contains) {
        write_deep_inventory(ob->contains, svp, 1, depth);
    }

    return dinv;
} /* deep_inventory() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_deep_inventory (svalue_t *sp, int num_arg)

/* EFUN deep_inventory()
 *
 *   object *deep_inventory(void)
 *   object *deep_inventory(object ob)
 *   object *deep_inventory(object ob, int depth)
 *
 * Returns an array of the objects contained in the inventory of
 * ob (or this_object() if no arg given) and in the inventories
 * of these objects, climbing down recursively.
 *
 * If <depth> is given and not 0, the result is limited as follows:
 *   <depth> > 0: Only the objects in the first <depth> levels of inventory
 *                are returned.
 *   <depth> < 0: Only the object in level -<depth> of inventory are returned.
 * In this, level '1' is the inventory of <ob> itself.
 */

{
    vector_t *vec;
    p_int depth = 0;

    /* Get the depth argument from the stack, if any */
    if (num_arg > 1)
    {
        depth = sp->u.number;
        sp--;
    }

    /* If no object was given, push the current object onto the stack */
    if (num_arg < 1)
        push_ref_object(sp, current_object, "deep_inventory");
    inter_sp = sp;

    vec = deep_inventory(sp->u.ob, MY_FALSE, depth);

    free_object_svalue(sp);
    put_array(sp, vec);

    return sp;
} /* f_deep_inventory() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_environment (svalue_t *sp, int num_arg)

/* EFUN environment()
 *
 *   object environment(void)
 *   object environment(object obj)
 *   object environment(string obj)
 *
 * Returns the surrounding object of obj (which may be specified
 * by name). If no argument is given, it returns the surrounding
 * of the current object.
 *
 * Destructed objects do not have an environment.
 */

{
    object_t *ob;

    if (num_arg)
    {
        if (sp->type == T_OBJECT)
        {
            ob = sp->u.ob->super;
            free_object_svalue(sp);
        }
        else /* it's a string */
        {
            ob = find_object(sp->u.str);
            if (!ob || ob->super == NULL || (ob->flags & O_DESTRUCTED))
                ob = NULL;
            else
                ob = ob->super;
            free_string_svalue(sp);
        }
    }
    else if (!(current_object->flags & O_DESTRUCTED))
    {
        ob = current_object->super;
        sp++;
    }
    else
    {
        ob = NULL; /* != environment(this_object()) *boggle* */
        sp++;
    }

    if (ob)
        put_ref_object(sp, ob, "environment");
    else
        put_number(sp, 0);

    return sp;
} /* v_environment() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_first_inventory (svalue_t *sp)

/* EFUN first_inventory()
 *
 *   object first_inventory()
 *   object first_inventory(string ob)
 *   object first_inventory(object ob)
 *
 * Get the first object in the inventory of ob, where ob is
 * either an object or the file name of an object. If ob is not
 * given, the current object is assumed.
 */

{
    object_t *ob;

    ob = NULL;
    if (sp->type == T_OBJECT)
    {
        ob = sp->u.ob->contains;
        free_object_svalue(sp);
    }
    else if (sp->type == T_STRING)
    {
        ob = get_object(sp->u.str);
        if (!ob)
            errorf("No object '%s' for first_inventory()\n", get_txt(sp->u.str));
        free_string_svalue(sp);
        ob = ob->contains;
    }

    if (ob)
        put_ref_object(sp, ob, "first_inventory");
    else
        put_number(sp, 0);

    return sp;
} /* f_first_inventory() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_next_inventory (svalue_t *sp)

/* EFUN next_inventory()
 *
 *   object next_inventory()
 *   object next_inventory(object ob)
 *
 * Get next object in the same inventory as ob. If ob is not
 * given, the current object will be used.
 *
 * This efun is mostly used together with the efun
 * first_inventory().
 */

{
    object_t *ob;

    ob = sp->u.ob;
    free_object(ob, "next_inventory");
    if (ob->next_inv)
        put_ref_object(sp, ob->next_inv, "next_inventory");
    else
        put_number(sp, 0);

    return sp;
} /* f_next_inventory() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_move_object (svalue_t *sp)

/* EFUN move_object()
 *
 *   void move_object(mixed item, mixed dest)
 *
 * The item, which can be a file_name or an object, is moved into
 * it's new environment dest, which can also be file_name or an
 * object.
 *
 * In !compat mode, the only object that can be moved with
 * move_object() is the calling object itself.
 *
 * Since 3.2.1, the innards of move_object() are implemented in
 * the mudlib, using the M_MOVE_OBJECT driver hooks.
 */

{
    object_t *item, *dest;

    inter_sp = sp;

    if ((sp-1)->type == T_STRING)
    {
        item = get_object((sp-1)->u.str);
        if (!item)
            errorf("Bad arg 1 to move_object(): object '%s' not found.\n"
                 , get_txt(sp[-1].u.str));
        free_string_svalue(sp-1);
        put_ref_object(sp-1, item, "move_object");
    }

    if (sp->type == T_STRING)
    {
        dest = get_object(sp->u.str);
        if (!dest)
            errorf("Bad arg 2 to move_object(): object '%s' not found.\n"
                 , get_txt(sp[0].u.str));
        free_string_svalue(sp);
        put_ref_object(sp, dest, "move_object");
    }

    /* move_object() reads its arguments directly from the stack */
    move_object();
    sp -= 2;

    return sp;
} /* f_move_object() */

/*-------------------------------------------------------------------------*/
static object_t *
object_present_in (string_t *str, object_t *ob, p_int num, p_int * num_matched)

/* <ob> is the first object in an environment: test all the objects there
 * if they match the id <str>.
 * If <hasNumber> is false, <str> may be of the form "<id> <num>" - then the
 * <num>th object with this <id> is returned, it it is found.
 * If <hasNumber> is true, the <num>th object with the given id is returned.
 *
 * If the object is not found, *<num_matched> (if not NULL) is set to the
 * number of objects which did match the id.
 */

{
    svalue_t *ret;
    p_int count = 0; /* return the <count+1>th object */

    if (num_matched)
        *num_matched = 0;

    count = num-1;

    /* Now look for the object */
    for (; ob; ob = ob->next_inv)
    {
        push_ref_string(inter_sp, str);
        ret = sapply(STR_ID, ob, 1);
        if (ob->flags & O_DESTRUCTED)
        {
            return NULL;
        }

        if (ret == NULL || (ret->type == T_NUMBER && ret->u.number == 0))
            continue;

        if (num_matched)
            (*num_matched)++;

        if (count-- > 0)
            continue;

        return ob;
    }

    /* Not found */
    return NULL;
} /* object_present_in() */

/*-------------------------------------------------------------------------*/
static object_t *
e_object_present (svalue_t *v, object_t *ob, p_int num)

/* Implementation of the efun present().
 *
 * Look for the <num>th object matching <v> in <ob> and return it if found.
 */

{
    svalue_t *ret;
    object_t *ret_ob;
    p_int num_matched = 0;
    Bool specific = MY_FALSE;

    if (num <= 0)
        num = 1;

    /* Search where? */
    if (!ob)
        ob = current_object;
    else
        specific = MY_TRUE;

    if (ob->flags & O_DESTRUCTED)
        return NULL;

    if (v->type == T_OBJECT)
    {
        /* Oooh, that's easy. */

        if (specific)
        {
            if (v->u.ob->super == ob)
                return v->u.ob;
            else
                return NULL;
        }
        if (v->u.ob->super == ob
         || (v->u.ob->super == ob->super && ob->super != 0))
            return v->u.ob;
        return NULL;
    }

    /* Always search in the object's inventory */
    ret_ob = object_present_in(v->u.str, ob->contains, num, &num_matched);
    if (ret_ob)
        return ret_ob;

    if (specific)
        return NULL;

    /* Search in the environment of <ob> if it was not specified */
    if (!specific && ob->super)
    {
        /* Is it _the_ environment? */
        push_ref_string(inter_sp, v->u.str);
        ret = sapply(STR_ID, ob->super, 1);
        if (ob->super->flags & O_DESTRUCTED)
            return NULL;
        if (ret && !(ret->type == T_NUMBER && ret->u.number == 0))
            return ob->super;

        /* No, search the other objects here. */
        if (num_matched < num)
            return object_present_in(v->u.str, ob->super->contains, num - num_matched, NULL);
    }

    /* Not found */
    return NULL;
} /* e_object_present() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_present (svalue_t *sp, int num_arg)

/* EFUN present()
 *
 *   object present(mixed str)
 *   object present(mixed str, int n)
 *   object present(mixed str, object ob)
 *   object present(mixed str, int n, object ob)
 *
 * If an object that identifies (*) to the name ``str'' is present
 * in the inventory or environment of this_object (), then return
 * it. If "str" has the form "<id> <n>" the <n>-th object matching
 * <id> will be returned.
 * it. If "str" has the form "<id> <n>" OR the (str, n) form is used,
 * the <n>-th object matching <id> will be returned.
 *
 * "str" can also be an object, in which case the test is much faster
 * and easier.
 *
 * A second optional argument ob is the enviroment where the search
 * for str takes place. Normally this_player() is a good choice.
 * Only the inventory of ob is searched, not its environment.
 */

{
    svalue_t *arg;
    object_t *ob;
    p_int num = 1;
    Bool hasNumber = MY_FALSE;

    arg = sp - num_arg + 1;

    /* Get the arguments */
    ob = NULL;
    if (num_arg == 3)
    {
        ob = arg[2].u.ob;
        free_svalue(sp--);
        num_arg--;
    }
    if (num_arg == 2)
    {
        if (arg[1].type == T_NUMBER)
        {
            num = arg[1].u.number;
            hasNumber = MY_TRUE;
        }
        else if (arg[1].type == T_OBJECT)
        {
            if (ob != NULL)
            {
                /* Two objects? No way. */
                vefun_arg_error(2, T_NUMBER, T_OBJECT, sp);
                /* NOTREACHED */
                return sp;
            }
            ob = arg[1].u.ob;
        }
        free_svalue(sp--);
        num_arg--;
    }

    /* If the string is in the form "<id> <number>" and no explicit
     * number was given, parse the <number> out of the string.
     */
    if (!hasNumber && arg->type == T_STRING)
    {
        int   length;
        char *p, *item;

        length = mstrsize(arg->u.str);
        item = get_txt(arg->u.str);

        p = item + length - 1;
        if (*p >= '0' && *p <= '9')
        {
            while(p > item && *p >= '0' && *p <= '9')
                p--;

            if (p > item && *p == ' ')
            {
                num = atoi(p+1);
                length = p - item;
                hasNumber = MY_TRUE;
            }
        }

        /* If we found a number, replace the "<id> <number>" string on
         * the stack with just "<id>".
         */
        if (hasNumber)
        {
            string_t * sitem;
            memsafe(sitem = new_n_mstring(item, length, arg->u.str->info.unicode), length, "id string");
            free_mstring(arg->u.str);
            arg->u.str = sitem;
        }
    }

    inter_sp = sp;
    ob = e_object_present(arg, ob, num);

    free_svalue(arg);
    if (ob)
        put_ref_object(sp, ob, "present");
    else
        put_number(sp, 0);

    return sp;
} /* v_present() */

/*-------------------------------------------------------------------------*/
static void
e_say (svalue_t *v, vector_t *avoid)

/* Implementation of the EFUN say().
 * <v> is the value to say, <avoid> the array of objects to exclude.
 * If the first element of <avoid> is not an object, the function
 * will store its command_giver object into it.
 */

{
    static svalue_t stmp = { T_OBJECT };

    object_t *ob;
    object_t *save_command_giver = command_giver;
    object_t *origin;
    string_t *message;
#define INITIAL_MAX_RECIPIENTS 48
    int max_recipients = INITIAL_MAX_RECIPIENTS;
      /* Current size of the recipients table.
       */
    object_t *first_recipients[INITIAL_MAX_RECIPIENTS];
      /* Initial table of recipients.
       */
    object_t **recipients = first_recipients;
      /* Pointer to the current table of recipients.
       * The end is marked with a NULL entry.
       */
    object_t **curr_recipient = first_recipients;
      /* Next recipient to enter.
       */
    object_t **last_recipients =
                 &first_recipients[INITIAL_MAX_RECIPIENTS-1];
      /* Last entry in the current table.
       */

    /* Determine the command_giver to use */
    if (current_object->flags & O_ENABLE_COMMANDS)
    {
        command_giver = current_object;
    }
    else if (current_object->flags & O_SHADOW
          && O_GET_SHADOW(current_object)->shadowing)
    {
        command_giver = O_GET_SHADOW(current_object)->shadowing;
    }

    /* Determine the originating object */
    if (command_giver)
    {
        interactive_t *ip;

        if (O_SET_INTERACTIVE(ip, command_giver))
        {
            trace_level |= ip->trace_level;
        }
        origin = command_giver;

        /* Save the commandgiver to avoid, if needed */
        if (avoid->item[0].type == T_INVALID)
        {
            put_ref_object(avoid->item, command_giver, "say");
        }
    }
    else
        origin = current_object;

    /* Sort the avoid vector for fast lookups.
     * The caller will free the original <avoid>.
     */
    avoid = order_array(avoid);
    push_array(inter_sp, avoid); /* In case of errors */

    /* Collect the list of propable recipients.
     * First, look in the environment.
     */
    if ( NULL != (ob = origin->super) )
    {
        interactive_t *ip;

        /* The environment itself? */
        if (ob->flags & O_ENABLE_COMMANDS
         || O_SET_INTERACTIVE(ip, ob))
        {
            *curr_recipient++ = ob;
        }

        for (ob = ob->contains; ob; ob = ob->next_inv)
        {
            if (ob->flags & O_ENABLE_COMMANDS
             || O_SET_INTERACTIVE(ip,ob))
            {
                if (curr_recipient >= last_recipients)
                {
                    /* Increase the table */
                    max_recipients *= 2;
                    curr_recipient = alloca(max_recipients * sizeof(object_t *));
                    memcpy( curr_recipient, recipients
                           , max_recipients * sizeof(object_t *) / 2);
                    recipients = curr_recipient;
                    last_recipients = &recipients[max_recipients-1];
                    curr_recipient += (max_recipients / 2) - 1;
                }
                *curr_recipient++ = ob;
            }
        } /* for() */
    } /* if(environment) */

    /* Now check this environment */
    for (ob = origin->contains; ob; ob = ob->next_inv)
    {
        interactive_t *ip;

        if (ob->flags & O_ENABLE_COMMANDS
         || O_SET_INTERACTIVE(ip, ob))
        {
            if (curr_recipient >= last_recipients)
            {
                /* Increase the table */
                max_recipients *= 2;
                curr_recipient = alloca(max_recipients * sizeof(object_t *));
                memcpy( curr_recipient, recipients
                      , max_recipients * sizeof(object_t *) / 2);
                recipients = curr_recipient;
                last_recipients = &recipients[max_recipients-1];
                curr_recipient += (max_recipients / 2) - 1;
            }
            *curr_recipient++ = ob;
        }
    }

    *curr_recipient = NULL;  /* Mark the end of the list */

    /* Construct the message. */

    switch(v->type)
    {
    case T_STRING:
        message = v->u.str;
        break;

    case T_OBJECT:
    case T_POINTER:
    case T_MAPPING:
    case T_STRUCT:
        /* tell_room()'s evil twin: send <v> to all recipients' catch_msg() lfun */

        for (curr_recipient = recipients; NULL != (ob = *curr_recipient++) ; )
        {
            if (ob->flags & O_DESTRUCTED)
                continue;
            stmp.u.ob = ob;
            if (lookup_key(&stmp, avoid) >= 0)
                continue;

            assign_svalue_no_free(++inter_sp, v);
            push_ref_object(inter_sp, origin, "say");
            sapply(STR_CATCH_MSG, ob, 2);
        }
        pop_stack(); /* free avoid alist */
        command_giver = check_object(save_command_giver);
        return;

    default:
        errorf("Invalid argument to say(): expected '%s', got '%s'.\n"
              , efun_arg_typename(T_POINTER|T_MAPPING|T_STRUCT|T_STRING|T_OBJECT)
              , typename(v->type));
    }

    /* Now send the message to all recipients */

    for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); )
    {
        if (ob->flags & O_DESTRUCTED)
            continue;
        stmp.u.ob = ob;
        if (lookup_key(&stmp, avoid) >= 0)
            continue;
        tell_object (ob, message);
    }

    pop_stack(); /* free avoid alist */
    command_giver = check_object(save_command_giver);
} /* e_say() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_say (svalue_t *sp, int num_arg)

/* EFUN say()
 *
 *   void say(string str)
 *   void say(string str, object exclude)
 *   void say(string str, object *excludes)
 *   void say(mixed *arr)
 *   void say(mixed *arr, object exclude)
 *   void say(mixed *arr, object *excludes)
 *
 * There are two major modes of calling:
 *
 * If the first argument is a string <str>, it will be send to
 * all livings in the current room	except to the initiator.
 *
 * If the first argument is an array <arr>, the lfun catch_msg()
 * of all living objects except the initiator will be called.
 * This array will be given as first argument to the lfun, and
 * the initiating object as the second.
 *
 * By specifying a second argument to the efun one can exclude
 * more objects than just the initiator. If the second argument
 * is a single object <exclude>, both the given object and the
 * initiator are excluded from the call. If the second argument
 * is an array <excludes>, all objects and just the objects in
 * this array are excluded from the call.
 *
 * The 'initiator' is determined according to these rules:
 *   - if the say() is called from within a living object, this
 *     becomes the initiator
 *   - if the say() is called from within a dead object as result
 *     of a user action (i.e. this_player() is valid), this_player()
 *     becomes the initiator.
 *   - Else the object calling the say() becomes the initiator.
 */

{
    if (num_arg == 1)
    {
        /* No objects to exclude */
        vector_t *vec = allocate_uninit_array(1);

        /* Mark this item as the place for the command giver. */
        vec->item[0].type = T_INVALID;

        /* Save it in case of errors. */
        push_array(sp, vec);
        inter_sp = sp;

        e_say(sp-1, vec);

        free_svalue(sp--);
    }
    else
    {
        /* We have objects to exclude */

        if ( sp->type == T_POINTER )
        {
            e_say(sp-1, sp->u.vec);
        }
        else /* it's an object */
        {
            /* Create an array with the command giver and the object. */
            vector_t *vec = allocate_uninit_array(2);
            vec->item[0].type = T_INVALID;
            transfer_svalue_no_free(vec->item + 1, sp);
            put_array(sp, vec);

            e_say(sp-1, vec);
        }
        free_svalue(sp--);
    }

    free_svalue(sp--);

    return sp;
} /* v_say() */

/*-------------------------------------------------------------------------*/
static void
e_tell_room (object_t *room, svalue_t *v, vector_t *avoid)

/* Implementation of the EFUN tell_room().
 *
 * Value <v> is sent to all living objects in <room>, except those
 * in <avoid>. <avoid> has to be in order_array() order.
 */

{
    object_t *ob;
    int num_recipients = 0;
    object_t *some_recipients[20];
    object_t **recipients;
    object_t **curr_recipient;
    string_t *message;
    static svalue_t stmp = { T_OBJECT, } ;
    interactive_t *ip;        

    /* Like in say(), collect the possible recipients.
     * First count how many there are.
     */

    for (ob = room->contains; ob; ob = ob->next_inv)
    {
        if ( ob->flags & O_ENABLE_COMMANDS
         ||  O_SET_INTERACTIVE(ip, ob))
        {
            num_recipients++;
        }
    }
    /* The room/environment itself? */
    if (room->flags & O_ENABLE_COMMANDS
        || O_SET_INTERACTIVE(ip, room)) 
    {
        num_recipients++;
    }

    /* Allocate the table */
    if (num_recipients < 20)
        recipients = some_recipients;
    else
        recipients =
          alloca( (num_recipients+1) * sizeof(object_t *) );

    /* Now fill the table */
    curr_recipient = recipients;
    /* The room/environment itself? */
    if (room->flags & O_ENABLE_COMMANDS
        || O_SET_INTERACTIVE(ip, room)) 
    {
            *curr_recipient++ = room;
    }
    /* now the objects in the room/container */
    for (ob = room->contains; ob; ob = ob->next_inv)
    {
        if ( ob->flags & O_ENABLE_COMMANDS
         ||  O_SET_INTERACTIVE(ip, ob))
        {
            *curr_recipient++ = ob;
        }
    }

    *curr_recipient = NULL; /* Mark the end of the table */

    /* Construct the message */
    switch(v->type)
    {
    case T_STRING:
        message = v->u.str;
        break;

    case T_OBJECT:
    case T_POINTER:
    case T_MAPPING:
    case T_STRUCT:
      {
        /* say()s evil brother: send <v> to all recipients'
         * catch_msg() lfun
         */
        object_t *origin = command_giver;

        if (!origin)
            origin = current_object;

        for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); )
        {
            if (ob->flags & O_DESTRUCTED)
                continue;
            stmp.u.ob = ob;
            if (lookup_key(&stmp, avoid) >= 0)
                continue;
            assign_svalue_no_free(++inter_sp, v);
            push_ref_object(inter_sp, origin, "tell_room");
            sapply(STR_CATCH_MSG, ob, 2);
        }
        return;
      }

    default:
        errorf("Invalid argument to tell_room(): expected '%s', got '%s'.\n"
              , efun_arg_typename(T_POINTER|T_MAPPING|T_STRUCT|T_STRING|T_OBJECT)
              , typename(v->type));
    }

    /* Now send the message to all recipients */

    for (curr_recipient = recipients; NULL != (ob = *curr_recipient++); )
    {
        if (ob->flags & O_DESTRUCTED) continue;
        stmp.u.ob = ob;
        if (lookup_key(&stmp, avoid) >= 0) continue;
        tell_object(ob, message);
    }
} /* e_tell_room() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_tell_room (svalue_t *sp, int num_arg)

/* EFUN tell_room()
 *
 *   void tell_room(string|object ob, string str)
 *   void tell_room(string|object ob, string str, object *exclude)
 *   void tell_room(string|object ob, mixed *msg)
 *   void tell_room(string|object ob, mixed *msg, object *exclude)
 *
 * Send a message str to all living objects in the room ob. ob
 * can also be the name of the room given as a string. If a
 * receiving object is not a interactive user the lfun
 * catch_tell() of the object will be invoked with the message as
 * argument. If living objects define catch_tell(), the string
 * will also be sent to that instead of being written to the
 * user. If the object is given as its filename, the driver
 * looks up the object under that name, loading it if necessary.
 * If array *exclude is given, all objects contained in
 * *exclude are excluded from the message str.
 *
 * If the second arg is an array, catch_msg() will be called in
 * all listening livings.
 */

{
    svalue_t *arg;
    vector_t *avoid;
    object_t *ob;

    arg = sp- num_arg + 1;

    /* Test the arguments */
    if (arg[0].type == T_OBJECT)
        ob = arg[0].u.ob;
    else /* it's a string */
    {
        ob = get_object(arg[0].u.str);
        if (!ob)
            errorf("Object '%s' not found.\n", get_txt(arg[0].u.str));
    }

    if (num_arg == 2)
    {
        avoid = &null_vector;
    }
    else
    {
        /* Sort the list of objects to exclude for faster
         * operation.
         */
        avoid = order_array(arg[2].u.vec);
        free_array(arg[2].u.vec);
        sp->u.vec = avoid; /* in case of an error, this will be freed. */
    }

    e_tell_room(ob, arg+1, avoid);

    if (num_arg > 2)
    {
        free_svalue(sp--);
    }
    free_svalue(sp--);
    free_svalue(sp--);

    return sp;
} /* v_tell_room() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_environment (svalue_t *sp)

/* EFUN set_environment()
 *
 *   void set_environment(object item, object env)
 *
 * The item is moved into its new environment env, which may be 0.
 * This efun is to be used in the H_MOVE_OBJECTx hook, as it does
 * nothing else than moving the item - no calls to init() or such.
 */

{
    object_t *item, *dest;
    object_t **pp, *ob;
    object_t *save_cmd = command_giver;

    /* Get and test the arguments */

    item = sp[-1].u.ob;

    if (item->flags & O_SHADOW && O_GET_SHADOW(item)->shadowing)
        errorf("Can't move an object that is shadowing.\n");

    if (sp->type != T_OBJECT)
    {
        dest = NULL;
    }
    else
    {
        dest = sp->u.ob;
        /* Recursive moves are not allowed. */
        for (ob = dest; ob; ob = ob->super)
            if (ob == item)
                errorf("Can't move object inside itself.\n");

        dest->flags &= ~O_RESET_STATE;
    }

    item->flags &= ~O_RESET_STATE; /* touch it */

    if (item->super)
    {
        /* First remove the item out of its current environment */
        Bool okey = MY_FALSE;

        item->super->flags &= ~O_RESET_STATE;

        if (item->sent)
        {
            remove_environment_sent(item);
        }

        if (item->super->sent)
            remove_action_sent(item, item->super);

        for (pp = &item->super->contains; *pp;)
        {
            if (*pp != item)
            {
                if ((*pp)->sent)
                    remove_action_sent(item, *pp);
                pp = &(*pp)->next_inv;
                continue;
            }
            *pp = item->next_inv;
            okey = MY_TRUE;
        }

        if (!okey)
            fatal("Failed to find object %s in super list of %s.\n",
                  get_txt(item->name), get_txt(item->super->name));
    }

    /* Now put it into its new environment (if any) */
    item->super = dest;
    if (!dest)
    {
        item->next_inv = NULL;
    }
    else
    {
        item->next_inv = dest->contains;
        dest->contains = item;
    }

    command_giver = check_object(save_cmd);
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    return sp - 1;
} /* f_set_environment() */

/*-------------------------------------------------------------------------*/
#ifdef USE_DEPRECATED

svalue_t *
f_transfer (svalue_t *sp)

/* EFUN transfer()
 *
 *   int transfer(object item, object dest)
 *
 * This efun is for backward compatibility only. It is only
 * available in compat mode.
 *
 * Move the object "item" to the object "dest". All kinds of
 * tests are done, and a number is returned specifying the
 * result:
 *
 *     0: Success.
 *     1: To heavy for destination.
 *     2: Can't be dropped.
 *     3: Can't take it out of it's container.
 *     4: The object can't be inserted into bags etc.
 *     5: The destination doesn't allow insertions of objects.
 *     6: The object can't be picked up.
 *
 * If an object is transfered to a newly created object, make
 * sure that the new object first is transfered to it's
 * destination.
 *
 * The efun calls add_weight(), drop(), get(), prevent_insert(),
 * add_weight(), and can_put_and_get() where needed.
 */

{
    object_t *ob, *to;
    svalue_t *v_weight, *ret;
    int       weight;
    object_t *from;
    int       result;

    /* Get and test the arguments */
    ob = sp[-1].u.ob;

    if (sp->type == T_OBJECT)
        to = sp->u.ob;
    else /* it's a string */
    {
        to = get_object(sp->u.str);
        if (!to)
            errorf("Object %s not found.\n", get_txt(sp->u.str));
        free_string_svalue(sp);
        put_ref_object(sp, to, "transfer"); /* for move_object() below */
    }

    from = ob->super;
    result = 0; /* Default: success result */

    /* Perform the transfer step by step */
    switch(0){default:

        /* Get the weight of the object
         */
        weight = 0;
        v_weight = sapply(STR_QUERY_WEIGHT, ob, 0);
        if (v_weight && v_weight->type == T_NUMBER)
            weight = v_weight->u.number;

        if (ob->flags & O_DESTRUCTED)
        {
            result = 3;
            break;
        }

        /* If the original place of the object is a living object,
         * then we must call drop() to check that the object can be dropped.
         */
        if (from && (from->flags & O_ENABLE_COMMANDS))
        {
            ret = sapply(STR_DROP, ob, 0);
            if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
            {
                result = 2;
                break;
            }

            /* This should not happen, but we can not trust LPC hackers. :-) */
            if (ob->flags & O_DESTRUCTED)
            {
                result = 2;
                break;
            }
        }

        /* If 'from' is not a room and not a player, check that we may
         * remove things out of it.
         */
        if (from && from->super && !(from->flags & O_ENABLE_COMMANDS))
        {
            ret = sapply(STR_CANPUTGET, from, 0);
            if (!ret
             || (ret->type == T_NUMBER && ret->u.number == 0)
             || (from->flags & O_DESTRUCTED)
             || (ob->flags & O_DESTRUCTED))
            {
                result = 3;
                break;
            }
        }

        /* If the destination is not a room, and not a player,
         * Then we must test 'prevent_insert', and 'can_put_and_get'.
         */
        if (to->super && !(to->flags & O_ENABLE_COMMANDS))
        {
            ret = sapply(STR_PREVENT_INSERT, ob, 0);
            if (ret && (ret->type != T_NUMBER || ret->u.number != 0))
            {
                result = 4;
                break;
            }

            ret = sapply(STR_CANPUTGET, to, 0);
            if (!ret
             || (ret->type == T_NUMBER && ret->u.number == 0)
             || (to->flags & O_DESTRUCTED)
             || (ob->flags & O_DESTRUCTED))
            {
                result = 5;
                break;
            }
        }

        /* If the destination is a player, check that he can pick it up.
         */
        if (to->flags & O_ENABLE_COMMANDS)
        {
            ret = sapply(STR_GET, ob, 0);
            if (!ret
             || (ret->type == T_NUMBER && ret->u.number == 0)
             || (ob->flags & O_DESTRUCTED))
            {
                result = 6;
                break;
            }
        }

        /* If it is not a room, correct the total weight in
         * the destination.
         */
        if (to->super && weight)
        {
            /* Check if the destination can carry that much.
             */
            push_number(inter_sp, weight);
            ret = sapply(STR_ADD_WEIGHT, to, 1);
            if (ret && ret->type == T_NUMBER && ret->u.number == 0)
            {
                result = 1;
                break;
            }

            if (to->flags & O_DESTRUCTED)
            {
                result = 1;
                break;
            }
        }

        /* If it is not a room, correct the weight in
         * the 'from' object.
         */
        if (from && from->super && weight)
        {
            push_number(inter_sp, -weight);
            (void)sapply(STR_ADD_WEIGHT, from, 1);
        }

        /* When we come here, the move is ok */
    } /* pseudo-switch() */

    if (result)
    {
        /* All the applys might have changed these */
        free_svalue(sp);
        free_svalue(sp-1);
    }
    else
    {
        /* The move is ok: do it (and use up both arguments) */
        inter_sp = sp;
        move_object();
    }

    put_number(sp-1, result);
    return sp-1;
} /* f_transfer() */

#endif /* USE_DEPRECATED */

/*=========================================================================*/

/*                        Save/Restore an Object                           */

/*
 * TODO: The functions don't work properly if an object contains several
 * TODO:: variables of the same name, and their order/location in the
 * TODO:: variable block change between save and restore.
 * TODO: The functions should push an error-handler-svalue on the stack so
 * TODO:: that in case of errors everything (memory, files, svalues) can
 * TODO:: be deallocated properly. Right now, some stuff may be left behind.
 */

/*-------------------------------------------------------------------------*/
/* The 'version' of each savefile is given in the first line as
 *   # <version>:<host>
 *
 * <version> is currently 2
 *    Version 0 didn't allow the saving of non-lambda closures, symbols
 *    or quoted arrays.
 *    Version 2 uses only one representation for storing LPC floats:
 *     the one used by sprintf("%la").
 *    Version 3 can also save lvalues references (without it,
 *     the plain rvalue will be saved).
 * <host> is 2 for drivers using the FLOAT_FORMAT_2, and 0 for everything else.
 *    The difference lies in the handling of float numbers (see svalue.h).
 */

  /* Current version of new save files, expressed as char and as int.
   */
#define SAVE_OBJECT_VERSION '3'

enum save_format_version
{
    SAVE_FORMAT_ORIGINAL = 0, /* Original format from Amylaar's LPMud */
    SAVE_FORMAT_CLOSURES = 1, /* LDMud 3.2.9 added Closures           */
    SAVE_FORMAT_FLOATS   = 2, /* LDMud 3.5.0 changed float format     */
    SAVE_FORMAT_LVALUES  = 3, /* LDMud 3.5.0 added lvalue references  */

    CURRENT_VERSION      = 3
};

#ifdef FLOAT_FORMAT_0
#    define SAVE_OBJECT_HOST '0'
#    define CURRENT_HOST 0
#endif

#ifdef FLOAT_FORMAT_2
#    define SAVE_OBJECT_HOST '2'
#    define CURRENT_HOST 2
#endif

/*-------------------------------------------------------------------------*/
/* Forward Declarations */

static Bool save_svalue(svalue_t *, char, Bool);
static Bool restore_svalue(svalue_t *, char **, char);
static void register_svalue(svalue_t *);

/*-------------------------------------------------------------------------*/

#define SAVE_OBJECT_BUFSIZE 4096
  /* Size of the read/write buffer.
   */

static int save_version = -1;
  /* The version of the savefile to write.
   */

static const char save_file_suffix[] = ".o";
  /* The suffix of the save file, in an array for easier computations.
   * (sizeof() vs. strlen()+1.
   */

static struct pointer_table *ptable = NULL;
  /* The pointer_table used to register all arrays and mappings.
   * If an error happens during the save, this table probably won't
   * be deallocated.
   */

static char number_buffer[36];
  /* Buffer to create numbers in - big enough for 64 Bit uints, our 48 Bit
   * floats and the native doubles used in FLOAT_FORMAT_2.
   */

static char *save_object_bufstart;
  /* Start of the write buffer (which lives on the stack).
   */

static char *buf_pnt;
  /* Current position in the write buffer.
   */

static int buf_left;
  /* Space left in the write buffer.
   */

static mp_int bytes_written;
  /* Number of bytes so far written to the file or strbuf.
   */

static Bool failed;
  /* An IO error occurred.
   */

static int save_object_descriptor = -1;
  /* FD of the savefile, -1 if not assigned.
   */

static strbuf_t save_string_buffer;
  /* When saving to a string: the string buffer.
   */

static mp_int current_sv_id_number;
  /* The highest ID number so far assigned to a shared value when
   * writing a savefile.
   */

struct restore_context_s {

    int restored_host;
      /* Type of the host which wrote the savefile being restored.
       */
    
    int restored_version;
      /* Version used in the savefile/string currently being restored.
       */

    long current_shared_restored;
      /* ID of the shared value currently restored
       */

    svalue_t *shared_restored_values;
      /* Array of restored shared values, so that later references
       * can do a simple lookup by ID-1 (IDs start at 1).
       */

    long max_shared_restored;
      /* Current size of shared_restored_values.
       */
       
    struct restore_context_s *previous;
      /* The previous context. */
};

static struct restore_context_s *restore_ctx = NULL;

/*-------------------------------------------------------------------------*/
/* Macros */

#define MY_PUTC(ch) {\
    *buf_pnt++ = ch;\
    if (!--  buf_left) {\
        buf_pnt = write_buffer();\
        buf_left = SAVE_OBJECT_BUFSIZE;\
    }\
}
/* Put <ch> into the write buffer, flushing the buffer to
 * the file if necessary.
 */


/* The following three macros handle the write buffer access
 * through local variables to achieve a greater speed:
 */

#define L_PUTC_PROLOG char *l_buf_pnt = buf_pnt;\
                      int  l_buf_left = buf_left;
/* Declare and initialize the local variables.
 */

#define L_PUTC(ch) {\
    *l_buf_pnt++ = ch;\
    if (!--l_buf_left) {\
        l_buf_pnt = write_buffer();\
        l_buf_left = SAVE_OBJECT_BUFSIZE;\
    }\
}
/* Put <ch> into the write buffer, flushing the buffer to
 * the file if necessary.
 */

#define L_PUT_HEX(d) {              \
    unsigned char digit = (d);      \
    if (digit >= 0x0a)              \
        L_PUTC('a' + digit - 0x0a)  \
    else                            \
        L_PUTC('0' + digit)         \
}
/* Write the digit (0 - 15) hexadecimally into the write buffer. */


#define L_PUTC_EPILOG buf_pnt = l_buf_pnt; buf_left = l_buf_left;
/* Update the global buffer variables with the local values.
 */

#define CTRLZ 30
/* MS-DOS and Windows files sometimes have this character :-(
 */

/*-------------------------------------------------------------------------*/
void
free_save_object_buffers(void)

/* Deallocate all lingering buffers from previous save operations, preferably
 * before the GC does it.
 */

{
    if (ptable)
        free_pointer_table(ptable);
    ptable = NULL;
} /* free_save_object_buffers() */

/*-------------------------------------------------------------------------*/
static char*
write_buffer (void)

/* Write the current content of the write buffer to the savefile
 * resp. to the string buffer and return a pointer to its start.
 *
 * On an error, set failed to TRUE.
 */

{
    char *start;

    start = save_object_bufstart;
    if (save_object_descriptor >= 0)
    {

        if (write( save_object_descriptor, start, SAVE_OBJECT_BUFSIZE )
          != SAVE_OBJECT_BUFSIZE )
            failed = MY_TRUE;
    }
    else
        strbuf_addn(&save_string_buffer, start, SAVE_OBJECT_BUFSIZE);

    bytes_written += SAVE_OBJECT_BUFSIZE;
    return start;
} /* write_buffer() */

/*-------------------------------------------------------------------------*/
static Bool
recall_pointer (void *pointer)

/* Lookup the (known to be registered) <pointer> in the pointertable and
 * check the number of registrations.
 *
 * If it was registered just once, just return FALSE.
 * If it was registered several times (ie. it is a shared array/mapping),
 * write its ID number (which is assigned if necessary) as '<id>'
 * to the write buffer. If this is not the first time this particular
 * pointer was recalled, add a '=' and return FALSE, else return TRUE.
 *
 * If the function returns FALSE, the caller has to write the actual
 * data of the array/mapping.
 */

{
    struct pointer_record *record;

    /* We know for sure that we will find the key, because it has been
     * registered before.
     */
    record = lookup_pointer(ptable, pointer);

    if (!record->ref_count)
        /* Used only once. No need for special treatment. */
        return MY_FALSE;

    if (pointer == (char*)&null_vector)
        /* Sharing enforced by the game driver */
        return MY_FALSE;

    /* Write the '<id>' text */

    {
        long old_id, id;
        char *source, c;
        L_PUTC_PROLOG

        /* If this pointer was recalled the first time, assign
         * an ID number.
         */
        if ( !(old_id = id = record->id_number) )
        {
            id = ++current_sv_id_number;
            record->id_number = id;
        }

        /* Write '<id>' */

        L_PUTC('<')
        source = number_buffer;
        (void)snprintf(source, sizeof(number_buffer), "%ld", id);
        c = *source++;
        do L_PUTC(c) while ( '\0' != (c = *source++) );
        L_PUTC('>')

        if (old_id)
        {
            /* has been written before */
            L_PUTC_EPILOG
            return MY_TRUE;
        }

        /* First encounter: add a '=' */
        L_PUTC('=')
        L_PUTC_EPILOG
        return MY_FALSE;
    }

    /* NOTREACHED */
} /* recall_pointer() */

/*-------------------------------------------------------------------------*/
static void
save_number (p_int num)

/* Write the number <num>to the write buffer.
 */

{
    L_PUTC_PROLOG
    char *source, c;

    source = number_buffer;
    (void)snprintf(source, sizeof(number_buffer), "%"PRIdPINT, num);
    c = *source++;
    do L_PUTC(c) while ( '\0' != (c = *source++) );
    L_PUTC_EPILOG
} /* save_number() */

/*-------------------------------------------------------------------------*/
static void
save_string (string_t *src, p_int start, p_int count, bool noquotes)

/* Write string <src> to the write buffer, but escape all funny
 * characters.
 * If <start> is >= 0, then only write src[start..start+count-1].
 * If <noquotes> is true, then don't print the quotation marks.
 */

{
    register char *cp;
    size_t len;

    L_PUTC_PROLOG

    cp = get_txt(src);

    if (start < 0)
    {
        len = mstrsize(src);
    }
    else
    {
        cp += start;
        len = count;
    }

    if (src->info.unicode == STRING_BYTES)
    {
        if (!noquotes)
        {
            L_PUTC('b')
            L_PUTC('\"')
        }

        while (len--)
        {
            unsigned char b = (unsigned char) *cp++;
            L_PUTC('\\')
            L_PUTC('x')

            L_PUT_HEX(b >> 4);
            L_PUT_HEX(b & 0x0f);
        }

        if (!noquotes)
            L_PUTC('\"')
    }
    else
    {
        if (!noquotes)
            L_PUTC('\"')

        while ( len )
        {
            p_int c;
            size_t clen = utf8_to_unicode(cp, len, &c);
            if (!clen)
            {
                c = *(unsigned char*)cp;
                clen = 1;
            }

            cp += clen;
            len -= clen;

            if (c >= 0x80)
            {
                L_PUTC('\\');
                if (c >= 0x10000)
                {
                    L_PUTC('U');
                    L_PUT_HEX((c & 0xf0000000) >> 28);
                    L_PUT_HEX((c & 0x0f000000) >> 24);
                    L_PUT_HEX((c & 0x00f00000) >> 20);
                    L_PUT_HEX((c & 0x000f0000) >> 16);
                }
                else
                    L_PUTC('u');
                L_PUT_HEX((c & 0xf000) >> 12);
                L_PUT_HEX((c & 0x0f00) >>  8);
                L_PUT_HEX((c & 0x00f0) >>  4);
                L_PUT_HEX((c & 0x000f));
                continue;
            }
            else if (isescaped(c))
            {
                switch(c) {
                case '\007': c = 'a'; break;
                case '\b'  : c = 'b'; break;
                case '\t'  : c = 't'; break;
                case '\n'  : c = 'n'; break;
                case '\013': c = 'v'; break;
                case '\014': c = 'f'; break;
                case '\r'  : c = 'r'; break;
                }
                L_PUTC('\\')
            }
            else if (c == '\0')
            {
                c = '0';
                L_PUTC('\\')
            }
            L_PUTC(c)
        }
        if (!noquotes)
            L_PUTC('\"')
    }

    L_PUTC_EPILOG
} /* save_string() */

/*-------------------------------------------------------------------------*/
static void
save_mapping_filter (svalue_t *key, svalue_t *data, void *extra)

/* Filter used by save_mapping: write <key> and (p_int)<extra> values
 * in <data>[] to the write buffer.
 */

{
    int i;

    i = (p_int)extra;
    if (save_svalue(key, (char)(i ? ':' : ','), MY_TRUE))
    {
        while (--i >= 0)
            save_svalue(data++, (char)(i ? ';' : ','), MY_FALSE );
    }
} /* save_mapping_filter() */

/*-------------------------------------------------------------------------*/
static void
save_mapping (mapping_t *m)

/* Write the mapping <m> to the write buffer.
 * Empty mappings with width != 1 are written as '([:<width>])'.
 */

{
    mp_int old_written;

    /* If it is shared, write its ID, and maybe we're already
     * done then.
     */
    if ( recall_pointer(m) )
        return;

    /* Nope, write it */

    MY_PUTC('(')
    MY_PUTC('[')

    old_written = bytes_written - buf_left;
    walk_mapping(m, save_mapping_filter, (void *)(p_int)m->num_values);

    /* If the mapping is empty and has width other than 1,
     * use a special format
     */
    if (m->num_values != 1 && old_written == bytes_written - buf_left)
    {
        char *source, c;

        MY_PUTC(':')
        source = number_buffer;
        (void)snprintf(source, sizeof(number_buffer), "%"PRIdPINT, m->num_values);
        c = *source++;
        do MY_PUTC(c) while ( '\0' != (c = *source++) );
    }

    MY_PUTC(']')
    MY_PUTC(')')
} /* save_mapping() */

/*-------------------------------------------------------------------------*/
static void
save_array (vector_t *v, p_int start, p_int count)

/* Encode the array <v> and write it to the write buffer.
 * If <start> is >= 0, then only write v[start..start+count-1].
 */

{
    p_int i;
    svalue_t *val;

    /* Recall the array from the pointer table.
     * If it is a shared one, there's nothing else to do.
     */
    if (start < 0 && recall_pointer(v))
        return;

    /* Write the '(<'... */
    {
        L_PUTC_PROLOG
        L_PUTC('(')
        L_PUTC('{')
        L_PUTC_EPILOG
    }

    if (start < 0)
    {
        start = 0;
        count = VEC_SIZE(v);
    }

    /* ... the values ... */
    for (i = count, val = v->item + start; --i >= 0; )
    {
        save_svalue(val++, ',', MY_FALSE);
    }

    /* ... and the '>)' */
    {
        L_PUTC_PROLOG
        L_PUTC('}')
        L_PUTC(')')
        L_PUTC_EPILOG
    }
} /* save_array() */

/*-------------------------------------------------------------------------*/
static void
save_struct (struct_t *st)

/* Encode the struct <st> and write it to the write buffer.
 */

{
    long i;
    svalue_t *val;

    /* Recall the struct from the pointer table.
     * If it is a shared one, there's nothing else to do.
     */
    if (recall_pointer(st))
        return;

    /* Write the '(<'... */
    {
        L_PUTC_PROLOG
        L_PUTC('(')
        L_PUTC('<')
        L_PUTC_EPILOG
    }

    /* The unique name (struct_name prog_name #id) as fake member */
    if (save_version < SAVE_FORMAT_CLOSURES || !recall_pointer(struct_unique_name(st)))
    {
        struct_type_t *stt = st->type;

        MY_PUTC('\"')
        save_string(struct_unique_name(st), -1, -1, true);

        /* Add a list of the members, so we can restore them
         * later even if the definition changed.
         */
        for (unsigned short ix = 0; ix < stt->num_members; ix++)
        {
            string_t *name = stt->member[ix].name;
            long pos = mstrchr(name, ',');

            if (ix)
                MY_PUTC(',')
            else
                MY_PUTC(' ')

            if (pos > 0)
                save_string(name, 0, pos, true);
            else if (pos == 0)
                save_string(STR_ANONYMOUS, -1, -1, true);
            else
                save_string(name, -1, -1, true);
        }

        MY_PUTC('\"')
    }
    MY_PUTC(',')

    /* ... the values ... */
    for (i = (long)struct_size(st), val = st->member; --i >= 0; )
    {
        save_svalue(val++, ',', MY_FALSE);
    }

    /* ... and the '>)' */
    {
        L_PUTC_PROLOG
        L_PUTC('>')
        L_PUTC(')')
        L_PUTC_EPILOG
    }
} /* save_struct() */

/*-------------------------------------------------------------------------*/
static Bool
save_closure (svalue_t *cl, Bool writable)

/* Encode the struct <st> and write it to the write buffer.
 * If <writable> is false, unwritable closure are written
 * as '0'. If <writable> is true, unwritable closures are not written at all.
 *
 * Return is true if something was written, and false otherwise.
 */

{
    Bool rc = MY_TRUE;
    int type;

    switch(type = cl->x.closure_type)
    {
    case CLOSURE_LFUN:
      {
        if (recall_pointer(cl->u.lambda))
            break;

        if (cl->u.lambda->function.lfun.ob == current_object
         && cl->u.lambda->ob == current_object
           )
        {
            lambda_t       *l;
            program_t      *prog;
            program_t      *inhProg = 0;
            int             ix;
            char           *source, c;
            object_t       *ob;

            l = cl->u.lambda;
            ob = l->function.lfun.ob;
            ix = l->function.lfun.index;
            inhProg = l->function.lfun.inhProg;

            prog = ob->prog;

            if (inhProg)
            {
                /* An inherited lfun closure. Go to the inherit. */
                while (prog != inhProg)
                {
                    inherit_t *inheritp = search_function_inherit(prog, ix);
                    ix -= inheritp->function_index_offset;
                    prog = inheritp->prog;
                }
            }

            source = get_txt(get_function_header(prog, ix)->name);

            {
                L_PUTC_PROLOG
                L_PUTC('#');
                if (l->function.lfun.context_size)
                {
                    L_PUTC('c');
                }
                else
                {
                    L_PUTC('l');
                }
                L_PUTC(':');
                c = *source++;
                do L_PUTC(c) while ( '\0' != (c = *source++) );
                L_PUTC_EPILOG
            }

            /* For inherited lfun closures, add the '|<inheritpath>' */
            if (inhProg)
            {
                prog = ob->prog;
                ix = l->function.lfun.index;
                
                while(prog != inhProg)
                {
                    inherit_t *inheritp;
                    string_t  *progName;
                    
                    inheritp = search_function_inherit(prog, ix);
                    ix -= inheritp->function_index_offset;
                    prog = inheritp->prog;
                    progName = del_dotc(prog->name);
                    
                    {
                        L_PUTC_PROLOG
                        source = get_txt(progName);
                        L_PUTC('|');
                        c = *source++;
                        do
                        {
                            if (issavedel(c))
                                L_PUTC('\\');
                            L_PUTC(c) 
                        } while ( '\0' != (c = *source++) );
                        L_PUTC_EPILOG
                    }
                    free_mstring(progName);
                }
            }

            if (l->function.lfun.context_size)
            {
                int i;
                svalue_t * val;

                {
                    L_PUTC_PROLOG
                    L_PUTC(':');
                    L_PUTC_EPILOG
                }
                /* Save the context size.
                 * It has to be saved separately because it is needed
                 * to allocated the lambda structure to the right size
                 * before the restore of the context can be done.
                 */
                {
                    svalue_t num;

                    put_number(&num, l->function.lfun.context_size);
                    save_svalue(&num, ':', MY_FALSE);
                }

                /* Save the actual context */
                {
                    L_PUTC_PROLOG
                    L_PUTC('(')
                    L_PUTC('{')
                    L_PUTC_EPILOG
                }

                for (i = l->function.lfun.context_size
                    , val = l->context
                    ; --i >= 0; )
                {
                    (void)save_svalue(val++, ',', MY_FALSE);
                }

                {
                    L_PUTC_PROLOG
                    L_PUTC('}')
                    L_PUTC(')')
                    L_PUTC_EPILOG
                }
            }
        }
        else
        {
            L_PUTC_PROLOG
            L_PUTC('0');
            L_PUTC_EPILOG
        }
        break;
      }

    case CLOSURE_IDENTIFIER:
      {
        lambda_t *l;
        char * source, c;

        if (recall_pointer(cl->u.lambda))
            break;

        l = cl->u.lambda;
        if (l->function.var_index == VANISHED_VARCLOSURE_INDEX)
        {
            rc = MY_FALSE;
            break;
        }
        if (l->ob->flags & O_DESTRUCTED
         || l->ob != current_object
           )
        {
            rc = MY_FALSE;
            break;
        }

        source = get_txt(l->ob->prog->variables[l->function.var_index].name);

        {
            L_PUTC_PROLOG

            L_PUTC('#');
            L_PUTC('v');
            L_PUTC(':');
            c = *source++;
            do L_PUTC(c) while ( '\0' != (c = *source++) );
        
            L_PUTC_EPILOG
        }
        break;
      }

    default:
        if (type < 0)
        {
            switch(type & -0x0800)
            {
#ifdef USE_PYTHON
            case CLOSURE_PYTHON_EFUN:
              {
                const char *source = closure_python_efun_to_string(type);
                L_PUTC_PROLOG
                char c;

                L_PUTC('#');
                L_PUTC('e');
                L_PUTC(':');

                c = *source++;
                do L_PUTC(c) while ( '\0' != (c = *source++) );

                L_PUTC_EPILOG
                break;
              }
#endif
            case CLOSURE_OPERATOR:
              {
                const char *s = closure_operator_to_string(type);

                if (s)
                {
                    L_PUTC_PROLOG
                    char c;

                    L_PUTC('#');
                    L_PUTC('e');
                    L_PUTC(':');

                    c = *s++;
                    do L_PUTC(c) while ( '\0' != (c = *s++) );

                    L_PUTC_EPILOG
                    break;
                }
                type += CLOSURE_EFUN - CLOSURE_OPERATOR;
              }
            /* default action for operators: FALLTHROUGH */

            case CLOSURE_EFUN:
              {
                const char *source = closure_efun_to_string(type);
                L_PUTC_PROLOG
                char c;

                L_PUTC('#');
                L_PUTC('e');
                L_PUTC(':');

                c = *source++;
                do L_PUTC(c) while ( '\0' != (c = *source++) );

                L_PUTC_EPILOG
                break;
              }

            case CLOSURE_SIMUL_EFUN:
              {
                L_PUTC_PROLOG
                char * source, c;

                source = get_txt(simul_efunp[type - CLOSURE_SIMUL_EFUN].name);

                L_PUTC('#');
                L_PUTC('s');
                L_PUTC(':');

                c = *source++;
                do L_PUTC(c) while ( '\0' != (c = *source++) );

                L_PUTC_EPILOG
                break;
              }
            }
            break;
        }
        else /* type >= 0: one of the lambda closures */
        {
            rc = MY_FALSE;
        }
        break;

    } /* switch(closure type) */

    /* We come here, we could write the closure */
    /* 'rc' at this point signifies whether the closure could be written.
     * If it couldn't, maybe write a default '0', and also adjust rc
     * to serve as function result.
     */
    if (!rc)
    {
        if (writable)
            rc = MY_FALSE;
        else
        {
            L_PUTC_PROLOG

            rc = MY_TRUE; /* Writing a default '0' counts */
            L_PUTC('0');
            L_PUTC_EPILOG
        }
    }

    return rc;
} /* save_closure() */

/*-------------------------------------------------------------------------*/
static Bool
save_lvalue (svalue_t *v, char delimiter, Bool writable)

/* Encode the lvalue <v> and write it to the write buffer.
 * <delimiter>, <writable> and the return value are as
 * in save_svalue().
 */

{
    /* Shall the fallback for plain rvalues come into play? */
    bool done = false;

    if (save_version >= SAVE_FORMAT_LVALUES)
    {
        /* The new format... */
        done = true;

        switch (v->x.lvalue_type)
        {
            default:
                fatal("(save_lvalue) Illegal lvalue %p type %d\n", v, v->x.lvalue_type);
                break;

            case LVALUE_PROTECTED:
            {
                /* Format: <1>=&(value) */
                struct protected_lvalue *l = v->u.protected_lvalue;

                if(recall_pointer(l))
                    break;

                /* Only one reference? No need for an lvalue reference here. */
                if(!lookup_pointer(ptable,l)->ref_count)
                {
                    /* Go to the fallback. */
                    done = false;
                    break;
                }

                {
                    L_PUTC_PROLOG
                    L_PUTC('&')
                    L_PUTC('(')
                    L_PUTC_EPILOG
                }

                save_svalue(&(l->val), ',', MY_FALSE);
                MY_PUTC(')')
                break;
            }

            case LVALUE_PROTECTED_CHAR:
            {
                /* We can't save the string links, so saving as a reference to a number.
                 *
                 * Format: <1>=&(123)
                 */

                struct protected_char_lvalue *l = v->u.protected_char_lvalue;

                if(recall_pointer(l))
                    break;

                /* Only one reference? No need for an lvalue reference here. */
                if(!lookup_pointer(ptable,l)->ref_count)
                {
                    /* Go to the fallback. */
                    done = false;
                    break;
                }

                {
                    L_PUTC_PROLOG
                    L_PUTC('&')
                    L_PUTC('(')
                    L_PUTC_EPILOG
                }

                save_number(*((unsigned char*)v->u.protected_char_lvalue->charp));

                {
                    L_PUTC_PROLOG
                    L_PUTC(',')
                    L_PUTC(')')
                    L_PUTC_EPILOG
                }

                break;
            }

            case LVALUE_PROTECTED_RANGE:
            {
                /* Format: <1>=&(<2>=&(vector),index1..index2-1)
                 *
                 * The <2>=&() only happens, when the lvalue still has a valid
                 * variable reference.
                 */
                struct protected_range_lvalue *r = v->u.protected_range_lvalue;
                bool var_valid, vec_referenced;

                if(recall_pointer(r))
                    break;

                /* Only one reference? No need for an lvalue reference here. */
                if(!lookup_pointer(ptable,r)->ref_count)
                {
                    /* Go to the fallback. */
                    done = false;
                    break;
                }

                /* Check whether the variable reference is still valid. */
                var_valid = (r->var != NULL
                 && ((r->vec.type == T_POINTER && r->var->val.type == T_POINTER && r->vec.u.vec == r->var->val.u.vec)
                  || (r->vec.type == T_STRING  && r->var->val.type == T_STRING  && r->vec.u.str == r->var->val.u.str)
                  || (r->vec.type == T_BYTES   && r->var->val.type == T_BYTES   && r->vec.u.str == r->var->val.u.str)));

                /* Let's have alook whether we have to disclose to full
                 * array (although we only see a part of it here).
                 */
                if (r->vec.type == T_POINTER)
                    vec_referenced = lookup_pointer(ptable, r->vec.u.vec)->ref_count != 0;
                else
                    vec_referenced = false;

                if (!vec_referenced && var_valid)
                    vec_referenced = lookup_pointer(ptable, r->var)->ref_count;

                {
                    L_PUTC_PROLOG
                    L_PUTC('&')
                    L_PUTC('(')
                    L_PUTC_EPILOG
                }

                if (!vec_referenced)
                {
                    switch (r->vec.type)
                    {
                        case T_POINTER:
                            save_array(r->vec.u.vec, r->index1, r->index2 - r->index1);
                            break;

                        case T_STRING:
                        case T_BYTES:
                            save_string(r->vec.u.str, r->index1, r->index2 - r->index1, false);
                            break;

                        default:
                             fatal("(save_lvalue) Illegal type for range lvalue '%s'.\n", typename(r->vec.type));
                             break;
                    }

                    {
                        L_PUTC_PROLOG
                        L_PUTC(',')
                        L_PUTC(')')
                        L_PUTC_EPILOG
                    }
                    break;
                }

                if (var_valid)
                {
                    /* Variable reference is still valid */
                    if (!recall_pointer(r->var))
                    {
                        if(!lookup_pointer(ptable,r->var)->ref_count)
                        {
                            save_svalue(&(r->vec), ',', MY_FALSE);
                        }
                        else
                        {
                            {
                                L_PUTC_PROLOG
                                L_PUTC('&')
                                L_PUTC('(')
                                L_PUTC_EPILOG
                            }

                            save_svalue(&(r->var->val), ')', MY_FALSE);

                            MY_PUTC(',')
                        }
                    }
                    else
                        MY_PUTC(',')
                }
                else
                    save_svalue(&(r->vec), ',', MY_FALSE);

                save_number(r->index1);

                MY_PUTC('.')
                MY_PUTC('.')

                save_number(r->index2 - 1);

                MY_PUTC(')')
                break;
            }

            case LVALUE_PROTECTED_MAPENTRY:
            {
                /* Format: <1>=&(<2>=map,key,index) */
                struct protected_mapentry_lvalue *e = v->u.protected_mapentry_lvalue;

                if(recall_pointer(e))
                    break;

                {
                    L_PUTC_PROLOG
                    L_PUTC('&')
                    L_PUTC('(')
                    L_PUTC_EPILOG
                }

                save_mapping(e->map);

                MY_PUTC(',')

                if (!save_svalue(&(e->key), ',', MY_FALSE))
                    return MY_FALSE;
                save_number(e->index);

                MY_PUTC(')')
                break;
            }
        } /* switch() */
    }


    if (!done)
    {
        /* In the old format, we save the plain value. */

        switch (v->x.lvalue_type)
        {
            default:
                fatal("(save_lvalue) Illegal lvalue %p type %d\n", v, v->x.lvalue_type);
                break;

            case LVALUE_PROTECTED:
                return save_svalue(&(v->u.protected_lvalue->val), delimiter, writable);

            case LVALUE_PROTECTED_CHAR:
                save_number(*((unsigned char*)v->u.protected_char_lvalue->charp));
                break;

            case LVALUE_PROTECTED_RANGE:
            {
                struct protected_range_lvalue *r = v->u.protected_range_lvalue;

                switch (r->vec.type)
                {
                    case T_POINTER:
                        if(recall_pointer(r))
                            break;

                        save_array(r->vec.u.vec, r->index1, r->index2 - r->index1);
                        break;

                    case T_STRING:
                    case T_BYTES:
                        save_string(r->vec.u.str, r->index1, r->index2 - r->index1, false);
                        break;

                    default:
                         fatal("(save_lvalue) Illegal type for range lvalue '%s'.\n", typename(r->vec.type));
                         break;
                }

                break;
            }

            case LVALUE_PROTECTED_MAPENTRY:
            {
                struct protected_mapentry_lvalue *e = v->u.protected_mapentry_lvalue;
                svalue_t *item = get_map_value(e->map, &(e->key));

                if (item == &const0)
                    save_number(0);
                else
                    return save_svalue(item + e->index, delimiter, writable);
                break;
            }
        } /* switch() */
    }

    MY_PUTC(delimiter)
    return MY_TRUE;

} /* save_lvalue() */

/*-------------------------------------------------------------------------*/
static Bool
save_svalue (svalue_t *v, char delimiter, Bool writable)

/* Encode the value <v> and write it to the write buffer, terminate
 * the output with <delimiter>.
 * If <writable> is false, unwritable svalues like objects are written
 * as '0'. If <writable> is true, unwritable svalues are not written at all.
 *
 * Return is true if something was written, and false otherwise.
 */

{
    Bool rc = MY_TRUE;

    assert_stack_gap();

    switch(v->type)
    {
    case T_STRING:
    case T_BYTES:
        save_string(v->u.str, -1, -1, false);
        break;

    case T_QUOTED_ARRAY:
      {
        L_PUTC_PROLOG
        char * source, c;

        source = number_buffer;
        (void)snprintf(source, sizeof(number_buffer), "#%"PRIdPHINT":", v->x.quotes);
        c = *source++;
        do L_PUTC(c) while ( '\0' != (c = *source++) );
        L_PUTC_EPILOG
        /* FALLTHROUGH to T_POINTER */
      }

    case T_POINTER:
        save_array(v->u.vec, -1, -1);
        break;

    case T_STRUCT:
        save_struct(v->u.strct);
        break;

    case T_NUMBER:
        save_number(v->u.number);
        break;

    case T_FLOAT:
      {
        /* To minimize rounding losses, the floats are written
         * in two forms: the nominal value, and the internal
         * representation.
         */

        L_PUTC_PROLOG
        char *source, c;
        
        double dtmpval = READ_DOUBLE(v);
        // infinite (NaN or INF) should never happen in an svalue. If they happen
        // and are written to the savefile, it is invalid and can't be restored.
        assert(isfinite(dtmpval));

        source = number_buffer;
        
        if (save_version >= SAVE_FORMAT_FLOATS)
        {
            (void)snprintf(source, sizeof(number_buffer),
                          "%la", dtmpval);
        }
        else
        {
            int32_t mantissa;
            int exponent;
            mantissa = SPLIT_DOUBLE(dtmpval, &exponent);
            // Casting to uint16_t is done because PRIx16 on some systems is %x, not
            // %hx, although int16_t is a short. That causes conversion of the 
            // exponent to signed integer and in case of negative exponents wrong 
            // data and too many chars will be written (ffffffff instead of ffff).
            // And because on the same system SCNx16 is "%hx", different values 
            // would be restored than written.
            // Casting to unsigned keeps the bit pattern.
            (void)snprintf(source, sizeof(number_buffer)
                         ,  "%.12e=%"PRIx16":%"PRIx32
                         , dtmpval, (uint16_t)exponent
                         , (int32_t)mantissa);
        }
        c = *source++;
        do L_PUTC(c) while ( '\0' != (c = *source++) );
        L_PUTC(delimiter);
        L_PUTC_EPILOG
        return rc;
      }

    case T_MAPPING:
        save_mapping(v->u.map);
        break;

    case T_LVALUE:
        return save_lvalue(v, delimiter, writable);

    case T_SYMBOL:
      {
        L_PUTC_PROLOG
        char * source, c;

        source = number_buffer;
        (void)snprintf(source, sizeof(number_buffer), "#%"PRIdPHINT":", v->x.quotes);
        c = *source++;
        do L_PUTC(c) while ( '\0' != (c = *source++) );
        L_PUTC_EPILOG
        save_string(v->u.str, -1, -1, false);
        break;
      }

    case T_CLOSURE:
        if (save_version >= SAVE_FORMAT_CLOSURES)
        {
            rc = save_closure(v, writable);
            break;
        }
        /* else: FALLTHROUGH */

    default:
      {
        /* Objects can't be saved */
        if (writable)
            rc = MY_FALSE;
        else
        {
            L_PUTC_PROLOG
            L_PUTC('0');
            L_PUTC(delimiter);
            L_PUTC_EPILOG
        }
        return rc;
      }
    }

    if (rc)
        MY_PUTC(delimiter);

    return rc;
}  /* save_svalue() */

/*-------------------------------------------------------------------------*/
static void
register_array (vector_t *vec)

/* Register the array <vec> in the pointer table. If it was not
 * in there, also register all array/mapping values.
 */

{
    svalue_t *v;
    p_int i;

    if (NULL == register_pointer(ptable, vec))
        return;

    v = vec->item;
    for (i = VEC_SIZE(vec); --i >= 0; v++)
    {
        register_svalue(v);
    }
} /* register_array() */

/*-------------------------------------------------------------------------*/
static void
register_struct (struct_t *st)

/* Register the struct <st> in the pointer table. If it was not
 * in there, also register all struct/array/mapping values.
 */

{
    svalue_t *v;
    long i;

    if (NULL == register_pointer(ptable, st))
        return;

    (void)register_pointer(ptable, struct_unique_name(st));

    v = st->member;
    for (i = (long)struct_size(st); --i >= 0; v++)
    {
        register_svalue(v);
    }
} /* register_struct() */

/*-------------------------------------------------------------------------*/
static void
register_mapping_filter (svalue_t *key, svalue_t *data, void *extra)

/* Callback to register one mapping entry of (p_int)<extra> values.
 */

{
    p_int i;

    register_svalue(key);

    for (i = (p_int)extra; --i >= 0; data++)
    {
        register_svalue(data);
    }
} /* register_mapping_filter() */

/*-------------------------------------------------------------------------*/
static void
register_mapping (mapping_t *map)

/* Register the mapping <map> in the pointer table. If it was not
 * in there, also register all array/mapping values.
 */

{
    if (NULL == register_pointer(ptable, map))
        return;
    walk_mapping(map, register_mapping_filter, (void *)map->num_values);
} /* register_mapping() */

/*-------------------------------------------------------------------------*/
static void
register_closure (svalue_t *cl)

/* Register closure <cl> in the pointer table. If it was not
 * in there, also register all associated svalues (if any).
 */

{
    int type;

    switch(type = cl->x.closure_type)
    {
    case CLOSURE_LFUN:
    case CLOSURE_IDENTIFIER:
        if (NULL == register_pointer(ptable, cl->u.lambda))
            return;
        break;

    default:
        /* Operator- or an unsaveable lambda closure */
        return;
    }

    if (type == CLOSURE_LFUN
     && cl->u.lambda->function.lfun.ob == current_object
     && cl->u.lambda->ob == current_object
     && cl->u.lambda->function.lfun.context_size
       )
    {
        lambda_t  *l;
        svalue_t *val;
        long i;

        l = cl->u.lambda;
        for (i = l->function.lfun.context_size
            , val = l->context
            ; --i >= 0; )
        {
            register_svalue(val++);
        }
    }
} /* register_closure() */

/*-------------------------------------------------------------------------*/
static void
register_svalue (svalue_t *svp)

/* If <svp> is a struct, array, or mapping, register it in the pointer
 * table, and also register all sub structures.
 */

{
    switch (svp->type)
    {
      case T_STRING:
      case T_BYTES:
        (void)register_pointer(ptable, svp->u.str);
        break;

      case T_POINTER:
      case T_QUOTED_ARRAY:
        register_array(svp->u.vec);
        break;

      case T_STRUCT:
        register_struct(svp->u.strct);
        break;

      case T_MAPPING:
        register_mapping(svp->u.map);
        break;

      case T_CLOSURE:
        register_closure(svp);
        break;

      case T_LVALUE:
        if (save_version < SAVE_FORMAT_LVALUES)
        {
            /* For older format, just do the recursion.
             * And treat ranges as normal arrays. */
            if (svp->x.lvalue_type == LVALUE_PROTECTED)
                register_svalue(&(svp->u.protected_lvalue->val));
            else if (svp->x.lvalue_type == LVALUE_PROTECTED_RANGE)
                register_pointer(ptable, svp->u.protected_range_lvalue);
            break;
        }

        switch (svp->x.lvalue_type)
        {
            default:
                fatal("(register_svalue) Illegal lvalue %p type %d\n", svp, svp->x.lvalue_type);
                /* NOTREACHED */
                break;

            case LVALUE_PROTECTED:
                if(register_pointer(ptable, svp->u.protected_lvalue) != NULL)
                    register_svalue(&(svp->u.protected_lvalue->val));
                break;

            case LVALUE_PROTECTED_CHAR:
                register_pointer(ptable, svp->u.protected_char_lvalue);
                break;

            case LVALUE_PROTECTED_RANGE:
            {
                struct protected_range_lvalue *r = svp->u.protected_range_lvalue;

                if(register_pointer(ptable, r) != NULL)
                {

                    if (r->var != NULL
                     && ((r->vec.type == T_POINTER && r->var->val.type == T_POINTER && r->vec.u.vec == r->var->val.u.vec)
                      || (r->vec.type == T_STRING  && r->var->val.type == T_STRING  && r->vec.u.str == r->var->val.u.str)
                      || (r->vec.type == T_BYTES   && r->var->val.type == T_BYTES   && r->vec.u.str == r->var->val.u.str)))
                    {
                        if (register_pointer(ptable, r->var) != NULL)
                            register_svalue(&(r->var->val));
                    }
                    else
                        register_svalue(&(r->vec));
                }
                break;
            }

            case LVALUE_PROTECTED_MAPENTRY:
            {
                struct protected_mapentry_lvalue *e = svp->u.protected_mapentry_lvalue;

                if (register_pointer(ptable, e) != NULL)
                {
                    register_mapping(e->map);
                    register_svalue(&(e->key));
                }
                break;
            }
        } /* switch (v->x.lvalue_type) */
    } /* switch() */
} /* register_svalue() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_save_object (svalue_t *sp, int numarg)

/* EFUN save_object()
 *
 *   int    save_object (string file, [int version])
 *   string save_object ([int version])
 *
 * Save the variables of the current object to the file <file> (the suffix
 * ".o" will be appended. Returns 0 if the save file could be created,
 * and non-zero otherwise (file could not be written, or current object
 * is destructed).
 *
 * The <file>.o will not be written immediately: first the savefile will
 * be created as <file>.o.tmp, which is after completion renamed to <file>.o.
 *
 * The validity of the filename is checked with a call to check_valid_path().
 *
 * In the second form, the a string with all variables and values is
 * returned directly, or 0 if an error occurs. This string can be used
 * with restore_object() to restore the variable values.
 *
 * In both forms, the optional argument <version> determines the format
 * of the save file. A value of '-1' creates the format native to the
 * driver. Currently the formats 0 and 1 are supported.
 *
 * TODO: "save_object()" looks nice, but maybe call that "save_variables()"?
 */

{
    static char save_object_header[]
      = { '#', SAVE_OBJECT_VERSION, ':', SAVE_OBJECT_HOST, '\n'
        };
      /* The version string to write
       */

    object_t *ob;
      /* The object to save - just a local copy of current_object.
       */
    char *file;
      /* The filename read from the stack, NULL if not saving
       * to a file.
       */
    char *name;
      /* Buffer for the final and the temporary filename.
       * name itself points to the final filename.
       */
    char *tmp_name;
      /* Pointer to the temporary filename in the buffer of name.
       */
    char save_buffer[SAVE_OBJECT_BUFSIZE];
      /* The write buffer
       */
    long len;
    int i;
    int f;
    svalue_t *v;
    variable_t *names;

    f = -1;
    file = NULL;
    name = NULL;
    tmp_name = NULL;
    save_version = CURRENT_VERSION;

    /* Test the arguments */
    switch (numarg)
    {
    case 0:
        strbuf_zero(&save_string_buffer);
        break;

    case 1:
        if (sp->type == T_STRING)
        {
            file = get_txt(sp->u.str);
        }
        else if (sp->type == T_NUMBER)
        {
            if (sp->u.number < -1 || sp->u.number > CURRENT_VERSION)
            {
                errorf("Illegal value for arg 1 to save_object(): %"PRIdPINT", "
                      "expected -1..%d\n"
                     , sp->u.number, CURRENT_VERSION
                     );
                /* NOTREACHED */
                return sp;
            }

            strbuf_zero(&save_string_buffer);
            save_version = sp->u.number >= 0 ? sp->u.number
                                             : CURRENT_VERSION;
        }
        else
        {
            vefun_gen_arg_error(1, sp->type, sp);
            /* NOTREACHED */
            return sp;
        }
        break;

    case 2:
        if (sp[-1].type != T_STRING)
            vefun_arg_error(1, T_STRING, sp[-1].type, sp);
        if (sp->type != T_NUMBER)
            vefun_arg_error(2, T_NUMBER, sp->type, sp);

        file = get_txt(sp[-1].u.str);

        if (sp->u.number < -1 || sp->u.number > CURRENT_VERSION)
        {
            errorf("Illegal value for arg 1 to save_object(): %"PRIdPINT", "
                  "expected -1..%d\n"
                 , sp->u.number, CURRENT_VERSION
                 );
            /* NOTREACHED */
            return sp;
        }

        save_version = sp->u.number >= 0 ? sp->u.number
                                         : CURRENT_VERSION;

        /* The main code wants sp == filename (T_NUMBER svalues need no free.)
         */
        sp--;
        numarg--;
        break;

    default:
        fatal("Too many arguments to save_object(): %d, expected 0..2\n"
             , numarg);
    } /* switch(numarg) */

    save_object_header[1] = '0' + save_version;

    /* No need in saving destructed objects */

    ob = current_object;
    if (ob->flags & O_DESTRUCTED)
    {
        if (numarg)
            sp = pop_n_elems(numarg, sp);
        sp++;
        put_number(sp, 0);
        return sp;
    }

    /* If saving to a file, get the proper name and open it
     * The code assumes that sp is the filename argument.
     */
    if (file)
    {
        string_t *sfile;
        char *native;

        /* Get a valid filename */

        sfile = check_valid_path(sp->u.str, ob, STR_SAVE_OBJECT, MY_TRUE);
        if (sfile == NULL)
        {
            errorf("Illegal use of save_object('%s')\n", get_txt(sp->u.str));
            /* NOTREACHED */
            return sp;
        }

        /* Remove any trailing '.c' */
        {
            string_t *tmp = del_dotc(sfile);
            if (!tmp)
                outofmem(mstrsize(sfile), "filename");
            free_mstring(sfile);
            sfile = tmp;
        }


        /* Create the final and the temporary filename */
        native = convert_path_str_to_native_or_throw(sfile);
        len = (long)strlen(native);
        inter_sp = sp;
        name = xalloc_with_error_handler(len + (sizeof save_file_suffix) +
                                        len + (sizeof save_file_suffix) + 4);
        if (!name)
        {
            errorf("Out of memory (%ld bytes) in save_object('%s')\n", 
                   2*len+2*sizeof(save_file_suffix)+4, get_txt(sp->u.str));
            /* NOTREACHED */
            return sp;
        }
        sp = inter_sp;

        tmp_name = name + len + sizeof save_file_suffix;

        memcpy(name, native, len);
        memcpy(name + len, save_file_suffix, sizeof save_file_suffix);

        memcpy(tmp_name, name, len + sizeof save_file_suffix);
        memcpy(tmp_name + len + sizeof save_file_suffix - 1, ".tmp", 5);

        /* Open the file */

        /* Always write savefiles in 'binary mode'. (O_BINARY is 0 on all platforms
         * except of Cygwin and therefore ignored. Cygwin may need it, if the
         * volume with the mudlib is mounted in textmode. */
        f = ixopen3(tmp_name, O_CREAT|O_TRUNC|O_WRONLY|O_BINARY, 0640);

        if (f < 0) {
            char * emsg, * buf;

            emsg = strerror(errno);
            buf = alloca(strlen(emsg)+1);
            if (buf)
            {
                strcpy(buf, emsg);
                errorf("Could not open %s for a save: %s.\n", tmp_name, buf);
            }
            else
            {
                perror("save object");
                errorf("Could not open %s for a save: errno %d.\n"
                     , tmp_name, errno);
            }
            /* NOTREACHED */
            return sp;
        }
        FCOUNT_SAVE(tmp_name);
    } /* if (file) */

    /* Publish where we are going to save the data (-1 means using
     * the string buffer.
     */
    save_object_descriptor = f;

    /* First pass through the variables to identify arrays/mappings
     * that are used more than once.
     */

    if (ptable)
    {
        debug_message("%s (save_object) Freeing lost pointertable\n", time_stamp());
        free_pointer_table(ptable);
    }

    ptable = new_pointer_table();
    if (!ptable)
    {
        if (file)
        {
            close(f);
            unlink(tmp_name);
        }
        errorf("(save_object) Out of memory for pointer table.\n");
        /* NOTREACHED */
        return sp;
    }

    v = ob->variables;
    names = ob->prog->variables;
    for (i = ob->prog->num_variables; --i >= 0; v++, names++)
    {
        if (names->type.t_flags & TYPE_MOD_STATIC)
            continue;

        register_svalue(v);
    }

    /* Prepare the actual save */

    failed = MY_FALSE;
    current_sv_id_number = 0;
    bytes_written = 0;
    save_object_bufstart = save_buffer;
    memcpy(save_buffer, save_object_header, sizeof(save_object_header));
    buf_left = SAVE_OBJECT_BUFSIZE - sizeof(save_object_header);
    buf_pnt = save_buffer + sizeof(save_object_header);

    /* Second pass through the variables, actually saving them */

    v = ob->variables;
    names = ob->prog->variables;
    for (i = ob->prog->num_variables; --i >= 0; v++, names++)
    {
        if (names->type.t_flags & TYPE_MOD_STATIC)
            continue;

        /* Write the variable name */
        {
            char *var_name, c;
            L_PUTC_PROLOG

            var_name = get_txt(names->name);
            c = *var_name++;
            do {
                L_PUTC(c)
            } while ( '\0' != (c = *var_name++) );
            L_PUTC(' ')
            L_PUTC_EPILOG
        }
        save_svalue(v, '\n', MY_FALSE);
    }

    free_pointer_table(ptable);
    ptable = NULL;

    if (file)
    {
        /* Finish up the file */

        len =  write( save_object_descriptor
                    , save_object_bufstart
                    , (size_t)(SAVE_OBJECT_BUFSIZE-buf_left));
        if (len != SAVE_OBJECT_BUFSIZE-buf_left )
            failed = MY_TRUE;


        /* On failure, delete the temporary file and return */

        if (failed)
        {
            close(f);
            unlink(tmp_name);
            add_message("Failed to save to file '%s'. Disk could be full.\n", file);
            /* free the error handler and the arguments (numarg + 1  from sp).
             */
            sp = pop_n_elems(numarg + 1, sp);
            sp++;
            put_number(sp, 1);
            return sp;
        }

        /* Delete any existing savefile, then rename the temporary
         * file to the real name.
         */

        i = 0; /* Result from efun */

        unlink(name);
        if (link(tmp_name, name) == -1)
        {
            perror(name);
            printf("%s Failed to link %s to %s\n"
                  , time_stamp(), tmp_name, name);
            add_message("Failed to save object !\n");
            i = 1;
        }
        close(f);
        unlink(tmp_name);

        /* free the error handler and the arguments (numarg + 1  from sp) and
         * push result on the stack.
         */
        sp = pop_n_elems(numarg + 1, sp);
        sp++;
        put_number(sp, i);
    } /* if (file) */
    else
    {
        /* Finish up the operation. Note that there propably is some
         * data pending in the save_buffer.
         */

        /* free the arguments (numarg from sp).
         */
        sp = pop_n_elems(numarg, sp);
      
        sp++; /* for the result */
        if (failed)
            put_number(sp, 0); /* Shouldn't happen */
        else if (buf_left != SAVE_OBJECT_BUFSIZE)
        {
            /* Data pending in the save_buffer. */
            if (!bytes_written)
            {
                /* Less than SAVE_OBJECT_BUFSIZE bytes generated
                 * we bypass the strbuf for speed.
                 */
                len = SAVE_OBJECT_BUFSIZE-buf_left;
                save_object_bufstart[len] = '\0';
                put_c_string(sp, save_object_bufstart);
                strbuf_free(&save_string_buffer);
            }
            else
            {
                /* More than SAVE_OBJECT_BUFSIZE of data generated
                 * Fill up the stringbuffer and create the result.
                 */
                strbuf_addn(&save_string_buffer, save_object_bufstart
                       , SAVE_OBJECT_BUFSIZE-buf_left);
                strbuf_store(&save_string_buffer, sp);
            }
        }
        else
            /* The save_buffer[] is empty, what means
             * that at least one buffer full was written into
             * the strbuf.
             */
            strbuf_store(&save_string_buffer, sp);
    } /* if (file or not file) */

    return sp;
} /* v_save_object() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_save_value (svalue_t *sp, int numarg)

/* EFUN save_value()
 *
 *   string save_value(mixed value, [int version])
 *
 * Encode the <value> into a string suitable for restoration with
 * restore_value() and return it.
 *
 * The created string consists of two lines, each terminated with a newline
 * character: the first line describes the format used to save the value in
 * the '#x:y' notation; the second line is the representation of the value
 * itself.
 *
 * The optional argument <version> determines the format
 * of the save file. A value of '-1' creates the format native to the
 * driver. Currently the formats 0 and 1 are supported.
 */

{
    static char save_value_header[]
      = { '#', SAVE_OBJECT_VERSION, ':', SAVE_OBJECT_HOST, '\n'
        };
      /* The version string to write
       */

    char save_buffer[SAVE_OBJECT_BUFSIZE];
      /* The write buffer.
       */

    /* Set up the globals */
    if (ptable)
    {
        debug_message("%s (save_value) Freeing lost pointer table.\n", time_stamp());
        free_pointer_table(ptable);
    }
    ptable = new_pointer_table();
    if (!ptable)
    {
        errorf("(save_value) Out of memory for pointer table.\n");
        return sp; /* flow control hint */
    }

    strbuf_zero(&save_string_buffer);
    save_object_descriptor = -1;
    save_version = CURRENT_VERSION;

    /* Evaluate the arguments */
    switch (numarg)
    {
    case 1:
        /* Ok */
        break;

    case 2:
        if (sp->type == T_NUMBER)
        {
            if (sp->u.number < -1 || sp->u.number > CURRENT_VERSION)
            {
                errorf("Illegal value for arg 1 to save_object(): %"PRIdPINT", "
                      "expected -1..%d\n"
                     , sp->u.number, CURRENT_VERSION
                     );
                /* NOTREACHED */
                return sp;
            }

            save_version = sp->u.number >= 0 ? sp->u.number
                                                    : CURRENT_VERSION;

            sp--;
        }
        else
        {
            vefun_gen_arg_error(2, sp->type, sp);
            /* NOTREACHED */
            return sp;
        }
        break;

    default:
        fatal("Illegal number of arguments to save_value(): %d, expected 1..2\n"
             , numarg);
    } /* switch(numarg) */

    save_value_header[1] = '0' + save_version;

    /* First look at the value for arrays and mappings
     */
    register_svalue(sp);

    /* Prepare the actual save */

    failed = MY_FALSE;
    current_sv_id_number = 0;
    bytes_written = 0;
    save_object_bufstart = save_buffer;
    memcpy(save_buffer, save_value_header, sizeof(save_value_header));
    buf_left = SAVE_OBJECT_BUFSIZE - sizeof(save_value_header);
    buf_pnt = save_buffer + sizeof(save_value_header);

    /* Save the value */
    save_svalue(sp, '\n', MY_FALSE);

    /* Finish up the operation. Note that there propably is some
     * data pending in the save_buffer.
     */

    free_svalue(sp);  /* No longer needed */

    if (failed)
        put_number(sp, 0); /* Shouldn't happen */
    else if (buf_left != SAVE_OBJECT_BUFSIZE)
    {
        /* Data pending in the save_buffer. */
        if (!bytes_written)
        {
            /* Less than SAVE_OBJECT_BUFSIZE bytes generated
             * we bypass the strbuf for speed.
             */
            size_t len = SAVE_OBJECT_BUFSIZE-buf_left;

            save_object_bufstart[len] = '\0';
            put_c_string(sp, save_object_bufstart);
            strbuf_free(&save_string_buffer);
        }
        else
        {
            /* More than SAVE_OBJECT_BUFSIZE of data generated
             * Fill up the stringbuffer and create the result.
             */
            strbuf_addn(&save_string_buffer, save_object_bufstart
                       , SAVE_OBJECT_BUFSIZE-buf_left);
            strbuf_store(&save_string_buffer, sp);
        }
    }
    else
        /* The save_buffer[] is empty, what means
         * that at least one buffer full was written into
         * the strbuf.
         */
        strbuf_store(&save_string_buffer, sp);

    /* Clean up */
    free_pointer_table(ptable);
    ptable = NULL;

    return sp;
} /* v_save_value() */

/*-------------------------------------------------------------------------*/
/* Structure used by restore_mapping() and restore_map_size()
 * to exchange data.
 */

struct rms_parameters
{
    char *str;       /* Current position in the input stream */
    int num_values;  /* Recognized number of values per key */
};

static int restore_map_size(struct rms_parameters *parameters);
static int restore_size(char **str);

/*-------------------------------------------------------------------------*/
static bool
skip_element (char **str)

/* In the string starting at *str search for the end of
 * the svalue. Return true on success, false otherwise.
 */

{
    char *pt = *str;

    if (pt[0] == '#' && pt[1] >= '0' && pt[1] <= '9')
    {
        /* Symbol or quoted array.
         * Just skip to header and parse it
         * as a normal string or array.
         */
        pt = strchr(pt+2,':');
        if (!pt)
            return false;
        pt++;
    }

    if (*pt == '<')
    {
        /* A shared array/mapping/lvalue */
        pt = strchr(pt, '>');
        if (!pt)
            return false;

        if (pt[1] == '=')
            pt += 2;
        else
        {
            *str = pt + 1;
            return true;
        }
    }

    switch(*pt)
    {
        case 'b':   /* byte sequence */
            pt++;
            if (*pt != '\"')
                return false;
            /* FALLTHROUGH. */
        case '\"':  /* string */
        {
            int backslashes;

            do
            {
                pt = strchr(&pt[1],'\"');
                if (!pt)
                    return false;
               /* the quote is escaped if and only
                * if the number of slashes is odd.
                */
               for (backslashes = -1; pt[backslashes] == '\\'; backslashes--)
                   NOOP;
            } while ( !(backslashes & 1) ) ;

            *str = pt+1;
            return true;
        }

        case '(':  /* array, struct or mapping */
        {
            /* Lazy way of doing it, a bit inefficient */
            struct rms_parameters tmp_par;
            int tsiz;

            tmp_par.str = pt + 2;
            if (pt[1] == '{'
             || pt[1] == '<'
               )
                tsiz = restore_size(&tmp_par.str);
            else if (pt[1] == '[')
                tsiz = restore_map_size(&tmp_par);
            else
                return false;

            if (tsiz < 0)
                return false;

            *str = tmp_par.str;
            return true;
        }

        case '#': /* A closure: skip the header and restart this check
                   * again from the data part.
                   */
        {
            if (pt[2] != ':')
                return false;

            switch (pt[1])
            {
                case 'e':
                {
                    /* efun or operator closure. */

                    /* Try parsing the closure as operator closure.
                     * If it is, restart the scanning from the end
                     * of the string (which is likely to contain magic
                     * characters like '<' or '-').
                     */
                    const char* end;
                    if (symbol_operator(pt+3, &end) >= 0)
                    {
                        *str = (char *)end;
                        return true;
                    }
                    /* else fallthrough */
                }

                case 's':    /* simul-efun */
                case 'l':    /* lfun       */
                case 'v':    /* variable   */
                    /* Skip name. */
                    pt += 3;
                    while (isalunum(*pt) || *pt == '#')
                        pt++;

                    *str = pt;
                    return true;

                case 'c':    /* context closure */
                    /* Skip the name */
                    pt = strchr(pt, ':');
                    /* Skip the number of variables */
                    pt = strchr(pt+1, ':');
                    if (pt[1] != '(' || pt[2] != '{')
                        return false;

                    /* And skip the array */
                    *str = pt+3;
                    return restore_size(str) >= 0;

                default:
                    return false;
            }
        }

        case '&': /* An lvalue reference. */
            if (pt[1] != '(')
                return false;

            pt += 2;
            if (!skip_element(&pt))
                return false;

            /* Skip the indices. */
            pt = strchr(pt, ')');
            if (!pt)
                return false;

            *str = pt+1;
            return true;

        case '-':  /* A number */
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
        {
            /* Skip any hexadecimal digits and one of the following chars:
             * "+-.:=epx", but the ':' we allow only after a '='.
             */
            const char* addchars = "+-.=";

            pt++;
            while (isalnum(*pt) || strchr(addchars, *pt))
            {
                if(*pt == '=')
                    /* old-style, after that only hexadecimal digits and ':' */
                    addchars = ":";
                else if(*pt == ':')
                    /* only one ':' allowed. */
                    addchars = "";

                pt++;
            }

            *str = pt;
            return true;
        }

        default:
            return false;
    }
} /* skip_element() */

/*-------------------------------------------------------------------------*/
static int
restore_map_size (struct rms_parameters *parameters)

/* Determine the size of a mapping to be restored.
 * The mapping text starts at parameters->str, which points after the
 * initial '(['.
 *
 * The recognized width of the mapping is returned in parameters->num_values,
 * parameters->str is set to the character after the mapping text, and
 * the size (number of entries) is returned directly.
 * If the mapping text is ill formed, the function returns -1.
 *
 * The function calls itself and restore_size() recursively
 * for embedded arrays and mappings.
 *
 * TODO: this function assumes that num_values and num_entries of mappings
 * TODO::are 'int'. Should be changed to p_int.
 */

{
    char *pt;                    /* Read pointer */
    int siz;                     /* Number of entries (so far) */
    int num_values = -1;         /* Last recognized width of the mapping */
    int current_num_values = 0;  /* Width of current entry */

    pt = parameters->str;
    siz = 0;

    /* Saveguard */
    if (!pt)
      return -1;

    /* The parse loop */

    while (MY_TRUE)
    {
        /* Parse the next element */
        switch (*pt)
        {

        case ']':  /* End of mapping */
          {
            if (pt[1] != ')')
                return -1;
            parameters->str = &pt[2];
            parameters->num_values = siz ? num_values : 1;
            return siz;
          }

        case ':':  /* Special case: ([:<width>]) */
          {
            if (siz || current_num_values)
                return -1;
            pt++;
            num_values = atoi(pt);
            pt = strchr(pt,']');
            if (!pt || pt[1] != ')' || num_values < 0)
                return -1;
            parameters->str = &pt[2];
            parameters->num_values = num_values;
            return siz;
          }

        default:
            if (!skip_element(&pt))
                return -1;
            break;
        } /* switch() */

        /* At this point, pt points just after the preceeding
         * non-terminal
         */
        switch (*pt)
        {
          case ':':
            /* current_num_values is 0 on the first encounter */

            if (current_num_values)
                return -1;

            /* FALL THROUGH */

          case ';':
            current_num_values++;
            break;

          case ',':  /* End of entry */
            siz++;
            if (current_num_values != num_values)
            {
                if (num_values >= 0)
                    return -1;
                num_values = current_num_values;
            }
            current_num_values = 0;
            break;

          default:
            return -1;
        }
        pt++;
    }

    /* NOTREACHED */

    return -1;
} /* restore_map_size() */

/*-------------------------------------------------------------------------*/
INLINE static void
free_shared_restored_values (void)

/* Deref all svalues in shared_restored_values[] up to
 * current_shared_restored, then deallocate the array itself.
 */

{
    struct restore_context_s *ctx = restore_ctx;
    
    while (ctx->current_shared_restored > 0)
        free_svalue(&(ctx->shared_restored_values[--ctx->current_shared_restored]));
    xfree(ctx->shared_restored_values);
    ctx->shared_restored_values = NULL;
}

/*-------------------------------------------------------------------------*/
INLINE static Bool
restore_mapping (svalue_t *svp, char **str)

/* Restore a mapping from the text starting at *<str> (which points
 * just after the leading '([') and store it into *<svp>.
 * Return TRUE if the restore was successful, FALSE else (*<svp> is
 * set to const0 in that case).
 * On a successful return, *<str> is set to point after the mapping
 * restored.
 *
 * TODO: this function assumes that num_values and num_entries of mappings
 * TODO::are 'int'. Should be changed to p_int.
 */

{
    mapping_t *z;
    svalue_t key, *data;
    int i;
    struct rms_parameters tmp_par;
    int siz;

    /* Determine the size and width of the mapping */

    tmp_par.str = *str;
    siz = restore_map_size(&tmp_par);
    if (siz < 0)
    {
        *svp = const0;
        return MY_FALSE;
    }

    if (max_mapping_size && siz * (1+tmp_par.num_values) > (p_int)max_mapping_size)
    {
        *svp = const0;
        errorf("Illegal mapping size: %ld elements (%d x %d).\n"
             , (long)siz * (1+tmp_par.num_values)
             , siz
             , 1+tmp_par.num_values );
        return MY_FALSE;
    }

    /* Allocate the mapping */
    z = allocate_mapping(siz, tmp_par.num_values);

    if (!z)
    {
        *svp = const0;
        errorf("(restore) Out of memory: mapping[%d, %d]\n"
             , siz, tmp_par.num_values);
        return MY_FALSE;
    }

    svp->type = T_MAPPING;
    svp->u.map = z;

    /* Loop through size and width, restoring the values */
    while (--siz >= 0)
    {
        i = tmp_par.num_values;
        key.type = T_NUMBER;
        if (!restore_svalue(&key, str, (char)(i ? ':' : ',') ))
        {
            free_svalue(&key);
            return MY_FALSE;
        }
        data = get_map_lvalue_unchecked(z, &key);
        if (!data)
        {
            outofmemory("restored mapping entry");
            /* NOTREACHED */
            return MY_FALSE;
        }
        free_svalue(&key);
        while (--i >= 0) {
            if (data->type != T_INVALID && data->type != T_NUMBER)
            {
                /* Duplicate key: this shouldn't happen - but it did */
                free_svalue(data);
            }
            if (!restore_svalue(data++, str, (char)(i ? ';' : ',') ))
                return MY_FALSE;
        }
    }
    *str = tmp_par.str;
    return MY_TRUE;
} /* restore_mapping() */

/*-------------------------------------------------------------------------*/
static int
restore_size (char **str)

/* Determine the size of an array/struct to be restored.
 * The array/struct text starts at *str, which points after the initial '({'
 * resp. '(<'.
 *
 * The recognized size of the array/struct is returned, or -1 if the text
 * is ill formed. *<str> is set to point to the character after the
 * array/struct text.
 *
 * The function calls itself and restore_map_size() recursively
 * for embedded arrays and mappings.
 *
 * TODO: this function assumes that the size of arrays and mappings is 
 * TODO::< INT_MAX. Should be changed to p_int.
 */

{
    char *pt;
    int siz;

    pt = *str;
    siz = 0;

    while (pt && *pt)
    {
        switch(*pt)
        {
        case '}':  /* End of array */
        case '>':  /* End of struct */
          {
            if (pt[1] != ')')
                return -1;
            *str = &pt[2];
            return siz;
          }

        default:
            if (!skip_element(&pt))
                return -1;

            if (*pt != ',')
                return -1;

            siz++;
            pt++;
            break;
        } /* switch() */
    } /* while() */

    return -1;
} /* restore_size() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
restore_array (svalue_t *svp, char **str)

/* Restore an array from the text starting at *<str> (which points
 * just after the leading '({' and store it into *<svp>.
 * Return TRUE if the restore was successful, FALSE else (*<svp> is
 * set to const0 in that case).
 * On a successful return, *<str> is set to point after the array text
 * restored.
 *
 * TODO: this function assumes that the size of arrays is < INT_MAX. Should 
 * TODO::be changed to p_int.
 */

{
    vector_t *v;
    char *pt, *end;
    int siz;

    end = *str;

    /* Get the size of the array */

    siz = restore_size(&end);
    if (siz < 0)
    {
        *svp = const0;
        return MY_FALSE;
    }

    if (max_array_size && siz > (p_int)max_array_size)
    {
        *svp = const0;
        errorf("Illegal array size: %ld.\n", (long int)siz);
        return MY_FALSE;
    }

    /* Allocate the array */

    *svp = const0; /* in case allocate_array throws an error */
    v = allocate_array(siz);
    put_array(svp, v);

    /* Restore the values */

    for ( svp = v->item; --siz >= 0; svp++)
    {
        if (!restore_svalue(svp, str, ','))
        {
            return MY_FALSE;
        }
    }

    /* Check for the trailing '})' */

    pt = *str;
    if (*pt++ != '}' || *pt++ != ')' ) {
        return MY_FALSE;
    }


    *str = pt;
    return MY_TRUE;
} /* restore_array() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
restore_struct (svalue_t *svp, char **str)

/* Restore a struct from the text starting at *<str> (which points
 * just after the leading '(<') and store it into *<svp>.
 * Return TRUE if the restore was successful, FALSE else (*<svp> is
 * set to const0 in that case).
 * On a successful return, *<str> is set to point after the struct text
 * restored.
 */

{
    svalue_t name;
    struct_t *st;
    struct_type_t *stt;
    char *pt, *end;
    int siz;
    Bool rtt_checks; // are RTT checks enabled for this objects program?
    bool anonymous = false;
    long memberpos;

    end = *str;

    *svp = const0; /* In case of errors */

    /* Get the size of the array */

    siz = restore_size(&end);
    if (siz < 1)
    {
        return MY_FALSE;
    }
    
    /* Get the name of the struct, and from it the type pointer */
    {
        string_t * structname;
        string_t * prog_name;
        long pos;

        if (!restore_svalue(&name, str, ','))
            return MY_FALSE;
        if (name.type != T_STRING)
        {
            free_svalue(&name);
            return MY_FALSE;
        }
        siz--;

        /* The struct name can be given in the following variants:
         *
         * - "structname"
         * - "structname progname #id"
         * - "structname progname #id member1,member2,..."
         */
        pos = mstrchr(name.u.str, ' ');
        if (pos < 0)
        {
            structname = ref_mstring(name.u.str);
            prog_name = NULL;
            memberpos = -1;
        }
        else
        {
            long pos2;

            pos2 = mstrchr(name.u.str, '#');
            if (pos2 < 0)
            {
                free_mstring(name.u.str);
                return MY_FALSE;
            }
            structname = mstr_extract(name.u.str, 0, pos-1);
            prog_name = mstr_extract(name.u.str, pos+1, pos2-2);

            if (mstreq(prog_name, STR_ANONYMOUS))
            {
                anonymous = true;

                free_mstring(prog_name);
                prog_name = NULL;
            }
            else if (!compat_mode)
            {
               string_t * tmp;
               tmp = add_slash(prog_name);
               if (tmp)
               {
                   free_mstring(prog_name);
                   prog_name = tmp;
               }
            }

            memberpos = mstrchrpos(name.u.str, ' ', pos2);
        }

        /* First, search the struct in the current program.
         * This allows to move inherited structs between modules without
         * breaking the savefiles.
         */
        if (anonymous)
            stt = NULL;
        else
            stt = struct_find(structname, current_object->prog);
        if (!stt && prog_name != NULL)
        {
            do {
                /* Alternatively try to find the struct by its program name.
                 */
                object_t *obj;

                /* Save the strings on the stack in case of errors. */
                push_string(inter_sp, name.u.str);
                push_string(inter_sp, structname);
                if (prog_name)
                    push_string(inter_sp, prog_name);

                obj = get_object(prog_name);

                inter_sp -= 2;
                if (prog_name)
                    inter_sp--;

                if (!obj)
                    break;

                if (O_PROG_SWAPPED(obj)
                 && load_ob_from_swap(obj) < 0
                   )
                    break;

                stt = struct_find(structname, obj->prog);
            } while(0);
        }

        /* Now stt is either NULL or the struct type */

        free_mstring(structname);
        if (prog_name)
            free_mstring(prog_name);

        if (!stt && !anonymous)
        {
            free_mstring(name.u.str);
            return MY_FALSE;
        }

    }
    /* Allocate the struct */
    if (anonymous)
        st = struct_new_anonymous(siz);
    else
        st = struct_new(stt);
    put_struct(svp, st);

    // check if the objects program has RTT checks enabled.
    if (current_object->prog->flags & P_RTT_CHECKS)
        rtt_checks = MY_TRUE;

    if (anonymous)
    {
        struct_member_t *member = st->type->member;
        svalue_t *member_value = st->member;

        push_string(inter_sp, name.u.str); // Save the string in case of errors.

        while (siz--)
        {
            if (!restore_svalue(member_value, str, ','))
            {
                pop_stack();
                return MY_FALSE;
            }

            if (memberpos > 0)
            {
                // We have the member names, change them in the anonymous struct.
                long nextpos = mstrchrpos(name.u.str, ',', memberpos + 1);
                string_t *member_name = mstr_extract(name.u.str, memberpos + 1, nextpos - 1);

                free_mstring(member->name);
                member->name = make_tabled(member_name);

                memberpos = nextpos;
            }

            member++;
            member_value++;
        }

        pop_stack(); // free name.u.str
    }
    else if (memberpos > 0)
    {
        // We restore the struct by member name.

        push_string(inter_sp, name.u.str); // Save the string in case of errors.

        while (true)
        {
            long nextpos = mstrchrpos(name.u.str, ',', memberpos + 1);
            string_t *member_name;
            int idx;

            if (nextpos < 0)
                member_name = find_tabled_str_n(get_txt(name.u.str) + memberpos + 1, mstrsize(name.u.str) - memberpos - 1, STRING_UTF8);
            else
                member_name = find_tabled_str_n(get_txt(name.u.str) + memberpos + 1, nextpos - memberpos - 1, STRING_UTF8);

            idx = member_name ? struct_find_member(stt, member_name) : -1;
            if (idx < 0)
            {
                // We don't have the member anymore, read and discard the value.
                svalue_t tmp;

                if (!restore_svalue(&tmp, str, ','))
                {
                    pop_stack();
                    return MY_FALSE;
                }

                free_svalue(&tmp);
            }
            else
            {
                svalue_t *member_value = st->member + idx;

                if (!restore_svalue(member_value, str, ','))
                {
                    pop_stack();
                    return MY_FALSE;
                }

                if (rtt_checks && !check_rtt_compatibility(stt->member[idx].type, member_value))
                {
                    pop_stack();
                    return MY_FALSE;
                }
            }

            if (nextpos < 0)
                break;
            memberpos = nextpos;
        }

        pop_stack(); // free name.u.str
    }
    else
    {
        // We don't have any member names,
        // so we restore them by index.

        // get a pointer to the structs members in the struct_type_t for checking the types.
        struct_member_t *member = stt->member;
        int extra = 0;

        // We don't need that anymore.
        free_mstring(name.u.str);

        if (struct_t_size(stt) < siz)
        {
            extra = siz - struct_t_size(stt);
            siz = struct_t_size(stt);
        }

        /* Restore the values */
        for ( svp = st->member; --siz >= 0; ++svp, ++member)
        {
            if (!restore_svalue(svp, str, ','))
            {
                return MY_FALSE;
            }
            if (rtt_checks && !check_rtt_compatibility(member->type, svp))
            {
                return MY_FALSE;
            }
        }

        /* If there are more values in the savefile than the struct
         * has members, read and ignore the others.
         */
        while (extra-- > 0)
        {
            svalue_t tmp;

            if (!restore_svalue(&tmp, str, ','))
            {
                return MY_FALSE;
            }
            free_svalue(&tmp);
        }
    }

    /* Check for the trailing '>)' */

    pt = *str;
    if (*pt++ != '>' || *pt++ != ')' ) {
        return MY_FALSE;
    }


    *str = pt;
    return MY_TRUE;
} /* restore_struct() */

/*-------------------------------------------------------------------------*/
static INLINE Bool
restore_closure (svalue_t *svp, char **str, char delimiter)

/* Restore a closure from the text starting at *<str> (which points
 * just after the leading '#') and store it into *<svp>.
 * Return TRUE if the restore was successful, FALSE else (*<svp> is
 * set to const0 in that case).
 * On a successful return, *<str> is set to point after the closure text
 * restored.
 */

{
    char *pt;
    char ct;
    char * name;
    char * nameend, name_delim; /* Holds the name delimiter while the name
                                 * is terminated by '\0' for processing.
                                 */

    pt = *str;
    switch(ct = *pt)
    {
    default:
        fatal("Unsupported closure-type '%c'\n", ct);
        break;

    case 'e': /* An efun closure */
    case 's': /* A sefun closure */
    case 'v': /* A variable closure */
    case 'c': /* A context-lfun closure */
    case 'l': /* A lfun closure */
      {
        char c;

        /* Parse the name of the closure item */
        if (*++pt != ':')
        {
            *svp = const0;
            return MY_FALSE;
        }

        name = ++pt;
        for(;;)
        {
            if ( !(c = *pt++) )
            {
                *svp = const0;
                return MY_FALSE;
            }

            /* Break at the delimiter, but make sure that it's not
             * part of an operator closure ('#e:,' for example).
             */
            if ((   c == delimiter
                 && !(pt[-4] == '#' && pt[-3] == 'e' && pt[-2] == ':')
                )
             || (ct == 'c' && (c == ':' || c=='|' || c=='-'))
             || (ct == 'l' && (c == '|' || c=='-'))
               ) break;
        }

        /* Save the delimiter, then replace it by '\0' */
        nameend = pt-1;
        name_delim = *nameend;
        pt[-1] = '\0';

        *str = pt;
          /* Note: for non-context closures, str now points one
           * char too far. For context closures, str now points 
           * to the first character of the context size value.
           */
      }
    } /* switch(ct) */

    /* Create the proper closure */
    switch (ct)
    {
    case 'e': /* An efun closure */
    case 's': /* A sefun closure */
      {
        symbol_efun_str(name, strlen(name), svp, ct == 'e' ? OVERRIDE_EFUN : OVERRIDE_SEFUN, false);
        break;
      }

    case 'v': /* A variable closure */
      {
        string_t *s;
        object_t *ob;
        variable_t *var;
        program_t *prog;
        int num_var;
        int n;

        ob = current_object;
        if (!current_variables
         || !ob->variables
         || current_variables < ob->variables
         || current_variables >= ob->variables + ob->prog->num_variables)
        {
            /* efun closures are called without changing current_prog
             * nor current_variables. This keeps the program scope for
             * variables for calls inside this_object(), but would
             * give trouble with calling from other ones if it were
             * not for this test.
             */
            current_prog = ob->prog;
            current_variables = ob->variables;
        }

        /* If the variable exists, it must exist as shared
         * string.
         */
        s = find_tabled_str(name, STRING_UTF8);
        if (!s)
        {
            *svp = const0;
            break; /* switch(ct) */
        }

        prog = current_prog;
        var = prog->variables;
        num_var = prog->num_variables;
        for (n = num_var; --n >= 0; var++)
        {
            if (mstreq(var->name, s)
             && !(var->type.t_flags & NAME_HIDDEN))
                break;
        }
        if (n < 0)
        {
            *svp = const0;
            break; /* switch(ct) */
        }

        n = num_var - n - 1;

        /* Check for virtual variables */
        if (current_prog == ob->prog)
        {
            /* No translation necessary. */
        }
        else if (n < current_prog->num_virtual_variables)
        {
            /* Search for the virtual variable in the current_object's
             * variable block.
             */
            n = translate_virtual_variable_index(n);
        }
        else
        {
            /* Ordinary variables, translate the index into current_object's
             * variable block.
             */
            n = n - current_prog->num_virtual_variables
                  + (current_variables - current_object->variables);
        }

        closure_identifier(svp, current_object
                          , (unsigned short)(n)
                          , /* raise_error: */ MY_FALSE);
        if (svp->type != T_CLOSURE)
        {
            /* Out of memory: abort restoring this closure. */
            break; /* switch(ct) */
        }

        break;
      } /* case 'v' */

    case 'c': /* A context closure */
    case 'l': /* A lfun closure */
      {
        string_t *s;
        int i;
        program_t *inhProg = NULL;
        int fun_ix_offs = 0;
        size_t context_size = 0;

        if (name_delim == '|' || name_delim == '-')
        {
            char progname_delim;
            int last_fun_ix_offs = 0;
            int last_num_functions = 0;
            
            /* An inherited lfun closure */
            inhProg = current_object->prog;
            
            do
            {
                char *progname_start, *progname_dest;
                int progname_length;
                inherit_t *inheritp;
                unsigned short inhCount;

                progname_start = pt;
                progname_dest = pt;
                
                for(;;)
                {
                    char c;
                    
                    if ( !(c = *pt++) )
                    {
                        *svp = const0;
                        return MY_FALSE;
                    }
                    
                    if (c == '\\')
                        c = *pt++;
                    else if (c==delimiter
                          || (name_delim=='|' && c=='|')
                          || (ct=='c' && c==':'))
                        break;
                    
                    *progname_dest++ = c;
                }
                
                progname_delim = pt[-1];
                
                if (inhProg) /* Not yet aborted. */
                {
                    last_fun_ix_offs = fun_ix_offs;
                    last_num_functions = inhProg->num_functions;
                
                    *progname_dest = '\0';
                    progname_length = progname_dest - progname_start;
                
                    /* Lookup the inherit */
                    inheritp = inhProg->inherit;
                
                    for ( inhCount = inhProg->num_inherited;
                          inhCount > 0; inheritp++, inhCount--)
                    {
                        int l;
                    
                        if (inheritp->inherit_type & INHERIT_TYPE_DUPLICATE)
                            continue;
                    
                        l = mstrsize(inheritp->prog->name)-2;
                        if (l != progname_length)
                            continue;
                        
                        if (strncmp(progname_start,
                                    get_txt(inheritp->prog->name),
                                    progname_length) != 0)
                            continue;
                        
                        /* Found the inherit. */
                        inhProg = inheritp->prog;
                        fun_ix_offs += inheritp->function_index_offset;
                
                        break;
                    }
                
                    if (!inhCount)
                    {
                        /* No inherit found. Let the while loop go
                         * to the end of the string. */
                        inhProg = NULL;
                        fun_ix_offs = -1;
                    }
                }
            }
            while (progname_delim == '|');
            
            /* Restore delimiter. */
            pt[-1] = progname_delim;
            *str = pt;

            if(inhProg)
            {
                /* Security check. We only allow closures that
                 * can be built by the current program and by the
                 * child programs. That means, the child program
                 * of inhProg must be or inherits the current program
                 * somehow. This is checked using the function index
                 * offset.
                 */

                if (function_index_offset < last_fun_ix_offs
                 || function_index_offset + current_prog->num_functions
                        > last_fun_ix_offs + last_num_functions)
                {
                    inhProg = NULL;
                    fun_ix_offs = -1;
                }
            }
        }

        if (ct == 'c')
        {
            svalue_t num = const0;

            /* Parse the context size information */
            if (!restore_svalue(&num, str, ':')
             || num.type != T_NUMBER
             || num.u.number <= 0
               )
            {
                free_svalue(&num);
                *svp = const0;
                return MY_FALSE;
            }
            context_size = num.u.number;
        }

        /* If the function exists, it must exist as shared
         * string.
         */
        if (fun_ix_offs < 0) /* No need to lookup in case of an error. */
            s = NULL;
        else
            s = find_tabled_str(name, STRING_UTF8);
        /* Although s is NULL, we parse to the end. */

        if (s)
            i = find_function(s, inhProg?inhProg:current_object->prog);
        else
            i = -1;
        
        /* If the function exists and is visible, create the closure.
         */
        if (i >= 0)
        {
            closure_lfun(svp, current_object, inhProg
                        , (unsigned short)i + fun_ix_offs
                        , context_size
                        , /* raise_error: */ MY_FALSE);

            if (svp->type != T_CLOSURE)
            {
                /* Out of memory: abort restoring this closure. */
                break; /* switch(ct) */
            }

            /* Note: the *svp must be set up before any context
             * values are restored, otherwise context values
             * referring to this very closure will be restored
             * as '0'.
             */
            if (context_size > 0)
            {
                svalue_t context = const0;
                int j;
                lambda_t * l = svp->u.lambda;

                /* Parse the context information */
                if (!restore_svalue(&context, str, delimiter)
                 || context.type != T_POINTER
                 || VEC_SIZE(context.u.vec) != context_size
                   )
                {
                    l->function.lfun.context_size = 0;
                    free_svalue(svp);
                    free_svalue(&context);
                    *svp = const0;
                    return MY_FALSE;
                }

                for (j = 0; (size_t)j < context_size; j++)
                    assign_svalue_no_free(l->context+j, context.u.vec->item+j);
                free_array(context.u.vec);
            }
        }
        else /* (i < 0) */
        {
            *svp = const0;

            if (context_size > 0)
            {
                svalue_t context = const0;

                /* Parse the string to its end. */
                if (!restore_svalue(&context, str, delimiter))
                    return MY_FALSE;
                else
                    free_svalue(&context);
            }
        }
        break;
      } /* case 'c', 'l' */
    } /* switch(ct) */

    /* Regardless of the restored closure, *str at this point
     * points to the character after the delimiter.
     * Make the delimiter visible again, and also restore the
     * 'name' delimiter to its original setting.
     */
    *str = *str - 1;
    *nameend = name_delim;
    return MY_TRUE;
} /* restore_closure() */

/*-------------------------------------------------------------------------*/
static INLINE bool
restore_lvalue (svalue_t *svp, char **str)

/* Restore an lvalue reference from the text starting at *<str>
 * (which points just after the leading '&') and store it into *<svp>.
 * Return TRUE if the restore was successful, FALSE else (*<svp> is
 * set to const0 in that case).
 * On a successful return, *<str> is set to point after the lvalue text
 * restored.
 */

{
    svalue_t dummy = const0;
    svalue_t *val;
    struct protected_lvalue *lval = NULL;

    if (restore_ctx->restored_version < SAVE_FORMAT_LVALUES)
    {
        /* Not supported in this version... */
        *svp = const0;
        return false;
    }

    if (**str != '(')
    {
        *svp = const0;
        return false;
    }

    ++*str;

    /* We first try to restore it into a protected lvalue.
     * If it's a range or mapentry lvalue, we convert it later.
     */
    assign_protected_lvalue_no_free(svp, &dummy);
    free_svalue(&dummy);

    val = &(svp->u.protected_lvalue->val);
    if (!restore_svalue(val, str, ','))
    {
        free_svalue(svp);
        *svp = const0;
        return false;
    }

    if (**str == ')')
    {
        ++*str;
        return true;
    }

    /* So, it's either a range or mapentry lvalue. */
    if (val->type == T_LVALUE && val->x.lvalue_type == LVALUE_PROTECTED)
    {
        lval = val->u.protected_lvalue;
        val = &(lval->val);
    }

    if (val->type == T_MAPPING)
    {
        svalue_t key;
        int idx;
        char * cp;

        if (!restore_svalue(&key, str, ','))
        {
            free_svalue(svp);
            *svp = const0;
            return false;
        }

        cp = *str;
        idx = strtol(*str, str, 10);
        if (cp == *str || *((*str)++) != ')')
        {
            free_svalue(&key);
            free_svalue(svp);
            *svp = const0;
            return false;
        }

        assign_protected_mapentry_lvalue_no_free(&dummy, val->u.map, &key, idx);
        free_svalue(&key);
    }
    else
    {
        p_int idx1, idx2, size;
        char *cp = *str;

        idx1 = strtol(*str, str, 10);
        if (cp == *str || *((*str)++) != '.' || *((*str)++) != '.')
        {
            free_svalue(svp);
            *svp = const0;
            return false;
        }

        cp = *str;
        idx2 = strtol(*str, str, 10);
        if (cp == *str || *((*str)++) != ')')
        {
            free_svalue(svp);
            *svp = const0;
            return false;
        }


        switch (val->type)
        {
            case T_POINTER:
                size = VEC_SIZE(val->u.vec);
                break;

            case T_STRING:
            case T_BYTES:
                size = mstrsize(val->u.str);
                break;

            default:
                free_svalue(svp);
                *svp = const0;
                return false;
        }

        if (idx1 < 0 || idx1 > size || idx2 <= -1 || idx2 >= size || idx1 > idx2 + 1)
        {
            free_svalue(svp);
            *svp = const0;
            return false;
        }

        /* Create our range lvalue and replace the normal
         * lvalue reference in <svp>.
         */
        assign_protected_range_lvalue_no_free(&dummy, lval, val, idx1, idx2+1);
        if (svp->u.protected_lvalue->ref != 1)
        {
            /* There is already a second reference to it.
             * Updating that one also.
             */
            free_svalue(val);
            assign_svalue_no_free(val, &dummy);
        }
    }

    free_svalue(svp);
    transfer_svalue_no_free(svp, &dummy);

    return true;
} /* restore_lvalue() */

/*-------------------------------------------------------------------------*/
static Bool
restore_svalue (svalue_t *svp, char **pt, char delimiter)

/* Restore an svalue from the text starting at *<pt> up to the <delimiter>,
 * storing the value in *<svp>.
 * On success, set *<pt> to the character after the <delimiter> and return
 * TRUE, else return FALSE.
 */

{
    char *cp;
    bool isbyte = false;

    assert_stack_gap();

    switch( *(cp = *pt) )
    {
    case '#':  /* A closure or quoted thing */
      {
        *pt = ++cp;
        switch (*cp)
        {
        case 'e': /* An efun closure */
        case 's': /* A sefun closure */
        case 'v': /* A variable closure */
        case 'c': /* A lfun closure */
        case 'l': /* A lfun closure */
            if ( !restore_closure(svp, pt, delimiter) )
            {
                return MY_FALSE;
            }
            break;

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          {
            long quotes;
            char * end;
            Bool   rc;

            quotes = strtol(cp, &end, 10);
            if (!end || end == cp || *end != ':')
            {
                *svp = const0;
                return MY_FALSE;
            }
            *pt = end+1;
            rc = restore_svalue(svp, pt, delimiter);
            if (rc)
            {
                svp->x.quotes = (ph_int)quotes;
                if (svp->type == T_STRING)
                    svp->type = T_SYMBOL;
                else if (svp->type == T_POINTER)
                    svp->type = T_QUOTED_ARRAY;
                return MY_TRUE;
            }
            else
                return MY_FALSE;
            break;
          }
        
        default:
            *svp = const0;
            return MY_FALSE;
        }

        break;
      }

    case 'b':   /* A byte sequence */
        cp++;
        if (*cp != '\"')
        {
            *svp = const0;
            return MY_FALSE;
        }

        isbyte = true;
        /* FALLTHROUGH. */

    case '\"':  /* A string */
      {
        char *source, *start, c;
        char *buf = NULL;       /* Dynamic memory in case the <cp> buf gets to small. */
        p_int len = 0;

        start = cp;
        source = cp+1;

        for(;;)
        {
            p_int result;

            if ( !(c = *source++) )
            {
                xfree(buf);
                *svp = const0;
                return MY_FALSE;
            }

            if (c == '\r')
                result = '\n';
            else if (c == '\\')
            {
                if ( !(c = *source++) )
                {
                    xfree(buf);
                    *svp = const0;
                    return MY_FALSE; /* String ends with a \\ buggy probably */
                }
                switch(c)
                {
                    case '0': result = '\0';   break;
                    case 'a': result = '\007'; break;
                    case 'b': result = '\b'  ; break;
                    case 't': result = '\t'  ; break;
                    case 'n': result = '\n'  ; break;
                    case 'v': result = '\013'; break;
                    case 'f': result = '\014'; break;
                    case 'r': result = '\r'  ; break;

                    case 'u': case 'U':
                    {
                        result = 0;
                        for(int digits = (c == 'u') ? 4 : 8; digits--;)
                        {
                            result <<= 4;
                            c = *source++;

                            if (c >= '0' && c <= '9')
                                result |= (c - '0');
                            else if (c >= 'a' && c <= 'f')
                                result |= (c - 'a' + 10);
                            else
                            {
                                xfree(buf);
                                *svp = const0;
                                return MY_FALSE;
                            }
                        }
                        break;
                    }

                    case 'x':
                    {
                        result = 0;

                        c = *source++;
                        if (c >= '0' && c <= '9')
                            result = (c - '0') << 4;
                        else if (c >= 'a' && c <= 'f')
                            result = (c - 'a' + 10) << 4;
                        else
                        {
                            xfree(buf);
                            *svp = const0;
                            return MY_FALSE;
                        }

                        c = *source++;
                        if (c >= '0' && c <= '9')
                            result += (c - '0');
                        else if (c >= 'a' && c <= 'f')
                            result += (c - 'a' + 10);
                        else
                        {
                            xfree(buf);
                            *svp = const0;
                            return MY_FALSE;
                        }
                        break;
                    }

                    default:
                        result = (unsigned char)c;
                        break;
                }
            }
            else if (c == '\"')
                break;
            else
                result = (unsigned char)c;

            if (isbyte)
            {
                *cp++ = (unsigned char)result;
                len++;
            }
            else
            {
                if (!buf && (result & ~0x7f) && source - cp < 4)
                {
                    /* We need to use another memory block for the target
                     * string.
                     */
                    buf = (char*) xalloc(len + 4*strlen(cp) + 4);
                    if (!buf)
                    {
                        *svp = const0;
                        errorf("(restore) Out of memory (%zu bytes) for string.\n"
                             , len + 4*strlen(cp) + 4);
                    }

                    memcpy(buf, start, len);
                    start = buf;
                    cp = buf + len;
                }

                size_t clen = unicode_to_utf8(result, cp);
                if (!clen)
                {
                    xfree(buf);
                    *svp = const0;
                    return MY_FALSE;
                }
                cp += clen;
                len += clen;
            }
        }
        *cp = '\0';
        *pt = source;
        if (isbyte)
            put_bytes(svp, new_n_tabled(start, len, STRING_BYTES));
        else
            put_string(svp, new_n_unicode_tabled(start, len));
        xfree(buf);
        if (!svp->u.str)
        {
            *svp = const0;
            errorf("(restore) Out of memory (%zu bytes) for string.\n"
                 , strlen(start));
        }
        break;
      }

    case '(': /* Unshared mapping, struct or array */
        *pt = cp+2;
        switch ( cp[1] )
        {
        case '[':
          {
            if ( !restore_mapping(svp, pt) )
            {
                return MY_FALSE;
            }
            break;
          }

        case '{':
          {
            if ( !restore_array(svp, pt) )
            {
                return MY_FALSE;
            }
            break;
          }

        case '<':
          {
            if ( !restore_struct(svp, pt) )
            {
                return MY_FALSE;
            }
            break;
          }

        default:
            *svp = const0;
            return MY_FALSE;
        }
        break;

    case '-':  /* A number */
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      {
        char *numstart = cp;
        double dval;
        intmax_t ival;

        // first try to restore the number as integer number.
        errno=0;
        ival = strtoimax(numstart, &cp, 10);
        if (errno == EINVAL || numstart == cp)
            errorf("Could not restore integer in restore_svalue() (probably "
              "illegal value format).\n");
        else if (errno == ERANGE || ival > PINT_MAX || ival < PINT_MIN) {
            warnf("Integer value out of range in restore_svalue(). Value was "
              "truncated!\n");
            if (ival < 0)
                ival = PINT_MIN;
            else
                ival = PINT_MAX;
        }

        // if strtoimax stopped at a 'x' or '.', this seems to be a floating
        // number (then start again below), otherwise this is regarded as
        // integer number and put into svp and we return.
        if ( (restore_ctx->restored_version >= SAVE_FORMAT_FLOATS  && *cp != 'x')
            || (restore_ctx->restored_version < SAVE_FORMAT_FLOATS && *cp != '.' ) )
        {
            put_number(svp, (p_int)ival);
            *pt = cp+1;   // cp points to the delimeter, pt to one char after that...
            return *cp == delimiter;  // if everything is OK.
        }

        svp->type = T_FLOAT;
        if (restore_ctx->restored_version >= SAVE_FORMAT_FLOATS )
        {
            // In version 2 (and above) restore the string written by sprintf("%a")
            cp = numstart;
            errno = 0;
            dval = strtod(cp, &cp);
            if (errno == ERANGE)
            {
                warnf("Floating point number out of range in restore_svalue().\n");
            }
        }
        else
        {
            /* If a float was written by the same host type as we are using,
             * restore the internal representation.
             * Otherwise, parse the float normally.
             */
            cp = strchr(cp, '=');
            if (restore_ctx->restored_host == CURRENT_HOST 
                && NULL != cp)
            {
                ++cp;
                int32_t mantissa;
                int16_t exponent;
                if (sscanf(cp, "%"SCNx16":%"SCNx32, &exponent, &mantissa) != 2)
                    return 0;
                dval = JOIN_DOUBLE(mantissa, exponent);
            }
            else
            {
                cp = numstart;
                errno=0;
                dval = strtod(numstart, &cp);
                if (errno == ERANGE)
                {
                    warnf("Floating point number out of range in restore_svalue().\n");
                }
            }
        }
        STORE_DOUBLE(svp,dval);
        cp = strchr(cp, delimiter);
        *pt = cp+1;
        return cp != NULL;
      }

    case '&': /* An lvalue reference */
      {
        *pt = ++cp;
        if ( !restore_lvalue(svp, pt) )
        {
            return MY_FALSE;
        }
        break;
      }

    case '<': /* A shared value */
      {
        int id;

        id = atoi(cp+1);
        cp = strchr(cp, '>');
        if (!cp)
        {
            *svp = const0;
            return MY_FALSE;
        }

        /* If a '=' follows, this is the first occurance of this
         * shared value, therefore restore it normally.
         */
        if (cp[1] == '=')
        {
            int res;

            *pt = cp+2;

            /* Shared values can be used even before they have been read in
             * completely.
             */
            if (id != ++(restore_ctx->current_shared_restored))
            {
                restore_ctx->current_shared_restored--;
                *svp = const0;
                return MY_FALSE;
            }

            /* Increase shared_restored_values[] if necessary */

            if (id > restore_ctx->max_shared_restored)
            {
                svalue_t *new;

                restore_ctx->max_shared_restored *= 2;
                new = rexalloc(restore_ctx->shared_restored_values
                              , sizeof(svalue_t)*(restore_ctx->max_shared_restored)
                              );
                if (!new)
                {
                    restore_ctx->current_shared_restored--;
                    *svp = const0;
                    errorf("(restore) Out of memory (%lu bytes) for "
                          "%ld shared values.\n"
                          , (unsigned long)restore_ctx->max_shared_restored * sizeof(svalue_t)
                          , restore_ctx->max_shared_restored);
                    return MY_FALSE;
                }
                restore_ctx->shared_restored_values = new;
            }

            /* in case of an error... */
            *svp = const0;
            restore_ctx->shared_restored_values[id-1] = const0;

            /* Restore the value */
            res = restore_svalue(&(restore_ctx->shared_restored_values[id-1]), pt, delimiter);
            assign_svalue_no_free(svp, &(restore_ctx->shared_restored_values[id-1]));
            return res;
        }

        if (id <= 0 || id > restore_ctx->current_shared_restored)
        {
            *svp = const0;
            return MY_FALSE;
        }

        /* We know this value already: simply assign it */

        assign_svalue_no_free(svp, &(restore_ctx->shared_restored_values[id-1]));

        cp = strchr(cp, delimiter);
        *pt = cp+1;
        return cp != NULL;
      }

    default:
        *svp = const0;
        return MY_FALSE;

    } /* switch()*/

    cp = *pt;
    if (delimiter == '\n' && *cp == '\r')
        cp++;
    if (*cp++ != delimiter)
        return MY_FALSE;
    *pt = cp;

    return MY_TRUE;
} /* restore_svalue() */

/*-------------------------------------------------------------------------*/
static Bool
old_restore_string (svalue_t *v, char *str)

/* Called to restore the string starting at <str> into the *<v>
 * from old-format savefiles
 * Return TRUE on success, FALSE else.
 *
 * In this format, no escaped characters exist.
 */

{
    char *cp, c;

    cp = ++str;
    if ( '\0' != (c = *cp++) )
    {
        do {
            if (c == '\r')
                cp[-1] = '\n';
        } while ( '\0' != (c = *cp++) );

        if (cp[-2] == '\n' && cp[-3] == '\"')
        {
            cp[-3] = '\0';
            put_string(v, new_unicode_tabled(str));
            if (!v->u.str)
            {
                *v = const0;
                errorf("(restore) Out of memory (%zu bytes) for string\n"
                     , strlen(str));
            }
            return MY_TRUE;
        }
    }
    *v = const0;
    return MY_FALSE;
} /* old_restore_string() */

/*-------------------------------------------------------------------------*/
/* Cleanup structure for restore_object().
 */

struct discarded {
    svalue_t v;
    struct discarded *next;
};

typedef struct restore_cleanup_s {
    error_handler_t    head;      /* The T_ERROR_HANDLER structure */
    int              * pNesting;  /* The nesting counter */
    char             * buff;      /* The optional allocated line buffer. */
    FILE             * f;         /* The optional input file */
    struct discarded * dp;
    char             * filename;  /* optional buffer for the filename */
      /* List of values for which the variables no longer exist. */
} restore_cleanup_t;


static void
restore_object_cleanup ( error_handler_t * arg)

/* The error handler during restore_object cleanup: free all resources
 * and update the nesting.
 */

{
    restore_cleanup_t * data = (restore_cleanup_t *)arg;
    struct restore_context_s * ctx;

    while (data->dp)
    {
        struct discarded * next = data->dp->next;
        free_svalue(&data->dp->v);
        xfree(data->dp);
        data->dp = next;
    }

    if (*data->pNesting > 1)
        xfree(data->buff);
    else
        mb_free(mbFile);

    if (data->f)
        fclose(data->f);

    (*data->pNesting)--;

    free_shared_restored_values();
  
    if (data->filename)
        xfree(data->filename);
  
    xfree(arg);
    
    ctx = restore_ctx;
    restore_ctx = ctx->previous;
    xfree(ctx);    
} /* restore_object_cleanup() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_restore_object (svalue_t *sp)

/* EFUN restore_object()
 *
 *   int restore_object (string name)
 *   int restore_object (string str)
 *
 * Restore values of variables for current object from the file <name>,
 * or directly from the string <str>.
 *
 * To restore directly from a string <str>, the string must begin
 * with the typical line "#x:y" as it is created by the save_object()
 * efun.
 *
 * When restoring from a file, the name may end in ".c" which is stripped
 * off by the parser. The master object will probably append a .o to the
 * <name>. The validity of the filename is checked with a call to
 * check_valid_path().
 *
 * Return 1 on success, 0 if there was nothing to restore.
 * TODO: This double-mode is rather ugly, maybe call the restoring
 * TODO:: from string-mode "restore_variables()"?
 */

{
static int nesting = 0;  /* Used to detect recursive calls */
    char *name;      /* Full name of the file to read */
    char *file;      /* Filename passed, NULL if restoring from a string */
    int   lineno;    /* Line number in file, for error messages */
    string_t *var;
    char *buff;      /* Current line read from the savefile
                      * resp. a copy of the string passed.
                      */
    char *cur;       /* Current position in the string passed */
    char *space;
    object_t *ob;    /* Local copy of current_object */
    size_t len;
    FILE *f;
    struct stat st;  /* stat() info of the savefile */
    svalue_t *arg;   /* pointer to the argument on the stack - for convenience */

    int var_rest;    /* Number of variables left after rover */
    int num_var;     /* Number of variables in the object */
    variable_t *rover = NULL;
      /* Roving pointer through the variable block. The next variable
       * to restore is searched from here, taking advantage of
       * the locality of save_object().
       */
    restore_cleanup_t * rcp;
      /* Cleanup structure */
    struct restore_context_s * ctx;
      /* Our helper structure. */
    
    arg = sp;
    
    /* Allocate memory for the error cleanup structure first. We need it 
     * anway and if we can't allocate it, then this is anyway a lost cause. */
    rcp = xalloc(sizeof(*rcp));
    if (!rcp)
    {
        errorf("(restore) Out of memory: (%zu bytes) for cleanup structure\n"
               , sizeof(*rcp));
        /* NOTREACHED */
        return sp;
    }
    ctx = xalloc(sizeof(*ctx));
    if (!ctx)
    {
        xfree(rcp);
        errorf("(restore) Out of memory: (%zu bytes) for context structure\n"
               , sizeof(*ctx));
        /* NOTREACHED */
        return sp;
    }

    rcp->pNesting = &nesting;
    rcp->buff = NULL;
    rcp->f = NULL;
    rcp->dp = NULL;
    rcp->filename = NULL;

    ctx->restored_host = -1;
    ctx->restored_version = -1;
    ctx->current_shared_restored = 0;
    ctx->shared_restored_values = NULL;
    ctx->previous = restore_ctx;
    
    /* Push it on top of the argument on the stack. */
    sp = push_error_handler(restore_object_cleanup, &(rcp->head));
    restore_ctx = ctx;
  
    /* Keep track of recursive calls */
    nesting++;

    /* No use in restoring a destructed object, or an object
     * with no variables. Do this check now before we allocate
     * any memory.
     */
    ob = current_object;
    if (ob->flags & O_DESTRUCTED)
    {
        sp = pop_n_elems(2, sp); /* pop and free error handler + argument */
        sp++;
        put_number(sp, 0);
        return sp;
    }

    /* no need to restore objects without variables */
    if (ob->prog->num_variables == 0)
    {
        sp = pop_n_elems(2, sp); /* pop and free error handler + argument */
        sp++;
        put_number(sp, 1);
        return sp;
    }

    /* Check if got a filename or the value string itself */
    buff = NULL;
    name = NULL;
    file = NULL;
    f = NULL;
    lineno = 0;
    if (get_txt(arg->u.str)[0] == '#')
    {
        /* We need a copy of the value string because we're
         * going to modify it a bit.
         */
        len = mstrsize(arg->u.str);
        buff = (nesting > 1) ? xalloc(len+1) : mb_alloc(mbFile, len+1);
        if (buff == NULL)
        {
            outofmem(len+1, "copy of value string");
        }
        /* keep track of buff in the cleanup structure. */
        rcp->buff = buff;
        memcpy(buff, get_txt(arg->u.str), len);
        buff[len] = '\0';
    }
    else
    {
        file = get_txt(arg->u.str);
    }

    /* If restoring from a file, set it up */

    if (file)
    {
        string_t *sfile;
        char *native;

        /* Get a valid filename */

        sfile = check_valid_path(arg->u.str, ob, STR_RESTORE_OBJECT, MY_FALSE);
        if (sfile == NULL)
        {
            errorf("Illegal use of restore_object('%s')\n", get_txt(arg->u.str));
            /* NOTREACHED */
            return sp;
        }

        /* Create the full filename */
        len = mstrsize(sfile);
        name = xalloc(len + (sizeof save_file_suffix));
        if (!name)
        {
            free_mstring(sfile);
            errorf("Out of memory (%zu bytes) for filename buffer in "
                   "restore_object('%s')\n", len, get_txt(arg->u.str));
            /* NOTREACHED */
            return sp;
        }
        rcp->filename = name;  /* in case of errrors -> cleanup structure */

        strcpy(name, get_txt(sfile));
        if (name[len-2] == '.' && name[len-1] == 'c')
            len -= 2;
        strcpy(name+len, save_file_suffix);

        free_mstring(sfile);

        /* Open the file and gets its length (binary mode is ignored on all
         * POSIX conforming platforms but not on Cygwin).
         */
        native = convert_path_to_native_or_throw(name, len + sizeof save_file_suffix);
        f = fopen(native, "rb");
        if (!f || fstat(fileno(f), &st) == -1) {
            if (f)
                fclose(f);
            sp = pop_n_elems(2, sp); /* pop and free error handler + argument */
            sp++;
            put_number(sp, 0);
            return sp;
        }
        rcp->f = f;    /* keep track of f in case of errors */
        if (st.st_size == 0)
        {
            sp = pop_n_elems(2, sp); /* pop and free error handler + argument */
            sp++;
            put_number(sp, 0);
            return sp;
        }
        FCOUNT_REST(native);

        /* Allocate the linebuffer. Unfortunately, the whole file
         * can be one single line.
         */
        buff = (nesting > 1) ? xalloc((size_t)(st.st_size + 1))
                             : mb_alloc(mbFile, (size_t)(st.st_size+1));
        if (!buff)
        {
            /* TODO: st_size is off_t which is most often int64_t or int32_t.
             * TODO:: PRIdMAX or PRId64 should be used, I think. */
            errorf("(restore) Out of memory (%ld bytes) for linebuffer.\n"
                 , (long) st.st_size+1);
            /* NOTREACHED */
            return sp;
        }
        rcp->buff = buff;
    } /* if (file) */

    /* Initialise the variables */

    ctx->max_shared_restored = 64;
    ctx->shared_restored_values = xalloc(sizeof(svalue_t)*(ctx->max_shared_restored));

    if (!ctx->shared_restored_values)
    {
        errorf("(restore) Out of memory (%lu bytes) for shared values.\n"
             , sizeof(svalue_t)*(unsigned long)ctx->max_shared_restored);
        /* NOTREACHED */
        return sp;
    }
  
    num_var = ob->prog->num_variables;
    var_rest = 0;

    /* Loop until we run out of text to parse */

    cur = buff;
    while(1)
    {
        svalue_t *v;        // the svalue to restore into
        fulltype_t vtype;    // the type of the variable being restored.
        char *pt;

        if (file)
        {
            /* Get the next line from the text */
            lineno++;
            if (fgets(buff, (int)st.st_size + 1, f) == NULL)
                break;
            cur = buff;
        }
        else if (cur[0] == '\0')
            break;

        /* Remember that we have a newline, and maybe even a CRLF at end of
         * buff!
         */
        pt = strchr(cur, '\r');
        if (pt && pt[1] == '\n') /* Convert a CRLF into a LF */
            *pt = '\n';
        pt = NULL;


        space = strchr(cur, ' ');
        if (!file)
            pt = strchr(cur, '\n');
        else
            pt = NULL;

        if (space == NULL || (!file && pt && pt < space))
        {
            /* No space? It must be the version line! */

            if (cur[0] == '#')
            {
                int i;

                i = sscanf(cur+1, "%d:%d", &(ctx->restored_version), &(ctx->restored_host));
                if (i > 0 && (i == 2 || ctx->restored_version >= CURRENT_VERSION) )
                {
                    if (pt)
                        cur = pt+1;
                    else if (!file)
                        break;
                    continue;
                }
            }

            /* No version line: illegal format.
             * Most of the cleanup will be done by the error handler during
             * stack unwinding.
             */
            if (file)
                errorf("Illegal format (version line) when restoring %s "
                      "from %s line %d.\n"
                      , get_txt(current_object->name), name, lineno);
            else
                errorf("Illegal format (version line) when restoring %s.\n"
                      , get_txt(current_object->name));
            /* NOTREACHED */
            return sp;
        }

        /* Split the line at the position of the space.
         * Left of it is the variable name, to the right is the value.
         */
        *space = '\0';

        /* Set 'v' to the variable to restore */

        v = NULL;

        do { /* A simple try.. environment */

            if ( NULL != (var = find_tabled_str(cur, STRING_UTF8)) )
            {
                /* The name exists in an object somewhere, now check if it
                 * is one of our variables
                 */

                do
                    rover++;
                while ( --var_rest > 0
                     && (rover->name != var
                         || rover->type.t_flags & TYPE_MOD_STATIC)
                      );

                if (var_rest > 0)
                {
                    v = &ob->variables[num_var-var_rest];
                    vtype = ob->prog->variables[num_var-var_rest].type;
                    break;
                }

                /* Wrap around and search again */

                rover = ob->prog->variables-1;
                var_rest = num_var + 1;
                do
                    rover++;
                while (--var_rest > 0
                   &&  (rover->name != var
                        || rover->type.t_flags & TYPE_MOD_STATIC)
                      );
                if (var_rest > 0)
                {
                    v = &ob->variables[num_var-var_rest];
                    vtype = ob->prog->variables[num_var-var_rest].type;
                    break;
                }
            }

            /* No 'else', but if we come here, the variable name was
             * not found in the shared string table or in the object.
             * That means we can eventually discard this line, but first
             * we have to parse it in case it contains the definition
             * of a shared array some other variable might use.
             *
             * Therefore we create a dummy variable and initialize
             * it to svalue-int, so that it can be freed without remorse.
             */

            {
                struct discarded *tmp;

                tmp = (struct discarded *)xalloc(sizeof(struct discarded));
                if (!tmp)
                {
                    if (file)
                        errorf("Out of memory when restoring %s "
                              "from %s line %d.\n"
                              , get_txt(current_object->name), name, lineno);
                    else
                        errorf("Out of memory when restoring %s.\n"
                              , get_txt(current_object->name));
                    /* NOTREACHED */
                    return sp;
                }

                tmp->next = rcp->dp;
                rcp->dp = tmp;
                v = &tmp->v;
                v->type = T_NUMBER;
                vtype.t_flags = 0;
                vtype.t_type = NULL;
                break;
            }

        } while (MY_FALSE);

        /* Get rid of the old value in v */

        free_svalue(v);
        *v = const0;

        /* ...and set it to the new one */

        pt = space+1;
        if ( (ctx->restored_version < 0 && pt[0] == '\"')
             ? !old_restore_string(v, pt)
             : !restore_svalue(v, &pt, '\n')
           )
        {

            /* Whoops, illegal format */

            if (file)
                errorf("Illegal format (value string) when restoring %s "
                      "from %s line %d.\n"
                      , get_txt(current_object->name), name, lineno);
            else
                errorf("Illegal format (value string) when restoring %s.\n"
                      , get_txt(current_object->name));
            /* NOTREACHED */
            return sp;
        }
        // now check if the type of the just restored svalue is compatible to
        // the type of the variable we restored to.
        // TODO: it would be nice to check P_RTT_CHECKS in the program defining
        // TODO::the var instead of ob->prog. But finding the right prog is not
        // TODO::trivial.
        if (ob->prog->flags & P_RTT_CHECKS)
        {
            if (!check_rtt_compatibility(vtype.t_type, v))
            {
                static char realtypebuf[512];
                lpctype_t *realtype = get_rtt_type(vtype.t_type, v);
                get_lpctype_name_buf(realtype, realtypebuf, sizeof(realtypebuf));
                free_lpctype(realtype);

                if (ob->prog->flags & P_WARN_RTT_CHECKS)
                    warnf("Bad type when restoring %s from %s line %d. Expected "
                       "%s, got %s.\n",
                       get_txt(current_object->name), name, lineno,
                       get_lpctype_name(vtype.t_type), realtypebuf);
                else
                {
                    free_svalue(v);
                    *v = const0;

                    errorf("Bad type when restoring %s from %s line %d. Expected "
                       "%s, got %s.\n",
                       get_txt(current_object->name), name, lineno,
                       get_lpctype_name(vtype.t_type), realtypebuf);
                }
            }
        }

        cur = pt;
    } /* while(1) */

    /* Restore complete - now clean up */

    if (d_flag > 1)
        debug_message("%s Object %s restored from %s.\n"
                     , time_stamp(), get_txt(ob->name)
                     , file ? name : "passed value");

    free_svalue(sp--); /* calls the cleanup handler */
    free_svalue(sp);  /* frees the argument */
    put_number(sp, 1);
    return sp;

} /* f_restore_object() */

/*-------------------------------------------------------------------------*/
static void
restore_value_cleanup ( error_handler_t * arg )

/* The error handler during restore value cleanup: free all resources.
 */

{
    restore_cleanup_t * data = (restore_cleanup_t *) arg;
    struct restore_context_s * ctx;

    if (data->buff)
        xfree(data->buff);

    free_shared_restored_values();

    xfree(arg);

    ctx = restore_ctx;
    restore_ctx = ctx->previous;
    xfree(ctx);    
} /* restore_value_cleanup() */

svalue_t *
f_restore_value (svalue_t *sp)

/* EFUN restore_value()
 *
 *   mixed restore_value (string str)
 *
 * Decode the string representation <str> of a value back into the value
 * itself and return it. <str> is a string as generated by save_value(),
 * the '#x:y' specification of the saveformat however is optional.
 */

{
    char      *buff;  /* The string to parse */
    char      *p;
    svalue_t  *arg;   /* pointer to the argument on the stack - for convenience */
    restore_cleanup_t *rcp; /* Cleanup structure */
    struct restore_context_s * ctx; /* Our helper structure. */

    /* Place the result variable onto the stack */
    arg = sp;
    inter_sp = ++sp;
    *sp = const0;

    /* Setup the error cleanup */
    rcp = xalloc(sizeof(*rcp));
    if (!rcp)
    {
        errorf("(restore) Out of memory (%zu bytes).\n"
              , sizeof(*rcp));
        /* NOTREACHED */
        return sp;
    }
    ctx = xalloc(sizeof(*ctx));
    if (!ctx)
    {
        xfree(rcp);
        errorf("(restore) Out of memory: (%zu bytes) for context structure\n"
             , sizeof(*ctx));
        /* NOTREACHED */
        return sp;
    }

    rcp->buff = NULL;

    ctx->restored_host = -1;
    ctx->restored_version = -1;
    ctx->current_shared_restored = 0;
    ctx->shared_restored_values = NULL;
    ctx->previous = restore_ctx;
    
    push_error_handler(restore_value_cleanup, &(rcp->head));
    restore_ctx = ctx;

    /* The restore routines will put \0s into the string, so we
     * need to make a copy of all but malloced strings.
     */
    {
        size_t len;

        len = mstrsize(arg->u.str);
        buff = xalloc(len+1);
        if (!buff)
        {
            errorf("(restore) Out of memory (%zu bytes).\n"
                 , len+1);
            /* NOTREACHED */
            return sp;
        }
        memcpy(buff, get_txt(arg->u.str), len);
        buff[len] = '\0';

        rcp->buff = buff;
    }

    /* Initialise the shared value table */

    ctx->max_shared_restored = 64;
    ctx->shared_restored_values = xalloc(sizeof(svalue_t)*(ctx->max_shared_restored));
    if (!ctx->shared_restored_values)
    {
        errorf("(restore) Out of memory (%lu bytes) for shared values.\n"
             , (unsigned long)ctx->max_shared_restored * sizeof(svalue_t));
        return sp; /* flow control hint */
    }

    /* Check if there is a version line */
    if (buff[0] == '#')
    {
        sscanf(buff+1, "%d:%d", &(ctx->restored_version), &(ctx->restored_host));

        /* Advance to the next line */
        p = strchr(buff, '\n');
        if (!p)
        {
            errorf("No data given.\n");
            return sp-1;
        }
        p++;
    }
    else
        p = buff; /* parse from beginning of buffer */


    /* Now parse the value in buff[] */

    if ( (ctx->restored_version < 0 && p[0] == '\"')
         ? !old_restore_string(sp, p)
         : !restore_svalue(sp, &p, '\n')
       )
    {
        /* Whoops, illegal format */

        errorf("Illegal format when restoring a value.\n");
        /* NOTREACHED */
        return sp; /* flow control hint */
    }

    if (*p != '\0')
    {
        errorf("Illegal format when restoring a value: extraneous characters "
              "at the end.\n");
        /* NOTREACHED */
        return sp; /* flow control hint */
    }

    /* Restore complete - now clean up and return the result */

    free_svalue(inter_sp--);
    sp = --inter_sp;
    free_string_svalue(sp);
    *sp = sp[1];

    return sp;
} /* f_restore_value() */

/***************************************************************************/

