/*---------------------------------------------------------------------------
 * Simul-Efun Handling
 *
 *---------------------------------------------------------------------------
 * Simul-efuns are a way to provide a mudlib with additional efuns which are
 * nevertheless implemented in LPC.
 *
 * The simul-efuns are implemented in one "simul-efun object" which is
 * loaded by the master and made known to the driver by it's name (this way
 * it is easier to find it again after an update). When a simul-efun is
 * removed from the game, the master may provide 'backup' objects which
 * still provide the old simul-efun so that older programs still run
 * albeit slower (see interpret.c:call_simul_efun()). The semantic of
 * a simul-efun call is that of a call-other, and specific calls may even
 * be implemented as such.
 *
 * The driver keeps a table of all simul-efuns compiled so far,
 * distinguished by name and number of arguments. If a simul-efun is removed
 * from the simul-efun object, its corresponding entry is only marked as
 * "discarded", again because older programs may still reference it by index.
 * If such a discarded simul-efun is re-implemented by a new simul-efun object,
 * the old table entry is reactivated.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <stdio.h>
#include <assert.h>

#include "simul_efun.h"

#include "array.h"
#include "closure.h"
#include "exec.h"
#include "gcollect.h"
#include "interpret.h"
#include "lex.h"
#include "mstrings.h"
#include "object.h"
#include "ptrtable.h"
#include "prolang.h" /* is_undef_function() */
#include "simulate.h"
#include "stdstrings.h"
#include "svalue.h"
#include "swap.h"
#include "xalloc.h"
#include "pkg-python.h"

/*-------------------------------------------------------------------------*/

object_t *simul_efun_object  = NULL;
  /* The primary simul_efun object.
   * The pointer is not counted in the references, and explicitely
   * check in simulate:destruct().
   */

vector_t *simul_efun_vector  = NULL;
  /* When available, all simul_efun object names
   * The first is the name of the primary object - it these object's
   * functions which are tabled. All alternative objects are used
   * using a normal apply() from interpret.c:call_simul_efun().
   */

#define SIZE_SEFUN_TABLE (sizeof simul_efun_table / sizeof simul_efun_table[0])

simul_efun_table_t simul_efun_table[SEFUN_TABLE_SIZE];
  /* The table holding the information for all simul-efuns which
   * can be called directly with the SIMUL_EFUN instruction.
   *
   * Sefuns are distinguished by name and number of arguments - discarded
   * sefuns are not removed because older programs might still reference
   * them. On the other hand, a discarded sefun may be re-actived if a
   * suitable function is compiled.
   *
   * The table contains the function_t of all (current and former)
   * sefuns. For discarded sefuns .funstart will be NULL,
   * otherwise .funstart, .program, .function_index_offset and
   * .variable_index_offset can be used to directly jump into the
   * function.
   */

ident_t *all_simul_efuns = NULL;
  /* The list of all active simul_efun identifiers, which are also
   * held in the symbol table.
   */

int num_simul_efun = 0;
  /* Number of functions (active or not) listed in simul_efun_table.
   */

static string_t *simul_efun_file_name = NULL;
  /* The shared name of the primary simul_efun object
   */

static program_t *simul_efun_program= NULL;
  /* The program of the primary simul_efun object.
   */

static unsigned short all_discarded_simul_efun = I_GLOBAL_SEFUN_OTHER;
  /* First index of the list of discarded sefuns in simul_efun_table.
   * With this list, it is faster to find a sefun entry to reactivate.
   */

/*-------------------------------------------------------------------------*/
static void
remove_efun_shadows (ident_t* list)

/* Remove sefun shadows for efuns / python efuns in the given list.
 */

{
    ident_t *id;
    for (id = list; id; id = id->next_all)
    {
        int j = id->u.global.sim_efun;

        /* If they are listed in the table, move them into the inactive list. */
        if (j != I_GLOBAL_SEFUN_OTHER && j != I_GLOBAL_SEFUN_BY_NAME && j < SIZE_SEFUN_TABLE)
        {
            simul_efun_table[j].function.offset.next_sefun = all_discarded_simul_efun;
            all_discarded_simul_efun = j;
        }

        id->u.global.sim_efun = I_GLOBAL_SEFUN_OTHER;
    }
} /* remove_efun_shadows() */

/*-------------------------------------------------------------------------*/
void
invalidate_simul_efuns (void)

/* Invalidate all simul_efun information - usually because the
 * object is destructed.
 */

{
    simul_efun_table_t *entry;
    int                 i;

    /* Invalidate the simul_efun table */
    for (entry = simul_efun_table, i = SIZE_SEFUN_TABLE; --i >= 0; )
    {
        entry->funstart = NULL;
        entry++;
    }

    /* Remove all sefun shadows for efuns and python efuns. */
    remove_efun_shadows(all_efuns);
#ifdef USE_PYTHON
    remove_efun_shadows(all_python_efuns);
#endif

    /* Mark all simulefun identifier entries as non-existing
     * and move them into the inactive list.
     */
    while (all_simul_efuns != NULL)
    {
        ident_t* id = all_simul_efuns;
        int j = id->u.global.sim_efun;

        all_simul_efuns = all_simul_efuns->next_all;

        if (j != I_GLOBAL_SEFUN_BY_NAME)
        {
            simul_efun_table[j].function.offset.next_sefun = all_discarded_simul_efun;
            all_discarded_simul_efun = j;
        }

        free_shared_identifier(id);
    }

    /* Free the old program and vector, if any */
    if (simul_efun_program)
    {
        free_prog(simul_efun_program, MY_TRUE);
        simul_efun_program = NULL;
    }

    if (simul_efun_vector)
    {
        free_array(simul_efun_vector);
        simul_efun_vector = NULL;
    }
} /* invalidate_simul_efuns() */

/*-------------------------------------------------------------------------*/
Bool
assert_simul_efun_object (void)

/* (Re)load the simul_efun object and extract all information we need.
 * Result is TRUE if either the simul_efun object could be loaded, or if
 * master::get_simul_efun() did not return a string/string vector to
 * name the simul efun object. The result is FALSE if master::get_simul_efun()
 * specified a simul efun object, which couldn't be found.
 *
 * In other words: after calling assert_simul_efun_object(), the caller
 * still has to check if simul_efun_object is NULL.
 *
 * At the time of call, simul_efun_object must be NULL.
 */

{
    svalue_t           *svp;
    object_t           *ob;
    program_t          *progp;
    CBool              *visible; /* Flag for every function: visible or not */
    string_t           *name;
    int                 i, j;

    invalidate_simul_efuns(); /* Invalidate the simul_efun information */

    free_defines(); /* to prevent #defines hideing places for globals */

    /* Get the name(s) of the simul_efun  object. */
    svp = apply_master(STR_GET_SEFUN, 0);

    /* If a simul_efun_object appears during the GET_SEFUN call, it
     * might have been due to a recursive get_simul_efun() call which may
     * have gotten an old backup copy. This can lead to hard-to-debug
     * variable and function definition inconsistencies.
     */
    if (simul_efun_object)
    {
        printf("%s simul_efun object appeared while asking for it.\n", time_stamp());
        return MY_TRUE;
    }

    if (svp == NULL)
    {
        printf("%s No simul_efun\n", time_stamp());
        return MY_TRUE;
    }

    if (svp->type == T_POINTER)
    {
        simul_efun_vector = svp->u.vec;
        svp->type = T_NUMBER;
        if (VEC_SIZE(svp->u.vec))
            svp = svp->u.vec->item;
    }

    if (svp->type != T_STRING)
    {
        printf("%s No simul_efun\n", time_stamp());
        return MY_TRUE;
    }

    /* Make the (primary) simul_efun name */
    name = del_slash(svp->u.str);
    if (simul_efun_file_name)
        free_mstring(simul_efun_file_name);
    simul_efun_file_name = make_tabled(name);

    /* Get the object and load the program */
    ob = find_object(simul_efun_file_name);
    if (ob == NULL)
    {
        fprintf(stderr, "%s The simul_efun file %s was not loaded.\n"
               , time_stamp(), get_txt(simul_efun_file_name));
        fprintf(stderr, "%s The function get_simul_efun() in the master must load it.\n"
               , time_stamp());
        return MY_FALSE;
    }
    if (O_PROG_SWAPPED(ob) && load_ob_from_swap(ob) < 0)
    {
        fprintf(stderr, "%s Out of memory (unswap object '%s') ==> "
                        "No simul_efun\n", time_stamp(), get_txt(ob->name));
        return MY_TRUE;
    }
    reference_prog( (simul_efun_program = ob->prog), "get_simul_efun");

    if (ob->prog->num_function_names == 0)
        return MY_TRUE;

    free_defines(); /* to prevent #defines hideing places for globals */

    /* locals and defines are freed now. There are still reserved words,
     * but it is impossible to define a function with the name being
     * a reserved word, thus, there will be no clashes with higher-priority
     * shared identifiers.
     */

    progp = ob->prog;
    visible = alloca((i = ob->prog->num_functions) * sizeof(*visible));
    memset(visible, 0, i);
    i = ob->prog->num_function_names;
    while (--i >= 0)
        visible[progp->function_names[i]] = MY_TRUE;
    /* The functions .num_function_names+1 .. .num_functions are not
     * visible by definition.
     */

    /* Loop over the functions in the simul_efun object and
     * copy the salient information.
     */
    for (i = 0; i < ob->prog->num_functions; i++)
    {
        int        ix;
        funflag_t  flags, flags2;
        bytecode_p funstart;
        mp_int     fun_ix_offs, var_ix_offs;
        program_t *inherit_progp;
        function_t*funheader;

        if (!visible[i])
            continue;

        ix = i;
        flags2 = flags = progp->functions[ix];
        flags &= ~FUNSTART_MASK;

        /* Pinpoint the function, resolving inheritance where
         * necessary.
         */
        fun_ix_offs = ix;
        var_ix_offs = progp->num_virtual_variables;
        inherit_progp = progp;
        while (flags2 & NAME_INHERITED)
        {
            inherit_t *inheritp;

            inheritp = &inherit_progp->inherit[flags2 & INHERIT_MASK];
            ix -= inheritp->function_index_offset;

            if (inheritp->inherit_type != INHERIT_TYPE_NORMAL)
                var_ix_offs = 0;
            var_ix_offs += inheritp->variable_index_offset;

            inherit_progp = inheritp->prog;
            flags2 = inherit_progp->functions[ix];
        }
        fun_ix_offs -= ix;

        funstart = inherit_progp->program + (flags2 & FUNSTART_MASK);
        funheader = inherit_progp->function_headers + FUNCTION_HEADER_INDEX(funstart);

        /* Don't stumble over undefined functions */
        if (is_undef_function(funstart))
        {
            flags |= NAME_UNDEFINED;
        }

        /* If the function is __INIT, pretend it's a private function */
        if ( !(flags & (TYPE_MOD_STATIC|TYPE_MOD_PRIVATE|NAME_UNDEFINED)) )
        {
            if (mstreq(funheader->name, STR_VARINIT))
                flags |= TYPE_MOD_PRIVATE;
        }

        /* If the function is indeed visible, get its information */
        if ( !(flags & (TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|TYPE_MOD_PRIVATE|NAME_UNDEFINED)) )
        {
            string_t *function_name;
            ident_t *p;
            unsigned short type_idx;
            unsigned char num_arg;

            function_name = funheader->name;
            num_arg = funheader->num_arg;

            /* Find or make the identifier for the function */
            p = make_shared_identifier_mstr(function_name, I_TYPE_GLOBAL, 0);
            if (p->type == I_TYPE_UNKNOWN)
            {
                init_global_identifier(p, /* bVariable: */ MY_FALSE);
                p->next_all = all_simul_efuns;
                all_simul_efuns = p;
            }

            /* Find the proper index in simul_efun_table[] */
            switch(0) { default: /* TRY... */

                /* Try to find a discarded sefun entry with matching
                 * name, number of arguments and XVARARGS flag to reuse.
                 */
                if (all_discarded_simul_efun != I_GLOBAL_SEFUN_OTHER)
                {
                    int last = I_GLOBAL_SEFUN_OTHER;
                    bool needs_ap = (flags & (TYPE_MOD_VARARGS|TYPE_MOD_XVARARGS)) || funheader->num_opt_arg;

                    j = all_discarded_simul_efun;
                    do
                    {
                        /* We reuse a discarded entry if already compiled
                         * programs are compatible with that. This means:
                         *  - The name didn't change.
                         *  - The requirement of an argument frame didn't change.
                         *  - For calls without an argument frame the number of
                         *    arguments didn't change.
                         */
                        if (!mstreq(function_name, simul_efun_table[j].function.name))
                            continue;

                        if (needs_ap != ((simul_efun_table[j].function.flags & (TYPE_MOD_VARARGS|TYPE_MOD_XVARARGS)) || simul_efun_table[j].function.num_opt_arg))
                            continue;

                        if (!needs_ap && num_arg != simul_efun_table[j].function.num_arg)
                            continue;

                        /* Found one: remove it from the 'discarded' list */
                        if (last == I_GLOBAL_SEFUN_OTHER)
                            all_discarded_simul_efun = simul_efun_table[j].function.offset.next_sefun;
                        else
                            simul_efun_table[last].function.offset.next_sefun = simul_efun_table[j].function.offset.next_sefun;
                        break;
                    }
                    while ( (j = simul_efun_table[last = j].function.offset.next_sefun) != I_GLOBAL_SEFUN_OTHER);

                    if (j != I_GLOBAL_SEFUN_OTHER)
                        break; /* switch */
                }

                /* New simul_efun: make a new entry */
                if (num_simul_efun == SEFUN_TABLE_SIZE)
                {
                    p->u.global.sim_efun = I_GLOBAL_SEFUN_BY_NAME;
                    continue;
                }

                j = num_simul_efun++;
                simul_efun_table[j].function.name    = ref_mstring(function_name);
            } /* switch() */

            /* j now indexes the simul_efun_table[] entry to use */

            p->u.global.sim_efun = j;
            simul_efun_table[j].function.flags       = funheader->flags;
            simul_efun_table[j].function.type        = funheader->type;
            simul_efun_table[j].function.num_locals  = funheader->num_locals;
            simul_efun_table[j].function.num_arg     = num_arg;
            simul_efun_table[j].function.num_opt_arg = funheader->num_opt_arg;

            if (inherit_progp->type_start
             && (type_idx = inherit_progp->type_start[funheader->offset.fx]) != INDEX_START_NONE)
                simul_efun_table[j].function.offset.argtypes = inherit_progp->argument_types + type_idx;
            else
                simul_efun_table[j].function.offset.argtypes = NULL;

            simul_efun_table[j].funstart = funstart;
            simul_efun_table[j].program = inherit_progp;
            simul_efun_table[j].function_index_offset = fun_ix_offs;
            simul_efun_table[j].variable_index_offset = var_ix_offs;
        } /* if (function visible) */
    } /* for ( all functions) */

    simul_efun_object = ob;

    return MY_TRUE;
} /* assert_simul_efun_object() */

/*-------------------------------------------------------------------------*/
string_t *
query_simul_efun_file_name(void)

/* Return the name of the primary simul_efun object.
 * Result is a tabled string, but no extra reference is added.
 */

{
#ifdef DEBUG
    if (simul_efun_file_name == NULL)
        fatal("query_simul_efun_file_name called when non exists!\n");
#endif
    return simul_efun_file_name;
}

/*-------------------------------------------------------------------------*/
function_t *
get_simul_efun_header (ident_t* name)

/* Return the function header for the simul-efun <name>.
 * <name> must be a valid simul-efun identifier (i.e. there
 * must be a simul-efun object with such a function).
 */

{
    assert(name->u.global.sim_efun != I_GLOBAL_SEFUN_OTHER);

    if (name->u.global.sim_efun == I_GLOBAL_SEFUN_BY_NAME)
    {
        /* We have to lookup the function in the simul-efun object itself. */
        assert(simul_efun_object != NULL);

        int fx = find_function(name->name, simul_efun_object->prog);
        if (fx == -1)
            fatal("Can't find simul_efun %s", get_txt(name->name));

        return get_function_header(simul_efun_object->prog, fx);
    }
    else
    {
        return &simul_efun_table[name->u.global.sim_efun].function;
    }
} /* get_simul_efun_header() */

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT

void
clear_simul_efun_refs (void)

/* GC support: clear the references of all memory held by the module.
 */

{
    if (simul_efun_vector && simul_efun_vector->ref)
    {
        simul_efun_vector->ref = 0;
        clear_ref_in_vector(
          simul_efun_vector->item,
          VEC_SIZE(simul_efun_vector)
        );
    }
    if (simul_efun_program)
        simul_efun_program->ref = 0;
} /* clear_simul_efun_refs() */

/*-------------------------------------------------------------------------*/
void
count_simul_efun_refs (void)

/* GC support: count the references of all memory held by the module.
 */

{
    if (simul_efun_file_name)
        count_ref_from_string(simul_efun_file_name);

    for (int i = num_simul_efun; --i >= 0; )
        count_ref_from_string(simul_efun_table[i].function.name);

    if (simul_efun_vector && !simul_efun_vector->ref++)
    {
        note_malloced_block_ref((char *)simul_efun_vector);
        count_ref_in_vector(
          simul_efun_vector->item,
          VEC_SIZE(simul_efun_vector)
        );
    }
    if (simul_efun_program)
        mark_program_ref(simul_efun_program);
} /* count_simul_efun_refs() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

void
count_simul_efun_extra_refs (struct pointer_table *ptable)

/* DEBUG support: count the extra refs for structures of this module.
 */

{
    if (simul_efun_vector)
    {
        simul_efun_vector->extra_ref++;
        if (NULL != register_pointer(ptable, simul_efun_vector) )
            count_extra_ref_in_vector(
              simul_efun_vector->item,
              VEC_SIZE(simul_efun_vector)
            );
    }

    if (simul_efun_program)
    {
        simul_efun_program->extra_ref++;
        if (NULL == register_pointer(ptable, simul_efun_program))
            return;
        simul_efun_program->extra_ref = 1;
        count_inherits(simul_efun_program);
    }
} /* count_simul_efun_extra_refs() */

#endif

/***************************************************************************/
