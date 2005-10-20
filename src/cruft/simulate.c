/*--------------------------------------------------------------------*/
#ifdef MALLOC_smalloc
#if 0

/* Used for debugging smalloc */
/* TODO: Make this a proper 'DEBUG_SMALLOC' option, accessible through
 * TODO:: debug_info() or even malloc_debug(), and with proper validity
 * TODO:: checks through privilege_violation() instead of player_level().
 */

int first_showsmallnewmalloced_call = 1;

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
#endif
#endif

From special_parse():

#if 0
        /* Used to debug smalloc, but the corresponding code
         * in smalloc.c is if-0-ed out, too.
         */
#if defined(MALLOC_malloc) || defined(MALLOC_smalloc)
        if (strcmp(buff,  "showsmallnewmalloced") == 0) {

#if !defined(DEBUG) || defined(SHOWSMALLNEWMALLOCED_RESTRICTED)
            struct svalue *arg;
            push_volatile_string("inspect memory");
            arg = apply_master_ob(STR_VALID_INSPECT_MEMORY, 1);
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
#endif

