#if 0
/*
 * For debugging purposes.
 */
void check_ob_ref(ob, from)
    struct object *ob;
    char *from;
{
    struct object *o;
    int i;

    for (o = obj_list, i=0; o; o = o->next_all) {
        if (o->flags & O_DESTRUCTED)
            continue;
        if (o->inherit == ob)
            i++;
    }
    if (i+1 > ob->ref) {
        fatal("FATAL too many references to inherited object %s (%d) from %s.\n",
              ob->name, ob->ref, from);
        if (current_object)
            fprintf(stderr, "current_object: %s\n", current_object->name);
        for (o = obj_list; o; o = o->next_all) {
            if (o->inherit != ob || o->flags & O_DESTRUCTED)
                continue;
            fprintf(stderr, "  %s\n", ob->name);
        }
    }
}
#endif /* 0 */


#if 0
int shadow_catch_message(ob, str)
    struct object *ob;
    char *str;
{
    if (!ob->shadowed)
        return 0;
    while(ob->shadowed != 0 && ob->shadowed != current_object)
        ob = ob->shadowed;
    while(ob->shadowing) {
        if (function_exists("catch_tell", ob))
        {
            push_volatile_string(str);
            if (apply(STR_CATCH_TELL, ob, 1)) /* this will work, since we know the */
                /* function is defined */
                return 1;
        }
        ob = ob->shadowing;
    }
    return 0;
}
#endif /* 0 */

