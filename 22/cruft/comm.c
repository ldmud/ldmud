/*-------------------------------------------------------------------------*/
#if 0 /* Outdated */
/*
 * Let object 'me' snoop object 'you'. If 'you' is 0, then turn off
 * snooping.
 */
void set_snoop(me, you)
    struct object *me, *you;
{
    struct interactive *on = 0, *by = 0, *tmp;
    int i;

    if (me->flags & O_DESTRUCTED)
        return;
    if (you && (you->flags & O_DESTRUCTED))
        return;
    for(i=0; i<MAX_PLAYERS && (on == 0 || by == 0); i++) {
        if (all_players[i] == 0)
            continue;
        if (all_players[i]->ob == me)
            by = all_players[i];
        else if (all_players[i]->ob == you)
            on = all_players[i];
    }
    if (you == 0) {
        if (by == 0)
            error("Could not find myself to stop snoop.\n");
        add_message("Ok.\n");
        if (by->snoop_on == 0)
            return;
        by->snoop_on->snoop_by = 0;
        by->snoop_on = 0;
        return;
    }
    if (on == 0 || by == 0) {
        add_message("Failed.\n");
        return;
    }
    if (by->snoop_on) {
        by->snoop_on->snoop_by = 0;
        by->snoop_on = 0;
    }
    if (on->snoop_by) {
        add_message("Busy.\n");
        return;
    }
    /*
     * Protect against snooping loops.
     */
    for (tmp = on; tmp; tmp = tmp->snoop_on) {
        if (tmp == by) {
            add_message("Busy.\n");
            return;
        }
    }
    on->snoop_by = by;
    by->snoop_on = on;
    add_message("Ok.\n");
    return;
}
#endif


