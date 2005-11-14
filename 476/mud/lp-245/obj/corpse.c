/*
 * This is a decaying corpse. It is created automatically
 * when a player or monster die.
 */

#define DECAY_TIME	120

string name;
int decay;

prevent_insert() {
    write("The corpse is too big.\n");
    return 1;
}

init() {
    add_action("search", "search");
}

reset(arg) {
    if (arg)
	return;
    name = "noone";
    decay = 2;
}

set_name(n)
{
    name = n;
    call_out("decay", DECAY_TIME);
}

short() {
    if (decay < 2)
	return "The somewhat decayed remains of " + capitalize(name);
    return "Corpse of " + capitalize(name);
}

long() {
    write("This is the dead body of " + capitalize(name) + ".\n");
}

id(str) {
    return str == "corpse" || str == "corpse of " + name ||
	str == "remains";
}

decay()
{
    decay -= 1;
    if (decay > 0) {
	call_out("decay", 20);
	return;
    }
    destruct(this_object());
}

can_put_and_get() { return 1; }

search(str)
{
    object ob;
    if (!str || !id(str))
	return 0;
    ob = present(str, environment(this_player()));
    if (!ob)
	ob = present(str, this_player());
    if (!ob)
	return 0;
    write("You search " + str + ", and find:\n");
    say(this_player()->query_name() + " searches " + str + ".\n");
    if (!search_obj(ob))
	write("\tNothing.\n");
    else
	write("\n");
    return 1;
}

search_obj(cont)
{
    object ob;
    int total;

    if (!cont->can_put_and_get())
	return 0;
    ob = first_inventory(cont);
    while(ob) {
	total += 1;
	write(ob->short() + ", ");
	ob = next_inventory(ob);
    }
    return total;
}

get() {
    return 1;
}

query_weight() {
    return 5;
}
