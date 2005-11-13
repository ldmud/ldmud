/*
 * This file defines a general purpose armour. See below for configuration
 * functions: set_xx.
 */

/*
 * If you are going to copy this file, in the purpose of changing
 * it a little to your own need, beware:
 *
 * First try one of the following:
 *
 * 1. Do clone_object(), and then configure it. This object is specially
 *    prepared for configuration.
 *
 * 2. If you still is not pleased with that, create a new empty
 *    object, and make an inheritance of this objet on the first line.
 *    This will automatically copy all variables and functions from the
 *    original object. Then, add the functions you want to change. The
 *    original function can still be accessed with '::' prepended on the name.
 *
 * The maintainer of this LPmud might become sad with you if you fail
 * to do any of the above. Ask other wizards if you are doubtful.
 *
 * The reason of this, is that the above saves a lot of memory.
 */

string name, alias, short_desc, long_desc, value, weight;
string type;
int worn, ac;
object worn_by;
object next;
string info;

reset(arg)
{
    if(arg)
	return;
    type = "armour";
}

link(ob)
{
    next = ob;
}

remove_link(str)
{
    object ob;

    if (str == name) {
	ob = next;
	next = 0;
	return ob;
    }
    if (next)
	next = next->remove_link(str);
    return this_object();
}

init() {
    add_action("wear", "wear");
    add_action("remove", "remove");
}

rec_short()
{
    if(next)
	return name + ", " + next->rec_short();
    return name;
}

short() {
    if (!short_desc)
	return 0;
    if (worn)
	return short_desc + " (worn)";
    return short_desc;
}

long(str) {
    write(long_desc);
}

id(str)
{
    return str == name || str == alias || str == type;
}

test_type(str)
{
    if(str == type)
	return this_object();
    if(next)
	return next->test_type(str);
    return 0;
}

tot_ac()
{
    if(next)
	return ac + next->tot_ac();
    return ac;
}

query_type() { return type; }

query_value() { return value; }

query_worn() { return worn; }

query_name() { return name; }

armour_class() { return ac; }

wear(str)
{
    object ob;

    if (!id(str))
	return 0;
    if (environment() != this_player()) {
	write("You must get it first!\n");
	return 1;
    }
    if (worn) {
	write("You already wear it!\n");
	return 1;
    }
    next = 0;
    ob = this_player()->wear(this_object());
    if(!ob) {
	worn_by = this_player();
	worn = 1;
	return 1;
    }
    write("You already have an armour of class " + type + ".\n");
write("Worn armour " + ob->short() + ".\n");
    return 1;
}

get() { return 1; }

drop(silently) {
    if (worn) {
	worn_by->stop_wearing(name);
	worn = 0;
	worn_by = 0;
	if (!silently)
	    tell_object(environment(this_object()),"You drop your worn armour.\n");
    }
    return 0;
}

remove(str) {
    if (!id(str))
	return 0;
    if (!worn) {
	return 0;
    }
    worn_by->stop_wearing(name);
    worn_by = 0;
    worn = 0;
    return 1;
}

query_weight() { return weight; }

set_id(n) { name = n; }
set_name(n) { name = n; }
set_short(s) { short_desc = s; long_desc = s + ".\n"; }
set_value(v) { value = v; }
set_weight(w) { weight = w; }
set_ac(a) { ac = a; }
set_alias(a) { alias = a; }
set_long(l) { long_desc = l; }
set_type(t) {
 type = t;
}
set_arm_light(l) { set_light(l); }
set_info(i) {
    info = i;
}

query_info() {
    return info;
}
