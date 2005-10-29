string short_desc, name, message;
int value, strength, heal;
int full;
int pub_drink;		/* Drinks from the pub can't be removed from pub */

set_value(str) {
    if (sscanf(str, "%s#%s#%s#%d#%d#%d", name, short_desc, message,
	heal, value, strength) != 6)
	    return 0;
    return 1;
}

set_pub() {
    pub_drink = 1;
}

prevent_insert() {
    if (full) {
	write("You would spill it out.\n");
	return 1;
    }
    return 0;
}

id(str) {
    if ((str == name || (str == "drk2" && pub_drink)) && full)
	return 1;
    return str == "bottle";
}

short() {
    if (full)
	return short_desc;
    return "empty bottle";
}

/* The shop only buys empty bottles ! */

query_value()
{
    if (!full) return 10;
    return 0;
}

long() {
    write(short() + ".\n");
}

reset(arg) {
    if (arg)
        return;
    full = 1;
}

drink(str)
{
    int level;
    string p_name;
    if (!str || !id(str))
	return 0;
    if (!full)
	return 0;
    level = call_other(this_player(), "query_level");
    p_name = call_other(this_player(), "query_name");
    if (strength == 12 && level < 10) {
	write("You sputter liquid all over the room.\n");
	say(p_name + " tries a " + name + " but coughs and sputters\n" +
	    "all over you.\n");
	full = 0;
	return 1;
    }
    if (strength == 8 && level < 5) {
	write("You throw it all up.\n");
	say(p_name + " tries to drink a " + name + " but throws up.\n");
	full = 0;
	return 1;
    }
    if (!call_other(this_player(), "drink_alcohol", strength))
	return 1;
    call_other(this_player(), "heal_self", heal);
    write(message + ".\n");
    say(call_other(this_player(), "query_name", 0) +
	" drinks " + name + ".\n");
    full = 0;
    return 1;
}

init() {
    add_action("drink"); add_verb("drink");
}

get() {
    return 1;
}

query_weight() {
    return 1;
}
