/*
 * This is a generic valuable object. Clone a copy, and
 * setup local values.
 */

/*
 * If you are going to copy this file, in the purpose of changing
 * it a little to your own need, beware:
 *
 * First try one of the following:
 *
 * 1. Do clone_object(), and then configur it. This object is specially
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

string short_desc, long_desc;
int value, local_weight;
string name, alias_name;
string read_msg;
string info;

id(str)
{
    return str == name || str == alias_name;
}

short() {
    return short_desc;
}

long() {
    write(long_desc);
}

query_value() { return value; }

set_id(str) {
    local_weight = 1;
    name = str;
}

set_alias(str) {
    alias_name = str;
}

set_short(str) {
    short_desc = str;
    long_desc = "You see nothing special.\n";
}

set_long(str) {
    long_desc = str;
}

set_value(v) {
    value = v;
}

set_weight(w) {
    local_weight = w;
}

set_read(str) {
    read_msg = str;
}

set_info(i) {
    info = i;
}

query_info() {
    return info;
}

get() {
    return 1;
}

query_weight() {
    return local_weight;
}

init() {
    if (!read_msg)
	return;
    add_action("read", "read");
}

read(str) {
    if (str != name &&  str != alias_name)
	return 0;
    write(read_msg);
    return 1;
}
