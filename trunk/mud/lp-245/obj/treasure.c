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

int id(string str)
{
    return str == name || str == alias_name;
}

string short() {
    return short_desc;
}

void long() {
    write(long_desc);
}

int query_value() { return value; }

void set_id(string str) {
    local_weight = 1;
    name = str;
}

void set_alias(string str) {
    alias_name = str;
}

void  set_short(string str) {
    short_desc = str;
    long_desc = "You see nothing special.\n";
}

void set_long(string str) {
    long_desc = str;
}

void set_value(int v) {
    value = v;
}

void  set_weight(int w) {
    local_weight = w;
}

void  set_read(string str) {
    read_msg = str;
}

void set_info(string i) {
    info = i;
}

string query_info() {
    return info;
}

int get() {
    return 1;
}

int query_weight() {
    return local_weight;
}

void init() {
    if (!read_msg)
	return;
    add_action("read", "read");
}

int read(string str) {
    if (str != name &&  str != alias_name)
	return 0;
    write(read_msg);
    return 1;
}
