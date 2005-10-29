/*
 * This is a generic valuable object. Clone a copy, and
 * setup local values.
 */

string short_desc, long_desc;
int value, local_weight;
string name, alias_name;
string read_msg;

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

get() {
    return 1;
}

query_weight() {
    return local_weight;
}

init() {
    if (!read_msg)
	return;
    add_action("read"); add_verb("read");
}

read(str) {
    if (str != name &&  str != alias_name)
	return 0;
    write(read_msg);
    return 1;
}
