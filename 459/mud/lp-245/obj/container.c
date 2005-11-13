int value, max_weight, local_weight;
string name_of_container ,cap_name ,alt_name ,alias_name;
string short_desc, long_desc;
string read_msg;

long() {
    write(long_desc);
    if (first_inventory(this_object()))
	write("There is something in it.\n");
    else
	write("You can put things in it.\n");
}

reset(arg) {
    if (arg)
	return;
    local_weight = 0;
}

query_weight() { return local_weight; }

query_max_weight() { return max_weight; }

add_weight(w) {
    if (local_weight + w > max_weight)
	return 0;
    local_weight += w;
    return 1;
}

short() { return short_desc; }

id(str) {
   return str == name_of_container || str == alt_name || str == alias_name;
}

query_value() { return value; }

can_put_and_get() { return 1; }

get() { return 1; }

prevent_insert() {
    if (local_weight > 0) {
	write("You can't when there are things in the " + name_of_container + ".\n");
	return 1;
    }
    return 0;
}

set_weight(w) { local_weight = w; }

set_max_weight(w) { max_weight = w; }

set_value(v) { value = v; }

set_name(n) {
    name_of_container = n;
    cap_name = capitalize(n);
    short_desc = cap_name;
    long_desc = cap_name +"\n";
}

set_alt_name(n) { alt_name = n; }

set_alias(n) { alias_name = n; }

set_short(sh) { short_desc = sh; long_desc = short_desc + "\n"; }

set_long(lo) { long_desc = lo; }

set_read(str) {
    read_msg = str;
}

init() {
    if (!read_msg)
        return;
    add_action("read", "read");
}

read(str) {
    if (str != name_of_container &&  str != alt_name && str != alias_name)
        return 0;
    write(read_msg);
    return 1;
}
