int value, max_weight, local_weight;
string name_of_container ,cap_name ,alt_name ,alias_name;
string short_desc, long_desc;
string read_msg;

void long() {
    write(long_desc);
    if (first_inventory(this_object()))
	write("There is something in it.\n");
    else
	write("You can put things in it.\n");
}

void reset(int arg) {
    if (arg)
	return;
    local_weight = 0;
}

int query_weight() { return local_weight; }

int query_max_weight() { return max_weight; }

int add_weight(int w) {
    if (local_weight + w > max_weight)
	return 0;
    local_weight += w;
    return 1;
}

string short() { return short_desc; }

int id(string str) {
   return str == name_of_container || str == alt_name || str == alias_name;
}

int query_value() { return value; }

int can_put_and_get() { return 1; }

int get() { return 1; }

int prevent_insert() {
    if (local_weight > 0) {
	write("You can't when there are things in the " + name_of_container + ".\n");
	return 1;
    }
    return 0;
}

void set_weight(int w) { local_weight = w; }

void set_max_weight(int w) { max_weight = w; }

void set_value(int v) { value = v; }

void set_name(string n) {
    name_of_container = n;
    cap_name = capitalize(n);
    short_desc = cap_name;
    long_desc = cap_name +"\n";
}

void set_alt_name(string n) { alt_name = n; }

void set_alias(string n) { alias_name = n; }

void set_short(string sh) { short_desc = sh; long_desc = short_desc + "\n"; }

void set_long(string lo) { long_desc = lo; }

void set_read(string str) {
    read_msg = str;
}

void init() {
    if (!read_msg)
        return;
    add_action("read", "read");
}

int read(string str) {
    if (str != name_of_container &&  str != alt_name && str != alias_name)
        return 0;
    write(read_msg);
    return 1;
}
