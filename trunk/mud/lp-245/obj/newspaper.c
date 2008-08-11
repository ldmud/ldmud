string short() {
    return "A newspaper" ;
}

void long() {
    cat("/NEWSPAPER");
}

void init() {
    add_action("read", "read");
}

int id(string str) {
    return str == "newspaper" || str == "paper" || str == "news";
}

int read(string str) {
    if (!id(str))
	return 0;
    say(this_player()->query_name() + " reads the newspaper.\n");
    long();
    return 1;
}

int query_weight() { return 1; }

int get() { return 1; }

int query_value() { return 5; }
