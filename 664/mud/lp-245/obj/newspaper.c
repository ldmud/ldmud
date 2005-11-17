short() {
    return "A newspaper" ;
}

long() {
    cat("/NEWSPAPER");
}

init() {
    add_action("read", "read");
}

id(str) {
    return str == "newspaper" || str == "paper" || str == "news";
}

read(str) {
    if (!id(str))
	return 0;
    say(this_player()->query_name() + " reads the newspaper.\n");
    long();
    return 1;
}

query_weight() { return 1; }

get() { return 1; }

query_value() { return 5; }
