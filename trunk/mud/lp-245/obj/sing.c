/*
 * This is a singularity. It is used when an error occurs in the
 * game driver, as a stand in for other objects.
 */

string short() {
    return "A singularity";
}

void long() {
    write("This is a singularity. It is created in the space from\n");
    write("another creation failure.\n");
    write("if you find it somewhere, just being here, call a wizard,\n");
    write("and he will remove it. The singularity has no affects on the\n");
    write("game, otherwise.\n");
}

int id(string str) {
    return str == "singularity";
}

int get() { return 1; }

int query_value() { return 1; }
