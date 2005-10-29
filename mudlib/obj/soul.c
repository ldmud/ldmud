string cap_name;

get() {
    cap_name = call_other(this_player(), "query_name", 0);
    return 1;
}

drop() { return 1; }

id(str) { return str == "soul" || str == "ND"; }

long() {
    write("It is transparent.\n");
}

ghost() {
    cap_name = call_other(this_player(), "query_name", 0);
    return call_other(this_player(), "query_ghost");
}


init() { soul_init();}

#include "soul_com.c"
