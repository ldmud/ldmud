object listen_ob, player_ob;

long() {
    write("A stethoscope.\n");
}

short() {
    return "A stethoscope";
}

query_weight() {
    return 1;
}

query_value() {
    return 15;
}

init() {
    add_action("apply", "apply");
    add_action("apply", "use");
    add_action("listen", "listen");
}

listen(str) {
    write("You must apply stethoscope to something.\n");
    return 1;
}

apply(str) {
    string what;
    object ob;

    if (!str)
	return 0;
    if (environment() != this_player()) {
	write("You must have the stethoscope on you to use it.\n");
	return 1;
    }
    if (id(str) || sscanf(str, "stethoscope to %s", what) != 1) {
	write("On what ?\n");
	return 1;
    }
    ob = present(what, this_player());
    if (!ob)
	ob = present(what, environment(this_player()));
    if (!ob)
	return 0;
    if (living(ob) || ob->use_stethoscope(this_object())) {
	write("You listen to the " + what + ".\n");
	listen_ob = ob;
	player_ob = this_player();
	set_heart_beat(1);
	return 1;
    }
    return 0;
}

/*
 * Detect if the playe leaves the object.
 */
heart_beat() {
    if (!present(listen_ob,environment(player_ob)) ||
	environment() != player_ob) {
	listen_ob = 0;
	set_heart_beat(0);
	return;
    }
    if (living(listen_ob))
	tell_object(player_ob, "Dunk dunk\n");
}

query_listening() {
    return listen_ob;
}

get() {
    return 1;
}

id(str) {
    return str == "stethoscope";
}
