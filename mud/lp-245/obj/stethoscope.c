object listen_ob, player_ob;

void long() {
    write("A stethoscope.\n");
}

string short() {
    return "A stethoscope";
}

int id(string str) {
    return str == "stethoscope";
}

int query_weight() {
    return 1;
}

int query_value() {
    return 15;
}

void init() {
    add_action("apply", "apply");
    add_action("apply", "use");
    add_action("listen", "listen");
}

int listen(string str) {
    write("You must apply stethoscope to something.\n");
    return 1;
}

int apply(string str) {
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
void heart_beat() {
    if (!present(listen_ob,environment(player_ob)) ||
	environment() != player_ob) {
	listen_ob = 0;
	set_heart_beat(0);
	return;
    }
    if (living(listen_ob))
	tell_object(player_ob, "Dunk dunk\n");
}

object query_listening() {
    return listen_ob;
}

int get() {
    return 1;
}
