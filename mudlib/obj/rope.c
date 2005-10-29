string tied_to;
object tied_to_ob;

id(str) {
    return str == "rope";
}

short() {
    if (tied_to)
        return "A rope tied to " + tied_to;
    return "A rope";
}

long() {
    write("You see nothing special about the rope.\n");
}

query_value() { return 15; }

get() {
    if (tied_to) {
        write("The rope is tied to " + tied_to + ".\n");
	return 0;
    }
    return 1;
}

query_weight() {
    return 1;
}

init() {
    add_action("tie"); add_verb("tie");
    add_action("untie"); add_verb("untie");
}

tie(str)
{
    string t1, t2;
    object ob;

    if (!str)
	return 0;
    if (tied_to) {
        write("It is already tied to " + tied_to + ".\n");
	return 1;
    }
    if (sscanf(str, "%s to %s", t1, t2) != 2)
        return 0;
    if (!id(t1))
	return 0;
    if (t2 == "me") {
        write("Why would you do that ?\n");
	return 1;
    }
    ob = present(t2, this_player());
    if (!ob)
	ob = present(t2, environment(this_player()));
    if (!ob) {
	if (call_other(environment(this_player()), "id", t2))
	    ob = environment(this_player());
    }
    if (!ob) {
	write("What ?\n");
	return 1;
    }
    if (!call_other(ob, "tie", t2)) {
        write("You can't tie the rope to " + t2 + ".\n");
	return 1;
    }
    /* Is he carrying the rope ? */
    if (environment() == this_player()) {
	move_object(this_object(), environment(this_player()));
	call_other(this_player(), "add_weight", - query_weight());
    }
    tied_to = t2;
    tied_to_ob = ob;
    write("Ok.\n");
    say(call_other(this_player(), "query_name") + " ties rope to " +
	t2 + ".\n");
    return 1;
}

untie(str) {
    if (!id(str))
        return 0;
    if (!tied_to) {
        write("It is not tied to anything.\n");
	return 1;
    }
    if (!call_other(tied_to_ob, "untie")) {
        write("You fail.\n");
	return 1;
    }
    write("Ok.\n");
    tied_to = 0;
    tied_to_ob = 0;
    return 1;
}
