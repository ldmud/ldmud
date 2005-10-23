object mark;

void init() {
    add_action("i_dump", "i_dump");
    add_action("i_mark", "i_mark");
    add_action("i_destruct", "i_destruct");
    add_action("i_call", "i_call");
}

int i_dump(string str) {
    int tmp;
    if (mark == 0) {
	write("Nothing marked.\n");
	return 1;
    }
    if (str == "list") {
	object ob;
	int i;
	ob = first_inventory(mark);
	while(ob) {
	    i += 1;
	    write(i + ":\t");
	    write(ob);
	    write("\t" + ob->short() + "\n");
	    ob = next_inventory(ob);
	}
	return 1;
    }
    write(mark->short());
    if (living(mark))
	write("(living)\n");
    if (mark->query_npc())
	write("(NPC)");
    write("\n");
    tmp = mark->query_value();
    if (tmp)
	write("Value:\t" + tmp + "\n");
    tmp = mark->query_weight();
    if (tmp)
	write("Weight:\t" + tmp + "\n");
    return 1;
}

int i_mark(string str) {
    mixed tmp;

    if (str == 0)
	return 0;
    if (sscanf(str, "living %s", tmp) == 1) {
	mark = find_living(tmp);
	if (!mark) {
	    write("No such living object.\n");
	    return 1;
	}
	write(mark); write("\n");
	return 1;
    }
    if (sscanf(str, "ob %s", tmp) == 1) {
	mark = find_object(tmp);
	if (!mark) {
	    write("No such object.\n");
	    return 1;
	}
	write(mark); write("\n");
	return 1;
    }
    if (str == "up") {
	if (mark == 0)
	    return 0;
	if (environment(mark) == 0)
	    return 0;
	mark = environment(mark);
	write(mark); write("\n");
	return 1;
    }
    if (str == "here") {
	mark = environment(this_player());
	write(mark); write("\n");
	return 1;
    }
    if (sscanf(str, "%d", tmp) == 1) {
	object ob;
	if (mark == 0)
	    return 0;
	ob = first_inventory(mark);
	while(tmp > 1) {
	    tmp -= 1;
	    if (ob == 0)
		return 0;
	    ob = next_inventory(ob);
	}
	if (ob == 0)
	    return 0;
	mark = ob;
	write(mark); write("\n");
	return 1;
    }
    return 0;
}

int id(string str) {
    return str == "marker";
}

string short() {
    return "Lars marker object";
}

void long() {
    write("Commands available:\n");
    write("i_mark living 'name'\n");
    write("i_mark here\n");
    write("i_mark up\n");
    write("i_dump list\n");
    write("i_dump\n");
    write("i_destruct\n");
    write("i_call 'function'\n");
    write("i_call 'function' 'argument'\n");
}

voidget() { return 1;}

int query_value() {
    return 10;
}

int i_destruct() {
    object ob;
    if (!mark)
	return 0;
    ob = environment(mark);
    destruct(mark);
    write("Ok. New: "); write(ob); write("\n");
    mark = ob;
    return 1;
}

int i_call(string str) {
    string with, what;
    int iwhat;

    if (!str)
        return 0;
    if (!mark) {
	write("No object marked.\n");
	return 1;
    }
    if (sscanf(str, "%s %d", with, what) == 2)
        iwhat = 1;
    else if (sscanf(str, "%s %s", with, what) != 2) {
	if (sscanf(str, "%s", with) == 1)
	    iwhat = 0;
	else
	    return 0;
    }
    write("Got: "); write(call_other(mark, with, what)); write("\n");
    say(this_player()->query_name() +
	" patched the internals of " + mark->short() + ".\n");
    return 1;
}
