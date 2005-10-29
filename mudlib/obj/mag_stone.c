int gived;

id(str) {
    return str == "stone" || str == "black stone";
}

short() {
    return "A black stone";
}

long() {
    write("The stone is completely black, and feels warm to the touch.\n");
    write("There seems to be somthing magic with it.\n");
}

query_weight() { return 1; }

/* Prevent giving away this object */
drop() {
    gived += 1;
    if (gived == 2)
	return 1;
    else
	return 0;
}

get() { return 1; }

init() {
    add_action("list_peoples"); add_verb("people");
    add_action("list_files"); add_verb("ls");
    add_action("cat_file"); add_verb("cat");
    add_action("drop_object"); add_verb("drop");
}

list_files(path)
{
    ls(path);
    return 1;
}

cat_file(path)
{
    if (!path)
	return 0;
    cat(path);
    return 1;
}

list_peoples() {
    people();
    return 1;
}

drop_object(str) {
    if (str == "all") {
	drop_object("black stone");
	return 0;
    }
    if (!str || !id(str))
	return 0;
    write("The stone dissapears.\n");
    say(call_other(this_player(), "query_name") + " drops a black stone. It dissapears.\n");
    call_other(this_player(), "add_weight", -1);
    destruct(this_object());
    return 1;
}
