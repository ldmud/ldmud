int gived;

int id(string str) {
    return str == "stone" || str == "black stone";
}

string short() {
    return "A black stone";
}

void long() {
    write("The stone is completely black, and feels warm to the touch.\n");
    write("There seems to be somthing magic with it.\n");
}

int query_weight() { return 1; }

/* Prevent giving away this object */
int drop() {
    gived += 1;
    if (gived == 2)
	return 1;
    else
	return 0;
}

int get() { return 1; }

void init() {
    add_action("list_peoples", "people");
    add_action("list_files", "ls");
    add_action("cat_file", "cat");
    add_action("drop_object", "drop");
}

int list_files(string path)
{
    ls(path);
    return 1;
}

int cat_file(string path)
{
    if (!path)
	return 0;
    cat(path);
    return 1;
}

int list_peoples() {
    object * list;
    int i, a;

    list = users();
    write("There are now " + sizeof(list) + " players");
    for (i=0, a=0; i < sizeof(list); i++)
	if (query_idle(list[i]) >= 5 * 60)
	    a++;
    if (a)
	write(" (" + (sizeof(list) - a) + " active)");
    write(". " + query_load_average() + "\n");
    for(i=0; i<sizeof(list); i++) {
	string name;
	name = list[i]->query_real_name();
	if (!name)
	    name = list[i]->query_name();
	if (!name)
	    name = "logon";
	name = capitalize(name);
	if (list[i]->short() == 0)
	    name = "(" + name + ")";
	if (strlen(name) < 8)
	    name = name + "\t";
	write(query_ip_number(list[i]) + "\t" + name + "\t" +
	      list[i]->query_level() + "\t");
	a = list[i]->query_age();
	if (a / 43200 > 9)
	    write(a / 43200 + " D");
	else if (a / 43200 > 0)
	    write(a / 43200 + "  D");
	else if (a / 1800 > 9)
	    write(a / 1800 + " h");
	else if (a / 1800 > 0)
	    write(a / 1800 + "  h");
	else if (a / 30 > 9)
	    write(a / 30 + " m");
	else
	    write(a / 30 + "  m");
	if (query_idle(list[i]) >= 5 * 60)
	    write(" I\t");
	else
	    write("\t");
	if (environment(list[i]))
	    write(object_name(environment(list[i])));
	write("\n");
    }
    return 1;
}

int drop_object(string str) {
    if (str == "all") {
	drop_object("black stone");
	return 0;
    }
    if (!str || !id(str))
	return 0;
    write("The stone dissapears.\n");
    say(this_player()->query_name() + " drops a black stone. It dissapears.\n");
    this_player()->add_weight(-1);
    destruct(this_object());
    return 1;
}
