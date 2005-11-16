id(str) {
    return str == "Go rules" || str == "rules";
}

short() {
    return "Go rules";
}

long() {
    write("A list of the rules for playing Go.\n");
    write("There are 5 rules.\n");
    write("Do 'rule #', to read a rule number.\n");
}

get() {
    write("The rules are attached to the floor!\n");
    return 0;
}

init() {
    add_action("rule", "rule");
}

rule(str) {
    int n;
    if (sscanf(str, "%d", n) != 1)
	return 0;
    if (n < 1 || n > 5) {
	write("Not that many rules.\n");
	return 1;
    }
    say(this_player()->query_name() + " reads rule " + n + "\n");
    cat("/obj/Go/rule" + n);
    if (n == 5)
	log_file("GO_RULES", this_player()->query_name() + "\n");
    return 1;
}
