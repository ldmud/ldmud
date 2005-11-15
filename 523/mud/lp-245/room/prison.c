short() {
    return "The local prison";
}

long() {
    write("You are in the local prison.\n");
    write("There are no exits.\n");
}

reset(arg) {
    if (arg)
	return;
    set_light(1);
}

init() {
    add_action("quit", "quit");
}

quit() { return 1; }
