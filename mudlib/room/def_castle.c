/*
 * This is just the facade to a castle. If you want to enable the
 * "enter" command, move the player to a hall or something (which
 * you have to design yourself).
 * The predefined string DEST is where a player should come when he
 * leaves the castle.
 *
 * This file is loaded automatically from "init_file". We have to move
 * ourself to where we are supposed to be.
 */

id(str) { return str == "castle"; }

short() {
    return "Castle of " + NAME;
}

long() {
    write("This is the " + short() + ".\n");
    write(NAME + " is a rather new wizard, but it is an amazing castle\n");
    write("just the same. However, the gates are closed.\n");
}

init() {
    add_action("enter"); add_verb("enter");
}

enter(str) {
    if (!id(str))
	return 0;
    write("It is not an open castle.\n");
    return 1;
}

reset(arg) {
    if (arg)
	return;
    move_object(this_object(), DEST);
}
