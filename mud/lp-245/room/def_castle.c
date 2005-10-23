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

/* In the blueprint, both NAME and DEST are undefined. To make it
 * possible to check the blueprint for errors, provide default
 * definitions.
 */
#ifndef NAME
#  define NAME "Nobody"
#endif

#ifndef DEST
#  define DEST "/room/church"
#endif

int id(string str) { return str == "castle"; }

string short() {
    return "Castle of " + NAME;
}

void long() {
    write("This is the " + short() + ".\n");
    write(NAME + " is a rather new wizard, but it is an amazing castle\n");
    write("just the same. However, the gates are closed.\n");
}

void init() {
    add_action("enter", "enter");
}

int enter(string str) {
    if (!id(str))
	return 0;
    write("It is not an open castle.\n");
    return 1;
}

void reset(int arg) {
    if (arg)
	return;
    move_object(this_object(), DEST);
}
