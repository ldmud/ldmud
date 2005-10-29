int exit_num;

short() {
    return "A maze";
}

long() {
    write("In a maze.\n");
    write("There are four obvious exits: north, south, west and east.\n");
}

reset() {
    exit_num = random(4);	/* "grin" */
}

init() {
    add_action("e0"); add_verb("north");
    add_action("e1"); add_verb("south");
    add_action("e2"); add_verb("east");
    add_action("e3"); add_verb("west");
}

e0() {
    if (exit_num == 0)
	call_other(this_player(), "move_player", "north#room/maze1/maze2");
    else
	call_other(this_player(), "move_player", "north#room/well");
    return 1;
}

e1() {
    if (exit_num == 1)
	call_other(this_player(), "move_player", "south#room/maze1/maze2");
    else
	call_other(this_player(), "move_player", "south#room/well");
    return 1;
}

e2() {
    if (exit_num == 2)
	call_other(this_player(), "move_player", "east#room/maze1/maze2");
    else
	call_other(this_player(), "move_player", "east#room/well");
    return 1;
}

e3() {
    if (exit_num == 3)
	call_other(this_player(), "move_player", "west#room/maze1/maze2");
    else
	call_other(this_player(), "move_player", "west#room/well");
    return 1;
}
