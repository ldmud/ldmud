int exit_num;
object leather;

short() {
    return "End of maze";
}

long() {
    write("The end of the maze.\n");
    write("There are one obvious exit to the south.\n");
}

init() {
    add_action("e1"); add_verb("south");
}

e1() {
    call_other(this_player(), "move_player", "south#room/maze1/maze4");
    return 1;
}

reset() {
    if (!leather || !present(leather)) {
	leather = clone_object("obj/armor");
	call_other(leather, "set_ac", 2);
	call_other(leather, "set_name", "armor");
	call_other(leather, "set_alias", "leather armor");
	call_other(leather, "set_value", 110);
	call_other(leather, "set_short", "A leather armor");
	call_other(leather, "set_weight", 3);
	call_other(leather, "set_type", "armor");
	move_object(leather, this_object());
    }
}

realm() {return "NT";}
