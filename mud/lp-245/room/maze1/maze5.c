int exit_num;
object leather;

string short() {
    return "End of maze";
}

void long() {
    write("The end of the maze.\n");
    write("There are one obvious exit to the south.\n");
}

void init() {
    add_action("e1", "south");
}

int e1() {
    this_player()->move_player("south#room/maze1/maze4");
    return 1;
}

void reset() {
    if (!leather || !present(leather)) {
	leather = clone_object("obj/armour");
	leather->set_ac(3);
	leather->set_name("armour");
	leather->set_alias("leather armour");
	leather->set_value(110);
	leather->set_short("A leather armour");
	leather->set_weight(3);
	leather->set_type("armour");
	move_object(leather, this_object());
    }
}
