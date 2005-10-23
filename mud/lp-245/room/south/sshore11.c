void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("east", "east");
    add_action("west", "west");
}

string short()
{
    return "The shore of Crescent Lake";
}

void long()
{
    write("You are standing on the shore of Crescent Lake, a beautiful and\n" +
	  "clear lake. Out in the centre of the lake stands the Isle\n" +
	  "of the Magi.\n" +
	  "The shore of Crescent Lake continues east and west\n");
}

int east()
{
    this_player()->move_player("east#room/south/sshore10");
    return 1;
}

int west()
{
    this_player()->move_player("west#room/south/sshore12");
    return 1;
}

