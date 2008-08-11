void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("east", "east");
    add_action("northwest", "northwest");
    add_action("southeast", "southeast");
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
	  "Trails lead into the forest to the south and west.\n" +
	  "The shore of Crescent Lake continues northwest and southeast\n");
}

int north()
{
    this_player()->move_player("north#room/south/sforst49");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sforst46");
    return 1;
}

int northwest()
{
    this_player()->move_player("northwest#room/south/sshore24");
    return 1;
}

int southeast()
{
    this_player()->move_player("southeast#room/south/sshore26");
    return 1;
}
