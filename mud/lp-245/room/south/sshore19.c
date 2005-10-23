void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("south", "south");
    add_action("northeast", "northeast");
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
	  "Trails lead into the forest to the north and south.\n" +
	  "The shore of Crescent Lake continues northeast and southeast\n");
}

int north()
{
    this_player()->move_player("north#room/south/sforst30");
    return 1;
}

int south()
{
    this_player()->move_player("south#room/south/sforst31");
    return 1;
}

int northeast()
{
    this_player()->move_player("northeast#room/south/sshore20");
    return 1;
}

int southeast()
{
    this_player()->move_player("southeast#room/south/sshore18");
    return 1;
}
