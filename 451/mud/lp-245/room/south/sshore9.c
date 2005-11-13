reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("northeast", "northeast");
    add_action("west", "west");
    add_action("east", "east");
}

short()
{
    return "The shore of Crescent Lake";
}

long()
{
    write("You are standing on the shore of Crescent Lake, a beautiful and\n" +
	  "clear lake. Out in the centre of the lake stands the Isle\n" +
	  "of the Magi.\n" +
	  "A trail leads into the forest to the east.\n" +
	  "The shore of Crescent Lake continues northeast and west\n");
}

northeast()
{
    this_player()->move_player("northeast#room/south/sshore8");
    return 1;
}

east()
{
    this_player()->move_player("east#room/south/sforst20");
    return 1;
}

west()
{
    this_player()->move_player("west#room/south/sshore10");
    return 1;
}
