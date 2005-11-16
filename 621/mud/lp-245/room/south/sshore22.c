reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("north", "north");
    add_action("west", "west");
    add_action("northeast", "northeast");
    add_action("southwest", "southwest");
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
	  "Trails lead into the forest to the north and west.\n" +
	  "The shore of Crescent Lake continues northeast and southwest\n");
}

north()
{
    this_player()->move_player("north#room/south/sforst21");
    return 1;
}

west()
{
    this_player()->move_player("west#room/south/sforst27");
    return 1;
}

northeast()
{
    this_player()->move_player("northeast#room/south/sshore23");
    return 1;
}

southwest()
{
    this_player()->move_player("southwest#room/south/sshore21");
    return 1;
}
