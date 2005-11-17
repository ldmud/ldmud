reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("north", "north");
    add_action("south", "south");
    add_action("east", "east");
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
	  "Trails lead into the forest to the south and east.\n" +
	  "The shore of Crescent Lake continues north and southwest\n");
}

north()
{
    this_player()->move_player("north#room/south/sshore6");
    return 1;
}

south()
{
    this_player()->move_player("south#room/south/sforst19");
    return 1;
}

east()
{
    this_player()->move_player("east#room/south/sforst15");
    return 1;
}

southwest()
{
    this_player()->move_player("southwest#room/south/sshore8");
    return 1;
}
