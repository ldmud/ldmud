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
	  "The shore of Crescent Lake continues north and south\n");
}

north()
{
    this_player()->move_player("north#room/south/sshore2");
    return 1;
}

south()
{
    this_player()->move_player("south#room/south/sshore4");
    return 1;
}

east()
{
    this_player()->move_player("east#room/south/sforst11");
    return 1;
}
