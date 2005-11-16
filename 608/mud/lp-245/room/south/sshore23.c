reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("east", "east");
    add_action("west", "west");
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
	  "A trail leads into the forest to the west.\n" +
	  "The shore of Crescent Lake continues southwest and east\n");
}

east()
{
    this_player()->move_player("east#room/south/sshore24");
    return 1;
}

west()
{
    this_player()->move_player("west#room/south/sforst21");
    return 1;
}

southwest()
{
    this_player()->move_player("southwest#room/south/sshore22");
    return 1;
}
