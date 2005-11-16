reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("east", "east");
    add_action("west", "west");
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
	  "The shore of Crescent Lake continues east and west\n");
}

east()
{
    this_player()->move_player("east#room/south/sshore12");
    return 1;
}

west()
{
    this_player()->move_player("west#room/south/sshore14");
    return 1;
}

