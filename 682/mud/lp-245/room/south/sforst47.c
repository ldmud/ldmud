reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("south", "south");
    add_action("east", "east");
    add_action("west", "west");
}

short()
{
    return "A dimly lit forest";
}

long()
{
    write("You are in part of a dimly lit forest.\n" +
	  "Trails lead south, east and west\n");
}

south()
{
    this_player()->move_player("south#room/south/sshore27");
    return 1;
}

east()
{
    this_player()->move_player("east#room/south/sshore28");
    return 1;
}

west()
{
    this_player()->move_player("west#room/south/sforst48");
    return 1;
}
