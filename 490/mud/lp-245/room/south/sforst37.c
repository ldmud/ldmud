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
    return "A dimly lit forest";
}

long()
{
    write("You are in part of a dimly lit forest.\n" +
	  "Trails lead north, south and east\n");
}

north()
{
    this_player()->move_player("north#room/south/sforst36");
    return 1;
}

south()
{
    this_player()->move_player("south#room/south/sforst45");
    return 1;
}

east()
{
    this_player()->move_player("east#room/south/sforst38");
    return 1;
}
