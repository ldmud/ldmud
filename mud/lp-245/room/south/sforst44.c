void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("east", "east");
    add_action("west", "west");
}

string short()
{
    return "A dimly lit forest";
}

void long()
{
    write("You are in part of a dimly lit forest.\n" +
	  "Trails lead north, east and west\n");
}

int north()
{
    this_player()->move_player("north#room/south/sforst38");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sforst43");
    return 1;
}

int west()
{
    this_player()->move_player("west#room/south/sforst45");
    return 1;
}
