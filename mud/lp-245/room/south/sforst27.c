void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("south", "south");
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
	  "Trails lead north, south, east and west\n");
}

int north()
{
    this_player()->move_player("north#room/south/sforst22");
    return 1;
}

int south()
{
    this_player()->move_player("south#room/south/sshore21");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sshore22");
    return 1;
}

int west()
{
    this_player()->move_player("west#room/south/sforst26");
    return 1;
}
