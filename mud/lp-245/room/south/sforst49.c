void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
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
	  "Trails lead south, east and west\n");
}

int south()
{
    this_player()->move_player("south#room/south/sshore25");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sforst48");
    return 1;
}

int west()
{
    this_player()->move_player("west#room/south/sshore24");
    return 1;
}
