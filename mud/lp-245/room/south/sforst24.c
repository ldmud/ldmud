void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("south", "south");
    add_action("east", "east");
}

string short()
{
    return "A dimly lit forest";
}

void long()
{
    write("You are in part of a dimly lit forest.\n" +
	  "Trails lead south and east\n");
}

int south()
{
    this_player()->move_player("south#room/south/sforst25");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sforst23");
    return 1;
}
