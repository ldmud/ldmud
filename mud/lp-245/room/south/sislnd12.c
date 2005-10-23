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
    add_action("northeast", "northeast");
}

string short()
{
    return "The shore of the Isle of the Magi";
}

void long()
{
    write("You are standing on the shore of the Isle of the Magi\n" +
	  "The shore of the island continues northeast to the ruined bridge\n" +
	  "and west into a small grove from here.\n" +
	  "To the south is a old, disused well.\n" +
	  "Standing atop a cliff to the southwest is the ruined tower of Arcanarton,\n" +
	  "but there is no way to get up there from here.\n" +
	  "A path does lead up the hill to the east though.\n");
}

int south()
{
     this_player()->move_player("south#room/south/sislnd17");
     return 1;
}

int east()
{
     this_player()->move_player("east#room/south/sislnd13");
     return 1;
}

int west()
{
     this_player()->move_player("west#room/south/sislnd11");
     return 1;
}

int northeast()
{
     this_player()->move_player("northeast#room/south/sislnd1");
     return 1;
}
