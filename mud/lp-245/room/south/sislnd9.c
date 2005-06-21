void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("east", "east");
    add_action("southeast", "southeast");
    add_action("northwest", "northwest");
}

string short()
{
    return "A small grove on the shore of the Isle of the Magi";
}

void long()
{
    write("You are standing in a small grove on the shore of the Isle of the Magi\n" +
	  "All of the trees here are either diseased, dead or heavily mutated\n" +
	  "The shoreline continues southeast from here, as well as heading northwest\n" +
	  "to Focus Point.\n" +
	  "The grove continues to the north.\n" +
	  "To the east, you can see an old disused well, and beyond that, on top\n" +
	  "of the hill, stands the ruined tower of Arcanarton\n");
}

int north()
{
     this_player()->move_player("north#room/south/sislnd11");
     return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sislnd17");
    return 1;
}

int southeast()
{
     this_player()->move_player("southeast#room/south/sislnd8");
     return 1;
}

int northwest()
{
     this_player()->move_player("northwest#room/south/sislnd10");
     return 1;
}
