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
    return "The shore of the Isle of the Magi";
}

void long()
{
    write("You are standing on the shore of the Isle of the Magi\n" +
	  "A path leads up the hill to the east.\n" +
	  "The shore of the island continues southeast and northwest into a\n" +
	  "small grove from here\n" +
	  "To the north, you can see an old disused well.\n");
}

int north()
{
     this_player()->move_player("north#room/south/sislnd17");
     return 1;
}

int east()
{
     this_player()->move_player("east#room/south/sislnd16");
     return 1;
}

int southeast()
{
     this_player()->move_player("southeast#room/south/sislnd7");
     return 1;
}

int northwest()
{
     this_player()->move_player("northwest#room/south/sislnd9");
     return 1;
}
