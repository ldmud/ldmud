void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("northeast", "northeast");
    add_action("northwest", "northwest");
    add_action("southeast", "southeast");
}

string short()
{
    return "The shore of Crescent Lake";
}

void long()
{
    write("You are standing on the shore of Crescent Lake, a beautiful and\n" +
	  "clear lake. Out in the centre of the lake stands the Isle\n" +
	  "of the Magi.\n" +
	  "A trail leads into the forest to the north.\n" +
	  "The shore of Crescent Lake continues northeast and northwest\n" +
	  "To the southeast, a stone bridge used to cross over to the\n" +
	  "Isle of the Magi, but it has fallen into the lake, making the\n" +
	  "crossing to the island impossible by that means.\n");
    if (this_player()->query_level() > 15)
	write("However, you can make out the faint outline of a magical bridge" +
	      "\nin its place.\n");
}

int north()
{
    this_player()->move_player("north#room/south/sforst46");
    return 1;
}

int northeast()
{
    this_player()->move_player("northeast#room/south/sshore27");
    return 1;
}

int northwest()
{
    this_player()->move_player("northwest#room/south/sshore25");
    return 1;
}

int southeast()
{
    if (this_player()->query_level() <= 15 )
	{
	    write("The bridge to the Isle of the Magi has collapsed, making thetrip across\n" +
		  "impossible.\n");
	    return 1;
	}
    write("Trusting in your faith, you step onto the magical bridge and move across\n" +
	  "to the Isle of the Magi.\n");
    this_player()->move_player("southeast#room/south/sislnd1");
    return 1;
}
