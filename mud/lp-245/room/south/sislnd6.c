void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("west", "west");
    add_action("northeast", "northeast");
    add_action("southwest", "southwest");
}

string short()
{
    return "The shore of the Isle of the Magi";
}

void long()
{
    write("You are standing on the shore of the Isle of the Magi\n" +
	  "The shore of the island continues northeast and southwest from here\n" +
	  "To the northwest, a hill rises up to the ancient ruins of the Tower\n" +
	  "of Arcanarton, the archmage who used to live on this island\n" +
	  "To the north, you can see some sort of crumbled monument\n");
}

int north()
{
     this_player()->move_player("north#room/south/sislnd5");
     return 1;
}

int west()
{
     this_player()->move_player("west#room/south/sislnd16");
     return 1;
}

int northeast()
{
     this_player()->move_player("northeast#room/south/sislnd5");
     return 1;
}

int southwest()
{
     this_player()->move_player("southwest#room/south/sislnd7");
     return 1;
}
