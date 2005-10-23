void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("west", "west");
    add_action("southwest", "southwest");
}

string short()
{
    return "The shore of the Isle of the Magi";
}

void long()
{
    write("You are standing on the shore of the Isle of the Magi\n" +
	  "The shore of the island continues north and southwest from here\n" +
	  "To the west, a hill rises up to the ancient ruins of the Tower\n" +
	  "of Arcanarton, the archmage who used to live on this island\n" +
	  "Halfway up the hill you can see some sort of crumbled monument\n");
}

int north()
{
     this_player()->move_player("north#room/south/sislnd4");
     return 1;
}

int west()
{
     this_player()->move_player("west#room/south/sislnd15");
     return 1;
}

int southwest()
{
     this_player()->move_player("southwest#room/south/sislnd6");
     return 1;
}
