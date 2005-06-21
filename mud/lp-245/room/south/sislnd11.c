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
    return "A grove on the shore of the Isle of the Magi";
}

void long()
{
    write("You are standing in a grove on the shore of the Isle of the Magi\n" +
	  "All of the trees in the grove are either diseased, dead or heavily mutated.\n" +
	  "The shore of the island continues to the east,and the grove follows\n" +
	  "the shoreline west to Focus Point.\n" +
	  "The grove also continues to the south.\n");
}

int south()
{
     this_player()->move_player("south#room/south/sislnd9");
     return 1;
}

int east()
{
     this_player()->move_player("east#room/south/sislnd12");
     return 1;
}

int west()
{
     this_player()->move_player("west#room/south/sislnd10");
     return 1;
}
