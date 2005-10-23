void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("south", "south");
    add_action("west", "west");
}

string short()
{
    return "Shore of the Isle of the Magi";
}

void long()
{
    write("You are standing on the shore of the Isle of the Magi\n" +
	  "The shore of the island continues south and west from here\n" +
	  "To the south, a hill rises up to the ancient ruins of the Tower\n" +
	  "of Arcanarton, the archmage who used to live on this island\n" +
	  "although no track leads directly there from here\n");
}

int south()
{
     this_player()->move_player("south#room/south/sislnd4");
     return 1;
}

int west()
{
     this_player()->move_player("west#room/south/sislnd2");
     return 1;
}
