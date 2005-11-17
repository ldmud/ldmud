reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("north", "north");
    add_action("northeast", "northeast");
    add_action("northwest", "northwest");
}

short()
{
    return "The shore of the Isle of the Magi";
}

long()
{
    write("You are standing on the shore of the Isle of the Magi\n" +
	  "The shore of the island continues northeast and northwest from here\n" +
	  "To the north, a hill rises up to the ancient ruins of the Tower\n" +
	  "of Arcanarton, the archmage who used to live on this island\n");
}

north()
{
     this_player()->move_player("north#room/south/sislnd16");
     return 1;
}

northeast()
{
     this_player()->move_player("northeast#room/south/sislnd6");
     return 1;
}

northwest()
{
     this_player()->move_player("northwest#room/south/sislnd8");
     return 1;
}
