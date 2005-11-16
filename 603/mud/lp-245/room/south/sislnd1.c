reset(started)
{
    if (!started)
	set_light(1);
}

init()
{
    add_action("south", "south");
    add_action("east", "east");
    add_action("northwest", "northwest");
    add_action("southwest", "southwest");
}

short()
{
    return "Link to the mainland";
}

long()
{
    write("You are standing on the shore of the Isle of the Magi\n" +
	  "The shore of the island continues east and southwest from here\n" +
	  "To the south, a hill rises up to the ancient ruins of the Tower\n" +
	  "of Arcanarton, the archmage who used to live on this island\n" +
	  "A magical bridge now stands on the ruins of the old stone bridge\n" +
	  "to the northwest\n");
}

south()
{
     this_player()->move_player("south#room/south/sislnd13");
     return 1;
}

east()
{
     this_player()->move_player("east#room/south/sislnd2");
     return 1;
}

northwest()
{
     write("You trust in your faith and step oun onto the near invisible " +
	   "bridge...");
     this_player()->move_player("northwest#room/south/sshore26");
     return 1;
}

southwest()
{
     this_player()->move_player("southwest#room/south/sislnd12");
     return 1;
}
