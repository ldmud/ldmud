void reset(int started)
{
    if (!started)
	set_light(1);
}

void init()
{
    add_action("north", "north");
    add_action("south", "south");
    add_action("east", "east");
    add_action("west", "west");
    add_action("look", "look");
}

string short()
{
    return "A crumbling stone monument";
}

void long()
{
    write("You are halfway up the hill.\n" +
	  "A crumbling monument stands on the side of the hill here.\n" +
	  "On top of the hill, to the west, stands the ruins of the tower of\n" +
	  "Arcanarton.\n" +
	  "A path winds around the hill to the north, and heads down to the shore\n" +
	  "of the island to the south and east\n");
}

int north()
{
    this_player()->move_player("north#room/south/sislnd14");
    return 1;
}

int south()
{
    this_player()->move_player("south#room/south/sislnd6");
    return 1;
}

int east()
{
    this_player()->move_player("east#room/south/sislnd5");
    return 1;
}

int west()
{
    this_player()->move_player("west#room/south/sislnd18");
    return 1;
}

int look(string str)
{
    if (str != "at monument")
	return 0;
    else
	{
	    write("The monument is very old and is crumbling.\n" +
		  "Affixed to the side of the monument is a corroded old plaque which reads:\n" +
"+-------------------------------------------------------------------------+\n"+
"|                   BEWARE ALL YE WHO READ THIS MESSAGE                   |\n"+
"|        Be it known, that on this day, the tower of the evil mage        |\n"+
"|       Arcanarton was destroyed in an attack by the combined forces      |\n"+
"|               of all of the mages of the land of Lustria.               |\n"+
"|     The body of the mage Arcanarton was not found, and it is feared     |\n"+
"|             that his evil work in this world is not yet over.           |\n"+
"+-------------------------------------------------------------------------+\n");
	}
    return 0;
}
