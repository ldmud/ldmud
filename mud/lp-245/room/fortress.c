void extra_reset()
{
    object orc, weapon;
    int n,i,class,value,weight;
    string w_name,alt_name;

    i = 0;
    if (!present("orc")) {
	while(i<8) {
	    i += 1;
	    orc = clone_object("obj/monster");
	    orc->set_name("orc");
	    orc->set_alias("dirty crap");
	    orc->set_race("orc");
	    orc->set_level(random(2) + 1);
	    orc->set_hp(30);
	    orc->set_ep(1014);
	    orc->set_al(-60);
	    orc->set_short("An orc");
	    orc->set_ac(0);
	    orc->set_aggressive(1);
	    orc->load_a_chat(40, "room/orc_vall"->get_chats());
	    n = random(3);
	    weapon = clone_object("obj/weapon");
	    if (n == 0) {
		w_name = "knife";
		class = 5;
		value = 8;
		weight = 1;
		alt_name = "knife";  /* JnA: 901127 axes != knives */
	    }
	    if (n == 1) {
		w_name = "curved knife";
		class = 7;
		value = 15;
		weight = 1;
		alt_name = "knife";
	    }
	    if (n == 2) {
		w_name = "hand axe";
		class = 9;
		value = 25;
		weight = 2;
		alt_name = "axe";
	    }
	    weapon->set_name(w_name);
	    weapon->set_class(class);
	    weapon->set_value(value);
	    weapon->set_weight(weight);
	    weapon->set_alt_name(alt_name);
	    transfer(weapon, orc);
	    command("wield " + w_name, orc);
	    move_object(orc, this_object());
	}
    }
}
void init()
{
    add_action("south", "south");
    add_action("north", "north");
}

int north()
{
    if (present("orc")) {
	write("An orc bars your way.\n");
	return 1;
    }
    this_player()->move_player("north#room/orc_treasure");
    return 1;
}

int south()
{
    this_player()->move_player("south#room/orc_vall");
    return 1;
}

void long()
{
    write("This is the local strong point of the orcs.\n");
    write("There is an entrance to a small room to the north.\n");
}

string short() {
    return "The orc fortress";
}

void reset(int arg)
{
    if (!arg)
	set_light(1);
    extra_reset();
}

