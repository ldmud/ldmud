#include "room.h"
#undef EXTRA_RESET
#define EXTRA_RESET extra_reset();

string * chats;

string * get_chats() {
    if (!chats) {
	chats = allocate(7);
	chats[0] = "Orc says: Kill him!\n";
	chats[1] = "Orc says: Bloody humans!\n";
	chats[2] = "Orc says: Stop him!\n";
	chats[3] = "Orc says: Get him!\n";
	chats[4] = "Orc says: Let's rip out his guts!\n";
	chats[5] = "Orc says: Kill him before he runs away!\n";
	chats[6] = "Orc says: What is that human doing here!\n";
    }
    return chats;
}

void extra_reset()
{
    object orc, weapon;
    int n,i,class,value,weight;
    string w_name,alt_name;

    i = 0;
    if (!present("orc")) {
	while(i<2) {
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
	    orc->load_a_chat(50, get_chats());
	    n = random(3);
	    weapon = clone_object("obj/weapon");
	    if (n == 0) {
		w_name = "knife";
		class = 5;
		value = 8;
		weight = 1;
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

TWO_EXIT("room/slope", "east",
	 "room/fortress", "north",
	 "The orc valley",
	 "You are in the orc valley. This place is inhabited by orcs.\n" +
	 "There is a fortress to the north, with lot of signs of orcs.\n", 1)
