#include "std.h"
#undef EXTRA_RESET
#define EXTRA_RESET extra_reset();

extra_reset()
{
    object orc, weapon;
    int n,i,class,value,weight;
    string w_name,alt_name;

    i = 0;
    if (!present("orc")) {
	while(i<2) {
	    i += 1;
	    orc = clone_object("obj/monster.talk");
	    call_other(orc, "set_name", "orc");
	    call_other(orc, "set_alias", "dirty crap");
	    call_other(orc, "set_race", "orc");
	    call_other(orc, "set_level", random(2) + 1);
	    call_other(orc, "set_hp", 30);
	    call_other(orc, "set_ep", 1014);
	    call_other(orc, "set_al", -60);
	    call_other(orc, "set_short", "An orc");
	    call_other(orc, "set_ac", 0);
	    call_other(orc, "set_aggressive", 1);
	    call_other(orc, "set_a_chat_chance", 50);
	    call_other(orc, "load_a_chat", "Orc says: Kill him!\n");
	    call_other(orc, "load_a_chat", "Orc says: Bloody humans!\n");
	    call_other(orc, "load_a_chat", "Orc says: Stop him!\n");
	    call_other(orc, "load_a_chat", "Orc says: Get him!\n");
	    call_other(orc, "load_a_chat", 
		"Orc says: Let's rip out his guts!\n");
	    call_other(orc, "load_a_chat", 
	       "Orc says: Kill him before he runs away!\n");
	    call_other(orc, "load_a_chat", 
	       "Orc says: What is that human doing here!\n");
	    n = random(3);
	    weapon = clone_object("obj/weapon");
	    if (n == 0) {
		w_name = "small knife";
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
	    call_other(weapon, "set_name", w_name);
	    call_other(weapon, "set_class", class);
	    call_other(weapon, "set_value", value);
	    call_other(weapon, "set_weight", weight);
	    call_other(weapon, "set_alt_name", alt_name);
	    transfer(weapon, orc);
	    call_other(weapon, "wield", w_name);
	    move_object(orc, this_object());
	}
    }
}

TWO_EXIT("room/slope", "east",
	 "room/fortress", "north",
	 "The orc valley",
	 "You are in the orc valley. This place is inhabited by orcs.\n" +
	 "There is a fortress to the north, with lot of signs of orcs.\n", 1)
