#include "std.h"
object beggar;

#undef EXTRA_RESET
#define EXTRA_RESET extra_reset();

extra_reset() {
    if (!present("knife")) {
        string weapon;
        weapon = clone_object("obj/weapon");
        call_other(weapon, "set_name", "small knife");
	call_other(weapon, "set_alias", "knife");
        call_other(weapon, "set_class", 5);
        call_other(weapon, "set_value", 8);
        call_other(weapon, "set_weight", 1);
	move_object(weapon, this_object());
    }
    if (!beggar || !living(beggar)) {
	beggar = clone_object("obj/monster.talk");
	call_other(beggar, "set_name", "beggar");
	call_other(beggar, "set_level", 3);
	call_other(beggar, "set_al", 200);
	call_other(beggar, "set_race", "human");
	call_other(beggar, "set_long",
		   "A really filthy looking poor beggar.\n");
	call_other(beggar, "set_chat", 3);
	call_other(beggar, "set_hp", 30);
	move_object(beggar, this_object());
	call_other(beggar, "set_object", this_object());
	call_other(beggar, "set_function", "give_beggar");
	call_other(beggar, "set_type", "gives");
	call_other(beggar, "set_match", " ");
	call_other(beggar, "set_chat_chance", 7);
	call_other(beggar, "set_a_chat_chance", 20);
	call_other(beggar, "load_chat",
		   "Beggar says: Please, give money to a poor beggar!\n");
	call_other(beggar, "load_a_chat",
		   "Beggar says: Why do you do this to me ?\n");
    }
}

THREE_EXIT("room/vill_road1", "south",
	 "room/pub2", "east",
	 "room/alley", "west",
	 "Small yard",
	 "A small yard surrounded by houses.\n", 1)


give_beggar(str) {
    int money;
    string who;

    say("Beggar says: Thank you.\n");
    if (sscanf(str, "%s gives you %d gold coins.", who, money) != 2)
	return;
    if (call_other(beggar, "query_money") >= 5 &&
	    environment(beggar) == this_object()) {
	call_other(beggar, "init_command", "east");
	call_other(beggar, "init_command", "buy beer");
	call_other(beggar, "init_command", "west");
    }
}

