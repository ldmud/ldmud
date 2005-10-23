#include "room.h"

object gold_stick, orc_slayer, shaman;

#undef EXTRA_RESET
#define EXTRA_RESET fix_shaman();

void fix_shaman()
{
    if (!shaman || !living(shaman)) {
	gold_stick = clone_object("obj/treasure");
	gold_stick->set_id("staff");
	gold_stick->set_alias("golden staff");
	gold_stick->set_short("A golden staff");
	gold_stick->set_value(300);
	orc_slayer = clone_object("obj/weapon");
	orc_slayer->set_name("short sword");
	orc_slayer->set_alias("sword");
	orc_slayer->set_short("Short sword");
	orc_slayer->set_alt_name("orc slayer");
	orc_slayer->set_long("This is a very fine blade.\n"+
		"It's covered with ancient runes.\n" +
		"Engraved on it is a picture of the sword slicing an orc.\n");
	orc_slayer->set_read("The only thing you can read is the word 'orc'.\n");
	orc_slayer->set_class(9);
	orc_slayer->set_weight(2);
	orc_slayer->set_value(200);
	orc_slayer->set_hit_func(this_object());
	shaman = clone_object("obj/monster.talk");
	shaman->set_name("shaman");
	shaman->set_alias("orc shaman");
	shaman->set_race("orc");
	shaman->set_level(10);
	shaman->set_al(-300);
	shaman->set_short("An orc shaman");
	shaman->set_wc(10);
	shaman->set_ac(1);
	shaman->set_aggressive(1);
	shaman->set_chance(20);
	shaman->set_spell_mess1("You are hit by a magic missile");
	shaman->set_spell_mess2("The shaman casts an magic missile");
	shaman->set_spell_dam(20);
	move_object(shaman, this_object());
	move_object(gold_stick, shaman);
	move_object(orc_slayer, shaman);
    }
}

ONE_EXIT("room/fortress", "south",
	 "The orc treasury",
	 "You are in the orc treasury. It is normally heavily guarded.\n", 1)

int weapon_hit(object attacker)
{
    string alig;


    if(attacker->id("orc")){
	write("Ziiing\n");
	return 10;
    }
    return 0;
}
