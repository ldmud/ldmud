#include "std.h"
#include "tune.h"

#undef EXTRA_RESET
#define EXTRA_RESET\
    if (!arg)\
	move_object("obj/book", this_object());

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("cost_for_level"); add_verb("cost");\
    add_action("advance"); add_verb("advance");\
    add_action("banish"); add_verb("banish");

ONE_EXIT("room/vill_road2", "north",
	 "The adventurers guild",
	 "You have to come here when you want to advance your level.\nYou can also buy points for a new level.\nCommands: cost, advance.\n", 1)

int next_level;
int next_exp;
int level;
int exp;
int title;
object player_ob;
string banished_by;

get_level() {
    if (level == 19) {
	next_exp = 1250000;
	next_level = 20;
	title = "the apprentice Wizard";
	return;
    }
    if (level == 18) {
	next_exp = 850000;
	next_level = 19;
	title = "the greater necromancer";
	return;
    }
    if (level == 17) {
	next_exp = 550000;
	next_level = 18;
	title = "the necromancer";
	return;
    }
    if (level == 16) {
	next_exp = 350000;
	next_level = 17;
	title = "the master sorcerer";
	return;
    }
    if (level == 15) {
	next_exp = 200000;
	next_level = 16;
	title = "the sorcerer";
	return;
    }
    if (level == 14) {
	next_exp = 130000;
	next_level = 15;
	title = "the lesser sorcerer";
	return;
    }
    if (level == 13) {
	next_exp = 100000;
	next_level = 14;
	title = "the master magician";
	return;
    }
    if (level == 12) {
	next_exp = 80000;
	next_level = 13;
	title = "the journeyman magician";
	return;
    }
    if (level == 11) {
	next_exp = 60000;
	next_level = 12;
	title = "the apprentice magician";
	return;
    }
    if (level == 10) {
	next_exp = 40000;
	next_level = 11;
	title = "the great champion";
	return;
    }
    if (level == 9) {
	next_exp = 25000;
	next_level = 10;
	title = "the champion";
	return;
    }
    if (level == 8) {
	next_exp = 17500;
	next_level = 9;
	title = "the myrmidon";
	return;
    }
    if (level == 7) {
	next_level = 8;
	next_exp = 12500;
	title = "the great adventurer";
	return;
    }
    if (level == 6) {
	next_level = 7;
	next_exp = 7500;
	title = "the expert adventurer";
	return;
    }
    if (level == 5) {
	next_level = 6;
	next_exp = 5000;
	title = "the experienced adventurer";
	return;
    }
    if (level == 4) {
	next_level = 5;
	next_exp = 3500;
	title = "the small adventurer";
	return;
    }
    if (level == 3) {
	next_level = 4;
	next_exp = 2000;
	title = "the medium beginner";
	return;
    }
    if (level == 2) {
	next_level = 3;
	next_exp = 1000;
	title = "the lowrank beginner";
	return;
    }
    if (level == 1) {
	next_level = 2;
	next_exp = 250;
	title = "the simple novice";
	return;
    }
    title = "the utter novice";
    next_level = 1;
    next_exp = 0;
}

/*
 * This routine is called by monster, to calculate how much they are worth.
 * This value should not depend on the tuning.
 */
query_cost(l) {
    player_ob = this_player();
    level = l;
    if (level >= 20)
	return 1000000;
    get_level();
    return next_exp;
}

cost_for_level() {
    player_ob = this_player();
    level = call_other(player_ob, "query_level", 0);
    if (level >= 20) {
	write("You will have to seek other ways.\n");
	return 1;
    }
    exp = call_other(player_ob, "query_exp", 0);
    get_level();
    if (next_exp <= exp) {
	write("It will cost you nothing to be promoted.\n");
	return 1;
    }
    write("It will cost you "); write((next_exp - exp) * 1000 / EXP_COST);
    write(" gold coins to advance to level "); write(next_level);
    write(".\n");
    return 1;
}

advance() {
    string name_of_player;
    int cost;
    player_ob = this_player();
    name_of_player = call_other(player_ob, "query_name", 0);
    level = call_other(player_ob, "query_level", 0);
    exp = call_other(player_ob, "query_exp", 0);
    title = call_other(player_ob, "query_title", 0);
    if (level >= 20) {
	write("You are still "); write(title); write("\n");
	return 1;
    }
    get_level();
    cost = (next_exp - exp) * 1000 / EXP_COST;
    if (next_exp > exp) {
	if (call_other(player_ob, "query_money", 0) < cost) {
	    write("You don't have enough gold coins.\n");
	    return 1;
	}
	call_other(player_ob, "add_money", - cost);
    }
    say(call_other(player_ob, "query_name", 0) + " is now level " +
	next_level + ".\n");
    call_other(player_ob, "set_level", next_level);
    call_other(player_ob, "set_title", title);
    if (exp < next_exp)
        call_other(player_ob, "add_exp", next_exp - exp);
    write("The guild secretary makes a note in the records...\nSaving character...\n");
    call_other(player_ob,"save_character");
    if (next_level < 7) {
	write("You are now " + name_of_player + " " + title +
	      " (level " + next_level + ").\n");
	return 1;
    }
    if (next_level < 14) {
	write("Well done, " + name_of_player + " " + title +
	      " (level " + next_level + ").\n");
	return 1;
    }
    if (next_level < 20) {
	write("Welcome to your new class, mighty one.\n" +
	      "You are now " + title + " (level " + next_level + ").\n");
    }
    if (next_level == 20) {
	write("A new Wizard has been born!\n");
	shout("A new Wizard has been born!\n");
	write("If you want a castle of your own, seek out Leo the archwizard,\n");
	write("and ask him.  Before you do, however, decide where you want to\n");
	write("put it.  You don't need to ask him for it now.  Feel free to wait.\n");
	return 1;
    }
}
 
/*
 * Banish a monster name from being used.
 */
banish(who) {
    level = call_other(this_player(), "query_level");
    if (level < 21)
	return 0;
    if (!who) {
	write("Who ?\n");
	return 1;
    }
    if (!call_other(this_player(), "valid_name", who))
	return 1;
    if (restore_object("players/" + who)) {
	write("That name is already used.\n");
	return 1;
    }
    if (restore_object("banish/" + who)) {
	write("That name is already banished.\n");
	return 1;
    }
    banished_by = call_other(this_player(), "query_name");
    title = call_other(this_player(), "query_title");
    if (banished_by == "Someone") {
	write("You must not be invisible!\n");
	return 1;
    }
    save_object("banish/" + who);
    return 1;
}

/* This function checks a person's experience, and makes sure that they are
   not a higher level than they should be.  Useful for after death, etc. */

correct_level(player)
{
    level = call_other(player, "query_level", 0);
    exp = call_other(player, "query_exp", 0);
    /* Shouldn't "correct" wizards. */
    if (level >= 20)
	return;
    get_level();
    while (exp < next_exp) {
	level -= 1;
	get_level();
    }
    call_other(player, "set_level", next_level);
    call_other(player, "set_title", title);
}
