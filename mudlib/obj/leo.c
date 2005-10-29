#include "../../security.h"
#include "living.h"
/*
 * This is the object wich tries to behave like Leo the archwizard.
 * The purpose is to converse with players and give them portable castles.
 */

object requesting_wizard;
object sponsor;
string next_out;
object next_dest;
object give_him_castle;
int delay;
int level;		/* Restored from the player save file. */

short() { return "Leo the Archwizard"; }

get() { return 0; }

long() {
    write(short() + ".\n");
}

id(str) { return str == name; }

reset(arg) {
    if (arg) {
	sponsor = 0;
	requesting_wizard = 0;
	return;
    }
    msgout = "leaves";
    msgin = "enters";
    name = "leo";
    next_out = 0;
    is_npc = 1;
    level = 40;
    alignment = 1000;
    weapon_class = WEAPON_CLASS_OF_HANDS;
    max_hp = 300;
    hit_point = 300;
    experience = 1000000;
    enable_commands();
    spell_points = 300;
}

catch_tell(str)
{
    object from;
    string a;
    string b;
    string c;
    from = this_player();
    if (!from)
	return;	/* Not from a real player. */
    if (sscanf(str, "%sello%s", a, b) == 2 ||
	sscanf(str, "%s hi%s", a, b) == 2 ||
	sscanf(str, "%s Hi%s", a, b) == 2){
	next_out = "Welcome, " + call_other(from, "query_name", 0) + ".\n";
	if (call_other(from, "query_level", 0) == 20)
	    next_out = next_out +
		"Now that you are a wizard, you can have a castle of your own.\n";
        delay=2;
	next_dest = from;
	set_heart_beat(1);
	return;
    }
    if (sscanf(str, "%sgive%scastle%s", a, b, c) == 3 ||
	sscanf(str, "%swant%scastle%s", a, b, c) == 3) {
	if (call_other(from, "query_level", 0) == 20) {
	    castle();
	    return;
	}
	next_out = "What ! Give a castle to you ?\n";
	next_dest = from;
	delay=2;
	set_heart_beat(1);
	return;
    }
    if (sscanf(str, "%syes%s", a, b) == 2 ||
	sscanf(str, "%sYes%s", a, b) == 2) {
	if (sponsor && from == sponsor) {
	    he_said_yes();
	    return;
	}
    }
    if (sscanf(str, "%sno%s", a, b) == 2 ||
	sscanf(str, "%sNo%s", a, b) == 2) {
	if (sponsor && from == sponsor) {
	    he_said_no();
	    return;
	}
    }
    log_file("LEO", str + "\n");
}

/*
 * Always let the heart_beat do the talking, to simulate delay.
 */

heart_beat()
{
    if (attacker_ob)
	fireball(attacker_ob);
    attack();
    if (random(40) == 1)
	say("Leo smiles.\n");
    if (delay>0) {
        delay -= 1;
        return;
    }
    if (next_out) {
	tell_object(next_dest, next_out);
	next_out = 0;
    }
    set_heart_beat(0);
}

castle() {
    write(
"You are now ready to take the step into true wizardhood. But, to do this,\n");
    write(
"you must select one wizard that will take responsibility for you.\n");
    write(
"He must also back up your claim of being a wizard, not by cheating.\n");
    write("If you have no name so far, come back here again.\n");
    write("Now give me the name: ");
    input_to("castle2");
}

castle2(back_up_wiz) {
    if (back_up_wiz == "") {
	write("Welcome back later.\n");
	return;
    }
    back_up_wiz = lower_case(back_up_wiz);
    sponsor = present(back_up_wiz,environment());
    if (!sponsor) {
	write("I'm sorry, the wizard must be here to sponsor you.  You might leave\n");
	write("them a note to meet you at a particular time here.\n");
	return;
    }
    if (call_other(sponsor, "query_level", 0) < SENIOR) {
	write("The person must be an experienced level wizard.  That person is not.\n");
	return;
    }
    tell_object(sponsor, call_other(this_player(),"query_name",0) +
		" wants you to sponsor him.  Please tell me 'yes' if you will, or\n" +
		"'no' if you won't.  Realize that you're expected to watch over this\n");
    tell_object(sponsor,
		"wizard and be responsible for him, help him if he has trouble,\n" +
		"and basically do sponsor-type things.\n");
    write("I'm asking him if he'll accept the responsibility.  Please wait.\n");
    requesting_wizard = this_player();
    return;
}

he_said_yes()
{
    string castle_name;
    string player_name;

    castle_name = clone_object("room/port_castle");
    /* We have to make sure we get the real name here! */
    player_name = capitalize(call_other(requesting_wizard, "query_real_name", 0));
    call_other(castle_name, "set_owner", player_name);
    move_object(castle_name, requesting_wizard);
    log_file("SPONSOR", call_other(sponsor, "query_real_name", 0) + " : " +
	     player_name + "\n");
    tell_object(requesting_wizard,
		"\n" +
		"Congratulations, you are now a complete wizard with your\n" +
		"own castle. But beware, you can only drop it once !\n" +
		"When it is dropped, it can never be moved again.\n" +
		"To get your wizard commands, type 'soul off' and then\n" +
		"'soul on'.\n\n");
    tell_object(requesting_wizard,
    "\nAlso, make sure to read /doc/build/RULES before you build anything!\n" +
    "If you don't know how, ask your sponsor!\n\n");
    call_other(requesting_wizard, "set_level", 21);
    call_other(requesting_wizard, "set_title", "the wizard");
    call_other(requesting_wizard, "save_character");
    requesting_wizard = 0;
    sponsor = 0;
    return 1;
}

he_said_no()
{
    if (!sponsor || !requesting_wizard) {
	say("You waited too long.  I forgot what was going on.\n");
	return;
    }
    tell_object(requesting_wizard, "Sorry, I'm afraid he declined.\n");
    tell_object(sponsor, "Probably a good choice on your part!\n");
    requesting_wizard = 0;
    sponsor = 0;
}
