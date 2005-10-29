#include "living.h"
#include "handshake.h"
#define SAVE_INTERVAL 200 /* How many heartbeats between auto-saves? */

object myself;		/* Ourselfs. */
object soul;        /* Our soul */
string title;       /* Our official title. Wiz's can change it. */
string password;	/* This players crypted password. */
string password2;   /* Temporary when setting password & used for Wizards*/
string al_title;
int intoxicated;	/* How many ticks to stay intoxicated. */
int headache, max_headache;
int save_counter;	/* How many heartbeats has it been since last save? */
string msghome;         /* wizard home string */
int power;              /* used in power get, power drop, etc.*/
string description;      /* players definable description */
int muffled;
object talker;
string pwd;

string it;		/* Last thing referenced. */

query_msgin() { return msgin; }
query_msgout() { return msgout; }
query_mmsgin() { return mmsgin; }
query_mmsgout() { return mmsgout; }

/* logon() is called when the players logs on. */

logon() {
    enable_commands();
    write("Welcome to the HMC LP-mud.\n");
    write("Please use the guest name if you just want a look.\n\n");
    write("What is your name: ");
    input_to("logon2");
    return 1;
}

object other_copy;

try_throw_out(str)
{
    object ob;
    if (str == "" || (str[0] != 'y' && str[0] != 'Y')) {
	write("Welcome another time then !\n");
	destruct(this_object());
	return;
    }
    soul = 0;
    soul("on");
    ob = first_inventory(other_copy);
    while(ob) {
	int weight;
	object next_ob;
	weight = call_other(ob, "query_weight");
	next_ob = next_inventory(ob);
	/*
	 * Don't move the soul.
	 */
	if (!call_other(ob, "id", "soul") && add_weight(weight)) {
	    call_other(ob, "drop");
	    move_object(ob, this_player());
	}
	ob = next_ob;
    }
    ob = environment(other_copy);
    if (call_other(other_copy, "quit")) {
	restore_object("players/" + name);
	write("Points restored from the other object.\n");
    }
    else
	destruct(other_copy);	/* Is this really needed ? */
    other_copy = 0;
    move_player_to_start(ob);
    log_file("ENTER", " (throw)\n");
}

logon2(str) {
    if (!str || str == "") {
	destruct(this_object());
	return;
    }
    str = lower_case(str);
    if (!valid_name(str)) {
	input_to("logon2");
	write("Give name again: ");
	return;
    }
    if (restore_object("banish/" + str)) {
	write("That name is reserved.\n");
	destruct(this_object());
	return;
    }
    if (!restore_object("players/" + str)) {
	write("New character.\n");
    }
    /*
     * Don't do this before the restore !
     */
    name = str;			/* Must be here for a new player. */
    myself = this_player();
    if (query_invis(0)>= SOMEONE)
	cap_name = "Someone";
    else
	cap_name = capitalize(name);

    add_action("give_object"); add_verb("give");
    add_action("score"); add_verb("score");
    add_action("save_character"); add_verb("save");
    add_action("quit"); add_verb("quit");
    add_action("kill"); add_verb("kill");
    add_action("communicate"); add_verb("say");
    add_action("shout_to_all"); add_verb("shout");
    add_action("put"); add_verb("put");
    add_action("pick_up"); add_verb("get");
    add_action("pick_up"); add_verb("take");
    add_action("drop_thing"); add_verb("drop");
    add_action("inventory"); add_verb("i");
    add_action("look"); add_verb("look");
    add_action("examine"); add_verb("examine");
    add_action("examine"); add_verb("exa");
    add_action("help"); add_verb("help");
    add_action("tell"); add_verb("tell");
    add_action("whisper"); add_verb("whisper");
    add_action("change_password"); add_verb("password");
    add_action("idea"); add_verb("idea");
    add_action("typo"); add_verb("typo");
    add_action("bug"); add_verb("bug");
    add_action("converse"); add_verb("converse");
    add_action("toggle_brief"); add_verb("brief");
    add_action("toggle_whimpy"); add_verb("wimpy");
    add_action("stop_hunting_mode"); add_verb("stop");
    add_action("spell_missile"); add_verb("missile");
    add_action("spell_shock"); add_verb("shock");
    add_action("spell_fire_ball"); add_verb("fireball");
    add_action("pose"); add_verb("pose");
    add_action("soul"); add_verb("soul");
    add_action("describe"); add_verb("describe");
    add_action("emergency"); add_verb("emergency");
    local_weight = 0;
    armor_class = 0;
    name_of_weapon = 0;
    weapon_class = WEAPON_CLASS_OF_HANDS;
    /* If this is a new character, we call the adventurers guild to get
     * our first title !
     */
    if (level == -1)
        write("Pick a password that has the first two characters unconnected with the rest of it.\n");
    write("Password: ");
    if (name == "guest")
	write("(just CR) ");
    if (level != -1)
	input_to("check_password");
    else 
	input_to("new_password");
    attacker_ob = 0;
    alt_attacker_ob = 0;
    return;
}

/* Called by command 'save' */
save_character() {
    save_me();
    write("Ok.\n");
    return 1;
}

/* This makes sure the amount we claim to be carrying is correct. */
recalc_carry() {
    object ob, next_ob;

    local_weight = 0;
    ob = first_inventory(myself);
    while(ob) {
	next_ob = next_inventory(ob);
	local_weight += call_other(ob, "query_weight", 0);
	ob = next_ob;
    }
}

reset(arg) {
    set_heart_beat(1);
    if (arg) {
	if (name == "logon") {
	    write("\nTimeout\n");
	    destruct(this_object());
	}
	if (level >= 20) return;
	/* These occasionally get screwed up.  Don't ask me why. */
	if (name == NAME_OF_GHOST) {
	    ghost = 1;
	    msgin = "drifts around";
	    msgout = "blows";
	} else {
	    ghost = 0;
	    msgin = "arrives";
	    msgout = "leaves";
	}
	recalc_carry();	    /* Make sure we're carrying the right amount. */
	if (time_to_heal > INTERVAL_BETWEEN_HEALING)
	    time_to_heal = INTERVAL_BETWEEN_HEALING;
	return;
    }
    level = -1;
    name = "logon";
    cap_name = "Logon";
    msgin = "arrives"; msgout = "leaves";
    mmsgin = "arrives in a puff of smoke";
    mmsgout = "disappears in a puff of smoke";
    msghome = "goes home";
    title = "the title-less";
    al_title = "neutral";
    save_counter = 0;
}

query_spell_point() {
    return spell_points;
}

short() {
    if (query_invis(call_other(this_player(),"query_level",0)) < level &&
        query_invis() >= NO_SHORT)
	    return 0;
    if (ghost)
	return "ghost of " + cap_name;
    if (frog)
	return cap_name + " the frog" + " (" + al_title + ")";
    return cap_name + " " + title + " (" + al_title + ")";
}

long() {
    write(short() + ".\n");
    if (call_other(this_player(),"query_level",0) > 19) {
        if (level >= GOD) { write("==> god\n");
        } else { if (level >= ELDER) { write("==> senior wizard\n");
                 } else { if (level >= SENIOR) {write ("==> senior wizard\n");
	                  } else { if (level > 19) write("==> wizard\n"); }
                 }
        }
    }	
    if (description) write(cap_name + description + ".\n");
    if (ghost || frog)
	return;
    if (hit_point < max_hp/10) {
	write(cap_name + " is in very bad shape.\n");
	return;
    }
    if (hit_point < max_hp/5) {
	write(cap_name + " is in bad shape.\n");
	return;
    }
    if (hit_point < max_hp/2) {
	write(cap_name + " is somewhat hurt.\n");
	return;
    }
    if (hit_point < max_hp - 20) {
	write(cap_name + " is slightly hurt.\n");
	return;
    }
    write(cap_name + " is in good shape.\n");
}

score() {
    int intox_level;
    
    if (ghost) {
	write("You are in an immaterial state with no scores.\n");
	return 1;
    }
    write("You have " + experience + " experience points, " +
	  money + " gold coins,\n");
    write(hit_point + " hit points (of " + max_hp + "), and ");
    write(spell_points + " spell points.\nYou are " + short() + " (level " + level + ").\n");
    if (hunter && call_other(hunter, "query_name", 0))
        write("You are hunted by " + call_other(hunter, "query_name", 0) + ".\n");
    if (!intoxicated)
	write("You are sober.\n");
    else {
	intox_level = (level + 4) / intoxicated;
	if (intox_level == 0)
	    write("You are in a drunken stupor.\n");
	else if (intox_level == 1)
            write("You are roaring drunk.\n");
	else if (intox_level == 2)
	    write("You are somewhat drunk.\n");
	else if (intox_level == 3)
	    write("You are quite tipsy.\n");
	else
	    write("You are slightly tipsy.\n");
    }
    if (whimpy)
	write("Wimpy mode.\n");
    show_age(); write("\n");
    return 1;
}

/* Identify ourself. */
id(str) {
    if (this_player() && query_invis(call_other(this_player(),"query_level",0)) < level &&
        query_invis() >= NO_ID)
	    return 0;
    if (ghost)
	return str == "ghost of " + name;
    if (str == name)
	return 1;
    return 0;
}

who() {
    if (query_invis(call_other(this_player(),"query_level",0)) < level &&
        query_invis() >= NO_WHO)
	    return 0;
    if (ghost)
	return "ghost of " + cap_name;
    if (frog)
	return capitalize(name) + " the frog" + " (" + al_title + ")";
    return capitalize(name) + " " + title + " (" + al_title + ")";
}

query_title() {
    return title;
}

set_level(lev) {
    if (lev > 21 || lev < level && level >= 20)
        return illegal_patch("set_level");      /* NOPE ! */
    level = lev;
    max_hp = 42 + level * 8;
    if (level >= 20) {
        tell_object(myself, "Adding wizard commands...\n");
        if (soul) destruct(soul);
        soul = clone_object("obj/wiz_soul");
	move_object(soul,myself);
    }
    if (level >= 20 && soul)
        call_other(soul,"update",1);
    log_file("ADVANCE",cap_name+" advanced to level "+level+"\n");
    save_me();
}

destruct_inventory() {
    object next_ob,ob;

    ob = first_inventory(this_object());
    while(ob) {
	next_ob = next_inventory(ob);
	destruct(ob);
	ob = next_ob;
    }
}

set_title(t) {
    if (!t) {
	write("Your title is " + title + ".\n");
	return 1;
    }
    title = t;
    return 1;
}

set_wiz_level(key){
    string lev;

    lev = call_other(call_other(this_player(),"query_soul",0),
	"get_handshake",key);
    if (lev) sscanf(lev,"%d",level);
    tell_object(myself,"You were promoted to level " + lev + " by " +
        capitalize(call_other(this_player(),"query_real_name",0)) + ".\n");
    tell_object(myself,"You will need to do a `soul on` to get your powers.\n");
    tell_object(myself,"Remember to read the help files to determine yous new abilities.\n");
    log_file("PROMOTIONS",capitalize(name) + "was promoted to level " + lev +
        " by " + capitalize(call_other(this_player(),"query_real_name",0)) +
	".\n");
    soul("off");
    save_me();
}

quit() {
    power = 0;
    drop_all(1);
    save_me();
    destruct_inventory();
    write("Saving "); write(capitalize(name)); write(".\n");
    checked_say(cap_name + " left the game.\n");
    log_file("ENTER", cap_name + " (" + name + ") exited with " + experience +
	     " ep, " + money + " gold.\n");
    destruct(this_object());
    return 1;
}

  
valid_attack(ob) {
    int their_level, can_attack;
    /* If we're already fighting them, then it must be OK. */
    if (ob == attacker_ob || ob == alt_attacker_ob) return 1;
    /* They can always attack NPCs */
    if (call_other(ob, "query_npc", 0)) return 1;
    /* Utter novices can't attack any other players */
    if (level < 2 && call_other(ob, "is_player", 0))
	return 0;
    their_level = call_other(ob, "query_level", 0);
    if (their_level <= 2)
	{ return 0; }
    else
	{ return 1; }
}

kill(str) {
    object ob;
    if (ghost)
	return 0;
    if (!str) {
        write("Kill what ?\n");
	return 1;
    }
    ob = present(lower_case(str), environment(this_player()));
    if (!ob) {
	write("No " + str + " here !\n");
	return 1;
    }
    if (!living(ob)) {
	write(str + " is not a living thing !\n");
	checked_say(cap_name + " tries foolishly to attack " + str + ".\n");
	return 1;
    }
    if (ob == this_object()) {
	write("What? Attack yourself?\n");
	return 1;
    }
    if (attacker_ob == ob) {
	write("Yes, yes.\n");
	return 1;
    }
    it = str;
    if (!attack_object(ob))
	write("You can't attack " + call_other(ob, "query_name", 0) + "!\n");
    return 1;
}

communicate(str) {
    if (!str) {
	write("Say what ?\n");
	return 1;
    }
    write("Ok.\n");
    if (ghost) {
	say(short() + " says: " + str + ".\n");
	return 1;
    }
    say(cap_name + " says: " + str + "\n");
    return 1;
}

heart_beat() {
    if (ghost)
	return;
    age += 1;
    save_counter += 1;
    if ((save_counter > SAVE_INTERVAL) && (level < 20)) {
	write("\nAuto-saving character...");
	save_character();
    }
    if (level > 20)
	save_counter = 0;
    if (intoxicated && random(20) == 0) {
	int n;
	n = random(7);
	if (n == 0) {
	    checked_say(cap_name + " hiccups.\n");
	    write("You hiccup.\n");
	}
	if (n == 1) {
	    checked_say(cap_name + " seems to fall, but takes a step and recovers.\n");
	    write("You stumble.\n");
	}
	if (n == 3) {
	    write("You feel drunk.\n");
	    checked_say(cap_name + " looks drunk.\n");
	}
	if (n == 5) {
	    checked_say(cap_name + " burps.\n");
	    write("You burp.\n");
	}
    }
    if (hit_point < max_hp || spell_points < max_hp || intoxicated || headache)
    {
	time_to_heal -= 1;
	if (time_to_heal < 0) {
	    if (headache) {
		headache -= 1;
		if (headache == 0)
		    tell_object(myself, "You no longer have a head ache.\n");
	    }
	    if (hit_point < max_hp) {
		hit_point += 1;
		if (intoxicated)
		    hit_point += 3;
		if (hit_point > max_hp)
		    hit_point = max_hp;
	    }
	    if (spell_points < max_hp) {
		spell_points += 1;
		if (intoxicated)
		    spell_points += 3;
		if (spell_points > max_hp)
		    spell_points = max_hp;
	    }
	    if (intoxicated) {
		intoxicated -= 1;
		if (intoxicated == 0) {
		    headache = max_headache;
		    max_headache = 0;
		    tell_object(myself,
		       "You suddenly without reason get a bad head ache.\n");
		    hit_point -= 3;
		    if (hit_point < 0)
			hit_point = 0;
		}
	    }
	    time_to_heal = INTERVAL_BETWEEN_HEALING;
	}
    }
    if (attacker_ob)
	attack();
    if (attacker_ob && whimpy && hit_point < max_hp/5)
	run_away();
}

/*
 * Update our aligment.
 */

add_alignment(a) {
    if (call_other(this_player(), "query_level") > 20)
        return;
    alignment -= (alignment*2/(30-level));
    alignment += a;
    if (alignment > KILL_NEUTRAL_ALIGNMENT * 64) {
	al_title = "white lord";
	return;
    }
    if (alignment > KILL_NEUTRAL_ALIGNMENT * 32) {
	al_title = "paladin";
	return;
    }
    if (alignment > KILL_NEUTRAL_ALIGNMENT * 16) {
	al_title = "crusader";
	return;
    }
    if (alignment > KILL_NEUTRAL_ALIGNMENT * 8) {
	al_title = "good";
	return;
    }
    if (alignment > KILL_NEUTRAL_ALIGNMENT * 4) {
	al_title = "honorable";
	return;
    }
    if (alignment > - KILL_NEUTRAL_ALIGNMENT * 4) {
	al_title = "neutral";
	return;
    }
    if (alignment > - KILL_NEUTRAL_ALIGNMENT * 8) {
	al_title = "malicious";
	return;
    }
    if (alignment > - KILL_NEUTRAL_ALIGNMENT * 16) {
	al_title = "evil";
	return;
    }
    if (alignment > - KILL_NEUTRAL_ALIGNMENT * 32) {
	al_title = "infamous";
	return;
    }
    if (alignment > - KILL_NEUTRAL_ALIGNMENT * 64) {
	al_title = "black knight";
	return;
    }
    al_title = "lord of evil";
}

test_dark() {
    if (set_light(0) <= 0) {
	write("It is too dark.\n");
	return 1;
    }
    return 0;
}

put(str) {
    int i;
    string item;
    string container;
    object item_o;
    object container_o;


    power = 0;
    if (level >= ITEM_OVER)
        if (sscanf(str,"! %s",power) == 1) {
            str = power;
	    power = "!";
        }
    if (!str)
	return 0;
    if (test_dark())
	return 1;
    if (sscanf(str, "%s in %s", item, container) != 2) {
	write("put what ?\n");
	return 1;
    }
    container = lower_case(container);
    container_o = present(container, this_player());
    if (!container_o)
	container_o = present(container, environment(this_player()));
    if (!container_o) {
	write("There are no " + container + "s here!\n");
	return 1;
    }
    if (!call_other(container_o, "can_put_and_get", 0) && !power) {
	write("You can't do that.\n");
	return 1;
    }
    item = lower_case(item);
    item_o = present(item, this_player());
    if (!item_o) {
	write("You have no " + item + "!\n");
	return 1;
    }
    if (item_o == container_o)
	return 0;
    if (call_other(item_o, "prevent_insert") && !power)
	return 1;
    if (call_other(item_o, "drop") && !power)
	return 1;
    i = call_other(item_o, "query_weight");
    if (call_other(container_o, "add_weight", i) || power) {
	/* Remove the weight from the previous container. */
	call_other(environment(item_o), "add_weight", -i);
	move_object(item_o, container_o);
	checked_say(cap_name + " puts the " + item + " in the " + container + ".\n");
	write("Ok.\n");
	it = item;
	return 1;
    }
    write("There is not room for more.\n");
    return 1;
}

pick_up(str) {
    string item;
    string container;
    object item_o;
    object container_o;

    power = 0;
    if (level >= ITEM_OVER)
    if (sscanf(str,"! %s",power) == 1) {
            str = power;
	    power = "!";
        }
    if (!str) {
	write("Get what?\n");
	return 1;
    }
    if (ghost && !power) {
	write("Your incorporeal hand passes right through it.\n");
	return 1;
    }
    if (test_dark() && !power)
	return 1;
    if (str == "all") {
	get_all(environment());
	return 1;
    }
    if (sscanf(str, "%s from %s", item, container) != 2) {
	pick_item(str);
	return 1;
    }
    container_o = present(lower_case(container));
    if (!container_o) {
	write("There is no " + container + " here.\n");
	return 1;
    }
    if (!call_other(container_o, "can_put_and_get", 0) && !power) {
	write("You can't do that!\n");
	return 1;
    }
    item_o = present(item, container_o);
    if (item_o) {
	if (call_other(item_o, "id", item)) {
	    int weight;
	    if (!call_other(item_o, "get", item) && !power) {
		write("You can not take " + item + " from " +
		      container + ".\n");
		return 1;
	    }
	    weight = call_other(item_o, "query_weight", 0);
	    if (!add_weight(weight) && !power) {
		write("You can not carry more.\n");
		return 1;
	    }
	    call_other(container_o, "add_weight", -weight);
	    move_object(item_o, myself);
	    it = item;
	    write("Ok.\n");
	    checked_say(cap_name + " takes " + item + " from " + container + ".\n");
	    return 1;
	}
    }
    write("There is no " + item + " in the " + container + ".\n");
    return 1;
}

pick_item(obj) {
    object ob;
    int i;

    obj = lower_case(obj);
    ob = present(obj, environment(this_player()));
    if (!ob) {
	write("That is not here.\n");
	return 1;
    }
    if (ghost && !power) {
	write("You fail.\n");
	return 1;
    }
    if (environment(ob) == myself) {
	write("You already have it!\n");
	return 1;
    }
    if (!call_other(ob, "get", 0) && !power) {
	write("You can not take that!\n");
	return 1;
    }
    i = call_other(ob, "query_weight", 0);
    if (add_weight(i) || power) {
	move_object(ob, myself);
	checked_say(cap_name + " takes " + obj + ".\n");
	it = obj;
	write("Ok.\n");
	return 1;
    }
    write("You can't carry that much.\n");
    return 1;
}

drop_thing(obj) {
    string tmp;
    string tmp2;
    int i;

    power = 0;
    if (level >= ITEM_OVER)
        if (sscanf(obj,"! %s",power) == 1) {
            obj = power;
	    power = "!";
        }
    if (!obj) {
	write("What ?\n");
	return 1;
    }
    if (obj == "all") {
	drop_all(1);
	return 1;
    }
    if (sscanf(obj, "%s in %s", tmp, tmp2) == 2) {
        put(obj);
	return 1;
    }
    if (obj == "money" || obj == "all money") {
	drop_all_money(1);
	return 1;
    }
    tmp = obj;
    obj = present(lower_case(obj), this_player());
    if (!obj) {
	write("That is not here.\n");
	return 1;
    }
    if (drop_one_item(obj)) {
	it = tmp;
	write("Ok.\n");
	checked_say(cap_name + " drops the " + tmp + ".\n");
    }
    return 1;
}

query_weight() { return 80; }

add_weight(w) {
    int max;

    max = level + 10;
    if (frog)
	max = max / 2;
    if (w + local_weight > max)
	return 0;
    local_weight += w;
    return 1;
}

shout_to_all(str) {
    if (spell_points < 0) {
	write("You are low on power.\n");
	return 1;
    }
    if (level < 20)
	spell_points -= 20;
    if (!str) {
	write("Shout what ?\n");
	return 1;
    }
    if (ghost) {
	write("You fail.\n");
	return 1;
    }
    shout(cap_name + " shouts: " + str + "\n");
    write("Ok.\n");
    return 1;
}

inventory() {
    object ob;
    if (test_dark())
	return 1;
    ob = first_inventory(myself);
    while(ob) {
	string str;
	str = call_other(ob, "short", 0);
	if (str) {
	    write(str + ".\n");
	    it = str;
	}
	ob = next_inventory(ob);
    }
    return 1;
}


examine(str) {
    return look("at " + str);
}

look(str) {
    object ob,ob2;
    string item;
    int max;
    if (test_dark())
	return 1;
    if (!str) {
	call_other(environment(), "long", 0);
	ob = first_inventory(environment());
	max = MAX_LIST;
	while(ob && max > 0) {
	    if (ob != myself) {
		string short_str;
		short_str = call_other(ob, "short", 0);
		if (short_str) {
		    max -= 1;
		    write(short_str + ".\n");
		    it = short_str;
		}
	    }
	    ob = next_inventory(ob);
	}
	return 1;
    }
    if (sscanf(str, "at %s", item) == 1 || sscanf(str, "in %s", item) == 1) {
	item = lower_case(item);
	ob = present(item, this_player());
	if (!ob && call_other(environment(this_player()), "id", item))
	    ob = environment(this_player());
	if (!ob)
	    ob = present(item, environment(this_player()));
	if (!ob) {
	    write("There is no " + item + " here.\n");
	    return 1;
	}
	it = item;
	call_other(ob, "long", item);
	if (!call_other(ob, "can_put_and_get", item))
	    return 1;
	ob2 = first_inventory(ob);
	while(ob2 && call_other(ob2, "short") == 0)
	    ob2 = next_inventory(ob2);
	if (ob2) {
	    if (living(ob))
		write("\t" + capitalize(item) + " is carrying:\n");
	    else
		write("\t" + capitalize(item) + " contains:\n");
	}
	max = MAX_LIST;
	while(ob2 && max > 0) {
	    string sh;
	    sh = call_other(ob2, "short", 0);
	    if (sh)
		write(sh + ".\n");
	    ob2 = next_inventory(ob2);
	    max -= 1;
	}
	return 1;
    }
    write("Look AT something, or what ?\n");
    return 1;
}

check_password(p)
{
    if (password == 0)
        write("You have no password ! Set it with the 'password' cmd.\n");
    else if (name != "guest" && crypt(p) != password) {
	write("Wrong password!\n");
	destruct(myself);
	return;
    }
    soul = 0;
    soul("on");
    move_player_to_start(0);
    log_file("ENTER", cap_name + " (" + name + ") entered with " + experience +
	     " ep, " + money + " gold.\n");
}

/*
 * Give a new password to a player.
 */
new_password(p)
{
    if (!p || p == "") {
	write("Try again another time then.\n");
	destruct(myself);
	return;
    }
    if (password == 0) {
	password = p;
	write("Password: (again) ");
	input_to("new_password");
	return;
    }
    if (password != p) {
	write("You changed !\n");
	destruct(myself);
	return;
    }
    password = crypt(password);
    call_other("room/adv_guild", "advance", 0);
    hit_point = max_hp;
    move_player_to_start(0);
    log_file("NEWPLAYER", cap_name + ".\n");
}

move_player_to_start(where) {
    object ob;
    string tmp_name;
    /*
     * See if we are already playing.
     * We must blank our own name, or we could find ourselves !
     */
    tmp_name = name;
    name = 0;
    other_copy = find_player(tmp_name);
    if (!other_copy)
	other_copy = find_player("ghost of " + tmp_name);
    name = tmp_name;
    if (other_copy) {
	write("You are already playing !\n");
	write("Throw the other copy out ? ");
	input_to("try_throw_out");
	return;
    }
    cat("/NEWS");
    if (where)
	move_object(myself, where);
    else {
	move_object(myself, "room/church");
	load_auto_obj(auto_load);
    }
    checked_say(cap_name + " enters the game.\n");
    if (query_invis(0))
        write("YOU ARE INVISIBLE = "+query_invis(0)+" !\n\n");
    if (muffled)
        write("YOU HAVE ON EAR MUFFS.\n");
    if (ghost)
        write("YOU ARE A GHOST!\n\n");
    call_other("room/post", "query_mail", 0);
    ob = first_inventory(environment());
    while(ob) {
	if (ob != this_object()) {
	    string sh;
	    sh = call_other(ob, "short");
	    if (sh)
		write(sh + ".\n");
	}
	ob = next_inventory(ob);
    }
}

help(what) {
    if (what) {
	cat("/doc/helpdir/" + what);
	return 1;
    }
    cat("/doc/help");
    return 1;
}

tell(str)
{
    object ob;
    string who;
    string msg;
    if (ghost) {
	write("You fail.\n");
	return 1;
    }
    if (spell_points < 0) {
	write("You are low on power.\n");
	return 1;
    }
    if (level < 20)
	spell_points -= 5;
    if (!str || sscanf(str, "%s %s", who, msg) != 2) {
	write("Tell what ?\n");
	return 1;
    }
    it = lower_case(who);
    ob = find_living(it);
    if (!ob) {
	write("No player with that name.\n");
	return 1;
    }
    tell_object(ob, cap_name + " tells you: " + msg + "\n");
    write("Ok.\n");
    return 1;
}

whisper(str)
{
    object ob;
    string who;
    string msg;
    if (ghost) {
	write("You fail.\n");
	return 1;
    }
    if (!str || sscanf(str, "%s %s", who, msg) != 2) {
	write("Whisper what ?\n");
	return 1;
    }
    it = lower_case(who);
    ob = find_living(it);
    if (!ob || !present(it, environment(this_player()))) {
	write("No player with that name in this room.\n");
	return 1;
    }
    tell_object(ob, cap_name + " whispers to you: " + msg + "\n");
    write("Ok.\n");
    checked_say(cap_name + " whispers something to " + who + ".\n");
    return 1;
}

add_hit_point(arg) {
    hit_point += arg;
    if (hit_point > max_hp)
	hit_point = max_hp;
    if (hit_point < 0)
	hit_point = 0;
}

add_spell_point(arg) {
    spell_points += arg;
    if (spell_points > max_hp)
	spell_points = max_hp;
    /* spell points can go negative */
}


/*
 * This routine is called from other routines to drop one specified object.
 * We return true if success.
 */

drop_one_item(ob)
{
    int weight;

    if ((call_other(ob, "drop", 0) && !power) ||
        call_other(ob,"id","soul"))
	    return 0;
    weight = call_other(ob, "query_weight", 0);
    if (!weight)
	weight = 0;
    add_weight(-weight);
    move_object(ob, environment(myself));
    return 1;
}

drop_all(verbose)
{
    object ob;
    object next_ob;
    if (!myself || !living(myself))
        return;
    ob = first_inventory(myself);
    while(ob) {
	string out;
	next_ob = next_inventory(ob);
	it = call_other(ob, "short", 0);
	if (drop_one_item(ob) && verbose) {
	    out = it + ".\n";
	    checked_say(cap_name + " drops " + out);
	    tell_object(myself, "drop: " + out);
	}
	ob = next_ob;
    }
}

/*
 * Check that a player name is valid. Only allow
 * lowercase letters.
 */
valid_name(str)
{
    int i, length;
    length = strlen(str);
    if (length > 11) {
	write("Too long name.\n");
	log_file("BAD_NAME", str + "\n");
	return 0;
    }
    i=0;
    while(i<length) {
	if (str[i] < 'a' || str[i] > 'z') {
	    write("Invalid characters in name:" + str + "\n");
	    write("Character number was " + i + ".\n");
	    log_file("BAD_NAME", str + "\n");
	    return 0;
	}
	i += 1;
    }
    return 1;
}

/*
 * This one is called when the player wants to change his password.
 */
change_password(str)
{
    if (password != 0 && !str) {
	write("Give old password as an argument.\n");
	return 1;
    }
    if (password != 0 && password != crypt(str)) {
	write("Wrong old password.\n");
	return 1;
    }
    write("New password: ");
    password2 = 0;
    input_to("change_password2");
    return 1;
}

change_password2(str)
{
    if (!str) {
	write("Password not changed.\n");
	return;
    }
    if (password2 == 0) {
	password2 = str;
	write("Again: ");
	input_to("change_password2");
	return;
    }
    if (password2 != str) {
	write("Wrong! Password not changed.\n");
	return;
    }
    password = crypt(password2);
    password2 = 0;
    write("Password changed.\n");
}

bug(str)
{
    if (!str) {
	write("Give an argument.\n");
	return 1;
    }
    log_file("BUGS", "\n");
    log_file("BUGS", cap_name + ":\n");
    log_file("BUGS", str + "\n");
    smart_report("Bug " + cap_name + "\n" + str);
    write("Ok.\n");
    return 1;
}

typo(str)
{
    if (!str) {
	write("Give an argument.\n");
	return 1;
    }
    log_file("TYPO", cap_name + ":\n");
    log_file("TYPO", str + "\n");
    smart_report("Typo " + cap_name + "\n" + str);
    write("Ok.\n");
    return 1;
}

idea(str)
{
    if (!str) {
	write("Give an argument.\n");
	return 1;
    }
    log_file("IDEA", cap_name + ":\n");
    log_file("IDEA", str + "\n");
    smart_report("Idea " + cap_name + "\n" + str);
    write("Ok.\n");
    return 1;
}

converse()
{
    write("Give '**' to stop.\n");
    input_to("converse_more");
    return 1;
}

converse_more(str)
{
    if (!str) {
	input_to("converse_more");
	return;
    }
    if (str == "**") {
	write("Ok.\n");
	return;
    }
    say(cap_name + " says: " + str + "\n");
    input_to("converse_more");
}

toggle_whimpy()
{
    whimpy = !whimpy;
    if (whimpy)
        write("Wimpy mode.\n");
    else
        write("Brave mode.\n");
    return 1;
}

query_brief() { return brief; }

toggle_brief()
{
    brief = !brief;
    if (brief)
        write("Brief mode.\n");
    else
        write("Verbose mode.\n");
    return 1;
}

add_exp(e) {
    experience += e;
    if (level <= 19)
	add_worth(e);
}

add_intoxination(i) {
    intoxicated += i;
    if(intoxicated < 0)
	intoxicated = 0;
}

query_intoxination() {
    return intoxicated;
}

second_life() {
    if (level >= 20)
	return illegal_patch("set_level");
    ghost = 1;
    msgin = "drifts around";
    msgout = "blows";
    headache = 0;
    intoxicated = 0;
    hunter = 0;
    hunted = 0;
    attacker_ob = 0;
    alt_attacker_ob = 0;
    save_me();
    tell_object(myself, "\nYou die.\nYou have a strange feeling.\n" +
		"You can see your own dead body from above.\n\n");
    return 1;
}

remove_ghost() {
    if (!ghost)
	return 0;
    write("You feel a very strong force.\n");
    write("You are sucked away...\n");
    write("You reappear in a more solid form.\n");
    say("Some mist disappears.\n");
    say(cap_name + " appears in a solid form.\n");
    ghost = 0;
    msgin = "arrives";
    msgout = "leaves";
    /* Get us to the appropriate level for our experience */
    call_other("room/adv_guild", "correct_level", this_player());
    save_me();
    return 1;
}

stop_hunting_mode()
{
    if (!hunted) {
        write("You are not hunting anyone.\n");
	return 1;
    }
    call_other(hunted, "stop_hunter");
    hunted = 0;
    write("Ok.\n");
    return 1;
}

drink_alcohol(strength)
{
    if (intoxicated > level + 3 && strength > 0) {
	return 0;   /* He's too drunk to drink any more! */
    }
    intoxicated += strength;
    if (intoxicated < 0)
	intoxicated = 0;
    if (intoxicated == 0)
	write("You are completely sober.\n");
    if (intoxicated > 0 && headache) {
	headache = 0;
	tell_object(myself, "Your head ache disappears.\n");
    }
    if (intoxicated > max_headache)
	max_headache = intoxicated;
    if (max_headache > 8)
	max_headache = 8;
    return 1;
}

spell_missile(str)
{
    object ob;
    if (test_dark())
	return 1;
    if (level < 5)
	return 0;
    if (!str)
	ob = attacker_ob;
    else
	ob = present(lower_case(str), environment(this_player()));
    if (!ob || !living(ob)) {
	write("At whom?\n");
	return 1;
    }
    if (ob == myself) {
	write("You manage to duck and only singe your hair.\n");
	return 1;
    }
    missile_object(ob);
    return 1;
}

spell_shock(str)
{
    object ob;
    if (test_dark())
	return 1;
    if (level < 10)
	return 0;
    if (!str)
	ob = attacker_ob;
    else
	ob = present(lower_case(str), environment(this_player()));
    if (!ob || !living(ob)) {
	write("At whom?\n");
	return 1;
    }
    if (ob == myself) {
	write("You put a few thousand volts through yourself.\n");
	return 1;
    }
    shock_object(ob);
    return 1;
}

spell_fire_ball(str)
{
    object ob;
    if (test_dark())
	return 1;
    if (level < 15)
	return 0;
    if (!str)
	ob = attacker_ob;
    else
	ob = present(lower_case(str), environment(this_player()));
    if (!ob || !living(ob)) {
	write("At whom?\n");
	return 1;
    }
    if (ob == myself) {
	write("Your fireball burns off half of your hair!\n");
	return 1;
    }
    fire_ball_object(ob);
    return 1;
}

give_object(str)
{
    string item, dest;
    object item_ob, dest_ob;
    int weight;
    int coins;

    power = 0;
    if (level >= ITEM_OVER)
        if (sscanf(str,"! %s",power) == 1) {
            str = power;
	    power = "!";
        }
    if (!str)
	return 0;
    if (test_dark())
	return 1;
    if (sscanf(str, "%d coins to %s", coins, dest) == 2)
	item = 0;
    else if ( sscanf(str, "1 coin to %s", dest) == 1)
	coins = 1;
    else if ( sscanf(str, "coin to %s", dest) == 1)
	coins = 1;
    else if (sscanf(str, "one coin to %s", dest) == 1)
	coins = 1;
    else if (sscanf(str, "%s to %s", item, dest) != 2) {
	write("Give what to whom?\n");
	return 1;
    }
    dest = lower_case(dest);
    if (item) {
	item = lower_case(item);
	item_ob = present(item, this_player());
	if (!item_ob) {
	    write("There is no " + item + " here.\n");
	    return 1;
	}
	it = item;
	if (environment(item_ob) == this_object() &&
	    call_other(item_ob, "drop", 0) == 1 && !power) {
	    return 1;
	} else {
	    if (!call_other(item_ob, "get") && !power) {
		write("You can't get that !\n");
		return 1;
	    }
	}
    }
    dest_ob = present(dest, environment(this_player()));
    if (!dest_ob) {
	write("There is no " + capitalize(dest) + " here.\n");
	return 1;
    }
    if (!living(dest_ob) && !power) {
	write("You can't do that.\n");
	return 1;
    }
    if (!item) {
	if (coins <= 0 && !power)
	    return 0;
	if (money < coins && !power) {
	    write("You don't have that much money.\n");
	    return 1;
	}
	if (!power) money -= coins;
	/* Checkpoint the character, to prevent cheating */
	if (coins > 1000 && !power)
	    save_me();
	call_other(dest_ob, "add_money", coins);
	if (coins != 1)
	    checked_say(cap_name + " gives " + coins + " coins to " + capitalize(dest) +
	    ".\n");
	else
	    checked_say(cap_name + " gives 1 coin to " + capitalize(dest) + ".\n");
	write("Ok.\n");
	return 1;
    }
    weight = call_other(item_ob, "query_weight", 0);
    if (!call_other(dest_ob, "add_weight", weight) && !power) {
	write(capitalize(dest) + " can't carry any more.\n");
	return 1;
    }
    add_weight(-weight);
    move_object(item_ob, dest_ob);
    checked_say(cap_name + " gives " + item + " to " + capitalize(dest) + ".\n");
    write("Ok.\n");
    return 1;
}

/*
 * Get all items here.
 */
get_all(from)
{
    object ob, next_ob;

    ob = first_inventory(from);
    while(ob) {
	string item;
	next_ob = next_inventory(ob);
	item = call_other(ob, "short", 0);
	if ((item && call_other(ob, "get", 0)) || (power && ob!=myself)) {
	    int weight;
	    weight = call_other(ob, "query_weight", 0);
	    if (add_weight(weight) || power) {
		write(item + ": Ok.\n");
		move_object(ob, this_object());
		checked_say(cap_name + " takes: " + item + ".\n");
	    } else {
		write(item + ": Too heavy.\n");
	    }
	    it = item;
	}
	ob = next_ob;
    }
}

pose() {
    if (level >= 15) {
        if (spell_points >= 15) {
	    write("You send a ball of fire into the sky.\n");
	    checked_say(cap_name + " makes a magical gesture.\n");
	    shout("A ball of fire explodes in the sky.\n");
	    spell_points -= 15;
	} else {
	    write("You are too low on power.\n");
	}
	return 1;
    }
    return 0;
}

save_me()
{
    compute_auto_str();
    save_object("players/" + name);
    save_counter = 0;
}

illegal_patch(what) {
    write("You are struck by a mental bolt from the interior of the game.\n");
    log_file("ILLEGAL",
	     call_other(this_player(), "query_name") + " " +
	     what + "\n");
    return 0;
}

load_auto_obj(str) {
    string file, argument, rest;
    object ob;

    while(str && str != "") {
	if (sscanf(str, "%s:%s,%s", file, argument, rest) != 3) {
	    write("Auto load string corrupt.\n");
	    return;
	}
	str = rest;
	ob = find_object(file);
	if (!ob)
	    continue;
	ob = clone_object(file);
	if (argument)
	    call_other(ob, "init_arg", argument);
	move_object(ob, this_object());
    }
}

compute_auto_str() {
    object next_ob,ob;
    string str;

    auto_load = "";
    ob = first_inventory(this_object());
    while(ob) {
	str = call_other(ob, "query_auto_load");
	next_ob = next_inventory(ob);
	ob = next_ob;
	if (!str)
	    continue;
	auto_load = auto_load + str + ",";
    }
}

smart_report(str) {
    string who;

    if (current_room == 0)
	return;
    if (sscanf(current_room, "players/%s/", who) != 1)
	return;
    log_file(who + ".rep", current_room + " " + str + "\n");
}

valid_write(arg) {
    string str, who, file, temp;

    str = arg;
    if (str == "~" || str == "~/") 
        str = "/players/" + name;
    else if (sscanf(str,"~/%s",temp) == 1)
	str = "/players/" + name + "/" + temp;
    else if (sscanf(str,"~%s",temp) == 1)
        str = "/players/" + temp;
    else if (str[0] != '/')
	str = pwd + str;
    if (sscanf(str, "/players/%s/%s", who, file) == 2) {
	if (who == name || level >= ED_OTHERS)
	    return "players/" + who + "/" + file;
	return 0;
    }
    if (sscanf(str, "/log/%s.%s", who,file) == 2)
        if (who == name || (capitalize(who) != who && level >= ED_OTHERS)
	    || level >=ED_LOG)
		    return "log/" + who + "." + file;
    if (sscanf(str, "/log/%s", who) == 1)
        if (who == name || (capitalize(who) != who && level >= ED_OTHERS)
	    || level >=ED_LOG)
		    return "log/" + who;
    if (sscanf(str, "/open/%s", file) == 1)
	return "open/" + file;
    if (level >= ALL_POWER) {
        sscanf(str,"/%s",file);
        return file;
    }
}

valid_read(arg) {
    string str, who, file, temp;

    file = valid_write(arg);
    if (file)
	return file;
    str = arg;
    if (str == "~" || str == "~/") 
        str = "/players/" + name;
    else if (sscanf(str,"~/%s",temp) == 1)
	str = "/players/" + name + "/" + temp;
    else if (sscanf(str,"~%s",temp) == 1)
        str = "/players/" + temp;
    else if (str[0] != '/')
	str = pwd + str;
    if (sscanf(str, "/players/%s/%s", who, file) == 2) {
	if (who == name || level >= READ_OTHERS)
	    return "players/" + who + "/" + file;
	return 0;
    }
    if (sscanf(str, "/players/%s.o", who) == 1) {
	if (who == name || level >= ELDER)
	    return "players/" + who + ".o";
	return 0;
    }
    if (sscanf(str, "/room/post_dir/%s.o", who) == 1) {
	if (who == name || level >= ALL_POWER)
	    return "room/post_dir/" + who + ".o";
	return 0;
    }
    if (sscanf(str, "/%s", file) == 1)
	return file;
    write("Bad file name.\n");
    return 0;		/* Should not happen */
}

soul(str) {
    if (!str) return 0;
    if (str == "on") {
        if (soul) {
	    write("You already have one.\n");
	    return 1;
	}
	if (level > 19)
	    soul = clone_object("obj/wiz_soul");
	if (level < 20)
	    soul = clone_object("obj/soul");
        move_object(soul,myself);
	return 1;
    }
    if (str == "off") {
        if (!soul) {
	    write("You don't have one.\n");
	    return 1;
	}
	save_me();	
        destruct(soul);
	return 1;
    }
}

describe(str) {
    string junk1,junk2;
    if (!str) {
	description = 0;
	write("Your description has been cleared.\n");
	return 1;
    }
    if (str) description = " " + str;
    write("Your description now says:\n");
    write(cap_name + description + ".\n");
    return 1;
}

remote_ed(key) {
    string file;

    file = call_other(soul,"get_handshake",key);
    file = valid_write(file);
    if (!file) {
	write("You can only edit your own files.\n");
	return 1;
    }
    tell_object(myself,"editing: "+file +"\n");
    ed(file);
}

remote_cmd(key) {
    string str;
    str = call_other(soul,"get_handshake",key);
    if (str)
        command(str);
}

remote_snoop(key) {
    object ob;
    int ob_level;
    string str;

    str = call_other(soul,"get_handshake",key);
    if (!str) {
	snoop();
	return 1;
    }
    ob = find_living(str);
    if (!ob) {
	write("No such player.\n");
	return 1;
    }
    ob_level = call_other(ob, "query_level");
    if (ob_level >= level && level < ALL_POWER) {
	write("You fail.\n");
	return 1;
    }
    snoop(ob);
}

query_soul() { return soul; }

remote_say(str) { say(str); }

set_wc(num) {
    weapon_class = num;
}

set_ac(num) {
    armor_class = num;
}

query_muffled() { return muffled; }

emergency(str) {
    if (!str) {
	write("Shout what ?\n");
	return 1;
    }
    shout("!" + cap_name + " screams: " + str + "\n");
    log_file("EMERGENCY",cap_name + ": " + str + "\n");
    write("Ok. But it better be an emergency!\n");
    return 1;
}

set_pwd(str) {
    if (!str) {
	write("Null path!\n");
	return;
    }
    if (str[0] == '/')
	pwd = str;
    else
	pwd = "/" + str;
}

update (num) {
    if (num == 1) msgin = call_other(soul,"query_msgin",0);
    if (num == 2) mmsgin = call_other(soul,"query_mmsgin",0);
    if (num == 3) msgout = call_other(soul,"query_msgout",0);
    if (num == 4) mmsgout = call_other(soul,"query_mmsgout",0);
    if (num == 5) {
        is_invis = call_other(soul,"query_invis",0);
	if (query_invis() > SOMEONE) { cap_name = "Someone";
	} else { cap_name = capitalize(name); }
    }
    if (num == 6) muffled = call_other(soul,"query_muffled",0);
    if (num == 7) alignment = call_other(soul,"query_alignment",0);
    if (num == 8) al_title = call_other(soul,"query_al_title",0);
    if (num == 9) msghome = call_other(soul,"query_msghome",0);
}

is_player() {
	return 1;
}
