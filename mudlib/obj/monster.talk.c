/*
 * A general purpose monster. Clone this object,
 * and call local functions to configure it.
 */
#include "living.h"

/*
 * The heart beat is always started in living.h when we are attacked.
 */

string short_desc, long_desc, alias, race;
int move_at_reset, aggressive, can_kill;
object kill_ob;
object desc_ob;

object chat_head;
int chat_chance;
int chat_nr;

object a_chat_head;
int a_chat_chance;
int a_chat_nr;

object head;
object talk_ob;
string talk_func;
string talk_match;
string talk_type;
string the_text;
int have_text;


object dead_ob;
object init_ob;

int random_pick;

int spell_chance, spell_dam;
string spell_mess1, spell_mess2;
object me;

set_desc_ob(ob) { desc_ob = ob; }

valid_attack(ob)
{
    /* Don't attack other monsters unless specifically allowed to. */
    return (!call_other(ob, "query_npc", 0) || can_kill);
}

reset(arg)
{
    if (arg) {
	if (move_at_reset)
	    random_move();
	return;
    }
    is_npc = 1;
    /* Only let this monster kill other monsters if specifically allowed. */
    can_kill = 0;
    enable_commands();
    me = this_object();
}

random_move()
{
    int n;
    n = random(4);
    if (n == 0)
	command("west");
    else if (n == 1)
	command("east");
    else if (n == 2)
	command("south");
    else if (n == 3)
	command("north");
}

short() {
    if (desc_ob) return call_other(desc_ob,"monster_short",0);
    return short_desc;
}

long() {
    if (desc_ob) {
        call_other(desc_ob,"monster_long",0);
	return;
    }
    write (long_desc);
}

id(str) {
    if (desc_ob) return call_other(desc_ob,"monster_id",str);
    return str == name || str == alias || str == race;
}

heart_beat()
{
    int c;

    age += 1;
    /* If there is none here test_if_any_here will turn of heat_beat */
    if(!test_if_any_here()) {
	if(have_text && head) {
	    have_text = 0;
	    call_other(head, "test_match", the_text);
	} else {
	    set_heart_beat(0);
	    return;
	}
    }
    if (kill_ob && present(kill_ob, environment(this_object()))) {
	if (random(2) == 1)
	    return;		/* Delay attack some */
	attack_object(kill_ob);
	kill_ob = 0;
	return;
    }
    if (attacker_ob && present(attacker_ob, environment(this_object())) &&
      spell_chance > random(100)) {
	say(spell_mess1 + "\n", attacker_ob);
	tell_object(attacker_ob, spell_mess2 + "\n");
	call_other(attacker_ob, "hit_player", random(spell_dam));
    }
    attack();
    if (attacker_ob && whimpy && hit_point < max_hp/5)
	run_away();
    if(chat_head || a_chat_head){
	c = random(100);
	if(attacker_ob && a_chat_head) {
	    if(c < a_chat_chance){
		c = random(a_chat_nr);
		call_other(a_chat_head,"chat",c);
	    }
	} else {
	    if(c < chat_chance && chat_head){
		c = random(chat_nr);
		call_other(chat_head,"chat",c);
	    }
	}
    }
    if(random_pick) {
	c = random(100);
	if(c < random_pick)
	    pick_any_obj();
    }
    if(have_text && head) {
	have_text = 0;
	call_other(head, "test_match", the_text);
    }
}

can_put_and_get(str)
{
    if (!str)
	return 0;
    return 1;
}

int busy_catch_tell;

catch_tell(str) {
    string who;

    if (busy_catch_tell)    /* Should not happen, but does! */
	return;
    busy_catch_tell = 1;
    if(head) {
	if(have_text) {
	    who = the_text;
	    the_text = str;
	    have_text = 1;
	    call_other(head, "test_match", the_text);
	} else {
	    the_text = str;
	    have_text = 1;
	}
    }
    busy_catch_tell = 0;
}

/*
 * Call the following functions to setup the monster.
 * Call them in the order they appear.
 */

set_name(n) {
    name = n;
    alignment = 0;		/* Neutral monster */
    cap_name = capitalize(n);
    short_desc = cap_name;
    long_desc = "You see nothing special.\n";
    move_object(clone_object("obj/soul"), this_object());
}

set_level(l) {
    level = l;
    weapon_class = WEAPON_CLASS_OF_HANDS;
    armor_class = 0;
    hit_point = 50 + (level - 1) * 8;	/* Same as a player */
    max_hp = hit_point;
    experience = call_other("room/adv_guild", "query_cost", l-1);
}

/* Optional */
set_alias(a) { alias = a; }
/* Optional */
set_race(r) { race = r; }
/* optional */
set_hp(hp) { max_hp = hp; hit_point = hp; }
/* optional */
set_ep(ep) { experience = ep; }
/* optional */
set_al(al) {
    /* Limit alignment to keep wizards from making their monsters worth
       much too much */
    if (al > 1000)
	al = 1000;
    if (al < -1000)
	al = -1000;
    alignment = al;
}
/* optional */
set_short(sh) { short_desc = sh; long_desc = short_desc + "\n";}
/* optional */
set_long(lo) { long_desc = lo; }
/* optional */
set_wc(wc) { weapon_class = wc; }
/* optional */
set_ac(ac) { armor_class = ac; }
/* optional */
set_move_at_reset() { move_at_reset = 1; }
/* optional
 * 0: Peaceful.
 * 1: Attack on sight.
 */
set_aggressive(a) {
    aggressive = a;
}
/* optional
 * 0: Can't attack other monsters.
 * 1: Can attack other monsters.
 */
set_can_kill(a) {
    can_kill = a;
}
/*
 * Now some functions for setting up spells !
 */
/*
 * The percent chance of casting a spell.
 */
set_chance(c) {
    spell_chance = c;
}
/* Message to the victim. */
set_spell_mess1(m) {
    spell_mess1 = m;
}
set_spell_mess2(m) {
    spell_mess2 = m;
}
set_spell_dam(d) {
    spell_dam = d;
}

/* Set the frog curse. */
set_frog() {
    frog = 1;
}

/* Set the whimpy mode */
set_whimpy() {
    whimpy = 1;
}

/*
 * Force the monster to do a command. The force_us() function isn't
 * always good, because it checks the level of the caller, and this function
 * can be called by a room.
 */
init_command(cmd) {
    command(cmd);
}

/* Load chat */

set_chat_chance(c) {
    chat_chance = c;
}

remove_chat(str) { 
    chat_nr -= 1;
    chat_head = call_other(chat_head,"remove_chat",str);
}


load_chat(str) {
    object old;

    chat_nr += 1;
    if(chat_head)
	old = chat_head;
    chat_head = clone_object("obj/chat");
    call_other(chat_head, "load_chat", str);
    call_other(chat_head, "set_monster", this_object());
    if(old)
	call_other(chat_head, "link", old);
}

/* Load attack chat */

set_a_chat_chance(c) {
    a_chat_chance = c;
}

remove_a_chat(str) { 
    a_chat_nr -= 1;
    head = call_other(a_chat_head,"remove_chat",str);
}


load_a_chat(str) {
    object old;

    a_chat_nr += 1;
    if(a_chat_head)
	old = a_chat_head;
    a_chat_head = clone_object("obj/chat");
    call_other(a_chat_head, "load_chat", str);
    call_other(a_chat_head, "set_monster", this_object());
    if(old)
	call_other(a_chat_head, "link", old);
}

/* Catch the talk */

set_object(ob) { 
    talk_ob = ob; 
}

set_function(func) { 
    talk_func = func; 
}

set_type(type) { 
    talk_type = type; 
}

set_match(match) {
    object old;

    talk_match = match; 
    if(head)
	old = head;
    head = clone_object("obj/catch_talk");
    call_other(head, "set_object", talk_ob);
    call_other(head, "set_function", talk_func);
    call_other(head, "set_match", talk_match);
    call_other(head, "set_type", talk_type);
    if(old)
	call_other(head, "link", old);
}

remove_match(match) { 
    head = call_other(head,"remove_match",match);
}

set_dead_ob(ob)
{
    dead_ob = ob;
}

second_life()
{
    /* We have died remove chat and catch_talk */
    if(head)
	call_other(head,"collaps");
    if(chat_head)
	call_other(chat_head,"collaps");
    if(a_chat_head)
	call_other(a_chat_head,"collaps");
    if(dead_ob)
	call_other(dead_ob,"monster_died",this_object());
}

set_random_pick(r)
{
    random_pick = r;
}

pick_any_obj() {
    object ob;
    int weight;

    ob = first_inventory(environment(this_object()));
    while(ob) {
	if (call_other(ob, "get", 0) && call_other(ob, "short")) {
	    weight = call_other(ob, "query_weight", 0);
	    if (!add_weight(weight)) {
		say(cap_name + " tries to take " + call_other(ob, "short", 0) +
		    " but fails.\n");
		return;
	    }
	    move_object(ob, this_object());
	    say(cap_name + " takes " + call_other(ob, "short", 0) + ".\n");
	    if (call_other(ob, "weapon_class", 0))
		call_other(ob, "wield", call_other(ob,"query_name"));
	    else if (call_other(ob, "armor_class", 0))
		call_other(ob, "wear", call_other(ob,"query_name"));
	    return;
	}
	ob = next_inventory(ob);
    }
}

dumpa(str)
{
    if(!str)
	str = "MONSTER_DUMP";
    save_object(str);
}

set_init_ob(ob)
{
    init_ob = ob;
}


init() {

    if(this_player() == me)
	return;
    if(dead_ob)
	if(call_other(dead_ob,"monster_init",this_object()))
	    return;
    if (attacker_ob) {
	set_heart_beat(1); /* Turn on heart beat */
    }
    if(this_player() && !call_other(this_player(),"query_npc")) {
	set_heart_beat(1);
	if (aggressive == 1)
	    kill_ob = this_player();
    }
}
