#include "log.h"

#define KILL_NEUTRAL_ALIGNMENT		10
#define ADJ_ALIGNMENT(al)		((-al - KILL_NEUTRAL_ALIGNMENT)/4)
#define NAME_OF_GHOST			"some mist"

/*
 * If you are going to copy this file, in the purpose of changing
 * it a little to your own need, beware:
 *
 * First try one of the following:
 *
 * 1. Do clone_object(), and then configur it. This object is specially
 *    prepared for configuration.
 *
 * 2. If you still is not pleased with that, create a new empty
 *    object, and make an inheritance of this objet on the first line.
 *    This will automatically copy all variables and functions from the
 *    original object. Then, add the functions you want to change. The
 *    original function can still be accessed with '::' prepended on the name.
 *
 * The maintainer of this LPmud might become sad with you if you fail
 * to do any of the above. Ask other wizards if you are doubtful.
 *
 * The reason of this, is that the above saves a lot of memory.
 */

/*
 * Include this file in objects that "lives".
 * The following variables are defined here:
 *
 */

int time_to_heal;	/* Count down variable. */
int money;		/* Amount of money on player. */
string name;		/* Name of object. */
string msgin, msgout;	/* Messages when entering or leaving a room. */
int is_npc, brief;	/* Flags. */
int level;		/* Level of monster. */
static int armour_class;	/* What armour class of monster. */
int hit_point;		/* Number of hit points of monster. */
int max_hp, max_sp;
int experience;		/* Experience points of monster. */
string mmsgout;		/* Message when leaving magically. */
string mmsgin;		/* Message when arriving magically. */
static object attacker_ob;	/* Name of player attacking us. */
static object alt_attacker_ob;	/* Name of other player also attacking us. */
static int weapon_class;	/* How good weapon. Used to calculate damage. */
static object name_of_weapon;	/* To see if we are wielding a weapon. */
static object head_armour;	/* What armour we have. */
int ghost;		/* Used for monsters that can leave a ghost. */
static int local_weight;	/* weight of items */
static object hunted, hunter;	/* used in the hunt mode */
static int hunting_time;	/* How long we will stay in hunting mode. */
static string cap_name;	/* Capital version of "name". */
int spell_points;	/* Current spell points. */
static string spell_name;
static int spell_cost, spell_dam;
int age;		/* Number of heart beats of this character. */
int is_invis;		/* True when player is invisible */
int frog;		/* If the player is a frog */
int whimpy;		/* Automatically run when low on HP */
string auto_load;	/* Special automatically loaded objects. */
int dead;		/* Are we alive or dead? */
string flags;		/* Bit field of flags */

/*
 * All characters have an aligment, depending on how good or chaotic
 * they are.
 * This value is updated when killing other players.
 */
int alignment;
int gender;	/* 0 means neuter ("it"), 1 male ("he"),  2 female ("she") */

/*
 * Character stat variables.
 */
int Str, Int, Con, Dex;

/*
 * The following routines are defined for usage:
 * stop_fight		Stop a fight. Good for scroll of taming etc.
 * hit_player		Called when fighting.
 * transfer_all_to:	Transfer all objects to dest.
 * move_player:		Called by the object that moves the monster.
 * query_name:		Gives the name to external objects.
 * attacked_by		Tells us who are attacking us.
 * show_stats		Dump local status.
 * stop_wielding	Called when we drop a weapon.
 * stop_wearing		Called when we drop an armour.
 * query_level		Give our level to external objects.
 * query_value		Always return 0. Can't sell this object.
 * query_npc		Return 1 if npc otherwise 0.
 * get			Always return 0. Can't take this object.
 * attack		Should be called from heart_beat. Will maintain attack.
 * query_attack
 * drop_all_money	Used when the object dies.
 * wield		Called by weapons.
 * wear			Called by armour.
 * add_weight		Used when picking up things.
 * heal_self		Enable wizards to heal this object.
 * can_put_and_get	Can look at inventory, but not take things from it.
 * attack_object	Called when starting to attack an object.
 * test_if_any_here	For monsters. Call this one if you suspect no enemy
 *			is here any more.
 *			Return 1 if anyone there, 0 if none.
 * force_us		Force us to do a command.
 * query_spell_points	Return how much spell points the character has.
 * reduce_hit_point	Reduce hit points, but not below 0.
 */

void attack_object(object ob);
void stop_fight();
void transfer_all_to(object dest);
string short();
string query_gender_string();
string query_stats();
void show_age();

/*
 * This routine is called from objects that moves the player.
 * Special: direction "X" means teleport.
 * The argument is "how#where".
 * The second optional argument is an object to move the player to.
 * If the second argument exists, then the first argument is taken
 * as the movement message only.
 */
void move_player(string dir_dest, object optional_dest_ob)
{
    string dir;
    object dest;
    object ob;
    int is_light, i;

    if (!optional_dest_ob) {
	if (sscanf(dir_dest, "%s#%s", dir, dest) != 2) {
	    tell_object(this_object(), "Move to bad dir/dest\n");
	    return;
	}
    } else {
	dir = dir_dest;
	dest = optional_dest_ob;
    }
    hunting_time -= 1;
    if (hunting_time == 0) {
	if (hunter)
	    hunter->stop_hunter();
	hunter = 0;
	hunted = 0;
    }
    if (attacker_ob && present(attacker_ob)) {
	hunting_time = 10;
	if (!hunter)
	    tell_object(this_object(), "You are now hunted by " +
			attacker_ob->query_name() + ".\n");
        hunter = attacker_ob;
    }
    is_light = set_light(0);
    if(is_light < 0)
	is_light = 0;
    if(is_light) {
	if (!msgout)
	    msgout = "leaves";
	if (ghost)
	    say(NAME_OF_GHOST + " " + msgout + " " + dir + ".\n");
	else if (dir == "X" && !is_invis)
	    say(cap_name + " " + mmsgout + ".\n");
	else if (!is_invis)
	    say(cap_name + " " + msgout + " " + dir + ".\n");
    }
    move_object(this_object(), dest);
    is_light = set_light(0);
    if(is_light < 0)
	is_light = 0;
    if (level >= 20) {
	if (!optional_dest_ob)
	    tell_object(this_object(), "/" + dest + "\n");
    }
    if(is_light) {
	if (!msgin)
	    msgin = "arrives";
	if (ghost)
	    say(NAME_OF_GHOST + " " + msgin + ".\n");
	else if (dir == "X" && !is_invis)
	    say(cap_name + " " + mmsgin + ".\n");
	else if (!is_invis)
	    say(cap_name + " " + msgin + ".\n");
    }
    if (hunted && present(hunted))
        attack_object(hunted);
    if (hunter && present(hunter))
        hunter->attack_object(this_object());
    if (is_npc)
	return;
    if (!is_light) {
	write("A dark room.\n");
	return;
    }
    ob = environment(this_object());
    if (brief)
	write(ob->short() + ".\n");
    else
	ob->long();
    for (i=0, ob=first_inventory(ob); ob; ob = next_inventory(ob)) {
	if (ob != this_object()) {
	    string short_str;
	    short_str = ob->short();
	    if (short_str)
		write(short_str + ".\n");
	}
	if (i++ > 40) {
	    write("*** TRUNCATED\n");
	    break;
	}
    }
}

/*
 * This function is called from other players when they want to make
 * damage to us. We return how much damage we received, which will
 * change the attackers score. This routine is probably called from
 * heart_beat() from another player.
 * Compare this function to reduce_hit_point(dam).
 */
int hit_player(int dam) {
    if (!attacker_ob)
	set_heart_beat(1);
    if (!attacker_ob && this_player() != this_object())
	attacker_ob = this_player();
    else if (!alt_attacker_ob && attacker_ob != this_player() &&
	     this_player() != this_object())
	alt_attacker_ob = this_player();
    /* Don't damage wizards too much ! */
    if (level >= 20 && !is_npc && dam >= hit_point) {
	tell_object(this_object(),
		    "Your wizardhood protects you from death.\n");
	return 0;
    }
    if(dead)
	return 0;	/* Or someone who is dead */
    dam -= random(armour_class + 1);
    if (dam <= 0)
	return 0;
    if (dam > hit_point+1)
	dam = hit_point+1;
    hit_point = hit_point - dam;
    if (hit_point<0) {
	object corpse;
	/* We died ! */
	
	if (!is_npc && !query_ip_number(this_object())) {
	    /* This player is linkdead. */
	    write(cap_name + " is not here. You cannot kill a player who is not logged in.\n");
	    hit_point = 20;
	    stop_fight();
	    if (this_player())
	        this_player()->stop_fight();
            return 0;
	}

	dead = 1;
	if (hunter)
	    hunter->stop_hunter();
	hunter = 0;
	hunted = 0;
	say(cap_name + " died.\n");
	experience = 2 * experience / 3;	/* Nice, isn't it ? */
	hit_point = 10;
	/* The player killing us will update his alignment ! */
	/* If he exist */
	if(attacker_ob) {
	    attacker_ob->add_alignment(ADJ_ALIGNMENT(alignment));
	    attacker_ob->add_exp(experience / 35);
	}
	corpse = clone_object("obj/corpse");
	corpse->set_name(name);
	transfer_all_to(corpse);
	move_object(corpse, environment(this_object()));
	if (!this_object()->second_life())
	    destruct(this_object());
	if (!is_npc)
	    save_object("players/" + name);
    }
    return dam;
}

void transfer_all_to(object dest)
{
    object ob;
    object next_ob;

    ob = first_inventory(this_object());
    while(ob) {
	next_ob = next_inventory(ob);
	/* Beware that drop() might destruct the object. */
	if (!ob->drop(1) && ob)
	    move_object(ob, dest);
	ob = next_ob;
    }
    local_weight = 0;
    if (money == 0)
	return;
    ob = clone_object("obj/money");
    ob->set_money(money);
    move_object(ob, dest);
    money = 0;
}

string query_name() {
    if (ghost)
	return NAME_OF_GHOST;
    return cap_name;
}

int query_alignment() {
    return alignment;
}

int query_npc() {
    return is_npc;
}

/*
 * This routine is called when we are attacked by a player.
 */
void attacked_by(object ob) {
    if (!attacker_ob) {
	attacker_ob = ob;
	set_heart_beat(1);
	return;
    }
    if (!alt_attacker_ob) {
	alt_attacker_ob = ob;
	return;
    }
}

void show_stats() {
    int i;
    write(short() + "\nlevel:\t" + level +
	  "\ncoins:\t" + money +
	  "\nhp:\t" + hit_point +
	  "\nmax:\t" + max_hp +
	  "\nspell\t" + spell_points +
	  "\nmax:\t" + max_sp);
    write("\nep:\t"); write(experience);
    write("\nac:\t"); write(armour_class);
    if (head_armour)
	write("\narmour: " + head_armour->rec_short());
    write("\nwc:\t"); write(weapon_class);
    if (name_of_weapon)
	write("\nweapon: " + name_of_weapon->query_name());
    write("\ncarry:\t" + local_weight);
    if (attacker_ob)
	write("\nattack: " + attacker_ob->query_name());
    if (alt_attacker_ob)
	write("\nalt attack: " + alt_attacker_ob->query_name());
    write("\nalign:\t" + alignment + "\n");
    write("gender:\t" + query_gender_string() + "\n");
    if (i = this_object()->query_quests())
	write("Quests:\t" + i + "\n");
    write(query_stats());
    show_age();
}

void stop_wielding() {
    if (!name_of_weapon) {
	/* This should not happen ! */
	log_file("wield_bug", "Weapon not wielded !\n");
	write("Bug ! The weapon was marked as wielded ! (fixed)\n");
	return;
    }
    name_of_weapon->un_wield(dead);
    name_of_weapon = 0;
    weapon_class = 0;
}

void stop_wearing(string name) {
    if(!head_armour) {
	/* This should not happen ! */
	log_file("wearing_bug", "armour not worn!\n");
	write("This is a bug, no head_armour\n");
	return;
    }
    head_armour = head_armour->remove_link(name);
    if(head_armour && objectp(head_armour))
	armour_class = head_armour->tot_ac();
    else {
	armour_class = 0;
	head_armour = 0;
    }
    if (!is_npc)
	if(!dead)
	    say(cap_name + " removes " + name + ".\n");
    write("Ok.\n");
}

int query_level() {
    return level;
}

/* This object is not worth anything in the shop ! */
int query_value() { return 0; }

/* It is never possible to pick up a player ! */
int get() { return 0; }

/*
 * Return true if there still is a fight.
 */
int attack()
{
    int tmp;
    mixed whit;
    string name_of_attacker;

    if (!attacker_ob) {
	spell_cost = 0;
	return 0;
    }
    name_of_attacker = attacker_ob->query_name();
    if (!name_of_attacker || name_of_attacker == NAME_OF_GHOST ||
	environment(attacker_ob) != environment(this_object())) {
	if (!hunter && name_of_attacker &&
	    !attacker_ob->query_ghost())
	{
	    tell_object(this_object(), "You are now hunting " +
			attacker_ob->query_name() + ".\n");
	    hunted = attacker_ob;
	    hunting_time = 10;
	}
	attacker_ob = 0;
	if (!alt_attacker_ob)
	    return 0;
	attacker_ob = alt_attacker_ob;
	alt_attacker_ob = 0;
	if (attack()) {
	    if (attacker_ob)
		tell_object(this_object(),
			    "You turn to attack " +
			    attacker_ob->query_name() + ".\n");
	    return 1;
	}
	return 0;
    }
    if (spell_cost) {
	spell_points -= spell_cost;
	tell_object(attacker_ob, "You are hit by a " + spell_name + ".\n");
	write("You cast a " + spell_name + ".\n");
    }
    if(name_of_weapon) {
	whit = name_of_weapon->hit(attacker_ob);
	if (!attacker_ob) {
	    tell_object(this_object(), "CRACK!\nYour weapon broke!\n");
	    log_file("BAD_SWORD", name_of_weapon->short() + ", " +
		     creator(name_of_weapon) + " XX !\n");
	    spell_cost = 0;
	    spell_dam = 0;
	    destruct(name_of_weapon);
	    weapon_class = 0;
	    return 1;
	}
    }
    if(whit != "miss") {
	tmp = ((weapon_class + whit) * 2 + Dex) / 3;
	if (tmp == 0)
	    tmp = 1;
	tmp = attacker_ob->hit_player(random(tmp) + spell_dam);
    } else
	tmp = 0;
    tmp -= spell_dam;
    if (!is_npc && name_of_weapon && tmp > 20 &&
      random(100) < weapon_class - level * 2 / 3 - 14) {
	tell_object(this_object(), "CRACK!\nYour weapon broke!\n");
	tell_object(this_object(),
		    "You are too inexperienced for such a weapon.\n");
	log_file("BAD_SWORD", name_of_weapon->short() + ", " +
		 creator(name_of_weapon) + "\n");
	spell_cost = 0;
	spell_dam = 0;
	destruct(name_of_weapon);
	weapon_class = 0;
	return 1;
    }
    tmp += spell_dam;
    if (tmp == 0) {
	tell_object(this_object(), "You missed.\n");
	say(cap_name + " missed " + name_of_attacker + ".\n");
	spell_cost = 0;
	spell_dam = 0;
	return 1;
    }
    experience += tmp;
    tmp -= spell_dam;
    spell_cost = 0;
    spell_dam = 0;
    /* Does the enemy still live ? */
    if (attacker_ob &&
      attacker_ob->query_name() != NAME_OF_GHOST) {
	string how, what;
	how = " to small fragments";
	what = "massacre";
	if (tmp < 30) {
	    how = " with a bone crushing sound";
	    what = "smash";
	}
	if (tmp < 20) {
	    how = " very hard";
	    what = "hit";
	}
	if (tmp < 10) {
	    how = " hard";
	    what = "hit";
	}
	if (tmp < 5) {
	    how = "";
	    what = "hit";
	}
	if (tmp < 3) {
	    how = "";
	    what = "grazed";
	}
	if (tmp == 1) {
	    how = " in the stomach";
	    what = "tickled";
	}
	tell_object(this_object(), "You " + what + " " + name_of_attacker +
		    how + ".\n");
	tell_object(attacker_ob, cap_name + " " + what + " you" + how +
		    ".\n");
	say(cap_name + " " + what + " " + name_of_attacker + how +
		    ".\n", attacker_ob);
	return 1;
    }
    tell_object(this_object(), "You killed " + name_of_attacker + ".\n");
    attacker_ob = alt_attacker_ob;
    alt_attacker_ob = 0;
    if (attacker_ob)
	return 1;
}

object query_attack() {
    /* Changed by Herder */
    return attacker_ob;
    /* OLD
    if (attacker_ob)
	return 1;
    return 0;
    */
}

void drop_all_money(int verbose) {
    object mon;
    if (money == 0)
	return;
    mon = clone_object("obj/money");
    mon->set_money(money);
    move_object(mon, environment());
    if (verbose) {
	say(cap_name + " drops " + money + " gold coins.\n");
	tell_object(this_object(), "You drop " + money + " gold coins.\n");
    }
    money = 0;
}

/* Wield a weapon. */
void wield(object w) {
    if (name_of_weapon)
	stop_wielding();
    name_of_weapon = w;
    weapon_class = w->weapon_class();
    say(cap_name + " wields " + w->query_name() + ".\n");
    write("Ok.\n");
}

/* Wear some armour. */
object wear(object a) {
    object old;

    if(head_armour) {
	old = head_armour->test_type(a->query_type());
	if(old)
	    return old;
	old = head_armour;
	a->link(old);
    }
    head_armour = a;
    /* Calculate new ac */
    armour_class = head_armour->tot_ac();
    say(cap_name + " wears " + a->query_name() + ".\n");
    write("Ok.\n");
    return 0;
}

int add_weight(int w) {
    if (w + local_weight > Str + 10 && level < 20)
	return 0;
    local_weight += w;
    return 1;
}

void  heal_self(int h) {
    if (h <= 0)
	return;
    hit_point += h;
    if (hit_point > max_hp)
	hit_point = max_hp;
    spell_points += h;
    if (spell_points > max_sp)
	spell_points = max_sp;
}

void restore_spell_points(int h) {
    spell_points += h;
    if (spell_points > max_sp)
	spell_points = max_sp;
}

int can_put_and_get(string str)
{
    return str != 0;
}

void attack_object(object ob)
{
   if (ob->query_ghost())
       return;
   set_heart_beat(1);	/* For monsters, start the heart beat */
   if (attacker_ob == ob) {
       attack();
       return;
   }
   if (alt_attacker_ob == ob) {
       alt_attacker_ob = attacker_ob;
       attacker_ob = ob;
       attack();
       return;
   }
   if (!alt_attacker_ob)
       alt_attacker_ob = attacker_ob;
   attacker_ob = ob;
   attacker_ob->attacked_by(this_object());
   attack();
}

int query_ghost() { return ghost; }

void zap_object(object ob)
{
    ob->attacked_by(this_object());
    say(cap_name + " summons a flash from the sky.\n");
    write("You summon a flash from the sky.\n");
    experience += ob->hit_player(100000);
    write("There is a big clap of thunder.\n\n");
}

void missile_object(object ob)
{
    if (spell_points < 10) {
	write("Too low on power.\n");
	return;
    }
    spell_name = "magic missile";
    spell_dam = random(20);
    spell_cost = 10;
    attacker_ob = ob;
}

void shock_object(object ob)
{
    if (spell_points < 15) {
	write("Too low on power.\n");
	return;
    }
    spell_name = "shock";
    spell_dam = random(30);
    spell_cost = 15;
    attacker_ob = ob;
}

void fire_ball_object(object ob)
{
    if (spell_points < 20) {
	write("Too low on power.\n");
	return;
    }
    spell_name = "fire ball";
    spell_dam = random(40);
    spell_cost = 20;
    attacker_ob = ob;
}

/*
 * If no one is here (except ourself), then turn off the heart beat.
 */

int test_if_any_here()
{
    object ob;
    ob = environment();
    if (!ob)
	return 0;
    ob = first_inventory(environment());
    while(ob) {
	if (ob != this_object() && living(ob) && !ob->query_npc())
	    return 1;
	ob = next_inventory(ob);
    }
    return 0;
}

void show_age() {
    int i;

    write("age:\t");
    i = age;
    if (i/43200) {
	write(i/43200 + " days ");
	i = i - (i/43200)*43200;
    }
    if (i/1800) {
	write(i/1800 + " hours ");
	i = i  - (i/1800)*1800;
    }
    if (i/30) {
	write(i/30 + " minutes ");
	i = i - (i/30)*30;
    }
    write(i*2 + " seconds.\n");
}

void stop_hunter()
{
    hunter = 0;
    tell_object(this_object(), "You are no longer hunted.\n");
}

/*
 * This function remains only because of compatibility, as command() now
 * can be called with an object as argument.
 */
void force_us(string cmd) {
    if (!this_player() || this_player()->query_level() <= level ||
	query_ip_number(this_player()) == 0) {
	tell_object(this_object(), this_player()->query_name() +
	    " failed to force you to " + cmd + "\n");
	return;
    }
    command(cmd);
}

/* This is used by the shop etc. */
void add_money(int m) {
#ifdef LOG_EXP
    if (this_player() && this_player() != this_object() &&
      query_ip_number(this_player()) && query_ip_number(this_object()) &&
      level < 20 && m >= ROOM_EXP_LIMIT)
	log_file("EXPERIENCE", ctime(time()) + " " +name + "(" + level +
		") " + m + " money by " + this_player()->query_real_name() +
		"(" + this_player()->query_level() + ")\n");
#endif
    money = money + m;
    if (level <= 19 && !is_npc)
	add_worth(m);
}

int query_money() {
    return money;
}

int query_exp() {
    return experience;
}

int query_frog() {
    return frog;
}

int frog_curse(string arg) {
    if (arg) {
	if (frog)
	    return 1;
	tell_object(this_object(), "You turn into a frog !\n");
	frog = 1;
	return 1;
    }
    tell_object(this_object(), "You turn HUMAN again.\n");
    frog = 0;
    return 0;
}

void run_away() {
    object here;
    int i, j;

    here = environment();
    i = 0;
    j = random(6);
    while(i<6 && here == environment()) {
	i += 1;
	j += 1;
	if (j > 6)
	    j = 1;
	if (j == 1) command("east");
	if (j == 2) command("west");
	if (j == 3) command("north");
	if (j == 4) command("south");
	if (j == 5) command("up");
	if (j == 6) command("down");
    }
    if (here == environment()) {
	say(cap_name + " tried, but failed to run away.\n", this_object());
	tell_object(this_object(),
	    "Your legs tried to run away, but failed.\n");
    } else {
	tell_object(this_object(), "Your legs run away with you!\n");
    }
}

int query_hp() {
    return hit_point;
}

int query_wimpy() {
    return whimpy;
}

string query_current_room() {
    return object_name(environment(this_object()));
}

int query_spell_points() {
    return spell_points;
}

void stop_fight() {
    attacker_ob = alt_attacker_ob;
    alt_attacker_ob = 0;
}

int query_wc() {
    return weapon_class;
}

int  query_ac() {
    return armour_class;
}

int reduce_hit_point(int dam) {
    object o;
    if(this_player()!=this_object()) {
	log_file("REDUCE_HP",query_name()+" by ");
	if(!this_player()) log_file("REDUCE_HP","?\n");
	else {
	    log_file("REDUCE_HP",this_player()->query_name());
	    o=previous_object();
	    if (o)
		log_file("REDUCE_HP", " " + object_name(o) + ", " +
			 o->short() + " (" + creator(o) + ")\n");
	    else
		log_file("REDUCE_HP", " ??\n");
	}
    }
    /* this will detect illegal use of reduce_hit_point in weapons */
    hit_point -= dam;
    if (hit_point <= 0)
	hit_point = 1;
    return hit_point;
}

int query_age() {
    return age;
}

/*----------- Most of the gender handling here: ------------*/

int query_gender() { return gender; }
int query_neuter() { return !gender; }
int query_male() { return gender == 1; }
int query_female() { return gender == 2; }

void set_gender(int g) {
    if (g == 0 || g == 1 || g == 2)
        gender = g;
}
int set_neuter() { return gender = 0; }
int set_male() { return gender = 1; }
int set_female() { return gender = 2; }

string query_gender_string() {
    if (!gender)
	return "neuter";
    else if (gender == 1)
	return "male";
    else
	return "female";
}

string query_pronoun() {
    if (!gender)
	return "it";
    else if (gender == 1)
	return "he";
    else
	return "she";
}

string query_possessive() {
    if (!gender)
	return "its";
    else if (gender == 1)
	return "his";
    else
	return "her";
}

string query_objective() {
    if (!gender)
	return "it";
    else if (gender == 1)
	return "him";
    else
	return "her";
}

/*
 * Flags manipulations. You are not supposed to do this arbitrarily.
 * Every wizard can allocate a few bits from the administrator, which
 * he then may use. If you mainpulate bits that you don't know what they
 * are used for, unexpected things can happen.
 */
void set_flag(int n) {
    if (flags == 0)
	flags = "";
#ifdef LOG_FLAGS
    log_file("FLAGS", name + " bit " + n + " set\n");
    if (previous_object()) {
	if (this_player() && this_player() != this_object() &&
	  query_ip_number(this_player()))
	    log_file("FLAGS", "Done by " +
		     this_player()->query_real_name() + " using " +
		     object_name(previous_object()) + ".\n");
    }
#endif
    flags = set_bit(flags, n);
}

int test_flag(int n) {
    if (flags == 0)
	flags = "";
    return test_bit(flags, n);
}

int clear_flag(int n) {
    if (flags == 0)
	flags = "";
#ifdef LOG_FLAGS
    log_file("FLAGS", name + " bit " + n + " cleared\n");
    if (previous_object()) {
	if (this_player() && this_player() != this_object() &&
	  query_ip_number(this_player()))
	    log_file("FLAGS", "Done by " +
		     this_player()->query_real_name() + " using " +
		     object_name(previous_object()) + ".\n");
    }
#endif

    flags = clear_bit(flags, n);
    return 1;
}

string query_stats() {
    return "str:\t" + Str +
	  "\nint:\t" + Int +
	  "\ncon:\t" + Con +
	  "\ndex:\t" + Dex + "\n";
}

int query_str() { return Str; }
int query_int() { return Int; }
int query_con() { return Con; }
int query_dex() { return Dex; }

/* Note that previous object is 0 if called from ourselves. */
void set_str(int i) {
    if (i<1 || i > 20)
	return;
    Str = i;
}

void set_int(int i) {
    if (i<1 || i > 20)
	return;
    Int = i;
    max_sp = 42 + Int * 8;
}

void set_con(int i) {
    if (i<1 || i > 20)
	return;
    Con = i;
    max_hp = 42 + Con * 8;
}

void  set_dex(int i) {
    if (i<1 || i > 20)
	return;
    Dex = i;
}
