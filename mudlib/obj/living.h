#include "../../security.h"
#define ALIGN_EXP_DIVISOR		100
#define EXP_DIVISOR			50
#define INTERVAL_BETWEEN_HEALING	10
#define WEAPON_CLASS_OF_HANDS		(3)
#define ARMOUR_CLASS_OF_BARE		0
#define KILL_NEUTRAL_ALIGNMENT		10
#define ADJ_ALIGNMENT(al)		((-al - KILL_NEUTRAL_ALIGNMENT)/4)
#define MAX_LIST			20
#define NAME_OF_GHOST			"some mist"


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
int armor_class;	/* What armor class of monster. */
int hit_point;		/* Number of hit points of monster. */
int max_hp;
int experience;		/* Experience points of monster. */
string mmsgout;		/* Message when leaving magically. */
string mmsgin;		/* Message when arriving magically. */
object attacker_ob;	/* Name of player attacking us. */
object alt_attacker_ob;	/* Name of other player also attacking us. */
int weapon_class;	/* How good weapon. Used to calculate damage. */
object name_of_weapon;	/* To see if we are wielding a weapon. */
object head_armor;	/* What armor we have. */
int ghost;		/* Used for monsters that can leave a ghost. */
int local_weight;	/* weight of items */
object hunted, hunter;	/* used in the hunt mode */
int hunting_time;	/* How long we will stay in hunting mode. */
string cap_name;	/* Capital version of "name". */
int spell_points;	/* Current spell points. Same max as max_hp. */
string spell_name;
int spell_cost, spell_dam;
int age;		/* Number of heart beats of this character. */
int is_invis;		/* True when player is invisible */
int frog;		/* If the player is a frog */
int whimpy;		/* Automatically run when low on HP */
string auto_load;	/* Special automatically loaded objects. */
string current_room;	/* Name of current room. */

/*
 * All characters have an aligment, depending on how good or chaotic
 * they are.
 * This value is updated when killing other players.
 */
int alignment;

/*
 * The following routines are defined for usage:
 * hit_player		Called when fighting.
 * transfer_all_to:	Transfer all objects to dest.
 * move_player:		Called by the object that moves the monster.
 * query_name:		Gives the name to external objects.
 * query_real_name:	Give the uncapitalized name, even if invisible.
 * attacked_by		Tells us who are attacking us.
 * show_stats		Dump local status.
 * stop_wielding	Called when we drop a weapon.
 * stop_wearing		Called when we drop an armor.
 * query_hp		Query the number of hit points we have
 * query_level		Give our level to external objects.
 * query_value		Always return 0. Can't sell this object.
 * query_npc		Return 1 if npc otherwise 0.
 * get			Always return 0. Can't take this object.
 * attack		Should be called from heart_beat. Will maintain attack.
 * query_attack
 * drop_all_money	Used when the object dies.
 * wield		Called by weapons.
 * wear			Called by armor.
 * add_weight		Used when picking up things.
 * heal_self		Enable wizards to heal this object.
 * can_put_and_get	Can look at inventory, but not take things from it.
 * attack_object	Called when starting to attack an object.
 * test_if_any_here	For monsters. Call this one if you suspect no enemy
 *			is here any more. This will be checked, and the
 *			heart beat turned off in that case.
 *			Return 1 if anyone there, 0 if none.
 * force_us		Force us to do a command.
 */

/*
 * This routine is called from objects that moves the player.
 * Special: direction "X" means teleport.
 */
move_player(dir_dest)
{
    string dir, dest;
    object ob;
    int is_light;

    if (sscanf(dir_dest, "%s#%s", dir, dest) != 2) {
	tell_object(this_object(), "Move to bad dir/dest\n");
	return;
    }
    current_room = dest;
    hunting_time -= 1;
    if (hunting_time == 0) {
	if (hunter)
	    call_other(hunter, "stop_hunter");
	hunter = 0;
	hunted = 0;
    }
    if (attacker_ob && present(attacker_ob)) {
	hunting_time = 10;
	if (!hunter)
	    tell_object(this_object(), "You are now hunted by " +
			call_other(attacker_ob, "query_name", 0) + ".\n");
        hunter = attacker_ob;
    }
    if (!msgout)
	msgout = "leaves";
    is_light = set_light(0);
    if (is_light < 0)
	is_light = 0;
    if (ghost)
	say(NAME_OF_GHOST + " " + msgout + " " + dir + ".\n");
    else if (dir == "X" && query_invis() < INVIS_TELEPORT)
	say(cap_name + " " + mmsgout + ".\n");
    else if (query_invis() < NO_SHORT && is_light)
	say(cap_name + " " + msgout + " " + dir + ".\n");
    move_object(this_object(), dest);
    is_light = set_light(0);
    if (is_light < 0)
	is_light = 0;
    if (level >= 20)
        tell_object(this_object(), dest + "\n");
    if (!msgin)
	msgin = "arrives";
    if (ghost && is_light)
	say(NAME_OF_GHOST + " " + msgin + ".\n");
    else if (query_invis() < INVIS_TELEPORT && dir == "X")
	say(cap_name + " " + mmsgin + ".\n");
    else if (query_invis() < NO_SHORT && is_light)
	say(cap_name + " " + msgin + ".\n");
    if (hunted && present(hunted))
        attack_object(hunted);
    if (hunter && present(hunter))
        call_other(hunter, "attack_object", this_object());
    if (is_npc)
	return;
    if (!is_light) {
	write("A dark room.\n");
	return;
    }
    ob = environment(this_object());
    if (brief)
	write(call_other(ob, "short", 0) + ".\n");
    else
	call_other(ob, "long", 0);
    ob = first_inventory(ob);
    while(ob) {
	if (ob != this_object()) {
	    string short_str;
	    short_str = call_other(ob, "short");
	    if (short_str)
		write(short_str + ".\n");
	}
	ob = next_inventory(ob);
    }
}

/*
 * This function is called from other players when they want to make
 * damage to us. We return how much damage we received, which will
 * change the attackers score. This routine is probably called from
 * heart_beat() from another player.
 */
hit_player(dam) {
    if (level >= 20)	/* You can't hit a wizard ! */
	return 0;
    dam -= random(armor_class + 1);
    if (dam <= 0)
	return 0;
    if (dam > hit_point+1)
	dam = hit_point+1;
    hit_point = hit_point - dam;
    if (hit_point<0) {
	object corpse;
	/* We died ! */
	if (hunter)
	    call_other(hunter, "stop_hunter");
	hunter = 0;
	hunted = 0;
	say(cap_name + " died.\n");
	/* If this is a player, log the death for future reference */
	if (!is_npc)
	    if (attacker_ob)
		log_file("DEATHS", name + " killed by " + call_other(attacker_ob, "query_real_name", 0) + ".\n");
	    else
		log_file("DEATHS", name + " - killer unknown.\n");
	experience = (experience * 3) / 4;	/* Take away 1/4 of exp. */
	if (max_hp)
	    hit_point = max_hp;
	else
	    hit_point = 20;
	corpse = clone_object("obj/corpse");
	call_other(corpse, "set_name", name);
	transfer_all_to(corpse);
	if (!attacker_ob)
	    attacker_ob = alt_attacker_ob;
	if (!is_npc) {
	    save_object("players/" + name);
	}
	/* The player killing us will update his alignment */
	/* and experience if he exists. */
	/* Note that you get no experience for killing other players! */
	else if(attacker_ob) {
	    int exp_diff, exp_bonus, base_bonus;
	    call_other(attacker_ob, "add_alignment",
		   ADJ_ALIGNMENT(alignment));
	    /* Give some more experience for staying in your alignment
	     * All the following basically works out to
	     * exp_bonus = abs(monster's align - player's align)/75000 * exp;
	     * Of course we can't just say that because of underflow problems.
	     */
	    exp_diff = (alignment - call_other(attacker_ob,"query_alignment",0));
	    if (exp_diff < 0)
		exp_diff = -exp_diff;	/* Get absolute value */
	    exp_diff /= 10;	/* Scale this down so we don't overflow! */
	    exp_bonus = experience / 100;
	    exp_bonus *= exp_diff;
	    exp_bonus /= ALIGN_EXP_DIVISOR;
	    /* Base bonus regardless of alignment is experience/50 */
	    base_bonus = experience/EXP_DIVISOR;
	    /* Don't let the alignment bonus exceed 1/2 the base bonus */
	    if (exp_bonus > base_bonus/2)
		exp_bonus = base_bonus/2;
	    exp_bonus += base_bonus;
	    call_other(attacker_ob, "add_exp", exp_bonus);
	}
	move_object(corpse, environment(this_object()));
	if (!call_other(this_object(), "second_life", 0))
	    destruct(this_object());
    }
    return dam;
}

transfer_all_to(dest)
{
    object ob;
    object next_ob;

    ob = first_inventory(this_object());
    while(ob) {
	next_ob = next_inventory(ob);
	if (!call_other(ob, "drop", 1))
	    move_object(ob, dest);
	ob = next_ob;
    }
    local_weight = 0;
    if (money == 0)
	return;
    ob = clone_object("obj/money");
    call_other(ob, "set_money", money);
    move_object(ob, dest);
    money = 0;
}

query_name() {
    if (ghost)
	return NAME_OF_GHOST;
    return cap_name;
}

query_real_name() {
    return name;
}

query_alignment() {
    return alignment;
}

query_npc() {
    return is_npc;
}

/*
 * This routine is called when we are attacked by a player.
 */
attacked_by(ob) {
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

show_stats() {
    int i;
    if (!this_player() || call_other(this_player(),"query_level",0) < CREATE)
	return;
    write(short() + "\nlevel:\t" + level + "\n");
    if (query_invis()) write("invis:\t"+is_invis+"\n");
    write("coins:\t" + money +
	  "\nspell:\t" + spell_points +
	  "\nmax:\t" + max_hp);
    /* Do this to prevent wizards from tipping off players. */
    if (call_other(this_player(),"query_level",0) > STAT)
	  write("\nhp:\t" + hit_point);
    write("\nep:\t"); write(experience);
    write("\nac:\t"); write(armor_class);
    if (head_armor)
	write("\narmor: " + call_other(head_armor, "rec_short", 0));
    write("\nwc:\t"); write(weapon_class);
    if (name_of_weapon)
	write("\nweapon: " + call_other(name_of_weapon, "query_name", 0));
    write("\ncarry:\t" + local_weight);
    if (attacker_ob && living(attacker_ob))
	write("\nattack: " + call_other(attacker_ob, "query_name"));
    write("\nalign:\t" + alignment + "\n");
    show_age();
}

stop_wielding() {
    if (!name_of_weapon) {
	/* This should not happen ! */
	log_file("wield_bug", "Weapon not wielded !\n");
	write("Bug ! The weapon was marked as wielded ! (fixed)\n");
	return;
    }
    call_other(name_of_weapon, "un_wield", 0);
    name_of_weapon = 0;
    weapon_class = WEAPON_CLASS_OF_HANDS;
}

stop_wearing(name) {
    if (!head_armor) {
	/* This shouldn't happen! */
	log_file("wearing_bug", "Armor not worn!\n");
	write("This is a bug, no head_armor.  Please tell Drax.\n");
	return 1;
    }
    head_armor = call_other(head_armor, "remove_link", name);
    if(head_armor)
	armor_class = call_other(head_armor, "tot_ac");
    else
	armor_class = 0;
    checked_say(cap_name + " removes " + name + ".\n");
    write("Ok.\n");
}

query_level() {
    return level;
}

/* This object is not worth anything in the shop ! */
query_value() { return 0; }

/* It is never possible to pick up a player ! */
get() { return 0; }

/*
 * Return true if there still is a fight.
 */
attack()
{
    int tmp;
    int whit;
    string name_of_attacker;

    if (!attacker_ob) {
	spell_cost = 0;
	return 0;
    }
    name_of_attacker = call_other(attacker_ob, "query_name", 0);
    if (!name_of_attacker || name_of_attacker == NAME_OF_GHOST ||
	environment(attacker_ob) != environment(this_object())) {
	if (!hunter && name_of_attacker &&
	    !call_other(attacker_ob, "query_ghost", 0))
	{
	    tell_object(this_object(), "You are now hunting " +
			call_other(attacker_ob, "query_name", 0) + ".\n");
	    hunted = attacker_ob;
	    hunting_time = 10;
	}
	attacker_ob = 0;
	if (!alt_attacker_ob)
	    return 0;
	attacker_ob = alt_attacker_ob;
	alt_attacker_ob = 0;
	if (attack()) {
	    tell_object(this_object(),
			"You turn to attack " +
			call_other(attacker_ob, "query_name", 0) + ".\n");
	    return 1;
	}
	return 0;
    }
    call_other(attacker_ob, "attacked_by", this_object());
    if (spell_cost) {
	if (level < 20) spell_points -= spell_cost;
	tell_object(attacker_ob, "You are hit by a " + spell_name + ".\n");
	write("You cast a " + spell_name + ".\n");
    }
    if(name_of_weapon)
	whit = call_other(name_of_weapon,"hit",attacker_ob);
    if(whit != "miss")
	tmp = call_other(attacker_ob, "hit_player", 
		    random(weapon_class + whit) + spell_dam);
    else
	tmp = 0;
    if (tmp == 0) {
	tell_object(this_object(), "You missed.\n");
	checked_say(cap_name + " missed " + name_of_attacker + ".\n");
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
      call_other(attacker_ob, "query_name", 0) != NAME_OF_GHOST) {
	string how, what;
	how = " to small fragments";
	what = "massacre";
	if (tmp < 30) {
	    how = " with a bone crushing sound";
	    what = "smashed";
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
	if (query_invis() >= INVIS_ACTION)
	    tell_object(attacker_ob, cap_name + " " + what + " you" + how +
		    ".\n");
	checked_say(cap_name + " " + what + " " + name_of_attacker + how +
		    ".\n");
	return 1;
    }
    tell_object(this_object(), "You killed " + name_of_attacker + ".\n");
    attacker_ob = alt_attacker_ob;
    alt_attacker_ob = 0;
    if (attacker_ob)
	return 1;
}

query_attack() {
    /* Changed by Herder */
    return attacker_ob;

    /* OLD
    if (attacker_ob)
	return 1;
    return 0;
    */
}

drop_all_money(verbose) {
    object mon;
    if (money == 0)
	return;
    mon = clone_object("obj/money");
    call_other(mon, "set_money", money);
    move_object(mon, environment());
    if (verbose) {
	checked_say(cap_name + " drops " + money + " gold coins.\n");
	tell_object(this_object(), "You drop " + money + " gold coins.\n");
    }
    money = 0;
}

/* Wield a weapon. */
wield(w) {
    if (name_of_weapon)
	stop_wielding();
    name_of_weapon = w;
    weapon_class = call_other(w, "weapon_class", 0);
    checked_say(cap_name + " wields " + call_other(w, "query_name", 0) + ".\n");
    write("Ok.\n");
}

/* Wear some armor. */
wear(a) {
    object old;

    if(head_armor) {
	old = call_other(head_armor, "test_type", call_other(a, "query_type"));
	if(old)
	    return old;
	old = head_armor;
	call_other(a, "link", old);
    }
    head_armor = a;
    /* Calculate new ac */
    armor_class = call_other(head_armor, "tot_ac");
    checked_say(cap_name + " wears " + call_other(a, "query_name", 0) + ".\n");
    write("Ok.\n");
    return 0;
}

add_weight(w) {
    if (w + local_weight > level + 10 && level < 20)
	return 0;
    local_weight += w;
    return 1;
}

heal_self(h) {
    if (!h)
	return;
    hit_point += h;
    if (hit_point > max_hp)
	hit_point = max_hp;
    spell_points += h;
    if (spell_points > max_hp)
	spell_points = max_hp;
}

can_put_and_get(str)
{
    return str != 0;
}

attack_object(ob)
{
   if (call_other(ob, "query_ghost", 0))
       return;
   set_heart_beat(1);	/* For monsters, start the heart beat */
   /* Zero these out just in case */
   spell_cost = 0;
   spell_dam = 0;
   if (attacker_ob == ob) {
       attack();
       return 1;
   }
   if (alt_attacker_ob == ob) {
       alt_attacker_ob = attacker_ob;
       attacker_ob = ob;
       attack();
       return 1;
   }
   if (!alt_attacker_ob)
       alt_attacker_ob = attacker_ob;
   /* If this is a player, make sure they are entitled to attack this person */
   if (!valid_attack(ob)) {
       return 0;
   }
   attacker_ob = ob;
   attack();
   return 1;
}

query_ghost() { return ghost; }

zap_object(ob)
{
    call_other(ob, "attacked_by", this_object());
    checked_say(cap_name + " summons a flash from the sky.\n");
    write("You summon a flash from the sky.\n");
    if (!call_other(ob, "query_npc", 0))
	shout("There is a big clap of thunder.\n\n");
    call_other(ob, "hit_player", 100000);
}

missile_object(ob)
{
    /* If this is a player, make sure they are entitled to attack this person */
    if (!valid_attack(ob)) {
	write("You can't attack " + call_other(ob, "query_name", 0) + "!\n");
        return 0;
    }
    if (spell_points < 10) {
	write("Too low on power.\n");
	return;
    }
    spell_name = "magic missile";
    spell_dam = random(20);
    spell_cost = 10;
    attacker_ob = ob;
}

shock_object(ob)
{
    /* If this is a player, make sure they are entitled to attack this person */
    if (!valid_attack(ob)) {
	write("You can't attack " + call_other(ob, "query_name", 0) + "!\n");
        return 0;
    }
    if (spell_points < 15) {
	write("Too low on power.\n");
	return;
    }
    spell_name = "shock";
    spell_dam = random(30);
    spell_cost = 15;
    attacker_ob = ob;
}

fire_ball_object(ob)
{
    /* If this is a player, make sure they are entitled to attack this person */
    if (!valid_attack(ob)) {
	write("You can't attack " + call_other(ob, "query_name", 0) + "!\n");
        return 0;
    }
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

test_if_any_here()
{
    object ob;
    ob = first_inventory(environment());
    while(ob) {
	if (ob != this_object() && living(ob) && !call_other(ob, "query_npc"))
	    return 1;
	ob = next_inventory(ob);
    }
    return 0;
}

show_age() {
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

stop_hunter()
{
    hunter = 0;
    tell_object(this_object(), "You are no longer hunted.\n");
}

force_us(cmd) {
    if (level >= call_other(this_player(), "query_level")) {
	tell_object(this_object(), call_other(this_player(), "query_name") +
		    " tried to force you to " + cmd + ".\n");
	return;
    }
    tell_object(this_object(), call_other(this_player(), "query_name") +
		" forced you to '" + cmd + "'.\n");
    command(cmd);
}

/* This is used by the shop etc. */
add_money(m) {
    money = money + m;
    if (level <= 19)
	add_worth(m);
}

query_money() {
    return money;
}

query_exp() {
    return experience;
}

query_frog() {
    return frog;
}

frog_curse(arg) {
    if (arg) {
	if (frog)
	    return 1;
	tell_object(this_player(), "You turn into a frog !\n");
	frog = 1;
	return 1;
    }
    tell_object(this_object(), "You turn HUMAN again.\n");
    frog = 0;
    return 0;
}

run_away() {
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

query_hp() {
    return hit_point;
}

query_wimpy() {
    return whimpy;
}

query_current_room() {
    return current_room;
}

checked_say(str) {
    if (query_invis() >= INVIS_ACTION) return;
    say(str);
}

query_invis(num) {
    if (!num) {
        return is_invis;
    } else {
        if (level <= num || num >= ALL_POWER) {
	    return -is_invis;
	} else { return is_invis;}
    }
}
