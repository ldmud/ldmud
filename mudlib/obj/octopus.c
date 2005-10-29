/* Nasty monster that carries the chest. */
#include "living.h"

reset(arg)
{
    if (arg)
	return;
    set_heart_beat(1);
    weapon_class = 12;
    max_hp = 100;
    hit_point = 100;
    level = 8;
    experience = 17000;
    is_npc = 1;
    name = "octopus";
    cap_name = "Octopus";
    alignment = -60;
    enable_commands();
}

short() {
    return "An octopus";
}

long() {
    write("A very big octopus with eight long arms.\n");
    write("It looks very dangerous.\n");
    if (hit_point > max_hp - 20)
	write("It seems to be in a good shape.\n");
}

id(str) { return str == name; }

heart_beat()
{
    object ob;
    age += 1;
    attack();
    if (attacker_ob) {
	int n;
	n = random(30);
	if (n == 0)
	    tell_object(attacker_ob,
			name + " says: I will convert you to a pulp.\n");
	if (n == 1)
	    tell_object(attacker_ob,
			name + " says: Come on, coward!\n");
	if (n == 2)
	    tell_object(attacker_ob,
			name + " says: You fight like a woman!\n");
    }
    if (attacker_ob || random(5))
	return;
    ob = first_inventory(environment(this_object()));
    while(ob) {
	if (ob != this_object() && living(ob)) {
	    attack_object(ob);
	    say(name + " attacks " + call_other(attacker_ob, "query_name", 0) +
		" viciously.\n");
	    return;
	}
	ob = next_inventory(ob);
    }
}

can_put_and_get()
{
    write(name + " says: Over my dead body.\n");
    return 0;
}

catch_tell(str)
{
    string who, extra;

    if (sscanf(str, "%s arrives%s", who, extra) == 2)
	test_if_any_here();
}
