#include "living.h"

/*
 * The heart beat is started in living.h when we are attacked.
 */

reset(arg)
{
    object weapon;

    if (arg)
       return;
    max_hp = 200;
    hit_point = 200;
    level = 10;
    experience = 39000;
    weapon_class = WEAPON_CLASS_OF_HANDS;
    is_npc = 1;
    name = "guard";
    cap_name = "guard";
    alignment = 100;
    enable_commands();
    weapon = clone_object("obj/weapon");
    call_other(weapon, "set_name", "sword");
    call_other(weapon, "set_short", "shortsword");
    call_other(weapon, "set_alias", "shortsword");
    call_other(weapon, "set_class", 15);
    call_other(weapon, "set_value",700);
    call_other(weapon, "set_weight", 3);
    move_object(weapon, this_object());
    call_other(weapon, "wield", "sword");
}

short() {
   return "A guard";
}

long() {
    write("A very big and strong guard.");
    if (hit_point > max_hp - 40)
       write("He seems to be in a good shape.\n");
}

id(str) { return str == name; }

heart_beat()
{
    age += 1;
    if (!attack())
	set_heart_beat(0);
}

can_put_and_get(str)
{
    if (!str) {
        write(name + " says: Over my dead body.\n");
	return 0;
    }
    return 1;
}
