/* Nasty monster that starts in the forrest. */
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
    name = "troll";
    cap_name = "Troll";
    alignment = -60;
    enable_commands();
}

short() {
    return "A troll";
}

long() {
    write("A big troll.\n");
    if (hit_point > max_hp - 20)
       write("It seems to be in a good shape.\n");
}

id(str) { return str == name; }

heart_beat()
{
    object ob;
    age += 1;
    if (attacker_ob && random(5) == 0)
	tell_object(attacker_ob, "troll says: your mother was a bitch!\n");
    attack();
    if (attacker_ob || random(5))
       return;
    ob = first_inventory(environment(this_object()));
    while(ob) {
       if (ob != this_object() && living(ob) && !call_other(ob, "query_ghost"))
       {
           attack_object(ob);
           say(name + " attacks " + call_other(ob, "query_name", 0) +
               " viciously.\n");
           return;
       }
       ob = next_inventory(ob);
    }
}

can_put_and_get(str)
{
    if (!str) {
        write(name + " says: Over my dead body.\n");
       return 0;
    }
    return 1;
}

catch_tell(str)
{
    string who, extra;

    if (sscanf(str, "%s arrives%s", who, extra) == 2)
	test_if_any_here();
}
