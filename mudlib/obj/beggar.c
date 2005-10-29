#include "living.h"

reset(arg) {
    if (arg)
	return;
    set_heart_beat(1);
    name = "beggar";
    cap_name = "Beggar";
    msgin = "enters";
    msgout = "leaves";
    max_hp = 30;
    hit_point = 30;
    level = 3;
    experience = 2283;
    weapon_class = 5;
    armor_class = 0;
    alignment = 200;
    is_npc = 1;
    enable_commands();
}

short() { return name; }

long() {
    write("A really filthy looking poor beggar.\n");
}

id(str) {
    return str == name;
}

catch_tell(str) {
    string who, what;
}

heart_beat()
{
    age += 1;
    attack();
}
