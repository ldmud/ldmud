#include "std.h"
int door_is_open, door_is_locked;
object guard;

#undef EXTRA_RESET
#define EXTRA_RESET extra_reset();

void extra_reset() {
    if (!guard || !living(guard)) {
	object key, weapon;
        guard = clone_object("obj/monster");
	guard->set_name("guard");
	guard->set_level(11);
	guard->set_hp(200);
	guard->set_al(100);
	guard->set_short("A guard");
	guard->set_long("A big and sturdy guard.");
	move_object(guard, this_object());
	weapon = clone_object("obj/weapon");
	weapon->set_name("shortsword");
	weapon->set_short("A shortsword");
	weapon->set_alias("sword");
	weapon->set_long(
"It is a professional looking short sword, used by warriors and guards");
	weapon->set_class(15);
	weapon->set_value(700);
	weapon->set_weight(3);
	transfer(weapon, guard);
	guard->init_command("wield shortsword");
	key = clone_object("obj/treasure");
	key->set_id("key");
	key->set_alias("bank key");
	key->set_short("A bronze key");
	key->set_value(10);
	key->set_weight(1);
	transfer(key, guard);
    }
    door_is_open = 0; door_is_locked = 1;
}
	
#undef EXTRA_LONG
#define EXTRA_LONG\
    if (str == "counter") {\
	write("There is a sign  in the counter that says\n" +\
	    "CLOSED FOR RECONSTRUCTION\n");\
	return;\
    }\
    if (str == "door") {\
	if (door_is_open) {\
	    write("The door is open.\n");\
	    return;\
	}\
	write("The door is closed.\n");\
	return;\
    }

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("open", "open");\
    add_action("unlock", "unlock");\
    add_action("east", "east");

ONE_EXIT("room/narr_alley","west",
	 "The bank",
	 "You are in the bank.\n" +
	 "To the east is a low counter. The counter is covered\n" +
	 "with heavy iron bars. On the wall beside the counter, a door\n" +
	 "leads further east\n", 1)

int id(string str) {
    return str == "door" || str == "counter";
}

int open(string str) {
    if (str && str != "door")
	return 0;
    if (door_is_open)
	return 0;
    if (door_is_locked) {
	write("The door is locked.\n");
	return 1;
    }
    door_is_open = 1;
    write("Ok.\n");
    say(this_player()->query_name() + " opened the door.\n");
    return 1;
}

int unlock(string str) {
    if (str && str != "door")
	return 0;
    if (door_is_open || !door_is_locked)
	return 0;
    if (!present("bank key", this_player())) {
	if (present("key", this_player()))
	    write("You don't have the right key.\n");
	else
	    write("You need a key.\n");
	return 1;
    }
    door_is_locked = 0;
    write("ok.\n");
    say(this_player()->query_name() + " unlocked the door.\n");
    return 1;
}

int east() {
    if (!door_is_open) {
	write("The door is closed.\n");
	return 1;
    }
    if (guard && present(guard, this_object())) {
	write("The guard bars the way.\n");
	return 1;
   }
    this_player()->move_player("east#room/bankroom");
    return 1;
}

int query_door() {
    return !door_is_open;
}

void open_door_inside() {
    door_is_locked = 0;
    door_is_open = 1;
}

int query_drop_castle() {
    return 1;
}
