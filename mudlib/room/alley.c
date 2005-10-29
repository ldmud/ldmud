#include "std.h"

object trixie;
int count;

#undef EXTRA_RESET
#define EXTRA_RESET\
    if (!find_living("trixie"))\
        starta_trixie();

ONE_EXIT("room/yard","east",
	 "Dirty alley",
	 "A dirty, trash-strewn alley.\n", 1)

starta_trixie() {
    if(!trixie || !living(trixie)) {
	trixie = clone_object("obj/monster.talk");
	call_other(trixie, "set_name", "trixie");
	call_other(trixie, "set_alias", "hooker");
	call_other(trixie, "set_short", "Trixie the hooker");
	call_other(trixie, "set_long",
		   "She is rather old, and has not aged well.\n");
	call_other(trixie, "set_ac", 0);
	call_other(trixie, "set_level",1);
	call_other(trixie, "set_al",125);
	call_other(trixie, "set_ep",30);
	call_other(trixie, "set_hp",20);
	call_other(trixie, "set_wc",5);
	call_other(trixie, "set_aggressive", 0);
	move_object(trixie, "room/alley");
	call_other(trixie, "set_object", this_object());
	call_other(trixie, "set_function", "smiles");
	call_other(trixie, "set_type", "smiles");
	call_other(trixie, "set_match", " happily.");
	call_other(trixie, "set_type", "arrives");
	call_other(trixie, "set_match", "");
	call_other(trixie, "set_function", "say_hello");
	call_other(trixie, "set_type", "arrives");
	call_other(trixie, "set_match", "");
	call_other(trixie, "set_function", "test_say");
	call_other(trixie, "set_type", "says:");
	call_other(trixie, "set_match", " ");
	call_other(trixie, "set_type", "tells you:");
	call_other(trixie, "set_match", " ");
	call_other(trixie, "set_function", "gives");
	call_other(trixie, "set_type", "gives");
	call_other(trixie, "set_match", " ");
	
	call_other(trixie, "set_chat_chance", 10);
	call_other(trixie, "set_a_chat_chance", 33);
	call_other(trixie, "load_chat", "Trixie says: Hi there, stranger!\n");
	call_other(trixie, "load_chat", "Trixie says: " +
		   "Hello there!\n");
	call_other(trixie, "load_chat", "Trixie says: " +
		   "Are you looking for some fun?\n");
	call_other(trixie, "load_chat", "Trixie says: " +
		   "Can I do anything for you?\n");
	call_other(trixie, "load_chat", "Trixie says: " +
		   "Nice weather, isn't it?\n");
	call_other(trixie, "load_chat", 
		   "Trixie smiles.\n");
	call_other(trixie, "load_a_chat", "Trixie says: " +
		   "Don't hit me!\n");
	call_other(trixie, "load_a_chat", "Trixie says: " +
		   "That hurt!\n");
	call_other(trixie, "load_a_chat", "Trixie says: " +
		   "Help, someone!\n");
	call_other(trixie, "load_a_chat", "Trixie says: " +
		   "I'm not into this sort of thing!\n");
	call_other(trixie, "load_a_chat", "Trixie says: " +
		   "What did I do to you?\n");
	call_other(trixie, "load_a_chat", "Trixie says: " +
		   "You big brute!\n");
    }
}

notify(str) {
    say(str);
    write(str);
}
	
smiles(str) {
    string who, what;
    sscanf(str, "%s %s", who, what);
    if(who == "trixie" || who == "Trixie")
	return;
    if (sscanf(str, "%s smiles happily", who) == 1 &&
	who != "Trixie") {
	notify("Trixie smiles happily.\n");
    }
}
    
say_hello(str) {
    string who;
    if (sscanf(str, "%s arrives.", who) == 1) {
	notify( "Trixie says: Hi " + who + "!  Were you looking for me?\n");
    }
}
    
test_say(str) {
    string a, b, message;

    sscanf(str, "%s %s", a, b);
    if(a == "trixie" || a == "Trixie")
	return;
    if (!sscanf(str, "%s says: %s\n", a, b) == 2) {
	return;
    }
    str = b;

    if (sscanf(str, "%sTrixie%s", a, b) == 2)
	message = "Trixie nods enthusiastically.\n";
    if(message)
	notify(message);
}

gives(str) {
    string who, what, whom;
    int rand;
    object obj, next_obj;
    if(sscanf(str, "%s gives %s to %s.\n", who, what, whom) != 3)
	return;
    if(whom != "Trixie")
	return;
    else if(what == "corpse") {
	notify("Trixie says: Hey, do I look like a mortician or something?\n");
	obj = first_inventory(trixie);
	while(!call_other(obj, "id", what))
	    obj = next_inventory(obj);
	transfer(obj, find_living(lower_case(who)));
	notify("Trixie returned the " + what + " to " + who + ".\n");
    } else if (what == "1 coin") {
        notify("Trixie says: 1 coin? What sort of cheap tramp do you think I am?\n");
	notify("Trixie pouts.\n");
    } else {
	notify("Trixie says: Oh, for me?\n");
	notify("Trixie blushes.\n");
	notify("Trixie gives " + who + " a peck on the lips. (Yuck!)\n");
	/* De-frog the person if necessary */
	if (call_other(find_living(lower_case(who)), "query_frog"))
		call_other(find_living(lower_case(who)), "frog_curse", 0);
    }
}
