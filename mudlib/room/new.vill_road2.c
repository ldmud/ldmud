#include "std.h"

object harry;
int count;

#undef EXTRA_RESET
#define EXTRA_RESET\
    if (!find_living("harry"))\
        starta_harry();

FOUR_EXIT("room/vill_road1","west",
	 "room/vill_shore","east",
	 "room/adv_guild","south",
	 "room/shop","north",
	 "Village road",
	 "A long road going through the village.\n", 1)

starta_harry() {
    if(!harry || !living(harry)) {
	harry = clone_object("obj/monster.talk");
	call_other(harry, "set_name", "harry");
	call_other(harry, "set_alias", "fjant");
	call_other(harry, "set_short", "Harry the affectionate");
	call_other(harry, "set_long", "Harry has an agreeable look.\n");
	call_other(harry, "set_ac", 0);
	call_other(harry, "set_level",3);
	call_other(harry, "set_al",50);
	call_other(harry, "set_ep",2283);
	call_other(harry, "set_hp",30);
	call_other(harry, "set_wc",5);
	call_other(harry, "set_aggressive", 0);
	move_object(harry, "room/vill_road2");
	call_other(harry, "set_object", this_object());
	call_other(harry, "set_function", "why_did");
	call_other(harry, "set_type", "sells");
	call_other(harry, "set_match", " ");
	call_other(harry, "set_type", "attack");
	call_other(harry, "set_match", " ");
	call_other(harry, "set_type", "left");
	call_other(harry, "set_match", "the game");
	call_other(harry, "set_type", "takes");
	call_other(harry, "set_match", " ");
	call_other(harry, "set_type", "drops");
	call_other(harry, "set_match", " ");
	call_other(harry, "set_function", "how_does_it_feel");
	call_other(harry, "set_type", "is now level");
	call_other(harry, "set_match", " ");
	call_other(harry, "set_function", "smiles");
	call_other(harry, "set_type", "smiles");
	call_other(harry, "set_match", " happily.");
	call_other(harry, "set_function", "say_hello");
	call_other(harry, "set_type", "arrives");
	call_other(harry, "set_match", "");
	call_other(harry, "set_function", "test_say");
	call_other(harry, "set_type", "says:");
	call_other(harry, "set_match", " ");
	call_other(harry, "set_type", "tells you:");
	call_other(harry, "set_match", " ");
	call_other(harry, "set_function", "follow");
	call_other(harry, "set_type", "leaves");
	call_other(harry, "set_match", " "); 
	call_other(harry, "set_function", "gives");
	call_other(harry, "set_type", "gives");
	call_other(harry, "set_match", " ");
	
	call_other(harry, "set_chat_chance", 3);
	call_other(harry, "set_a_chat_chance", 33);
	call_other(harry, "load_chat", "Harry says: What are you waiting for?\n");
	call_other(harry, "load_chat", "Harry says: " +
		   "Hello there!\n");
	call_other(harry, "load_chat", "Harry says: " +
		   "My allergies are acting up.\n");
	call_other(harry, "load_chat", "Harry says: " +
		   "I don't want to go to college anymore.\n");
	call_other(harry, "load_chat", "Harry says: " +
		   "Who are you?\n");
	call_other(harry, "load_chat", "Harry says: " +
		   "Why do you look like that?\n");
	call_other(harry, "load_chat", "Harry says: " +
		   "What are you doing here?\n");
	call_other(harry, "load_chat", "Harry says: " +
		   "Nice weather, isn't it?\n");
	call_other(harry, "load_chat", 
		   "Harry smiles.\n");
	call_other(harry, "load_a_chat", "Harry says: " +
		   "Don't hit me!\n");
	call_other(harry, "load_a_chat", "Harry says: " +
		   "That hurt!\n");
	call_other(harry, "load_a_chat", "Harry says: " +
		   "Help, someone!\n");
	call_other(harry, "load_a_chat", "Harry says: " +
		   "Why can't you go bullying elsewhere?\n");
	call_other(harry, "load_a_chat", "Harry says: " +
		   "Aooooo\n");
	call_other(harry, "load_a_chat", "Harry says: " +
		   "I hate bashers!\n");
	call_other(harry, "load_a_chat", "Harry says: " +
		   "Bastard\n");
	call_other(harry, "load_a_chat", "Harry says: " +
		   "You big brute!\n");
	call_other(harry, "set_random_pick", 20);
	call_other(harry, "set_move_at_reset", 0);
    }
}

why_did(str) {
    string who, what;
    sscanf(str, "%s %s", who, what);
    if(who == "harry" || who == "Harry")
	return;
    if (sscanf(str, "%s sells %s.", who, what) == 2) {
	notify("Harry says: Why did you sell " + what + "\n");
    }
    if (sscanf(str, "%s attacks %s.", who, what) == 2) {
	notify("Harry says: Why does " + who + " attack " + what + "?\n");
    }
    if (sscanf(str, "%s left the game.", who) == 1) {
	notify("Harry says: Why did " + who + " quit the game ?\n");
    }
    if (sscanf(str, "%s takes %s.\n", who, what) == 2) {
	notify("Harry says: Why did " + who + " take " + what + " ?\n");
    }
    if (sscanf(str, "%s drops %s.\n", who, what) == 2) {
	notify("Harry says: Why did " + who + " drop " + what + " ?\n");
    }
}

notify(str) {
    say(str);
    write(str);
}
	
how_does_it_feel(str) {
    string who, what;
    sscanf(str, "%s %s", who, what);
    if(who == "harry" || who == "Harry")
	return;
    if (sscanf(str, "%s is now level %s.\n", who, what) == 2) {
	notify("Harry says: How does it feel, being of level " + what);
    }
}
    
smiles(str) {
    string who, what;
    sscanf(str, "%s %s", who, what);
    if(who == "harry" || who == "Harry")
	return;
    if (sscanf(str, "%s smiles happily", who) == 1 &&
	who != "Harry") {
	notify("Harry smiles happily.\n");
    }
}
    
say_hello(str) {
    string who;
    if (sscanf(str, "%s arrives.", who) == 1) {
	notify( "Harry says: Hi " + who + ", nice to see you !\n");
    }
}
    
test_say(str) {
    string a, b, message;

    sscanf(str, "%s %s", a, b);
    if(a == "harry" || a == "Harry")
	return;
    if (!sscanf(str, "%s says: %s\n", a, b) == 2) {
	return;
    }
    str = b;

    if (str == "hello" || str == "hi" || str == "hello everybody") {
	message = "Harry says: Pleased to meet you!\n";
    }
    if (str == "shut up") {
	message = "Harry says: Why do you want me to shut up ?\n";
    }
    if (sscanf(str, "%sstay here%s", a, b) == 2 ||
	sscanf(str, "%snot follow%s", a, b) == 2 ||
	sscanf(str, "%sget lost%s", a, b) == 2) {
	message = "Harry says: Ok then.\n";
    }
    if(!message)
	message = "Harry says: Why do you say '" + str + "'???\n";
    notify(message);
}

follow(str) {
    string who, where;
    if(sscanf(str, "%s leaves %s.\n", who, where) == 2)
	call_other(harry, "init_command", where);
}

gives(str) {
    string who, what, whom;
    int rand;
    object obj, next_obj;
    if(sscanf(str, "%s gives %s to %s.\n", who, what, whom) != 3)
	return;
    if(whom != "Harry")
	return;
    if(what == "firebreather" || what == "special" ||
       what == "beer" || what == "bottle") {
	rand = random(4);
	if(rand == 0) {
	    if(random(10) > 6) {
		notify("Harry sighs and says: I guess you're gonna kill me now.\n");
		obj = first_inventory(harry);
		while(obj) {
		    next_obj = next_inventory(harry);
		    transfer(obj, environment(harry));
		    notify("Harry drops " + call_other(obj, "short") + ".\n");
		    obj = next_obj;
		}
		call_other(harry, "init_command", "west");
	    }
	}
	if(rand == 1) {
	    call_other(harry, "init_command", "drink " + what);
	}
	if(rand == 2) {
	    obj = first_inventory(harry);
	    while(!call_other(obj, "id", what))
		obj = next_inventory(obj);
	    transfer(obj, environment(harry));
	    notify("Harry drops the " + what + ".\n");
	}
	if(rand == 3) {
	    obj = first_inventory(harry);
	    while(!call_other(obj, "id", what))
		obj = next_inventory(obj);
	    transfer(obj, find_living(lower_case(who)));
	    notify("Harry returned the " + what + " to " + who + ".\n");
	}
    } else if(what == "corpse") {
	notify("Harry says: HEY, burry your corpses yourself, ashole.\n");
	obj = first_inventory(harry);
	while(!call_other(obj, "id", what))
	    obj = next_inventory(obj);
	transfer(obj, find_living(lower_case(who)));
	notify("Harry returned the " + what + " to " + who + ".\n");
    } else {
	notify("Harry says: Thank you very much, sir.\n");
    }
}

monster_died() {
    object obj, b;
    int num;
    obj = first_inventory(harry);
    while(obj) {
	b = next_inventory(harry);
	if(call_other(obj, "id", "bottle")) {
	    destruct(obj);
	    num = 1;
	}
	obj = b;
    }
    if(num)
	notify("There is a crushing sound of bottles breaking, as the body falls.\n");
}

