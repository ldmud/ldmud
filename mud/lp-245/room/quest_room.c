#include "room.h"
#include "tune.h"

ONE_EXIT("room/wiz_hall", "south",
	 "Room of quests",
"This is the room of quests. Every wizard can make at most one quest.\n" +
"When he has made a quest, he should have it approved by an arch wizard.\n" +
"When it is approved, put a permanent object in this room, wich has as\n"+
"short description the name of the wizard. All objects in this room will be\n"+
"checked when a player wants to become a wizard. The player must have\n"+
"solved ALL quests. To mark that a quest is solved, call 'set_quest(\"name\")'\n"+
"in the player object. The objects must all accept the id 'quest' and the\n"+
"name of the wizard. The objects must also define a function hint(),\n"+
"that should return a message giving a hint of where to start the quest.\n"+
"Note that players never can come here. set_quest(str) will return 1 if\n"+
"this is the first time it was solved by this player, otherwise 0.\n", 1)

int count(int silently) {
    object ob;
    int i;

    ob = first_inventory(this_object());
    while(ob) {
	if (ob->id("quest")) {
	    string str;
	    str = ob->short();
	    if (!this_player()->query_quests(str))
		i += 1;
	}
	ob = next_inventory(ob);
    }
    if (!silently) {
	if (i == 0)
	    write("You have solved all quests!\n");
	else {
	    write("You have " + i + " quests unsolved.\n");
	    if (i - FREE_QUESTS <= 0)
		write("You don't have to solve any more quests.\n");
	    else
		write("You must solve " + (i - FREE_QUESTS) + " of these.\n");
	}
    }
    if (i - FREE_QUESTS < 0)
	return 0;
    return i - FREE_QUESTS;
}

void list(int i) {
    object ob;

    ob = first_inventory(this_object());
    while(ob) {
	if (ob->id("quest")) {
	    string str;
	    str = ob->short();
	    if (!this_player()->query_quests(str))
		i -= 1;
	    if (i == 0) {
		write(ob->hint() + "\n");
		return;
	    }
	}
	ob = next_inventory(ob);
    }
}
