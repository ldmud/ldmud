#include "std.h"

int lamp_is_lit;
object leo;

void init() {
    add_action("west", "west");
    add_action("open", "open");
    add_action("close", "close");
    add_action("push", "push");
    add_action("north", "north");
    add_action("south", "south");
}

string short() {
    return "wizards hall";
}

void long() {
    write("You are in the hall of the wizards.\n" +
"There is a door to the west and a shimmering field to the north.\n");
    if (lamp_is_lit)
	write("There is a lit lamp beside the elevator.\n");
}

int open(string str)
{
    if (str != "door")
	return 0;
    if ("room/elevator"->query_level() != 1) {
	write("You can't when the elevator isn't here.\n");
	return 1;
    }
    "room/elevator"->open_door("door");
    return 1;
}

int close(string str)
{
    if (str != "door")
	return 0;
    "room/elevator"->close_door("door");
    return 1;
}

int west() {
    if ("room/elevator"->query_door() ||
	"room/elevator"->query_level() != 1) {
	write("The door is closed.\n");
	return 1;
    }
    this_player()->move_player("west#room/elevator");
    return 1;
}

void reset(int arg) {
    if (!arg)
	set_light(1);
    if (!leo) {
	leo = clone_object("obj/leo");
	move_object(leo, this_object());
    }
}

int push(string str)
{
    if (str && str != "button")
	return 0;
    if ("room/elevator"->call_elevator(1))
	lamp_is_lit = 1;
    return 1;
}

void elevator_arrives()
{
    say("The lamp on the button beside the elevator goes out.\n");
    lamp_is_lit = 0;
}

int north() {
    if (this_player()->query_level() < 21) {
	write("A strong magic force stops you.\n");
	return 1;
    }
    write("You wriggle through the force field...\n");
    this_player()->move_player("north#room/quest_room");
    return 1;
}
