#include "std.h"

int lamp_is_lit;
object leo;

init() {
    add_action("west"); add_verb("west");
    add_action("open"); add_verb("open");
    add_action("close"); add_verb("close");
    add_action("push"); add_verb("push");
}

short() {
    return "wizards hall";
}

long() {
    write("You are in the hall of the wizards.\n" +
	  "There is a door to the west.\n");
    if (lamp_is_lit)
	write("There is a lit lamp beside the elevator.\n");
}

open(str)
{
    if (str != "door")
	return 0;
    if (call_other("room/elevator", "query_level", 0) != 1) {
	write("You can't when the elevator isn't here.\n");
	return 1;
    }
    call_other("room/elevator", "open_door", "door");
    return 1;
}

close(str)
{
    if (str != "door")
	return 0;
    call_other("room/elevator", "close_door", "door");
    return 1;
}

west() {
    if (call_other("room/elevator", "query_door", 0) ||
	call_other("room/elevator", "query_level", 0) != 1) {
	write("The door is closed.\n");
	return 1;
    }
    call_other(this_player(), "move_player", "west#room/elevator");
}

reset(arg) {
    if (!arg)
	set_light(1);
    if (!leo || !living(leo)) {
	leo = clone_object("obj/leo");
	move_object(leo, this_object());
    }
}

push(str)
{
    if (str && str != "button")
	return 0;
    if (call_other("room/elevator", "call_elevator", 1))
	lamp_is_lit = 1;
    return 1;
}

elevator_arrives()
{
    say("The lamp on the button beside the elevator goes out.\n");
    lamp_is_lit = 0;
}
