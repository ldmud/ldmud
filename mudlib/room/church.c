#include "std.h"

int lamp_is_lit;

reset(arg)
{
    if (arg)
	return;
    set_light(1);
}

init()
{
    if (call_other(this_player(), "query_level", 0) >= 20) {
	add_action("xyzzy"); add_verb("xyzzy");
    }
    add_action("east"); add_verb("east");
    add_action("west"); add_verb("west");
    add_action("open"); add_verb("open");
    add_action("push"); add_verb("push");
    add_action("push"); add_verb("press");
    add_action("close"); add_verb("close");
    add_action("regenerate"); add_verb("regenerate");
    add_action("south"); add_verb("south");
}

short() {
    return "Endoplasmatorium";
}

long(str)
{
    if (str == "elevator" || str == "door") {
	if (!call_other("room/elevator", "query_door", 0) &&
	    call_other("room/elevator", "query_level", 0))
	    write("The door is open.\n");
	else
	    write("The door is closed.\n");
	return;
    }
    write("This is the Endoplasmatorium.  As the name implies, it is a place where\n");
    write("people without bodies come to get them.  Ghosts come here to regenerate.\n");
    write("   To the east is a clinic for adventurers.\n");
    write("   There is an elevator on the west wall.\n");
    write("   There is a door marked 'exit' to south.\n");
    if (lamp_is_lit)
        write("The lamp beside the elevator is lit.\n");

}

id(str) {
    return (str == "door" || str == "elevator");
}

xyzzy() {
    write("Everything shimmers.\n");
    write("You wake up elsewhere...\n");
    call_other(this_player(), "move_player", "elsewhere#room/test");
}

west() {
    if (call_other("room/elevator", "query_door", 0) ||
	call_other("room/elevator", "query_level", 0) != 2) {
	write("The door is closed.\n");
	return 1;
    }
    call_other(this_player(), "move_player", "west#room/elevator");
    return 1;
}

east() {
    call_other(this_player(), "move_player", "east#room/clinic");
    return 1;
}

open(str)
{
    if (str != "door")
	return 0;
    if (call_other("room/elevator", "query_level", 0) != 2) {
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

push(str)
{
    if (str && str != "button")
	return 0;
    if (call_other("room/elevator", "call_elevator", 2))
	lamp_is_lit = 1;
    return 1;
}

elevator_arrives()
{
    say("The lamp on the button beside the elevator goes out.\n");
    lamp_is_lit = 0;
}

regenerate() {
    return call_other(this_player(), "remove_ghost", 0);
}

prevent_look_at_inv(str)
{
    return str != 0;
}

south() {
    call_other(this_player(), "move_player", "south#room/vill_green");
    return 1;
}
