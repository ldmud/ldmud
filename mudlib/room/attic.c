int lamp_is_lit;

reset(arg)
{
    if (arg)
	return;
    set_light(1);
}

init()
{
    add_action("west"); add_verb("west");
    add_action("open"); add_verb("open");
    add_action("push"); add_verb("push");
    add_action("push"); add_verb("press");
    add_action("close"); add_verb("close");
}

short() {
    return "The attic";
}

long(str)
{
    if (str == "door") {
	if (!call_other("room/elevator", "query_door", 0) &&
	    call_other("room/elevator", "query_level", 0))
	    write("The door is open.\n");
	else
	    write("The door is closed.\n");
	return;
    }
    write("This is the attic above the church.\n");
    if (lamp_is_lit)
        write("The lamp beside the elevator is lit.\n");

}

id(str) {
    return str == "door";
}

west() {
    if (call_other("room/elevator", "query_door", 0) ||
	call_other("room/elevator", "query_level", 0) != 3) {
	write("The door is closed.\n");
	return 1;
    }
    call_other(this_player(), "move_player", "west#room/elevator");
}

open(str)
{
    if (str != "door")
	return 0;
    if (call_other("room/elevator", "query_level", 0) != 3) {
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
    if (call_other("room/elevator", "call_elevator", 3))
	lamp_is_lit = 1;
    return 1;
}

elevator_arrives()
{
    say("The lamp on the button beside the elevator goes out.\n");
    lamp_is_lit = 0;
}

prevent_look_at_inv(str)
{
    return str != 0;
}
