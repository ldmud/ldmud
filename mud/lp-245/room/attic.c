int lamp_is_lit;

void reset(int arg)
{
    if (arg)
	return;
    set_light(1);
}

void init()
{
    add_action("west", "west");
    add_action("open", "open");
    add_action("push", "push");
    add_action("push", "press");
    add_action("close", "close");
}

string short() {
    return "The attic";
}

void long(string str)
{
    if (str == "door") {
	if (!"room/elevator"->query_door() &&
	    "room/elevator"->query_level())
	    write("The door is open.\n");
	else
	    write("The door is closed.\n");
	return;
    }
    write("This is the attic above the church.\n" +
	"There is a door to the west.\n");
    if (lamp_is_lit)
        write("The lamp beside the elevator is lit.\n");

}

int id(string str) {
    return str == "door";
}

int west() {
    if ("room/elevator"->query_door() ||
	"room/elevator"->query_level() != 3) {
	write("The door is closed.\n");
	return 1;
    }
    this_player()->move_player("west#room/elevator");
    return 1;
}

int open(string str)
{
    if (str != "door")
	return 0;
    if ("room/elevator"->query_level() != 3) {
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

int push(string str)
{
    if (str && str != "button")
	return 0;
    if ("room/elevator"->call_elevator(3))
	lamp_is_lit = 1;
    return 1;
}

void elevator_arrives()
{
    say("The lamp on the button beside the elevator goes out.\n");
    lamp_is_lit = 0;
}

int prevent_look_at_inv(string str)
{
    return str != 0;
}
