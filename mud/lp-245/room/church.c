#include "std.h"

int lamp_is_lit, reboot_time, time_from_reset, last_reset_cycle;
int list_length;

void reset(string arg)
{
    if (time_from_reset)
	last_reset_cycle = time() - time_from_reset;
    time_from_reset = time();
    if (arg)
	return;
    set_light(1);
    reboot_time = time();
}

void init()
{
    add_action("west", "west");
    add_action("open", "open");
    add_action("push", "push");
    add_action("push", "press");
    add_action("close", "close");
    add_action("pray", "pray");
    add_action("pray", "regenerate");
    add_action("south", "south");
}

string short() {
    return "Village church";
}

void long(string str)
{
    if (str == "clock") {
	int i, j;
	write("The clock shows ");
	i = time() - reboot_time;
	j = i / 60 / 60 / 24;
	if (j == 1)
	    write("1 day ");
	else if (j > 0)
	    write(j + " days ");
	i -= j * 60 * 60 * 24;
	j = i / 60 / 60;
	if (j == 1)
	    write("1 hour ");
	else if (j > 0)
	    write(j + " hours ");
	i -= j * 60 * 60;
	j = i / 60;
	if (j == 1)
	    write("1 minute ");
	else if (j > 0)
	    write(j + " minutes ");
	i -= j * 60;
	if (i == 1)
	    write("1 second");
	else if (i > 0)
	    write(i + " seconds");
	write("\n");
	if (this_player()->query_level() < 20)
	    return;
	write("Time since reset is " + (time() - time_from_reset) +
	      " seconds.\n");
	if (last_reset_cycle)
	    write("Reset cycle: " + last_reset_cycle + "\n");
	write("Free block list length: " + list_length + "\n");
	return;
    }
    if (str == "door") {
	if (!"room/elevator"->query_door(0) &&
	    "room/elevator"->query_level(0))
	    write("The door is open.\n");
	else
	    write("The door is closed.\n");
	return;
    }
    if (str == "pit") {
	write("In the middle of the church is a deep pit.\n"+
	      "It was used for sacrifice in the old times, but nowadays\n" +
	      "it is only left for tourists to look at.\n");
	return;
    }
    write("You are in the local village church.\nThere is a huge pit in the center,\n" +
	 "and a door in the west wall. There is a button beside the door.\n");
    write("This church has the service of reviving ghosts. Dead people come\n");
    write("to the church and pray.\n");
    write("There is a clock on the wall.\n");
    write("There is an exit to south.\n");
    if (lamp_is_lit)
        write("The lamp beside the elevator is lit.\n");

}

int id(string str) {
    return str == "door" || str == "pit" || str == "clock";
}

int xyzzy() {
    write("Everything shimmers.\n");
    write("You wake up elsewhere...\n");
    this_player()->move_player("elsewhere#room/test");
    return 1;
}

int west() {
    if ("room/elevator"->query_door(0) ||
	"room/elevator"->query_level(0) != 2) {
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
    if ("room/elevator"->query_level(0) != 2) {
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
    if ("room/elevator"->call_elevator(2))
	lamp_is_lit = 1;
    return 1;
}

void elevator_arrives()
{
    say("The lamp on the button beside the elevator goes out.\n");
    lamp_is_lit = 0;
}

int pray() {
    return this_player()->remove_ghost();
}

int prevent_look_at_inv(string str)
{
    return str != 0;
}

int south() {
    this_player()->move_player("south#room/vill_green");
    return 1;
}

int query_drop_castle() {
    return 1;
}
