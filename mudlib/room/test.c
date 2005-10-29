string east_door_open;
string amiga_present;
string amiga_power;
string name;
int a;

reset(arg) {
    if (arg)
	return;
    set_light(1);
    east_door_open = 0;
    amiga_present = 0;
    amiga_power = 0;
    name = 0;
    a = 0;
    move_object(clone_object("obj/bag"), this_object());
    move_object(clone_object("obj/bullboard"), this_object());
}

init() {
    add_action("open_door"); add_verb("open");
    add_action("go_east"); add_verb("east");
    add_action("sesam"); add_verb("sesam");
    add_action("hit"); add_verb("hit");
    write("You are in the computer room.\n");
}

long() {
    if (east_door_open)
	write("An empty room with an open door to the east.\n");
    if (!east_door_open)
	write("An empty room with a closed door to the east.\n");
    if (amiga_present) {
	if (!amiga_power)
	    write("There is an amiga here.\n");
	if (amiga_power)
	    write("There is a powered on amiga here.\n");
    }
}

open_door() {
    east_door_open = 1;
    write("Ok.\n");
}

close_door() {
    east_door_open = 0;
    write("Ok.\n");
}

go_east() {
    if (!east_door_open)
	write("The door is closed\n");
    if (east_door_open)
	move_object(this_player(), "room/rum2");
}

sesam() {
    write("An amiga materialises!\n");
    amiga_present = 1;
    add_action("power"); add_verb("power");
}

power() {
    amiga_power = 1;
    write("The screen lights up.\n");
}

door_open() {
    return east_door_open;
}

summon() {
    name = clone_object("obj/player");
    write("Summoning a player...\n");
    write(name);
    write(", His hp is ");
    write(call_other(name, "condition", 0));
    write("\n");
}

apa() {
    bepa(1);
}

hit() {
    if (!name) {
	write("Hit what ?\n");
	return;
    }
    call_other(name, "hit_player", 3);
}

fac(n) {
    if (n <= 0)
	return 1;
    return n * fac(n-1);
}


test() {
    a = a + 1;
    write("Fac "); write(a); write(" is "); write(fac(a)); write("\n");
}

short() {
    write("Computer room\n");
    if (amiga_present) {
	if (amiga_power)
	    write("A powered amiga.\n");
	if (!amiga_power)
	    write("An amiga.\n");
    }
    return 0;
}

