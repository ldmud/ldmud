int east_door_open;
int amiga_present;
int amiga_power;
object name;
int a;

void reset(int arg) {
    if (arg)
	return;
    set_light(1);
    east_door_open = 0;
    amiga_present = 0;
    amiga_power = 0;
    name = 0;
    a = 0;
}

void init() {
    add_action("open_door", "open");
    add_action("go_east", "east");
    add_action("sesam", "sesam");
    add_action("hit", "hit");
    write("You are in the computer room.\n");
}

void long() {
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

void open_door() {
    east_door_open = 1;
    write("Ok.\n");
}

void close_door() {
    east_door_open = 0;
    write("Ok.\n");
}

void go_east() {
    if (!east_door_open)
	write("The door is closed\n");
    if (east_door_open)
	move_object(this_player(), "room/rum2");
}

void sesam() {
    write("An amiga materialises!\n");
    amiga_present = 1;
    add_action("power", "power");
}

void power() {
    amiga_power = 1;
    write("The screen lights up.\n");
}

int door_open() {
    return east_door_open;
}

void summon() {
    name = clone_object("obj/player");
    write("Summoning a player...\n");
    write(name);
    write(", His hp is ");
    write(name->condition());
    write("\n");
}

void hit() {
    if (!name) {
	write("Hit what ?\n");
	return;
    }
    name->hit_player(3);
}

int fac(int n) {
    if (n <= 0)
	return 1;
    return n * fac(n-1);
}


void test() {
    a = a + 1;
    write("Fac "); write(a); write(" is "); write(fac(a)); write("\n");
}

string short() {
    write("Computer room\n");
    if (amiga_present) {
	if (amiga_power)
	    write("A powered amiga.\n");
	if (!amiga_power)
	    write("An amiga.\n");
    }
    return 0;
}

