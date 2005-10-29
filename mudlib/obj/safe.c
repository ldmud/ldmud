object listen_ob;
int num_turn, safe_is_unlocked, safe_is_open;
object money;

reset(arg) {
    num_turn = 0;
    safe_is_unlocked = 0;
    safe_is_open = 0;
    if (!money || environment(money) != this_object()) {
	money = clone_object("obj/money");
	call_other(money, "set_money", random(1000));
	move_object(money, this_object());
    }
}

long(str) {
    if (str == "wheel" || str == "code wheel") {
	write("You see nothing special.\n");
	return;
    }
    write("It is a rather small safe, formed as a cube. It looks\n");
    write("Very heavy. On the safe is a numbered code wheel.\n");
}

short() {
    if (safe_is_open)
	return "A safe (open)";
    return "A safe";
}

init() {
    add_action("open"); add_verb("open");
    add_action("turn"); add_verb("turn");
}

open(str) {
    if (!id(str))
	return 0;
    if (!safe_is_unlocked) {
	write("The safe is locked.\n");
	return 1;
    }
    safe_is_open = 1;
    write("Ok.\n");
    say(call_other(this_player(), "query_name") + " opens the safe.\n");
    return 1;
}

id(str) {
    return str == "safe" || str == "wheel" || 
	str == "code wheel";
}

turn(str) {
    int listen;
    if (str != "wheel" && str != "code wheel")
	return 0;
    if (listen_ob && call_other(listen_ob, "query_listening")) {
	num_turn += 1;
	if (num_turn >= 3) {
	    write("klock\n");
	    safe_is_unlocked = 1;
	    return 1;
	}
	write("klick\n");
	return 1;
    }
    write("You turn the wheel randomly, but nothing happens.\n");
    say(call_other(this_player(), "query_name") +
	" turns the wheel on the safe randomly.\n");
    return 1;
}

use_stethoscope(stet)
{
    listen_ob = stet;
    return 1;
}

can_put_and_get() {
    return safe_is_open;
}

add_weight() { return 1; }
