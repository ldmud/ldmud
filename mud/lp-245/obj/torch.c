/*
 * This is a generic torch.
 * It will have some good initialisations by default.
 * The torch can't be sold when it is lighted.
 */

int amount_of_fuel;
string name;
status is_lighted;
int weight;

string short() {
    if (is_lighted)
	return name + " (lighted)";
    return name;
}

void long() {
    write(short() + "\n");
}

void reset(int arg) {
    if (arg)
	return;
    amount_of_fuel = 2000; name = "torch"; is_lighted = 0; weight = 1;
}

void set_weight(int w) { weight = w; }

int query_weight() { return weight; }

void set_name(string n) { name = n; }
void set_fuel(int f) { amount_of_fuel = f; }

void init() {
    add_action("light", "light");
    add_action("extinguish", "extinguish");
}

int light(string str) {
    if (!str || str != name)
	return 0;
    if (is_lighted) {
	write("It is already lighted.\n");
	return 1;
    }
    is_lighted = 1;
    call_out("out_of_fuel", amount_of_fuel * 2);
    if (set_light(1) == 1) {
	write("You can see again.\n");
	say(this_player()->query_name() +
	    "lights a " + name + "\n");
    } else
	write("Ok.\n");
    amount_of_fuel = 0;
    return 1;
}

void out_of_fuel() {
    object ob;
    if (set_light(-1) == 0)
	say("There is darkness as a " + name + " goes dark.\n");
    else
	say("The " + name + " goes dark.\n");
    ob = environment(this_object());
    if (living(ob))
	ob->add_weight(-weight);
    destruct(this_object());
}

int id(string str) {
    return str == name;
}

int query_value() {
    return amount_of_fuel/100;
}

int get() { return 1; }

int extinguish(string str) {
    int i;

    if (str && !id(str))
	return 0;
    if (!is_lighted)
	return 0;
    i = remove_call_out("out_of_fuel");
    if (i == -1) {
	write("Error.\n");
	return 1;
    }
    amount_of_fuel = i/2;
    is_lighted = 0;
    if (set_light(-1) == 0) {
	write("It turns dark.\n");
	say(this_player()->query_name() +
	    " extinguishes the only light source.\n");
    } else {
	write("Ok.\n");
    }
    return 1;
}
