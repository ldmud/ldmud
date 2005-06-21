int full;

int id(string str) {
    if (str == "beer" && full)
	return 1;
    return str == "bottle";
}

string short() {
    if (full)
	return "bottle of beer";
    return "empty bottle";
}

/* The shop only buys empty bottles ! */

int query_value()
{
    if (!full) return 10;
    return 0;
}

void long() {
    write(short() + ".\n");
}

void reset(int arg) {
    if (arg)
        return;
    full = 1;
}

int drink(string str)
{
    if (str && str != "beer" && str != "from bottle")
	return 0;
    if (!full)
	return 0;
    if (!this_player()->drink_alcohol(2))
	return 1;
    full = 0;
    write("It is really good beer!\n");
    say(this_player()->query_name() +
	" drinks a bottle of beer.\n");
    return 1;
}

void init() {
    add_action("drink", "drink");
}

int get() {
    return 1;
}

int query_weight() {
    return 1;
}
