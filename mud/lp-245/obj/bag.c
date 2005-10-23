#define MAX_WEIGTH	6
int local_weight;

void long() {
    write("A bag. ");
    if (first_inventory(this_object()))
	write("There is something in it.\n");
    else
	write("You can put things in it.\n");
}

void reset(int arg) {
    if (arg)
	return;
    local_weight = 0;
}

int query_weight() {
    return 1;
}

int add_weight(int w) {
    if (local_weight + w > MAX_WEIGTH)
	return 0;
    local_weight += w;
    return 1;
}

string short() {
    return "bag";
}

int id(string str) {
    return str == "bag";
}

int query_value() {
    return 12;
}

int can_put_and_get() { return 1; }

int get() {
    return 1;
}

int prevent_insert() {
    if (local_weight > 0) {
	write("You can't when there are things in the bag.\n");
	return 1;
    }
    return 0;
}
