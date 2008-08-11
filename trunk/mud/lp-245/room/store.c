#define MAX_LIST	30

int value;
object name_of_item;

string short() {
    return "store room for the shop";
}

void init() {
    add_action("south", "south");
}

void list(object ob)
{
    int value;

    value = ob->query_value();
    if (value) {
	write(value*2 + ":\t" + ob->short() + ".\n");
    }
}

void inventory(string str)
{
    object ob;
    int max;
    if (!str)
	str = "all";
    max = MAX_LIST;
    ob = first_inventory(this_object());
    while(ob && max > 0) {
	if (str == "all") {
	    list(ob);
	    max -= 1;
	}
	if (str == "weapons" && ob->weapon_class()) {
	    list(ob);
	    max -= 1;
	}
	if (str == "armours" && ob->armour_class()) {
	    list(ob);
	    max -= 1;
	}
	ob = next_inventory(ob);
    }
}

int value(string item) {
    name_of_item = present(item);
    if (!name_of_item) {
	return 0;
    }
    value = name_of_item->query_value();
    if (!value) {
	return 0;
    }
    write("The "); write(item); write(" would cost you ");
    write(value*2); write(" gold coins.\n");
    return 1;
}

void buy(string item) {
    name_of_item = present(item);
    if (!name_of_item) {
	write("No such item in the store.\n");
	return;
    }
    value = name_of_item->query_value();
    if (!value) {
	write("Item has no value.\n");
	return;
    }
    value *= 2;
    if (this_player()->query_money() < value) {
	write("It would cost you ");
	write(value); write(" gold coins, which you don't have.\n");
	return;
    }
    if (!this_player()->add_weight(name_of_item->query_weight())) {
	write("You can't carry that much.\n");
	return;
    }
    this_player()->add_money(- value);
    move_object(name_of_item, this_player());
    write("Ok.\n");
    say(this_player()->query_name() + " buys " + item + ".\n");
}

int south() {
    this_player()->move_player("leaves#room/shop");
    return 1;
}

void heart_beat()
{
    object ob, next_ob;
    ob = first_inventory(this_object());
    while(ob) {
	next_ob = next_inventory(ob);
	destruct(ob);
	ob = next_ob;
    }
}

void reset(int arg) {
    if (!arg)
	set_light(1);
    if (!present("torch")) {
	object torch;
	torch = clone_object("obj/torch");
	torch->set_name("torch");
	torch->set_fuel(2000);
	torch->set_weight(1);
	move_object(torch, this_object());
    }
}

void long()
{
    write("All things from the shop are stored here.\n");
}

void store(object item)
{
    string short_desc;
    object ob;

    short_desc = item->short();
    ob = first_inventory(this_object());
    while(ob) {
	if (ob->short() == short_desc) {
	    /* Move it before destruct, because the weight
	       has already been compensated for. */
	    move_object(item, this_object());
	    destruct(item);
	    return;
	}
	ob = next_inventory(ob);
    }
    move_object(item, this_object());
}
