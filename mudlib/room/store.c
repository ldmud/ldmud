#define MAX_LIST	30

int value;
string name_of_item;

short() {
    return "store room for the shop";
}

init() {
    add_action("south"); add_verb("south");
}

inventory(str)
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
	if (str == "weapons" && call_other(ob, "weapon_class", 0)) {
	    list(ob);
	    max -= 1;
	}
	if (str == "armors" && call_other(ob, "armor_class", 0)) {
	    list(ob);
	    max -= 1;
	}
	ob = next_inventory(ob);
    }
}

list(ob)
{
    int value;
    
    value = call_other(ob, "query_value", 0);
    if (value) {
	write(value*2 + ":\t" + call_other(ob, "short") + ".\n");
    }
}

value(item) {
    name_of_item = present(item);
    if (!name_of_item) {
	return 0;
    }
    value = call_other(name_of_item, "query_value", 0);
    if (!value) {
	return 0;
    }
    write("The "); write(item); write(" would cost you ");
    write(value*2); write(" gold coins.\n");
    return 1;
}

buy(item) {
    name_of_item = present(item);
    if (!name_of_item) {
	write("No such item in the store.\n");
	return;
    }
    value = call_other(name_of_item, "query_value", 0);
    if (!value) {
	write("Item has no value.\n");
	return;
    }
    value *= 2;
    if (call_other(this_player(), "query_money", 0) < value) {
	write("It would cost you ");
	write(value); write(" gold coins, which you don't have.\n");
	return;
    }
    if (!call_other(this_player(), "add_weight",
		    call_other(name_of_item, "query_weight"))) {
	write("You can't carry that much.\n");
	return;
    }
    call_other(this_player(), "add_money", 0 - value);
    move_object(name_of_item, this_player());
    write("Ok.\n");
    say(call_other(this_player(), "query_name") + " buys " + item + ".\n");
}

south() {
    call_other(this_player(), "move_player", "leaves#room/shop");
    return 1;
}

heart_beat()
{
    object ob, next_ob;
    ob = first_inventory(this_object());
    while(ob) {
	next_ob = next_inventory(ob);
	destruct(ob);
	ob = next_ob;
    }
}

reset(arg) {
    if (!arg)
	set_light(1);
    if (!present("torch")) {
	object torch;
	torch = clone_object("obj/torch");
	call_other(torch, "set_name", "torch");
	call_other(torch, "set_fuel", 2000);
	call_other(torch, "set_weight", 1);
	move_object(torch, this_object());
    }
}

long()
{
    write("All things from the shop are stored here.\n");
}

store(item)
{
    string short_desc;
    object ob;

    short_desc = call_other(item, "short");
    ob = first_inventory(this_object());
    while(ob) {
	if (call_other(ob, "short") == short_desc) {
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

realm() { return "NT"; }
