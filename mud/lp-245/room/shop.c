#include "std.h"

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("sell", "sell");\
    add_action("value", "value");\
    add_action("buy", "buy");\
    add_action("north", "north");\
    add_action("list", "list");

TWO_EXIT("room/vill_road2", "south",
	"room/storage", "west",
	 "The shop",
"You are in a shop. You can buy or sell things here.\n" +
"Commands are: 'buy item', 'sell item', 'sell all', 'list', 'list weapons'\n"+
"'list armours' and 'value item'.\n" +
"There is an opening to the north, and some shimmering\n" +
"blue light in the doorway.\nTo the west you see a small room.\n", 1)


object find_item_in_player(string i)
{
    object ob;

    ob = first_inventory(this_player());
    while(ob) {
        if (ob->id(i))
	    return ob;
	ob = next_inventory(ob);
    }
    return 0;
}

int do_sell(object ob) {
    int value, do_destroy;
    value = ob->query_value();
    if (!value) {
	write(ob->short() + " has no value.\n");
	return 1;
    }
    if (environment(ob) == this_player()) {
        int weight;
	if (ob->drop()) {
	    write("I can't take it from you!\n");
	    return 1;
	}
        weight = ob->query_weight();
	this_player()->add_weight(- weight);
    }
    if (value > 2000)
	do_destroy = 1;
    if (value > 1000) {
	write("The shop is low on money...\n");
	value = 1000;
    }
    write("You get "); write(value); write(" gold coins.\n");
    say(this_player()->query_name() + " sells " + ob->short() + ".\n");
    this_player()->add_money(value);
    add_worth(value, ob);
    if (do_destroy) {
	write("The valuable item is hidden away.\n");
	destruct(ob);
	return 1;
    }
    "room/store"->store(ob);
    return 1;
}

int sell(string item) {
    object ob;

    if (!item)
	return 0;
    if (item == "all") {
	object next;
	ob = first_inventory(this_player());
	while(ob) {
	    next = next_inventory(ob);
	    if (!ob->drop() && ob->query_value()) {
		write(ob->short() + ":\t");
		do_sell(ob);
	    }
	    ob = next;
	}
	write("Ok.\n");
	return 1;
    }
    ob = present(item, this_player());
    if (!ob)
	ob = present(item, this_object());
    if (!ob) {
	write("No such item ("); write(item); write(") here.\n");
	return 1;
    }
    do_sell(ob);
    return 1;
}

int value(string item) {
    int value;
    object name_of_item;

    if (!item)
	return 0;
    name_of_item = present(item);
    if (!name_of_item)
      name_of_item = find_item_in_player(item);
    if (!name_of_item) {
	if ("room/store"->value(item))
	    return 1;
	write("No such item ("); write(item); write(") here\n");
	write("or in the store.\n");
	return 1;
    }
    value = name_of_item->query_value();
    if (!value) {
	write(item); write(" has no value.\n");
	return 1;
    }
    write("You would get "); write(value); write(" gold coins.\n");
    return 1;
}

int buy(string item) {
    if (!item)
	return 0;
    "room/store"->buy(item);
    return 1;
}

int north() {
    if (this_player()->query_level() < 20) {
	write("A strong magic force stops you.\n");
	say(this_player()->query_name() +
	    " tries to go through the field, but fails.\n");
	return 1;
    }
    write("You wriggle through the force field...\n");
    this_player()->move_player("north#room/store");
    return 1;
}

int list(string obj) {
    "room/store"->inventory(obj);
    return 1;
}

int query_drop_castle() {
    return 1;
}
