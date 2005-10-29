#include "std.h"

#undef EXTRA_INIT
#define EXTRA_INIT\
    add_action("sell"); add_verb("sell");\
    add_action("value"); add_verb("value");\
    add_action("buy"); add_verb("buy");\
    add_action("north"); add_verb("north");\
    add_action("list"); add_verb("list");

ONE_EXIT("room/vill_road2", "south",
	 "The shop",
	 "You are in a shop. You can buy or sell things here.\n" +
	 "Commands are: 'buy item', 'sell item', 'list', 'list weapons'\n" +
	 "'list armors' and 'value item'.\n" +
	 "There is an opening to the north, and some shimmering\n" +
	 "blue light in the doorway.\n", 1)


sell(item) {
    int value;
    string name_of_item;
    object ob;

    if (!item)
	return 0;
    name_of_item = present(item, this_player());
    if (!name_of_item)
      name_of_item = find_item_in_player(item);
    if (!name_of_item) {
	write("No such item ("); write(item); write(") here.\n");
	return 1;
    }
    value = call_other(name_of_item, "query_value", 0);
    if (!value) {
	write(item); write(" has no value.\n");
	return 1;
    }
    if (environment(name_of_item) == this_player()) {
        int weight;
	if (call_other(name_of_item, "drop", 0)) {
	    write("I can't take it from you!\n");
	    return 1;
	}
        weight = call_other(name_of_item, "query_weight", 0);
	call_other(this_player(), "add_weight", - weight);
    }
    if (value > 1000) {
	write("The shop is low on money...\n");
	value = 1000;
    }
    write("You get "); write(value); write(" gold coins.\n");
    say(call_other(this_player(), "query_name", 0) + " sells " +
	call_other(name_of_item, "short", 0) + ".\n");
    call_other(this_player(), "add_money", value);
    add_worth(value, name_of_item);
    call_other("room/store", "store", name_of_item);
    return 1;
}

value(item) {
    int value;
    string name_of_item;

    if (!item)
	return 0;
    name_of_item = present(item);
    if (!name_of_item)
      name_of_item = find_item_in_player(item);
    if (!name_of_item) {
	if (call_other("room/store", "value", item))
	    return 1;
	write("No such item ("); write(item); write(") here\n");
	write("or in the store.\n");
	return 1;
    }
    value = call_other(name_of_item, "query_value", 0);
    if (!value) {
	write(item); write(" has no value.\n");
	return 1;
    }
    if (value < 1001) {
	write("You would get "); write(value); write(" gold coins.\n");
    }
    else {
	write("It's really worth "); write(value); write(" gold coins.\n");
	write("But the store is low on money, so you would only get 1000.\n");
    }
    return 1;
}

buy(item) {
    if (!item)
	return 0;
    call_other("room/store", "buy", item);
    return 1;
}

north() {
    if (call_other(this_player(), "query_level", 0) < 20) {
	write("A strong magic force stops you.\n");
	say(call_other(this_player(), "query_name", 0) +
	    " tries to go through the field, but fails.\n");
	return 1;
    }
    write("You wriggle through the force field...\n");
    call_other(this_player(), "move_player", "north#room/store");
    return 1;
}

list(obj) {
    call_other("room/store", "inventory", obj);
    return 1;
}

find_item_in_player(i)
{
    object ob;

    ob = first_inventory(this_player());
    while(ob) {
        if (call_other(ob, "id", i))
	    return ob;
	ob = next_inventory(ob);
    }
    return 0;
}
