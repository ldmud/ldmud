object newspaper, top_list;

reset(arg) {
    if (!present("go player"))
	move_object(clone_object("obj/go_player"), this_object());
    if (!top_list || !present(top_list)) {
	top_list = clone_object("obj/level_list");
	move_object(top_list, this_object());
    }
    if (!newspaper || !present(newspaper)) {
	newspaper = clone_object("obj/newspaper");
	move_object(newspaper, this_object());
    }
    if (arg) return;
    set_light( 1);
}

short() {
    return "The local pub";
}

init() {
    add_action("move");
    add_verb( "west");
    add_action("order");
    add_verb("order");
    add_action("order");
    add_verb("buy");
}

move() {
    object ob;

    ob = first_inventory(this_player());
    while(ob) {
	if (call_other(ob, "id", "drk2")) {
	    tell_object(this_player(),
			"You are not allowed to leave with drinks!\n");
	    return 1;
	}
	ob = next_inventory(ob);
    }
    call_other(this_player(), "move_player",  "west" + "#" +"room/yard");
    return 1;
}

long() {
    write("You are in the local pub.\n");
    write("You can order drinks here.\n\n");
    write("     First class beer    : 12 coins\n");
    write("     Special of the house: 50 coins\n");
    write("     Firebreather        : 150 coins\n\n");
    /* write("     Potion of healing   : 200 coins\n\n"); */
    write("The only obvious exit is to " +  "west" + ".\n");
}

order(str)
{
    object drink;
    string name, short_desc, mess;
    int value, cost, strength, heal;

    if (!str) {
       write("Order what ?\n");
       return 1;
    }
    if (str == "beer") {
	name = str;
	short_desc = "A bottle of beer";
	mess = "That feels good";
	heal = 0;
	value = 12;
	strength = 2;
    }
    else if (str == "special" || str == "special of the house") {
	name = "special";
	short_desc = "A special of the house";
	mess = "A tingling feeling goes through your body";
	heal = 10;
	value = 50;
	strength = 8;
    } else if (str == "firebreather" || str == "fire") {
	name = "firebreather";
	short_desc = "A firebreather";
	mess = "A shock wave runs through your body";
	heal = 25;
	value = 150;
	strength = 12;
#if 0
    } else if (str == "potion" || str == "potion of healing") {
	name = "potion";
	short_desc = "A potion of healing";
	mess = "You are totally healed";
	heal = 1000;
	value = 200;
	strength = 0;
#endif
    } else {
       write("What ?\n");
       return 1;
    }
    if (call_other(this_player(), "query_money", 0) < value) {
        write("That costs " + value + " gold coins, which you don't have.\n");
	return 1;
    }
    drink = clone_object("obj/drink");
    if (!call_other(this_player(), "add_weight",
	call_other(drink, "query_weight"))) {
	write("You can't carry more.\n");
	destruct(drink);
	return 1;
    }
    if (!call_other(drink, "set_value", name + "#" + short_desc + "#" + mess +
	"#" + heal + "#" + value + "#" + strength)) {
	write("Error in creating drink.\n");
	destruct(drink);
	return 1;
    }
    call_other(drink, "set_pub");
    move_object(drink, this_player());
    call_other(this_player(), "add_money", - value);
    write("You pay " + value + " for a " + name + ".\n");
    say(call_other(this_player(), "query_name", 0) + " orders a " +
	name + ".\n");
    return 1;
}
