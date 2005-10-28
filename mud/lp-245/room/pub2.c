/* #define MUST_STAY_WITH_DRINKS */
object top_list;
object player;
int current_problem, starts_thinking;
object solved_by, wrong_by;
int problem_value;

object rules;

void start_player();
void notify(string str);

void reset(int arg) {
    start_player();
    if (!top_list || !present(top_list, this_object())) {
	top_list = clone_object("obj/level_list");
	move_object(top_list, this_object());
    }
    if (!rules || !present(rules, this_object())) {
	rules = clone_object("obj/Go/rules");
	move_object(rules, this_object());
    }
    if (arg) return;
    set_light( 1);
}

string short() {
    return "The local pub";
}

void init() {
    add_action("move", "west");
    add_action("order", "order");
    add_action("order", "buy");
    add_action("look", "look");
}

int move() {
#ifdef MUST_STAY_WITH_DRINKS
     if (has_drink(this_player())) {
        tell_object(this_player(),
                    "You are not allowed to leave with drinks!\n");
        return 1;
     }
#endif
     this_player()->move_player("west#room/yard");
     return 1;
}

void long() {
    write("You are in the local pub.\n");
    write("You can order drinks here.\n\n");
    write("     First class beer    : 12 coins\n");
    write("     Special of the house: 50 coins\n");
    write("     Firebreather        : 150 coins\n\n");
    /* write("     Potion of healing   : 200 coins\n\n"); */
    write("The only obvious exit is to " +  "west" + ".\n");
}

int order(string str)
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
    if (this_player()->query_money() < value) {
        write("That costs " + value + " gold coins, which you don't have.\n");
	return 1;
    }
    drink = clone_object("obj/drink");
    if (!this_player()->add_weight(drink->query_weight())) {
	write("You can't carry more.\n");
	destruct(drink);
	return 1;
    }
    if (!drink->set_value(name + "#" + short_desc + "#" + mess +
	"#" + heal + "#" + value + "#" + strength)) {
	write("Error in creating drink.\n");
	destruct(drink);
	return 1;
    }
    drink->set_pub();
    move_object(drink, this_player());
    this_player()->add_money(- value);
    write("You pay " + value + " for a " + name + ".\n");
    say(this_player()->query_name() + " orders a " +
	name + ".\n");
    return 1;
}

/*
 * Make this global, and only initialized once.
 */
string * chat_str, * action, * type, * match;

void start_player() {
    if(!player) {
	player = clone_object("obj/monster");
	player->set_name("player");
	player->set_alias("go player");
	player->set_short("Go player");
	player->set_long(
		   "A man sitting beside a go board, concentrating on a problem.\n" +
		   "He looks as if he wants help. Why not look at his problem,\n" +
		   "and tell him where to play ?\n");
	player->set_level(10);
	player->set_al(200);
	player->set_ep(39000);
	player->set_hp(100);
	player->set_wc(12);
	move_object(player, "room/pub2");
	if (!action) {
	    action = allocate(2);
	    type = allocate(2);
	    match = allocate(2);
	    action[0] = "got_play";
	    type[0] = "says:";
	    match[0] = " play";
	    action[1] = "make_move";
	    type[1] = "PISS";
	    match[1] = " OFF";
	}
	player->set_match(this_object(), action, type, match);
	if (!chat_str) {
	    chat_str = allocate(5);
	    chat_str[0] = "Go player says: Hm. This is tricky!\n";
	    chat_str[1] = "Go player says: The moron who wrote this book didn't deal with this problem.\n";
	    chat_str[2] = "Go player says: A throw in here should just be wasted.\n";
	    chat_str[3] = "Go player says: This group is more alive than I am.\n";
	    chat_str[4] = "Go player says: This is simple using oi-otoshi.\n";
	}
	player->load_chat(1, chat_str);
    }
    current_problem = 0;
}

int got_play(string str) {
    string who, what;
    if (sscanf(str, "%s tells you: play %s\n", who , what) == 2 ||
	sscanf(str, "%s says: play %s\n", who , what) == 2) {
	if (current_problem == 0) {
	    if (what == "b1" || what == "b 1" || what == "1b" || what == "1 b")
		solved_by = find_living(lower_case(who));
	    else
		wrong_by = find_living(lower_case(who));
	    problem_value = 50;
	}
	if (current_problem == 1) {
	    if (what == "b2" || what == "b 2" || what == "2b" || what == "2 b")
		solved_by = find_living(lower_case(who));
	    else
		wrong_by = find_living(lower_case(who));
	    problem_value = 100;
	}
	if (current_problem == 2) {
	    if (what == "d3" || what == "d 3" || what == "3d" || what == "3 d")
		 solved_by = find_living(lower_case(who));
	     else
		 wrong_by = find_living(lower_case(who));
	    problem_value = 200;
	}
	notify("The go player contemplates a proposed play.\n");
	tell_object(player, "Arne PISS OFF\n");
          /* Crude way to trigger the Go players next action */
    } else if (sscanf(str, "%s tells you: %s", who, what) == 2) {
	say("The go player says: what ?\n");
    }
    return 0;
}

void show_problem() {
    if(current_problem > 2) {
	write("The player looks tired.\n");
	return;
    }
    if(!player || !living(player))
       return;
    write("The board looks like this:\n\n");
    say(this_player()->query_name() +
	" examines the go problem.\n");
    if (current_problem == 0) {
	write("5|.......\n" +
	      "4|.......\n" +
	      "3|@@@@@..\n" +
	      "2|OOOO@..\n" +
	      "1|...O@..\n" +
	      " --------\n" +
	      "  abcdefg\n" +
	      "\nIt is black ('@') to play.\n");
	return;
    } else if (current_problem == 1) {
	write("7|.......\n" +
	      "6|.......\n" +
	      "5|@@@....\n" +
	      "4|OOO@@..\n" +
	      "3|O.OO@..\n" +
	      "2|...O@..\n" +
	      "1|..OO@..\n" +
	      " --------\n" +
	      "  abcdefg\n" +
	      "\nIt is black ('@') to play.\n");
	return;
    } else if (current_problem == 2) {
	write("5|..........\n" +
	      "4|...@@@@@..\n" +
	      "3|@@@.O...@.\n" +
	      "2|@OO@OOOO@.\n" +
	      "1|@OO.O...@.\n" +
	      " -----------\n" +
	      "  abcdefg\n" +
	      "\nIt is white ('O') to play.\n");
	return;
    } else {
	write("The go player does not want to be disturbed any more.\n");
    }
}

void make_move(string str) {
    if (solved_by) {
	int i;
	i = current_problem + 1;
	say("The go player says: Right ! That works !\n" +
	    "He immediately plays out a new problem.\n");
	tell_object(solved_by,
		    "You feel that you have gained some experience.\n");
	solved_by->add_exp(problem_value);
	solved_by = 0;
	current_problem += 1;
    }
    if (wrong_by) {
	say("The go player says: No, that doesn't work.\n");
	say("He sinks back into his deep thought.\n");
	wrong_by = 0;
    }
}

void notify(string str) {
    say(str);
    write(str);
}

int look(string str) {
    string what, rest;
    if(str) {
	if(!player || !living(player))
	    return 0;
	if(sscanf(str, "at %s", what) == 1 || sscanf(str, "%s", what) == 1) {
	    if(what == "game" || what == "problem" || what == "board") {
		show_problem();
		return 1;
	    }
	}
    }
    return 0;
}

#ifdef MUST_STAY_WITH_DRINKS
int has_drink(object obj) {
     int drink;
     object ob;
     ob = first_inventory(obj);
     while(ob) {
        if (ob->id("drk2"))
             drink = 1;
        if (ob->can_put_and_get()) {
             if (has_drink(ob))
                  drink = 1;
        }
        ob = next_inventory(ob);
     }
     return drink;
}
#endif

int query_drop_castle() {
    return 1;
}
