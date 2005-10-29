
#include <mudlib.h>
#include <daemons.h>
#include <ships.h>

#define MAX_LEN 15

inherit ROOM;

#define RN this_player()->query_real_name()

// Variables
string type, name, local_port;
string * ports;
int class, decks;
status in_progress, cargo;
mixed *info;

// Predefined Functions
void get_ship_name(string nm);
void configure_decks(int num);
void get_deck_info(string str, int num);
void ask_ship_name();
object find_in_port(string str);

// Room Code
void reset(int arg) {
  ::reset(arg);
  if (arg) return;
  ports = ({ });
}

int purchase_func(string str) {
  if (!str) {
    write("Purchase what?\n");
    return 1;
  }

  if (!SHIP_TYPES[str]) {
    write("We don't sell " + str + "'s here.\n");
    return 1;
  }

  if (this_player()->query_money() < SHIP_TYPES[str, COST]) {
    write("You don't have the " + SHIP_TYPES[str, COST] +
     " coins it takes to buy a " + str + "\n");
    return 1;
  }

  if (in_progress) {
    write("Someone else is already purchasing a ship.\n\
Please try again after they have finished.\n");
    return 1;
  }

  in_progress = 1;
  cargo = 0;
  type = str;
  class = SHIP_TYPES[type, DECKS];
  decks = class - 1; // Decks are actually 1 less than what's stored in
  // SHIP_TYPES due to 1 mandatory control deck.

  this_player()->add_money(-SHIP_TYPES[type, COST]);

  info = ({ SHIP_TYPES[type, MAINDECK] });

  if (!SHIP_D->query_fleet(RN)) {
    write("You have not yet named your fleet.\n\
Please name it now, with no spaces in the name:  ");
    input_to("get_fleet_name");
  } else ask_ship_name();

  return 1;
}

void get_fleet_name(string str) {
  string t1, t2;
  if (str == "") {
    write("You must enter a name:  ");
    input_to("get_fleet_name");
  } else if (strlen(str) > MAX_LEN) {
    write("That name is too long.\nPlease choose another one:  ");
    input_to("get_fleet_name");
  } else if (SHIP_D->query_fleet_names(str)) {
    write("That fleet name is taken already.\nPlease choose another one:  ");
    input_to("get_fleet_name");
  } else if(sscanf(str, "%s %s", t1, t2) == 2) {
    write("You have entered a fleet name with one or more spaces in it.\n\
This is not allowed.\nPlease choose another one:  ");
    input_to("get_fleet_name");
  } else {
    SHIP_D->add_fleet(RN, lower_case(str));
    write("\n\n");
    ask_ship_name();
  }
  return;
}

void ask_ship_name() {
  write("\
You have become the proud owner of a newly constructed " + type + ".\n\n\
Please name your new ship, with no spaces in the name:  ");
  input_to("get_ship_name");
  return;
}

void get_ship_name(string str) {
  string t1, t2;
  if (str == "") {
    write("You must enter a name:  ");
    input_to("get_ship_name");
  } else if (strlen(str) > MAX_LEN) {
    write("That name is too long.\nPlease choose another one:  ");
    input_to("get_ship_name");
  } else if (SHIP_D->query_ship_name(lower_case(str), RN)) {
    write("That ship name is taken already.\nPlease choose another one:  ");
    input_to("get_ship_name");
  } else if(sscanf(str, "%s %s", t1, t2) == 2) {
    write("You have entered a ship name with one or more spaces in it.\n\
This is not allowed.\nPlease choose another one:  ");
    input_to("get_ship_name");
  } else {
    write("\n\
Now you get to configure the " + decks + " decks of your ship.  Each deck has\n\
four rooms.  Those rooms can be either Cargo Holds or Gunnery Rooms.\n\n");
    name = lower_case(str);
    configure_decks(decks);
  }
  return;
}

void setup_ship() {
  SHIP_D->add_ship_name(name, RN);
  SHIP_D->setup_ship(name, type, RN, local_port, info);

  if (!this_player()->query_event("login", SHIP_D, "load_fleet"))
    this_player()->add_event("login", SHIP_D, "load_fleet", RN);
  if (!this_player()->query_event("logout", SHIP_D, "reset_fleet"))
    this_player()->add_event("logout", SHIP_D, "reset_fleet", RN);

  in_progress = 0;
  cargo = 0;

  write("\
Your new ship is now in the port.  You'll need to recruit crewmembers,\n\
purchase provisions, and more. Type 'help ships' for more info.\n\n");
  return;
}

void configure_decks(int num) {
  if (type == "cog") {
    write("Please pick one of the following configurations:\n\
  1) 4 Cargo Holds, 0 Gunnery Rooms.\n\
  2) 2 Cargo Holds, 2 Gunnery Rooms (2 Catapults).\n\
  3) 2 Cargo Holds, 2 Gunnery Rooms (2 Ballistas).\n\
Please enter the number of your choice:  ");
    input_to("get_deck_info", 0, (num-1));
  } else if (cargo == 0 && num == 1) {
    write("\nFor your last deck, you must have at least 2 Cargo Holds\n\
so your ship will be able to move.\n\
Please pick one of the following configurations:\n\
  1) 4 Cargo Holds, 0 Gunnery Rooms.\n\
  2) 2 Cargo Holds, 2 Gunnery Rooms (2 Catapults).\n\
  3) 2 Cargo Holds, 2 Gunnery Rooms (2 Ballistas).\n\
Please enter the number of your choice:  ");
    input_to("get_deck_info", 0, (num-1));
  } else {
    write("\n\
For deck " + num + " please pick one of the following configurations:\n\
  1) 4 Cargo Holds, 0 Gunnery Rooms.\n\
  2) 2 Cargo Holds, 2 Gunnery Rooms (2 Catapults).\n\
  3) 2 Cargo Holds, 2 Gunnery Rooms (2 Ballistas).\n\
  4) 0 Cargo Holds, 4 Gunnery Rooms (4 Catapults).\n\
  5) 0 Cargo Holds, 4 Gunnery Rooms (4 Ballistas).\n\
  6) 0 Cargo Holds, 4 Gunnery Rooms (2 Catapults, 2 Ballistas).\n\
Please enter the number of your choice:  ");
    input_to("get_deck_info", 0, (num-1));
  }

  return;
}

void get_deck_info(string str, int num) {
  int i;

  i = to_int(str);
  if (i > 0 && i < 7) {
    if (i == 1 || i == 2 || i == 3) cargo = 1;
    info += ({ TYPES[i-1] });
    if (num) configure_decks(num);
    else setup_ship();
  } else {
    write("\nPlease enter a valid selection!\n");
    num++;
    configure_decks(num);
  }
  return;
}

void set_local_port(string str) {
  local_port = str;
  return;
}

void add_port(string str) {
  ports += ({ str });
  return;
}


int scrap_func(string str) {
  object ob;
  string tp;
  int cost;
write("Scrapping is currently disabled.\n");
return 1;

  if (!str) {
    write("Scrap what?\n");
    return 1;
  }

  if (!SHIP_D->query_ship_name(lower_case(str), RN)) {
    write("You have no ship named " + str + " in your fleet!\n");
    return 1;
  }

  ob = find_in_port(str);

  if (!ob) {
    write("The ship " + str + " isn't at port.\n");
    return 1;
  }

  tp = ob->query_type();

  switch(tp) {
  case "cog" : cost = 20000; break;
  case "caravel" : cost = 32000; break;
  case "galleon" : cost = 45000; break;
  }

  ob->sink();
  ob = find_in_port("wreck_" + tp);
  if (ob) destruct(ob);
  this_player()->add_money(cost);
  write("Your ship has been scrapped and you have been reimbursed " + cost +
   " coins.\n");

  return 1;
}

object find_in_port(string str) {
  object ob;
  int i, siz;

  siz = sizeof(ports);

  for(i = 0; i < siz; i++) {
    if(find_object(ports[i]))
      // lexicon, 3-28-98.  temp fix to some bug (bad arg 2 to present) //
      ob = present(str, find_object(ports[i]));
    if (ob) break;
  }

  return ob;
}

int load_func() {
  if (!this_player()->query_event("login", SHIP_D, "load_fleet"))
    this_player()->add_event("login", SHIP_D, "load_fleet", RN);
  if (!this_player()->query_event("logout", SHIP_D, "reset_fleet"))
    this_player()->add_event("logout", SHIP_D, "reset_fleet", RN);
  SHIP_D->load_fleet(this_player()->query_real_name());
  return 1;
}

int rescue_func(string str) {
  object ob;

  if (!str) {
    write("Rescue what?\n");
    return 1;
  }

  ob = find_in_port(str);

  if (!ob) {
    write("The ship " + str + " isn't at port.\n");
    return 1;
  }

  if (!ob->query_stranded()) {
    if (!ob->query_docked()) {
      ob->set_docked(1, local_port);
      write("Your ship was re-doecked for you.\n");
    } else
      write(
       "That ship was not stranded, and is not being held ransom.\n");
    return 1;
  }

  if (this_player()->query_money() < SHIP_TYPES[str, RESCUE]) {
    write("You don't have the " + SHIP_TYPES[str, RESCUE] +
     " coins it takes to rescue her.\n");
  } else {
    this_player()->add_money(-SHIP_TYPES[type, RESCUE]);
    write("Your ship has been rescued and she's free to sail again now.\n");
    ob->set_stranded(0);
    ob->set_docked(1, local_port);
  }

  return 1;

}

