#include <mudlib.h>
#include <daemons.h>
#include <ships.h>

inherit ROOM;

#define RN this_player()->query_real_name()
#define PURCHASE_CLOSED 0

// Varibles
static string * masPorts;
int * maiAvailShips;
string msPortName;

// Predefined Functions
object find_in_port(string str);

// Room Code
void reset(int arg) {
  ::reset(arg);
  if (arg) return;
  masPorts = ({ });
  maiAvailShips = ({0,0,0});
}

void init() {
  ::init();
  add_action("lookup_func","lookup");
  add_action("inquire_func","inquire");
  add_action("request_func","request");
  add_action("turnin_func","turnin");
  add_action("read_func","read");
}

status is_available(string sClass) {
  if(sClass == "cog") {
    if(maiAvailShips[0])
      return 1;
    return 0;
  }
  if(sClass == "galley") {
    if(maiAvailShips[1])
      return 1;
    return 0;
  }
  if(sClass == "carrack") {
    if(maiAvailShips[2])
      return 1;
    return 0;
  }
  return 0;
}

status read_func(string sArg) {
  if(sArg != "sign") {
    notify_fail("Read what?\n");
    return 0;
  }
  write("\
Current classes of ships available:\n\
Ship Class                       Price                Available\n\
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n\
Cog -------------------------- 120,000 coins             " + maiAvailShips[0] + "\n\
Lateen Galley ---------------- 180,000 coins             " + maiAvailShips[1] + "\n\
Carrack ---------------------- 400,000 coins             " + maiAvailShips[2] + "\n\n\
There is a scribbled note below about possible future classes of ships.\n\
To purchase a ship, you must fill out an application. If you are\n\
interested, please <request application> and get started. For deck\n\
plans of the current classes of ships, you may <lookup [class]>.\n");
  return 1;
}

status inquire_func(string sArg) {
  string sShipName;
  string sGarbage;
  object oShip;
  object oEnv;

  if(!sArg) {
    write("What ship did you want to inquire about?\n");
  return 1;
  }
  if(sscanf(sArg,"about %s",sShipName) != 1)
    sShipName = sArg;
  if(sscanf(sShipName,"%s %s",sArg,sGarbage) == 2) {
    write("The name of the ship must be one word only.\n");
    return 1;
  }
  if(!SHIP_D->is_ship_name(sArg)) {
    write("That is not a currently registered ship.\n");
    return 1;
  }
  if(!SHIP_D->is_loaded(sArg))
    SHIP_D->load_ship(SHIP_D->query_owner(sArg));
  oShip = SHIP_D->query_ship_object(sArg);
  if(!oShip) {
    write("That ship is not currently anywhere in Darkwind.\n");
    return 1;
  }
  oEnv = SHIP_D->query_ship_environment(sArg);
  if(!oEnv) {
    write("That ship is appears to be lost.\n");
    return 1;
  }
  switch(object_name(oEnv)) {
  case "domains/darkwind/wharf/port":
  case "domains/darkwind/wharf/hidden_port":
    write("That ship is at the south Darkwind port.\n");
    break;

  case "objects/ships/upgrade_shop/dock":
    write("That ship is at Erison's dock.\n");
    break;

  case "objects/ships/upgrade_shop/holding_room":
    write("That ship is currently being worked on and is unavailable.\n");
    break;

  case "vrrooms/east_sea/dock9":
    write("That ship is at the north Darkwind port.\n");
    break;

  case "domains/hyperborea/firei/jetty1a":
  case "domains/hyperborea/firei/jetty1b":
  case "domains/hyperborea/firei/jetty2a":
  case "domains/hyperborea/firei/jetty2b":
    write("That ship is at the port of Grimsfjord.\n");
    break;

  case "domains/souvrael/beach/souvrael_dock":
    write("That ship is docked in Souvrael.\n");
    break;

  case "domains/kerei/river/dock1":
    write("That ship is in far-off Kerei.\n");
    break;

  case "domains/darkwind/pirate/rooms/docks":
    write("That ship is docked in Pirate Town!\n");
    break;

  case "vrrooms/east_sea/beach":
    write("That ship was last heard from at Dwork Island.\n");
    break;

  case "vrrooms/east_sea/island1":
    write("That ship last registered at Volcano Island.\n");
    break;

  case "players/rampage/area/town/dock":
    write("That ship is at the Sunken Island of Gwent.\n");
    break;

  default:
    write("That ship is currently at sea.\n");
    break;
  }
  write("That ship is owned by " +
    capitalize(SHIP_D->query_owner(sArg)) + ".\n");
  return 1;
}

status lookup_func(string sArg) {
  object oPlans;

  if(!sArg) {
    notify_fail("You must specify the class of ship you want plans for.\n");
    return 0;
  }
  if(sArg != "cog" && sArg != "galley" && sArg != "carrack") {
    notify_fail(capitalize(sArg) + " is not a valid class of ship.\n");
    return 0;
  }
  switch(sArg) {
  case "cog":
    oPlans = clone_object("/objects/ships/obj/cog_plan");
    break;
  case "galley":
    oPlans = clone_object("/objects/ships/obj/galley_plan");
    break;
  case "carrack":
    oPlans = clone_object("/objects/ships/obj/carr_plan");
    break;
  }
  write("\
You rummage around in the files and pull out a set of plans.\n");
  say(this_player()->query_cap_name() + " \
rummages around in the files and takes something.\n");
  move_object(oPlans, this_player());
  return 1;
}

status request_func(string sArg) {
  object oApp;

  if(sArg != "application") {
    notify_fail("What do you want to request? Maybe an application?\n");
    return 0;
  }
  if(SHIP_D->is_banished(this_player()->query_real_name())) {
    write("You are not allowed to apply for a ship.\n");
    return 1;
  }
  if(SHIP_D->is_owner(this_player()->query_real_name())) {
    write("\
You already own a ship, and are ineligible to purchase a new one\n\
until you sell your present ship.\n");
    return 1;
  }
#if PURCHASE_CLOSED
  write("I'm sorry: Ship purchasing is currently closed.\n");
  return 1;
#endif
  oApp = clone_object("/objects/ships/obj/application");
  write("You take an application from the stack.\n");
  say(this_player()->query_cap_name() + " takes an application.\n");
  move_object(oApp,this_player());
  oApp->setup(this_player());
  return 1;
}

void add_port(string sFile) {
  masPorts += ({ sFile });
  return;
}

void set_port_name(string sName) {
  msPortName = sName;
  maiAvailShips = SHIP_D->query_available_ships(sName);
}


object find_in_port(string str) {
  object ob;
  int i, siz;

  siz = sizeof(masPorts);

  for(i = 0; i < siz; i++) {
    if(find_object(masPorts[i]))
      // lexicon, 3-28-98.  temp fix to some bug (bad arg 2 to present) //
      ob = present(str, find_object(masPorts[i]));
    if (ob) break;
  }

  return ob;
}

int rescue_func(string sName) {
  object oShip;

  if (!sName) {
    write("Rescue what?\n");
    return 1;
  }
  if(sizeof(masPorts) < 1) {
    write("This shop shop has no port!\n");
    return 1;
  }
  if(!SHIP_D->is_ship_name(sName)) {
    write("That is not the name of any ship.\n");
    return 1;
  }
  if(!SHIP_D->is_loaded(sName)) {
    write("That ship has not been seen on Darkwind thus far.\n");
    return 1;
  }
  oShip = SHIP_D->query_ship_object(sName);
  if (!oShip) {
    write("The ship " + capitalize(sName) + " appears to have been destroyed.\n");
    SHIP_D->destroy_ship(sName);
    return 1;
  }
  if(oShip->query_docked()) {
    write("\
That ship is already docked somewhere and is not in need of a rescue.\n");
    return 1;
  }
  if(oShip->query_ship_sailing()) {
    write("\
That ship is being sailed right now! It is not in need of a rescue.\n");
    return 1;
  }
  move_object(oShip,masPorts[0]);
  oShip->set_docked(1,masPorts[0]);
  write("That ship was re-docked.\n");
  return 1;
}

int get_cost(string sArg) {
  switch(sArg) {
  case "cog":
    return 120000;
  case "galley":
    return 180000;
  case "carrack":
    return 400000;
  default:
    return 0;
  }
}

status turnin_func(string sArg) {
  object oApp;
  string sPlayer;
  string sShipName;
  string sShipClass;
  int iCost;

  if(sArg != "application") {
    notify_fail("Turnin what?\n");
    return 0;
  }
  if(sizeof(masPorts) < 1) {
    write("The ship shop has no port! It cannot build ships.\n");
    return 1;
  }
  oApp = present("application",this_player());
  if(!oApp) {
    write("You don't have an application to turn in!\n");
    return 1;
  }
  if(!oApp->query_finished()) {
    write("That application is not finished.\n");
    return 1;
  }
  sPlayer = oApp->query_player();
  sShipName = oApp->query_ship_name();
  sShipClass = oApp->query_ship_class();
  write("You turn in your application.\n");
  destruct(oApp);

  iCost = get_cost(sShipClass);
  if(!is_available(sShipClass)) {
    write("\
The application is rejected because no ships of that class are currently\n\
available.\n");
    write("\
New ships will be available " + ctime(SHIP_D->query_next_build_time()) + ".\n");
    return 1;
  }
  if(find_player(sPlayer) != this_player()) {
    write("The application is rejected because it is another's application.\n");
    return 1;
  }
  if(this_player()->query_money() < iCost) {
    write("The application is rejected because you do not have enough money.\n");
    return 1;
  }
  if(SHIP_D->is_owner(this_player()->query_real_name())) {
    write("The application is rejected because you already own a ship.\n");
    return 1;
  }
  write("The application is accepted after you pay the requisite fee.\n");
  this_player()->add_money(-iCost);
  SHIP_D->add_owner(sPlayer, sShipName,sShipClass);
  SHIP_D->setup_ship(sShipName,sPlayer, masPorts[0]);
  SHIP_D->load_ship(sPlayer);
  iCost = (sShipClass == "cog" ? 0 : (sShipClass == "galley" ? 1 : 2));
  SHIP_D->reduce_available(msPortName,iCost);
  write("Your new ship, the " + capitalize(sShipName) + ", is waiting for you in port.\n");
  return 1;
}
