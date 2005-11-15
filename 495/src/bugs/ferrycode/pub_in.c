
#include <mudlib.h>
#include <ships.h>
#include <daemons.h>
inherit ROOM;

// Variables
string  local_port;
string *asPorts;
object  moShip;

// Predefined Functions
int find_in_port(string sShipName);

// Code 
void reset(int arg) {
  ::reset(arg);
  if (arg) return;
  asPorts = ({ });
}

void init() {
  ::init();
  add_action("read_func","read");
}

void set_local_port(string str) {
  local_port = str;
  return;
}

void add_port(string str) {
  asPorts += ({ str });
  return;
}

int purchase_func(string sArg) {
  int    iAmount;
  string sWhat, sShipName;

  if (!sArg) {
    write("Purchase what?\n");
    write("|YKOThe format for making a purchase is:\n\
  Purchase X WHAT for SHIPNAME.\n\
  You can purchase " + english_list(m_indices(PROVISIONS)) + " here.\n");
    return 1;
  }

  if (sscanf(sArg, "%d %s for %s", iAmount, sWhat, sShipName) != 3) {
    write("|YKOThe format for making a purchase is:\n\
  Purchase X WHAT for SHIPNAME.\n\
  You can purchase " + english_list(m_indices(PROVISIONS)) + " here.\n");
    return 1;
  }

  if (sWhat == "crew") {
    write("|YKOWouldn't it be nicer to recruit crew, instead of purchase them?\n");
    return 1;
  }

  if (member_array(sWhat, m_indices(PROVISIONS)) == -1) {
    write("|YKOYou cannot purchase any " + sWhat + " here.\n");
    return 1;
  }

  if (this_player()->query_money() < (iAmount * PROVISIONS[sWhat, PROV_COST])) {
    write("|YKOYou don't have enough money to purchase any " + sWhat + " here.\n");
    return 1;
  }

  // Can this person purchase for this ship?
  if (find_in_port(lower_case(sShipName)) == 0) {
    write("|YK0" + 
          sprintf("You are not authorized to purchase supplies for the ship %s.\n",
                  capitalize(sShipName)));
    return 1;
  }

  if ((call_other(moShip, "query_" + sWhat) + iAmount) <=
      (call_other(moShip, "query_max_" + sWhat))) {
    call_other(moShip, PROVISIONS[sWhat, PROV_FUNC], iAmount);
    this_player()->add_money((iAmount * -PROVISIONS[sWhat, PROV_COST]));
    write("|YKO" + 
          wrap(sprintf("You have purchased %d units of %s for your ship(%s).\n",
                       iAmount, sWhat, capitalize(sShipName))));
  } else {
    write("|YKO" + 
          wrap(sprintf("\
You don't have enough room to purchase %d units of %s for your ship (%s).\n\n",
                       iAmount, sWhat, capitalize(sShipName))));
    write("|YKO" + sprintf("%s has room for %d units of %s.\n",
                           capitalize(sShipName), 
                           (call_other(moShip, "query_max_" + sWhat) - 
                            call_other(moShip, "query_" + sWhat)),
                           sWhat));
  }
  return 1;
}

int recruit_func(string sArg) {
  int    iAmount, iCrewCost;
  int    iCrewBattle, iCrewSail;
  string sShipName;

  if (!sArg) {
    write("|YKORecruit what?\n");
    write("|YKOThe format for recruiting crew is:  Recruit X for SHIPNAME.\n");
    return 1;
  }

  if (sscanf(sArg, "%d for %s", iAmount, sShipName) != 2) {
    write("|YKOThe format for recruiting crew is:  Recruit X for SHIPNAME.\n");
    return 1;
  }

  // Can this person recruit for this ship?
  if (find_in_port(lower_case(sShipName)) == 0) {
    write("|YK0" + 
          sprintf("You are not authorized to purchase supplies for the ship %s.\n",
                  capitalize(sShipName)));
    return 1;
  }

  if (iAmount <= 0) {
    write("|RKOYou must recruit someone!\n");
    return 1;
  }

  if ((moShip->query_crew() + iAmount) <= moShip->query_max_crew()) {
    iCrewBattle = moShip->query_crew_battle();
    iCrewSail   = moShip->query_crew_sailing();
    iCrewCost   = 0;
    if(iCrewBattle == 20)  iCrewCost = iAmount * 150;
    if(iCrewBattle == 40)  iCrewCost = iAmount * 450;
    if(iCrewBattle == 60)  iCrewCost = iAmount * 900;
    if(iCrewBattle == 80)  iCrewCost = iAmount * 1500;
    if(iCrewBattle == 100) iCrewCost = iAmount * 2200;
    if(iCrewSail == 20)  iCrewCost += iAmount * 125;
    if(iCrewSail == 40)  iCrewCost += iAmount * 375;
    if(iCrewSail == 60)  iCrewCost += iAmount * 725;
    if(iCrewSail == 80)  iCrewCost += iAmount * 1200;
    if(iCrewSail == 100) iCrewCost += iAmount * 1800;

    iCrewCost += iAmount*1000;

    if(this_player()->query_money() < iCrewCost) {
      write("|GKO"+ wrap(sprintf("\
%d sailors stand up, they ask for %d gold as pay.  Noticing that you are not able to pay, \
they go about their business.\n", iAmount, iCrewCost)));
      return 1;
    }

    moShip->restore_crew(iAmount);
    this_player()->add_money(-iCrewCost);
    moShip->set_wc();
    moShip->save_ship();
    write("|YKO" + wrap(sprintf("You have recruited %d sailors for your ship (%s).\n",
                                iAmount, capitalize(sShipName))));
  } else {
    write("|YKO" + 
          wrap(sprintf("You don't have room to recruit %d sailors for your ship (%s).\n\n",
                       iAmount, capitalize(sShipName))));
    write("|YKO" + wrap(sprintf("%s has room for %d additional sailors.\n", 
                                capitalize(sShipName), iAmount)));
  }

  return 1;
}

int read_func(string str) {
  int i, siz;
  string *stuff;
  if (!str) return (0);
  if (str != "list") return (0);

  stuff = SHIP_D->query_pub_items();
  siz = sizeof(stuff);

  write("|YKOHere in the pub you can <PURCHASE> things for you ships,\n\
provided they are in the port.  You can buy:\n");
  for (i = 0; i < siz; i++)
    write("|YKO"+stuff[i]);

  write("|GKO\n\You may also <RECRUIT> sailors for your ships,\n\
provided they are in the port.\n");
  write("|GKOEach sailor will cost between 1,000 and 5,000 to hire\n\
depending on the training of your current crew.\n");
  return 1;
}

int manifest_func(string sShipName) {
  if (!sShipName) {
    write("List the manifest of what ship?\n");
    return 1;
  }

  // Can this person list the manifest of this ship?
  if (find_in_port(lower_case(sShipName)) == 0) {
    write("|YK0" + 
          sprintf("You are not authorized to see this ship's (%s) manifest.\n",
                  capitalize(sShipName)));
    return 1;
  }

  write(capitalize(sShipName) + "'s manifest:\n");
  printf("Crew:  %d/%d\n", moShip->query_crew(), moShip->query_max_crew());
  printf("Provisisons:  %d/%d\n", moShip->query_provisions(), 
                                  moShip->query_max_provisions());
  printf("Wood:  %d/%d\n", moShip->query_wood(), moShip->query_max_wood());
  printf("Rope:  %d/%d\n", moShip->query_rope(), moShip->query_max_rope());
  printf("Rocks:  %d/%d\n", moShip->query_rocks(), moShip->query_max_rocks());
  printf("Bolts:  %d/%d\n", moShip->query_bolts(), moShip->query_max_bolts());

  return 1;
}   

// Returns 1  if ship is there, and this player may "modify" it.
// Returns 0  if its there, but the player can't modify it.
// Returns -1 if its not there at all.

int find_in_port(string sShipName) {
  int iIndex;

  moShip = 0;
  iIndex = sizeof(asPorts);

  while(iIndex--) {
    if(find_object(asPorts[iIndex]))
      moShip = present(sShipName, find_object(asPorts[iIndex]));
    if (moShip) break;
  }

  if (!moShip) {
    write("|YKOThe ship (" + capitalize(sShipName) + ") isn't in the port!\n");
    return -1;
  } else if (!moShip->query_captains(this_player()->query_real_name()))
    return 0;
  else return 1;
}


int stock_func(string sArg) {
  int iCost, iAmount;
  string sShipName, sWhat;

  if (!sArg) {
    write("|YKOThe format for stocking a ship is:\n\
  Stock WHAT for SHIPNAME.\n\
  You can stock " + english_list(m_indices(PROVISIONS)) + " here.\n");
    return 1;
  }

  if (sscanf(sArg, "%s for %s", sWhat, sShipName) != 2) {
    write("|YKOThe format for stocking a ship is:\n\
   Stock WHAT for SHIPNAME.\n\
    You can stock " + english_list(m_indices(PROVISIONS)) + " here.\n");
    return 1;
  }

  if (find_in_port(lower_case(sShipName)) == 0) {
    write("|YK0" + 
          sprintf("You are not authorized to stock this ship's (%s).\n",
                  capitalize(sShipName)));
    return 1;
  }

  iAmount = call_other(moShip, "query_max_" + sWhat) -
            call_other(moShip, "query_" + sWhat);

  if (!iAmount) {
    write("|YKOYour ship doesn't need any " + sWhat + ".\n");
    return 1;
  }

  if (this_player()->query_money() < (PROVISIONS[sWhat, PROV_COST] * iAmount)) {
    write("|YKOYou don't have enough money to purchase any " + sWhat + " here.\n");
    return 1;
  }

  call_other(moShip, PROVISIONS[sWhat, PROV_FUNC], iAmount);
  this_player()->add_money(-(PROVISIONS[sWhat, PROV_COST] * iAmount));
  write("|YKO" + 
        wrap(sprintf("You have purchased %d units of %s for your ship (%s).\n", 
                     iAmount, sWhat, capitalize(sShipName))));

  return 1;
}
