#include <mudlib.h>
#include <ships.h>
#include <perms.h>

inherit NPC_SHIP;

string msFerryName;
mapping mmTripProgram;
int miTripCounter;
int miCurrentTrip;
string msCurrentDest;

void reset(int arg) {
    if(!is_clone(this_object())) return;
    ::reset(arg);
    if (arg) return;

    //Setup defaults in case vars don't restore
    set_name("a ferry");
    reset_short("mysterious ferry");
    set_main_long("\
This is a long a sleek cruiser. Two masts rise into the sky, the\n\
mizzenmast in the front and the mainmast in the center. Both masts\n\
are covered with square sails. The ship has a stearncastle with the\n\
bridge on top. Including the bowsprit, the ship is about 70 feet\n\
in length.\n");
    set_ship_name("ferry");
    set_enter_messages(({"You board the ferry","boards the ferry",
      "comes aboard the ferry"}));
    set_deboard_messages(({"You disembark the ferry","leaves the ferry",
      "disembarks from the ferry"}));
    set_docked(1,1);
    call_out("execute_program",6);
}

void set_ferry_name(string sArg) {
  msFerryName = sArg;
  set_enter_messages(({"You board the " + msFerryName,"boards the " + msFerryName,
      "comes aboard the " + msFerryName}));
  set_deboard_messages(({"You disembark the " + msFerryName,"leaves the " + msFerryName,
      "disembarks from the " + msFerryName}));
  set_name(msFerryName);
  set_ship_name(msFerryName);
}

void set_trip_program(mapping mProgram) { mmTripProgram = mProgram; }

//Overloads of virtual function in parent
string query_ship_class() { return "ferry";}
string query_ship_arrive() { return "sails in";}
string query_ship_depart() { return "sails swiftly";}

string get_destination() {
  if(msCurrentDest == "darkwind") return "the land of Darkwind";
  if(msCurrentDest == "souvrael") return "Souvrael, city of sand";
  if(msCurrentDest == "odako") return "the land of Odako in Kerei";
  if(msCurrentDest == "chimotage") return "the province of Chimotage in Kerei";
  if(msCurrentDest == "hyperborea") return "Grimsfjord, port of Hyperborea";
  return "parts unknown";
}

void execute_announce(string sArg) {
  string sDomain;
  string sWhat;
  string sMsg;

  if(sscanf(sArg,"%s %s",sWhat, sDomain) != 2) return;
  //Check for "local, to-ship only" msgs
  if(sWhat == "arrive") sMsg = "The " + msFerryName + " has arrived at " +
    capitalize(sDomain) + ". Its next destination is " + get_destination() + ".\n";
  else if(sWhat == "warning") sMsg = "The " + msFerryName +
    " will depart in 2 minutes, bound for " + get_destination() + ".\n";
  else if(sWhat == "leave") sMsg = "The " + msFerryName + " is now departing for "
    + get_destination() + ".\n";
  else sMsg = sArg;
  dshout(sDomain,wrap(sMsg),1);
}

void execute_program() {
  string * asCmds;
  string sCmd;
  int iAmt;
  string sRest;

  asCmds = mmTripProgram[miCurrentTrip];
  if(!asCmds) return;
  sCmd = asCmds[miTripCounter];
  if(!sCmd) return;

  miTripCounter++;
  if(miTripCounter >= sizeof(asCmds)) {
    miTripCounter = 0;
    miCurrentTrip ++;
    if(!member(mmTripProgram,miCurrentTrip))
      miCurrentTrip = 0;
  }
  if(sscanf(sCmd,"hold %d",iAmt)) {
    call_out("execute_program",iAmt * 60);
    return;
  }
  if(sscanf(sCmd,"announce %s",sRest)) {
    execute_announce(sRest);
    call_out("execute_program",0);
    return;
  }
  if(sscanf(sCmd,"nextdest %s",sRest)) {
    msCurrentDest = sRest;
    call_out("execute_program",0);
    return;
  }
  if(sCmd == "dock") set_docked(1);
  else if(sCmd == "push") set_docked(0);
  else if(sCmd == "hoist") set_sails(1);
  else if(sCmd == "furl") set_sails(0);

  else move_vehicle(sCmd,1);
  call_out("execute_program",2);
}

status enter_func(string str) {
  object o;

  if(!id(str)) return(0);
  if(IS_APPLICANT(this_player())) return ::enter_func(str);
  if(this_player()->query_sitting()) return ::enter_func(str);
  if(!(o = present("ferry_ticket",this_player()))) {
    notify_fail("The sailors won't let you on with no ticket.\n");
    return 0;
  }
  if(::enter_func(str)) {
    write("\
A sailor takes your ticket and warns you not to get off until you\n\
have reached your destination.\n");
    destruct(o);
    return 1;
  }
  return 0;
}

