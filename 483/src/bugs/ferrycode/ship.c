#include <mudlib.h>
#include <ships.h>
#include <perms.h>
#include <daemons.h>
#include <terrain.h>
#include <darkwind.h>

inherit NEW_VEHICLE;

#define DW_PORT        WHARF + "port"
#define DW_HIDDEN_PORT WHARF + "hidden_port"
#define ENV(xx) environment(xx)
#define TO this_object()

//Saved variables
string last_port;                  // The filename of the last port visited
string flag_color;                 // The current color being flown
string captain_long;               // Long Desc of Captain's Cabin.

status mtDocked;                   // Is this ship currently docked?
status mtStranded;                 // Was the ship stranded at sea?
string msShipName;                 // Name of the ship
string msOwnerName;                // Name of the ship's owner
string msFleetName;                // Name of the ship's fleet
string * masPassengers;            // Who can board the ship
string * masCaptains;              // Who can captain the ship
string msLastPort;                 // Last port docked at
//string msFlagColor;                // Current color of flag
string msCaptainLong;              // Long description of captain's cabin

mapping mmShipAutoLoads; // Mapping takes the form (["key":"path";"roomname";type;arg,])

//Static Variables
static object moBridge;              // The Bridge
static string msSaveFile;            // Ship's save file
static string msFlagColor;           // The current color being flown
static string msLong;                // Long desc without flag
static status mtSails;               // Are the sails up?
static mapping mmExtraLooks;        // Extra looks in the ship object
static mapping mmRoomLooks;         // Visisble Rooms from outside
static object * maoContainers;      // Containers on board the ship
//static mapping mmWeapons;           // Weapons on board the ship

//Holdover vars
string type;
string name;

//Functions
status is_player(string sPlayer);
status is_passenger(string sPlayer);
status is_captain(string sPlayer);
status is_owner(string sPlayer);


void save_ship() {
  save_object(msSaveFile);
}

void setup(string sName, string sOwnerName, string sFile,
           string sLastPort) {
  msShipName = sName;
  msOwnerName = sOwnerName;
  msSaveFile = sFile;
  msLastPort = sLastPort;
  mtDocked = 1;
  save_ship();
}

void reset(int arg) {
    if(!is_clone(this_object())) return;
    ::reset(arg);
    if (arg) return;
    set_necessary_terrains(TER_RIVER | TER_SEA);
    set_alt_name("ship");
    msFlagColor = "green";
    masPassengers = ({ });
    masCaptains = ({ });
    mmExtraLooks = ([]);
    mmRoomLooks = ([]);
    mmShipAutoLoads = ([ ]);
    maoContainers = ({ });

    //Defaults in case nothing else is set
    set_name("malfuctioning ship");
    set_short("a malfunctioning ship");
    set_long("This ship isn't working right - stay away from it.\n");
    set_arrival_msg("The ship arrives.");
    set_depart_msg("The ship sails");
}

void init() {
  ::init();
  add_action("enter_func","board");
}

string short() {
  return (query_short() + (mtDocked == 1 ? " (Docked)" : ""));
}

//Virtual Functions - Overloaded in "children"
string query_ship_class() { return ""; }
string query_ship_arrive() { return "arrives.";}
string query_ship_depart() { return "sails";}
//End virtual functions

void restore_autoloads() {
  int i;
  string * asKeys;
  object oObj;
  object oRoom;

  if(!mmShipAutoLoads) return;
  asKeys = m_indices(mmShipAutoLoads);
  if(!asKeys) return;
  if(sizeof(asKeys) < 1) return;

  for(i=0; i<sizeof(asKeys); i++) {
    oObj = clone_object(mmShipAutoLoads[asKeys[i],0]);
    oRoom = find_room(mmShipAutoLoads[asKeys[i],1]);
    if(!oRoom) continue;
    move_object(oObj,oRoom);
    oRoom->fill_slot(mmShipAutoLoads[asKeys[i],2]);
    oObj->init_arg(mmShipAutoLoads[asKeys[i],3]);
    if(oObj->id("ship_container_object"))
      maoContainers += ({oObj});

  }
}

void manual_load_autoload(string sKey) {
  object oObj;
  object oRoom;

  if(!mmShipAutoLoads) return;
  if(!member(mmShipAutoLoads,sKey)) return;
  oObj = clone_object(mmShipAutoLoads[sKey,0]);
  oRoom = find_room(mmShipAutoLoads[sKey,1]);
  if(!oRoom) return;
  move_object(oObj,oRoom);
  oRoom->fill_slot(mmShipAutoLoads[sKey,2]);
  oObj->init_arg(mmShipAutoLoads[sKey,3]);
  if(oObj->id("ship_container_object"))
    maoContainers += ({oObj});
}

void change_ship_name(string sNewName, string sNewFile) {
  rm(msSaveFile + ".o");
  msShipName = sNewName;
  msSaveFile = sNewFile;
  save_ship();
}


void clear_fleet_name() {
  msFleetName = "none";
  save_ship();
}

void init_ship(string sFile) {
    msSaveFile = sFile;
    if (!restore_object(msSaveFile))
      return;

    mtDocked = 1;
    if (mtStranded)
      mtDocked = 0;

    //Setup names even if file did not reload
    if(!msShipName)
      msShipName = "Unknown";
    if(!msOwnerName || msOwnerName == "unknown") {
      msOwnerName = SHIP_D->query_owner(msShipName);
      if(!msOwnerName)
        msOwnerName = "unknown";
    }
    if(!msFleetName || msFleetName == "unknown") {
      msFleetName = SHIP_D->query_fleet_name(msOwnerName);
      if(!msFleetName)
        msFleetName = "none";
    }
    //Move it back to the port
    if (!msLastPort)
      msLastPort = WHARF + "port";
    if(catch(move_object(this_object(), msLastPort))) {
      save_ship();
      return;
    }
    save_ship();
    //Setup descriptions and messages
    set_name(msShipName);
    if(msFleetName == "none")
      reset_short(capitalize(msShipName) + ", an independent " + query_ship_class());
    else
      reset_short(capitalize(msShipName) + ", a " + query_ship_class() + " of the " +
        capitalize(msFleetName) + " fleet");
    set_long(msLong +"The ship is flying a "+ msFlagColor +
      " flag.\n");
    set_arrival_msg("The " + capitalize(msShipName) + " " + query_ship_arrive());
    set_depart_msg("The " + capitalize(msShipName) + " " + query_ship_depart());
    set_enter_messages(({"You climb onboard the " + capitalize(msShipName),
        "boards the " + capitalize(msShipName),
        "comes aboard the " + capitalize(msShipName)}));
    set_deboard_messages(({"You disembark the " + capitalize(msShipName),
        "leaves the " + capitalize(msShipName),
        "disembarks from the " + capitalize(msShipName)}));
    restore_autoloads();
    //Announce the ship's arrival
    tell_outside("The dockworkers bring the " + capitalize(msShipName) + " \
into port.\n");
}

void reset_ship() {
  if (!msLastPort) msLastPort = WHARF + "port";
  move_object(this_object(), msLastPort);
  mtDocked = 1;
  save_ship();
}

void set_main_long(string sArg) {
  ::set_long(sArg);
  msLong = sArg;
}

status query_docked() { return mtDocked; }

void set_docked(status tDocked) {
  if(!tDocked) {
    mtDocked = 0;

    //More hidden port stuff - remove when HPORT is fixed
    if (environment(this_object()) == find_object(DW_HIDDEN_PORT)) {
      move_object(this_object(), DW_PORT);
    }
    tell_outside("The " + capitalize(msShipName) + " has pushed off.\n");
    tell_vehicle("The ship has pushed off.\n");
    save_ship();
    return;
  }

  mtDocked    = 1;
  msLastPort = "/" + object_name(ENV(TO));
  tell_outside("The " + capitalize(msShipName) + " has docked.\n");
  tell_vehicle("The ship has docked.\n");

  //More HPORT garbage
  if (environment(this_object()) == find_object(DW_PORT)) {
      msLastPort = DW_HIDDEN_PORT;
      move_object(this_object(), DW_HIDDEN_PORT);
    }
  save_ship();
  return;
}

status set_last_port(string sFile) {
  object oFile;

  oFile = find_object(sFile);
  if(!oFile) {
    sFile->do_absolutely_nothing();
    oFile = find_object(sFile);
    if(!oFile) return 1;
  }
  msLastPort = sFile;
  save_ship();
  return 0;
}

status query_sails() { return mtSails; }

void set_sails(status tSails) {
  if(!tSails) {
    mtSails = 0;
    tell_outside("The " + capitalize(msShipName) + " has lowered its sails.\n");
    tell_vehicle("The sails have been lowered.\n");
    return;
  }

  mtSails = 1;
  tell_outside("The " + capitalize(msShipName) + " has raised its sails.\n");
  tell_vehicle("The sails have been raised.\n");
  return;
}

void vehicle_move_success(string sDirection, int tSilently) {
  if(!tSilently) {
    write("The ship sails " + sDirection + " at your command.\n");
    say("The ship sails " + sDirection + " at " +
      this_player()->query_cap_name() + "'s command.\n");
  }
  tell_vehicle("The ship rocks as it sails across the ocean.\n");
  ::vehicle_move_success(sDirection,tSilently);
}

status enter_func(string str) {
  if(!id(str)) return(0);
  if (!mtDocked) {
    write("\n" + capitalize(msShipName) + " \
is currently not docked.  You may not board at this time.\n");
    return 1;
  }
  if(!is_passenger(this_player()->query_real_name())) {
    write("You are not a registered passenger of this ship.\n");
    return 1;
  }
  return (::enter_func(str));
}

status is_player(string sPlayer) {
  string sTemp1;
  string sTemp2;

  if(sscanf(sPlayer,"%s %s",sTemp1,sTemp2) == 2) {
    write("Player names can only be one word.\n");
    return 0;
  }
  if(find_object(PLAYER_INFO_D))
    destruct(find_object(PLAYER_INFO_D));
  switch (PLAYER_INFO_D->restore_player(sPlayer)) {
    case -1:
      write("Player does not exist.\n");
      return 0;

    case 0:
      write("Error retrieving player information information on " +
            capitalize(sPlayer) + ".\n");
      write("Please tell an Implementor about this.\n");
      return 0;

    default:
      return 1;
  }
}

status add_passenger(string sPlayer) {
  if(member(masPassengers,sPlayer) != -1) {
    write("That person is already on your boarding list.\n");
    return 1;
  }
  if(is_captain(sPlayer)) {
    write("That person is a captain and already has boarding privilges.\n");
    return 1;
  }
  if(!is_player(sPlayer)) {
    write("That is not the name of any player.\n");
    return 1;
  }
  masPassengers += ({sPlayer});
  write("You have added " + capitalize(sPlayer) + " to your passenger list.\n");
  if(find_player(sPlayer))
    tell_object(find_player(sPlayer),"\
You have been added to the passenger list of the ship " +
      capitalize(msShipName) + ".\n");
  save_ship();
  return 1;
}

status remove_passenger(string sPlayer) {
  if(sPlayer == msOwnerName) {
    write("You are already the owner of the ship!\n");
    return 1;
  }
  if(member(masPassengers,sPlayer) == -1) {
    write("That person is not on your passenger list.\n");
    return 1;
  }
  write("You have removed " + capitalize(sPlayer) + " \
from your passenger list.\n");
  if(find_player(sPlayer))
    tell_object(find_player(sPlayer),"\
You have been removed as a passenger of the ship " +
      capitalize(msShipName) + ".\n");
  masPassengers -= ({sPlayer});
  save_ship();
  return 1;
}

void print_passengers() {
  int iCounter;

  for(iCounter = 0; iCounter < sizeof(masPassengers); iCounter++)
    write(capitalize(masPassengers[iCounter]) + "\n");
}

status is_passenger(string sPlayer) {
  if(find_player(sPlayer))
    if(IS_APPLICANT(find_player(sPlayer)))
    return 1;
  if(member(masPassengers,sPlayer) != -1)
    return 1;
  if(is_captain(sPlayer))
    return 1;
  return 0;
}

string * query_passengers() { return masPassengers; }

status add_captain(string sPlayer) {
  if(sPlayer == msOwnerName) {
    write("You are already the owner of the ship.\n");
    return 1;
  }
  if(member(masCaptains,sPlayer) != -1) {
    write("That person can already captain this ship!\n");
    return 1;
  }
  if(member(masPassengers,sPlayer) == -1) {
    write("\
That person is not a passenger! Only passengers can be promoted\n\
to captain.\n");
    return 1;
  }
  write(capitalize(sPlayer) + " has been promoted to captain.\n");
  if(find_player(sPlayer))
    tell_object(find_player(sPlayer),"\
You have been promoted to captain of the ship " +
      capitalize(msShipName) + ".\n");

  masCaptains += ({sPlayer});
  masPassengers -= ({sPlayer});
  save_ship();
  return 1;
}

status remove_captain(string sPlayer) {
  if(member(masCaptains,sPlayer) == -1) {
    write("That person is not a captain of this ship!\n");
    return 1;
  }
  write("You have demoted " + capitalize(sPlayer) + " \
to a passenger.\n");
  if(find_player(sPlayer))
    tell_object(find_player(sPlayer),"\
You have been demoted to passenger of the ship " +
      capitalize(msShipName) + ".\n");
  masCaptains -= ({sPlayer});
  masPassengers += ({sPlayer});
  save_ship();
  return 1;
}

void print_captains() {
  int iCounter;

  for(iCounter = 0; iCounter < sizeof(masCaptains); iCounter++)
    write(capitalize(masCaptains[iCounter]) + "\n");
}

status is_captain(string sPlayer) {
  if(find_player(sPlayer))
    if(IS_ARCHITECT(find_player(sPlayer)))
      return 1;
  if(member(masCaptains,sPlayer) != -1)
    return 1;
  if(sPlayer == msOwnerName)
    return 1;
  return 0;
}

string * query_captains() { return masCaptains; }

status is_owner(string sPlayer) {
  if(find_player(sPlayer))
    if(IS_ARCHITECT(find_player(sPlayer)))
      return 1;
  if(sPlayer == msOwnerName)
    return 1;
  return 0;
}

void set_bridge(object ob) {
  moBridge = ob;
  return;
}

object query_bridge() { return moBridge; }
status query_stranded() { return mtStranded; }

void set_stranded(status tStranded) {
  mtStranded == tStranded;
  save_ship();
  return;
}

void set_capt_long(string sDesc) {
  msCaptainLong = sDesc;
  save_ship();
  return;
}

string query_capt_long() { return msCaptainLong; }

object is_onboard(string sName) {
  object oTarget;

  oTarget = find_player(sName);
  if(!oTarget)
    oTarget = find_living(sName);
  if(!oTarget)
    oTarget = find_object(sName);
  if(!oTarget)
    return 0;
  if(member(all_environment(oTarget),this_object()) == -1)
    return 0;
  return oTarget;
}

object * captains_onboard() {
  object * asCaptains;
  int iCounter;
  object oTemp;

  asCaptains = ({});
  for(iCounter = 0; iCounter < sizeof(masCaptains); iCounter++) {
    oTemp = is_onboard(masCaptains[iCounter]);
    if(oTemp)
      asCaptains += ({oTemp});
  }
  return asCaptains;
}


//Needs to be revised
/*
void strike_color(string str) {
  if (member_array(str, COLORS) != -1) flag_color = str;
  set_long(SHIP_TYPES[type, LONG]+"The ship is flying a "+ flag_color +
    " flag.\n");
  save_ship();
  return;
}

string query_flag_color() { return(flag_color); }
*/

//Remove this when old vars are purged
string query_type() { return (type); }


//Added - Kanan  12/14/99

//Clean dest - moves all players to safe room and destroys all room
//objects and the actual ship
varargs void destroy_ship(int iSilently) {
  object * aoAllRooms;
  object * aoInventory;
  int iRoomCounter;
  int iInvCounter;

  if(!iSilently)
  tell_outside("\
The " + capitalize(msShipName) + " is battered to pieces by a tidal\n\
wave that seemed to come from nowhere.\n");
  save_ship();
  aoAllRooms = query_room_objects();
  for(iRoomCounter = 0; iRoomCounter < sizeof(aoAllRooms); iRoomCounter++) {
    //Move players out of ship
    if(aoAllRooms[iRoomCounter])
      aoInventory = all_inventory(aoAllRooms[iRoomCounter]);
    else
      aoInventory = ({ });
    for(iInvCounter = 0; iInvCounter < sizeof(aoInventory); iInvCounter++) {
      if(interactive(aoInventory[iInvCounter])) {
        tell_object(aoInventory[iInvCounter],"The ship is breaking up!\n\
You are swept out into the ocean by a great wave!\n\
You wash ashore on a beach.\n");
        move_object(aoInventory[iInvCounter], "/domains/darkwind/sea/beach");
      }
    }
    //Destruct room object
    if(aoAllRooms[iRoomCounter])
      destruct(aoAllRooms[iRoomCounter]);
  }
  destruct(this_object());
}

//This will indicate whether the ship is being actively sailed or not
//Right now it only indicates whether someone is on the bridge
status query_ship_sailing() {
  object * aoInventory;
  int iCounter;

  if(!query_bridge()) return 0;
  aoInventory = all_inventory(query_bridge());
  for(iCounter = 0; iCounter < sizeof(aoInventory); iCounter++ ) {
    if(interactive(aoInventory[iCounter]))
      return 1;
  }
  return 0;
}

string query_ship_name() {
  return msName;
}

string query_fleet_name() {
  return (msFleetName ? msFleetName : "unknown");
}

string query_owner_name() {
  return msOwnerName;
}

void add_room_look(string sLookName, string sRoomName) {
  if(member(mmRoomLooks,sLookName)) {
    mmRoomLooks[sLookName] = sRoomName;
    return;
  }
  mmRoomLooks += ([sLookName:sRoomName]);
}

void add_extra_look(string sLookName, string sLookText) {
  if(member(mmExtraLooks,sLookName)) {
    mmExtraLooks[sLookName] = sLookText;
    return;
  }
  mmExtraLooks += ([sLookName: sLookText]);
}

//Code to allow extra looks in the room outside the ship
status id(string sArg) {
  string sRoomName;
  string sShipName;

  if(!sArg)
    return 0;
  //Check for proper format
  if(sscanf(sArg,"%s of %s",sRoomName,sShipName) != 2)
    return ::id(sArg);
  sRoomName = lower_case(sRoomName);
  sShipName = lower_case(sShipName);

  //Check for the proper ship
  if(sShipName != msShipName) return 0;

  //Check for "extra look"
  if(member(mmExtraLooks,sRoomName)) return 1;

  //Check for visible room name
  if(member(mmRoomLooks,sRoomName)) return 1;
  return 0;
}

//Can't stricttype this because it inherits a void long() and it won't let me
//  override it unless I do this.
long(sArg) {
  string sShipName;
  string sLookName;

  //Check for proper format
  if(sscanf(sArg,"%s of %s",sLookName,sShipName) != 2) {
    ::long();
    return;
  }
  sLookName = lower_case(sLookName);
  sShipName = lower_case(sShipName);

  //Check for the proper ship
  if(sShipName != msShipName) {
    ::long();
    return;
  }

  if(member(mmRoomLooks,sLookName)) {
    if(!find_room(mmRoomLooks[sLookName])) {
      write("That place seems to be missing!\n");
      return;
    }
    find_room(mmRoomLooks[sLookName])->long(0);
    show_object_list(visible_inventory(find_room(mmRoomLooks[sLookName]),
      this_player()),this_player());
    return;
  }
  if(!member(mmExtraLooks,sLookName))
    return ::long();

  write(mmExtraLooks[sLookName]);
}

//Ship Autoload code
status add_ship_autoload(string sKey, string sPath, string sRoom, int ibType, mixed xArg) {
  object oTest;

  if(member(mmShipAutoLoads,sKey)) return 0;
  if(!mmShipAutoLoads) mmShipAutoLoads = ([ ]);
  if(catch(oTest = clone_object(sPath))) return 0;
    destruct(oTest);
  if(!find_room(sRoom)) return 0;
  mmShipAutoLoads += ([sKey:sPath;sRoom;ibType;xArg]);
  save_ship();
  return 1;
}

void remove_ship_autoload(string sKey) {
  if(!member(mmShipAutoLoads,sKey)) return;
  mmShipAutoLoads = m_delete(mmShipAutoLoads,sKey);
  save_ship();
}

string * query_autoload_keys() { return m_indices(mmShipAutoLoads); }
mapping query_autoload_map() { return mmShipAutoLoads; }
//End of Ship Autoload code

object * query_ship_containers() { return maoContainers; }

int query_no_destruct() { return 1; }
