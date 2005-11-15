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

status mtDocked;                   // Is this ship currently docked?
status mtStranded;                 // Was the ship stranded at sea?
string msShipName;                 // Name of the ship
string msFlagColor;                // Current color of flag

mapping mmShipAutoLoads; // Mapping takes the form (["key":"path";"roomname";type;arg,])

//Static Variables
static object moBridge;              // The Bridge
//static string msFlagColor;           // The current color being flown
static string msLong;                // Long desc without flag
static status mtSails;               // Are the sails up?
static mapping mmExtraLooks;        // Extra looks in the ship object
static mapping mmRoomLooks;         // Visisble Rooms from outside
static object * maoContainers;      // Containers on board the ship
//static mapping mmWeapons;           // Weapons on board the ship


//Functions
status is_player(string sPlayer);
status is_passenger(string sPlayer);
status is_captain(string sPlayer);
status is_owner(string sPlayer);


void reset(int arg) {
    if(!is_clone(this_object())) return;
    ::reset(arg);
    if (arg) return;
    set_necessary_terrains(TER_RIVER | TER_SEA);
    set_alt_name("ship");
    msShipName = "DefaultShip";
    msFlagColor = "green";
    mmExtraLooks = ([]);
    mmRoomLooks = ([]);
    mmShipAutoLoads = ([ ]);
    maoContainers = ({ });

    //Defaults in case nothing else is set
    set_name(msShipName);
    set_short(msShipName);
    set_long("This is a boring ship.\n");
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

/*
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
*/


void set_ship_name(string sArg) {
  msShipName = sArg;
}

void setup() {
  reset_short("The " + query_ship_class() + " " + capitalize(msShipName));
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
  tell_outside("The " + capitalize(msShipName) + " appears.\n");
}

void set_main_long(string sArg) {
  ::set_long(sArg);
  msLong = sArg;
}

status query_docked() { return mtDocked; }

varargs void set_docked(status tDocked, status tSilently) {
  if(!tDocked) {
    mtDocked = 0;

    //More hidden port stuff - remove when HPORT is fixed
    if (environment(this_object()) == find_object(DW_HIDDEN_PORT)) {
      move_object(this_object(), DW_PORT);
    }
    if(!tSilently) {
      tell_outside("The " + capitalize(msShipName) + " has pushed off.\n");
      tell_vehicle("The ship has pushed off.\n");
    }
    return;
  }

  mtDocked    = 1;
  if(!tSilently) {
    tell_outside("The " + capitalize(msShipName) + " has docked.\n");
    tell_vehicle("The ship has docked.\n");
  }

  //More HPORT garbage
  if (environment(this_object()) == find_object(DW_PORT)) {
      move_object(this_object(), DW_HIDDEN_PORT);
    }
  return;
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
    tell_vehicle("The ship rocks as it sails across the ocean.\n");
  }
  ::vehicle_move_success(sDirection,tSilently);
}

status enter_func(string str) {
  if(!id(str)) return(0);
  if (!mtDocked) {
    if(!this_object()->allow_enter_undock(this_player())) {
      write("\n" + capitalize(msShipName) + " \
is currently not docked.  You may not board at this time.\n");
      return 1;
    }
  }
  return (::enter_func(str));
}

status is_captain(string sPlayer) {
  if(find_player(sPlayer))
    if(IS_ARCHITECT(find_player(sPlayer)))
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
  return;
}

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
        move_object(aoInventory[iInvCounter], "/vrrooms/oceans/salmo/beach");
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
  return msShipName;
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
/*
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
*/
object * query_ship_containers() { return maoContainers; }

int query_no_destruct() { return 1; }
