#include <mudlib.h>
#include <terrain.h>
#include <perms.h>
#include <ships.h>

#define DW_PORT "domains/darkwind/wharf/port"
#define DW_HIDDEN_PORT "domains/darkwind/wharf/hidden_port"

inherit "/objects/ships/rooms/ship_room";

void reset(int arg) {
  ::reset(arg);
  if(arg) return;
  set_terrain(TER_OUTSIDE);
  set_no_teleport(1);
  set_light(1);

  //Calls to vehicle_room
  set_audible_room();
  set_listen_to_outside(1);

  //Calls to ship_room
  set_leave_room();
  set_see_outside();
  set_no_drop_here();

  set_short("The maindeck");
  set_long("\
You are on the main deck of the Fairweather. This is small merchantman\n\
built to manoevre ably through the sea lanes and trade with far off\n\
lands. A narrow mast rises from the deck to the crow's nest far above.\n\
Mounted on the mast is a medium-sized square sail that billows and\n\
waves as the wind blows through it. Towards the prow of the ship there\n\
is a small platform with the spar sticking out of it. Towards the stern\n\
is a larger platform, which probably holds the bridge, the command\n\
center of the ship. The sides of the ship are low here, permitting\n\
you to disembark when the ship docks. A small hatch near the starboard\n\
rail provides a way to go below.\n");
  add_look("hatch","The hatch is open, allowing you to go below decks.\n");
  add_look(({"deck","maindeck"}),"\
The deck is made of long planks of sturdy wood.\n");
  add_look("sail","\
The square sail billows at it catches the wind. Rigging extends from\n\
the sail down to the deck, forecastle, and sterncastle.\n");
  add_look("mast","\
The mast is located amidship, and stabs up towards the sky.\n");
  add_look("rigging","\
The lines which allow the sailors to control the sails.\n");
  add_vehicle_exit("fore","forecastle");
  add_vehicle_exit("hatch","hold");
  add_special_exit("disembark","/path/irrelevant","disembark_func");
  set_concealed_exit("disembark",0);
}

string query_exit_path(string sArg) {
  string sName;

  if(sArg != "disembark") return ::query_exit_path(sArg);
  sName = object_name(query_ship_object()->query_vehicle_environment());
  if(sName == DW_HIDDEN_PORT) return DW_PORT;
  return sName;
}
void init() {
  ::init();
  add_action("stop_func","climb");
  add_action("stop_func","aft");
}

status stop_func(string sArg) {
  if(query_verb() == "climb" && sArg != "mast") {
    notify_fail("Climb what?\n");
    return 0;
  }
  write("A sailor yells at you: \"AY ye lubber. Don't ya be doin' that now.\"\n");
  return 1;
}

