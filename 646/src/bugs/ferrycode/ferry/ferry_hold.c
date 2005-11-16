#include <mudlib.h>
#include <terrain.h>
#include <ships.h>

inherit "/objects/ships/rooms/ship_room";

void reset(int arg) {
  ::reset(arg);
  if(arg) return;
  set_terrain(TER_INSIDE);
  set_light(1);
  set_listen_to_outside(0);
  set_weight_limit(20);
  set_short("Ship's Hold");
  set_long("\
You are standing in the hold below the main deck. It is not very\n\
large, just big enough to allow a few passangers to sleep or take\n\
shelter from the weather. Presumably the rest of the lower deck\n\
is taken up with crew quarters or cargo space, but they must be\n\
accessed from a different hatch.\n");
  add_look("bulkhead","The wall that seperates the rooms of the deck.\n");
  add_look("hold","It isn't the largest or most comfortable, but it will do.\n");
  add_vehicle_exit("ladder","deck");
}

