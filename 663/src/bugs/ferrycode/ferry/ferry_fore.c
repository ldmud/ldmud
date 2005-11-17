#include <mudlib.h>
#include <terrain.h>
#include <ships.h>

inherit "/objects/ships/rooms/ship_room";

void reset(int arg) {
  ::reset(arg);
  if(arg) return;
  set_terrain(TER_OUTSIDE);
  set_light(1);

  //Calls to vehicle_room
  set_listen_to_outside(1);

  //Calls to ship_room
  set_see_outside();
  set_no_drop_here();

  set_short("The forecastle");
  set_long("\
This is a slim structure built near the prow of the ship. It is not\n\
nearly so broad or sturdy as the sterncastle, which houses the bridge.\n\
This structure provides a place to mount weapons or to mount assults\n\
on other ships.\n");
  add_vehicle_exit("aft","deck");
}

