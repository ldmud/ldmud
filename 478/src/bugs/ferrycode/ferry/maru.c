#include <mudlib.h>
#include <ships.h>
#include <perms.h>
#include "ferry.h"

inherit GEN_FERRY;

#define TRIP_PROGRAM ([0:({"nextdest odako"})+AT_SV, 1:SV_TO_RIVER,\
     2:RIVER_TO_OD, 3:({"nextdest souvrael"})+AT_OD, 4:OD_TO_RIVER,\
     5:RIVER_TO_SV])

void reset(int arg) {
    if(!is_clone(this_object())) return;
    ::reset(arg);
    if (arg) return;

    //Setup defaults in case vars don't restore
    set_ferry_name("Oshimara Maru");
    set_trip_program(TRIP_PROGRAM);
    reset_short("the Oshimara Maru");
    set_main_long("\
This is a long a sleek cruiser. Two masts rise into the sky, the\n\
mizzenmast in the front and the mainmast in the center. Both masts\n\
are covered with square sails. The ship has a stearncastle with the\n\
bridge on top. Including the bowsprit, the ship is about 70 feet\n\
in length.\n");
    set_alias("maru");
    add_alias("oshimara");
    add_alias("oshimara maru");
    //Give it rooms
    add_room("deck", FERRY_ROOMS + "ferry_deck");
    add_room("forecastle", FERRY_ROOMS + "ferry_fore");
    add_room("hold", FERRY_ROOMS + "ferry_hold");

    set_enter_room("deck");
//  set_bridge(find_room("bridge"));
    add_room_look(({"deck","aftdeck"}),"aftdeck");
    add_room_look("forecastle","fore");
    init_vehicle();
    setup();
}

//Overloads of virtual function in parent
string query_ship_class() { return "trade ship";}
string query_ship_arrive() { return "drifts in";}
string query_ship_depart() { return "meanders out";}

