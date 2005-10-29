#include <mudlib.h>
#include <ships.h>
#include <perms.h>
#include "ferry.h"

inherit GEN_FERRY;

#define TRIP_PROGRAM ([0:({"nextdest darkwind"}) + AT_HB, 1: HB_TO_FJORD,\
     2:FJORD_TO_GEARNAT, 3:AEGIR_TO_SALMO, 4:GEARNAT_TO_DW,\
     5:({"nextdest hyperborea"}) + AT_DW, 6: DW_TO_GEARNAT,\
     7:SALMO_TO_AEGIR, 8:GEARNAT_TO_FJORD, 9:FJORD_TO_HB ])


void reset(int arg) {
    if(!is_clone(this_object())) return;
    ::reset(arg);
    if (arg) return;

    set_ferry_name("Pequod");
    set_trip_program(TRIP_PROGRAM);
    reset_short("The whaling ship Pequod");
    set_main_long("\
This is a long a sleek cruiser. Two masts rise into the sky, the\n\
mizzenmast in the front and the mainmast in the center. Both masts\n\
are covered with square sails. The ship has a stearncastle with the\n\
bridge on top. Including the bowsprit, the ship is about 70 feet\n\
in length.\n");
    set_alias("pequod");
    set_alias("whaler");
    set_alias("whaling ship");
    //Give it rooms
    add_room("deck", FERRY_ROOMS + "ferry_deck");
    add_room("forecastle", FERRY_ROOMS + "ferry_fore");
    add_room("hold", FERRY_ROOMS + "ferry_hold");

    set_enter_room("deck");
//    set_bridge(find_room("bridge"));
    add_room_look(({"deck","aftdeck"}),"aftdeck");
    add_room_look("forecastle","fore");
    init_vehicle();
    setup();
}

//Overloads of virtual function in parent
string query_ship_class() { return "whaling ship";}
string query_ship_arrive() { return "kicks up spray as it sails in";}
string query_ship_depart() { return "sails";}

