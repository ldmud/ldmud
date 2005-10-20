#include <mudlib.h>
#include <ships.h>

inherit SHIP;

void reset(int arg) {
    if(!is_clone(this_object())) return;
    ::reset(arg);
    if (arg) return;

    set_alias("galley");
    add_alias("lateen galley");

    //Setup defaults in case vars don't restore
    set_name("unknown Galley");
    reset_short("mysterious Galley");
    set_main_long("\
The lateen galley is a graceful ship, with gently curved lines\n\
and picturesque twin triangular sails. It measures about 75\n\
feet from prow to stern, slightly larger than a cog, but not\n\
as grand as the heavy ships. The waves cascade across its\n\
prow, rendering it even more beautiful.\n");

    //Give it rooms
    add_room("aftdeck", SHIP_ROOMS + "galley_deck");
    add_room("foredeck", SHIP_ROOMS + "galley_foredeck");
    add_room("bridge", SHIP_ROOMS + "galley_bridge");
    add_room("prow", SHIP_ROOMS + "galley_prow");
    add_room("afthall", SHIP_ROOMS + "galley_hall");
    add_room("forehall", SHIP_ROOMS + "galley_forehall");
    add_room("cabin", SHIP_ROOMS + "galley_cabin");
    add_room("crew", SHIP_ROOMS + "galley_crew");
    add_room("cargo1", SHIP_ROOMS + "galley_cargo1");
    add_room("cargo2", SHIP_ROOMS + "galley_cargo2");
    add_room("cargo3", SHIP_ROOMS + "galley_cargo3");
    add_room("cargo4", SHIP_ROOMS + "galley_cargo4");
    add_room("crow's nest", SHIP_ROOMS + "galley_nest");

    //Make some rooms visible from outside
    add_room_look(({"deck","aftdeck"}),"aftdeck");
    add_room_look("foredeck","foredeck");
    add_room_look(({"nest","crow's nest"}),"crow's nest");
    add_room_look("bridge","bridge");
    add_room_look("prow","prow");

    //Add a few extra looks to the ship
    add_extra_look(({"mast","masts"}),"\
The masts sport large triangular sails that gracefully catch\n\
the wind. Both look quite strong.\n");
    add_extra_look(({"sail","sails"}),"\
The sails are shaped like a triangle and hang from the two masts\n\
of the ship. Their shape is what allows the lateen galley to\n\
sail effectively in nearly any direction, regardless of the\n\
wind. They billow now, drifiting on the ocean breeze.\n");

    set_enter_room("aftdeck");
    set_bridge(find_room("bridge"));

    //Set default messages (reset in init_ship)
    set_arrival_msg("The galley arrives.");
    set_depart_msg("The galley sails swiftly");
    set_enter_messages(({"You climb onboard the galley","boards the galley",
      "comes aboard the galley"}));
    set_deboard_messages(({"You disembark the galley","leaves the galley",
      "disembarks from the galley"}));
    init_vehicle();
}

//Overloads of virtual functions in parent
string query_ship_class() { return "lateen galley"; }
string query_ship_arrive() { return "sails gracefully in";}
string query_ship_depart() { return "tacks out to the";}
