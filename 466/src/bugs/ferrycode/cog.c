#include <mudlib.h>
#include <ships.h>

inherit SHIP;

void reset(int arg) {
    if(!is_clone(this_object())) return;
    ::reset(arg);
    if (arg) return;

    set_alias("cog");

    //Setup defaults in case vars don't restore
    set_name("unknown Cog");
    reset_short("mysterious cog");
    set_main_long("\
This is a long a sleek cruiser. Two masts rise into the sky, the\n\
mizzenmast in the front and the mainmast in the center. Both masts\n\
are covered with square sails. The ship has a stearncastle with the\n\
bridge on top. Including the bowsprit, the ship is about 70 feet\n\
in length.\n");
    //Give it rooms
    add_room("deck", SHIP_ROOMS + "cog_deck");
    add_room("bridge", SHIP_ROOMS + "cog_bridge");
    add_room("cabin", SHIP_ROOMS + "cog_cabin");
    add_room("forecastle", SHIP_ROOMS + "cog_fore");
    add_room("hall", SHIP_ROOMS + "cog_hall");
    add_room("crew", SHIP_ROOMS + "cog_crew");
    add_room("cargo", SHIP_ROOMS + "cog_cargo");
    add_room("crow's nest", SHIP_ROOMS + "cog_nest");

    set_enter_room("deck");
    set_bridge(find_room("bridge"));

    //Set default messages (reset in init_ship)
    set_arrival_msg("The cog arrives.");
    set_depart_msg("The cog sails swiftly");
    set_enter_messages(({"You climb onboard the cog","boards the cog",
      "comes aboard the cog"}));
    set_deboard_messages(({"You disembark the cog","leaves the cog",
      "disembarks from the cog"}));
    init_vehicle();
}

//Overloads of virtual function in parent
string query_ship_class() { return "cog";}
string query_ship_arrive() { return "sails in";}
string query_ship_depart() { return "sails swiftly";}
