#include <mudlib.h>
#include <ships.h>

inherit SHIP;

void reset(int arg) {
    if(!is_clone(this_object())) return;
    ::reset(arg);
    if (arg) return;

    set_alias("carrack");
    add_alias("cruiser");
    add_alias("carrack cruiser");

    //Setup defaults in case vars don't restore
    set_name("unknown Carrack");
    reset_short("mysterious Carrack");
    set_main_long("\
This carrack is truly a fine example of a sailing ship. Its\n\
two proud masts each carry a large sail, the mainmast a\n\
gigantic square sail, the mizzenmast a graceful lateen. The\n\
sterncastle of the carrack is quite long and extends fornearly\n\
a third of the keel length of the ship, which is 100 feet. This\n\
proud ship could stand valiantly in any sea battle.\n");

    //Give it rooms
    add_room("aftdeck", SHIP_ROOMS + "carr_deck");
    add_room("sterncastle",SHIP_ROOMS + "carr_stern");
    add_room("cabin", SHIP_ROOMS + "carr_cabin");
    add_room("foredeck", SHIP_ROOMS + "carr_foredeck");
    add_room("bridge", SHIP_ROOMS + "carr_bridge");
    add_room("weapons", SHIP_ROOMS + "carr_weapon");
    add_room("forecastle", SHIP_ROOMS + "carr_fore");
    add_room("hall1", SHIP_ROOMS + "carr_hall1");
    add_room("hall2", SHIP_ROOMS + "carr_hall2");
    add_room("hall3", SHIP_ROOMS + "carr_hall3");
    add_room("hall4", SHIP_ROOMS + "carr_hall4");
    add_room("hall5", SHIP_ROOMS + "carr_hall5");
    add_room("hall6", SHIP_ROOMS + "carr_hall6");
    add_room("hall7", SHIP_ROOMS + "carr_hall7");
    add_room("crew", SHIP_ROOMS + "carr_crew");
    add_room("mess", SHIP_ROOMS + "carr_mess");
    add_room("cabin1", SHIP_ROOMS + "carr_cabin1");
    add_room("cabin2", SHIP_ROOMS + "carr_cabin2");
    add_room("cargo1", SHIP_ROOMS + "carr_cargo1");
    add_room("cargo2", SHIP_ROOMS + "carr_cargo2");
    add_room("cargo3", SHIP_ROOMS + "carr_cargo3");
    add_room("cargo4", SHIP_ROOMS + "carr_cargo4");
    add_room("cargo5", SHIP_ROOMS + "carr_cargo5");
    add_room("cargo6", SHIP_ROOMS + "carr_cargo6");
    add_room("crow's nest", SHIP_ROOMS + "carr_nest");

    //Make some rooms visible from outside
    add_room_look(({"deck","aftdeck"}),"aftdeck");
    add_room_look("foredeck","foredeck");
    add_room_look(({"nest","crow's nest"}),"crow's nest");
    add_room_look("bridge","bridge");
    add_room_look("forecastle","forecastle");
    add_room_look("sterncastle","sterncastle");

    //Add extra looks to the ship


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
string query_ship_class() { return "carrack"; }
string query_ship_arrive() { return "sails smoothly in";}
string query_ship_depart() { return "rides the wind";}
