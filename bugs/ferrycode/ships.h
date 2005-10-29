
#ifndef __SHIPS__
#define __SHIPS__

// Ships
#define SHIPS       "/objects/ships/"
#define SHIP_SHOP   SHIPS + "shop"
#define SHIP_ROOMS SHIPS + "rooms/"
#define SHIP_ROOM  SHIP_ROOMS  + "ship_room"
#define SHIP_SAVE   "/pvt/player/ships/"
#define SHIP_PUB    SHIPS + "pub_in"
#define SHIP_D_SAVE "/secure/savedir/shipd"
#define SHIP        SHIPS + "ship"
#define NPC_SHIP    SHIPS + "npcship"

//Bitfield flag for item types
#define RACK 1
#define LCONT 2
#define SHIPWEAPON 4
#define CARGO 8
#define SHIPSYS 16
#define FIGUREHEAD 32

//Ship Daemon is #defined in daemons.h as SHIP_D
#endif
