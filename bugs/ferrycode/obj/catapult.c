#include <types.h>
#include <mudlib.h>
#include <material.h>
#include <treasure.h>
inherit TREASURE;

int loaded;
object ship_ob;

void reset(int arg) {
  ::reset(arg);
  if(arg) return;
  set_treasure_type(TYP_TRINKET);
  set_material(MAT_WOOD);
  set_name("catapult");
  set_alias("catapult");
  set_short("A huge catapult");
  set_long("\
A large catapult that is used to destroy any enemy ship that it fires upon\n\
in battle. It launches huge boulders at it's target. It is manned by a well\n\
trained crew of 4 hardened sailors.\n");
  set_weight(10000);
  set_value(0);
  loaded = 0;
}

void init() {
  ::init();
  add_action("load","load");
  add_action("fire","fire");
}

int load(string str) {
  ship_ob = environment(this_object())->query_ship();
  if(!ship_ob->query_captains(this_player()->query_real_name())) {
    write("The catapult crew ignores you.\n");
    return 1;
  }
  if(str == "catapult") {
    if(ship_ob->query_bolts() <= 0) {
      write("The catapult crew looks around, but can't find any to load.\n");
      return 1;
    }
    if(loaded) {
      write("It is already loaded.\n");
      return 1;
    }
    else {
      loaded = 1;
      write("The sailors load the catapult.\n");
      return 1;
    }
  }
  return 0;
}

int fire(string str) {
  ship_ob = environment(this_object())->query_ship();
  if(!ship_ob->query_captains(this_player()->query_real_name())) {
    write("The catapult crew ignores you.\n");
    return 1;
  }
  if(str == "catapult") {
    int portopen;
    portopen = environment()->query_opened();
    if(!loaded) {
      write("It isn't loaded.\n");
      return 1;
    }
    if(!portopen) {
      write("A sailor says: We can't do that with the porthole closed.\n");
      return 1;
    }
    else {
      loaded = 0;
      write("The sailors fire a round from the catapult.\n");
      ship_ob->restore_bolts(-1);
      return 1;
    }
  }
  return 0;
}
