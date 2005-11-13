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
  set_name("ballista");
  set_alias("ballista");
  set_short("A huge ballista");
  set_long("\
A huge ballista that fires large bolts at enemy ships. It is manned by a\n\
well trained crew of 3 hardened sailors.\n");
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
    write("The ballista crew ignores you.\n");
    return 1;
  }
  if(str == "ballista") {
    if(ship_ob->query_rocks() <= 0) {
      write("The ballista crew looks around, but can't find any to load.\n");
      return 1;
    }
    if(loaded) {
      write("It is already loaded.\n");
      return 1;
    }
    else {
      loaded = 1;
      write("The sailors load the ballista.\n");
      return 1;
    }
  }
  return 0;
}

int fire(string str) {
  ship_ob = environment(this_object())->query_ship();
  if(!ship_ob->query_captains(this_player()->query_real_name())) {
    write("The ballista crew ignores you.\n");
    return 1;
  }
  if(str == "ballista") {
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
      write("The sailors fire a round from the ballista.\n");
      ship_ob->restore_rocks(-1);
      return 1;
    }
  }
  return 0;
}
