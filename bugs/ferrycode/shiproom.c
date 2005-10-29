
// This is the room inheritable for rooms of ships.

#include <mudlib.h>
#include <darkwind.h>

inherit VEHICLE_ROOM;

#define TP             this_player()
#define RN             TP->query_real_name()
#define DW_PORT        WHARF + "port"
#define DW_HIDDEN_PORT WHARF + "hidden_port"

// Variables
object ship_ob;

void reset(int arg) {
  ::reset(arg);
  if (arg) return;
  set_no_teleport();
}

void init_room(object ob) {
  ::init_room(ob);
  ship_ob = ob;
}

void init() {
  ::init();
  add_action("quit","quit");
  ship_ob->inc_players();
}

void exit() {
  ship_ob->dec_players();
}

int quit() {
  if (ship_ob->query_captains(RN)) {
    if (sizeof(ship_ob->captains_onboard()) == 1) {
      if (ship_ob->query_target()) ship_ob->sink();
      else ship_ob->reset_ship();
    }
  }
  return 0;
}

int exit_func() {
  if (!ship_ob->query_docked()) {
    write("You cannot leave the ship until it has docked.\n");
    return 1;
  }

  if (environment(ship_ob) == find_object(DW_HIDDEN_PORT)) {
    move_object(ship_ob, DW_PORT);
    ::leave_func();
    move_object(ship_ob, DW_HIDDEN_PORT);
  } else ::leave_func();

  return (1);
}

object query_ship() { return(query_vehicle()); }

void vehicle_move_success(string str) {
  write ("You sail "+vehicle -> short() +" to the "+str+".\n");
  say (this_player()->query_name()+" sails "+vehicle->short() +
       " to the "+str+".\n");
  vehicle->tell_vehicle(this_player()->query_name() +" sails "+
           vehicle -> short()+" to the "+str+".\n",({ query_room_name() }) );
  return;
}

int move_func(string str) {
    int cost, returned;

    switch(str) {
      case "n"  : str = "north"; break;
      case "s"  : str = "south"; break;
      case "e"  : str = "east"; break;
      case "w"  : str = "west"; break;
      case "ne" : str = "northeast"; break;
      case "nw" : str = "northwest"; break;
      case "se" : str = "southeast"; break;
      case "sw" : str = "southwest"; break;
    }

    if (!ship_ob->crew_check()) return 1;
    if (ship_ob->query_target()) {
        write("You're engaged in combat!  You must disengage first.\n");
        return 1;
    }

    cost = ship_ob->query_move_cost();
    if (!ship_ob->query_sp()) cost *= 2;
    if (cost < 1) cost = 1;
    if (cost > ship_ob->query_provisions()) {
        write("The ship doesn't have the provisions to do that!\n");
        return 1;
    }

    returned = ship_ob->move_vehicle(str);

    if (returned != 1) {
      switch (returned) {
        case  0: write("Sail in what direction?\n"); break;
        case -1: write(vehicle->short()+" can't sail in that terrain.\n");
                 break;
        case -2: write(vehicle -> short()+" won't sail that way.\n");
                 break;
      }
      return 1;
    }

    ship_ob->restore_provisions(-cost);
    ship_ob->inc_distance();
    ship_ob->save_ship();
    vehicle_move_success(str);

    if(this_player()->query_brief())
      write(environment(vehicle)->short() + "\n");
    else
      environment(vehicle)->long(0);

    outer_inven();
    return 1;
}

