#include <mudlib.h>
#include <material.h>

inherit OBJECT;

void reset(int iArg) {
  if(!is_clone(this_object())) return;
  ::reset();
  if(iArg) return;
  set_name("cog deck plan");
  set_alt_name("deck plan");
  set_alias("plan");
  add_alias("plans");
  set_short("deck plan for the Cog-class ship");
  set_long("\
This is a small scroll of paper detailing the arrangement of rooms\n\
in the Cog class vessel. You should \"read\" it to view it.\n");
  set_value(0);
  set_no_sell();
}

void init() {
  ::init();
  add_action("read_func","read");
}

status read_func(string sArg) {
  if(present(sArg,environment(this_player())) != this_object() &&
    present(sArg,this_player()) != this_object()) {
    notify_fail("Read what?\n");
    return 0;
  }
  more("/doc/ships/COG_PLAN");
  return 1;
}
