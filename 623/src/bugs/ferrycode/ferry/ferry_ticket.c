#include <mudlib.h>
#include <material.h>
#include <daemons.h>

inherit TREASURE;


void reset(int iArg) {

  ::reset(iArg);
  if (iArg) return;

  set_id("ferry_ticket");
  set_alias("ticket");
  set_short("a ferry ticket");
  set_long(wrap("\
This is a small paper ticket. The edges are frayed and it looks like \
the ticket has been well handled. This ticket gains you entrance to one \
of the many ferrys available throughout the lands."));

  set_material(MAT_NATURAL);
  set_weight(0);
  set_value(0);

}
