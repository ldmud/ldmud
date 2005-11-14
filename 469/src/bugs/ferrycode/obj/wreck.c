#include <mudlib.h>

inherit TREASURE;

void reset(int arg) {
    set_name("wreckage");
    set_long("This is the twisted wreckage of a ship.  Frayed rope and broken\n\
beams abound.  This vessel will never sail again.\n");
    set_short("wreckage of a ship");
    set_weight(1000);
}

void setup(string type, string name, string fleet) {
    set_long("This is the twisted wreckage of the "+type+" "+name+",\n\
of the "+fleet+" fleet.\n\
Frayed pieces of rope and broken beams float\n\
on top of the water.\n\
This vessel will never sail again!\n");
    set_short("The wreckage of a "+type);
    set_alias("wreck_" + type);
    call_out("over", 500);
}

void over() {
  tell_room(environment(this_object()),
            "The wreckage finally settles into the sea, and sinks to the floor.\n");
  destruct(this_object());
  return;
}
 
