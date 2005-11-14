#include "../room.h"

object hobgoblin;

#undef EXTRA_RESET
#define EXTRA_RESET\
    if (!hobgoblin || !living(hobgoblin)) {\
        object money;\
   	hobgoblin = clone_object("obj/monster");\
	hobgoblin->set_name("hobgoblin");\
	hobgoblin->set_level(5);\
        hobgoblin->set_wc(9);\
	hobgoblin->set_short("a hobgoblin");\
	hobgoblin->set_long(\
	  "This hobgoblin looks really nasty.\n");\
	move_object(hobgoblin, this_object());\
	money = clone_object("obj/money");\
	money->set_money(random(50));\
	move_object(money, hobgoblin);\
    }
ONE_EXIT("room/mine/tunnel5", "south",
	 "small room",
	 "A small room with rough cut walls.\n", 0)
