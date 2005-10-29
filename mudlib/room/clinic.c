#include <std.h>

#undef EXTRA_INIT
#define EXTRA_INIT  add_action("register"); add_verb("register");

ONE_EXIT("room/church", "west",
	 "The clinic",
	 "This is an outpatient clinic, for adventurers who find themselves with\n" +
	 "minor problems.  This clinic can treat problems such as your not being\n" +
	 "able to fight, not being able to carry as much as you should, and\n" +
	 "similar inconveniences.  If you have one of these problems, just type\n" +
	 "'register' and the problem should go away.\n", 1)

register() {
    call_other(this_player(),"reset",1);
    return 1;
}
