
/* --------- debug.h ---------------------
 created by Tech the toolmaker also know
 as Anders Ripa (ripa@cd.chalmers.se)
--------------------------------------------*/

int is_debug;

/*
generic debug support for any object

typical uses are:

	if(is_debug) {
		write("in routine...\n");
	}
or
	if(is_debug) {
		tell_object(ob, "in routine...\n");
	}
*/

/* do add_action("debug_toggle", "debug"); */

int id(string str);
string short();

int debug_toggle(string str) {
	if(!str || !id(str)) return 0;

	is_debug += 1;
	if(is_debug > 1) is_debug = 0;
	if(is_debug) {
		write("Debug is enabled for " + short() + "\n");
	} else {
		write("Debug is disabled for " + short() + "\n");
	}
	return 1;
}

int query_debug() { return is_debug; }

void set_debug(int arg) { is_debug = arg; }

