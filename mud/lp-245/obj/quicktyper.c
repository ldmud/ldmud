
/*
--- quicktyper.c ---

A quicktyping utility that stores a list of command aliases and
keeps a history of the last command given by the player
and let you put several commands on a single line.

do get more information do "help quicktyper"

Tech's quicktyper.c

this file requires the debug.h file

and it also requires LPC version 2.3

This utility was developed in Genesis (the original LP-Mud)
by Tech the toolmaker also nown as Anders Ripa (ripa@cd.chalmers.se)
and bug reports etc should be sent to me

*/

#define	VERSION		"2.06"
#define	VERSION_DATE	"901020"

#define	FILE_NAME	"obj/quicktyper" /* used by query_autoload */

#include "debug.h"

string	owner;
string	* list_ab;
string	* list_cmd;
string	* list_history;

#define MAX_HISTORY	20
int	history_pos;
int	history_offset;
int	no_history_add;

int	refreshing;
int	needs_refresh;

refresh();
contains();

/*
  return som info for the interested
 */
string query_info()
{
    return "A magic quick typing utility made by Tech.";
}

/*
  make it possible to retrieve information from the quicktyper
 */
mixed query_quicktyper(int arg)
{
    if(arg == 0) {
	return list_ab;
    }
    if(arg == 1) {
	return list_cmd;
    }
    if(arg == 2) {
	return list_history;
    }
    if(arg == 3) {
	return history_pos;
    }
    if(arg == 4) {
	return history_offset;
    }
    return 0;
}

int id(string str)
{
    if(str && (str == "quicktyper" || str == owner + "'s quicktyper" || str == "tech_quicktyper")) {
	return 1;
    }
    return 0;
}

string query_name()
{
    return owner + "'s quicktyper";
}

object	ob;	/* used to hold this_player() */

void reset(int  arg)
{

    if(is_debug) {
	write("reset(" + arg + ")\n");
	write(VERSION + "\n");
	
	write("this_object()="); write(this_object()); write("\n");
	write("environment(this_object())="); write(environment(this_object())); write("\n");
	write("this_player()="); write(this_player()); write("\n");
	write("environment(this_player())=");
    if (this_player()) write(environment(this_player()));
    else write("none");
    write("\n");
    }

    if(!refreshing && this_player()) {
	owner = this_player()->query_name();
    }

    if(!list_history) {
	list_history = allocate(MAX_HISTORY);
    }
}

void init_alias_list() {
    object	obj;

#if 0
    if(!list_ab && this_player()) {
	obj = first_inventory(this_player());
	while(obj) {
	    if(obj->query_quicktyper(0)) {
		if(1 || is_debug) {
		    write("retrieving alias from existing quicktyper ");
		    write(obj); write ("\n");
		}
		/* retrieve alias from an existing quicktyper */
		list_ab = obj->query_quicktyper(0);
		list_cmd = obj->query_quicktyper(1);
		break;
	    }
	    obj = next_inventory(obj);
	}
	if(!list_ab) {
	    list_cmd = 0;
	}
	if(!list_cmd) {
	    list_ab = 0;
	}
    }
#endif
    if(!list_ab) {
	list_ab = allocate(24);
    }
    if(!list_cmd) {
	list_cmd = allocate(24);
    }
}

void init()
{
    int	i;
    object	obj;

    if(is_debug) {
	write("init()\n");
	write("this_object()="); write(this_object()); write("\n");
	write("environment(this_object())="); write(environment(this_object())); write("\n");
	write("this_player()="); write(this_player()); write("\n");
	write("environment(this_player())="); write(environment(this_player())); write("\n");
    }

    if(this_player()) {
	owner = this_player()->query_name();
    }
    init_alias_list();

    if(environment(this_object()) == this_player()) {
	add_action("alias", "alias");
	add_action("do_cmd", "do");
	add_action("history", "history");
	add_action("resume", "resume");
	add_action("do_refresh", "refresh");
	add_action("help", "help");
	/*
	  add_action("drop_object", "drop");
	 */
	/* let wizards have some additional information commands */
	if(this_player()->query_level() >= 20) {
	    add_action("version", "ver");
	    add_action("debug_toggle", "debug");     /* declared in debug.h */
	}
	
	i = 0;
	while(i < sizeof(list_ab)) {
	    if(list_ab[i] && list_ab[i] != "" && list_cmd[i] && list_cmd[i] != "") {
		add_action("do_it", list_ab[i]);
	    }
	    i += 1;
	}
	
	add_action("history_add", "", 1);
	
	if(!refreshing) {
	    write("Quicktyper....\n");
	} else {
	    if(is_debug) {
		write("quick refresh -init \n");
	    }
	}
	if(!needs_refresh && !refreshing) {
	    if(is_debug) {
		write("registred an refresh in 30 sec\n");
		needs_refresh = 1;
	    }
	    call_out("refresh", 30, this_player());
	}
	
    }
}

int do_refresh() {
    write("Refreshing Quicktyper ..");
    refresh(this_player());
    write("Done.\n");
    return 1;
}

int refresh(object obj) {

    int	may_need_warning;

    may_need_warning = 0;

    if(is_debug) {
	tell_object(obj, "Refreshing Quicktyper,");
    }
    if(first_inventory(obj) != this_object()) {
	may_need_warning = 1;
    }
    refreshing = 1;

    move_object(this_object(), "room/storage");

    if(is_debug) {
	tell_object(obj, "moved to storage,");
    }

    move_object(this_object(), obj);

    if(is_debug) {
	tell_object(obj, "back again\n");
    }

    if(may_need_warning && obj->query_level() > 19)  {
	tell_object(obj, "Quicktyper: Your inventory has been rearranged.\n");
    }
    refreshing = 0;
    needs_refresh = 0;

    return 1;
}

int	wrapped;

int do_old(string verb, string str) {
    int	pos;
    string	temp;

    if(is_debug) {
	write("verb=" + verb + "\n");
	write("arg=" + str + "\n");
    }

    if(strlen(verb) <= 1 || verb[0] != '%') {
	write("do_old: return 0\n");
	return 0;
    }

    if(verb == "%%") {	
	if(is_debug) {
	    write("last command\n");
	}
	if(history_pos == 0) {
	    if(!wrapped) {
		write("No history!\n");
		return 1;
	    }
	    pos = MAX_HISTORY -1;
	} else {
	    pos = history_pos -1;
	}
	
	if(is_debug) {
	    write("history_pos=" + history_pos + "\n");
	    write("pos=" + pos + "\n");
	    write("will do: " + list_history[pos] + "\n");
	}
	
	if(str && str != "") {
	    write(list_history[pos] + " " + str + "\n");
	    command(list_history[pos] + " " + str, this_player());
	} else {
	    write(list_history[pos] + "\n");
	    command(list_history[pos], this_player());
	}
	return 1;
    }
    if(sscanf(verb, "%%d%s", pos, temp) >= 1) {
	if(is_debug) {
	    write("old command\n");
	}
	if(temp == 0) {
	    temp = "";
	}
	if(pos < 1 || pos <= history_offset) {
	    write("History position " + pos + " is not available!\n");
	    return 1;
	}
	if(!wrapped && (pos-1) >= history_pos) {
	    write("History position " + pos + " is not available!\n");
	    return 1;
	}
	if(pos > MAX_HISTORY + history_offset - 1) {
	    write("History position " + pos + " is not available!\n");
	    return 1;
	}
	if(!wrapped) {
	    if(is_debug) {
		write("Not wrapped.\n");
	    }
	    if(str && str != "") {
		write(list_history[pos-1] + temp + " " + str + "\n");
		command(list_history[pos-1] + temp + " " + str, this_player());
	    } else {
		write(list_history[pos-1] + temp + "\n");
		command(list_history[pos-1] + temp, this_player());
	    }
	    return 1;
	} else {
	    if(is_debug) {
		write("pos=" + pos + "\n");
		write("history_offset=" + history_offset + "\n");
		write("history_pos=" + history_pos + "\n");
	    }
	
	    pos -= history_offset;
	
	    if(is_debug) {
		write("pos-history_offset=" + pos + "\n");
	    }
	
	    pos += history_pos;
	
	    if(is_debug) {
		write("pos-history_offset+history_pos=" + pos + "\n");
	    }
	
	    if(pos >= MAX_HISTORY) {
		pos -= MAX_HISTORY;
	    }
	
	    if(is_debug) {
		write("pos-history_offset+history_pos=" + pos + "\n");
		write("would do: " + list_history[pos] + "\n");
	    }
	    if(str && str != "") {
		write(list_history[pos] + " " + str + "\n");
		command(list_history[pos] + " " + str, this_player());
	    } else {
		write(list_history[pos] + "\n");
		command(list_history[pos], this_player());
	    }
	    return 1;
	}
    }
    write("do_old: return 0\n");
    return 0;
}

int history() {
    int	i;
    int	number;

    owner = this_player()->query_name();

    if(wrapped) {
	number = history_offset + 1;
	i = history_pos + 1;
	while(i < MAX_HISTORY) {
	    if(is_debug) {
		write(i + " ");
	    }
	    write("%" + number + "\t" + list_history[i] + "\n");
	    i += 1;
	    number += 1;
	}
    } else {
	number = 1;
    }
    i=0;
    while(i < history_pos) {
	if(is_debug) {
	    write(i + " ");
	}
	write("%" + number + "\t" + list_history[i] + "\n");
	i += 1;
	number += 1;
    }
    return 1;
}

string	last_cmd_added;
string	last_str_added;

int	counter;
#define COUNT_UNTIL_REFRESH	40

int history_add(string str) {

    string	verb;
    int	i;

    if(is_debug) {
	write("history_add\n");
    }

    verb = query_verb();

    if(!needs_refresh) {
	counter += 1;
    }

    if(counter >= COUNT_UNTIL_REFRESH || verb == "get" || verb == "take") {
	counter = 0;
	if(!needs_refresh) {
	    needs_refresh = 1;
	    if(is_debug) {
		write("registered an refresh in 20 sec\n");
	    }
	    call_out("refresh", 20, this_player());
	}
    }

    if(is_debug) {
	write("verb=" + verb + "\n");
	write("str=" + str + "\n");
    }

    if(verb == 0 ||  verb =="") {
	return 0;
    }

    if(strlen(verb) > 1 && verb[0] == '%') {
	if(is_debug) {
	    write("calling do_old\n");
	}
	return do_old(verb, str);
    }

    if(verb == last_cmd_added) {
	if(!str) {
	    return 0;
	}
	if(str == last_str_added) {
	    return 0;
	}
    }

    if(no_history_add) {
	no_history_add = 0;
	return 0;
    }

    last_cmd_added = verb;
    last_str_added = str;

    i = 0;
    while(i < sizeof(list_ab)) {
	if(list_ab[i] == verb) {
	    return 0;	/* dont add aliases to the list */
	}
	i += 1;
    }

    if(str && str != "") {
	list_history[history_pos] = verb + " " + str;
    } else {
	list_history[history_pos] = verb;
    }
    history_pos += 1;
    if(history_pos >= MAX_HISTORY) {
	history_pos = 0;
	wrapped = 1;
    }
    if(wrapped) {
	history_offset += 1;
    }
    return 0;
}

string short()
{
    int	temp;

    return owner + "'s quicktyper";
}

void long()
{
    write("This is a typing aid to allow long commands to be replaced with short aliases.\n");
    write("It also contains a history of your commands\n");
    write("Do \"help quicktyper\" to get more information about how to use this tool.\n");

}

int version(string str) {
    if(!str || !id(str)) {
	return 0;
    }
    write("Tech's quicktyper version " + VERSION + " created " + VERSION_DATE + "\n");
    return 1;
}

int alias(string str) {
    int	i;
    string	ab, cmd;

    owner = this_player()->query_name();

    if(!str || str == "") {
	write("The aliases in your quicktyper are:\n");
	i = 0;
	while(i < sizeof(list_ab)) {
	    if(list_ab[i]) {
		write((list_ab[i] + "         ")[0..9] + list_cmd[i] + "\n");
	    }
	    i += 1;
	}
	return 1;
    }
    if(sscanf(str, "%s %s", ab, cmd) == 2) {
	/* adding a new alias */
	i = 0;
	while(i < sizeof(list_ab)) {
	    if(list_ab[i] == ab) {
		/* replace old definition */
		list_cmd[i] = cmd;
		write("Ok.\n");
		return 1;
	    }
	    i += 1;
	}
	i = 0;
	while(i < sizeof(list_ab)) {
	    if(!list_ab[i]) {
		list_ab[i] = ab;
		list_cmd[i] = cmd;
		add_action("do_it", list_ab[i]);
		add_action("history_add", "", 1);
		write("Ok.\n");
		return 1;
	    }
	    i += 1;
	}
	write("Sorry the quicktyper is full!\n");
	return 1;
    }
    if(sscanf(str, "%s", ab) == 1) {
	/* removing an alias */
	i = 0;
	while(i < sizeof(list_ab)) {
	    if(list_ab[i] && list_ab[i] == ab) {
		list_ab[i] = 0;
		list_cmd[i] = 0;
		write("Removed alias for " + ab + ".\n");
		return 1;
	    }
	    i += 1;
	}
	write(ab + " didn't have an alias!\n");
	return 1;
    }
    write("This can't happen!\n");
    return 0;
}

int help(string str) {
    if(!str || !id(str)) {
	return 0;
    }
    write("This Quicktyper alows for command alias, e.g. short commands \nthat is expanded by the Quicktyper\n");
    write("The commands available for the quicktyper are:\n");
    write("alias			- show the list of current alias\n");
    write("alias command what to do\n			- make \"command\" an alias for the \"what to do\"\n");
    write("alias command		- remove alias for \"command\"\n");
    write("do cmd1,cmd2,cmd3,..	- do a series of commands\n");
    write("do			- pauses execution of a series of commands\n");
    write("resume			- resume paused commands\n");
    write("history			- give a list of previous commands\n");
    write("%%			- repeat last command\n");
    write("%n			- repeat command number 'n'\n");
    write("help quicktyper		- this helptext\n");

    if(this_player()->query_level() >= 20) {
	write("ver quicktyper		- shows version information\n");
	write("debug quicktyper	- toggle internal debug status\n");
    }
    write("examples:	'alias l look at watch'\n		enables you to write l to look at your watch.\n");
    write("		'do smile,look,laugh'\n	will first make you smile then look and laugh.\n");
    write("		doing '%%' will then repeat this three commands again\n\n");
    write("Another product from the kingdom of Zalor.\n(send bugreports etc. to Tech)\n");
    write("(Error: messages that tell you that something is not found,\nis due to the LP-Mud security system and can not be avoided.)\n");
    owner = this_player()->query_name();
    return 1;
}

int get()
{
    if(contains("tech_quicktyper", this_player())) {
	return 0;
    }
    return 1;
}

int drop()
{
    return 1;	/* cant drop ! */
}

int query_value()
{
    return 0;	/* no value */
}

string query_auto_load()
{
    string	temp;
    int	i, count;

    i = 0;
    count = 0;
    while(i < sizeof(list_ab)) {
	if(list_ab[i] && list_cmd[i]) {
	    count += 1;
	}
	i += 1;
    }
    temp = FILE_NAME + ":"  + count + ";";
    i = 0;
    while(i < sizeof(list_ab)) {
	if(list_ab[i] && list_cmd[i]) {
	    temp += list_ab[i] + " " + list_cmd[i] + ";.X.Z;";
	}
	i += 1;
    }

    return temp;	
}

int do_it(string str)
{
    int	i;
    string	verb;

    if(is_debug) {
	write("query_verb=" + query_verb() + "\n");
	write("str=" + str + "\n");
    }
    verb = query_verb();
    if(verb == 0) return 0;

    i = 0;
    while(i < sizeof(list_ab)) {
	if(list_ab[i] == verb) {
	    if(list_cmd[i] == 0) {
		list_ab[i] = 0;
	    } else {
		if(str && str != "") {
		    if(is_debug) {
			write(list_cmd[i] + " " + str + "\n");
		    }
		    /*
		      no_history_add = 1;
		     */
		    command(list_cmd[i] + " " + str, this_player());
		    no_history_add = 0;
		} else {
		    if(is_debug) {
			write(list_cmd[i] + "\n");
		    }
		    /*
		      no_history_add = 1;
		     */
		    command(list_cmd[i], this_player());
		    no_history_add = 0;
		}
		return 1;
	    }
	}
	i += 1;
    }
    /* not found */
    return 0;
}

void init_arg(string arg) {
    int	temp;
    int	count, place;
    string	ab, cmd;
    string	the_rest;

    if(is_debug) write("init_arg(" + arg + ")\n");

    if(arg) {
	the_rest = "";
	if(sscanf(arg, "%d;%s", count, the_rest) == 2) {
	    if(is_debug) write("count=" + count + "\n");
	    init_alias_list();
	
	    while(the_rest && the_rest != "" && place < sizeof(list_ab))
	    {
		arg = the_rest;
		if(sscanf(arg, "%s %s;.X.Z;%s", ab, cmd, the_rest) >= 2) {
		    if(ab && ab != "" && cmd && cmd != "") {
			list_ab[place] = ab;
			list_cmd[place] = cmd;
			place += 1;
		    }
		}
	    }
	}
    }
}


/* do one ore more commands */

string	org_cmds;
string	more_cmds;
int	first_call;
int	paused;

void  heart_beat()
{
    string	the_rest;
    string	cmd;

    if(ob && more_cmds && more_cmds != "") {
	if(sscanf(more_cmds, "%s,%s", cmd, the_rest) == 2) {
	    tell_object(ob, "doing: " + cmd + "\n");
	    no_history_add = 1;
	    command(cmd, ob);
	    no_history_add = 0;
	    more_cmds = the_rest;
	} else {
	    cmd = more_cmds;
	    tell_object(ob, "doing: " + cmd + "\n");
	    no_history_add = 1;
	    command(cmd, ob);
	    no_history_add = 0;
	    more_cmds = 0;
	    if(!first_call) {
		set_heart_beat(0);
	    }
	    tell_object(ob, "Done.\n");
	}
	
    } else {
	ob = 0;
	more_cmds = 0;
	if(!first_call) {
	    set_heart_beat(0);
	}
    }
}

int do_cmd(string  str)
{

    if(!str || str == "")  {
	if(more_cmds) {
	    set_heart_beat(0);
	    write("Paused. Use \"resume to continue\"\n");
	    paused = 1;
	} else {
	    write("usage: do cmd1,cmd2, cmd3,...\n");
	}
	return 1;
    }

    if(more_cmds && !paused) {
	write("Busy doing your commands:\n" + more_cmds + "\n");
	return 1;
    }
    if(paused) {
	write("Skipping paused commands:\n" + more_cmds + "\n");
	paused = 0;
    }
    more_cmds = str;
    ob = this_player();
    first_call = 1;
    heart_beat();
    first_call = 0;
    set_heart_beat(1);
    return 1;
}

int resume() {
    if(paused && ob && more_cmds && more_cmds != "") {
	paused = 0;
	first_call = 1;
	heart_beat();
	first_call = 0;
	set_heart_beat(1);
	return 1;
    }
    write("Nothing to resume.\n");
    return 1;
}

/* ------- contains ----------- */

/* check to see if an object "obj" contains another object "str" */

int contains(string str, object obj)
{
    object	ob;


    if(!str || str == "") return 0;

    ob = first_inventory(obj);

    if(!ob) return 0;	/* of cource it didn't contain */
    while(ob) {
	if(ob->id(str)) {
	    return 1;	/* we found it */
	}
	/* could add an recursive call here to check
	   for items in items !*/
	
	ob = next_inventory(ob);
    }
    return 0;	/* not found */
}

/* --- end of quicktyper.c */

