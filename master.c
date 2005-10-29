#pragma strict_types

/*
 * MOVE THIS FILE TO /obj/master.c !
 *
 * This is the LPmud master object, used from version 3.0.
 * It is the second object loaded after void.c.
 * Everything written with 'write()' at startup will be printed on
 * stdout.
 * 1.  reset() will be called first.
 * 2. flag() will be called once for every argument to the flag -f
 * 	supplied to 'parse'.
 * 3. epilog() will be called.
 * 4. The game will enter multiuser mode, and enable log in.
 *
 * This version of master.c is specifically designed to be used with mudlib
 * 2.4.6 or earlier. The master.c for mudlib 3.0 will be supplied with
 * that source.
 *
 * One thing is different in the file access permission system in game
 * driver 3.0. The master object can read and write any file.
 * That means that all occurences of such
 * manipulations below must be carefully garded against calls from
 * elsewhere. Always make a comment at the start of a function that is doing
 * such things, to make it easy for other to quickly assert security.
 * The master object can of course not access files above the mudlib
 * directory.
 */

/*
 * To test a new function xx in object yy, do
 * parse "-fcall yy xx arg" "-fshutdown"
 */
void flag(string str) {
    string file, arg;
    int num_arg;

    if (sscanf(str, "for %d", num_arg) == 1) {
	write("Got : " + catch(this_object()->xx(num_arg)) + "\n");
	return;
    }
    if (str == "shutdown") {
	shutdown();
	return;
    }
    if (sscanf(str, "dhrystone %d", arg) == 1) {
	call_other("players/lars/dry", "main", arg);
	return;
    }
    if (sscanf(str, "echo %s", arg) == 1) {
	write(arg + "\n");
	return;
    }
    if (sscanf(str, "call %s %s", file, arg) == 2) {
	arg = (string)call_other(file, arg);
	write("Got " + arg + " back.\n");
	return;
    }
    write("master: Unknown flag " + str + "\n");
}

/*
 * This function is called every time a player connects.
 * input_to() can't be called from here.
 */
object connect() {
    object ob;
    string ret;

    write("obj/master: Connect to player.c...");
#if 0
    ret = (string)catch(ob = clone_object("obj/player"));
#else
    ob = clone_object("obj/player");
#endif
    write("\n");
    if (ret) {
	write(ret + "\n");
	return 0;
    }
    return ob;
}

int xx(int arg)
{
    return 1/arg;
}

/*
 * This function is called for a wizard that has dropped a castle.
 * The argument is the file name of the object that called create_wizard().
 * Verify that this object is allowed to do this call.
 */
int verify_create_wizard(object ob) {
    int dummy;

    if (sscanf(file_name(ob), "room/port_castle#%d", dummy) == 1)
	return 1;
    return 0;
}

/*
 * Get the owner of a file. This is called from the game driver, so as
 * to be able to know which wizard should have the error.
 */
string get_wiz_name(string file) {
    string name, rest;

    if (sscanf(file, "players/%s/%s", name, rest) == 2) {
	return name;
    }
    return 0;
}

/*
 * Write an error message into a log file. The error occured in the object
 * 'file', giving the error message 'message'.
 */
void log_error(string file, string message)
{
    string name;

    name = get_wiz_name(file);
    if (name == 0)
	name = "log";
    log_file(name, message);
}

/* save_ed_setup and restore_ed_setup are called by the ed to maintain
   individual options settings. These functions are located in the master
   object so that the local gods can decide what strategy they want to use.
   suggestions:
	A setup file for every wizard.
		advantages:	transparent to the user
				independent of wizard count
		disadvantage:	extra file access at ed invocation
	An array in the master object, wizards are searched by member_array
		advantage:	easy to implement
		disadvantage:	performance degradation with high wizard counts
	An AVL-tree to access wizards by name
		advantage:	can fit any need
		disadvantage:	hard to implement, will need more overhead on
				small and medium muds than it can ever make
				good by lg(wizcount) complexity
	Dedicated flags in every wizard object, inherited from /obj/living
		advantages:	easy to implement ( as shown here)
				independent of wizard count
				Will also work for nonm-wizards.
		disadvantage:	care has to be taken to avoid collision with
				other uses of the /obj/living flags.          */

#if 0 /* Dedicated flags not used. A save file method is used below */

#define FIRST_ED_FLAG 0 /* adjust this value so that no flags are clobbered */
#define NUM_ED_FLAGS 10
int save_ed_setup(object wiz, int setup) {
    int mask,i;

    for (mask=1,i=FIRST_ED_FLAG;i<FIRST_ED_FLAG+NUM_ED_FLAGS;mask<<=1,i++) {
	if ( setup & mask ) wiz->set_flag(i);
	else wiz->clear_flag(i);
    }
    return 1; /* function is defined, success */
}

int retrieve_ed_setup(object wiz) {
    int i,setup;

    for(i=FIRST_ED_FLAG+NUM_ED_FLAGS;i>FIRST_ED_FLAG;) {
	setup+=setup+wiz->test_flag(--i);
    }
    return setup;
}

#else

/*
 * The wizard object 'who' wants to save his ed setup. It is saved in the
 * file /players/wiz_name/.edrc . A test should be added to make sure it is
 * a call from a wizard.
 *
 * Don't care to prevent unauthorized access of this file. Only make sure
 * that a number is given as argument.
 */
int save_ed_setup(object who, int code) {
    string file;

    if (!intp(code))
	return 0;
    file = "/players/" + lower_case((string)who->query_name()) + "/.edrc";
    rm(file);
    return write_file(file, code + "");
}

/*
 * Retrieve the ed setup. No meaning to defend this file read from
 * unauthorized access.
 */
int retrieve_ed_setup(object who) {
    string file;
    int code;

    file = "/players/" + lower_case((string)who->query_name()) + "/.edrc";
    if (file_size(file) <= 0)
	return 0;
    sscanf(read_file(file), "%d", code);
    return code;
}
#endif

/*
 * Create a home dritectory and a castle for a new wizard. It is called
 * automatically from create_wizard(). We don't use the 'domain' info.
 * The create_wizard() efun is not really needed any longer, as a call
 * could be done to this function directly.
 *
 * This function can create directories and files in /players. It is
 * garded from calls from the wrong places.
 */
string master_create_wizard(string owner, string domain, object caller) {
    string def_castle;
    string dest, castle, wizard;
    object player;

    player = find_player(owner);
    if (!player)
	return 0;
    if (!verify_create_wizard(caller)) {
	tell_object(player, "That is an illegal attempt!\n");
	return 0;
    }
    if (caller != previous_object()) {
	tell_object(player, "Faked call!\n");
	return 0;
    }
    wizard = "/players/" + owner;
    castle = "/players/" + owner + "/castle.c";
    if (file_size(wizard) == -1) {
	tell_object(player, "You now have a home drirectory: " +
		    wizard + "\n");
	mkdir(wizard);
    }
    dest = file_name(environment(player));
    def_castle = "#define NAME \"" + owner + "\"\n#define DEST \"" +
	dest + "\"\n" + read_file("/room/def_castle.c");
    if (file_size(castle) > 0) {
	tell_object(player, "You already had a castle !\n");
    } else {
	/* The master object can do this ! */
	if (write_file(castle, def_castle)) {
	    tell_object(player, "You now have a castle: " + castle + "\n");
	    if (!write_file("/room/init_file", extract(castle, 1) + "\n"))
		tell_object(player, "It couldn't be loaded automatically!\n");
	} else {
	    tell_object(player, "Failed to make castle for you!\n");
	}
    }
    return castle;
}

/*
 * When an object is destructed, this function is called with every
 * item in that room. We get the chance to save players !
 */
void destruct_environment_of(object ob) {
    if (!interactive(ob))
	return;
    tell_object(ob, "Everything you see is disolved. Luckily, you are transported somewhere...\n");
    ob->move_player("is transfered#room/void");
}

/*
 * Define where the '#include' statement is supposed to search for files.
 * "." will automatically be searched first, followed in order as given
 * below. The path should contain a '%s', which will be replaced by the file
 * searched for.
 */
string *define_include_dirs() {
    return ({"/room/%s", "/sys/%s"});
}

/*
 * The master object is asked if it is ok to shadow object ob. Use
 * previous_object() to find out who is asking.
 *
 * In this example, we allow shadowing as long as the victim object
 * hasn't denied it with a query_prevent_shadow() returning 1.
 */
int query_allow_shadow(object ob) {
    return !ob->query_prevent_shadow(previous_object());
}

/*
 * Default language functions used by parse_command() in non -o mode
 */
string *parse_command_id_list()
{
    return ({ "one", "thing" });
}

string *parse_command_plural_id_list()
{
    return ({ "ones", "things", "them" });
}

string *parse_command_adjectiv_id_list()
{
    return ({ "iffish" });
}

string *parse_command_prepos_list()
{
    return ({ "in", "on", "under", "behind", "beside" });
}

string parse_command_all_word()
{
    return "all";
}

/*
 * Give a file name for edit preferences to be saved in.
 */
string get_ed_buffer_save_file_name(string file) {
    string *file_ar;

    file_ar=explode(file,"/");
    file=file_ar[sizeof(file_ar)-1];
    return "/players/"+this_player()->query_real_name()+"/.dead_ed_files/"+file;
}

/*
 * Give a path to a simul_efun file. Observe that it is a string returned,
 * not an object. But the object has to be loaded here. Return 0 if this
 * feature isn't wanted.
 */
string get_simul_efun() {
    string fname;
    fname = "/obj/simul_efun";
    if (catch(call_other(fname, "??"))) {
	write("Failed to load " + fname + "\n");
	shutdown();
	return 0;
    }
    return fname;
}

int test_dir(string *dir, string path) {
    int i, len, max;

    if ( file_name(previous_object())[0..13] != "obj/simul_efun" ) {
        write("illegal call of test_dir\n");
	return 0;
    }
    for (i=0; i < sizeof(dir); i++) {
	if (file_size(path + "/" + dir[i]) == -2)
            dir[i]+="/";
        len = strlen(dir[i]);
        if (len >= max)
            max = len+1;
    }
    return max;
}

/*
 * There are several occasions when the game driver wants to check if
 * a player has permission to specific things.
 *
 * These types are implemented so far:
 * "error messages":	If the player is allowed to see runtime error
 *			messages.
 * "trace":		If the player is allowed to use tracing.
 * "wizard":		Is the player considered at least a "minimal" wizard ?
 * "error messages":	Is the player allowed to get run time error messages ?
 */
int query_player_level(string what) {
    int level;

    if (this_player() == 0)
	return 0;
    level = (int)this_player()->query_level();
    switch(what) {
    case "wizard":
	return level >= 20;
    case "error messages":
	return level >= 20;
    case "trace":
	return level >= 24;
    }
}

/*
 * Function name:   valid_exec
 * Description:     Checks if a certain 'program' has the right to use exec()
 * Arguments:       name: Name of the 'program' that attempts to use exec()
 *                        Note that this is different from file_name(),
 *                        Programname is what 'function_exists' returns.
 *                  NOTE, the absence of a leading slash in the name.
 * Returns:         True if exec() is allowed.
 */
int
valid_exec(string name)
{
    if (name == "secure/login.c")
	return 1;

    return 0;
}

