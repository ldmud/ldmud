#pragma strong_types

#define INIT_FILE "/room/init_file"

#include "/sys/wizlist.h"
#include "/sys/driver_hook.h"
#include "/sys/functionlist.h"
#include "/sys/erq.h"

#if !defined(__COMPAT_MODE__) && !defined(NO_NATIVE_MODE) && \
    !__EFUN_DEFINED__(creator) && defined(__EUIDS__)
#define NATIVE_MODE
#endif

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

/* For native mode, #define DEFAULT_REPLACE_PROGRAM if you want programs
 * that only redefined create() to be replaced automatically.
 */

#define BACKBONE_WIZINFO_SIZE 5
#define MUDWHO_INDEX 1
/* #define MUDWHO */

#ifdef MUDWHO
#define MUDWHO_SERVER "134.2.62.161"
#define MUDWHOPORT 6888
#define MUDWHO_NAME "TestNase"
#define MUDWHO_PASSWORD mudwho_password /* stored in /secure/mudwho-password */
#define MUDWHO_REFRESH_TIME 100
/* QUOTE_PERCENT needs new explode() to work right on leading/trailing '%' */
#define QUOTE_PERCENT(s) (implode(explode((s), "%"), "%%"))
string mudwho_ping;
private mapping mudwho_info = ([]);
closure send_mudwho_info;
private string mudwho_password;

void send_mudwho_info() {
    send_imp(MUDWHO_SERVER, MUDWHOPORT, sprintf(mudwho_ping, time()));
    walk_mapping(mudwho_info, send_mudwho_info);
    call_out("send_mudwho_info", MUDWHO_REFRESH_TIME);
}

void adjust_mudwho(object ob) {
    if (ob && interactive(ob)) {
        if (mudwho_info[ob][<5..] == "login") {
            mudwho_info[ob][<5..] = ob->query_real_name()[0..24];
            send_imp(MUDWHO_SERVER, MUDWHOPORT, mudwho_info[ob]);
        }
    }
}

void disconnect(object ob) {
    send_imp(MUDWHO_SERVER, MUDWHOPORT,
      "Z\t" + implode(explode(mudwho_info[ob], "\t")[1..4],"\t"));
}

#endif /* MUDWHO */

static void wiz_decay() {
    mixed *wl;
    int i;

    wl = wizlist_info();
    for (i=sizeof(wl); i--; ) {
        set_extra_wizinfo(wl[i][WL_NAME], wl[i][WL_EXTRA] * 99 / 100);
    }
    call_out("wiz_decay", 3600);
}

mixed creator_file(string obj_name);
string *define_include_dirs();

void inaugurate_master(int arg)
{
    if (!arg) {
        if (previous_object() && previous_object() != this_object())
            return;
        set_extra_wizinfo(0, allocate(BACKBONE_WIZINFO_SIZE));
    }
#ifdef MUDWHO
    MUDWHO_PASSWORD = read_file("/secure/mudwho-password", 3, 1)[0..<2];
    send_mudwho_info =
      lambda(({'key, 'info}), ({#'send_imp, MUDWHO_SERVER, MUDWHOPORT, 'info}));
    if (!arg) {
        send_imp(MUDWHO_SERVER, MUDWHOPORT,
          sprintf("U\t%.20s\t%.20s\t%.20s\t%:010d\t0\t%.25s",
            MUDWHO_NAME, MUDWHO_PASSWORD, MUDWHO_NAME,time(), __VERSION__[1..]
          )
        );
        get_extra_wizinfo(0)[MUDWHO_INDEX] = mudwho_info;
    } else {
        mudwho_info = get_extra_wizinfo(0)[MUDWHO_INDEX];
    }
    mudwho_ping = sprintf("M\t%.20s\t%.20s\t%.20s\t%%:010d\t0\t%.25s",
      QUOTE_PERCENT(MUDWHO_NAME),
      QUOTE_PERCENT(MUDWHO_PASSWORD),
      QUOTE_PERCENT(MUDWHO_NAME),
      QUOTE_PERCENT(__VERSION__[1..])
    );
    if (find_call_out("send_mudwho_info") < 0)
        call_out("send_mudwho_info", MUDWHO_REFRESH_TIME);
#endif /* MUDWHO */
    if (find_call_out("wiz_decay") < 0)
        call_out("wiz_decay", 3600);
    set_driver_hook(
      H_MOVE_OBJECT0,
      unbound_lambda( ({'item, 'dest}), ({#',,
#ifdef NATIVE_MODE
	({#'?, ({#'!=, 'item, ({#'this_object})}),
	  ({#'raise_error,
	    "Illegal to move other object than this_object()\n"}) }),
#endif
#ifdef COMPAT_FLAG
	({#'&&, ({#'living, 'item}), ({#'environment, 'item}), ({#',,
	  ({#'efun::set_this_player, 'item}),
	  ({#'call_other, ({#'environment, 'item}), "exit", 'item}),
	}) }),
#endif
	({#'efun::set_environment, 'item, 'dest}),
	({#'?, ({#'living, 'item}), ({#',,
	  ({#'efun::set_this_player, 'item}),
	  ({#'call_other, 'dest, "init"}),
	  ({#'?, ({#'!=, ({#'environment, 'item}), 'dest}), ({#'return})}),
	}) }),
	({#'=, 'others, ({#'all_inventory, 'dest}) }),
	({#'=, ({#'[, 'others, ({#'member, 'others, 'item}) }), 0}),
	({#'filter_array, 'others,
	  ({#'bind_lambda,
	    unbound_lambda( ({'ob, 'item}),
	      ({#'?, ({#'living, 'ob}), ({#',,
		({#'efun::set_this_player, 'ob}),
		({#'call_other, 'item, "init"}),
	      }) })
	    )
	  }),
	  'item,
	}),
	({#'?, ({#'living, 'item}), ({#',,
	  ({#'efun::set_this_player, 'item}),
	  ({#'filter_objects, 'others, "init"}),
	}) }),
	({#'?, ({#'living, 'dest}), ({#',,
	  ({#'efun::set_this_player, 'dest}),
	  ({#'call_other, 'item, "init"}),
	}) }),
      }) )
    );
#ifdef COMPAT_FLAG
    set_driver_hook(
      H_LOAD_UIDS,
      unbound_lambda( ({'obj_name}),
	({#'?,
	  ({#'==,
	    ({#'sscanf, 'obj_name, "players/%s", 'wiz_name}),
	    1,
	  }),
	  ({#'?,
	    ({#'==,
	      ({#'sscanf, 'wiz_name, "%s/%s", 'start, 'trailer}),
	      2,
	    }),
	    ({#'&&, ({#'strlen, 'start}), 'start}),
	    'wiz_name
	  }),
	  ({#'&&,
	    ({#'!=, ({#'[..], 'obj_name, 0, 3}), "ftp/"}),
	    ({#'!=, ({#'[..], 'obj_name, 0, 4}), "open/"}),
	  })
	})
      )
    );
    set_driver_hook(
      H_CLONE_UIDS,
      unbound_lambda( ({'blueprint, 'new_name}), ({
	#'||,
	  ({#'creator, 'blueprint}),
	  ({#'creator, ({#'previous_object})}),
	  1
      }) )
    );
    set_driver_hook(H_CREATE_SUPER, "reset");
    set_driver_hook(H_CREATE_OB,    "reset");
    set_driver_hook(H_CREATE_CLONE, "reset");
#else
#if __EFUN_DEFINED__(geteuid)
    /* the following closures illustrate how the 3.1.2 driver used to
     * set (e)uids. It should be usable for tests, but to get better
     * performance, you are encouraged to replace the function
     * calls to get_bb_uid() and creator_file() by the equivalent code,
     * even if you are satisfied with the current behaviour.
     */
    set_driver_hook(
      H_LOAD_UIDS,
      unbound_lambda( ({'obj_name}), ({
	#'?,
	({#'==,
	  ({#'=, 'creator_name, ({#'creator_file, 'obj_name})}),
	  ({#'getuid, ({#'previous_object})}),
	}),
	 ({#'geteuid, ({#'previous_object})}),
	({#'==, 'creator_name, ({#'get_bb_uid})}),
	 ({#'geteuid, ({#'previous_object})}),
	 ({#'({, 'creator_name, 1}),
      }) )
    );
    set_driver_hook(
      H_CLONE_UIDS,
      unbound_lambda( ({ /* object */ 'blueprint, 'new_name}), ({
	#'?,
	({#'==,
	  ({#'=, 'creator_name, ({#'creator_file, 'new_name})}),
	  ({#'getuid, ({#'previous_object})}),
	}),
	 ({#'geteuid, ({#'previous_object})}),
	({#'==, 'creator_name, ({#'get_bb_uid})}),
	 ({#'geteuid, ({#'previous_object})}),
	 ({#'({, 'creator_name, 1}),
      }) )
    );
#else /* no geteuid */
    set_driver_hook(H_LOAD_UIDS, #'creator_file);
    set_driver_hook(
      H_CLONE_UIDS,
      unbound_lambda( ({ /* object */ 'blueprint, 'new_name}), ({
	#'||,
	  ({#'?,
	    ({#'==,
	      ({#'=, 'creator_name, ({#'creator_file, 'new_name})}),
	      ({#'get_bb_uid}),
	    }),
	     ({#'getuid, ({#'previous_object})}),
	     'creator_name,
	  }),
	  1
	}) )
    );
#endif /* geteuid */
#ifdef NATIVE_MODE
    set_driver_hook(H_CREATE_OB,    "create");
#if !defined(DEFAULT_REPLACE_PROGRAM)
    set_driver_hook(H_CREATE_OB,    "create");
#else
    set_driver_hook(H_CREATE_OB,
      unbound_lambda(({'ob}), ({#',,
	  ({#'&&,
	    ({#'>, ({#'sizeof, ({#'=, 'list, ({#'inherit_list, 'ob})})}), 1}),
	    ({#'find_object, ({#'=, 'first, ({#'[, 'list, 1})})}),
	    ({#'==,
	      ({#'sizeof, 'list}),
	      ({#'+, 1,
		({#'sizeof, ({#'inherit_list, ({#'find_object, 'first})})})})}),
	    ({#'==, 1, ({#'sizeof, ({#'=, 'list,
		({#'functionlist, 'ob,
		  RETURN_FUNCTION_NAME|NAME_INHERITED})})})}),
	    ({#'==, ({#'[, 'list, 0}), "create"}),
#if 0
	    ({#',, ({#'printf, "replacing %O\n", 'ob}), 1}),
#endif
	    ({#'funcall,
	      ({#'funcall, #'bind_lambda, #'replace_program, 'ob}),
	      'first})
	  }),
          ({#'call_other, 'ob, "create"})
        })
      )
    );
#endif
    set_driver_hook(H_CREATE_CLONE, "create");
#else
    set_driver_hook(H_CREATE_SUPER,
      unbound_lambda(0, ({#',,
	  ({#'call_other, ({#'this_object}), "create"}),
	  ({#'call_other, ({#'this_object}), "reset"})
	})
      )
    );
    set_driver_hook(H_CREATE_OB,
      unbound_lambda(0, ({#',,
	  ({#'call_other, ({#'this_object}), "create"}),
	  ({#'call_other, ({#'this_object}), "reset"})
	})
      )
    );
    set_driver_hook(H_CREATE_CLONE,
      unbound_lambda(0, ({#',,
	  ({#'call_other, ({#'this_object}), "create"}),
	  ({#'call_other, ({#'this_object}), "reset"})
	})
      )
    );
#endif
#endif
    set_driver_hook(H_RESET,        "reset");
    set_driver_hook(H_CLEAN_UP,     "clean_up");
    set_driver_hook(H_MODIFY_COMMAND, ([
      "e":"east", "w":"west", "s":"south", "n":"north",
      "d":"down", "u":"up",
      "nw":"northwest", "ne":"northeast", "sw":"southwest", "se":"southeast",
    ]));
    set_driver_hook(H_MODIFY_COMMAND_FNAME, "modify_command");
    set_driver_hook(H_NOTIFY_FAIL, "What ?\n");
    set_driver_hook(H_INCLUDE_DIRS, map_array(
	define_include_dirs(),
	#'[..<],
	0, 3
    ));
}

int get_master_uid() {
    return 1;
}

mixed current_time;

string *epilog(int eflag) {
    if (eflag) return ({});
    debug_message(sprintf("Loading init file %s\n", INIT_FILE));
    current_time = rusage();
    current_time = current_time[0] + current_time[1];
    return explode(read_file(INIT_FILE), "\n");
}

void preload(string file) {
    int last_time;

    if (strlen(file) && file[0] != '#') {
        last_time = current_time;
        debug_message(sprintf("Preloading: %s", file));
        call_other(file, "");
        current_time = rusage();
        current_time = current_time[0] + current_time[1];
        debug_message(sprintf(" %.2f\n", (current_time - last_time)/1000.));
    }
}

void save_wiz_file() {
    rm("/WIZLIST");
    write_file(
      "/WIZLIST",
      implode(
        map_array(wizlist_info(),
          lambda(({'a}),
            ({#'sprintf, "%s %d %d\n",
              ({#'[, 'a, WL_NAME}),
              ({#'[, 'a, WL_COMMANDS}),
              ({#'[, 'a, WL_EXTRA})
            })
          )
        ), ""
      )
    );
}

void notify_shutdown() {
    if (previous_object() && previous_object() != this_object())
        return;
    filter_array(users(), #'tell_object,
      "Game driver shouts: LPmud shutting down immediately.\n");
    save_wiz_file();
#ifdef MUDWHO
    send_imp(MUDWHO_SERVER, MUDWHOPORT,
      sprintf("D\t%.20s\t%.20s\t%.20s",
        MUDWHO_NAME, MUDWHO_PASSWORD, MUDWHO_NAME
      )
    );
#endif /* MUDWHO */
}

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

    write("Demos says: Let's get a body for your character ...");
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
#ifdef MUDWHO
    mudwho_info[ob] =
      sprintf("A\t%.20s\t%.20s\t%.20s\t%.20s\t%:010d\t0\tlogin",
        MUDWHO_NAME, MUDWHO_PASSWORD, MUDWHO_NAME,
        explode(object_name(ob), "#")[<1] + "@" MUDWHO_NAME, time()
      )
    ;
#endif /* MUDWHO */
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

    if (sscanf(object_name(ob), "room/port_castle#%d", dummy) == 1
      || sscanf(object_name(ob), "global/port_castle#%d", dummy) == 1)
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
 * Write a compile time error message into a log file. The error occured in
 * the object 'file', giving the error message 'message'.
 */
void log_error(string file, string message)
{
    string name;

    name = get_wiz_name(file);
    if (name == 0)
	name = "log";
    write_file("/log/"+name, message);
}

int query_player_level(string what);

void runtime_error(string error,string program,string current_object, int line)
{
  if (this_player() && query_ip_number(this_player()))
    catch( write(
	query_player_level("error messages") ?
	    current_object ?
		error +
		"program: " + program +
		", object: " + current_object +
		" line " + line + "\n"
	    :
		error
	:
	    "Your sensitive mind notices a wrongness in the fabric of space.\n"
    ) );
}

int heart_beat_error(
    object heart_beat,
    string error,
    string program,
    string current_object,
    int line)
{
    if ( query_ip_number(heart_beat) ) {
	tell_object(
	  heart_beat,
	  "Game driver tells you: You have no heart beat !\n"
	);
    }
    return 0; /* Don't restart */
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

#if 1 /* Dedicated flags not used. A save file method is used below */

#define FIRST_ED_FLAG 0 /* adjust this value so that no flags are clobbered */
#define NUM_ED_FLAGS 12
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

    /* find_player() is a simul_efun. Resolve it at run time. */
    player = funcall(symbol_function('find_player),owner);
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
	tell_object(player, "You now have a home directory: " +
		    wizard + "\n");
	mkdir(wizard);
    }
    dest = object_name(environment(player));
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
    mixed error;

    if (!interactive(ob))
	return;
    tell_object(ob, "Everything you see is disolved. Luckily, you are transported somewhere...\n");
    if (error = catch(ob->move_player("is transfered#room/void"))) {
	write(error);
	if (error = catch(move_object(ob, "room/void"))) {
	    object new_player;

	    write(error);
	    new_player = clone_object("obj/player");
	    if (!function_exists("replace_player", new_player)) {
		destruct(new_player);
		return;
	    }
	    exec(new_player, ob);
	    if (error = catch(new_player->replace_player(ob, "room/void"))) {
		write(error);
	    }
	}
    }
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
string get_ed_buffer_save_object_name(string file) {
    string *file_ar;
    string path;

    path = "/players/"+this_player()->query_real_name()+"/.dead_ed_files";
    if (file_size(path) == -1) {
        mkdir(path);
    }
    file_ar=explode(file,"/");
    file=file_ar[sizeof(file_ar)-1];
    return path+"/"+file;
}

/* amylaar: allow to change simul_efun_file without editing patchlevel.h */
#ifndef SIMUL_EFUN_FILE
    /* the definition should be compatible with filename(). */
#define SIMUL_EFUN_FILE "obj/simul_efun"
#endif

#ifndef SPARE_SIMUL_EFUN_FILE
#define SPARE_SIMUL_EFUN_FILE "obj/spare_simul_efun"
#endif
/*
 * Give a path to a simul_efun file. Observe that it is a string returned,
 * not an object. But the object has to be loaded here. Return 0 if this
 * feature isn't wanted.
 */
string get_simul_efun() {
    string fname;
    string error;

    fname = SIMUL_EFUN_FILE ;
    if (error = catch(fname->start_simul_efun())) {
	write("Failed to load " + fname + "\n");
	write("error message :"+error+"\n");
	fname = SPARE_SIMUL_EFUN_FILE ;
	if (error = catch(fname->start_simul_efun())) {
	    write("Failed to load " + fname + "\n");
	    write("error message :"+error+"\n");
	    shutdown();
	    return 0;
	}
    }
    return fname;
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
 *                        Note that this is different from object_name(),
 *                        Programname is what 'function_exists' returns.
 *                  NOTE, the absence of a leading slash in the name.
 * Returns:         True if exec() is allowed.
 */
int
valid_exec(string name, object ob, object obfrom)
{
    switch(name) {
      case "secure/login.c":
      case "obj/master.c":
	if (!interactive(ob)) {
#ifdef MUDWHO
	    if (interactive(obfrom)) {
		mudwho_info[ob] = mudwho_info[obfrom];
		efun::m_delete(mudwho_info, obfrom);
		call_out("adjust_mudwho", 0, ob);
	    }
#endif /* MUDWHO */
	    return 1;
        }
    }

    return 0;
}

mixed valid_write(string path, string eff_user, string call_fun, object caller)
{
    string user;

    switch ( call_fun ) {
        case "save_object":
	    if ( user = creator(previous_object()) ) {
		if ( path[ 0 .. strlen(user)+7 ] == "players/" + user &&
		     sscanf(path, ".%s", user) == 0)
			return path;
	    } else {
		user = object_name(previous_object());
		if ( user[0..3] == "obj/"  ||
		     user[0..4] == "room/" ||
		     user[0..3] == "std/"  )
			return path;
	    }
        default:
            return 0; /* deny access */
	case "write_file":
            if (caller == this_object()) return 1;
	    if(path[0..4] == "/log/" && !(
	      sizeof(regexp(({path[5..34]}), "/")) ||
	      path[5] == '.' ||
	      strlen(path) > 35
	    ) ) {
	        return path;
	    }
	    break;
	case "ed_start":
	    if (path[0] != '/')
		path = "/"+path;
	    break;
	case "do_rename":
	    if((object_name(caller) == SIMUL_EFUN_FILE ||
		object_name(caller) == SPARE_SIMUL_EFUN_FILE) &&
	     path[0..4] == "/log/" && !(
	      sizeof(regexp(({path[5..34]}), "/")) ||
	      path[5] == '.' ||
	      strlen(path) > 35
	    ) ) {
		return 1;
	    }
	case "mkdir":
	case "rmdir":
	case "write_bytes":
	case "remove_file":
            if (caller == this_object()) return 1;
	case "cindent":
	    break;
	}
    set_this_object(caller);
    if( this_player() && query_ip_number(this_player()) ) {
	path = (string)this_player()->valid_write(path);
	if (!stringp(path)) {
	    write("Bad file name.\n");
	    return 0;
	}
	return path;
    }
    path = (string)"obj/player"->valid_write(path);
    if (stringp(path))
	return path;
    /* else return 0, denying access */
}

mixed valid_read(string path, string eff_user, string call_fun, object caller)
{
    string user;

    switch ( call_fun ) {
	case "restore_object": return 1;
	case "ed_start":
	    if ( previous_object() && previous_object() != this_player() )
		return 0;
	    if (!path) {
		/* request for file with last error */
		mixed *error;

		error =
		  get_error_file(({string})this_player()->query_real_name());
		if (!error || error[3]) {
		    write("No error.\n");
		    return 0;
		}
		write(error[0][1..]+" line "+error[1]+": "+error[2]+"\n");
		return error[0];
	    }
	    if (path[0] != '/')
		path = "/"+path;
	case "read_file":
	case "read_bytes":
	case "file_size":
	case "get_dir":
	case "do_rename":
	    if (caller == this_object()) return 1;
	case "tail":
	case "print_file":
	case "make_path_absolute": /* internal use, see below */
	    set_this_object(caller);
	    if( this_player() && query_ip_number(this_player()) ) {
	        path = (string)this_player()->valid_read(path);
	        if (!stringp(path)) {
	            write("Bad file name.\n");
	            return 0;
	        }
	        return path;
	    }
	    path = (string)"obj/player"->valid_read(path);
	    if (stringp(path))
	        return path;
	    return 0;
    }
    /* if a case failed to return a value or the caller function wasn't
     * recognized, we come here.
     * The default returned zero indicates deny of access.
     */
}

mixed make_path_absolute(string path) {
    return valid_read(path,0,"make_path_absolute", this_player());
}

mixed creator_file(string obj_name)
{
    string wiz_name;
    string start, trailer;

    if ( sscanf(obj_name, "players/%s", wiz_name) == 1 ) {
        if ( sscanf(wiz_name, "%s/%s", start, trailer) == 2 )
	    /* file names with // are illegal -> return 0 */
            return strlen(start) && start;
        return wiz_name;
    } else if (obj_name[0..3] != "ftp/" && obj_name[0..4] != "open/") {
        /* no creator, but legal. */
        return 1;
    }
}

void move_or_destruct(object what, object to)
/* An error in this function can be very nasty. Note that unlimited recursion
 * is likely to cause errors when environments are deeply nested
 */
{
#ifdef COMPAT_FLAG
    do {
        int res;
        if (catch( res = transfer(what, to) )) res = 5;
        if ( !(res && what) ) return;
    } while( (res == 1 || res == 4 || res == 5) && (to = environment(to)) );
#else /* !COMPAT_FLAG */
    if ( !catch( what->move(to, 1) ) ) return;
#endif /*COMPAT_FLAG */
    
    /*
     * Failed to move the object. Then, it is destroyed.
     */
    destruct(what);
}

int valid_snoop(object snooper, object snoopee) {
#ifdef NATIVE_MODE
    if (!geteuid(previous_object()))
	return 0;
#endif
    if (object_name(previous_object()) == get_simul_efun())
        return 1;
}

int valid_query_snoop(object wiz) {
    return this_player()->query_level() >= 22;
}

#ifdef COMPAT_FLAG
mixed *prepare_destruct1(object ob) {
    object super;

    super = environment(ob);
    if (super) {
	mixed error, *errors;
	mixed weight;
	object me;

	me = this_object();
	set_this_object(ob);
	errors = ({});
	if ( living(ob) ) {
	    if (error = catch(super->exit(ob),0))
		errors = ({"exit"+": "+error});
	}
	if ( error = catch((weight = (mixed)ob->query_weight()),0) ) {
	    set_this_object(me);
	    return ({"query_weight"+": "+error}) + errors;
	}
	if (weight && intp(weight)) {
	    if (error = catch(super->add_weight(-weight),0)) {
		set_this_object(me);
		return ({"add_weight"+": "+error}) + errors;
	    }
	}
	set_this_object(me);
    }
    return ({});
}
#endif /* COMPAT_FLAG */

mixed prepare_destruct(object ob) {
    object super;
    mixed *errors;
    int i;
    object sh, next;

#if 6 * 7 != 42 || 6 * 9 == 42
    return "Preprocessor error";
#endif

    if (!query_shadowing(ob)) for (sh = shadow(ob, 0); sh; sh = next) {
	next = shadow(sh, 0);
	funcall(bind_lambda(#'unshadow, sh)); /* avoid deep recursion */
	destruct(sh);
    }
#ifdef COMPAT_FLAG
    errors = prepare_destruct1(ob);
    for(i = sizeof(errors); i--; ) {
	write(errors[i]);
    }
#endif /* COMPAT_FLAG */
    super = environment(ob);
    if (!super) {
	object item;

	while ( item = first_inventory(ob) ) {
	    destruct_environment_of(item);
	    if (item && environment(item) == ob) destruct(item);
	}
    } else {
	while ( first_inventory(ob) )
	    move_or_destruct(first_inventory(ob), super);
    }
#ifdef MUDWHO
    if (interactive(ob)) disconnect(ob);
#endif /* MUDWHO */
    return 0; /* success */
}

/* privilege_violation is called when objects try to do illegal things,
 * or files being compiled request a privileged efun.
 *
 * return values: 
 *   1: The caller/file is allowed to use the privilege.
 *   0: The caller was probably misleaded; try to fix the error.
 *  -1: A real privilege violation. Handle it as error.
 */
int privilege_violation(string what, mixed who, mixed where, mixed how) {
    switch(what) {
      case "nomask simul_efun":
	if (who == SIMUL_EFUN_FILE || who == SPARE_SIMUL_EFUN_FILE) {
	    return 1;
	}
	return -1;
      case "set_extra_wizinfo":
      case "get_extra_wizinfo": /* needed in start_simul_efun() */
	if (who == find_object(SIMUL_EFUN_FILE) ||
	    who == find_object(SPARE_SIMUL_EFUN_FILE) )
	{
	    return 1;
	}
	return -1;
      case "erq":
	switch(how) {
	  case ERQ_RLOOKUP:
	    return 1;
	  case ERQ_EXECUTE:
	  case ERQ_FORK:
	  case ERQ_AUTH:
	  case ERQ_SPAWN:
	  case ERQ_SEND:
	  case ERQ_KILL:
          case ERQ_OPEN_UDP:
          case ERQ_OPEN_TCP:
          case ERQ_LISTEN:
          case ERQ_ACCEPT:
          case ERQ_LOOKUP:
	  default:
	    return -1;
	}
      default:
	return -1; /* Make this violation an error */
    }
}

void slow_shut_down(int minutes) {
    filter_array(users(), #'tell_object,
      "Game driver shouts: The memory is getting low !\n");
    "obj/shut"->shut(minutes);
}

void dangling_lfun_closure() {
    raise_error("dangling lfun closure\n");
}

void remove_player(object victim) {
    catch(victim->quit());
    if (victim)
	destruct(victim);
}
