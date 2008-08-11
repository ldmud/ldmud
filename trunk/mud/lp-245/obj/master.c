#pragma strong_types

/*  obj/master.c     (compat default)
**
** The master is the gateway between the gamedriver and the mudlib to perform
** actions with mudlib specific effects.
** Calls to the master by the gamedriver have an automatic catch() in effect.
**
** Note that the master is loaded first of all objects. Thus it is possible,
** you shouldn't inherit an other object (as most files expect the master
** to exist), nor is the compiler able to search include files
** (read: they must be specified with full path).
**
** This master was written specifically for the 2.4.5 compat mode mudlib.
** However it contains in comments marked with 'PLAIN' explanations how
** a non-compat master differs from a compat one.
*/

#include "/sys/wizlist.h"
#include "/sys/driver_hook.h"
#include "/sys/objectinfo.h"
#include "/sys/functionlist.h"
#include "/sys/erq.h"

#define INIT_FILE "/room/init_file"
#define BACKBONE_WIZINFO_SIZE 5
#define MUDWHO_INDEX 1
  /* Index of the mudwho information in the extra wizinfo */
#define SIMUL_EFUN_FILE "obj/simul_efun"
#define SPARE_SIMUL_EFUN_FILE "obj/spare_simul_efun"

#ifdef __COMPAT_MODE__
#    define ADD_SLASH(p) p
#    define GETUID(p) creator(p)
#    define TRANSFER(a,b) transfer(a,b)
#else
#    define ADD_SLASH(p) "/"+p
#    define GETUID(p) getuid(p)
#    define TRANSFER(a,b) funcall(symbol_function('transfer), a,b)
#endif

#ifdef MUDWHO
static void mudwho_init(int arg);
static void mudwho_connect (object ob);
static void mudwho_disconnect (object ob);
static void mudwho_shutdown();
static void mudwho_exec(object obfrom, object ob);
#else
#    define mudwho_init(arg)       0
#    define mudwho_connect(ob)     0
#    define mudwho_disconnect(ob)  0
#    define mudwho_shutdown()      0
#    define mudwho_exec(a,b)       0
#endif

void save_wiz_file(); // forward
int query_player_level (string what); // forward

//===========================================================================
// Compatmode compat functions
//
// The functions here adapt a handful of efuns which differ between compat
// and plain mode. They are a subset of the functions in simul_efun.c
//===========================================================================

#ifndef __COMPAT_MODE__

//---------------------------------------------------------------------------
string object_name(object ob)
{
    string rc;

    rc = efun::object_name(ob);
    return stringp(rc) ? rc[1..] : 0;
}

#endif /* __COMPAT_MODE__ */

//===========================================================================
//  Hooks
//
// These functions are set as driver hooks to perform low-level functions
// for the mud.
//===========================================================================

//---------------------------------------------------------------------------
static string _include_dirs_hook (string include_name, string current_file)

// Return the full pathname of an include file.
//
// Argument:
//   include_name: the name given in the #include <...> directive.
//   current_file: the filename of the file compiled.
//
// Result:
//   The full pathname of the include file.
//   0 if no such file exists.
//
// If include_name can't be found as such, the function looks in /sys
// and /room for /sys/<include_name> resp. /room/<include_name>.

{
  string name, part;
  int pos;

  if (file_size(ADD_SLASH(include_name)) >= 0)
    return include_name;
  name = "sys/"+include_name;
  if (file_size(ADD_SLASH(name)) >= 0)
    return name;
  name = "room/"+include_name;
  if (file_size(ADD_SLASH(name)) >= 0)
    return name;
  return 0;
}


//---------------------------------------------------------------------------
static void _move_hook_fun (object item, object dest)

// Move object <item> into object <dest>.
//
// Argument:
//   item: the object to be moved.
//   dest: the destination for the object.
//
// The function performs all the checks for a valid move (item exists,
// destination exists, destination is not current environment, etc).
// In addition, it implements the init() protocol needed for add_action()
// to work.

{
  object *others;
  int i;
  string name;

  /* PLAIN:
  if (item != this_object)
      raise_error("Illegal to move other object than this_object()\n");
  */

  if (living(item) && environment(item))
  {
      name = query_once_interactive(item) ? item->query_real_name()
                                          : object_name(item);
      /* PLAIN: the call to exit() is needed in compat mode only */
      efun::set_this_player(item);
      object env = environment(item);
      env->exit(item);
      if (!item)
          raise_error(sprintf("%O->exit() destructed item %s before move.\n"
                             , env, name));
  }
  else
      name = object_name(item);

  /* This is the actual move of the object. */

  efun::set_environment(item, dest);

  /* Moving a living object will cause init() to be called in the new
   * environment.
   */
  if (living(item)) {
    efun::set_this_player(item);
    dest->init();
    if (!item)
      raise_error(sprintf("%O->init() destructed moved item %s\n", dest, name));
    if (environment(item) != dest)
      return;
  }

  /* Call init() in item once foreach living object in the new environment
   * but only if the item is (still) in the same environment.
   */
  others = all_inventory(dest) - ({ item });
  foreach (object obj : others)
  {
    if (living(obj) && environment(obj) == environment(item)) {
      efun::set_this_player(obj);
      item->init();
    }
    if (!item)
      raise_error(sprintf("item->init() for %O destructed moved item %s\n", obj, name));
  }

  /* Call init() in each of the  objects themselves, but only if item
   * didn't move away already.
   */
  if (living(item)) {
    foreach (object obj : others)
    {
        efun::set_this_player(item); // In case something new was cloned
        if (environment(obj) == environment(item))
            obj->init();
    }
  }

  /* If the destination is alive as well, call item->init() for it. */
  if (living(dest) && item && environment(item) == dest) {
    efun::set_this_player(dest);
    item->init();
  }
}

//---------------------------------------------------------------------------
static mixed _load_uids_fun (mixed object_name, object prev)

// Return the uids given to an object just loaded.
//
// Argument:
//   object_name: name of the object loaded
//   prev       : loading object.
//
// Result:
//   The uid to give to the object. For objects /players/<name>/xxx
//   it is "<name>", for everything else it is 1 which translates
//   into the 0-uid (which is actually the backbone-uid).
//
// In general, the function can have these results (<num> is a non-zero
// number):
//   "<uid>"                     -> uid = "<uid>", euid = "<uid>"
//   ({ "<uid>", "<euid>" })     -> uid = "<uid>", euid = "<euid>"
//   ({ "<uid>", not-a-string }) -> uid = "<uid>", euid = 0
//
// If strict-euids is not set, the following results are possible, too:
//   <num>                       -> uid = 0, euid = 0
//   ({ <num>, "<euid>" })       -> uid = 0, euid = "<euid>"
//   ({ <num>, not-a-string })   -> uid = 0, euid = 0

{
  string * parts;

  parts = explode(object_name, "/");
  if (sizeof(parts) > 2 && parts[0] == "players")
      return parts[1];
  return 1;
}

#if 0
// PLAIN: The following code fragment is used in OSB to give 'backbone'
// objects (e.g. objects from /obj, /room) the uids of the loader.  Other
// objects get their uid as normal, but a 0 euid.  One important effect is
// that when user A loads his own objects, the come with a valid euid, but
// other users objects come with a 0 euid.

{
  string creator_name;

  creator_name = get_wiz_name(object_name);
  if (prev && creator_name == getuid(prev))
    return geteuid(prev);
  if (prev && creator_name == get_bb_uid())
    return geteuid(prev);
  return ({ creator_name, 1 });
}
#endif

//---------------------------------------------------------------------------
static mixed _clone_uids_fun (object blueprint, string new_name, object prev)

// Return the uids given to an object just cloned.
//
// Argument:
//   blueprint: the blueprint cloned.
//   new_name : name of the object cloned
//   prev     : cloning object.
//
// Result:
//   The uids to give.
//   This is either, the uid of the blueprint, or the uid of the previous
//   object, or 1 (tested in this order).
//
// The possible results in general are the same as for _load_uids_fun().

{
  string creator_name;

  return GETUID(blueprint) || GETUID(prev) || 1;
}

#if 0
// PLAIN: The following code fragment is used in OSB to give 'backbone'
// objects (e.g. objects from /obj, /room) the uids of the cloner.  Other
// objects get their uid as normal, but a 0 euid.  One important effect is
// that when user A clones his own objects, the come with a valid euid, but
// other users objects come with a 0 euid.

{
  string creator_name;

  creator_name = get_wiz_name(new_name);
  if (prev && creator_name == getuid(prev))
    return geteuid(prev);
  if (prev && creator_name == get_bb_uid())
    return geteuid(prev);
  return ({ creator_name, 1 });
}
#endif

//===========================================================================
//  Initialisation
//
// These functions are called after (re)loading the master to establish the
// most basic operation parameters.
//
// The initialisation of LPMud on startup follows this schedule:
//   - The gamedriver evaluates the commandline options and initializes
//     itself.
//   - The master is loaded, but since the driverhooks are not set yet,
//     no standard initialisation lfun is called.
//   - get_master_uid() is called. If the result is valid, it becomes the
//     masters uid and euid.
//   - inaugurate_master() is called.
//   - flag() is called for each given '-f' commandline option.
//   - get_simul_efun() is called.
//   - the WIZLIST is read in.
//   - epilog() is called. If it returns an array of strings, they are given
//     one at a time as argument to preload().
//     Traditionally, these strings are the filenames of the objects to
//     preload, read from /room/init_file, which preload() then does.
//   - The gamedriver sets up the IP communication and enters the backend
//     loop.
//
// If the master is reloaded during the game, this actions are taken:
//   - The master is loaded, and its initialisation lfun is called according
//     to the settings of the driverhooks (if set).
//   - Any auto-include string and all driverhooks are cleared.
//   - get_master_uid() is called. If the result is valid, it becomes the
//     masters uid and euid.
//   - inaugurate_master() is called.
//
// If the master was destructed, but couldn't be reloaded, the old
// master object could be reactivated. In that case:
//   - reactivate_destructed_master() is called.
//   - inaugurate_master() is called.
//===========================================================================

//---------------------------------------------------------------------------
// Initialization of the master object.
//
// As the lfuns which are called to initialize objects after a load are
// defined through driver hooks, and these hooks are cleared prior to
// a master (re)load, the first function called is inaugurate_master().
// Anyway it's not very sensible to do anything earlier as the master is
// not recognized as such at that time, and so a number of (important) things
// would not work.
//
// Which lfun is called during runtime to reset the master is also depending
// on the driverhook settings. Arbitrary actions may be done on a reset.

//---------------------------------------------------------------------------
void inaugurate_master (int arg)

// Perform mudlib specific setup of the master.
//
// Argument:
//   arg: 0 if the mud just started.
//        1 if the master is reactivated destructed one.
//        2 if the master is a reactivated destructed one, which lost all
//             variables.
//        3 if the master was just reloaded.
//
// This function is called whenever the master becomes fully operational
// after (re)loading (it is now recognized as _the_ master).
// This doesn't imply that the game is up and running.
//
// This function has at least to set up the driverhooks to use. Also, any
// mudwho or wizlist handling has to be initialized here.
//
// Besides that, do whatever you feel you need to do,
// e.g. set_driver_hook(), or give the master a decent euid.

{
    if (!arg) {
        if (previous_object() && previous_object() != this_object())
            return;
        set_extra_wizinfo(0, allocate(BACKBONE_WIZINFO_SIZE));
    }

    mudwho_init(arg);

  // Wizlist simulation
  if (find_call_out("wiz_decay") < 0)
    call_out("wiz_decay", 3600);

  set_driver_hook(
        H_MOVE_OBJECT0,
        unbound_lambda( ({'item, 'dest}),
        ({#'_move_hook_fun, 'item, 'dest })
                      )
                 );
  set_driver_hook(
    H_LOAD_UIDS,
    unbound_lambda( ({'object_name}), ({
      #'_load_uids_fun, 'object_name, ({#'previous_object}) })
                  )
  );
  set_driver_hook(
    H_CLONE_UIDS,
    unbound_lambda( ({ /* object */ 'blueprint, 'new_name}), ({
      #'_clone_uids_fun, 'blueprint, 'new_name, ({#'previous_object}) })
                  )
  );
  set_driver_hook(H_CREATE_SUPER, "reset");
  set_driver_hook(H_CREATE_OB,    "reset");
  set_driver_hook(H_CREATE_CLONE, "reset");
    /* PLAIN: Non-compat muds like OSB use "create" or other functions
     * for the above.
     */
  set_driver_hook(H_RESET,        "reset");
  set_driver_hook(H_CLEAN_UP,     "clean_up");
  set_driver_hook(H_MODIFY_COMMAND,
    ([ "e":"east", "w":"west", "s":"south", "n":"north"
     , "d":"down", "u":"up", "nw":"northwest", "ne":"northeast"
     , "sw":"southwest", "se":"southeast" ]));
  set_driver_hook(H_MODIFY_COMMAND_FNAME, "modify_command");
  set_driver_hook(H_NOTIFY_FAIL, "What?\n");
  set_driver_hook(H_INCLUDE_DIRS, #'_include_dirs_hook);
}

//---------------------------------------------------------------------------
mixed get_master_uid ()

// Return the value to be used as uid (and -euid) of a (re)loaded master.
//
// Possible results are in general:
//
//     "<uid"> -> uid = "<uid>", euid = "<euid>"
//
// In non-strict-euids mode, more results are possible:
//
//     0       -> uid = 0, euid = 0
//     <num>   -> uid = 'default', euid = 0
//
// If your uids are in general based on filenames, it is wise to return
// a value here which can not be legally generated from any filename.
// OSB for example uses 'ze/us'.

{
    return 1;
}

//---------------------------------------------------------------------------
void flag (string arg)

// Evaluate an argument given as option '-f' to the driver.
//
// Arguments:
//   arg: The argument string from the option text '-f<arg>'.
//        If several '-f' options are given, this function
//        will be called sequentially with all given arguments.
//
// This function can be used to pass the master commands via arguments to
// the driver. This is useful when building a new mudlib from scratch.
// It is called only when the game is started.
//
// The code given implements these commands:
//   '-fcall <ob> <fun> <arg>': call function <fun> in object <ob> with
//                              argument <arg>.
//   '-fshutdown': shutdown the game immediately.
// Thus, starting the game as 'parse "-fcall foo bar Yow!" -fshutdown' would
// first do foo->bar("Yow!") and then shutdown the game.

{
  string obj, fun, rest;

  if (arg == "shutdown")
  {
    shutdown();
    return;
  }
  if (sscanf(arg, "call %s %s %s", obj, fun, rest) >= 2)
  {
    write(obj+"->"+fun+"(\""+rest+"\") = ");
    write(call_other(obj, fun, rest));
    write("\n");
    return;
  }
  write("master: Unknown flag "+arg+"\n");
}

//---------------------------------------------------------------------------
static mixed current_time;
  /* Saved start time of epilog, updated during preloading */

string *epilog (int eflag)

// Perform final actions before opening the game to players.
//
// Arguments:
//   eflag: This is the number of '-e' options given to the parser.
//          Normally it is just 0 or 1.
//
// Result:
//   An array of strings, which traditionally designate the objects to be
//   preloaded with preload(), read from the file /room/init_file.
//
//   Any other result is interpreted as 'no object to preload'.
//   The resulting strings will be passed one at the time as
//   arguments to preload().

{

    if (eflag)
        return ({});

    debug_message(sprintf("Loading init file %s\n", INIT_FILE));
    current_time = rusage();
    current_time = current_time[0] + current_time[1];
    return explode(read_file(INIT_FILE), "\n");
}

//---------------------------------------------------------------------------
void preload (string file)

// Preload a given object.
//
// Arguments:
//   file: The filename of the object to preload, as returned by epilog().
//
// It is task of the epilog()/preload() pair to ensure the validity of
// the given strings (e.g. filtering out comments and blank lines).
// For preload itself a 'load_object(file)' is sufficient, but it
// should be guarded by a catch() to avoid premature blockings.
// Also it is wise to change the master's euid from master_uid to something
// less privileged for the time of the preload.
//
// You can of course do anything else with the passed strings - preloading
// is just the traditional task.

{
    int last_time;

    if (strlen(file) && file[0] != '#')
    {
        last_time = current_time;
        debug_message(sprintf("Preloading file: %s", file));
        load_object(file);
        current_time = rusage();
        current_time = current_time[0] + current_time[1];
        debug_message(sprintf(" %.2f\n", (current_time - last_time) / 1000.0));
    }
}

//---------------------------------------------------------------------------
//void external_master_reload ()

// Master was reloaded on external request by SIGUSR1.
//
// If the gamedriver destruct and reloads the master on external request
// via SIGUSR1, it does this by a call to this function.
// It will be called after inaugurate_master() of course.
// If you plan to do additional magic here, you're welcome.


//---------------------------------------------------------------------------
//void reactivate_destructed_master (int removed)

// Reactivate a formerly destructed master.
//
// Arguments:
//   removed: True if the master was already on the list of destructed
//            objects.
//
// This function is called in an formerly destructed master since a new master
// couldn't be loaded.
// This function has to reinitialize all variables at least to continue
// operation.


//---------------------------------------------------------------------------
mixed get_simul_efun ()

// Load the simul_efun object(s) and return one or more paths of it.
//
// Result:
//   Either a single string with the object_name() of the simul_efun object,
//   or an array of strings which has to start with that object_name().
//   Return 0 if this feature isn't wanted.
//
// Note that the object(s) must be loaded by this function!
//
// When you return an array of strings, the first string is taken as path
// to the simul_efun object, and all other paths are used for backup
// simul_efun objects to call simul_efuns that are not present in the
// main simul_efun object. This allows to remove simul_efuns at runtime
// without getting errors from old compiled programs that still use the
// obsolete simul_efuns. A side use of this mechanism is to provide
// a 'spare' simul_efun object in case the normal one fails to load.
//
// If the game depends on the simul_efun object, and none could be loaded,
// an immediate shutdown should occur.

{
  mixed error;
  object ob;

  error = catch(ob = load_object(SIMUL_EFUN_FILE));
  if (!error)
  {
    ob->start_simul_efun();
    return SIMUL_EFUN_FILE;
  }
  efun::write("Failed to load " + SIMUL_EFUN_FILE + ": "+error);
  error = catch(ob = load_object(SPARE_SIMUL_EFUN_FILE));
  if (!error)
  {
    ob->start_simul_efun();
    return SPARE_SIMUL_EFUN_FILE;
  }
  efun::write("Failed to load " + SPARE_SIMUL_EFUN_FILE + ": "+error);
  efun::shutdown();
  return 0;
}

//===========================================================================
//  Handling of player connections
//
// See also valid_exec().
//===========================================================================

//---------------------------------------------------------------------------
object connect ()

// Handle the request for a new connection.
//
// Result:
//   An login object the requested connection should be bound to,
//   for us a copy of obj/player.c .
//
// Note that the connection is not bound yet!
//
// The gamedriver will call the lfun 'logon()' in the login object after
// binding the connection to it. That lfun has to return !=0 to succeed.

{
    object ob;
    string ret;

    write("Lars says: Let's get a body for your character ...");
    ob = clone_object("obj/player");
    write("\n");
    if (ret) {
	write(ret + "\n");
	return 0;
    }
    mudwho_connect(ob);
    return ob;
}

//---------------------------------------------------------------------------
void disconnect (object obj)

// Handle the loss of an IP connection.
//
// Argument:
//   obj: The (formerly) interactive object (player).
//
// This called by the gamedriver to handle the removal of an IP connection,
// either because the connection is already lost ('netdeath') or due to
// calls to exec() or remove_interactive().
// The connection will be unbound upon return from this call.

{
    mudwho_disconnect(ob);
}

//---------------------------------------------------------------------------
void remove_player (object player)

// Remove a player object from the game.
//
// Argument:
//   player: The player object to be removed.
//
// This function is called by the gamedriver to expell remaining players
// from the game on shutdown in a polite way.
// If this functions fails to quit/destruct the player, it will be
// destructed the hard way by the gamedriver.
//
// Note: This function must not cause runtime errors.

{
    catch(player->quit());
    if (player)
	destruct(player);
}

//---------------------------------------------------------------------------
void stale_erq (closure callback)

// Notify the loss of the erq demon.
//
// Argument:
//   callback: the callback closure set for an erq request.
//
// If the erq connection dies prematurely, the driver will call this lfun for
// every pending request with set callback. This function should notify the
// originating object that the answer will never arrive.
//
// In our case, we simply reattach the default erq.

{
  attach_erq_demon("", 0);
}


//===========================================================================
//  Runtime Support
//
// Various functions used to implement advanced runtime features.
//===========================================================================

//---------------------------------------------------------------------------
object compile_object (string filename)

// Compile an virtual object.
//
// Arguments:
//   previous_object(): The object requesting the virtual object.
//   filename         : The requested filename for the virtual object.
//
// Result:
//   The object to serve as the requested virtual object, or 0.
//
// This function is called if the compiler can't find the filename for an
// object to compile. The master has now the opportunity to return an other
// which will then serve as if it was compiled from <filename>.
// If the master returns 0, the usual 'Could not load'-error will occur.
//
// The function will try several possible VMasters in sequence, calling
// compile_object(<filename>) in each of them, until on of them returns
// an object. The objects tried in order (if existing) are:
//  1. <path_of_filename>/vmaster.c
//  2  <path_of_filename>.c
//    In both cases, only the basename part of <filename> is passed
//    to compile_object().

{
  object obj, room;
  mixed vmaster;
  string filepath;

  if (filename[0] != '/') filename = "/"+filename;
  filepath = implode(explode(filename,"/")[0..<2],"/");
  vmaster = filepath+"/vmaster";
  if (0 <= file_size(vmaster+".c"))
    room = (object)vmaster->compile_object(explode(filename,"/")[<1]);
  if (!room && 0 <= file_size(filepath+".c"))
    room = (object)filepath->compile_object(explode(filename,"/")[<1]);
  return room;
}


//---------------------------------------------------------------------------
string get_wiz_name (string file)

// Return the author of a file.
//
// Arguments:
//   file: The name of the file in question.
//
// Result:
//   The name of the file's author (or 0 if there is none).
//
// This function is called for maintenance of the wiz-list, to score errors
// to the right wizard.

{
    string name, rest;

    if (sscanf(file, "players/%s/%s", name, rest) == 2) {
	return name;
    }
    return 0;
}

//---------------------------------------------------------------------------
// string printf_obj_name (object obj)

// Return a printable name for an object.
//
// Arguments:
//   obj: The object which name is of interest.
//
// Result:
//   A string with the objects name, or 0.
//
// This function is called by sprintf() to print a meaningful name
// in addition to the normal object_name().
// If this functions returns a string, the object will be printed
// as "<obj_name> (<printf_obj_name>)".


//---------------------------------------------------------------------------
void destruct_environment_of(object ob)

/* When an object is destructed, this function is called with every
 * item in that room. We get the chance to save players !
 */

{
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

//---------------------------------------------------------------------------
void move_or_destruct(object what, object to)

/* Move <what> into <to>, or destruct <what> if that is not possible.
 *
 * An error in this function can be very nasty. Note that unlimited recursion
 * is likely to cause errors when environments are deeply nested
 */

{
    int res;

    /* PLAIN: the following loop is for compat mode only */
    do {
        if (catch( res = TRANSFER(what, to) )) res = 5;
        if ( !(res && what) ) return;
    } while( (res == 1 || res == 4 || res == 5) && (to = environment(to)) );
    /* PLAIN: native muds make this
    if (!catch(what->move(to, 1)))
        return;
    */

    /*
     * Failed to move the object. Therefore it is destroyed.
     */
    destruct(what);
}

//---------------------------------------------------------------------------
private int
handle_super_compat (object super, object ob)

/* For compat muds: handle the weight handling in the environment for
 * prepare_destruct().
 * Return non-0 if an error occured, and 0 if not.
 */

{
    if (super)
    {
	mixed error;
	mixed weight;

	set_this_object(ob);
	if ( living(ob) ) {
	    if (error = catch(super->exit(ob),0))
		write("exit"+": "+error);
	}
	if ( error = catch((weight = (mixed)ob->query_weight()),0) ) {
	    write("query_weight"+": "+error);
            return 1;
	}
	if (weight && intp(weight)) {
	    if (error = catch(super->add_weight(-weight),0)) {
		write("add_weight"+": "+error);
                return 1;
	    }
	}
    }

    return 0;
}

//---------------------------------------------------------------------------
mixed prepare_destruct (object ob)

// Prepare the destruction of the given object.
//
// Argument:
//   obj : The object to destruct.
//
// Result:
//   Return 0 if the object is ready for destruction, any other value
//   will abort the attempt.
//   If a string is returned, an error with the string as message will
//   be issued.
//
// The gamedriver calls this function whenever an object shall be destructed.
// It expects, that this function cleans the inventory of the object, or
// the destruct will fail.
// Furthermore, the function could notify the former inventory objects that
// their holder is under destruction (useful to move players out of rooms which
// are updated); and it could announce mudwide the destruction(quitting) of
// players.

{
    object super;
    int i;
    object sh, next;

    /* Remove all pending shadows */
    if (!query_shadowing(ob)) for (sh = shadow(ob, 0); sh; sh = next) {
	next = shadow(sh, 0);
	funcall(bind_lambda(#'unshadow, sh)); /* avoid deep recursion */
	destruct(sh);
    }

    super = environment(ob);

    /* PLAIN: This whole if (super) {...} block is for compat muds only */
    if (super) {
        if (funcall(#'handle_super_compat, super, ob))
            return;
    }
    /* PLAIN: end of compat-mud block */

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

    if (interactive(ob))
        disconnect(ob);

    return 0; /* success */
}


//---------------------------------------------------------------------------
//void quota_demon (void)

// Handle quotas in times of memory shortage.
//
// This function is called during the final phase of a garbage collection if
// the reserved user area couldn't be reallocated. This function (or a called
// demon) has now the opportunity to remove some (still active) objects from
// the game. If this does not free enough memory to reallocate the user
// reserve, slow_shut_down() will be called to start Armageddon.
//
// Note: Up to now, the wizlist lacks various informations needed to detect
//   the memory-hungriest wizards.


//---------------------------------------------------------------------------
//void receive_imp (string host, string msg, int port)

// Handle a received IMP message.
//
// Arguments:
//   host: Name of the host the message comes from.
//   msg : The received message.
//   port: the port number from which the message was sent.
//
// This function is called for every message received on the IMP port.


//---------------------------------------------------------------------------
void slow_shut_down (int minutes)

// Schedule a shutdown for the near future.
//
// Argument:
//   minutes: The desired time in minutes till the shutdown:
//             6, if just the user reserve has been put into use;
//             1, if the (smaller) system or even the master reserve
//                has been put into use as well.
//
// The gamedriver calls this function when it runs low on memory.
// At this time, it has freed its reserve, but since it won't last long,
// the games needs to be shut down. Don't take the 'minutes' as granted
// remaining uptime, just deduce the urgency of the shutdown from it.
// The delay is to give the players the opportunity to finish quests,
// sell their stuff, etc.
// It is possible that the driver may reallocate some memory after the
// function has been called, and then run again into a low memory situation,
// calling this function again.
//
// In our case, this function loads an 'Armageddon' object and tells
// it what to do. It is the Armageddon object then which performs
// the shutdown.
//
// Technical:
//   The memory handling of the gamedriver includes three reserved areas:
//   user, system and master. All three are there to insure that the game
//   shuts down gracefully when the memory runs out: the user area to give
//   the players time to quit normally, the others to enable emergency-logouts
//   when the user reserve is used up as well.
//   The areas are allocated at start of the gamedriver, and released when
//   no more memory could be obtained from the host. In such a case, one
//   of the remaining areas is freed (so the game can continue a short
//   while) and a garbagecollection is initiated.
//   If the garbagecollection recycles enough memory (either true garbage
//   or by the aid of the quota_demon) to reallocate the areas, all is
//   fine, else the game is shut down by a call to this function.

{
    filter(users(), #'tell_object,
      "Game driver shouts: The memory is getting low !\n");
    "obj/shut"->shut(minutes);
}

//---------------------------------------------------------------------------
varargs void notify_shutdown (string crash_reason)

// Notify the master about an immediate shutdown. If <crash_reason> is 0,
// it is a normal shutdown, otherwise it is a crash and <crash_reason>
// gives a hint at the reason.
//
// The function has the opportunity to perform any cleanup operation, like
// informing the mudwho server that the mud is down. This can not be done
// when remove_player() is called because the udp connectivity is already
// gone then.
//
// If the gamedriver shuts down normally , this is the last function called
// before the mud shuts down the udp connections and the accepting socket
// for new players.
//
// If the gamedriver crashes, this is the last function called before the
// mud attempts to dump core and exit. WARNING: Since the driver is in
// an unstable state, this function may not be able to run to completion!
// The following crash reasons are defined:
//   "Fatal Error": an internal sanity check failed.

{
    if (previous_object() && previous_object() != this_object())
        return;
    if (!crash_reason)
        filter(users(), #'tell_object,
          "Game driver shouts: LPmud shutting down immediately.\n");
    else
        filter(users(), #'tell_object,
          "Game driver shouts: PANIC! "+ crash_reason+"!\n");
    save_wiz_file();
    mudwho_shutdown();
}

//===========================================================================
//  Error Handling
//
//===========================================================================

//---------------------------------------------------------------------------
void dangling_lfun_closure ()

// Handle a dangling lfun-closure.
//
// This is called when the gamedriver executes a closure using a vanished lfun.
// A proper handling is to raise a runtime error.
//
// Technical:
//   Upon replacing programs (see efun replace_program()), any existing
//   lambda closures of the object are adjusted to the new environment.
//   If a closure uses a lfun which vanished in the replacement process,
//   the reference to this lfun is replaced by a reference to this function.
//   The error will then occur when the execution of the adjusted lambda
//   reaches the point of the lfun reference.
//   There are two reasons for the delayed handling. First is that the
//   program replacement and with it the closure adjustment happens at
//   the end of a backend cycle, outside of any execution thread: noone
//   would see the error at this time.
//   Second, smart closures might know/recognize the program replacement
//   and skip the call to the vanished lfun.

{
  raise_error("dangling lfun closure\n");
}

//---------------------------------------------------------------------------
void log_error (string file, string err)

// Announce a compiler-time error.
//
// Arguments:
//   file: The name of file containing the error (it needn't be an object
//         file!).
//   err : The error message.
//
// Whenever the LPC compiler detects an error, this function is called.
// It should at least log the error in a file, and also announce it
// to the active player if it is an wizard.

{
    string name;

    name = get_wiz_name(file);
    if (name == 0)
	name = "log";
    write_file("/log/"+name, err);
}

//---------------------------------------------------------------------------
mixed heart_beat_error (object culprit, string err,
                        string prg, string curobj, int line)

// Announce an error in the heart_beat() function.
//
// Arguments:
//   culprit: The object which lost the heart_beat.
//   err    : The error message.
//   prg    : The executed program (might be 0).
//   curobj : The object causing the error (might be 0).
//   line   : The line number where the error occured (might be 0).
//
// Result:
//   Return anything != 0 to restart the heart_beat in culprit.
//
// This function has to announce an error in the heart_beat() function
// of culprit.
// At time of call, the heart_beat has been turned off.
// A player should at least get a "You have no heartbeat!" message, a more
// advanced handling would destruct the offending object and allow the
// heartbeat to restart.
//
// Note that <prg> denotes the program actually executed (which might be
// inherited one) whereas <curobj> is just the offending object.

{
    if ( query_ip_number(culprit) ) {
	tell_object(
	  culprit,
	  "Game driver tells you: You have no heart beat !\n"
	);
    }
    return 0; /* Don't restart */
}

//---------------------------------------------------------------------------
void runtime_error ( string err, string prg, string curobj, int line
                   , mixed culprit)

// Announce a runtime error.
//
// Arguments:
//   err    : The error message.
//   prg    : The executed program.
//   curobj : The object causing the error.
//   line   : The line number where the error occured.
//   culprit: -1 for runtime errors; the object holding the heart_beat()
//            function for heartbeat errors.
//
// This function has to announce a runtime error to the active user,
// resp. handle a runtime error which occured during the execution of
// heart_beat() of <culprit>.
//
// For a normal runtime error, if the active user is a wizard, it might
// give him the full error message together with the source line; if the
// user is a is a player, it should issue a decent message ("Your sensitive
// mind notices a wrongness in the fabric of space") and could also announce
// the error to the wizards online.
//
// If the error is a heartbeat error, the heartbeat for the offending
// <culprit> has been turned off. The function itself shouldn't do much, since
// the lfun heart_beat_error() will be called right after this one.
//
// Note that <prg> denotes the program actually executed (which might be
// inherited) whereas <curobj> is just the offending object for which the
// program was executed.

{
  if (this_player() && query_ip_number(this_player()))
    catch( write(
	query_player_level("error messages") ?
	    curobj ?
		err +
		"program: " + prg +
		", object: " + curobj +
		" line " + line + "\n"
	    :
		err
	:
	    "Your sensitive mind notices a wrongness in the fabric of space.\n"
    ) );
}

//===========================================================================
//  Security and Permissions
//
// Most of these functions guard critical efuns. A good approach to deal
// with them is to redefine the efuns by simul_efuns (which can then avoid
// trouble prematurely) and give root objects only the permission to
// execute the real efuns.
//
// See also valid_read() and valid_write().
//===========================================================================

//---------------------------------------------------------------------------
int privilege_violation (string op, mixed who, mixed arg, mixed arg2)

// Validate the execution of a privileged operation.
//
// Arguments:
//   op   : the requestion operation (see below)
//   who  : the object requesting the operation (filename or object pointer)
//   arg  : additional argument, depending on <op>.
//   arg2 : additional argument, depending on <op>.
//
// Result:
//     >0: The caller is allowed for this operation.
//      0: The caller was probably misleaded; try to fix the error
//   else: A real privilege violation; handle it as error.
//
// Privileged operations are:
//   attach_erq_demon  : Attach the erq demon to object <arg> with flag <arg2>.
//   bind_lambda       : Bind a lambda-closure to object <arg>.
//   call_out_info     : Return an array with all call_out informations.
//   erq               : A the request <arg4> is to be send to the
//                       erq-demon by the object <who>.
//   input_to          : Object <who> issues an 'ignore-bang'-input_to() for
//                       commandgiver <arg3>; the exakt flags are <arg4>.
//   nomask simul_efun : Attempt to get an efun <arg> via efun:: when it
//                       is shadowed by a 'nomask'-type simul_efun.
//   rename_object     : The current object <who> renames object <arg>
//                       to name <arg2>.
//   send_imp          : Send UDP-data to host <arg>.
//   get_extra_wizinfo : Get the additional wiz-list info for wizard <arg>.
//   set_extra_wizinfo : Set the additional wiz-list info for wizard <arg>.
//   set_extra_wizinfo_size : Set the size of the additional wizard info
//                       in the wiz-list to <arg>.
//   set_driver_hook   : Set hook <arg> to <arg2>.
//   limited:          : Execute <arg> with reduced/changed limits.
//   set_limits        : Set limits to <arg>.
//   set_this_object   : Set this_object() to <arg>.
//   shadow_add_action : Add an action to function <arg> from a shadow.
//   symbol_variable   : Attempt to create symbol of a hidden variable
//                       of object <arg> with with index <arg2> in the
//                       objects variable table.
//   wizlist_info      : Return an array with all wiz-list information.
//
// call_out_info can return the arguments to functions and lambda closures
// to be called by call_out(); you should consider that read access to
// closures, mappings and pointers means write access and/or other privileges.
// wizlist_info() will return an array which holds, among others, the extra
// wizlist field. While a toplevel array, if found, will be copied, this does
// not apply to nested arrays or to any mappings. You might also have some
// sensitive closures there.
// send_imp() should be watched as it could be abused to mess up the IMP.
// The xxx_extra_wizinfo operations are necessary for a proper wizlist and
// should therefore be restricted to admins.
// All other operations are potential sources for direct security breaches -
// any use of them should be scrutinized closely.

{
    /* This object and the simul_efun objects may do everything */
    if (who == this_object()
     || who == find_object(SIMUL_EFUN_FILE)
     || who == find_object(SPARE_SIMUL_EFUN_FILE))
        return 1;

    switch(op) {
      case "erq":
	switch(arg) {
	  case ERQ_RLOOKUP:
	    return 1;
	  case ERQ_EXECUTE:
	  case ERQ_FORK:
	  case ERQ_AUTH:
	  case ERQ_SPAWN:
	  case ERQ_SEND:
	  case ERQ_KILL:
	  default:
	    return -1;
	}
      default:
	return -1; /* Make this violation an error */
    }
    return 0;
}

//---------------------------------------------------------------------------
int query_allow_shadow (object victim)


// Validate a shadowing.
//
// Arguments:
//   previous_object(): the wannabe shadow
//   victim           : the object to be shadowed.
//
// Result:
//   Return 0 to disallow the shadowing, any other value to allow it.
//   Destructing the shadow or the victim is another way of disallowing.
//
// This function simply asks the victim if it denies a shadow.

{
    if (object_info(victim, OINFO_MEMORY)[OIM_NO_SHADOW])
        return 0;
    return !victim->query_prevent_shadow(previous_object());
}

//---------------------------------------------------------------------------
int query_player_level (string what)

// Check if the player is of high enough level for several things.
//
// Argument:
//   what: The 'thing' type (see below).
//
// Result:
//   Return 0 to disallow, any other value to allow it.
//
// Types asked for so far are:
//   "error messages": Is the player allowed to see error messages (used
//                     by the master)?
//                     (min-level: 20)
//   "wizard"        : Is the player is considered a wizard (used by
//                     the mudlib)?
//                     (min-level: 20)

{
    int level;

    if (this_player() == 0)
	return 0;
    level = (int)this_player()->query_level();
    switch(what) {
    case "wizard":
	return level >= 20;
    case "error messages":
	return level >= 20;
    }
    return 0;
}

//---------------------------------------------------------------------------
int valid_trace (string what)

// Check if the player is allowed to use tracing.
//
// Argument:
//   what: The actual action (see below).
//
// Result:
//   Return 0 to disallow, any other value to allow it.
//
// Actions asked for so far are:
//   "trace":       Is the user allowed to use tracing?
//   "traceprefix": Is the user allowed to set a traceprefix?
//                  (min-level: 24 for both)

{
    int level;

    if (this_player() == 0)
	return 0;
    level = (int)this_player()->query_level();
    switch(what) {
    case "trace":
    case "traceprefix":
	return level >= 24;
    }
    return 0;
}

//---------------------------------------------------------------------------
int valid_exec (string name, object ob, object obfrom)

// Validate the rebinding of an IP connection by usage of efun exec().
//
// Arguments:
//    name  : The name of the _program_ attempting to rebind the connection.
//            This is not the object_name() of the object, and has no leading
//            slash.
//    ob    : The object to receive the connection.
//    obfrom: The object giving the connection away.
//
// Result:
//   Return a non-zero number to allow the action,
//   any other value to disallow it.
//
// Only obj/master.c and secure/login.c are allowed to do that.

{
    switch(name) {
      case "secure/login.c":
      case "obj/master.c":
	if (!interactive(ob)) {
            mudwho_exec(obfrom, ob);
	    return 1;
        }
    }

    return 0;
}

//---------------------------------------------------------------------------
int valid_query_snoop (object obj)

// Validate if the snoopers of an object may be revealed by usage of the
// efun query_snoop().
//
// Arguments:
//   previous_object(): the asking object.
//   obj              : the object which snoopers are to be revealed.
//
// Result:
//   Return a non-zero number to allow the action,
//   any other value to disallow it.
//
// Every true wizard can test for a snoop.

{
    return this_player()->query_level() >= 22;
}

//---------------------------------------------------------------------------
int valid_snoop (object snoopee, object snooper)

// Validate the start/stop of a snoop.
//
// Arguments:
//   snoopee: The victim of the snoop.
//   snooper: The wannabe snooper, or 0 when stopping a snoop.
//
// Result:
//   Return a non-zero number to allow the action,
//   any other value to disallow it.
//
// It is up to the simul_efun object to start/stop snoops.

{
    /* PLAIN:
    if (!geteuid(previous_object()))
        return 0;
    */
    if (object_name(previous_object()) == get_simul_efun())
        return 1;
}


//===========================================================================
//  Userids and depending Security
//
// For each object in the mud exists a string attribute which determines the
// objects rights in security-sensitive matters. In compat muds this attribute
// is called the "creator" of the object, in !compat muds the object's "userid"
// ("uid" for short).
//
// "Effective Userids" are an extension of this system, to allow the easier
// implementation of mudlib security by diffentiating between an objects
// theoretical permissions (uid) and its current permissions (euid) (some
// experts think that this attempt has failed (Heya Macbeth!)).
//
// The driver mainly implements the setting/querying of the (e)uids -- it is
// task of the mudlib to give out the right (e)uid to the right object, and
// to check them where necessary.
//
// If the driver is set to use 'strict euids', the loading and cloning
// of objects requires the initiating object to have a non-zero euid.
//
// The main use for (e)uids is for determination of file access rights, but
// you can of course use the (e)uids for other identification purposes as well.
//===========================================================================

//---------------------------------------------------------------------------
// string get_bb_uid()

// Return the string (or 0) to be used as backbone-euid.
// It is just used by process_string() only if no this_object() is present.
// If strict-euids, the function must exist and return a string.

//---------------------------------------------------------------------------
// int valid_seteuid (object obj, string neweuid)

// Validate the change of an objects euid by efun seteuid().
//
// Arguments:
//   obj    : The object requesting the new euid.
//   neweuid: The new euid requested.
//
// Result:
//   Return 1 to allow the change, any other value to disallow it.


//---------------------------------------------------------------------------
mixed valid_read  (string path, string euid, string fun, object caller)

// Validate a reading/writing file operation.
//
// Arguments:
//   path   : The (possibly partial) filename given to the operation.
//   euid   : the euid of the caller (might be 0).
//   fun    : The name of the operation requested (see below).
//   caller : The calling object.
//
// Result:
//   The full pathname of the file to operate on, or 0 if the action is not
//   allowed.
//   You can also return 1 to indicate that the path can be used unchanged.
//
// The path finally to be used must not contain spaces or '..'s .
//
// These are the central functions establishing the various file access
// rights. In this implementation, the main work is done by
// obj/player->valid_read(<path>).
//
// valid_read() is called for these operations:
//   ed_start        (when reading a file)
//   file_size
//   get_dir
//   print_file
//   read_bytes
//   read_file
//   restore_object
//   tail

{
    string user;

    switch ( fun ) {
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
                return ADD_SLASH(error[0]);
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
                return ADD_SLASH(path);
            }
            path = (string)"obj/player"->valid_read(path);
            if (stringp(path))
                return ADD_SLASH(path);
            return 0;
    }
    /* if a case failed to return a value or the caller function wasn't
     * recognized, we come here.
     * The default returned zero indicates deny of access.
     */
    return 0;
}

//---------------------------------------------------------------------------
mixed valid_write (string path, string euid, string fun, object caller)

// Validate a writing file operation.
//
// Arguments:
//   path   : The (possibly partial) filename given to the operation.
//   euid   : the euid of the caller (might be 0).
//   fun    : The name of the operation requested (see below).
//   caller : The calling object.
//
// Result:
//   The full pathname of the file to operate on, or 0 if the action is not
//   allowed.
//   You can also return 1 to indicate that the path can be used unchanged.
//
// The path finally to be used must not contain spaces or '..'s .
//
// These are the central functions establishing the various file access
// rights. In this implementation, the main work is done by
// obj/player->valid_write(<path>).
//
// valid_write() is called for these operations:
//   ed_start     (when writing a file)
//   rename_from  (for each the old name of a rename())
//   rename_to    (for the new name of a rename())
//   mkdir
//   save_object
//   objdump
//   opcdump
//   remove_file
//   rmdir
//   write_bytes
//   write_file

{
    string user;

    if (path[0] == '/' && path != "/")
        path = path[1..];

    switch ( fun ) {
    case "objdump":
        if (path == "OBJ_DUMP") return path;
        return 0;

    case "opcdump":
        if (path == "OPC_DUMP") return path;
        return 0;

    case "save_object":
        if ( user = GETUID(previous_object()) ) {
            if ( path[0 .. strlen(user)+7] == "players/" + user
             &&  sscanf(path, ".%s", user) == 0)
                return ADD_SLASH(path);
        } else {
            user = efun::object_name(previous_object());
#ifndef __COMPAT_MODE__
            user = user[1..];
#endif
            if ( user[0..3] == "obj/"
             ||  user[0..4] == "room/"
             ||  user[0..3] == "std/"  )
                return ADD_SLASH(path);
        }
        return 0; /* deny access */
    default:
        return 0; /* deny access */
    case "write_file":
        if (caller == this_object()) return 1;
        if (path[0..3] == "log/"
         && !(   sizeof(regexp(({path[4..33]}), "/"))
              || path[4] == '.'
              || strlen(path) > 34
            ) ) {
            return ADD_SLASH(path);
        }
        break;
    case "ed_start":
        if (path[0] != '/')
            path = "/"+path;
        break;
    case "rename_from":
    case "rename_to":
        if ((   efun::object_name(caller) == SIMUL_EFUN_FILE
             || efun::object_name(caller) == SPARE_SIMUL_EFUN_FILE)
         && path[0..3] == "log/"
         && !(   sizeof(regexp(({path[4..33]}), "/"))
              || path[4] == '.'
              || strlen(path) > 34
            ) ) {
            return 1;
        }
    case "mkdir":
    case "rmdir":
    case "write_bytes":
    case "remove_file":
        if (caller == this_object()) return 1;
    }

    set_this_object(caller);
    if( this_player() && query_ip_number(this_player()) )
    {
        path = (string)this_player()->valid_write(path);
        if (!stringp(path)) {
            write("Bad file name.\n");
            return 0;
        }
        return ADD_SLASH(path);
    }
    path = (string)"obj/player"->valid_write(path);
    if (stringp(path))
        return ADD_SLASH(path);

    return 0;
}

//===========================================================================
//  ed() Support
//
//===========================================================================

//---------------------------------------------------------------------------
string make_path_absolute (string str)

// Absolutize a relative filename given to the editor.
//
// Argument:
//   str : The relative filename (without leading slash).
//
// Result:
//   The full pathname of the file to use.
//   Any non-string result will act as 'bad file name'.

{
    return valid_read(str,0,"make_path_absolute", this_player());
}

//---------------------------------------------------------------------------
int save_ed_setup (object who, int code)

// Save individual settings of ed for a wizard.
//
// Arguments:
//   who : The wizard using the editor.
//   code: The encoded options to be saved.
//
// Result:
//   Return 0 on failure, any other value for success.
//
// This function has to save the given integer into a safe place in the
// realm of the given wizard, either a file, or in the wizard itself.
//
// Be aware of possible security breaches: under !compat, a write_file()
// should be surrounded by a temporary setting of the masters euid to
// that of the wizard.

{
    string file;

    if (!intp(code))
	return 0;
    file = "/players/" + lower_case((string)who->query_name()) + "/.edrc";
    rm(file);
    return write_file(file, code + "");
}

//---------------------------------------------------------------------------
int retrieve_ed_setup (object who)

// Retrieve individual settings of ed for a wizard.
//
// Arguments:
//   who : The wizard using the editor.
//
// Result:
//   The encoded options retrieved (0 if there are none).

{
    string file;
    int code;

    file = "/players/" + lower_case((string)who->query_name()) + "/.edrc";
    if (file_size(file) <= 0)
	return 0;
    sscanf(read_file(file), "%d", code);
    return code;
}

//---------------------------------------------------------------------------
string get_ed_buffer_save_file_name (string file)

// Return a filename for the ed buffer to be saved into.
//
// Arguments:
//   this_player(): The wizard using the editor.
//   file         : The name of the file currently in the buffer.
//
// Result:
//   The name of the file to save the buffer into, or 0.
//
// This function is called whenever a wizard is destructed/goes netdeath
// while editing. Using this function, his editing is not done in vain.

{
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

//===========================================================================
//  parse_command() Support  (!compat, SUPPLY_PARSE_COMMAND defined)
//
// LPMud has a builtin support for parsing complex commands.
// It does this by requestion several types of ids from the objects.
// The same queried functions are also in the master to provide decent
// defaults, especially for generic ids like 'all the blue ones'.
//
// Each of the functions has to return an array of strings (with the exception
// of parse_command_all_word), each string being one of the ids for that type
// of id.
//
// The whole parsing has a preference for the english language, so the
// the code for parsing english constructs is given as well.
//===========================================================================

//---------------------------------------------------------------------------
string *parse_command_id_list ()

// Return generic singular ids.

{
  return ({ "one", "thing" });
}


//---------------------------------------------------------------------------
string *parse_command_plural_id_list ()

// Return generic plural ids.

{
  return ({ "ones", "things", "them" });
}


//---------------------------------------------------------------------------
string *parse_command_adjectiv_id_list ()

// Return generic adjective ids.
// If there are none (like here), return some junk which is likely never
// typed.

{
  return ({ "iffish" });
}


//---------------------------------------------------------------------------
string *parse_command_prepos_list ()

// Return common prepositions.

{
    return ({ "in", "on", "under", "behind", "beside" });
}


//---------------------------------------------------------------------------
string parse_command_all_word()

// Return the one(!) 'all' word.

{
  return "all";
}

#ifdef MUDWHO

//===========================================================================
// Mudwho support functions
//
// Mudwho was (is?) a system of interconnected servers which keep track
// who is in which mud. Once quite popular, it is now propably defunct.
// But anyway, here is the code once used by Nightfall.
//===========================================================================

#define MUDWHO_SERVER   "134.2.62.161"
#define MUDWHO_PORT     6888
  /* IP and port of the closest mudwho server */

#define MUDWHO_NAME     "TestNase"
#define MUDWHO_PASSWORD "TestPassword"
  /* Participation in the mudwho service required registration */

#define MUDWHO_REFRESH_TIME 100

#define QUOTE_PERCENT(s) (implode(explode(s, "%"), "%%"))

private string mudwho_ping;
private mapping mudwho_info = ([]);
private closure send_mudwho_info;

//---------------------------------------------------------------------------
static void mudwho_init (int arg)
{
    send_mudwho_info
      = lambda( ({'key, 'info})
              , ({#'send_imp, MUDWHO_SERVER, MUDWHO_PORT, 'info }));
    if (!arg)
    {
        send_imp(MUDWHO_SERVER, MUDWHO_PORT
                , sprintf("U\t%.20s\t%.20s\t%.20s\t%:010d\t0\t%.25s"
                         , MUDWHO_NAME, MUDWHO_PASSWORD, MUDWHO_NAME
                         , time(), __VERSION__)
                );
        get_extra_wizinfo(0)[MUDWHO_INDEX] = mudwho_info;
    }
    else
    {
        mudwho_info = get_extra_wizinfo(0)[MUDWHO_INDEX];
    }

    mudwho_ping = sprintf("M\t%.20s\t%.20s\t%.20s\t%%:010d\t0\t%.25s"
                         , QUOTE_PERCENT(MUDWHO_NAME)
                         , QUOTE_PERCENT(MUDWHO_PASSWORD)
                         , QUOTE_PERCENT(MUDWHO_NAME)
                         , QUOTE_PERCENT(__VERSION__)
                         );

    if (find_call_out("send_mudwho_info") < 0)
        call_out("send_mudwho_info", MUDWHO_REFRESH_TIME);
}

//---------------------------------------------------------------------------
static void mudwho_shutdown()
{
    send_imp(MUDWHO_SERVER, MUDWHO_PORT
            , sprintf("D\t%.20s\t%.20s\t%.20s"
                     , MUDWHO_NAME, MUDWHO_PASSWORD, MUDWHO_NAME));
}

//---------------------------------------------------------------------------
static void mudwho_connect (object ob)
{
    mudwho_info[ob] = sprintf("A\t%.20s\t%.20s\t%.20s\t%:010d\t0\tlogin"
                             , MUDWHO_NAME, MUDWHO_PASSWORD, MUDWHO_NAME
                             , explode(object_name(ob), "#")[<1]
                               + "@" MUDWHO_NAME
                             , time()
                             );
}

//---------------------------------------------------------------------------
static void send_mudwho_info()
{
    send_imp(MUDWHO_SERVER, MUDWHO_PORT, sprintf(mudwho_ping, time()));
    walk_mapping(mudwho_info, send_mudwho_info);
    call_out("send_mudwho_info", MUDWHO_REFRESH_TIME);
}

//---------------------------------------------------------------------------
static void adjust_mudwho (object ob)
{
    if (ob && interactive(ob) && mudwho_info[ob][<5..] == "login")
    {
        mudwho_info[ob][<5..] = ob->query_real_name()[0..24];
        send_imp(MUDWHO_SERVER, MUDWHO_PORT, mudwho_info[ob]);
    }
}

//---------------------------------------------------------------------------
static void mudwho_exec (object obfrom, object ob)
{
    if (interactive(obfrom))
    {
        mudwho_info[ob] = mudwho_info[obfrom];
        efun::m_delete(mudwho_info, obfrom);
        call_out("adjust_mudwho", 0, ob);
    }
}

//---------------------------------------------------------------------------
static void mudwho_disconnect (object ob)
{
    send_imp(MUDWHO_SERVER, MUDWHO_PORT
            , "Z\t"+implode(explode(mudwho_info[ob], "\t")[1..4], "\t"));
}

//---------------------------------------------------------------------------

#endif /* MUDWHO */

//===========================================================================
// 2.4.5-Lib related functions
//===========================================================================

//---------------------------------------------------------------------------
static void wiz_decay()

/* Decay the 'worth' entry in the wizlist
 */

{
    mixed *wl;
    int i;

    wl = wizlist_info();
    for (i=sizeof(wl); i--; ) {
        set_extra_wizinfo(wl[i][WL_NAME], wl[i][WL_EXTRA] * 99 / 100);
    }
    call_out("wiz_decay", 3600);
}

//---------------------------------------------------------------------------
void save_wiz_file()

/* Save the wizlist file.
 */

{
#ifdef __WIZLIST__
    rm(__WIZLIST__);
    write_file(
      __WIZLIST__,
      implode(
        map(wizlist_info(),
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
#endif
}

//---------------------------------------------------------------------------
int verify_create_wizard (object ob)

/* This function is called for a wizard that has dropped a castle.
 * The argument is the file name of the object that called create_wizard().
 * Verify that this object is allowed to do this call.
 */

{
    int dummy;

    if (sscanf(object_name(ob), "room/port_castle#%d", dummy) == 1
      || sscanf(object_name(ob), "global/port_castle#%d", dummy) == 1)
	return 1;
    return 0;
}

//---------------------------------------------------------------------------
string master_create_wizard(string owner, string domain, object caller)

/* Create a home dritectory and a castle for a new wizard. It is called
 * automatically from create_wizard(). We don't use the 'domain' info.
 * The create_wizard() efun is not really needed any longer, as a call
 * could be done to this function directly.
 *
 * This function can create directories and files in /players. It is
 * garded from calls from the wrong places.
 */

{
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
	    if (!write_file("/room/init_file", castle[1..] + "\n"))
		tell_object(player, "It couldn't be loaded automatically!\n");
	} else {
	    tell_object(player, "Failed to make castle for you!\n");
	}
    }
    return castle;
}

/****************************************************************************/
