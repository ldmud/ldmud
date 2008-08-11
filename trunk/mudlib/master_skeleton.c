/*  MASTER_NAME      (#define in config.h, or as commandline option)
**  obj/master.c     (compat default)
**  secure/master.c  (native default)
**
** The master is the gateway between the gamedriver and the mudlib to perform
** actions with mudlib specific effects.
** Calls to the master by the gamedriver have an automatic catch() in effect.
**
** This skeleton names all functions called by the gamedriver and gives
** suggestions on how to program then (a few by code).
** Functions which are specific to compat or native mode are tagged as such:
**   '// !compat' means: only when not running in compat mode.
**   '// native'  means: only when running in native mode.
** and similar for every other combination.
**
** Note that the master is loaded first of all objects. Thus it is possible,
** you shouldn't inherit an other object (as most files expect the master
** to exist), nor is the compiler able to search include files
** (read: they must be specified with full path).
*/


// A short reference to all functions...
//---------------------------------------------------------------------------
//     Initialisation
//
// void inaugurate_master (int arg)
//   Perform mudlib specific setup of the master.
//
// string get_master_uid ()
//   Return the string to be used as uid (and -euid) of a (re)loaded master.
//
// void flag (string arg)
//   Evaluate an argument given as option '-f' to the driver.
//
// string *epilog (int eflag)
//   Perform final actions before opening the game to players.
//
// void preload (string file)
//   Preload a given object.
//
// void external_master_reload ()
//   Called after a reload of the master on external request.
//
// void reactivate_destructed_master (int removed)
//   Reactivate a formerly destructed master.
//
// string|string * get_simul_efun ()
//   Load the simul_efun object(s) and return one or more paths of it.
//
//---------------------------------------------------------------------------
//     Handling of player connections
//
// object connect ()
//   Handle the request for a new connection.
//
// void disconnect (object obj, string remaining)
//   Handle the loss of an IP connection.
//
// void remove_player (object player)
//   Remove a player object from the game.
//
// void stale_erq (closure callback)
//   Notify the loss of the erq demon.
//---------------------------------------------------------------------------
//     Runtime Support
//
// object compile_object (string filename)
//   Compile an virtual object.
//
// string get_wiz_name (string file)
//   Return the author of a file.
//
// mixed include_file (string file, string compiled_file, int sys_include)
//   Return the full pathname for an included file.
//
// mixed inherit_file (string file, string compiled_file)
//   Return the full pathname for an inherited object.
//
// string printf_obj_name (object obj)
//   Return a printable name for an object.
//
// mixed prepare_destruct (object obj)
//   Prepare the destruction of the given object.
//
// void quota_demon (void)
//   Handle quotas in times of memory shortage.
//
// void receive_udp (string host, string msg, int port)
//   Handle a received UDP message.
//
// void slow_shut_down (int minutes)
//   Schedule a shutdown for the near future.
//
// void notify_shutdown (void|string crash_reason)
//   Notify the master about an immediate shutdown.
//
//---------------------------------------------------------------------------
//     Error Handling
//
// void dangling_lfun_closure ()
//   Handle a dangling lfun-closure.
//
// void log_error (string file, string err, int warn)
//   Announce a compiler-time error or warning.
//
// mixed heart_beat_error (object culprit, string err,
//                         string prg, string curobj, int line,
//                         int caught )
//   Announce an error in the heart_beat() function.
//
// void runtime_error (string err, string prg, string curobj, int line
//                    , mixed culprit, int caught)
//   Announce a runtime error.
//
// void runtime_warning (string msg, string curobj, string prg, int line
//                      , int inside_catch)
//   Announce a runtime warning.
//
//---------------------------------------------------------------------------
//     Security and Permissions
//
// int privilege_violation (string op, mixed who, mixed arg, mixed arg2)
//   Validate the execution of a privileged operation.
//
// int query_allow_shadow (object victim)
//   Validate a shadowing.
//
// int valid_trace (string what, int|string arg)
//   Check if the player may use tracing.
//
// int valid_exec (string name, object ob, object obfrom)
//   Validate the rebinding of an IP connection by usage of efun exec().
//
// int valid_query_snoop (object obj)
//   Validate if the snoopers of an object may be revealed by usage of the
//   efun query_snoop().
//
// int valid_snoop (object snoopee, object snooper)
//   Validate the start/stop of a snoop.
//
//---------------------------------------------------------------------------
//     Userids and depending Security
//
// string get_bb_uid()
//   Return the string to be used as temporary euid by process_string().
//
// int valid_seteuid (object obj, string neweuid)
//   Validate the change of an objects euid by efun seteuid().
//
// int|string valid_read  (string path, string euid, string fun, object caller)
// int|string valid_write (string path, string euid, string fun, object caller)
//   Validate a reading/writing file operation.
//
//---------------------------------------------------------------------------
//     ed() Support
//
// string make_path_absolute (string str)
//   Absolutize a relative filename given to the editor.
//
// int save_ed_setup (object who, int code)
//   Save individual settings of ed for a wizard.
//
// int retrieve_ed_setup (object who)
//   Retrieve individual settings of ed for a wizard.
//
// string get_ed_buffer_save_file_name (string file)
//   Return a filename for the ed buffer to be saved into.
//
//---------------------------------------------------------------------------
//     parse_command() Support  (!compat, SUPPLY_PARSE_COMMAND defined)
//
// string *parse_command_id_list ()
//   Return generic singular ids.
//
// string *parse_command_plural_id_list ()
//   Return generic plural ids.
//
// string *parse_command_adjectiv_id_list ()
//   Return generic adjective ids.
//
// string *parse_command_prepos_list ()
//   Return common prepositions.
//
// string parse_command_all_word()
//   Return the one(!) 'all' word.
//
//---------------------------------------------------------------------------


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
//     preload, which preload() then does.
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


//---------------------------------------------------------------------------
int|string get_master_uid ()

// Return the string to be used as uid (and -euid) of a (re)loaded master.
// Under !native, the function may also return a non-zero number.
// In that case, the uid is set to 0, as is the euid.


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
string *epilog (int eflag)

// Perform final actions before opening the game to players.
//
// Arguments:
//   eflag: This is the number of '-e' options given to the parser.
//          Normally it is just 0 or 1.
//
// Result:
//   An array of strings, which traditionally designate the objects to be
//   preloaded with preload().
//   Any other result is interpreted as 'no object to preload'.
//   The resulting strings will be passed one at the time as
//   arguments to preload().


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


//---------------------------------------------------------------------------
void external_master_reload ()

// Master was reloaded on external request by SIGUSR1.
//
// If the gamedriver destruct and reloads the master on external request
// via SIGUSR1, it does this by a call to this function.
// It will be called after inaugurate_master() of course.
// If you plan to do additional magic here, you're welcome.


//---------------------------------------------------------------------------
void reactivate_destructed_master (int removed)

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
string|string * get_simul_efun ()

// Load the simul_efun object(s) and return one or more paths of it.
//
// Result:
//   Either a single string with the file_name() of the simul_efun object,
//   or an array of strings which has to start with that file_name().
//   Return 0 if this feature isn't wanted.
//
// Note that the object(s) must be loaded by this function!
//
// When you return an array of strings, the first string is taken as path
// to the simul_efun object, and all other paths are used for backup
// simul_efun objects to call simul_efuns that are not present in the
// main simul_efun object. This allows to remove simul_efuns at runtime
// without getting errors from old compiled programs that still use the
// obsolete simul_efuns.
//
// The additional simul-efun objects can not serve as backups for
// the primary one!
//
// If the game depends on the simul_efun object, and none could be loaded,
// an immediate shutdown should occur.


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
//   An login object the requested connection should be bound to.
//
// Note that the connection is at this time bound to the master object,
// and will be re-bound to the returned object.
//
// The gamedriver will call the lfun 'logon()' in the login object after
// binding the connection to it. That lfun has to return !=0 to succeed.
//
// If connect() initiates a secure connection without setting a callback,
// and the connection is still handshaking at the time connect() returns,
// the driver will delay the call to logon() until the handshake either
// succeeds or fails.



//---------------------------------------------------------------------------
void disconnect (object obj, string remaining)

// Handle the loss of an IP connection.
//
// Argument:
//   obj: The (formerly) interactive object (player).
//   remaining: The remaining unprocessed input data from the connection.
//
// This called by the gamedriver to handle the removal of an IP connection,
// either because the connection is already lost ('netdeath') or due to
// calls to exec() or remove_interactive().
//
// The connection will be unbound upon return from this call, so
// for the time of this call, interactive(ob) will still return TRUE
// even if the actual network connection has already been lost.
//
// This method is not called if the object has been destructed already.


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


//---------------------------------------------------------------------------
mixed include_file (string file, string compiled_file, int sys_include)

// Generate the pathname of an included file.
//
// Arguments:
//   previous_object(): The object causing the compile.
//   file             : The name given in the #include directive.
//   compiled_file    : The object file which is just compiled
//                      (compat: name without leading "/").
//   sys_include      : TRUE for #include <> directives.
//
// Result:
//   0:      use the normal include filename generation (""-includes are used
//           as they are, <>-includes are handled according to H_INCLUDE_DIRS).
//   <path>: the full absolute pathname of the file to include without
//           parentdir parts ("/../"). Leading slashes ("/") may be omitted.
//   else:   The include directive is not legal.

//---------------------------------------------------------------------------
mixed inherit_file (string file, string compiled_file)

// Generate the pathname of an inherited file.
//
// Arguments:
//   previous_object(): The object causing the compile.
//   file             : The name given in the inherit directive.
//   compiled_file    : The object file which is just compiled
//                      (compat: name without leading "/").
//
// Result:
//   0:      use the filename as it is.
//   <path>: the full absolute pathname of the file to inherit without
//           parentdir parts ("/../"). Leading slashes ("/") are ignored.
//   else:   The include directive is not legal.

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


//---------------------------------------------------------------------------
string printf_obj_name (object obj)

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
mixed prepare_destruct (object obj)

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
// the destruct will fail. It is also recommended to clean up all
// shadows on obj at this point.
//
// Furthermore, the function could notify the former inventory objects that
// their holder is under destruction (useful to move players out of rooms which
// are updated); and it could announce mudwide the destruction(quitting) of
// players.
//
// Another use for this apply is to take care of any other 'cleanup'
// work needed to be done, like adjusting weights, light levels, and
// such. Alternatively and traditionally this is done by calling an
// lfun 'remove()' in the object, which then calls the efun destruct()
// after performing all the adjustments.


//---------------------------------------------------------------------------
void quota_demon (void)

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
void receive_udp (string host, string msg, int port)

// Handle a received UDP message.
//
// Arguments:
//   host: Name of the host the message comes from.
//   msg : The received message.
//   port: the port number from which the message was sent.
//
// This function is called for every message received on the UDP port.


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
// For example: this function might load an 'Armageddon' object and tells
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


//===========================================================================
//  Error Handling
//
//===========================================================================

//---------------------------------------------------------------------------
void dangling_lfun_closure ()

// Handle a dangling lfun-closure.
//
// This is called when the gamedriver executes a closure using a vanished
// lfun, with previous_object() showing the originating object. A proper
// handling is to raise a runtime error.
//
// Technical:
//   Upon replacing programs (see efun replace_program()), any existing
//   lambda closures of the object are adjusted to the new environment.
//   If a closure uses a lfun which vanished in the replacement process,
//   the reference to the lfun is replaced by an alien-lfun closure
//   referencing this function.  The error will then occur when the execution
//   of the adjusted lambda reaches the point of the lfun reference.
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
void log_error (string file, string err, int warn)

// Announce a compiler-time error or warning.
//
// Arguments:
//   file: The name of file containing the error/warning (it needn't be
//         an object file!).
//   err : The error/warning message.
//   warn: non-zero if this is a warning, zero if it is an error.
//
// Whenever the LPC compiler detects an error or wants to issue a warning,
// this function is called.
// It should at least log the message in a file, and also announce it
// to the active player if it is an wizard.


//---------------------------------------------------------------------------
mixed heart_beat_error (object culprit, string err,
                        string prg, string curobj, int line,
                        int caught)

// Announce an error in the heart_beat() function.
//
// Arguments:
//   culprit: The object which lost the heart_beat.
//   err    : The error message.
//   prg    : The executed program (might be 0).
//   curobj : The object causing the error (might be 0).
//   line   : The line number where the error occured (might be 0).
//   caught : 0 if the error is not caught, != 0 if it is caught.
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


//---------------------------------------------------------------------------
void runtime_error (string err, string prg, string curobj, int line
                   , mixed culprit, int caught)

// Announce a runtime error.
//
// Arguments:
//   err    : The error message.
//   prg    : The executed program.
//   curobj : The object causing the error.
//   line   : The line number where the error occured.
//   culprit: -1 for runtime errors; the object holding the heart_beat()
//            function for heartbeat errors.
//   caught : 0 if the error is not caught, != 0 if it is caught.
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
//
// One common pitfall in the implementation of runtime_error() is that
// runtime_error() itself could run out of evaluation ticks, causing a
// runtime error itself. The workaround is to use limited() like this:
//
//   static void
//   handle_runtime_error (string err, string prg, string curobj, int line)
//   { ... the actual error handler ... }
//
//   static void
//   call_runtime_error (string err, string prg, string curobj, int line)
//   {
//       limited(#'handle_runtime_error, ({ 200000 }), err, prg, curobj, line);
//   }
//
//   void
//   runtime_error (string err, string prg, string curobj, int line)
//   {
//       limited(#'call_runtime_error, ({ LIMIT_UNLIMITED })
//              , err, prg, curobj, line);
//   }


//---------------------------------------------------------------------------
void runtime_warning (string msg, string curobj, string prg, int line
                     , int inside_catch)

// Announce a runtime warning.
//
// Arguments:
//   err    : The warning message.
//   curobj : The object causing the warning, may be 0.
//   prg    : The executed program, may be 0.
//   line   : The line number where the warning occured.
//   inside_catch : != 0 if the warning occurs inside a catch().
//
// This function is to allow the mudlib to handle runtime warnings, for
// example to log them into a database.
//
// Note that <prg> denotes the program actually executed (which might be
// inherited) whereas <curobj> is just the offending object for which the
// program was executed.
// 
// The driver imposes a limit of three nested warnings, to prevent endless
// recursions.


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
//   erq               : A the request <arg2> is to be send to the
//                       erq-demon by the object <who>.
//   enable_telnet     : Enable/disable telnet (<arg2>) for object <arg>.
//   execute_command   : Execute command string <arg2> for the object <arg>.
//   input_to          : Object <who> issues an 'ignore-bang'-input_to() for
//                       commandgiver <arg>; the exakt flags are <arg2>.
//   mysql             : Object <who> attempted to execute mySQL efun <arg>.
//   pgsql             : Object <who> attempted to execute Postgres efun <arg>.
//   net_connect       : Attempt to open a connection to host <arg>,
//                        port <arg2>.
//   nomask simul_efun : Attempt to get an efun <arg> via efun:: when it
//                       is shadowed by a 'nomask'-type simul_efun.
//   rename_object     : The current object <who> renames object <arg>
//                       to name <arg2>.
//   send_udp          : Send UDP-data to host <arg>.
//   get_extra_wizinfo : Get the additional wiz-list info for wizard <arg>.
//   set_extra_wizinfo : Set the additional wiz-list info for wizard <arg>.
//   set_extra_wizinfo_size : Set the size of the additional wizard info
//                       in the wiz-list to <arg>.
//   set_driver_hook   : Set hook <arg> to <arg2>.
//   set_max_commands  : Set the max. number of commands interactive
//                       object <arg> can issue per second to <arg2>.
//   limited           : Execute <arg> with reduced/changed limits
//                       <arg2> (as return by query_limits()).
//   set_limits        : Set limits to <arg> (as returned by query_limits()).
//   set_this_object   : Set this_object() to <arg>.
//   shadow_add_action : Add an action to function <arg2> of object <arg>
//                       from the shadow <who> which is shadowing <arg>.
//   symbol_variable   : Attempt to create symbol of a hidden variable
//                       of object <arg> with with index <arg2> in the
//                       objects variable table.
//   variable_list     : An attempt to return the variable values of object
//                        <arg> is made from a different object <who>.
//   wizlist_info      : Return an array with all wiz-list information.
//
// call_out_info can return the arguments to functions and lambda closures
// to be called by call_out(); you should consider that read access to
// closures, mappings and pointers means write access and/or other privileges.
// wizlist_info() will return an array which holds, among others, the extra
// wizlist field. While a toplevel array, if found, will be copied, this does
// not apply to nested arrays or to any mappings. You might also have some
// sensitive closures there.
// send_udp() should be watched as it could be abused.
// The xxx_extra_wizinfo operations are necessary for a proper wizlist and
// should therefore be restricted to admins.
// All other operations are potential sources for direct security breaches -
// any use of them should be scrutinized closely.

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
// The function should deny shadowing on all root objects, else it might
// query the victim for clearance.


//---------------------------------------------------------------------------
int valid_trace (string what, int|string arg)

// Check if the player is allowed to use tracing.
//
// Argument:
//   what: The actual action (see below).
//   arg:  The argument given to the traceing efun.
//
// Result:
//   Return 0 to disallow, any other value to allow it.
//
// Actions asked for so far are:
//   "trace":       Is the user allowed to use tracing?
//                  <arg> is the tracelevel argument given.
//   "traceprefix": Is the user allowed to set a traceprefix?
//                  <arg> is the prefix given.

//---------------------------------------------------------------------------
int valid_exec (string name, object ob, object obfrom)

// Validate the rebinding of an IP connection by usage of efun exec().
//
// Arguments:
//    name  : The name of the _program_ attempting to rebind the connection.
//            This is not the file_name() of the object, and has no leading
//            slash.
//    ob    : The object to receive the connection.
//    obfrom: The object giving the connection away.
//
// Result:
//   Return a non-zero number to allow the action,
//   any other value to disallow it.


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
string get_bb_uid()

// This method is called when efun process_string() is used without a
// current object (e.g. from notify_fail method). The current object
// will be set to the current command giver, and will receive the euid
// returned from this function.
//
// If strict-euids, this function must exist and return a string.
// Otherwise the function is optional and/or may return 0.

//---------------------------------------------------------------------------
int valid_seteuid (object obj, string neweuid)

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
mixed valid_write (string path, string euid, string fun, object caller)

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
// The returned pathname must not contain ``..'', a leading / will be stripped
// by the interpreter. By default, the returned path must also not contain
// space characters; if the driver is instructed to allow them, the
// preprocessor macro __FILENAME_SPACES__ is defined.
//
// These are the central functions establishing the various file access
// rights.
//
// Note that this function is called in compat mode as well!
// If you need to be compatible with the old 2.4.5-mudlib, redirect these
// calls to the valid_read/valid_write in the player object.
//
// valid_read() is called for these operations:
//   copy_file       (for the source file)
//   ed_start        (when reading a file)
//   file_size
//   get_dir
//   print_file
//   read_bytes
//   read_file
//   restore_object
//   tail
//
// For restore_object(), the <path> passed is the filename as given
// in the efun call.
//
//
// valid_write() is called for these operations:
//   copy_file           (for the target file resp. directory name)
//   ed_start            (when writing a file)
//   garbage_collection  (for the log filename)
//   rename_from         (for each the old name of a rename())
//   rename_to           (for the new name of a rename())
//   mkdir
//   memdump
//   objdump
//   opcdump
//   save_object
//   remove_file
//   rmdir
//   write_bytes
//   write_file
//
// For save_object(), the <path> passed is the filename as given
// in the efun call. If for this efun a filename ending in ".c" is
// returned, the ".c" will be stripped from the filename.



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


//---------------------------------------------------------------------------
int retrieve_ed_setup (object who)

// Retrieve individual settings of ed for a wizard.
//
// Arguments:
//   who : The wizard using the editor.
//
// Result:
//   The encoded options retrieved (0 if there are none).


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


/****************************************************************************/
