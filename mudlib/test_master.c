/* Test Master.c
 *
 * Minimal master.c to provide a very rudimentary test of the gamedriver.
 * To perform the test, cd to the directory this file is in and give the
 * command
 *    driver -N -e -m. -Mtest_master.c -s-1 -sv-1 4242
 * (this assumes that you called the gamedriver 'driver' and that it is
 * in your searchpath).
 * Once it's running, telnet to localhost, port 4242. You should be
 * connected to the driver now and be able to enter commands.
 * The command 'help' will list you the commands available.
 *
 * A second test consists of the simple command
 *    driver --version
 * The driver shall just print the version and exit immediately.
 */

//---------------------------------------------------------------------------
void inaugurate_master (int arg)

// Initialise the master object.
// We have to set the uid hooks, otherwise we can't clone a login object.

{
    set_driver_hook(2, unbound_lambda(({}), "uid"));
    set_driver_hook(3, unbound_lambda(({}), "uid"));
    set_driver_hook(10, "What?\n");
#ifdef NOECHO
    set_driver_hook(13, "telnetneg");
    set_driver_hook(14, "set_noecho");
#endif
}

//---------------------------------------------------------------------------
int old_flag;

void set_noecho(int flag)
{
    if (flag & ~old_flag & 1)
    {
        write("sending IAC WILL ECHO\n");
        binary_message(({ 0xff, 0xfb, 0x01 })); // IAC WILL ECHO
    }
    else if (old_flag & ~flag & 1)
    {
        write("sending IAC WONT ECHO\n");
        binary_message(({ 0xff, 0xfc, 0x01 })); // IAC WONT ECHO
    }
    if (flag & ~old_flag & 2)
    {
        write("sending IAC WILL+DO SGA\n");
        binary_message(({ 0xff, 0xfb, 0x03 })); // IAC WILL SGA
        binary_message(({ 0xff, 0xfd, 0x03 })); // IAC DO SGA
    }
    else if (old_flag & ~flag & 2)
    {
        write("sending IAC WONT+DONT SGA\n");
        binary_message(({ 0xff, 0xfc, 0x03 })); // IAC WONT SGA
        binary_message(({ 0xff, 0xfe, 0x03 })); // IAC DONT SGA
    }
    old_flag = flag;
} /* set_noecho() */

void telnetneg(int a, int b, int* c)
{
    // just ignore, should work with linux telnet
    printf("got %d %d %O\n", a,b,c);
}

//---------------------------------------------------------------------------
string get_simul_efun ()

// Load the simul-efun object "/sefun" if existing and return its pathname.

{
    object sefun;

    if (!catch(sefun = load_object("/sefun")))
        return object_name(sefun);
    return 0;
}

//---------------------------------------------------------------------------
string get_master_uid()

// Return the master uid.
{
    return " R O O T ";
}

//---------------------------------------------------------------------------
void flag (string arg)

// Evaluate an argument given as option '-f' to the driver.

{
    debug_message(sprintf("%O: flag(%O)\n", this_object(), arg));
    if (arg == "test")
    {
        /* Insert your test code here */
        return;
    }

    if (arg == "gc")
    {
        garbage_collection();
        return;
    }

    if (arg == "dhry")
    {
        limited( (: load_object("dhrystone")->main(1000) :) );
        shutdown();
        return;
    }

    if (arg == "dhry2")
    {
        limited( (: load_object("dhrystone2")->main(1000) :) );
        shutdown();
        return;
    }

    if (arg == "shutdown")
    {
        shutdown();
        return;
    }
    write ("master: Unknown flag "+arg+"\n");
}

//---------------------------------------------------------------------------
mixed prepare_destruct (object obj)

// Prepare the destruction of the object.

{
    debug_message(sprintf("%O: prepare_destruct(%O)\n", this_object(), obj));
    return 0;
}

//---------------------------------------------------------------------------
object connect ()

// Handle the request for a new connection.
// We simply return a clone of ourself (we can't simply return this object
// unfortunately), the gamedriver will then call logon() here.

{
    object obj;
    debug_message(sprintf("%O: connect()\n", this_object()));
    obj = clone_object(object_name(this_object()));
    return obj;
}

//---------------------------------------------------------------------------
static nomask mixed logon ()

// A connection was successfully bound to this object.
// Print some status data and add the commands.

{
    debug_message(sprintf("%O: logon()\n", this_object()));
    write("\nLDMud " __VERSION__ "\n\n----------\n");
    write(debug_info(4,0));
    write("----------\n\n");
    enable_commands();
    add_action("f_help", "help");
    add_action("f_shutdown", "shutdown");
    add_action("f_echo", "echo");
    add_action("f_flag", "flag");
    add_action("f_gc", "gc");
    add_action("f_upd", "upd");
    add_action("f_quit", "quit");
    add_action("f_charmode", "charmode");
    set_combine_charset("abc");

    return 1; // To verify that the connection was accepted.
}

//---------------------------------------------------------------------------
int f_help (string arg)

// The 'help' command.

{
    debug_message(sprintf("%O: f_help()\n", this_object()));
    write(
"  help     - Prints this message\n"
"  shutdown - shuts down the driver\n"
"  flag     - passes the argument to the flag() function\n"
"  echo     - tests the input_to() function\n"
"  gc       - performes a garbage collection\n"
"  upd      - reloads the master object\n"
"  quit     - terminates the connection, but leaves the driver running\n"
    );
    return 1;
}

//---------------------------------------------------------------------------
int f_flag (string arg)

// The 'flag' command.

{
    debug_message(sprintf("%O: f_flag()\n", this_object()));
    flag(arg);
    return 1;
}

//---------------------------------------------------------------------------
int f_gc (string arg)

// The 'gc' command.

{
    write("Requested a garbage collection.\n");
    garbage_collection();
    return 1;
}

//---------------------------------------------------------------------------
int f_echo (string arg)

// The 'echo' command.

{
    debug_message(sprintf("%O: f_echo()\n", this_object()));
    input_to("echoline", 4, "Please enter a line: ");
    return 1;
}

//---------------------------------------------------------------------------
void echoline (string text)

// The user entered some text. Echo it.

{
    debug_message(sprintf("%O: echoline()\n", this_object()));
    write("You entered: '"+text+"'\n");
}

//---------------------------------------------------------------------------
int f_charmode (string arg)
{
    write("Entering charmode. Enter 'enough' to stop.\n");
    input_to("charinput", 2);
    return 1;
}

string in;

void charinput(string char)
{
    string cmd;

    printf("  Got %O (%s)\n", char
          , implode(map(to_array(char), #'to_string),","));

    if (!in)
       in = char;
    else if (!strlen(char) || char[0] == 13)
    {
        cmd = in;
        in = char[1..];
    }
    else
    {
        in += char;
    }
    if (cmd !="enough")
        input_to("charinput", 2);
    else
        write("Ok.\n");
} /* charinput() */

//---------------------------------------------------------------------------
int f_shutdown (string arg)

// The 'shutdown' command.

{
    debug_message(sprintf("%O: f_shutdown()\n", this_object()));
    write("Shutting down.\n");
    shutdown();
    return 1;
}

//---------------------------------------------------------------------------
int f_upd (string arg)

// The 'upd' command.

{
    debug_message(sprintf("%O: f_upd()\n", this_object()));
    write("Removing old master...\n");
    destruct(find_object(__MASTER_OBJECT__));
    write("Loading master again...\n");
    load_object(__MASTER_OBJECT__);
    write("...done.\n");
    return 1;
}

//---------------------------------------------------------------------------
int f_quit (string arg)

// The 'quit' command.

{
    debug_message(sprintf("%O: f_quit()\n", this_object()));
    write("Bye-bye.\n");
    destruct(this_object());
    return 1;
}

