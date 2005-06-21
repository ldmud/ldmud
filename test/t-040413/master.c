/* Testscenario 040413
 *
 * Test two lookup issues with structs.
 * The object "a" should load without an error.
 */

//---------------------------------------------------------------------------
void inaugurate_master (int arg)

// Initialise the master object.
// We have to set the uid hooks, otherwise we can't clone a login object.

{
    set_driver_hook(2, unbound_lambda(({}), "uid"));
    set_driver_hook(3, unbound_lambda(({}), "uid"));
    set_driver_hook(10, "What?\n");

    call_out("flag", 2, "test");
}

//---------------------------------------------------------------------------
string get_simul_efun ()

// Load the simul-efun object "/sefun" if existing and return its pathname.

{
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
        catch(load_object("a"); publish);
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
