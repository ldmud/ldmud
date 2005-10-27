/* Testscenario 030925
 *
 * Shadowing a public sefun with a private lfun, which was then virtually
 * inherited twice in parallel, used to crash the driver.
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
    if (arg == "test")
    {
        load_object("/d");
        debug_message("/d loaded\n");
        garbage_collection();
        call_out("flag", 2, "shutdown");
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

