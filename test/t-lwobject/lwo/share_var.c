#pragma save_types, strong_types, rtt_checks, lightweight, share_variables

// We'll check that the variables will be indeed copied from the blueprint
// and not initialized with __INIT.

int var1;

int init_var1()
{
    var1 = 10;
    return 20;
}

int var2 = init_var1();

void create()
{
    // Initialization of the blueprint.
    var1 = 30;
    var2 = 40;
}

int check_lwobject()
{
    return var1 == 30 && var2 = 40;
}
