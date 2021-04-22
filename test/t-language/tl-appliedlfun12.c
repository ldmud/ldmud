#pragma warn_applied_functions

/* H_COMMAND hook, wrong return type. */
protected string command(string cmd, object giver)
{
    return 0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
