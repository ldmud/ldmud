#pragma warn_applied_functions

/* H_COMMAND hook, this should be okay. */
protected int command(varargs <string|object>* args)
{
    return 0;
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
