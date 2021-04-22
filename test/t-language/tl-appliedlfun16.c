#pragma warn_applied_functions

/* H_CREATE_LWOBJECT hook, this is okay. */
protected void create_lwobject()
{
    return 0;
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
