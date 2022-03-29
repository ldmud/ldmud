#pragma warn_applied_functions

/* H_CREATE_LWOBJECT hook, this is okay. */
static void create_lwobject(int arg1, string arg2, object arg3)
{
    return 0;
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
