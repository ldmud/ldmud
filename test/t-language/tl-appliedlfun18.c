#pragma warn_applied_functions

/* H_CREATE_LWOBJECT hook, wrong return type. */
object create_lwobject()
{
    return 0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
