#pragma warn_dead_code

int fun()
{
    return 20;

    int x = 40;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
