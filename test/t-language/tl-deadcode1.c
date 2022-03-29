#pragma warn_dead_code

int fun()
{
    return 20;

    1;
    2;
    3;

    return 30;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
