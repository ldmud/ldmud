#pragma warn_dead_code

int fun()
{
    return 30;;;;
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
