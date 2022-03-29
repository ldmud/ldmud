#pragma warn_dead_code

int fun()
{
    while (1)
    {
        break;
        return 20;
    }

    return 30;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
