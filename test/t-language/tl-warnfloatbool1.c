#pragma warn_deprecated

int fun(float var)
{
    if (var)
        return 1;
    return -1;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
