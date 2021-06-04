#pragma warn_deprecated

int fun(float var)
{
    return !var;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
