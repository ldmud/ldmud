#pragma warn_deprecated

float fun(float var)
{
    return var || 1.0;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
