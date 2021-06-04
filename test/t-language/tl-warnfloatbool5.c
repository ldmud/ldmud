#pragma warn_deprecated

float fun(float var)
{
    var &&= 1.0;
    return var;
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
