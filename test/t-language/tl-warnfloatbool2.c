#pragma warn_deprecated

void fun(float var)
{
    while (var)
    {
        var -= var;
    }
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
