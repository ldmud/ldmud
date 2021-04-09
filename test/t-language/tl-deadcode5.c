#pragma warn_dead_code

#define notreached() raise_error("This code shouldn't be reached.\n");

int fun()
{
    if (random(3))
        return 30;
    else
        return 0;

    notreached();
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
