#pragma warn_missing_return

int fun()
{
    switch(random(10))
    {
        case 1..2:
            return 2;
        default:
            return 0;
    }
}

int run_test()
{
    return !__MASTER_OBJECT__.warning_occured();
}
