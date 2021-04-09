#pragma warn_missing_return

int fun()
{
    switch(random(10))
    {
        case 0:
            break; /* This should warn. */
        case 1..2:
            return 2;
        default:
            return 3;
    }
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
