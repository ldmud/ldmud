#pragma warn_unused_values

async void afun()
{
}

void fun()
{
    afun();
}

int run_test()
{
    return __MASTER_OBJECT__.warning_occured();
}
