// The pragma should prevent calling the sefun.
#pragma no_simul_efuns

int run_test()
{
    return increment(10) == 12;
}
