// Just check that the pragma works.
#pragma no_simul_efuns

int run_test()
{
#pragma simul_efuns
    return increment(10) == 12;
#pragma no_simul_efuns
}
