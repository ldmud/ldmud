// Trailing comments after an #ifdef should not throw an error.

#pragma pedantic

#ifdef WHATEVER /* this should not fail */
#endif

int run_test()
{
    return 1;
}
