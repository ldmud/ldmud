// Trailing comments after an #if should not throw an error.

#pragma pedantic

#if 1 /* this should not fail */
#endif

int run_test()
{
    return 1;
}
