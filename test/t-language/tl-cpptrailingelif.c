// Trailing comments after an #elif should not throw an error.

#pragma pedantic

#if 0
#elif 1 /* this should not fail */
#endif

int run_test()
{
    return 1;
}
