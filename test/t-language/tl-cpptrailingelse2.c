// Trailing comments after an #else should not throw an error.

#pragma pedantic

#if 0
#else /* this should not fail */
#endif

int run_test()
{
    return 1;
}
