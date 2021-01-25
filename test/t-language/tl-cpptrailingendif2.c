// Trailing comments after an #endif should not throw an error.

#pragma pedantic

#if 0
#else
#endif /* this should not fail */

int run_test()
{
    return 1;
}
