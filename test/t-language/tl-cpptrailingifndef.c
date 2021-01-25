// Trailing comments after an #ifndef should not throw an error.

#pragma pedantic

#ifndef WHATEVER /* this should not fail */
#endif

int run_test()
{
    return 1;
}
