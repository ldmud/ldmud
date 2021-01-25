// Trailing comments after an #undef should not throw an error.

#pragma pedantic

#define WHATEVER
#undef WHATEVER /* this should not fail */

int run_test()
{
    return 1;
}
