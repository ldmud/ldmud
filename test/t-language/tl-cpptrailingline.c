// Trailing comments after an #line should not throw an error.

#pragma pedantic

#line 10 /* this should not fail */

int run_test()
{
    return 1;
}
