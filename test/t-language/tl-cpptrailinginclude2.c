// Trailing comments after an #include should not throw an error.

#pragma pedantic

#include <include_list.h> /* this should not fail */

int run_test()
{
    return 1;
}
