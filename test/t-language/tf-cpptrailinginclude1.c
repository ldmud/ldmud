// Trailing characters after an #include should throw an error.

#pragma pedantic

#include "/sys/include_list.h" this should fail
