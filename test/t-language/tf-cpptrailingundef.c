// Trailing characters after an #undef should throw an error.

#pragma pedantic

#define WHATEVER
#undef WHATEVER this should fail
