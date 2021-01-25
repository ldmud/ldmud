// Trailing characters after an #ifdef should throw an error.

#pragma pedantic

#ifdef WHATEVER this should fail
#endif
