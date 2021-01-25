// Trailing characters after an #elif should throw an error.

#pragma pedantic

#if 0
#elif 1 this should fail
#endif
