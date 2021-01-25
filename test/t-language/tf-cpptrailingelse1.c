// Trailing characters after an #else should throw an error.

#pragma pedantic

#if 1
#else this should fail
#endif
