// Trailing characters after an #endif should throw an error.

#pragma pedantic

#if 1
#else
#endif this should fail
