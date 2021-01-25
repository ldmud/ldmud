// Trailing characters after an #endif should throw an error.

#pragma pedantic

#if 0
#else
#endif this should fail
