// Trailing characters after an #if should throw an error.

#pragma pedantic

#if 1 this should fail
#endif
