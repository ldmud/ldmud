// Trailing characters after an #ifndef should throw an error.

#pragma pedantic

#ifndef WHATEVER this should fail
#endif
