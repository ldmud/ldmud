#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Putting it all into a xvarargs argument should be okay
varargs void fun_varargs(varargs <string|object>* first) {}

int run_test() { return 1; }
