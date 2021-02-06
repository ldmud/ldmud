#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Removing arguments should be okay.
varargs void fun_varargs_xvarargs(string first) {}

int run_test() { return 1; }
