#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Removing an argument should be okay.
varargs void fun_varargs(string first) {}

int run_test() { return 1; }
