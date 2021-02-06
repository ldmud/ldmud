#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Making the last argument xvarargs should be okay
varargs void fun_varargs(string first, varargs object* second) {}

int run_test() { return 1; }
