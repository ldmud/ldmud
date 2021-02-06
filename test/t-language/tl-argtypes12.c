#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Adding an argument should be okay
varargs void fun_varargs(string first, object second, closure third) {}

int run_test() { return 1; }
