#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Making optional arguments non-option should be okay.
void fun_def_xvarargs(string first, object second, varargs int* rest) {}

int run_test() { return 1; }
