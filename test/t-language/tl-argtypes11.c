#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Removing optional arguments should be okay.
void fun_def_xvarargs(string first) {}

int run_test() { return 1; }
