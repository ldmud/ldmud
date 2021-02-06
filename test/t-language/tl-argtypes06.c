#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Removing the xvarargs argument should be okay
void fun_xvarargs(string first, object second) {}

int run_test() { return 1; }
