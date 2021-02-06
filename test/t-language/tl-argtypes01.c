#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Adding an argument should be okay.
void fun(string first, object second, closure cl) {}

int run_test() { return 1; }
