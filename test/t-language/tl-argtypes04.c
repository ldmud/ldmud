#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Making non-optional arguments optional should be okay
void fun(string first, object second = this_object()) {}

int run_test() { return 1; }
