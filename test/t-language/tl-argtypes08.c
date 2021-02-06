#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Adding an argument should be okay, as long the types match.
void fun_def_xvarargs(string first, object second = this_object(), int third = 42, varargs int* rest) {}

int run_test() { return 1; }
