#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Putting it all into a xvarargs argument should be okay
void fun(varargs <string|object>* args) {}

int run_test() { return 1; }
