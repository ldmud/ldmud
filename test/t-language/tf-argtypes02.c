#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Changing the argument type should throw an error
void fun(string first, closure second) {}
