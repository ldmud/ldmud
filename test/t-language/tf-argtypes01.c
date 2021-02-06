#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Removing a non-optional argument should throw an error
void fun(string first) {}
