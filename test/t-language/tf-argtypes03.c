#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Changing the argument type should throw an error, also with xvarargs
void fun(string first, varargs closure* second) {}
