#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Adding an argument is not okay, if the type changes
void fun_xvarargs(string first, object second, int third, int fourth, closure fifth) {}
