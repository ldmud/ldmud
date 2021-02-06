#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Changing the xvarargs type should not be okay.
void fun_xvarargs(string first, object second, varargs symbol* rest) {}
