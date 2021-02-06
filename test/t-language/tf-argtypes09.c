#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Changing the xvarargs type and removing the varargs should not be okay.
void fun_xvarargs(string first, object second, symbol rest) {}
