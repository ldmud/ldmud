#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Changing the xvarargs type should not be okay.
void fun_def_xvarargs(string first, object second = this_object(), varargs symbol* rest) {}
