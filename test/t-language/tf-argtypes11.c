#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Removing non-optional arguments should not be okay.
void fun_def_xvarargs() {}
