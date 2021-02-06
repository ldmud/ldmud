#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Adding an argument is not okay, if the type changes
void fun_def_xvarargs(string first, object second = this_object(), closure third = 0, varargs int* rest) {}
