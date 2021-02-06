#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Adding an argument is not okay, if the type changes
varargs void fun_varargs_xvarargs(string first, object second, symbol third, varargs int* rest) {}
