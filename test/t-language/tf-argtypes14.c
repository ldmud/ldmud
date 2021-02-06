#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Changing the argument type should not be okay.
varargs void fun_varargs(string first, closure second) {}
