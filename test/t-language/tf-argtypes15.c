#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Changing the argument type and making it xvarargs should not be okay.
varargs void fun_varargs(string first, varargs closure* second) {}
