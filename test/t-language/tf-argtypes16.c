#pragma strong_types, save_types, pedantic
inherit "ti-argtypes";

// Putting it all into a xvarargs argument with a wrong type should throw.
varargs void fun_varargs(varargs string* first) {}
