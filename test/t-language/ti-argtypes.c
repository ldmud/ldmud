/* Define several functions for testing overloading. */
#pragma strong_types, save_types

void fun(string first, object second) {}
void fun_xvarargs(string first, object second, varargs int* rest) {}
void fun_def_xvarargs(string first, object second = this_object(), varargs int* rest) {}
varargs void fun_varargs(string first, object second) {}
varargs void fun_varargs_xvarargs(string first, object second, varargs int* rest) {}
