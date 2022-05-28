/* Test that base 8 numeric literal with trailing separator fails to load.
 */
#pragma strong_types, save_types, pedantic

int a = 0o1000'; // illegal.
