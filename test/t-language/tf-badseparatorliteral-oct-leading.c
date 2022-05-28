/* Test that base 8 numeric literal with leading separator fails to load.
 */
#pragma strong_types, save_types, pedantic

int a = 0o'1000; // illegal.
