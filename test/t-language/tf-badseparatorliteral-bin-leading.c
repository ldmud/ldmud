/* Test that base 2 numeric literals with leading separator fails to load.
 */
#pragma strong_types, save_types, pedantic

int a = 0b'1000; // illegal.
