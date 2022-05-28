/* Test that base 16 numeric literal with leading separator fails to load.
 */
#pragma strong_types, save_types, pedantic

int a = 0x'1000; // illegal.
