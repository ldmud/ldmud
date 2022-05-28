/* Test that base 16 numeric literal with trailing separator fails to load.
 */
#pragma strong_types, save_types, pedantic

int a = 0x1000'; // illegal.
