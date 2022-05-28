/* Test that base 10 numeric literals with trailing separator fails to load.
 */
#pragma strong_types, save_types, pedantic

int a = 1000'; // illegal.
