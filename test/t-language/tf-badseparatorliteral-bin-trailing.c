/* Test that numeric binary literal with a trailing separator fails to load.
 */
#pragma strong_types, save_types, pedantic

int a = 0b1000'; // illegal.
