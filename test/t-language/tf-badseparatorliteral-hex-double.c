/* Test that base 16 numeric literals with more than one concurrent separators
 * fail to load.
 */
#pragma strong_types, save_types, pedantic

int a = 0x10''00; // illegal.
