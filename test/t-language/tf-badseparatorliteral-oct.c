/* Test that base 8 numeric literals with more than one concurrent separators
 * fail to load.
 */
#pragma strong_types, save_types, pedantic

int a = 0o10''00; // illegal.
