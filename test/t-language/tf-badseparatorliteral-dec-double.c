/* Test that base 10 numeric literals with more than one concurrent separators
 * fail to load.
 */
#pragma strong_types, save_types, pedantic

int a = 10''00; // illegal.
