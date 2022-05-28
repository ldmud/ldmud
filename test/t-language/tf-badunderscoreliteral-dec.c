/* Test that base 10 numeric literals with more than one concurrent underscores
 * fail to load.
 */
#pragma strong_types, save_types, pedantic

int a = 10__00; // illegal.
