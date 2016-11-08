/* This is a more complicated setup:
 *
 * This file is used alongside a inherit of b.c (a1.c).
 * It contains c.c (a2.c), but we cross-define some functions
 * of a2.c to another inherit (a3.c). Then all functions
 * of a1.c are patched to the newer inherit (a2.c).
 * And because the the first inherit has priority over
 * any later inherited functions, the cross definitions
 * shouldn't matter anymore.
 */
inherit "a3";
TEST_VIRTUAL inherit "c";
