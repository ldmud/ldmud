#ifndef __PATCHLEVEL_H__
#define __PATCHLEVEL_H__ 1

#define RELEASE_DATE "Mon Sep 21 23:45:00 BST 1998"

/* Version of the game in the form <ver>.<rev>.<patchlvl>, without
 * leading zeroes.
 */
#define GAME_VERSION "3.2"
#define PATCH_LEVEL ".3"
/* $Format: "#define LOCAL_LEVEL \"\""$ */
#define LOCAL_LEVEL ""

/* Sketch of versioning:
 * 
 * The gd is versioned as <version>.<revision>.<patchlevel>
 * Even revisions are stable releases - any increase in patchlevel signify
 * recent bugfixes, but not new features.
 * Odd revisions are development releases - increase in patchlevel signify
 * new features, mirrored bugfixes from the stable release and bugfixes
 * for new features.
 *
 * The versions are stored in prcs, with the main development versioned
 * as <x>.<y>. <x> increases with every change in <version> and <revision>,
 * <y> increases with every checkin (that means: <y> != <patchlevel>).
 * Every release is additionally checked in as
 * <version>-<revision>.<patchlevel>. As a result, the releases are documented
 * and archived in their own branches, off the main development tree.
 *
 * To identify every release and intermediate version, an 'internal' version
 * number is introduced, consisting of
 * <version>.<revision>.<patchlevel>-<tag>.<y>
 * <tag> is 'release' for official releases, and 'dev' for interim stages of
 * the driver.
 *
 * Subbranches, to keep various packages up to date, are kept as:
 * <version>-<revision>-<package>.<patchlevel>. This does not allow
 * intermediate checkins, of course.
 */

/* TODO: Add something like the perl local patchlevel management. */
/* TODO: Add LPC defines for the various version number parts */
/* TODO: Make the necessary mods for PRCS */

#endif /* __PATCHLEVEL_H__ */

