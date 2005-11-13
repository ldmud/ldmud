#ifndef PATCHLEVEL_H__
#define PATCHLEVEL_H__ 1

/*--------------------------------------------------------------------------
 * Various version numbers and strings, collected here so that PRCS has
 * to change only one file.
 *--------------------------------------------------------------------------
 */

/* $Format: "#define RELEASE_DATE \"$ProjectDate$\""$ */
#define RELEASE_DATE "Sun, 11 Aug 2002 00:00:15 -0600"

/* $Format: "#define PROJ_VERSION \"$ProjectVersion$\""$ */
#define PROJ_VERSION "3-2-dev.444"

/* $Format: "#define GAME_VERSION \"$ReleaseVersion$\""$ */
#define GAME_VERSION "3.2.9"

/* $Format: "#define LONG_VERSION \"$ReleaseVersion$-$ReleaseType$.$ProjectMinorVersion$\""$ */
#define LONG_VERSION "3.2.9-dev.444"

/* $Format: "#define VERSION_MAJOR \"$ReleaseMajor$\""$ */
#define VERSION_MAJOR "3"

/* $Format: "#define VERSION_MINOR \"$ReleaseMinor$\""$ */
#define VERSION_MINOR "2"

/* $Format: "#define VERSION_MICRO \"$ReleaseMicro$\""$ */
#define VERSION_MICRO "9"

/* $Format: "#define VERSION_PATCH \"$ProjectMinorVersion$\""$ */
#define VERSION_PATCH "444"

/* $Format: "#define LOCAL_LEVEL \"\""$ */
#define LOCAL_LEVEL ""

/* TODO: Add something like the perl local patchlevel management. */
/* TODO: Add LPC defines for the various version number parts */

/* $Format: "#define IS_RELEASE() (!strcmp(\"$ReleaseType$\", \"rel\"))"$ */
#define IS_RELEASE() (!strcmp("dev", "rel"))

#endif /* PATCHLEVEL_H__ */

