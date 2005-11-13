#ifndef __PATCHLEVEL_H__
#define __PATCHLEVEL_H__ 1

/* $Format: "#define RELEASE_DATE \"$Date$\""$ */
#define RELEASE_DATE "Mon, 09 Nov 1998 15:03:08 -0700"

/* $Format: "#define PROJ_VERSION \"$ProjectVersion$\""$ */
#define PROJ_VERSION "3-2-dev.18"

/* $Format: "#define GAME_VERSION \"$ReleaseVersion$\""$ */
#define GAME_VERSION "3.2.5"

/* $Format: "#define LONG_VERSION \"$ReleaseVersion$-$ReleaseType$.$ProjectMinor$\""$ */
#define LONG_VERSION "3.2.5-dev.$ProjectMinor$"

/* $Format: "#define LOCAL_LEVEL \"\""$ */
#define LOCAL_LEVEL ""

/* TODO: Add something like the perl local patchlevel management. */
/* TODO: Add LPC defines for the various version number parts */
/* TODO: Make the necessary mods for PRCS */

/* $Format: "#define IS_RELEASE() (!strcmp(\"$ReleaseType$\", \"rel\"))"$ */
#define IS_RELEASE() (!strcmp("dev", "rel"))

#endif /* __PATCHLEVEL_H__ */

