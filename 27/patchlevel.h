#ifndef __PATCHLEVEL_H__
#define __PATCHLEVEL_H__ 1

/* $Format: "#define RELEASE_DATE \"$ReleaseDate$\""$ */
#define RELEASE_DATE "Tue, 01 Dec 1998 16:32:19 -0700"

/* $Format: "#define PROJ_VERSION \"$ProjectVersion$\""$ */
#define PROJ_VERSION "3-2-dev.27"

/* $Format: "#define GAME_VERSION \"$ReleaseVersion$\""$ */
#define GAME_VERSION "3.2.5"

/* $Format: "#define LONG_VERSION \"$ReleaseVersion$-$ReleaseType$.$ProjectMinorVersion$\""$ */
#define LONG_VERSION "3.2.5-dev.27"

/* $Format: "#define LOCAL_LEVEL \"\""$ */
#define LOCAL_LEVEL ""

/* TODO: Add something like the perl local patchlevel management. */
/* TODO: Add LPC defines for the various version number parts */
/* TODO: Make the necessary mods for PRCS */

/* $Format: "#define IS_RELEASE() (!strcmp(\"$ReleaseType$\", \"rel\"))"$ */
#define IS_RELEASE() (!strcmp("dev", "rel"))

#endif /* __PATCHLEVEL_H__ */

