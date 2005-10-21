#ifndef PATCHLEVEL_H__
#define PATCHLEVEL_H__ 1

/* $Format: "#if $ReleaseMinor$ / 2 == 0"$ */
#if 3 / 2 == 0
#    define IS_STABLE 1
#else
#    define IS_DEVELOPMENT 1
#endif

/* $Format: "#define IS_RELEASE() (!strcmp(\"$ReleaseType$\", \"rel\"))"$ */
#define IS_RELEASE() (!strcmp("dev", "rel"))

/* $Format: "#define RELEASE_DATE \"$ProjectDate$\""$ */
#define RELEASE_DATE "Thu, 24 Aug 2000 23:45:24 -0600"

/* $Format: "#define PROJ_VERSION \"$ProjectVersion$\""$ */
#define PROJ_VERSION "3-3.2"

#if IS_STABLE

/* $Format: "#define GAME_VERSION \"$ReleaseVersion$\""$ */
#define GAME_VERSION "3.3.0"

/* $Format: "#define LONG_VERSION \"$ReleaseVersion$-$ReleaseType$.$ProjectMinorVersion$\""$ */
#define LONG_VERSION "3.3.0-dev.2"

#else

/* $Format: "#define GAME_VERSION \"$DevelopmentVersion$\""$ */
#define GAME_VERSION "3.3.$ProjectMinor$"

/* $Format: "#define LONG_VERSION \"$DevelopmentVersion$\""$ */
#define LONG_VERSION "3.3.$ProjectMinor$"

#endif

/* $Format: "#define LOCAL_LEVEL \"\""$ */
#define LOCAL_LEVEL ""

/* TODO: Add something like the perl local patchlevel management. */
/* TODO: Add LPC defines for the various version number parts */

#endif /* PATCHLEVEL_H__ */

