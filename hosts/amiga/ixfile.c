/* hosts/amiga/ixfile.c
**
** The functions needed to let the driver believe into a Unix-like
** filesystem.
** This is a separate file since its used by make_func as well.
** Do NOT include hosts/amiga/ixfile.h here!
**
**   30-Nov-93 [lars] Extracted from amiga.c
**   16-Dec-93 [lars] SystemTagList() for 1.3 extracted from amiga.c
*/

/*-----------------------------------------------------------------------*/

#include <exec/types.h>

#ifdef INCLUDE_VERSION
#include <dos/dos.h>
#include <clib/dos_protos.h>
#else
#include <libraries/dos.h>
#endif

#include <strings.h>

/*-----------------------------------------------------------------------
** char *do_ixconvert (char *name)
** char *ixconvert (char *fname)
** char *ixconvert2 (char *fname)
**
**   do_ixconvert() takes a unix filename and amigaizes by changing it.
**
**   ixconvert() and ixconvert2() are the interface, each providing
**   an own static buffer for the changed name, thus keeping the original
**   intact.
*/

#define BUFLEN 1024

char *do_ixconvert (char *name) {
  char *s1;
  int flag;

  flag = 0;
  while (!flag) {
      /* Replace ':/' by ':' */
    if ((s1 = strstr(name, ":/")) != NULL) strcpy(s1+1, s1+2);
      /* Replace ':./' by ':' */
    else if ((s1 = strstr(name, ":./")) != NULL) strcpy(s1+1, s1+3);
    else flag = 1;
  }
    /* Replace ':../' by ':/' */
  if ((s1 = strstr(name, ":../")) != NULL) strcpy(s1+1, s1+3);

    /* Remove leading '/' */
  for (s1 = name; *s1 == '/'; s1++);
  if (s1 != name) strcpy(name, s1);

    /* Replace '../' by '/' */
  for (s1 = name; (s1 = strstr(s1, "../")) != NULL; strcpy(s1, s1+2));

    /* Replace './' by '' */
  for (s1 = name; (s1 = strstr(s1, "./")) != NULL; strcpy(s1, s1+2));

    /* Remove trailing '/.' */
  while ((flag=strlen(name)) >= 2 && !strcmp (s1 = name+flag-2, "/."))
    *s1 = '\0';

    /* Replace trailing '/..' by '/' */
  if (strlen(name) >= 3 && !strcmp (s1 = name+strlen(name)-3, "/.."))
    strcpy(s1, "/");

    /* Replace '..' name by '/' */
  if (!strcmp(name, "..")) strcpy(name, "/");

    /* Replace '.' name by '' */
  if (!strcmp(name, ".")) *name = '\0';
  return name;
}

char *ixconvert (char *fname) {
  static char name[BUFLEN];

  if (strlen(fname) < BUFLEN) strcpy (name, fname);
  else { strncpy(name, fname, BUFLEN-1); name[BUFLEN-1] = '\0'; }
  return do_ixconvert (name);
}

char *ixconvert2 (char *fname) {
  static char name[BUFLEN];

  if (strlen(fname) < BUFLEN) strcpy (name, fname);
  else { strncpy(name, fname, BUFLEN-1); name[BUFLEN-1] = '\0'; }
  return do_ixconvert (name);
}

/*-----------------------------------------------------------------------
** Set the access mode of a file.
** For Amiga-OS, only the owner access can be set.
** It is not guaranteed that the mode will be checked with any OS
** older than 2.0.
*/

int chmod (char *file, long mode) {
  BPTR lock;
  struct FileInfoBlock *info;
  int rc;

  /* Unix mode 'rwx??????' => DOS mode 'rwxw' aka 'rwxd'.
  ** Well, this should be done using the FIB* constants from dos.h
  ** but they will hardly change and 'knowing' them keeps this a one-liner.
  ** Note that the Amiga-OS bits disallow the operation when set.
  */
  mode = ~((mode & 0700) >> 5 | (mode & 0200) >> 7) & 017;
  info = (struct FileInfoBlock *) malloc(sizeof (struct FileInfoBlock));
  if (info == NULL) return -1;
  rc = -1;
  if ((lock = Lock(file, SHARED_LOCK)) == NULL) goto chmod_exit;
  if (Examine(lock, info) == DOSFALSE) goto chmod_exit;
  UnLock(lock);
  if (SetProtection(file, (info->fib_Protection & (~017)) | mode) != DOSFALSE)
    rc = 0;
chmod_exit:
  free (info);
  return rc;
}

/*-----------------------------------------------------------------------*/

#if defined(_DCC) && !defined(INCLUDE_VERSION) && !defined(DICE206)

/*-----------------------------------------------------------------------
** Not used with OS 1.3, but statically mentioned with the lib.
** It is put here instead of amiga.c because make_func needs it as well,
** and amiga.c includes references to signal.c and others.
*/

LONG SystemTagList (UBYTE *command, /* struct TagItem * */ void *tags) {
  return 0;
}

#endif

/*************************************************************************/
