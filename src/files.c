/*---------------------------------------------------------------------------
 * File and directory functions and efuns.
 *
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#if defined(HAVE_DIRENT_H) || defined(_POSIX_VERSION)
#    include <dirent.h>
#    define generic_dirent dirent
#    define DIRENT_NLENGTH(dirent) (strlen((dirent)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#    define generic_dirent direct
#    define DIRENT_NLENGTH(dirent) ((dirent)->d_namlen)
#    ifdef HAVE_SYS_NDIR_H
#        include <sys/ndir.h>
#    endif /* SYSNDIR */
#    ifdef HAVE_SYS_DIR_H
#        include <sys/dir.h>
#    endif /* SYSDIR */
#    ifdef HAVE_NDIR_H
#        include <ndir.h>
#    endif /* NDIR */
#endif /* not (HAVE_DIRENT_H or _POSIX_VERSION) */

#if defined(__CYGWIN__)
extern int lstat(const char *, struct stat *);
#endif

#ifndef S_ISDIR
#    define S_ISDIR(m) (((m)&S_IFMT) == S_IFDIR)
#endif

#ifndef S_ISREG
#    define S_ISREG(m) (((m)&S_IFMT) == S_IFREG)
#endif

#ifdef SunOS4
#    if !defined (__GNUC__) || __GNUC__ < 2 || __GNUC__ == 2 && __GNUC_MINOR__ < 7
extern int lstat PROT((CONST char *, struct stat *));
#    endif
extern int fchmod PROT((int, int));
#endif

#if defined(OS2)
#    define lstat stat
#endif

/*-------------------------------------------------------------------------*/

#define USES_SVALUE_STRLEN
#include "files.h"

#include "array.h"
#include "comm.h"
#include "filestat.h"
#include "interpret.h"
#include "lex.h"  /* lex_close() */
#include "main.h"
#include "simulate.h"
#include "smalloc.h" /* svalue_strlen() */
#include "svalue.h"
#include "xalloc.h" /* string_copy() */

#include "../mudlib/sys/files.h"

/*-------------------------------------------------------------------------*/
static Bool
isdir (char *path)

/* Helper function for copy and move: test if <path> is a directory.
 */

{
    struct stat stats;

    return ixstat (path, &stats) == 0 && S_ISDIR (stats.st_mode);
} /* isdir() */

/*-------------------------------------------------------------------------*/
static void
strip_trailing_slashes (char *path)

/* Strip trailing slashed from <path>, which is modified in-place.
 */

{
    int last;

    last = strlen (path) - 1;
    while (last > 0 && path[last] == '/')
        path[last--] = '\0';
} /* strip_trailing_slashes() */

/*-------------------------------------------------------------------------*/
static int
copy_file (char *from, char *to, int mode)

/* Copy the file <from> to <to> with access <mode>. 
 * Return 0 on success, 1 or errno on failure.
 */

{
    int ifd;
    int ofd;
    char buf[1024 * 8];
    int len;                        /* Number of bytes read into `buf'. */

    if (unlink(to) && errno != ENOENT)
    {
        error("cannot remove `%s'\n", to);
        return 1;
    }

    ifd = ixopen3(from, O_RDONLY | O_BINARY, 0);
    if (ifd < 0)
    {
        error("%s: open failed\n", from);
        return errno;
    }

    ofd = ixopen3(to, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, 0600);
    if (ofd < 0)
    {
        error("%s: open failed\n", to);
        close(ifd);
        return 1;
    }

#ifdef HAVE_FCHMOD
    if (fchmod(ofd, mode))
    {
        error("%s: fchmod failed\n", to);
        close(ifd);
        close(ofd);
        unlink(to);
        return 1;
    }
#endif

    FCOUNT_READ(from);
    FCOUNT_WRITE(to);

    while ((len = read(ifd, buf, sizeof (buf))) > 0)
    {
        int wrote = 0;
        char *bp = buf;

        do
        {
            wrote = write(ofd, bp, len);
            if (wrote < 0)
            {
                error("%s: write failed\n", to);
                close(ifd);
                close(ofd);
                unlink(to);
                return 1;
            }
            bp += wrote;
            len -= wrote;
        } while (len > 0);
    }

    if (len < 0)
    {
        error("%s: read failed\n", from);
        close(ifd);
        close(ofd);
        unlink(to);
        return 1;
    }

    if (close (ifd) < 0)
    {
        error("%s: close failed", from);
        close(ofd);
        return 1;
    }

    if (close (ofd) < 0)
    {
        error("%s: close failed", to);
        return 1;
    }

#ifndef HAVE_FCHMOD
    if (chmod (to, mode))
    {
        error("%s: chmod failed\n", to);
        return 1;
    }
#endif

    return 0;
} /* copy_file() */

/*-------------------------------------------------------------------------*/
static int
move_file (char *from, char *to)

/* Move the file or directory <from> to <to>, copying it if necessary.
 * Result is 0 on success, 1 or errno on failure.
 */

{
    struct stat to_stats, from_stats;

    if (lstat(from, &from_stats) != 0)
    {
        error("%s: lstat failed\n", from);
        return 1;
    }

    if (lstat (to, &to_stats) == 0)
    {
        if (from_stats.st_dev == to_stats.st_dev
          && from_stats.st_ino == to_stats.st_ino)
        {
            error("`%s' and `%s' are the same file", from, to);
            return 1;
        }

        if (S_ISDIR (to_stats.st_mode))
        {
            error("%s: cannot overwrite directory", to);
            return 1;
        }

    }
    else if (errno != ENOENT)
    {
        perror("do_move");
        error("%s: unknown error\n", to);
        return 1;
    }
#ifndef RENAME_HANDLES_DIRECTORIES
    /* old SYSV */
    if (isdir(from))
    {
        char cmd_buf[100];

        if (strchr(from, '\'') || strchr(to, '\''))
            return 0;
        sprintf(cmd_buf, "/usr/lib/mv_dir '%s' '%s'", from, to);
        return system(cmd_buf);
    }
    else
#endif /* RENAME_HANDLES_DIRECTORIES */
    if (rename (from, to) == 0)
    {
        FCOUNT_DEL(from);
        return 0;
    }

#if !defined(AMIGA)
    if (errno != EXDEV)
    {
        error("cannot move `%s' to `%s'", from, to);
        return 1;
    }
#endif

    /* rename failed on cross-filesystem link.  Copy the file instead. */

    if (!S_ISREG(from_stats.st_mode))
    {
        error("cannot move `%s' across filesystems: Not a regular file\n", from);
        return 1;
    }

    if (copy_file(from, to, from_stats.st_mode & 0777))
       return 1;

    if (unlink(from))
    {
        error("cannot remove `%s'", from);
        return 1;
    }
    FCOUNT_DEL(from);

    return 0;
} /* move_file() */

/*-------------------------------------------------------------------------*/
#ifdef atarist
/* this code is provided to speed up ls() on an Atari ST/TT . */

#include <support.h>
#include <limits.h>
#include <osbind.h>

extern long _unixtime PROT((unsigned, unsigned));

struct xdirect {
    /* inode and position in directory aren't usable in a portable way,
     * so why support them anyway?
     */
    short d_namlen;
    char  d_name[16];
    int   size;
    int   time;
};

typedef struct
{
    _DTA dta;
    char *dirname;
    long status;
} xdir;
#define XDIR xdir

static long olddta;

/*-------------------------------------------------------------------------*/
static XDIR *xopendir(path)
char *path;
{
    char pattern[MAXPATHLEN+1];
    XDIR *d;
    long status;

    d = (XDIR *)xalloc(sizeof(XDIR));
    _unx2dos(path, pattern);
    strcat(pattern, "\\*.*");
    olddta = Fgetdta();
    Fsetdta(&d->dta);
    d->status = status = Fsfirst(pattern, 0xff);
    if (status && status != -ENOENT) {
        xfree(d);
        return 0;
    }
    d->dirname = string_copy(pattern);
    return d;
}

/*-------------------------------------------------------------------------*/
#define XOPENDIR(dest, path) ((dest) = xopendir(path))

static struct xdirect *xreaddir(d)
XDIR *d;
{
    static struct xdirect xde;

    if (d->status)
        return 0;
    _dos2unx(d->dta.dta_name, xde.d_name);
    xde.d_namlen = strlen(xde.d_name);
    if (FA_DIR & d->dta.dta_attribute)
        xde.size = -2;
    else
        xde.size = d->dta.dta_size;
    xde.time = _unixtime(d->dta.dta_time, d->dta.dta_date);
    d->status = Fsnext();
    return &xde;
}

/*-------------------------------------------------------------------------*/
static void xclosedir(d)
XDIR *d;
{
    Fsetdta(olddta);
    xfree(d->dirname);
    xfree(d);
}

/*-------------------------------------------------------------------------*/
static void xrewinddir(d)
XDIR *d;
{
    long status;

    Fsetdta(&d->dta);
    d->status = status = Fsfirst(d->dirname, 0xff);
}

#endif /* atarist */

/*-------------------------------------------------------------------------*/
#ifndef XDIR

struct xdirect
{
    /* inode and position in directory aren't usable in a portable way,
     * so why support them anyway?
     */
    short d_namlen;
    char  *d_name;
    int   size;
    int   time;
};

#define XOPENDIR(dest, path) (\
    (!chdir(path) &&\
    NULL != ((dest) = opendir("."))) ||\
        (chdir(mud_lib),MY_FALSE)\
)

#define xclosedir(dir_ptr)   (chdir(mud_lib),closedir(dir_ptr))
#define xrewinddir(dir_ptr)  rewinddir(dir_ptr)
#define XDIR DIR

/*-------------------------------------------------------------------------*/
static struct xdirect *
xreaddir (XDIR *dir_ptr, int mask)

/* Read the next entry from <dir_ptr> and return it via a pointer
 * to a static xdirect structure.
 * <mask> is tested for GETDIR_SIZES and GETDIR_DATES - only the data
 * for requested items is returned.
 */

{
    static struct xdirect xde;
    struct generic_dirent *de;
    int namelen;
    struct stat st;

    de = readdir(dir_ptr);
    if (!de)
        return NULL;
    namelen = DIRENT_NLENGTH(de);
    xde.d_namlen = namelen;
    xde.d_name   = de->d_name;
    if (mask & (GETDIR_SIZES|GETDIR_DATES) )
    {
        if (ixstat(xde.d_name, &st) == -1) /* who knows... */
        {
            xde.size = FSIZE_NOFILE;
            xde.time = 0;
        }
        else
        {
            if (S_IFDIR & st.st_mode)
                xde.size = FSIZE_DIR;
            else
                xde.size = st.st_size;
            xde.time = st.st_mtime;
        }
    }
    return &xde;
} /* xreaddir() */

#endif /* XDIR */

/*-------------------------------------------------------------------------*/
static int
pstrcmp (const void *p1, const void *p2)

/* qsort() comparison function: strcmp() on two svalue-strings.
 */

{
    return strcmp(((svalue_t*)p1)->u.string, ((svalue_t*)p2)->u.string);
} /* pstrcmp() */


/*-------------------------------------------------------------------------*/
struct get_dir_error_context
{
    svalue_t head;
    XDIR *dirp;
    vector_t *v;
};

/*-------------------------------------------------------------------------*/
static void
get_dir_error_handler (svalue_t *arg)

/* T_ERROR_HANDLER function: <arg> is a (struct get_dir_error_context*)
 * with the directory which needs to be closed.
 */

{
    struct get_dir_error_context *ecp;

    ecp = (struct get_dir_error_context *)arg;
    xclosedir(ecp->dirp);
    if (ecp->v)
        free_array(ecp->v);
} /* get_dir_error_handler() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_cat (svalue_t *sp, int num_arg)

/* EFUN cat()
 *
 *     int cat(string pathi [, int start [, int num]])
 *
 * List the file found at path.
 * The optional arguments start and num are start line
 * number and number of lines. If they are not given the whole
 * file is printed from the beginning.
 *
 * Result is the number of lines printed.
 */

{
    int rc;
    svalue_t *arg;
    int start, len;

    arg = sp- num_arg + 1;

    /* Get and test the arguments */
    start = 0; len = 0;
    if (num_arg > 1)
    {
        start = arg[1].u.number;
        if (num_arg == 3)
        {
            len = arg[2].u.number;
        }
    }

    /* Print the file */

    rc = 0;
    do{
#       define MAX_LINES 50

        char buff[1000];
        char *path;
        FILE *f;
        int i;

        if (len < 0)
            break;

        path = check_valid_path(arg[0].u.string, current_object, "print_file", MY_FALSE);

        if (path == 0)
            break;
        if (start < 0)
            break;
        f = fopen(path, "r");
        if (f == NULL)
            break;
        FCOUNT_READ(path);

        if (len == 0)
            len = MAX_LINES;
        if (len > MAX_LINES)
            len = MAX_LINES;

        if (start == 0)
            start = 1;

        for (i = 1; i < start + len; i++)
        {
            if (fgets(buff, sizeof buff, f) == 0)
                break;
            if (i >= start)
                add_message("%s", buff);
        }
        fclose(f);

        if (i <= start)
            break;

        if (i == MAX_LINES + start)
            add_message("*****TRUNCATED****\n");

        rc = i-start;

#       undef MAX_LINES
    }while(0);

    sp = pop_n_elems(num_arg, sp);
    push_number(sp, rc);

    return sp;
} /* f_cat() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_copy_file (svalue_t *sp)

/* EFUN copy_file()
 *
 *   int copy_file(string from, string to)
 *
 * The efun rename() will copy the file <from> to the new name <to>.
 * If <to> is a directory, then <from> will be placed in that
 * directory and keep its original name.
 *
 * You must have read permission for <from> and write permission
 * for the target name to copy the file.
 *
 * On successfull completion copy_file() will return 0. If any error
 * occurs, 1 is returned, or a runtime is generated.
 *
 * TODO: Add two more args: start, length to implement slicing?
 * TODO:: See f-981229-10 "truncate_file()".
 */

{
    struct stat to_stats, from_stats;
    char *from, *to, *cp;
    int result;
    
    /* Check the arguments */

    switch(0){default:
        result = 1; /* Default: failure */

        from = check_valid_path(sp[-1].u.string, current_object, "copy_file"
                               , MY_FALSE);

        if (!from || isdir(from))
            break;

        /* We need our own copy of the result */
        cp = alloca(strlen(from)+1);
        strcpy(cp, from);
        from = cp;

        to = check_valid_path(sp->u.string, current_object, "copy_file"
                             , MY_TRUE);
        if (!to)
            break;

        if (!strlen(to) && !strcmp(sp->u.string, "/"))
        {
            to = alloca(3);
            strcpy(to, "./");
        }

        strip_trailing_slashes(from);

        if (isdir(to))
        {
            /* Target is a directory; build full target filename. */

            char *newto;

            cp = strrchr(from, '/');
            if (cp)
                cp++;
            else
                cp = from;

            newto = alloca(strlen(to) + 1 + strlen(cp) + 1);
            strcpy(newto, to);
            strcat(newto, "/");
            strcat(newto, cp);
            to = newto;
        }

        /* Now copy the file */

        if (lstat(from, &from_stats) != 0)
        {
            error("%s: lstat failed\n", from);
            break;
        }

        if (lstat(to, &to_stats) == 0)
        {
            if (from_stats.st_dev == to_stats.st_dev
              && from_stats.st_ino == to_stats.st_ino)
            {
                error("`%s' and `%s' are the same file", from, to);
                break;
            }

            if (S_ISDIR(to_stats.st_mode))
            {
                error("%s: cannot overwrite directory", to);
                break;
            }

        }
        else if (errno != ENOENT)
        {
            perror("copy_file");
            error("%s: unknown error\n", to);
            break;
        }

        if (!S_ISREG(from_stats.st_mode))
        {
            error("cannot copy `%s': Not a regular file\n", from);
            break;
        }

        result = copy_file(from, to, from_stats.st_mode & 0777);
    } /* switch(0) */

    /* Clean up the stack and return the result */
    free_svalue(sp);
    free_svalue(sp-1);
    put_number(sp-1, result);
   
    return sp-1;
} /* f_copy_file() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_file_size (svalue_t *sp)

/* EFUN file_size()
 *
 *   int file_size(string file)
 *
 * Returns the size of the file in bytes.
 *
 * Size FSIZE_NOFILE (-1) indicates that the file either does not
 * exist, or that it is not readable for the calling object/user.
 * Size FSIZE_DIR (-2) indicates that it is a directory.
 */

{
    long len;
    struct stat st;
    char * file;

    file = check_valid_path(sp->u.string, current_object, "file_size", MY_FALSE);
    if (!file || ixstat(file, &st) == -1)
        len = FSIZE_NOFILE;
    else if (S_IFDIR & st.st_mode)
        len = FSIZE_DIR;
    else
        len = (long)st.st_size;

    free_svalue(sp);
    put_number(sp, len);

    return sp;
} /* f_file_size() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_get_dir (svalue_t *sp)

/* EFUN get_dir()
 *
 *   string *get_dir(string str, int mask)
 *
 * This function takes a path as argument and returns an array of
 * file names and attributes in that directory.
 * 
 * The filename part of the path may contain '*' or '?' as
 * wildcards: every '*' matches an arbitrary amount of characters
 * (or just itself). Thus get_dir ("/path/ *") would return an
 * array of all files in directory "/path/", or just ({ "/path/ *"
 * }) if this file happens to exist.
 * 
 * The optional second argument mask can be used to get
 * information about the specified files.
 * 
 *   GETDIR_EMPTY (0x00)  get_dir returns an emtpy array (not very useful)
 *   GETDIR_NAMES (0x01)  put the file names into the returned array.
 *   GETDIR_SIZES (0x02)  put the file sizes into the returned array.
 *                        directories have size FSIZE_DIR (-2)
 *   GETDIR_DATES (0x04)  put the file modification dates into the returned
 *                        array.
 *   GETDIR_UNSORTED (0x20)  if this mask bit is set, the result of will
 *                           _not_ be sorted.
 * 
 * The values of mask can be added together.
 */

{
    vector_t *v;
    char * path;
    int mask;

    path = sp[-1].u.string;
    mask = sp->u.number;

    v = NULL;
    do {
        static struct get_dir_error_context ec; /* must survive errors */

        vector_t       *w;
        int             i, j, count = 0;
        XDIR           *dirp;
        int             namelen;
        Bool            do_match = MY_FALSE;
        struct xdirect *de;
        struct stat     st;
        char           *temppath;
        char           *p; 
        char           *regexpr = 0;
        int             nqueries;

        if (!path)
            break;;

        path = check_valid_path(path, current_object, "get_dir", MY_FALSE);

        if (path == NULL)
            break;

        /* We need to modify the returned path, and thus to make a
         * writeable copy.
         * The path "" needs 2 bytes to store ".\0".
         */
        temppath = alloca(strlen(path) + 2);
        if (strlen(path) < 2)
        {
            temppath[0] = path[0] ? path[0] : '.';
            temppath[1] = '\000';
            p = temppath;
        }
        else
        {
            strcpy(temppath, path);

            /* If path ends with '/' or "/." remove it
             */
            if ((p = strrchr(temppath, '/')) == NULL)
                p = temppath;

            if ((p[0] == '/' && p[1] == '.' && p[2] == '\0')
             || (p[0] == '/' && p[1] == '\0')
               )
                *p = '\0';
        }

        /* Number of data items per file */
        nqueries = (mask & 1) + (mask>>1 & 1) + (mask>>2 & 1);

        if (strchr(p, '*') || ixstat(temppath, &st) < 0)
        {
            /* We got a wildcard and/or a directory:
             * prepare to match.
             */
            if (*p == '\0')
                break;
            regexpr = alloca(strlen(p)+2);
            if (p != temppath)
            {
                strcpy(regexpr, p + 1);
                *p = '\0';
            }
            else
            {
                strcpy(regexpr, p);
                strcpy(temppath, ".");
            }
            do_match = MY_TRUE;
        }
        else if (*p != '\0' && strcmp(temppath, "."))
        {
            /* We matched a single file */
            
            svalue_t *stmp;

            if (*p == '/' && *(p + 1) != '\0')
                p++;
            v = allocate_array(nqueries);
            stmp = v->item;
            if (mask & GETDIR_NAMES)
            {
                put_malloced_string(stmp, string_copy(p));
                stmp++;
            }
            if (mask & GETDIR_SIZES){
                put_number(stmp, (S_IFDIR & st.st_mode) ? FSIZE_DIR : st.st_size);
                stmp++;
            }
            if (mask & GETDIR_DATES)
            {
                put_number(stmp, st.st_mtime);
                stmp++;
            }
            break;
        }

        if ( XOPENDIR(dirp, temppath) == 0)
            break;

        /* Prepare the error handler to do clean up.
         */
        ec.head.type = T_ERROR_HANDLER;
        ec.head.u.error_handler = get_dir_error_handler;
        ec.dirp = dirp;
        ec.v = NULL;
        inter_sp = sp+1;
        inter_sp->type = T_LVALUE;
        inter_sp->u.lvalue = &ec.head;

        /* Count files
         */
        for (de = xreaddir(dirp, 1); de; de = xreaddir(dirp, 1))
        {
            namelen = de->d_namlen;
            if (do_match)
            {
                if ( !match_string(regexpr, de->d_name, namelen) )
                    continue;
            }
            else
            {
                if (namelen <= 2 && *de->d_name == '.'
                 && (namelen == 1 || de->d_name[1] == '.' ) )
                    continue;
            }
            count += nqueries;
            if (max_array_size && count >= max_array_size)
                break;
        }

        if (nqueries)
            count /= nqueries;

        /* Make array and put files on it.
         */
        v = allocate_array(count * nqueries);
        if (count == 0)
        {
            /* This is the easy case :-) */
            inter_sp--;
            xclosedir(dirp);
            break;
        }

        ec.v = v;
        xrewinddir(dirp);
        w = v;
        j = 0;

        /* Taken into account that files might be added/deleted from outside. */
        for(i = 0, de = xreaddir(dirp,mask); de; de = xreaddir(dirp,mask))
        {

            namelen = de->d_namlen;
            if (do_match)
            {
                if ( !match_string(regexpr, de->d_name, namelen) )
                    continue;
            }
            else
            {
                if (namelen <= 2 && *de->d_name == '.'
                 && (namelen == 1 || de->d_name[1] == '.' ) )
                    continue;
            }
            if (i >= count)
            {
                /* New file. Don't need efficience here, but consistence. */

                vector_t *tmp, *new;

                count++;
                tmp = allocate_array(nqueries);
                new = add_array(v, tmp);
                free_array(v);
                free_array(tmp);
                ec.v = v = new;
                w = v;
            }

            if (mask & GETDIR_NAMES)
            {
                char *name;

                xallocate(name, (size_t)namelen+1, "getdir() names");
                if (namelen)
                    memcpy(name, de->d_name, namelen);
                name[namelen] = '\0';
                put_malloced_string(w->item+j, name);
                j++;
            }
            if (mask & GETDIR_SIZES)
            {
                put_number(w->item + j, de->size);
                j++;
            }
            if (mask & GETDIR_DATES)
            {
                put_number(w->item + j, de->time);
                j++;
            }
            i++;
        }
        xclosedir(dirp);
        inter_sp--;

        if ( !((mask ^ 1) & (GETDIR_NAMES|GETDIR_UNSORTED)) )
        {
            /* Sort by names. */
            qsort(v->item, i, sizeof v->item[0] * nqueries, pstrcmp);
        }

    }while(0);

    sp--; /* just an int */
    free_string_svalue(sp);

    if (v)
        put_array(sp, v);
    else
        put_number(sp, 0);

    return sp;
} /* f_get_dir() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_mkdir (svalue_t *sp)

/* EFUN mkdir()
 *
 *   int mkdir(string path)
 *
 * Make a directory named path. Return 1 for success and 0 for
 * failure.
 */

{
    int i;
    char *path;

    path = check_valid_path(sp->u.string, current_object, "mkdir", MY_TRUE);
    i = !(path == 0 || mkdir(path, 0775) == -1);
    free_svalue(sp);
    put_number(sp, i);

    return sp;
} /* f_mkdir() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_read_bytes (svalue_t *sp, int num_arg)

/* EFUN read_bytes()
 *
 *   string read_bytes (string file, int start, int number)
 *
 * Reads a given amount of bytes from file.
 * If <start> is not given or 0, the file is read from the
 * beginning, else from the <start>th byte on. If <start> is
 * negative, it is counted from the end of the file.
 * <number> is the number of bytes to read. 0 or negative values
 * are possible, but not useful.
 * If <start> would be outside the actual size of the file, 0 is
 * returned instead of a string.
 * TODO: Can't read nul-characters.
 */

{
    char *rc;
    svalue_t *arg;
    int start, len;

    arg = sp- num_arg + 1;

    /* Get the arguments */
    start = 0;
    len = 0;
    if (num_arg > 1)
    {
        start = arg[1].u.number;
        if (num_arg == 3)
        {
            len = arg[2].u.number;
            sp--;
        }
        sp--;
    }

    /* Read the file */

    rc = NULL;

    do{
        struct stat st;

        char *str, *file;
        int f;
        long size; /* TODO: fpos_t? */

        /* Perform some sanity checks */
        if (len < 0 || (max_byte_xfer && len > max_byte_xfer))
            break;;

        file = check_valid_path(arg[0].u.string, current_object, "read_bytes", MY_FALSE);
        if (!file)
            break;;

        /* Open the file and determine its size */
        f = ixopen(file, O_RDONLY);
        if (f < 0)
            break;;
        FCOUNT_READ(file);

        if (fstat(f, &st) == -1)
            fatal("Could not stat an open file.\n");
        size = (long)st.st_size;

        /* Determine the proper start and len to use */
        if (start < 0)
            start = size + start;

        if (start >= size) {
            close(f);
            break;;
        }
        if ((start+len) > size)
            len = (size - start);

        /* Seek and read */
        if ((size = (long)lseek(f,start, 0)) < 0) {
            close(f);
            break;;
        }

        str = xalloc((size_t)len + 1);
        if (!str) {
            close(f);
            break;
        }

        size = read(f, str, (size_t)len);

        close(f);

        if (size <= 0) {
            xfree(str);
            break;
        }

        /* No postprocessing, except for the adding of the '\0' without
         * the gamedriver won't be happy.
         */
        str[size] = '\0';

        /* We return a copy of the life parts of the buffer, and get rid
         * of the largish buffer itself.
         */
        rc = string_copy(str);
        xfree(str);

    }while(0);

    free_svalue(sp--);
    if (rc == NULL)
        push_number(sp, 0);
    else
        push_malloced_string(sp, rc);

    return sp;
} /* f_read_bytes() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_read_file (svalue_t *sp, int num_arg)

/* EFUN read_file()
 *
 *   string read_file(string file, int start, int number)
 *
 * Reads lines from file.
 * If <start> is not given or 0, the file is read from the
 * beginning, else from the numbered line on.
 * If <number> is not given or 0, the whole file is read, else
 * just the given amount of lines.
 * If <start> would be outside the actual size of the file, 0 is
 * returned instead of a string.
 */

{
    char *rc;
    svalue_t *arg;
    int start, len;

    arg = sp- num_arg + 1;

    /* Get the arguments */
    start = 0;
    len = 0;
    if (num_arg > 1)
    {
        start = arg[1].u.number;
        if (num_arg == 3)
        {
            len = arg[2].u.number;
            sp--;
        }
        sp--;
    }

    /* Read the file */
    rc = NULL;

    do {
        struct stat st;
        FILE *f;
        char *file, *str, *p, *p2, *end, c;
        long size; /* TODO: fpos_t? */

        p = NULL; /* Silence spurious warnings */
        end = NULL; 

        if (len < 0 && len != -1)
            break;

        file = check_valid_path(arg[0].u.string, current_object, "read_file", MY_FALSE);
        if (!file)
            return NULL;

        /* If the file would be opened in text mode, the size from fstat would
         * not match the number of characters that we can read.
         */
        f = fopen(file, "rb");
        if (f == NULL)
            break;;
        FCOUNT_READ(file);

        /* Check if the file is small enough to be read. */

        if (fstat(fileno(f), &st) == -1)
        {
            fatal("Could not stat an open file.\n");
            /* NOTREACHED */
            break;;
        }

        size = (long)st.st_size;
        if (max_file_xfer && size > max_file_xfer)
        {
            if ( start || len )
                size = max_file_xfer;
            else {
                fclose(f);
                break;;
            }
        }

        /* Make the arguments sane */
        if (!start) start = 1;
        if (!len) len = size;

        /* Get the memory */
        str = xalloc((size_t)size + 2); /* allow a trailing \0 and leading ' ' */
        if (!str) {
            fclose(f);
            error("(read_file) Out of memory (%ld bytes) for buffer\n", size+2);
            /* NOTREACHED */
            break;
        }

        *str++ = ' '; /* this way, we can always read the 'previous' char... */
        str[size] = '\0';

        /* Search for the first line to read.
         * For this, the file is read in chunks of <size> bytes, st.st_size
         * records the remaining length of the file.
         */
        do {
            /* Read the next chunk */
            if (size > st.st_size) /* Happens with the last block */
                size = (long)st.st_size;

            if ((!size && start > 1) || fread(str, (size_t)size, 1, f) != 1) {
                fclose(f);
                xfree(str-1);
                break;
            }
            st.st_size -= size;
            end = str+size;

            /* Find all the '\n' in the chunk and count them */
            for (p = str; NULL != ( p2 = memchr(p, '\n', (size_t)(end-p)) ) && --start; )
                p = p2+1;

        } while ( start > 1 );

        /* p now points to the first requested line.
         * st.st_size is the remaining size of the file.
         */

        /* Shift the found lines back to the front of the buffer, and
         * count them.
         * Also convert \r\n pairs into \n on MS-DOS filesystems.
         */
        for (p2 = str; p != end; ) {
            c = *p++;
            if ( c == '\n' ) {
    #ifdef MSDOS_FS
                if ( p2[-1] == '\r' ) p2--;
    #endif
                if (!--len) {
                    *p2++=c;
                    break;
                }
            }
            *p2++ = c;
        }

        /* If there are still some lines missing, and parts of the file
         * are not read yet, read and scan those remaining parts.
         */

        if ( len && st.st_size ) {

            /* Read the remaining file, but only as much as there is
             * space left in the buffer. As that one is max_file_xfer
             * long, it has to be sufficient.
             */

            size -= ( p2-str) ;
            if (size > st.st_size)
                size = (long)st.st_size;

            if (fread(p2, (size_t)size, 1, f) != 1) {
                fclose(f);
                xfree(str-1);
                break;
            }

            st.st_size -= size;
            end = p2+size;

            /* Count the remaining lines, again converting \r\n into \n
             * when necessary.
             */
            for (p = p2; p != end; ) {
                c = *p++;
                if ( c == '\n' ) {
    #ifdef MSDOS_FS
                    if ( p2[-1] == '\r' ) p2--;
    #endif
                    if (!--len) {
                        *p2++ = c;
                        break;
                    }
                }
                *p2++ = c;
            }

            /* If there are lines missing and the file is not at its end,
             * we have a failure.
             */
            if ( st.st_size && len > 0) {
                /* tried to read more than READ_MAX_FILE_SIZE */
                fclose(f);
                xfree(str-1);
                break;
            }
        }

        *p2 = '\0';
        fclose(f);

        /* Make a copy of the valid parts of the str buffer, then
         * get rid of the largish buffer itself.
         */
        p2 = string_copy(str); /* TODO: string_n_copy() */
        xfree(str-1);
        if (!p2)
            error("(read_file) Out of memory for result\n");

        rc = p2;
    } while(0);

    free_svalue(sp--);
    if (rc == NULL)
        push_number(sp, 0);
    else
        push_malloced_string(sp, rc);

    return sp;
} /* f_read_file() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_rename (svalue_t *sp)

/* EFUN rename()
 *
 *   int rename(string from, string to)
 *
 * The efun rename() will move from to the new name to. If from
 * is a file, then to may be either a file or a directory. If
 * from is a directory, then to has to be a directory. If to
 * exists and is a directory, then from will be placed in that
 * directory and keep its original name.
 *
 * You must have write permission for from to rename the file.
 *
 * On successfull completion rename() will return 0. If any error
 * occurs 1 is returned.
 */

{
    int rc;
    char *from, *to;

    rc = 1;
    do {

        from = check_valid_path(sp[-1].u.string, current_object, "rename_from", MY_TRUE);
        if (!from)
            break;

        push_apply_value();

        to = check_valid_path(sp->u.string, current_object, "rename_to", MY_TRUE);
        if (!to)
        {
            pop_apply_value();
            break;
        }

        if (!strlen(to) && !strcmp(sp->u.string, "/"))
        {
            to = alloca(3);
            sprintf(to, "./");
        }

        strip_trailing_slashes(from);

        if (isdir(to))
        {
            /* Target is a directory; build full target filename. */
            char *cp;
            char *newto;

            cp = strrchr(from, '/');
            if (cp)
                cp++;
            else
                cp = from;

            newto = alloca(strlen(to) + 1 + strlen(cp) + 1);
            sprintf(newto, "%s/%s", to, cp);
            pop_apply_value();
            rc = move_file(from, newto);
            break;
        }

        /* File to file move */
        rc = move_file(from, to);
        pop_apply_value();
    }while(0);

    free_svalue(sp--);
    free_svalue(sp);
    put_number(sp, rc);

    return sp;
} /* f_rename() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_rm (svalue_t *sp)

/* EFUN rm()
 *
 *   int rm(string file)
 *
 * Remove the file. Returns 0 for failure and 1 for success.
 */

{
    int i;
    char *path;

    path = check_valid_path(sp->u.string, current_object, "remove_file", MY_TRUE);

    i = 0;
    if (path != 0 && unlink(path) != -1)
    {
        FCOUNT_DEL(path);
        i = 1;
    }

    free_svalue(sp);
    put_number(sp, i);

    return sp;
} /* f_rm() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_rmdir (svalue_t *sp)

/* EFUN rmdir()
 *
 *   int rmdir(string dir)
 *
 * Remove directory dir. Return 1 on success, 0 on failure.
 */

{
    int i;
    char *path;

    path = check_valid_path(sp->u.string, current_object, "rmdir", MY_TRUE);
    i = !(path == 0 || rmdir(path) == -1);
    free_svalue(sp);
    put_number(sp, i);

    return sp;
} /* f_rmdir() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tail (svalue_t *sp)

/* EFUN tail()
 *
 *   void tail(string file)
 *
 * Print out the tail of a file. There is no specific amount of
 * lines given to the output. Only a maximum of 1000 bytes will
 * be printed.
 */

{
    int rc;

    rc = 0;

    do {
        char buff[1000];
        char *path;
        FILE *f;
        struct stat st;
        int offset;

        path = check_valid_path(sp->u.string, current_object, "tail", MY_FALSE);

        if (path == NULL)
            break;
        f = fopen(path, "r");
        if (f == NULL)
            break;
        FCOUNT_READ(path);
        if (fstat(fileno(f), &st) == -1)
            fatal("Could not stat an open file.\n");
        if ( !S_ISREG(st.st_mode) ) {
            fclose(f);
            break;
        }
        offset = st.st_size - 54 * 20;
        if (offset < 0)
            offset = 0;
        if (fseek(f, offset, 0) == -1)
            fatal("Could not seek.\n");

        /* Throw away the first incomplete line. */
        if (offset > 0)
            (void)fgets(buff, sizeof buff, f);

        while(fgets(buff, sizeof buff, f))
        {
            add_message("%s", buff);
        }
        fclose(f);
        rc = 1;
    }while(0);

    free_svalue(sp);
    put_number(sp, rc);

    return sp;
} /* f_tail() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_write_bytes (svalue_t *sp)

/* EFUN write_bytes()
 *
 *   int write_bytes(string file, int start, string str)
 *
 * Write string str to file file by overwriting the old bytes at
 * position start. If start is a negative value then it will be
 * counted from the end of the file. The file will not be
 * appended, instead the function will be aborted. Returns 1 for
 * success 0 for failure during execution.
 */

{
    int rc;

    rc = 0;

    do {
        struct stat st;
        mp_int size, len,  start;
        char * file;
        int f;

        start = sp[-1].u.number;

        /* Sanity checks */
        file = check_valid_path(sp[-2].u.string, current_object, "write_bytes", MY_TRUE);
        if (!file)
            break;

        len = svalue_strlen(sp);
        if (max_byte_xfer && len > max_byte_xfer)
            break;

        f = ixopen(file, O_WRONLY);
        if (f < 0)
            break;
        FCOUNT_WRITE(file);

        if (fstat(f, &st) == -1)
            fatal("Could not stat an open file.\n");
        size = (mp_int)st.st_size;

        if (start < 0)
            start = size + start;

        if (start > size) {
            close(f);
            break;;
        }
        if ((size = (mp_int)lseek(f,start, 0)) < 0) {
            close(f);
            break;;
        }

        size = write(f, sp->u.string, (size_t)len);

        close(f);

        if (size <= 0) {
            break;
        }

        rc = 1;
    }while(0);

    free_svalue(sp--);
    free_svalue(sp--);
    free_svalue(sp);
    put_number(sp, rc);

    return sp;
} /* f_write_bytes() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_write_file (svalue_t *sp)

/* EFUN write_file()
 *
 *   int write_file(string file, string str)
 *
 * Append the string str to the file file. Returns 1 for success
 * and 0 if any failure occured.
 */

{
    int rc;

    rc = 0;

    do {
        FILE *f;
        char *file;

        file = check_valid_path(sp[-1].u.string, current_object, "write_file", MY_TRUE);
        if (!file)
            break;

        f = fopen(file, "a");
        if (f == NULL) {
            if ((errno == EMFILE
  #ifdef ENFILE
                 || errno == ENFILE
                ) && current_file
  #endif
              ) {
                /* We are called from within the compiler, probably to write
                 * an error message into a log.
                 * Call lex_close() (-> lexerror() -> yyerror() ->
                 * parse_error() -> apply_master_ob() ) to try to close some
                 * files, the try again.
                 */
                push_apply_value();
                lex_close(NULL);
                pop_apply_value();
                f = fopen(file, "a");
            }
            if (f == NULL) {
                char * emsg, * buf;

                emsg = strerror(errno);
                buf = alloca(strlen(emsg+1));
                if (buf)
                {
                    strcpy(buf, emsg);
                    error("Could not open %s for append: %s.\n", file, buf);
                }
                else
                {
                    perror("write_file");
                    error("Could not open %s for append: errno %d.\n"
                         , file, errno);
                }
                /* NOTREACHED */
            }
        }
        FCOUNT_WRITE(file);
        fwrite(sp->u.string, svalue_strlen(sp), 1, f);
        fclose(f);
        rc = 1;
    } while(0);

    free_svalue(sp--);
    free_svalue(sp);
    put_number(sp, rc);

    return sp;
} /* f_write_file() */

/***************************************************************************/

