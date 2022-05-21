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
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#define generic_dirent dirent
#define DIRENT_NLENGTH(dirent) (strlen((dirent)->d_name))

#if defined(CYGWIN)
extern int lstat(const char *, struct stat *);
#endif

/*-------------------------------------------------------------------------*/

#include "files.h"

#include "array.h"
#include "comm.h"
#include "filestat.h"
#include "interpret.h"
#include "lex.h"  /* lex_close() */
#include "main.h"
#include "mempools.h"
#include "mstrings.h"
#include "simulate.h"
#include "stdstrings.h"
#include "strfuns.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/driver_hook.h"
#include "../mudlib/sys/files.h"

/*-------------------------------------------------------------------------*/
static Bool
isdir (const char *path)

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
copy_file (const char *from, const char *to, int mode)

/* Copy the file <from> to <to> with access <mode>.
 * Return 0 on success, 1 or errno on failure.
 */

{
    int fromfd, tofd;
    ssize_t count;
    char buf[4096];
    
    fromfd = ixopen(from, O_RDONLY);
    if (fromfd < 0)
    {
        debug_message("copy_file(): can't open '%s': %s\n", from, strerror(errno));
        return 1;
    }
    
    /* We have to unlink 'to', because it may be a symlink.
       O_CREAT won't remove that. */
    if (unlink(to) < 0 && errno != ENOENT)
    {
        debug_message("copy_file(): can't unlink '%s': %s\n", to, strerror(errno));
        close(fromfd);
        return 1;
    }
    
    tofd = ixopen3(to, O_WRONLY|O_CREAT|O_TRUNC, mode);
    if (tofd < 0)
    {
        debug_message("copy_file(): can't open '%s': %s\n", to, strerror(errno));
        close(fromfd);
        return 1;
    }

#ifdef HAVE_FCHMOD     
    /* We have given the file mode to ixopen3, this is just to counter umask.
       So don't worry if this fchmod fails. */
    fchmod(tofd, mode);
#endif                                                                                                                                                                           
    
    do
    {
        ssize_t written;
        
        count = read(fromfd, buf, sizeof(buf));
        if (count < 0)
        {
            debug_message("copy_file(): can't read from '%s': %s\n", from, strerror(errno));
            close(fromfd);
            close(tofd);
            unlink(to);
            return 1;
        }
        
        written = 0;
        while (written < count)
        {
            ssize_t len;
            
            len = write(tofd, buf + written, count - written);
            if (len <= 0)
            {
                debug_message("copy_file(): can't write to '%s': %s\n", to, strerror(errno));
                close(fromfd);
                close(tofd);
                unlink(to);
                return 1;
            }
            
            written += len;
        }
    } while (count > 0);

    FCOUNT_READ(from);
    FCOUNT_WRITE(to);

    close(fromfd);
    close(tofd);

#ifndef HAVE_FCHMOD
    chmod(to, mode);
#endif

    return 0;
} /* copy_file() */

/*-------------------------------------------------------------------------*/
static int
move_file (const char *from, const char *to)

/* Move the file or directory <from> to <to>, copying it if necessary.
 * Result is 0 on success, 1 or errno on failure.
 */

{
    struct stat fromstat, tostat;
    
    if (lstat(from, &fromstat) < 0)
    {
        debug_message("move_file(): can't lstat '%s': %s\n", from, strerror(errno));
        return 1;
    }
    
    if (!lstat(to, &tostat))
    {
        if (fromstat.st_dev == tostat.st_dev
         && fromstat.st_ino == tostat.st_ino)
        {
            /* Same file. */
            debug_message("move_file(): '%s' and '%s' are the same.\n", from, to);
            return 1;
        }
        
        if (S_ISDIR(tostat.st_mode))
        {
            debug_message("move_file(): destination '%s' is a directory.\n", to);
    	    return 1;    
        }
    }
    
    if (rename(from, to))
    {
        if (errno == EXDEV)
        {
            if (!S_ISREG(fromstat.st_mode))
            {
                debug_message("move_file(): can't move '%s' across filesystems: Not a regular file.\n", to);
                return 1;
            }
            
            if (copy_file(from, to, fromstat.st_mode & (S_IRWXU|S_IRWXG|S_IRWXO)))
                return 1;
            
            unlink(from);
        }
        else
        {
            debug_message("move_file(): can't rename '%s' to '%s': %s\n", to, from, strerror(errno));
            return 1;
        }
    }

    FCOUNT_DEL(from);
    
    return 0;
} /* move_file() */

/*-------------------------------------------------------------------------*/

/* If your system's native methods can provide a speedier directory access
 * than opendir/readdir/closedir(), implement them as xopendir() and friends,
 * and define XDIR to the datastructure you need.
 */

#ifndef XDIR

/* Use the normal Posix calls */

struct xdirect
{
    /* inode and position in directory aren't usable in a portable way,
     * so why support them anyway?
     */
    short d_namlen;
    char  *d_name;
    int   size;
    int   time;
    int   atime;
    int   mode;
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
 * <mask> is tested for GETDIR_SIZES, GETDIR_DATES, GETDIR_ACCESS,
 * GETDIR_MODES - only the data for requested items is returned.
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
    if (mask & (GETDIR_SIZES|GETDIR_DATES|GETDIR_ACCESS|GETDIR_MODES) )
    {
        if (ixstat(xde.d_name, &st) == -1) /* who knows... */
        {
            xde.size = FSIZE_NOFILE;
            xde.time = 0;
            xde.atime = 0;
            xde.mode = 0;
        }
        else
        {
            if (S_IFDIR & st.st_mode)
                xde.size = FSIZE_DIR;
            else
                xde.size = st.st_size;
            xde.time = st.st_mtime;
            xde.atime = st.st_atime;
            xde.mode = st.st_mode;
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
    return mstrcmp(((svalue_t*)p1)->u.str, ((svalue_t*)p2)->u.str);
} /* pstrcmp() */


/*-------------------------------------------------------------------------*/
struct get_dir_error_context
{
    error_handler_t head;
    XDIR *dirp;
    vector_t *v;
};

/*-------------------------------------------------------------------------*/
static void
get_dir_error_handler (error_handler_t *arg)

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

/* Error handling structure to close a iconv conversion descriptor.
 */

struct iconv_error_context
{
    error_handler_t head;
    iconv_t cd;
};

/*-------------------------------------------------------------------------*/
static void
iconv_error_handler (error_handler_t *arg)

/* T_ERROR_HANDLER function: <arg> is a (struct iconv_error_context*)
 * with a conversion descriptor that needs to be closed.
 * If the conversion descriptor is -1, then nothing needs to be done.
 */

{
    struct iconv_error_context *ecp = (struct iconv_error_context *)arg;

    if (iconv_valid(ecp->cd))
        iconv_close(ecp->cd);
    xfree(ecp);

} /* iconv_error_handler() */

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
 * occurs, a non-0 value is returned.
 *
 * TODO: Add two more args: start, length to implement slicing?
 * TODO:: See f-981229-10 "truncate_file()".
 * TODO: Return useful error messages.
 */

{
    struct stat to_stats, from_stats;
    string_t *path;
    int result;

    /* Check the arguments */

    do
    {
        char fromB[MAXPATHLEN+1];
        char toB[MAXPATHLEN+1];

        result = 1; /* Default: failure */

        path = check_valid_path(sp[-1].u.str, current_object, STR_COPY_FILE
                               , MY_FALSE);

        if (!path)
            break;

        if (!convert_path_to_native_buf(get_txt(path), mstrsize(path), fromB, sizeof(fromB)))
        {
            push_string(inter_sp, path);
            errorf("Could not encode path '%s'.\n", get_txt(path));
        }
        free_mstring(path);

        if (isdir(fromB))
            break;


        path = check_valid_path(sp->u.str, current_object, STR_COPY_FILE
                             , MY_TRUE);
        if (!path)
            break;

        if (!mstrsize(path) && mstreq(sp->u.str, STR_SLASH))
        {
            strcpy(toB, "./");
        }
        else if (!convert_path_to_native_buf(get_txt(path), mstrsize(path), toB, sizeof(toB)))
        {
            push_string(inter_sp, path);
            errorf("Could not encode path '%s'.\n", get_txt(path));
        }

        free_mstring(path);

        strip_trailing_slashes(fromB);

        if (isdir(toB))
        {
            /* Target is a directory; build full target filename. */

            size_t tolen = strlen(toB);
            char *cp = strrchr(fromB, '/');
            if (cp)
                cp++;
            else
                cp = fromB;

            if (tolen + strlen(cp) + 2 > sizeof(toB))
                errorf("Target file name too long.\n");

            toB[tolen++] = '/';
            strcpy(toB + tolen, cp);
        }

        /* Now copy the file */

        if (lstat(fromB, &from_stats) != 0)
        {
            char *strFromB = convert_path_from_native(fromB, strlen(fromB));
            errorf("%s: lstat failed\n", strFromB == NULL ? get_txt(sp[-1].u.str) : strFromB);
            break;
        }

        if (lstat(toB, &to_stats) == 0)
        {
            if (from_stats.st_dev == to_stats.st_dev
              && from_stats.st_ino == to_stats.st_ino)
            {
                char tmp[MAXPATHLEN];
                size_t fromLen = convert_path_from_native_buf(fromB, strlen(fromB), tmp, sizeof(tmp));
                char *strToB = convert_path_from_native(toB, strlen(toB));

                errorf("'%s' and '%s' are the same file\n",
                    fromLen ? tmp : get_txt(sp[-1].u.str),
                    strToB == NULL ? get_txt(sp[0].u.str) : strToB);
                break;
            }

            if (S_ISDIR(to_stats.st_mode))
            {
                char *strToB = convert_path_from_native(toB, strlen(toB));
                errorf("%s: cannot overwrite directory\n", strToB == NULL ? get_txt(sp[0].u.str) : strToB);
                break;
            }

        }
        else if (errno != ENOENT)
        {
            char *strToB = convert_path_from_native(toB, strlen(toB));
            perror("copy_file");
            errorf("%s: unknown error\n", strToB == NULL ? get_txt(sp[0].u.str) : strToB);
            break;
        }

        if (!S_ISREG(from_stats.st_mode))
        {
            char *strFromB = convert_path_from_native(fromB, strlen(fromB));
            errorf("cannot copy '%s': Not a regular file\n", strFromB == NULL ? get_txt(sp[-1].u.str) : strFromB);
            break;
        }

        result = copy_file(fromB, toB, from_stats.st_mode & 0777);
    }while(0);

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
    string_t * file;

    st.st_mode = 0; /* Silences ZeroFault/AIX under high optimizations */
    file = check_valid_path(sp->u.str, current_object, STR_FILE_SIZE, MY_FALSE);
    if (!file)
        len = FSIZE_NOFILE;
    else
    {
        char *native = convert_path_to_native(get_txt(file), mstrsize(file));

        if (!native || ixstat(native, &st) == -1)
            len = FSIZE_NOFILE;
        else if (S_IFDIR & st.st_mode)
            len = FSIZE_DIR;
        else
            len = (long)st.st_size;
    }

    free_mstring(file);
    free_svalue(sp);
    put_number(sp, len);

    return sp;
} /* f_file_size() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_get_dir (svalue_t *sp)

/* EFUN get_dir()
 *
 *     string *get_dir(string str)
 *     string *get_dir(string str, int mask)
 *
 * This function takes a path as argument and returns an array of file
 * names and attributes in that directory.
 *
 * Returns 0 if the directory to search in does not exist.
 *
 * The filename part of the path may contain '*' or '?' as wildcards:
 * every '*' matches an arbitrary amount of characters (or just itself).
 * Thus get_dir("/path/ *") would return an alphabetically sorted array
 * of all files in directory "/path/", or just ({ "/path/ *" }) if this
 * file happens to exist.
 *
 * To query the content of a directory, use the directory name with a
 * trailing '/' or '/.', for example get_dir("/path/."). Use the
 * directory name as it is to get information about the directory itself.
 *
 * The optional second argument mask can be used to get
 * information about the specified files.
 *
 * GETDIR_EMPTY    (0x00)  get_dir returns an empty array (not very
 *                         useful).
 * GETDIR_NAMES    (0x01)  put the alphabetically sorted file names into
 *                         the returned array.
 * GETDIR_SIZES    (0x02)  put the file sizes unsorted into the returned
 *                         array. directories have size FSIZE_DIR (-2).
 * GETDIR_DATES    (0x04)  put the file modification dates unsorted into
 *                         the returned array.
 * GETDIR_ACCESS   (0x40)  put the file access dates unsorted into
 *                         the returned array.
 * GETDIR_MODES    (0x80)  put the unix file modes unsorted into
 *                         the returned array.
 *
 * GETDIR_ALL      (0xDF)  Return all.
 *
 * GETDIR_PATH     (0x10)  if this mask bit is set, the filenames with
 *                         the full path will be returned
 *                         (GETDIR_NAMES is implied).
 * GETDIR_UNSORTED (0x20)  if this mask bit is set, the result of will
 *                         _not_ be sorted.
 *
 * Note: You should use GETDIR_NAMES|GETDIR_UNSORTED to get the entries
 * in the same order as with GETDIR_SIZES and GETDIR_DATES.
 *
 * The values of mask can be added together.
 */

{
    vector_t *v;
    char      path[MAXPATHLEN+1];
    int       mask;
    string_t *fpath = NULL;

    mask = sp->u.number;

    v = NULL;
    do {
        static struct get_dir_error_context ec; /* must survive errors */

        vector_t       *w;
        int             i, j, count = 0;
        XDIR           *dirp;
        int             namelen;
        Bool            do_match = MY_FALSE;
        size_t          pathlen;
        Bool            in_top_dir = MY_FALSE;
        struct xdirect *de;
        struct stat     st;
        char           *p;
        char           *regexpr = 0;
        int             nqueries;

        /* Adjust the mask for implied bits */
        if (mask & GETDIR_PATH)
            mask |= GETDIR_NAMES;

        if (!sp[-1].u.str)
            break;

        fpath = check_valid_path(sp[-1].u.str, current_object, STR_GET_DIR, MY_FALSE);

        if (fpath == NULL)
            break;

        /* We need to modify the returned path, and thus to make a
         * writeable copy.
         */
        convert_path_to_native_buf(get_txt(fpath), mstrsize(fpath), path, sizeof(path));
        free_mstring(fpath);

        /* Convert the empty path to '.' */
        if (strlen(path) < 2)
        {
            path[0] = path[0] ? path[0] : '.';
            path[1] = '\0';
            p = path;
            in_top_dir = MY_TRUE;
        }
        else
        {
            /* If path ends with '/' or "/." remove it
             */
            if ((p = strrchr(path, '/')) == NULL)
                p = path;

            if ((p[0] == '/' && p[1] == '.' && p[2] == '\0')
             || (p[0] == '/' && p[1] == '\0')
               )
                *p = '\0';

            in_top_dir = (p == path);
        }

        /* Number of data items per file */
        nqueries =   ((mask & GETDIR_NAMES) != 0)
                   + ((mask & GETDIR_SIZES) != 0)
                   + ((mask & GETDIR_DATES) != 0)
                   + ((mask & GETDIR_ACCESS) != 0)
                   + ((mask & GETDIR_MODES) != 0)
                   ;

        if (strchr(p, '*') || ixstat(path, &st) < 0)
        {
            /* We got a wildcard and/or a directory:
             * prepare to match.
             */
            if (*p == '\0')
                break;
            regexpr = alloca(strlen(p)+2);
            if (p != path)
            {
                strcpy(regexpr, p + 1);
                *p = '\0';
            }
            else
            {
                strcpy(regexpr, p);
                strcpy(path, ".");
                in_top_dir = MY_TRUE;
            }
            do_match = MY_TRUE;
        }
        else if (*p != '\0' && strcmp(path, "."))
        {
            /* We matched a single file */

            svalue_t *stmp;

            if (*p == '/' && *(p + 1) != '\0')
                p++;
            v = allocate_array(nqueries);
            stmp = v->item;
            if (mask & GETDIR_NAMES)
            {
                if (mask & GETDIR_PATH)
                {
                    char *native = convert_path_from_native(path, strlen(path));
                    if (!native)
                    {
                        /* If we can't encode it, it doesn't exist... */
                        free_array(v);
                        v = ref_array(&null_vector);
                        break;
                    }

                    put_c_string(stmp, native);
                    if (!compat_mode)
                    {
                        string_t *tmp = stmp->u.str;
                        stmp->u.str = add_slash(tmp);
                        free_mstring(tmp);
                    }
                }
                else
                {
                    char *native = convert_path_from_native(p, strlen(p));
                    if (!native)
                    {
                        /* If we can't encode it, it doesn't exist... */
                        free_array(v);
                        v = ref_array(&null_vector);
                        break;
                    }

                    put_c_string(stmp, native);
                }
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
            if (mask & GETDIR_ACCESS)
            {
                put_number(stmp, st.st_atime);
                stmp++;
            }
            if (mask & GETDIR_MODES)
            {
                put_number(stmp, st.st_mode);
                stmp++;
            }
            break;
        }
        
        if ( XOPENDIR(dirp, path) == 0)
            break;

        /* Prepare the error handler to do clean up.
         */
        ec.dirp = dirp;
        ec.v = NULL;
        inter_sp = sp+1;
        push_error_handler(get_dir_error_handler, &ec.head);

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
            if (max_array_size && count >= (long)max_array_size)
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

        // do not prepend the path for the mudlib root directory. (effect is
        // in the loop below if GETDIR_PATH == true).
        if (in_top_dir)
            pathlen = 0;
        else
            pathlen = strlen(path);
        
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
                char result[MAXPATHLEN];
                size_t respos = 0;

                if (mask & GETDIR_PATH)
                {
                    size_t reslen;
                    if (!compat_mode)
                    {
                        result[0] = '/';
                        respos++;
                    }

                    if (pathlen)
                    {
                        reslen = convert_path_from_native_buf(path, pathlen, result + respos, sizeof(result) - respos - 1);
                        if (!reslen)
                            continue;
                        respos += reslen;
                        result[respos++] = '/';
                    }
                    if (namelen)
                    {
                        reslen = convert_path_from_native_buf(de->d_name, namelen, result + respos, sizeof(result) - respos);
                        if (!reslen)
                            continue;
                        respos += reslen;
                    }
                }
                else
                {
                    respos = convert_path_from_native_buf(de->d_name, namelen, result, sizeof(result));
                    if (!respos)
                        continue;
                }
                put_c_n_string(w->item+j, result, respos);
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
            if (mask & GETDIR_ACCESS)
            {
                put_number(w->item + j, de->atime);
                j++;
            }
            if (mask & GETDIR_MODES)
            {
                put_number(w->item + j, de->mode);
                j++;
            }
            i++;
        }
        xclosedir(dirp);

        if (i < count)
        {
            /* Some files vanished... */
            vector_t *tmp = allocate_array(j);
            svalue_t *old = v->item, *new = tmp->item;
            while (j--)
                *new++ = *old++;

            free_empty_vector(v);
            v = tmp;
        }

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
    int i = 0;
    string_t *path;

    path = check_valid_path(sp->u.str, current_object, STR_MKDIR, MY_TRUE);
    if (path != NULL)
        i = (mkdir(convert_path_str_to_native_or_throw(path), 0775) != -1);

    free_svalue(sp);
    put_number(sp, i);

    return sp;
} /* f_mkdir() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_read_bytes (svalue_t *sp, int num_arg)

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
 */

{
    string_t *rc;
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
        string_t *file;

        char *str;
        int f;
        long size; /* TODO: fpos_t? */
        char *native;

        /* Perform some sanity checks */
        if (len < 0 || (max_byte_xfer && len > max_byte_xfer))
            break;;

        file = check_valid_path(arg[0].u.str, current_object, STR_READ_BYTES, MY_FALSE);
        if (!file)
            break;

        native = convert_path_str_to_native_or_throw(file);

        /* Open the file and determine its size */
        f = ixopen(native, O_RDONLY);
        if (f < 0)
            break;;
        FCOUNT_READ(native);

        if (fstat(f, &st) == -1)
            fatal("Could not stat an open file.\n");
        size = (long)st.st_size;

        /* Determine the proper start and len to use */
        if (start < 0)
            start = size + start > 0 ? size + start : 0;

        if (start >= size) {
            close(f);
            break;;
        }
        if (num_arg < 3 || (start+len) > size)
            len = (size - start);

        if (len <= 0)
        {
            close(f);
            break;;
        }

        /* Seek and read */
        if ((size = (long)lseek(f,start, 0)) < 0) {
            close(f);
            break;;
        }

        str = mb_alloc(mbFile, (size_t)len);
        if (!str) {
            close(f);
            break;
        }

        size = read(f, str, (size_t)len);

        close(f);

        if (size <= 0) {
            mb_free(mbFile);
            break;
        }

        /* We return a copy of the life parts of the buffer, and get rid
         * of the largish buffer itself.
         */
        rc = new_n_mstring(str, size, STRING_BYTES);
        mb_free(mbFile);

    }while(0);

    free_svalue(sp--);
    if (rc == NULL)
        push_number(sp, 0);
    else
        push_bytes(sp, rc);

    return sp;
} /* v_read_bytes() */

/*-------------------------------------------------------------------------*/
static iconv_t
get_file_encoding (string_t* filename, bool source)

/* Determines the file's encoding via H_FILE_ENCODING hook.
 * Returns an iconv conversion descriptor if successful.
 * (Otherwise an error is thrown.)
 * If <source> is true, the file is opened for reading.
 */

{
    string_t *encoding = NULL;
    iconv_t cd;

    if (driver_hook[H_FILE_ENCODING].type == T_STRING)
    {
        encoding = driver_hook[H_FILE_ENCODING].u.str;
    }
    else if (driver_hook[H_FILE_ENCODING].type == T_CLOSURE)
    {
        svalue_t *svp;
        svalue_t master_sv = svalue_object(master_ob);

        /* Setup and call the closure */
        push_ref_string(inter_sp, filename);
        svp = secure_apply_lambda_ob(driver_hook+H_FILE_ENCODING, 1, &master_sv);

        if (svp && svp->type == T_STRING)
            encoding = svp->u.str;
    }

    if (source)
        cd = iconv_open("utf-8", encoding == NULL ? "ascii" : get_txt(encoding));
    else
        cd = iconv_open(encoding == NULL ? "ascii" : get_txt(encoding), "utf-8");

    if (!iconv_valid(cd))
        errorf("Unsupported encoding '%s'.\n", get_txt(encoding));

    return cd;
} /* get_file_encoding() */

/*-------------------------------------------------------------------------*/
struct iconv_file_info
{
    FILE *f;        /* The file to read. */
    char *buffer;   /* Buffer containing the read bytes from the file. */
    size_t size;    /* Size of the buffer. */
    size_t num;     /* Number of bytes read. */
    size_t left;    /* Number of bytes left to process. */
    iconv_t cd;     /* Conversion descriptor. */
    char *error;    /* Error message. */
};

static size_t
read_file_iconv (struct iconv_file_info* info, char* dest, size_t len, char* prevstr, size_t prevlen)

/* Reads <len> UTF-8 bytes from the given file into <dest>.
 * It tries to read as many characters as possible and returns
 * the number of UTF-8 bytes in <dest>.
 * Sets the <error> member in <info> upon decoding failures
 * and returns the number of bytes done until that.
 * <prevstr> and <prevlen> denote the previous read strings
 * to offer context in error messages.
 */

{
    char *bufstart = info->buffer + info->num - info->left;
    char *deststart = dest;
    size_t bufleft = info->left;
    size_t destleft = len;
    bool ineof = false;

    while (destleft)
    {
        size_t rc;
        bool fillbuf = false;

        if (bufleft)
            rc = iconv(info->cd, &bufstart, &bufleft, &deststart, &destleft);
        else if (ineof)
            rc = iconv(info->cd, NULL, NULL, &deststart, &destleft);
        else
            fillbuf = true;

        if (fillbuf || rc == (size_t)-1)
        {
            /* Incomplete sequence, read next bytes from the file. */
            if (fillbuf || errno == EINVAL)
            {
                /* It is expected that we always fill the buffer,
                 * otherwise we are at the end of the file.
                 */
                if (bufstart + bufleft == info->buffer + info->size)
                {
                    size_t num;

                    if (bufleft > 0)
                        memmove(info->buffer, bufstart, bufleft);

                    num = fread(info->buffer + bufleft, 1, info->size - bufleft, info->f);

                    bufstart = info->buffer;
                    bufleft += num;

                    if (num != 0)
                        continue;
                }

                if (fillbuf)
                {
                    ineof = true;
                    continue;
                }
                else
                {
                    info->error = "Unexpected end of file";
                    break;
                }
            }

            /* So we're finished for now? */
            if (errno == E2BIG)
                break;

            if (errno != EILSEQ)
                info->error = strerror(errno);
            else if (!bufleft)
                info->error = "Invalid character sequence";
            else
            {
                static char errmsg[256];                // The final error message
                char context[128];                      // The context string
                int pos;                                // The position in the context string

                bool indest = true;                     // Wether we search the context in <dest>
                char *contextstr = dest, *lastcontext = deststart;

                pos = sizeof(context);                  // We are going backwards.
                context[--pos] = 0;
                context[--pos] = '"';

                /* Let's generate some context... */
                for (int contextlen = 0; contextlen < 10; )
                {
                    if (lastcontext > contextstr)
                    {
                        char escbuf[16];
                        char *prev = utf8_prev(lastcontext, lastcontext - contextstr);
                        p_int c;
                        size_t clen = utf8_to_unicode(prev, lastcontext - prev, &c);
                        size_t esclen;

                        if (!clen)
                            c = *(unsigned char*)prev;

                        lastcontext = prev;
                        contextlen++;

                        esclen = get_escaped_character(c, escbuf, sizeof(escbuf));
                        if (esclen && esclen < pos)
                        {
                            pos -= esclen;
                            memcpy(context + pos, escbuf, esclen);
                        }
                    }
                    else if (indest)
                    {
                        indest = false;
                        contextstr = prevstr;
                        lastcontext = prevstr + prevlen;
                    }
                    else
                        break;
                }

                context[--pos] = '"';

                snprintf(errmsg, sizeof(errmsg), "Invalid character sequence at byte %ld after %s: %s"
                                               , ftell(info->f) - bufleft
                                               , context + pos
                                               , get_illegal_sequence(bufstart, bufleft, info->cd));
                errmsg[sizeof(errmsg)-1] = 0;

                info->error = errmsg;
            }
            break;
        }

        if (ineof)
            break;
    }

    info->num = bufstart - info->buffer + bufleft;
    info->left = bufleft;

    return len - destleft;
} /* read_file_iconv() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_read_file (svalue_t *sp, int num_arg)

/* EFUN read_file()
 *
 *   string read_file(string file, int start, int number, string encoding)
 *
 * Reads lines from file.
 * If <start> is not given or 0, the file is read from the
 * beginning, else from the numbered line on.
 * If <number> is not given or 0, the whole file is read, else
 * just the given amount of lines.
 * If <start> would be outside the actual size of the file, 0 is
 * returned instead of a string.
 * The read bytes are decoded with the charset <encoding>.
 * If not given, ask the H_FILE_ENCODING driver hook.
 */

{
    struct iconv_error_context* iec;
    string_t *rc;
    svalue_t *arg;
    int start, len;
    iconv_t cd;

    arg = sp- num_arg + 1;

    /* Get the arguments */
    start = 0;
    len = 0;
    cd = iconv_init();

    if (num_arg > 1)
    {
        start = arg[1].u.number;
        if (num_arg > 2)
        {
            len = arg[2].u.number;
            if (num_arg == 4)
            {
                cd = iconv_open("utf-8", get_txt(arg[3].u.str));
                if (!iconv_valid(cd))
                    errorf("Unsupported encoding '%s'.\n", get_txt(arg[3].u.str));

                free_mstring(arg[3].u.str);
                sp--;
            }
            sp--;
        }
        sp--;
    }

    inter_sp = sp;
    iec = xalloc(sizeof(*iec));
    if (!iec)
        errorf("(read_file) Out of memory (%zd bytes) for error handler\n", sizeof(*iec));
    iec->cd = cd;
    push_error_handler(iconv_error_handler, &(iec->head));
    sp++;

    /* Read the file */
    rc = NULL;

    do
    {
        struct stat st;
        string_t *file;
        struct iconv_file_info conv;
        char *native;
        char *str, *p, *p2, *end, c;
        size_t size, num = 0;

        p = NULL; /* Silence spurious warnings */
        end = NULL;

        if (len < 0 && len != -1)
            break;

        file = check_valid_path(arg[0].u.str, current_object, STR_READ_FILE, MY_FALSE);
        if (!file)
            break;

        if (!iconv_valid(cd))
        {
            push_string(inter_sp, file); /* In case of an error. */
            iec->cd = cd = get_file_encoding(arg[0].u.str, true);
            inter_sp--;
        }

        native = convert_path_str_to_native_or_throw(file);

        /* If the file would be opened in text mode, the size from fstat would
         * not match the number of characters that we can read.
         */
        conv.f = fopen(native, "rb");
        if (conv.f == NULL)
            break;
        FCOUNT_READ(native);

        /* Check if the file is small enough to be read. */

        if (fstat(fileno(conv.f), &st) == -1)
        {
            fatal("Could not stat an open file.\n");
            /* NOTREACHED */
            break;;
        }

        size = st.st_size;
        if (max_file_xfer && size > max_file_xfer)
        {
            if ( start || len )
                size = max_file_xfer;
            else
            {
                fclose(conv.f);
                break;;
            }
        }

        /* Make the arguments sane */
        if (!start) start = 1;
        if (!len) len = INT_MAX;

        /* Get the memory */
        conv.size = size;

        /* A UTF-8 character is at most 4 bytes long,
         * so using that for our buffer. */
        size *= 4;

        str = mb_alloc(mbFile, conv.size + size + 1); /* allow a leading ' ' */
        if (!str)
        {
            fclose(conv.f);
            errorf("(read_file) Out of memory (%zd bytes) for buffer\n", size+1);
            /* NOTREACHED */
            break;
        }
        *str++ = ' '; /* this way, we can always read the 'previous' char... */

        conv.buffer = str + size;
        conv.num = conv.size;
        conv.left = 0;
        conv.cd = cd;
        conv.error = NULL;

        /* Search for the first line to read.
         * For this, the file is read in chunks of <size> bytes, st.st_size
         * records the remaining length of the file.
         */
        do
        {
            /* Read the next chunk */
            num = read_file_iconv(&conv, str, size, str, num);

            if (!num)
            {
                fclose(conv.f);
                conv.f = NULL;
                mb_free(mbFile);
                if (conv.error)
                    errorf("%s.\n", conv.error);
                break;
            }

            end = str + num;

            /* Find all the '\n' in the chunk and count them */
            for (p = str; NULL != ( p2 = memchr(p, '\n', (size_t)(end-p)) ) && --start; )
                p = p2+1;

        } while ( start > 1 );

        if (conv.f == NULL) /* then the inner loop aborted and we have to, too */
            break;

        /* p now points to the first requested line.
         *
         * Shift the found lines back to the front of the buffer, and
         * count them.
         */
        for (p2 = str; p != end; ) {
            c = *p++;
            if ( c == '\n' ) {
                if (!--len) {
                    *p2++=c;
                    break;
                }
            }
            *p2++ = c;
        }

        /* If there are still some lines missing, and parts of the file
         * are not read yet, read and scan those remaining parts.
         *
         * len is the number of lines still to read.
         * p2 is the position where to append the remaining data.
         */

        if ( len )
        {
            /* Read the remaining file, but only as much as there is
             * space left in the buffer. As that one is max_file_xfer
             * long, it has to be sufficient.
             */
            if (p2 - str < str + num - p2)
                end = p2 + read_file_iconv(&conv, p2, str + size - p2, p2, str + num - p2);
            else
                end = p2 + read_file_iconv(&conv, p2, str + size - p2, str, p2 - str);

            /* Count the remaining lines.
             */
            for (p = p2; p != end; ) {
                c = *p++;
                if ( c == '\n' ) {
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
            if ( len > 0 && (conv.left > 0 || ftell(conv.f) < st.st_size))
            {
                /* tried to read more than READ_MAX_FILE_SIZE */
                fclose(conv.f);
                mb_free(mbFile);
                if (conv.error)
                    errorf("%s.\n", conv.error);
                break;
            }
        }

        fclose(conv.f);

        /* If we are above max_file_xfer, that's also a failure. */
        if (max_file_xfer && byte_to_char_index(str, p2-str, NULL) > max_file_xfer)
        {
            mb_free(mbFile);
            break;
        }

        /* Make a copy of the valid parts of the str buffer, then
         * get rid of the largish buffer itself.
         */
        rc = new_n_unicode_mstring(str, p2-str);
        mb_free(mbFile);
        if (!rc)
            errorf("(read_file) Out of memory for result\n");
    } while(0);

    /* Free the error handler and the first argument. */
    free_svalue(sp--);
    free_svalue(sp--);
    if (rc == NULL)
        push_number(sp, 0);
    else
        push_string(sp, rc);

    return sp;
} /* v_read_file() */

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
 * occurs, a non-0 value is returned.
 * TODO: Return useful error messages.
 */

{
    int rc = 1;
    char from[MAXPATHLEN+1], to[MAXPATHLEN+1];

    do {
        string_t *path;

        path = check_valid_path(sp[-1].u.str, current_object, STR_RENAME_FROM, MY_TRUE);
        if (!path)
            break;

        if (!convert_path_to_native_buf(get_txt(path), mstrsize(path), from, sizeof(from)))
        {
            push_string(inter_sp, path);
            errorf("Could not encode path '%s'.\n", get_txt(path));
        }
        free_mstring(path);

        path = check_valid_path(sp->u.str, current_object, STR_RENAME_TO, MY_TRUE);
        if (!path)
            break;

        if (!mstrsize(path) && mstreq(sp->u.str, STR_SLASH))
        {
            strcpy(to, "./");
        }
        else if (!convert_path_to_native_buf(get_txt(path), mstrsize(path), to, sizeof(to)))
        {
            push_string(inter_sp, path);
            errorf("Could not encode path '%s'.\n", get_txt(path));
        }
        free_mstring(path);

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
            rc = move_file(from, newto);
            break;
        }

        /* File to file move */
        rc = move_file(from, to);
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
    string_t *path;

    path = check_valid_path(sp->u.str, current_object, STR_REMOVE_FILE, MY_TRUE);

    i = 0;
    if (path != 0 && unlink(get_txt(path)) != -1)
    {
        FCOUNT_DEL(get_txt(path));
        i = 1;
    }

    if (path != NULL)
        free_mstring(path);
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
    int i = 0;
    string_t *path;

    path = check_valid_path(sp->u.str, current_object, STR_RMDIR, MY_TRUE);
    if (path != NULL)
        i = (rmdir(convert_path_str_to_native_or_throw(path)) != -1);

    free_svalue(sp);
    put_number(sp, i);

    return sp;
} /* f_rmdir() */

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
    int rc = 0;

    do {
        struct stat st;
        string_t * file;
        mp_int size, len,  start;
        int f;
        char *native;

        start = sp[-1].u.number;

        /* Sanity checks */
        file = check_valid_path(sp[-2].u.str, current_object, STR_WRITE_BYTES, MY_TRUE);
        if (!file)
            break;

        native = convert_path_str_to_native_or_throw(file);

        len = mstrsize(sp->u.str);
        if (max_byte_xfer && len > max_byte_xfer)
            break;

        f = ixopen(native, O_WRONLY);
        if (f < 0)
            break;
        FCOUNT_WRITE(native);

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

        size = write(f, get_txt(sp->u.str), (size_t)len);

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
v_write_file (svalue_t *sp, int num_arg)

/* EFUN write_file()
 *
 *   int write_file(string file, string str, int flags, string encoding)
 *
 * Append the string str to the file <file>. Returns 1 for success
 * and 0 if any failure occurred.
 *
 * If <flags> is 1, the file is first removed; thus effectively
 * changing the 'append' into an 'overwrite'.
 *
 * Write the string with <encoding> to the file. If it is not given,
 * ask the H_FILE_ENCODING driver hook.
 */

{
    struct iconv_error_context* iec;
    svalue_t *arg = sp - num_arg + 1;
    iconv_t cd = iconv_init();
    int flags = 0;
    int rc = 0;

    if (num_arg > 2)
    {
        flags = arg[2].u.number;
        if (num_arg > 3)
        {
            cd = iconv_open(get_txt(arg[3].u.str), "utf-8");
            if (!iconv_valid(cd))
                errorf("Unsupported encoding '%s'.\n", get_txt(arg[3].u.str));
            free_mstring(arg[3].u.str);
            sp--;
        }
        sp--;
    }

    inter_sp = sp;
    iec = xalloc(sizeof(*iec));
    if (!iec)
        errorf("(write_file) Out of memory (%zd bytes) for error handler\n", sizeof(*iec));
    iec->cd = cd;
    push_error_handler(iconv_error_handler, &(iec->head));
    sp++;

    do
    {
        FILE *f;
        string_t *file;
        char *native;

        file = check_valid_path(arg[0].u.str, current_object, STR_WRITE_FILE, MY_TRUE);
        if (!file)
            break;

        push_ref_string(inter_sp, file); /* Save the reference for later... */
        native = convert_path_str_to_native_or_throw(file);

        if (!iconv_valid(cd))
            iec->cd = cd = get_file_encoding(arg[0].u.str, false);

        if (flags & 1)
            if (remove(native) && errno != ENOENT)
            {
                perror("write_file (remove)");
                errorf("Could not remove %s: errno %d.\n", get_txt(file), errno);
            }

        f = fopen(native, "a");
        if (f == NULL)
        {
            if ((errno == EMFILE
  #ifdef ENFILE
                 || errno == ENFILE
                ) && current_loc.file
  #endif
              )
            {
                /* We are called from within the compiler, probably to write
                 * an error message into a log.
                 * Call lex_close() (-> lexerror() -> yyerror() ->
                 * parse_error() -> apply_master_ob() ) to try to close some
                 * files, then try again.
                 */
                lex_close(NULL);
                f = fopen(native, "a");
            }

            if (f == NULL)
            {
                char * emsg, * buf, * buf2;
                int err = errno;

                emsg = strerror(errno);
                buf = alloca(strlen(emsg)+1);
                buf2 = alloca(mstrsize(file)+1);
                if (buf && buf2)
                {
                    strcpy(buf, emsg);
                    extract_cstr(buf2, file, mstrsize(file)+1);
                    errorf("Could not open %s for append: (%d) %s.\n"
                         , buf2, err, buf);
                }
                else if (buf2)
                {
                    extract_cstr(buf2, file, mstrsize(file)+1);
                    perror("write_file");
                    errorf("Could not open %s for append: errno %d.\n"
                         , buf2, err);
                }
                else
                {
                    perror("write_file");
                    errorf("Could not open file for append: errno %d.\n"
                         , err);
                }
                /* NOTREACHED */
            }
        }
        FCOUNT_WRITE(native);
        free_string_svalue(inter_sp--);

        {
            size_t inbufleft = mstrsize(arg[1].u.str);
            size_t outbufsize = inbufleft;
            char *inbuf = get_txt(arg[1].u.str);
            char *outbuf;
            bool atend = false;

            if (outbufsize < 16)
                outbufsize = 16;

            outbuf = mb_alloc(mbFile, outbufsize);
            if (!outbuf)
            {
                fclose(f);
                errorf("(write_file) Out of memory (%zd bytes) for buffer\n", outbufsize);
            }

            while (true)
            {
                char * outbufptr = outbuf;
                size_t outbufleft = outbufsize;
                size_t res;

                if (inbufleft)
                    res = iconv(cd, &inbuf, &inbufleft, &outbufptr, &outbufleft);
                else
                {
                    res = iconv(cd, NULL, NULL, &outbufptr, &outbufleft);
                    atend = true;
                }

                if (outbufptr != outbuf)
                    fwrite(outbuf, outbufptr - outbuf, 1, f);

                if (res == (size_t)-1)
                {
                    if (errno != E2BIG && (inbufleft || errno != EILSEQ))
                    {
                        mb_free(mbFile);
                        fclose(f);
                        errorf("%s.\n", strerror(errno));
                    }
                }
                else if (atend)
                    break;
            }

            mb_free(mbFile);
        }

        fclose(f);
        rc = 1;
    } while(0);

    free_svalue(sp--);
    free_svalue(sp--);
    free_svalue(sp);
    put_number(sp, rc);

    return sp;
} /* v_write_file() */

/***************************************************************************/

