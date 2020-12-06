/*------------------------------------------------------------------
 * Common functions for all TLS modules.
 *------------------------------------------------------------------
 */

#include "driver.h"
#include "machine.h"

#ifdef USE_TLS

#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <termios.h>

#include "pkg-tls.h"

#include "actions.h"
#include "array.h"
#include "comm.h"
#include "interpret.h"
#include "main.h"
#include "mstrings.h"
#include "object.h"
#include "sha1.h"
#include "svalue.h"
#include "xalloc.h"
     
#include "../mudlib/sys/tls.h"

/*-------------------------------------------------------------------------*/
/* Variables */

char * tls_keyfile = NULL;
char * tls_keydirectory = NULL;
char * tls_certfile = NULL;
char * tls_trustfile = NULL;
char * tls_trustdirectory = NULL;
char * tls_crlfile = NULL;
char * tls_crldirectory = NULL;
  /* The filenames of the x509 key and cert file, set by the argument
   * parser. If not set, the package will use defaults.
   */
char * tls_password = NULL;
  /* The password command line argument for the key files.
   */

/*-------------------------------------------------------------------------*/

Bool
tls_opendir (const char * dir, const char * desc, struct tls_dir_s * info)

/* Wrapper around opendir(), that prints error messages and
 * and prepares the tls_dir_s structure for use with tls_readdir.
 *
 * When successful it returns MY_TRUE and info->dir will not be NULL.
 * After success tls_readdir() must(!) be called until it returns NULL,
 * so all resources are closed and freed.
 *
 * If <desc> is given then (in any case) a message will be printed
 * to the log with <desc> as the description of the file type.
 */

{
    info->dir = NULL;
    if (!dir)
        return MY_FALSE;

    if (desc)
    {
        /* Inform the user where we looked for his files. */
        printf("%s TLS: %s from directory '%s'.\n"
              , time_stamp(), desc, dir);
        debug_message("%s TLS: %s from directory '%s'.\n"
                     , time_stamp(), desc, dir);
    }

    /* Initialize fname and dirlen with the directory name,
     * so readdir later has just to append the plain filename.
     */
    info->dirlen = strlen(dir);
    info->fname = (char*) xalloc(info->dirlen + NAME_MAX + 2);
    if (!info->fname)
    {
        errno = ENOMEM;
    }
    else
    {
        strcpy(info->fname, dir);
        info->fname[info->dirlen++] = '/';
        info->dir = opendir(dir);
    }

    if (info->dir == NULL)
    {
        if (desc)
        {
            printf("%s TLS: Can't read %s directory: %s.\n"
                  , time_stamp(), desc, strerror(errno));
            debug_message("%s TLS: Can't read %s directory: %s\n"
                         , time_stamp(), desc, strerror(errno));
        }

        if(info->fname)
            xfree(info->fname);

        return MY_FALSE;
    }

    return MY_TRUE;
}

/*-------------------------------------------------------------------------*/
const char *
tls_readdir (struct tls_dir_s * info)

/* Wrapper around readdir() that looks for a regular file and
 * returns the concatenation of the directory and file name.
 * tls_opendir() must be called prior to this to initialize <info>.
 *
 * Returns NULL at the end of the directory and then frees
 * all variables in the tls_dir_s structure.
 */

{
    struct dirent *file;

    /* Are we already finished? This shouldn't happen. */
    if (info->dir == NULL)
        return NULL;

    while ((file = readdir((DIR*)info->dir)) != NULL)
    {
        struct stat st;

        strcpy(info->fname+info->dirlen, file->d_name);
        stat(info->fname, &st);

        if (S_ISREG(st.st_mode))
            return info->fname;
    }

    closedir((DIR*)info->dir);
    info->dir = NULL;
    xfree(info->fname);
    return NULL;
}

/*-------------------------------------------------------------------------*/
ssize_t
tls_get_password (char* buf, size_t size)

/* Read the TLS key password into <buf> which has <size> bytes.
 * The password will be null-terminated, the size (without the null
 * byte) will be returned, -1 for no password.
 */

{
    FILE *file = NULL;
    size_t len;
    struct termios settings;
    bool termios_changed = false;

    if (tls_password == NULL)
        return -1;
    else if (!strcmp(tls_password, "none"))
        return -1;
    else if (!strncmp(tls_password, "pass:", 5))
    {
        len = strlen(tls_password + 5);
        if (len >= size)
        {
            printf("%s TLS: Password too long.\n", time_stamp());
            debug_message("%s TLS: Password too long.\n", time_stamp());

            return -1;
        }

        memcpy(buf, tls_password + 5, len + 1);

        return (ssize_t)len;
    }
    else if (!strncmp(tls_password, "env:", 4))
    {
        const char* pwd = getenv(tls_password + 4);
        if (pwd == NULL)
        {
            printf("%s TLS: Can't read environment variable '%s' for password.\n", time_stamp(), tls_password + 4);
            debug_message("%s TLS: Can't read environment variable '%s' for password.\n", time_stamp(), tls_password + 4);

            return -1;
        }
        else
        {
            len = strlen(pwd);
            if (len >= size)
            {
                printf("%s TLS: Password too long.\n", time_stamp());
                debug_message("%s TLS: Password too long.\n", time_stamp());

                return -1;
            }

            memcpy(buf, pwd, len + 1);

            return (ssize_t)len;
        }
    }
    else if (!strncmp(tls_password, "file:", 5))
    {
        file = fopen(tls_password + 5, "rt");
        if (!file)
        {
            printf("%s TLS: Can't read password file '%s': %s.\n", time_stamp(), tls_password + 5, strerror(errno));
            debug_message("%s TLS: Can't read password file '%s': %s.\n", time_stamp(), tls_password + 5, strerror(errno));

            return -1;
        }
    }
    else if (!strncmp(tls_password, "fd:", 3))
    {
        int fd = atoi(tls_password + 3);
        if (fd < 0)
        {
            printf("%s TLS: Illegal file descriptor '%s' for password.\n", time_stamp(), tls_password + 3);
            debug_message("%s TLS: Illegal file descriptor '%s' for password.\n", time_stamp(), tls_password + 3);

            return -1;
        }

        file = fdopen(fd, "rt");
        if (!file)
        {
            printf("%s TLS: Can't open file descriptor %d: %s.\n", time_stamp(), fd, strerror(errno));
            debug_message("%s TLS: Can't open file descriptor %d: %s.\n", time_stamp(), fd, strerror(errno));

            return -1;
        }
    }
    else if (!strcmp(tls_password, "stdin"))
    {
        file = stdin;
        if (!file)
        {
            printf("%s TLS: Standard input for password is not available.\n", time_stamp());
            debug_message("%s TLS: Standard input for password is not available.\n", time_stamp());

            return -1;
        }

        if (!tcgetattr(fileno(file), &settings))
        {
            if (settings.c_lflag & ECHO)
            {
                termios_changed = true;
                settings.c_lflag &= ~ECHO;

                tcsetattr(fileno(file), TCSANOW, &settings);
            }
        }
        fprintf(stderr, "Please enter password for TLS key: ");
    }
    else
    {
        printf("%s TLS: Unknown scheme for the password.\n", time_stamp());
        debug_message("%s TLS: Unknown scheme for the password.\n", time_stamp());

        return -1;
    }

    if (!fgets(buf, size, file))
        len = 0;
    else
    {
        for (len = strlen(buf); len; len--)
        {
            if (buf[len-1] != '\n' && buf[len-1] != '\r')
                break;
        }
        buf[len] = 0;
    }

    if (termios_changed)
    {
        settings.c_lflag |= ECHO;
        tcsetattr(fileno(file), TCSANOW, &settings);
    }

    fclose(file);

    if (len + 1 >= size)
    {
        printf("%s TLS: Password too long.\n", time_stamp());
        debug_message("%s TLS: Password too long.\n", time_stamp());

        return -1;
    }

    return (ssize_t)len;

} /* tls_get_password() */

/*-------------------------------------------------------------------------*/
/* To protect the tls callback during it's execution, it is pushed onto
 * the stack as an T_ERROR_HANDLER value for guaranteed cleanup.
 */

typedef struct tls_cb_handler_s
{
    error_handler_t head;
    callback_t    * cb;
} tls_cb_handler_t;

/*-------------------------------------------------------------------------*/
static void
handle_tls_cb_error (error_handler_t * arg)

{
    tls_cb_handler_t * data = (tls_cb_handler_t *)arg;
    free_callback(data->cb);
    xfree(data->cb);
    xfree(arg);
} /* handle_tls_cb_error() */

/*-------------------------------------------------------------------------*/
int
tls_continue_handshake (interactive_t *ip)

/* Continue the TLS initialisation handshake for interactive <ip>.
 * Return a negative error code if the connection can not be set up.
 * Return 0 if the connection is still begin set up.
 * Return 1 if the connection is now active (or if no secure connection
 * had been requested).
 *
 * If a callback is set for <ip> and connection comes out of the handshaking
 * state, it will be called with the result code.
 */

{
    int ret;

    if (ip->tls_status != TLS_HANDSHAKING)
        return 1;

    ret = tls_do_handshake(ip);
    if (!ret)
	return 0;


    if (ret < 0)
    {
        ip->tls_status = TLS_INACTIVE;
    }
    else
    {
        ip->tls_status = TLS_ACTIVE;
    }

    /* If the connection is no longer in handshake, execute the callback */
    if (ip->tls_cb != NULL)
    {
        tls_cb_handler_t * handler;

        xallocate( handler, sizeof(*handler)
                 , "TLS: Callback handler protector");
        handler->cb = ip->tls_cb;
        ip->tls_cb = NULL;
            /* Protect the callback during its execution. */

        push_error_handler (handle_tls_cb_error, &(handler->head));

        push_number(inter_sp, ret < 0 ? ret : 0);
        push_ref_object(inter_sp, ip->ob, "tls_handshake");

        command_giver = ip->ob;
        current_interactive = ip->ob;
        /* Set current_interactive as it would be for a normal logon() call. */

        (void)backend_callback(handler->cb, 2);
        xfree(handler->cb); /* The callback is already gone. */
        xfree(handler);
        inter_sp--;         /* Remove the error handler without executing. */

        if (!(ip->ob->flags & O_DESTRUCTED))
            print_prompt();

        command_giver = NULL;
    }

    return ret;
} /* tls_continue_handshake() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_refresh_certs (svalue_t *sp)

/* EFUN tls_refresh_certs()
 *
 *   void tls_refresh_certs()
 *
 * Reload the certificates and certificate revocation information.
 */

{
    if (!tls_available())
        errorf("tls_refresh_certs(): TLS layer hasn't been initialized.\n");

    tls_verify_init();

    return sp;
} /* f_tls_refresh_certs() */

/*-------------------------------------------------------------------------*/
svalue_t *
v_tls_init_connection (svalue_t *sp, int num_arg)

/* EFUN tls_init_connection()
 *
 *   int tls_init_connection(object ob)
 *   int tls_init_connection(object ob, string fun, string|object fob, mixed extra...)
 *   int tls_init_connection(object ob, closure fun, mixed extra...)
 *
 * tls_init_connection() tries to start a TLS secured connection to the
 * interactive object <ob> (or this_object() if <ob> is not given).
 * Result:
 *   errorcode < 0: unsuccessful, use tls_error() to get an useful
 *                  description of the error
 *      number > 0: the secure connection is still being set up in the
 *                   background
 *     number == 0: the secure connection is active.
 *
 * If the callback <fun>/<fun>:<fob> is specified, it will be called once
 * the fate of the secure connection has been determined. The first argument
 * will be the return code from the handshake (errorcode < 0 on failure,
 * or 0 on success), followed by the interactive object <ob> and any <extra>
 * arguments.
 */

{
    svalue_t * argp = sp - num_arg + 1;
    long ret;
    object_t * obj;
    interactive_t *ip;

    if (!tls_available())
        errorf("tls_init_connection(): TLS layer hasn't been initialized.\n");

    if (num_arg > 0)
    {
        obj = argp->u.ob;
        put_number(argp, 0); /* remove obj reference from the stack */
    }
    else
    {
        if (current_object.type != T_OBJECT)
            errorf("tls_init_connection() for lightweight object.\n");
        obj = ref_object(current_object.u.ob, "tls_init_connection");
    }

    if (!O_SET_INTERACTIVE(ip, obj))
    {
        free_object(obj, "tls_init_connection");
        errorf("Bad arg 1 to tls_init_connection(): "
              "object not interactive.\n");
    }

    free_object(obj, "tls_init_connection");
      /* ip has another reference to obj, so this is safe to do */

    if (ip->tls_status != TLS_INACTIVE)
        errorf("tls_init_connection(): Interactive already has a secure "
              "connection.\n");

    /* Extract the callback information from the stack */
    if (num_arg > 1)
    {
        /* Extract the callback information from the stack */
        int error_index;
        callback_t * cb;

        inter_sp = sp;

        assign_eval_cost();

        error_index = setup_efun_callback(&cb, argp+1, num_arg-1);

        if (error_index >= 0)
        {
            /* The callback values have already been removed. */
            inter_sp = sp = argp;
            vefun_bad_arg(error_index+2, argp);
            /* NOTREACHED */
            return argp;
        }

        /* Callback creation successful */
        ip->tls_cb = cb;

    }

    inter_sp = sp = argp - 1;

    /* Flush the connection */

    {
        object_t * save_c_g = command_giver;
        command_giver = obj;
        add_message_flush();
        command_giver = save_c_g;
    }

    ret = tls_init_connection(ip);
    if (ret >= 0)
    {
        ip->tls_status = TLS_HANDSHAKING;
        ret = tls_continue_handshake(ip);

        /* Adjust the return value of tls_continue_handshake() */
        if (ret >= 0)
            ret = !ret;
    }

    push_number(sp, ret);
    return sp;
} /* f_tls_init_connection() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_check_certificate(svalue_t *sp)

/* EFUN tls_check_certificate()
 *
 *   mixed *tls_check_certificate(object obj);
 *   mixed *tls_check_certificate(object obj, int extra);
 * 
 * tls_check_certificate() checks the certificate of the secured
 * connection bound to <obj> (default is the current object).  If
 * <obj> is not interactive, or if TLS is not available, an error
 * is thrown.
 * 
 * If <obj> doesn't have a secure connection up and running, an
 * error is thrown.
 * Otherwise, the result is an array with these values:
 * 
 *   int [0]      : Result code of SSL_get_verify_result (see man 1 verify
 *                  subsection DIAGNOSTICS for possible values)
 *   array [1]          : array with 3*n entries of extra x509 data.
 *                       structure is:
 *                       3*i    : numerical form of object name, e.g. "2.5.4.3"
 *                       3*i + 1: long or short name if available, e.g. "commonName"
 *                       3*i + 2: value
 *   array [2]          : if extra is set:
 *                       array with 3*n entries of x509 extension data
 *                       data structure is:
 *                       3*i    : numerical form of extension name
 *                       3*i + 1: long or short name of extension name if available
 *                       3*i + 2: array of strings with the data structure of [1]
 *
 * Note: a x509 certificate can have more than one object with the same name
 *
 * See associated documentation for code that generates more convient mapping
 * data structures
 */

{
    vector_t *v = NULL;
    interactive_t *ip;
    int more;

    /* more information requested */
    more = sp->u.number;
    free_svalue(sp--);

    if (!tls_available())
        errorf("tls_check_certificate(): TLS layer hasn't been initialized.\n");

    if (!O_SET_INTERACTIVE(ip, sp->u.ob))
        errorf("Bad arg 1 to tls_check_certificate(): "
              "object not interactive.\n");

    if (ip->tls_status != TLS_ACTIVE) 
        errorf("tls_check_certificate(): object doesn't have a secure connection.\n");

    if (more < 0 || more > 1)
        errorf("tls_check_certificate(): invalid flag passed as second argument.\n");

    v = tls_check_certificate(ip, (more == 1) ? MY_TRUE : MY_FALSE);

    free_svalue(sp);

    if (v != NULL)
        put_array(sp, v);
    else
        put_number(sp, 0);
    return sp;
} /* tls_check_certificate() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_deinit_connection(svalue_t *sp)

/* EFUN tls_deinit_connection()
 *
 *      void tls_deinit_connection(object ob)
 *
 * tls_deinit_connection() shuts down a TLS connection to the interactive
 * object <ob> (or this_object() if <ob> is not given) but the connection is
 * not closed.
 */

{
    interactive_t *ip;

    if (!O_SET_INTERACTIVE(ip, sp->u.ob))
        errorf("Bad arg 1 to tls_deinit_connection(): "
              "object not interactive.\n");

    /* Flush the connection */

    {
        object_t * save_c_g = command_giver;
        command_giver = sp->u.ob;
        add_message_flush();
        command_giver = save_c_g;
    }

    tls_deinit_connection(ip);

    free_svalue(sp--);
    return sp;
} /* f_tls_deinit_connection() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_error(svalue_t *sp)

/* EFUN tls_error()
 *
 *     string tls_error(int errorno)
 *
 * tls_error() returns a string describing the error behind the
 * error number <errorno>.
 */

{
    string_t *s;
    const char *text;
    int err = sp->u.number;

    text = tls_error(err);

    if (text)
    {
        memsafe(s = new_unicode_mstring(text), strlen(text), "tls_error()");
        free_svalue(sp);
        put_string(sp, s);
    }
    else
    {
        free_svalue(sp);
        put_number(sp, 0);
    }

    return sp;
} /* f_tls_error() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_query_connection_state (svalue_t *sp)

/* EFUN tls_query_connection_state()
 *
 *      int tls_query_connection_state(object ob)
 *
 * tls_query_connection_state() returns a positive number if <ob>'s connection
 * is TLS secured, 0 if it's unsecured, and a negative number if the
 * TLS connection setup is still being set-up.
 * Returns 0 for non-interactive objects.
 */

{
    interactive_t *ip;
    Bool rc;

    if (!O_SET_INTERACTIVE(ip, sp->u.ob))
        rc = 0;
    else if (ip->tls_status == TLS_HANDSHAKING)
        rc = -1;
    else if (ip->tls_status == TLS_INACTIVE)
        rc = 0;
    else
        rc = 1;
    free_svalue(sp);
    put_number(sp, rc);
    return sp;
} /* f_tls_query_connection_state() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_query_connection_info (svalue_t *sp)

/* EFUN tls_query_connection_info()
 *
 *
 *       #include <sys/tls.h>
 *       int *tls_query_connection_info (object ob)
 *
 * If <ob> does not have a TLS connection, or if the TLS connection is
 * still being set up, the efun returns 0.
 *
 * If <ob> has a TLS connection, tls_query_connection_info() returns an array
 * that contains some parameters of <ob>'s connection:
 *
 *    int|string [TLS_CIPHER]: the cipher used
 *    int        [TLS_COMP]:   the compression used
 *    int        [TLS_KX]:     the key-exchange used
 *    int        [TLS_MAC]:    the digest algorithm used
 *    int|string [TLS_PROT]:   the protocol used
 *
 * To translate these numbers into strings, <tls.h> offers a number of macros:
 *
 *    TLS_xxx_TABLE: a literal array of strings describing the value in
 *        question.
 *    TLS_xxx_NAME(x): a macro translating the numeric result value into a
 *        string.
 *
 *    xxx: CIPHER, COMP, KX, MAC, PROT
 */

{
    interactive_t *ip;

    if (O_SET_INTERACTIVE(ip, sp->u.ob) && ip->tls_status == TLS_ACTIVE)
    {
        vector_t * rc;
	
        rc = tls_query_connection_info(ip);
        free_svalue(sp);
        put_array(sp, rc);
    }
    else
    {
        free_svalue(sp);
        put_number(sp, 0);
    }

    return sp;
} /* tls_query_connection_info() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_tls_available (svalue_t *sp)

/* EFUN tls_available()
 *
 *       int tls_available ()
 *
 * If the global TLS Initialisation could not been set up, tls_available()
 * returns 0, otherwise 1.
 */

{
    sp++;
    put_number(sp, tls_available() == MY_TRUE ? 1 : 0);
    return sp;
} /* f_tls_available() */


/***************************************************************************/
#endif /* USE_TLS */
