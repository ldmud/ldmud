/*------------------------------------------------------------------
 * Common functions for all TLS modules.
 *------------------------------------------------------------------
 */

#include "driver.h"
#include "machine.h"

#ifdef USE_TLS

#include <stdio.h>

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
char * tls_certfile = NULL;
char * tls_trustfile = NULL;
char * tls_trustdirectory = NULL;
char * tls_crlfile = NULL;
char * tls_crldirectory = NULL;
  /* The filenames of the x509 key and cert file, set by the argument
   * parser. If not set, the package will use defaults.
   */

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

        (void)apply_callback(handler->cb, 2);

        if (!(ip->ob->flags & O_DESTRUCTED))
            print_prompt();

        command_giver = NULL;

        free_svalue(inter_sp); inter_sp--; /* free the callback */
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
        obj = ref_object(current_object, "tls_init_connection");
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

        memsafe(cb = xalloc(sizeof *cb) , sizeof *cb , "callback structure");

        assign_eval_cost();

        error_index = setup_efun_callback(cb, argp+1, num_arg-1);

        if (error_index >= 0)
        {
            /* The callback values have already been removed. */
            
            xfree(cb);
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
        add_message(message_flush);
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
        add_message(message_flush);
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
        memsafe(s = new_mstring(text), strlen(text), "tls_error()");
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
