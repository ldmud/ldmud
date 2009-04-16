/*---------------------------------------------------------------------------
 * MCCP package.
 *
 * Efuns written and donated 2003 by Bastian Hoyer.
 *---------------------------------------------------------------------------
 * Support functions are based on code from the mccp project,
 * see http://www.randomly.org/projects/MCCP/
 *
 * Copyright (c) 1999, Oliver Jowett <oliver@randomly.org>
 *
 * This code may be freely distributed and used if this copyright
 * notice is retained intact.
 *---------------------------------------------------------------------------
 */

#include "driver.h"

#ifdef USE_MCCP

#include "typedefs.h"

#include <errno.h>
#include <stdio.h>
#include <zlib.h>

#ifdef SOCKET_INC
#    include SOCKET_INC
#endif

/* When no special networking code is needed, define the
 * socket function to their normal Unix names.
 */
#if !defined (SOCKET_LIB) && !defined(SOCKET_INC)
#    define socket_number(s) (s)
#    define socket_ioctl  ioctl
#    ifndef hpux
#        define socket_select select
#    else
#        define socket_select(n,r,w,x,t) select(n, (int *)r, (int *)w, (int *)x, t)
         /* Silences the compiler */
#    endif
#    define socket_read   read
#    define socket_write  write
#    define socket_close  close
#endif /* SOCKET_LIB */

#include "pkg-mccp.h"

#include "array.h"
#include "comm.h"
#include "mstrings.h"
#include "object.h"
#include "svalue.h"
#include "xalloc.h"

#include "../mudlib/sys/telnet.h"

/*=========================================================================*/

/*                          Support functions                              */

/*-------------------------------------------------------------------------*/

#define UMIN(a,b) ((a) < (b) ? (a) : (b))

/*-------------------------------------------------------------------------*/
void *
zlib_alloc (void *opaque UNUSED, unsigned int items, unsigned int size)

/* Callback function for the zlib to allocate an zeroed block of
 * memory.
 */

{
#ifdef __MWERKS__
#   pragma unused(opaque)
#endif
    return calloc (items, size);
} /* zlib_alloc() */

/*-------------------------------------------------------------------------*/
void
zlib_free (void *opaque UNUSED, void *address)

/* Callback function for the zlib to free a block of memory allocated with
 * zlib_alloc().
 */

{
#ifdef __MWERKS__
#   pragma unused(opaque)
#endif
    free (address);
} /* zlib_free() */

/*-------------------------------------------------------------------------*/
Bool
start_compress (interactive_t * ip, unsigned char telopt)

/* Enable MCCP compression on interactive <ip>, using <telopt> for the
 * negotiations.
 *
 * Return TRUE on success.
 */

{
    z_stream *s;
    
    /* already compressing */
    if (ip->out_compress)
        return MY_TRUE;
    
    /* allocate and init stream, buffer */
    s = xalloc(sizeof (*s));
    ip->out_compress_buf = xalloc(COMPRESS_BUF_SIZE);
    
    s->next_in = NULL;
    s->avail_in = 0;
    s->next_out = ip->out_compress_buf;
    s->avail_out = COMPRESS_BUF_SIZE;
    s->zalloc = zlib_alloc;
    s->zfree = zlib_free;
    s->opaque = NULL;
    
    if (deflateInit (s, 9) != Z_OK)
    {
        xfree(ip->out_compress_buf);
        xfree(s);
        return MY_FALSE;
    }
    
    if (ip->tn_enabled)
    {
        /* version 1 or 2 support */
        if (telopt == TELOPT_COMPRESS)
        {
            DTF (("%s TDEBUG: send IAC SB %02x WILL SE\n", time_stamp (), telopt));
            SEND_TELNET_COMMAND (add_message ("%c", IAC);
                                 add_message ("%c%c%c%c", SB, telopt, WILL, SE);
                                 add_message (message_flush););
        }
        else if (telopt == TELOPT_COMPRESS2)
        {
            DTF (("%s TDEBUG: send IAC SB %02x WILL SE\n", time_stamp (), telopt));
            SEND_TELNET_COMMAND (add_message ("%c", IAC);
                                 add_message ("%c%c%c%c", SB, telopt, IAC, SE);
                                 add_message (message_flush););
        }
        else
        {
            printf("Bad teloption %d passed", telopt);
            xfree(ip->out_compress_buf);
            xfree(s);
            return MY_FALSE;
        }
    }
    
    ip->compressing = telopt;
    ip->out_compress = s;
    
    printf("%s MCCP-DEBUG: '%s' mccp started (%d)\n"
          , time_stamp(), get_txt(ip->ob->name), telopt);
    
    /* success */
    return MY_TRUE;
} /* start_compress() */

/*-------------------------------------------------------------------------*/
Bool
end_compress (interactive_t * ip, Bool force)

/* Cleanly shut down compression on <ip>. If <force> is TRUE, the compression
 * will be forcefully shut down even if this incurs a data loss.
 * Return TRUE on success.
 */

{
    unsigned char dummy[1];
    unsigned char buf[256];
    size_t len;
    Bool retval = MY_TRUE;
    
    if (!ip->out_compress)
        return MY_TRUE;
   
    ip->out_compress->avail_in = 0;
    ip->out_compress->next_in = dummy;
    
    ip->out_compress->next_out = buf;
    ip->out_compress->avail_out = sizeof(buf);

    /* No terminating signature is needed - receiver will get Z_STREAM_END */
    if (deflate (ip->out_compress, Z_FINISH) != Z_STREAM_END && !force)
        return MY_FALSE;
   
    len = ip->out_compress->next_out - buf;
    
    /* first reset compression values */
    deflateEnd(ip->out_compress);
    xfree(ip->out_compress_buf);
    xfree(ip->out_compress);
    ip->compressing = 0;
    ip->out_compress = NULL;
    ip->out_compress_buf = NULL;

    /* try to send any residual data */
    comm_socket_write((char*) buf, len, ip, WB_NONDISCARDABLE);
   
    printf("%s MCCP-DEBUG: '%s' mccp ended\n"
          , time_stamp(), get_txt(ip->ob->name));
    
    /* Finished */
    return retval;
} /* end_compress() */

/*=========================================================================*/

/*                           EFUNS                                         */

/*-------------------------------------------------------------------------*/
svalue_t *
f_start_mccp_compress (svalue_t * sp)

/* EFUN start_mccp_compress()
 *
 *     int start_mccp_compress(int telopt)
 *
 * This efun must be called inside an interactive player and starts
 * compression of the driver -> client traffic immediatly.
 *
 * <telopt> denotes the MCCP version and must be either TELOPT_COMPRESS2
 * or TELOPT_COMRESS from <telnet.h>.
 *
 * Return non-zero on success, and 0 on failure.
 */

{
    interactive_t *ip;
    p_int mccpver;
    int retval;
    
    if (!O_SET_INTERACTIVE(ip, current_object))
    {
        errorf("start_mccp_compress() called for non-interactive object.\n");
        return sp;
    }
    
    if (!ip->tn_enabled)
        mccpver = -1;
    else
    {
        mccpver = sp->u.number;
        
        if ((mccpver != TELOPT_COMPRESS)
         && (mccpver != TELOPT_COMPRESS2)
           )
        {
            errorf("Illegal value to arg 1 of start_mccp_compress(): %ld, "
                  "expected TELOPT_COMPRESS (%d) or TELOPT_COMPRESS2 (%d).\n"
                 , (long)mccpver, TELOPT_COMPRESS, TELOPT_COMPRESS2
                 );
            /* NOTREACHED */
            return sp;
        }
    }
    
#if 0
    if (!ip->tn_enabled)
    {
        errorf("start_mccp_compress() called for object with telnet disabled.\n");
        return sp;
    }
#endif

    free_svalue(sp);
    retval = start_compress(ip, mccpver);
    put_number(sp, retval);
    
    return sp;
} /* start_mccp_compress() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_end_mccp_compress (svalue_t * sp)

/* EFUN end_mccp_compress()
 *
 *   int end_mccp_compress()
 *
 * This efun must be called inside an interactive player and stops
 * the compression of the driver -> client traffic.
 *
 * Returns non-zero on success, and zero on a failure.
 */

{
    interactive_t *ip;
    int retval;
    
    if (!O_SET_INTERACTIVE(ip, current_object))
    {
        errorf("end_mccp_compress() called for non-interactive object.\n");
        /* NOTREACHED */
        return sp;
    }
    
#if 0
    if (!ip->tn_enabled)
    {
        errorf("end_mccp_compress() called for object with telnet disabled.\n");
        return sp;
    }
#endif

    retval = end_compress(ip, MY_FALSE);

    sp++;
    put_number(sp,retval);
    
    return sp;
} /* end_mccp_compress() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_mccp (svalue_t * sp)

 /* EFUN query_mccp()
  *
  *   int query_mccp(object ob)
  *
  * this efun returns 0 if no mccp is used for interactive ob.
  * if ob|this_player uses mccp it returns TELOPT_COMPRESS or
  * TELOPT_COMPRESS2
  */

{
    interactive_t *ip;
    
    /* Make sure the object is interactive */
    if (!(O_SET_INTERACTIVE (ip, sp->u.ob)) || ip->closing)
    {
        errorf("Bad arg 1 to query_mccp(): object not interactive.\n");
        return sp;
    }
    
    free_svalue (sp);
    put_number (sp, ip->compressing);
    
    return sp;
} /* query_mccp() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_query_mccp_stats (svalue_t * sp)

/* EFUN query_mccp_stats()
 *
 *   int *query_mccp_stats(object ob)
 *
 * if the connection of interactive ob ( or this_object() if ob==0 )
 * is compressed it returns an array with zlib statistics
 * ({ total_in, total_out }) ( uncompressed/compressed)
 */

{
    interactive_t *ip;
    vector_t *mccp_stats;
    
    /* Make sure the object is interactive */
    if (!(O_SET_INTERACTIVE (ip, sp->u.ob)) || ip->closing)
    {
        errorf("Bad arg 1 to query_mccp_stats(): object not interactive.\n");
        return sp;
    }
    
    free_svalue (sp);
    
    if (ip->compressing > 0)
    {
        mccp_stats = allocate_uninit_array (2);
        put_number (mccp_stats->item, ip->out_compress->total_in);
        put_number (mccp_stats->item + 1, ip->out_compress->total_out);
        put_array (sp, mccp_stats);
    }
    else
    {
        put_number (sp, 0);
    }
    return sp;
} /* query_mccp_stats() */

/*-------------------------------------------------------------------------*/

#endif /* USE_MCCP */

/*************************************************************************/
