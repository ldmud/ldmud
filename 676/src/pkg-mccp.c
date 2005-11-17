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
#include "object.h"
#include "interpret.h"
#include "svalue.h"
#include "xalloc.h"

#include "telnet.h"

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
    
    ip->compressing = telopt;
    ip->out_compress = s;
    
    printf("%s MCCP-DEBUG: '%s' mccp started (%d)\n"
          , time_stamp(), ip->ob->name, telopt);
    
    /* success */
    return MY_TRUE;
} /* start_compress() */

/*-------------------------------------------------------------------------*/
static Bool
process_compressed (interactive_t * ip)

/* Try to send any pending compressed-but-not-sent data in for <ip>.
 * Return TRUE on success.
 */

{
    int iStart, nBlock, nWrite, len;
    
    if (!ip->out_compress)
        return MY_TRUE;
    
    len = ip->out_compress->next_out - ip->out_compress_buf;
    if (len > 0)
    {
        for (iStart = 0; iStart < len; iStart += nWrite)
        {
            nBlock = UMIN (len - iStart, 4096);
            if ((nWrite =
                 socket_write(ip->socket, ip->out_compress_buf + iStart, nBlock)) < 0)
            {
                if (errno == EAGAIN)
                  break;
#ifdef ENOSR
                if (errno == ENOSR)
                  break;
#endif /* ENOSR */
                
                /* write error */
                return MY_FALSE;
            }
            if (nWrite <= 0)
                break;
        }
        
        if (iStart)
        {
            if (iStart < len)
              memmove (ip->out_compress_buf, ip->out_compress_buf + iStart,
                       len - iStart);
            
            ip->out_compress->next_out = ip->out_compress_buf + len - iStart;
        }
    }
    
    /* success */
    return MY_TRUE;
} /* process_compressed() */

/*-------------------------------------------------------------------------*/
Bool
end_compress (interactive_t * ip)

/* Cleanly shut down compression on <ip>.
 * Return TRUE on success.
 */

{
    unsigned char dummy[1];
    Bool retval;
    
    if (!ip->out_compress)
        return MY_TRUE;
   
    retval = MY_TRUE;
    ip->out_compress->avail_in = 0;
    ip->out_compress->next_in = dummy;
    
    /* No terminating signature is needed - receiver will get Z_STREAM_END */
    if (deflate (ip->out_compress, Z_FINISH) != Z_STREAM_END)
        return MY_FALSE;
   
    /* try to send any residual data */
    if (!process_compressed (ip))
    {
        printf("%s MCCP-DEBUG: '%s' mccp had error while ending\n"
              , time_stamp(), ip->ob->name);
        retval = MY_FALSE;
    }
     
    /* reset compression values */
    deflateEnd(ip->out_compress);
    xfree(ip->out_compress_buf);
    xfree(ip->out_compress);
    ip->compressing = 0;
    ip->out_compress = NULL;
    ip->out_compress_buf = NULL;
   
    printf("%s MCCP-DEBUG: '%s' mccp ended\n"
          , time_stamp(), ip->ob->name);
    
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
    
    if (sp->type != T_NUMBER) 
        bad_xefun_arg(1, sp);

    mccpver = sp->u.number;
    
    if ((mccpver != TELOPT_COMPRESS)
     && (mccpver != TELOPT_COMPRESS2)
       )
    {
        error("Illegal value to arg 1 of start_mccp_compress(): %ld, "
              "expected TELOPT_COMPRESS (%d) or TELOPT_COMPRESS2 (%d).\n"
             , (long)mccpver, TELOPT_COMPRESS, TELOPT_COMPRESS2
             );
        /* NOTREACHED */
        return sp;
    }
    
    if (!O_SET_INTERACTIVE(ip, current_object))
    {
        error("start_mccp_compress() called for non-interactive object.\n");
        return sp;
    }
    
    if (!ip->tn_enabled)
    {
        error("start_mccp_compress() called for object with telnet disabled.\n");
        return sp;
    }

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
        error("end_mccp_compress() called for non-interactive object.\n");
        /* NOTREACHED */
        return sp;
    }
    
    if (!ip->tn_enabled)
    {
        error("end_mccp_compress() called for object with telnet disabled.\n");
        return sp;
    }

#ifdef USE_PTHREADS
    ip->compressing = 0; /* Signal the writer to stop compressing */
    retval = 0;
#else
    retval = end_compress(ip);
#endif

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
    
    if (sp->type != T_OBJECT) 
        bad_xefun_arg(1, sp);

    /* Make sure the object is interactive */
    if (!(O_SET_INTERACTIVE (ip, sp->u.ob)) || ip->closing)
    {
        error ("Bad arg 1 to query_mccp(): object not interactive.\n");
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
    
    if (sp->type != T_OBJECT) 
        bad_xefun_arg(1, sp);

    /* Make sure the object is interactive */
    if (!(O_SET_INTERACTIVE (ip, sp->u.ob)) || ip->closing)
    {
        error ("Bad arg 1 to query_mccp_stats(): object not interactive.\n");
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
