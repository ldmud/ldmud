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
            char cmd[] = { IAC, SB, telopt, WILL, SE };
            DTF (("%s TDEBUG: send IAC SB %02x WILL SE\n", time_stamp (), telopt));
            add_message_bytes(cmd, sizeof(cmd));
            add_message_flush();
        }
        else if (telopt == TELOPT_COMPRESS2)
        {
            char cmd[] = { IAC, SB, telopt, IAC, SE };
            DTF (("%s TDEBUG: send IAC SB %02x IAC SE\n", time_stamp (), telopt));
            add_message_bytes(cmd, sizeof(cmd));
            add_message_flush();
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

#endif /* USE_MCCP */

/*************************************************************************/
