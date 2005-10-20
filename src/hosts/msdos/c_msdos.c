/* communication class dispatcher - WA */

#include "../../telnet.h"
#include "pcomm.h"
#include "c_msdos.h"
#include <stdio.h>

int c_errno = 0;

static CLASS_DEF vcon,serial;


static CLASS_DEF *class(channel)
int channel;
{
    if (channel < serial.base)
        return &vcon;
    else
        return &serial;
}


static int session(channel)
int channel;
{
    if (channel < serial.base)
        return channel-vcon.base;
    else
        return channel-serial.base;
}


void c_listen()
{
    vcon.base = 1;
    vc_init(&vcon);
    serial.base = vcon.base + vcon.max_sess;
    ser_init(&serial);
}


void c_shutdown()
{
    ser_shutdown();
    vc_shutdown();
}


int c_read(channel,data,length)
int channel,length;
char *data;
{
    return class(channel)->read(session(channel),data,length);
}


int c_write(channel, data, length)
int channel,length;
char *data;
{
    static enum modes { m_norm, m_iac, m_will, m_wont} mode = m_norm;
    int (*send)(int,char);
    int count,sess;

    send = class(channel)->write;
    sess = session(channel);

    for (count = length; count; count--) {
        switch (mode) {
            case m_norm:
                if (*(unsigned char *) data == IAC)
                    mode = m_iac;
                else if ((*send)(sess, *data) < 0)
                    return -1;
            case m_iac :
                if (*(unsigned char *) data == WILL)
                    mode = m_will;
                else {
                    if (*(unsigned char *) data == WONT)
                        mode = m_wont;
                    else
                        mode = m_norm;
                }
                break;
            case m_will:
                if (*data == TELOPT_ECHO)
                   class(channel)->echo(sess, 0);
                mode = m_norm;
                break;
            case m_wont:
                if (*data == TELOPT_ECHO)
                    class(channel)->echo(sess, 1);
                mode = m_norm;
                break;
        }
        data++;
    }
    return length;
}


int c_accept(addr,length)
struct caddr *addr;
int *length;
{
    int status;

    if ((status = vc_accept()) >= 0) {
        addr->addr = status + vcon.base + 100;
        *length = sizeof(*addr);        /* deref'd addr. rmhh */
        /*fprintf(stderr, "c_accept : %d\n", status + vcon.base);*/
        return status + vcon.base;
    }
    fprintf(stderr, "c_accept() : %d\n", status);
    return status;

    if ((status = ser_accept()) >= 0) {
        addr->addr = status+serial.base+100;
        *length = sizeof(addr);
        return status+serial.base;
    }
    return status;
}


/*
 * c_select
 *      (socket_)select
 * returns:
 *   -1 : error
 *    0 : timeout?
 *   >0 : data ready on one of the channels in *mask
 */
int c_select(mask, timeout)
fd_set *mask;
struct timeval *timeout;
{
    int vc_mask, ser_mask, vc_stat, ser_stat;
    int new_con, new_ser;

    new_con = *mask & 1;               /* look for any new connections */
    vc_mask = (*mask >> vcon.base) & ((1 << vcon.max_sess)-1);
    ser_mask = (*mask >> serial.base) & ((1 << serial.max_sess)-1);

    if (new_con || vc_mask) {
        vc_stat = vc_select(&vc_mask, timeout, &new_con);
        if (vc_stat < 0) {
            *mask = 0;
            return vc_stat;
        }
    }

    /* check for new serial connections iff no new vconsole connections */
    new_ser = !new_con && (*mask & 1);
    if (new_ser || ser_mask) {
        ser_stat = ser_select(&ser_mask, timeout, &new_ser);
        if (ser_stat < 0) {
            *mask = 0;
            return vc_stat;
        }
    }
    *mask = (vc_mask << vcon.base) | (ser_mask << serial.base);
    *mask |= (new_con || new_ser);
    return vc_stat + ser_stat;

#if 0
    if ((vc_stat = vc_select(&vc_mask, timeout)) < 0)
        return vc_stat;
    *mask = vc_mask << vcon.base;

    if ((ser_stat = ser_select(&ser_mask, timeout)) < 0)
        return ser_stat;
    *mask |= ser_mask << serial.base;

    return vc_stat+ser_stat;
#endif
}


int c_close(channel)
int channel;
{
    return class(channel)->close(session(channel));
}


void c_echo(int channel,int mode)
{
    class(channel)->echo(session(channel),mode);
}


/*ARGSUSED*/
int c_getaddr(channel,addr,length)
int channel;
struct caddr *addr;
int *length;
{
    addr->addr = channel+100;
    *length = sizeof(addr);
    return(0);
}
