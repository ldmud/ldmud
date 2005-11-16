/* serial lines - WA */

#include <std.h>
#include <stdio.h>
#include <dos.h>

#include "pcdef.h"
#include "c_msdos.h"


#define LINES 16 /* max. number of serial lines */

static int ser_write(int,char);


static struct bps_data {
    int rate, code, second;
} bps[] = {{300,2,-1},{1200,4,-1},{2400,5,-1},{9600,7,-1},{19200,7,0},
          {38400,7,1},{0,5,-1}};


static int rate[LINES];
/*
 * bit fields (bit 0 = comm 1, bit 1 = comm 2, etc.):
 * present      - available comm ports
 * carrier      - dcd
 * connected    - if connected to game
 * hangup       - hmm, flags are never set
 * draining     - game connection closed, but line still live
 */
static int present, carrier, connected, hangup, draining;

#define error(c) { c_errno = (c); return -1; }


static int sio(channel,code,data)
int channel,code,data;
{
    union REGS regs;

    regs.h.ah = code;
    regs.h.al = data;
    regs.x.dx = channel;
    int86(0x14, &regs, &regs);
    return regs.x.ax;
}


static void reset(channel)
int channel;
{
    int walk;

    for (walk = 0; bps[walk].rate; walk++)
        if (bps[walk].rate == rate[channel])
            break;
    sio(channel, 0, (bps[walk].code << 5) | 3);
    if (bps[walk].second > -1)
        sio(channel, 8, bps[walk].second);
}


static int sense_carrier(channel)
int channel;
{
    return sio(channel, 3, 0) & 0x80;
}


static int data_waiting(channel)
int channel;
{
    return sio(channel, 3, 0) & 0x100;
}


static void send_char(channel, ch)
int channel;
int ch;
{
    sio(channel, 1, ch);
}


static char receive_char(channel)
int channel;
{
    return sio(channel, 2, 0) & 0x7f;
}


static int access_chan(channel)
int channel;
{
    /* changed logical and (&&) to bitwise and (&). odd.. */
    if (!(present & (1 << channel))) {
        fprintf(stderr, "\nERROR: invalid channel #%d\n", channel);
        error(ECOMUSG);
    }
    if (connected & (1 << channel))
        return (0);
    fprintf(stderr, "\nWARNING: accessing inactive channel #%d\n", channel);
    error(ECOMUSG);
}


void ser_listen()
{
    FILE *cfg;
    char line[81];
    char *name, dcd;
    int number, bps;

    present = carrier = connected = hangup = draining = 0;

    if (!(name = getenv("SERIAL")))
        name = "serial";
    if ((cfg = fopen(name, "r")) == NULL) {
        fprintf(stderr,"\nERROR: can't open %s\n",name);
        exit(1);
    }
    while (fgets(line, 80, cfg))
        if (*line != '#' && *line != '\n') {
            if (sscanf(line, "%d %d %c\n", &number, &bps, &dcd) != 3) {
                fprintf(stderr, "\nERROR: invalid line: %s", line);
                exit(1);
            }
            present |= 1 << --number;
            rate[number] = bps;
            if (dcd == 'y' || dcd == 'Y')
                carrier |= 1 << number;
            reset(number);
        }
    fclose(cfg);
}


static int ser_read(channel, data, length)
int channel, length;
char *data;
{
    int count;

    if (access_chan(channel) < 0)
        error(ECOMUSG);
    if ((hangup >> channel) & 1)
        return 0;
    for (count = 0; count < length; count++) {
        if (data_waiting(channel))
            *data++ = receive_char(channel);
        else
            break;
    }
    return count;
}


static int ser_write(channel, ch)
int channel;
char ch;
{
    if (access_chan(channel) < 0)
        error(ECOMUSG);
    if ((hangup >> channel) & 1)
        return 0;
    send_char(channel, ch);
    return 1;
}

static void close_draining()
{
    int mask, channel;

    /* check for closed channels to hang up */
    channel = 0;
    for (mask = draining; mask; mask = mask >> 1) {
        if ((mask & 1) && !sense_carrier(channel)) {
            reset(channel);
            draining &= ~(1 << channel);

            /* should hangup flag be set? somewhere?? */
        }
        channel++;
    }
}

int ser_accept()
{
    int mask, channel;

    close_draining();

    /* look for new connections */
    mask = present & ~connected & ~draining;
    for (channel = 0; mask; channel++) {
        if (mask & 1) {
            if (carrier & (1 << channel) ? sense_carrier(channel)
                                         : data_waiting(channel))
                break;
        }
        mask = mask >> 1;
    }

    if (mask) {
        /* we have a new connection! */
        reset(channel);
        connected |= 1 << channel;
        return channel;
    }
/*
    error(ECOMFLW);
    Use a non-fatal error code */
    error(EAGAIN);
}


/*ARGSUSED*/
int ser_select(mask, timeout, new_ser)
int *mask;
c_time_t *timeout;
int *new_ser;
{
    int poll, copy, channels, channel;
    int avail;

    poll = *mask & connected & ~hangup & ~draining;
    *mask = copy = hangup;

    /* don't know what this is good for */
    for (channels = 0; copy; copy = copy >> 1)
        if (copy & 1)
            channels++;

    /* check for new connects */
    if (*new_ser) {
        avail = present & ~connected & ~draining;
        for (channel = 0; avail; avail = avail >> 1) {
            if (avail & 1) {
                /* not too sure if this is the right check */
                if (carrier & (1 << channel) ? sense_carrier(channel)
                                             : data_waiting(channel))
                    break;
            }
        }
        if (!avail)
            *new_ser = 0;
        else
            channels++;
    }

    /* check existing connections for data */
    for (channel = 0; poll; poll = poll >> 1) {
        if (poll & 1) {
            if (data_waiting(channel)
            ||  ((carrier & (1 << channel)) && !sense_carrier(channel))) {
                *mask |= 1 << channel;
                channels++;
            }
        }
        channel++;
    }

    return channels;
}


static int ser_close(channel)
int channel;
{
    if (access_chan(channel) < 0)
        error(ECOMUSG);
    connected &= ~(1 << channel);

    if (!((hangup >> channel) & 1))
        draining |= carrier & (1 << channel);
    hangup &= ~(1 << channel);
    return 0;
}


static void ser_echo(int channel, int mode)
{
    if (access_chan(channel) >= 0)
        sio(channel, 9, mode ? 1 : 0);
}


void ser_init(CLASS_DEF *cd)
{
    ser_listen();
    cd->max_sess = LINES;
    cd->close = ser_close;
    cd->read = ser_read;
    cd->write = ser_write;
    cd->echo = ser_echo;
}


void ser_shutdown()
{
}
