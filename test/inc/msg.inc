#ifndef __MSG_INC__
#define __MSG_INC__

#include "/sys/debug_message.h"

static void msg(string str, varargs mixed *par)
{
    if (!sizeof(par))
        debug_message(str, DMSG_STDERR | DMSG_LOGFILE);
    else
        debug_message(sprintf(str,par...), DMSG_STDERR | DMSG_LOGFILE);
}

#endif
