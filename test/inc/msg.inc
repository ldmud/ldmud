#ifndef __MSG_INC__
#define __MSG_INC__

#include "/sys/debug_message.h"

static void msg(string str, varargs mixed *par)
{
    debug_message(apply(#'sprintf, str,par), DMSG_STDERR | DMSG_LOGFILE);
}

#endif
