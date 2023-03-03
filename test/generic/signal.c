#include "/inc/base.inc"
#include "/sys/configuration.h"
#include "/sys/signals.h"

// This doesn't exist.
#define DC_SIGACTION_SIGTERM -1

int handle_external_signal(int signal)
{
    if (signal == SIGNAL)
    {
        shutdown(0);
        remove_call_out(#'shutdown);
    }
    return 1;
}

string *epilog(int eflag)
{
    if (SIGNAL_CONF != DC_SIGACTION_SIGTERM)
        configure_driver(SIGNAL_CONF, SIGNAL_ACTION);
    call_out(#'shutdown, 1, 1);
    return 0;
}
