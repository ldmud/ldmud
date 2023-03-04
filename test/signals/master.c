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
    call_out(#'shutdown, 1, 1);

    switch (SIGNAL_TEST)
    {
        case "DCS_DEFAULT":
            if (SIGNAL_CONF != DC_SIGACTION_SIGTERM)
                configure_driver(SIGNAL_CONF, DCS_DEFAULT);
            break;

        case "DCS_SHUTDOWN":
            if (SIGNAL_CONF != DC_SIGACTION_SIGTERM)
                configure_driver(SIGNAL_CONF, DCS_SHUTDOWN);
            break;

        case "Python":
#ifdef __PYTHON__
            if (SIGNAL_CONF != DC_SIGACTION_SIGTERM)
                configure_driver(SIGNAL_CONF, DCS_IGNORE);
            expect_signal(SIGNAL_NAME);
#endif
            break;
    }

    return 0;
}
