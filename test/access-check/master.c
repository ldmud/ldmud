#include "/inc/base.inc"
#include "/sys/driver_info.h"

void do_connect()
{
    // Make a connection to localhost. This should be rejected by
    // the ACCESS.ALLOW policy, and the test driver script will
    // assert on the access log containing a rejection.
    net_connect("127.0.0.1", efun::driver_info(DI_MUD_PORTS)[0]);
}

string *epilog(int eflag)
{
    call_out(#'do_connect, 0);
    call_out(#'shutdown, 1, 0);
    return 0;
}
