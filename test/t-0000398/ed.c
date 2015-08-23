#include "/sys/configuration.h"

int logon(int flag)
{
    configure_interactive(this_object(), IC_TELNET_ENABLED, 0);
    configure_interactive(this_object(), IC_PROMPT, "");
    
    ed("/dummy","ed_ends");
    return 1;
}

void ed_ends()
{
    rm("/dummy");
}
