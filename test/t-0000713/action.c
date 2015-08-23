#include "/sys/configuration.h"

int unique_function_name(string str)
{
    return 1;
}

void start()
{
    configure_object(this_object(), OC_COMMANDS_ENABLED, 1);
    add_action("unique_function_name", "test");
}
