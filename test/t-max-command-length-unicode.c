// Check that MAX_COMMAND_LENGTH doesn't split a unicode codepoint.

#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/commands.h"


void run_server()
{
    // The first Unicode codepoint should just be in the middle of the
    // command limit. The remaining chars are for checking that the
    // limit actually works.
    write(2 * ("X" * (__MAX_COMMAND_LENGTH__ - 2) + "\U000101DB" * 100 + "\n"));
}

void run_client()
{
    add_action(function int(string str)
    {
        // Check that the string is valid.
        string err = catch(sizeof(str));

        if (err)
        {
            msg("FAILURE: %s", err[1..]);
            shutdown(1);
        }
        else if (sizeof(str) > __MAX_COMMAND_LENGTH__)
        {
            msg("FAILURE: Command length not limited.\n");
            shutdown(1);
        }
        else
        {
            msg("Success!\n");
            shutdown(0);
        }

        return 0;
    }, "X", AA_NOSPACE);
}

void run_test()
{
    msg("\nRunning test for unicode-aware MAX_COMMAND_LENGTH:\n"
          "--------------------------------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    // __MAX_COMMAND_LENGTH__ is applied on the result of H_MODIFY_COMMAND.
    set_driver_hook(H_MODIFY_COMMAND, function string(string cmd, object pl)
    {
        return cmd;
    });

    configure_interactive(0, IC_ENCODING, "UTF-8");

    run_test();
    return 0;
}
