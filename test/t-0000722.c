#pragma save_types, rtt_checks
#include "/inc/base.inc"
#include "/inc/client.inc"

void run_ed()
{
    // Just to make sure.
    call_out(#'shutdown, __HEART_BEAT_INTERVAL__, 1);

    rm("/dummy");
    ed("/dummy","ed_ends");
}

void ed_ends()
{
    string result = read_file("/dummy");
    // Either the second 's' command was successful
    // (repeat the last substitution, which is not yet supported)
    // or it was rejected. Both are okay, as long as it is not
    // executed with some arbitrary replacement string.
    int success = (result == "bbbbbbbb\naaaa\n"
                || result == "bbbbbbbb\nbbbbbbbb\n");
    if(!success)
        msg("Wrong result: %Q\n", result);

    rm("/dummy");
    shutdown(!success);
}

void send_ed_cmds()
{
    write("a\naaaa\naaaa\n.\n1\ns/a/bb/g\n2\ns\nw\nq\n");
}

void run_test()
{
    msg("\nRunning test for #0000722:\n"
          "--------------------------\n");

    connect_self("run_ed", "send_ed_cmds");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
