#pragma save_types, rtt_checks
#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/regexp.h"

struct TestCase
{
    string name;                // Name of the test case.
    string orig_content;        // Original content for file (0 = no file)
    string command;             // Commands to sent in ed.
    string dest_content_re;     // PCRE regular expression to check the file
                                // contents afterwards.
    string dest_output_re;      // PCRE regular expression to check the output
                                // of ed() afterwards.
};

struct TestCase* cases = ({
    (<TestCase>
        name:           "#722",
        orig_content:   0,
        command:        "a\naaaa\naaaa\n.\n1\ns/a/bb/g\n2\ns\nw\nq\n",
        // Either the second 's' command was successful
        // (repeat the last substitution, which is not yet supported)
        // or it was rejected. Both are okay, as long as it is not
        // executed with some arbitrary replacement string.
        dest_content_re: "^bbbbbbbb\n(aaaa|nbbbbbbbb)\n$",
    ),
    (<TestCase>
        name:           "Printing Unicode Characters",
        orig_content:   "\ua4ef\ua4ed\ua4db\ua4f7\ua4f1\ua4de\n",
        command:        "p\nq\n",
        dest_output_re: "\n\ua4ef\ua4ed\ua4db\ua4f7\ua4f1\ua4de\n$",
    ),
});

object server, client;

async void sleep(int sec = __ALARM_TIME__)
{
    call_out(#'call_coroutine, sec, this_coroutine());

    yield();
}

async void run_tests_async()
{
    int errors = 0;

    foreach (struct TestCase test: cases)
    {
        string result, output;
        int fail;

        msg("Running Test %s...", test.name);

        // Just to make sure.
        call_out(#'shutdown, 2*__HEART_BEAT_INTERVAL__, 1);

        server.start_ed(test.orig_content, this_coroutine());
        client.send_ed_cmd(test.command);
        result = yield();
        output = await(client.get_output());

        if (test.dest_content_re && !regmatch(result, test.dest_content_re, RE_PCRE))
            fail++;
        if (test.dest_output_re && !regmatch(output, test.dest_output_re, RE_PCRE))
            fail++;

        if (fail)
        {
            msg("FAILED.\n");
            errors++;
        }
        else
            msg("Success.\n");

        remove_call_out(#'shutdown);
    }

    shutdown(errors && 1);
}

void set_server(object s)
{
    server = s;
    if (client)
        call_coroutine(run_tests_async());
}

void set_client(object c)
{
    client = c;
    if (server)
        call_coroutine(run_tests_async());
}

void register_server()
{
    __MASTER_OBJECT__.set_server(this_object());
}

void register_client()
{
    input_to("receive_ed_output");
    __MASTER_OBJECT__.set_client(this_object());
}

coroutine ed_cb;
void start_ed(string content, coroutine cb)
{
    if (content)
        write_file("/dummy-ed", content, 1);
    else
        rm("/dummy-ed");

    ed_cb = cb;
    ed("/dummy-ed", "ed_ends");
}

void ed_ends()
{
    string result = read_file("/dummy-ed");
    rm("/dummy-ed");
    call_coroutine(ed_cb, result);
}

void send_ed_cmd(string cmd)
{
    tell_object(this_object(), cmd);
}

string output = "";
void receive_ed_output(string str)
{
    output += (str||"") + "\n";
    input_to("receive_ed_output");
}

async string get_output()
{
    // Wait one cycle, so all inputs are handled.
    await(sleep());

    string result = output;
    output = "";
    return result;
}

void run_test()
{
    msg("\nRunning test for ed():\n"
          "----------------------\n");

    connect_self("register_server", "register_client");
}

string *epilog(int eflag)
{
    set_driver_hook(H_FILE_ENCODING, "UTF-8");
    configure_interactive(0, IC_ENCODING, "UTF-8");

    run_test();
    return 0;
}
