#include "/inc/base.inc"
#include "/inc/client.inc"

#include "/sys/input_to.h"

/* We test several combinations of newlines and how they
 * are handled in charmode and linemode.
 */

struct test
{
    string name;                // Name to print
    bytes send;                 // Bytes to send
    string* receive;            // Strings to receive
    int flags;                  // Flags for input_to
};

struct test* tests = ({
    (<test> "CR LF",  b"A\r\n",      ({"A"}),  0), // Standard
    (<test> "CR NUL", b"B\r\x00",    ({"B"}),  0), // Standard
    (<test> "CR",     b"C\r",        ({"C"}),  0), // Non-standard
    (<test> "LF",     b"D\n",        ({"D"}),  0), // Non-standard
    (<test> "NUL",    b"E\x00F\r\n", ({"EF"}), 0), // Ignored according to standard

    // In charmode those should be passed as they are.
    (<test> "CR LF",  b"A\r\n",      ({"A", "\r", "\n" }),            INPUT_CHARMODE),
    (<test> "CR NUL", b"B\r\x00",    ({"B", "\r", "\0" }),            INPUT_CHARMODE),
    (<test> "CR",     b"C\r",        ({"C", "\r" }),                  INPUT_CHARMODE),
    (<test> "LF",     b"D\n",        ({"D", "\n" }),                  INPUT_CHARMODE),
    (<test> "NUL",    b"E\x00F\r\n", ({"E", "\0", "F", "\r", "\n" }), INPUT_CHARMODE),
});

object client;

void set_client(object cl)
{
    client = cl;
}

object get_client()
{
    return client;
}

/* This is the MUD object */
void do_test(struct test* remaining);
void receive(string msg, string* expected, int flags, struct test* remaining)
{
    if (msg != expected[0])
    {
        msg("FAILED.\n");
        msg("Received: %Q, Expected: %Q\n", msg, expected[0]);
        shutdown(1);
        return;
    }

    if (sizeof(expected) == 1)
    {
        msg("Success.\n");
        do_test(remaining);
    }

    input_to(#'receive, flags, expected[1..], flags, remaining);
}

void do_test(struct test* remaining)
{
    struct test t;

    if (!sizeof(remaining))
    {
        shutdown(0);
        return;
    }

    t = remaining[0];

    msg("Testing %s...", t.name);
    input_to(#'receive, t.flags, t.receive, t.flags, remaining[1..]);
    __MASTER_OBJECT__.get_client().send(t.send);
}

void run_server()
{
    do_test(tests);
}

/* This is the object simulating a player. */
void run_client()
{
    __MASTER_OBJECT__.set_client(this_object());
    call_out(#'shutdown, 3, 1); // If something goes wrong.
}

void send(bytes b)
{
    binary_message(b, 3);
}

void run_test()
{
    msg("\nRunning test for handling of newlines:\n"
          "--------------------------------------\n");

    connect_self("run_server", "run_client");
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
