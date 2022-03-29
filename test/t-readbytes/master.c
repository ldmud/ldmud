/* Tests for read_bytes():
 *
 * Test negative start offsets given to efun::read_bytes()
 */

#include "/inc/base.inc"

private struct testCase
{
    string name;
    int start;
    int end;
    bytes expected;
};

void run_test()
{
    string filename = "small.file.txt";
    bytes full_content = read_bytes(filename);
    int errors = 0;

    struct testCase *test_cases =
    ({
        (<testCase> "0 start offset, no size",         0, 0, full_content),
        (<testCase> "0 start offset, 4 size",          0, 4, full_content[0..3]),
        (<testCase> "2 start offset, no size",         2, 0, full_content[2..]),
        (<testCase> "2 start offset, 4 size",          2, 4, full_content[2..5]),
        (<testCase> "-8 start offset, no size",       -8, 0, full_content[<8..]),
        (<testCase> "-8 start offset, 4 size",        -8, 4, full_content[<8..<5]),
        (<testCase> "-9999 start offset, no size", -9999, 0, full_content),
        (<testCase> "-9999 start offset, 4 size",  -9999, 4, full_content[..3]),
    });

    msg("\nRunning test for read_bytes():\n"
          "------------------------------\n");

    foreach(struct testCase test: test_cases)
    {
        bytes result;

        msg("Running Test %s... ", test->name);

        if (test->end)
            result = read_bytes(filename, test->start, test->end);
        else
            result = read_bytes(filename, test->start);

        if (bytesp(result) && result == test->expected)
            msg("Success.\n");
        else
        {
            msg("FAILED. Got %Q, expected %Q.\n",
                bytesp(result) ? to_text(result, "UTF-8") : result,
                to_text(test->expected, "UTF-8"));
            errors++;
        }
    }

    shutdown(errors != 0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
