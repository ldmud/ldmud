/* Testscenario XXXXXXX
 *
 * Test negative start offsets given to efun::read_bytes()
 */

#include "/inc/base.inc"
#include "/inc/gc.inc"

private struct testCase {
  string name;
  int start;
  int end;
  string expected;
};

void run_test()
{
    msg("\nRunning test for t-XXXXXXX:\n"
          "--------------------------\n");

    string filename = "small.file.txt";
    string full_content = to_text(read_bytes(filename), "UTF-8");
    string *failed = ({ });

    struct testCase *test_cases = ({
      (<testCase> "0 start offset, no size", 0, 0, full_content ),
      (<testCase> "0 start offset, 4 size", 0, 4, full_content[0..3] ),
      (<testCase> "2 start offset, no size", 2, 0, full_content[2..] ),
      (<testCase> "2 start offset, 4 size", 2, 4, full_content[2..5] ),
      (<testCase> "-8 start offset, no size", -8, 0, full_content[<8..] ),
      (<testCase> "-8 start offset, 4 size", -8, 4, full_content[<8..<5] ),
      (<testCase> "-9999 start offset, no size", -9999, 0, full_content ),
      (<testCase> "-9999 start offset, 4 size", -9999, 4, full_content[..3] ),
    });

    foreach(struct testCase test : test_cases) {
      msg(sprintf("Test case: %Q\n", test->name));

      bytes result;
      if(test->end) {
        result = read_bytes(filename, test->start, test->end);
      } else {
        result = read_bytes(filename, test->start);
      }

      if(!bytesp(result) || to_text(result, "UTF-8") != test->expected) {
        msg(sprintf("Failed. Got %Q expected %Q\n", 
              bytesp(result) ? to_text(result, "UTF-8") : result, 
              test->expected));
        failed += ({ test->name });
      }
    }

    if(!sizeof(failed)) {
      start_gc(#'shutdown);
      return;
    }

    shutdown(1);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
