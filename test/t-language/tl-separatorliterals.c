/* Test that numeric literals with separators for readability lex to the
 * correct values.
 */
#pragma strong_types, save_types, pedantic

private int** testcases = ({
  /*
   * Format:
   *   ({ separated literal, expected value })
   */

  /*
   * Base 10 literals
   */
  ({ 1'0'0'0, 1000 }),
  ({ 1'000'000, 1000000 }),
  ({ 99'99, 9999 }),

  /*
   * Base 2 literals
   */
  ({ 0b1'0'1'0, 0b1010 }),
  ({ 0B101'0, 0b1010 }),

  /*
   * Base 8 literals
   */
  ({ 0o55'55, 0o5555 }),
  ({ 0o5'5'55, 0o5555 }),

  /*
   * Base 16 literals
   */
  ({ 0xF'F, 0xFF }),
  ({ 0XC0'FF'ee, 0xC0FFEE }),
});

int run_test() {
  foreach(int* testcase : testcases) {
    if(testcase[0] != testcase[1]) {
      return 0;
    }
  }
  return 1;
}
