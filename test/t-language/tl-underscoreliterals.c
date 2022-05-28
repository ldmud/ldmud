/* Test that numeric literals with underscores for readability lex to the
 * correct values.
 */
#pragma strong_types, save_types, pedantic

private int** testcases = ({
  /*
   * Format:
   *   ({ underscore literal, expected value })
   */

  /*
   * Base 10 literals
   */
  ({ 1_0_0_0, 1000 }),
  ({ 1_000_000, 1000000 }),
  ({ 99_99, 9999 }),

  /*
   * Base 2 literals
   */
  ({ 0b1_0_1_0, 0b1010 }),
  ({ 0B101_0, 0b1010 }),

  /*
   * Base 8 literals
   */
  ({ 0o55_55, 0o5555 }),
  ({ 0o5_5_55, 0o5555 }),

  /*
   * Base 16 literals
   */
  ({ 0xF_F, 0xFF }),
  ({ 0XC0_FF_ee, 0xC0FFEE }),
});

int run_test() {
  foreach(int* testcase : testcases) {
    if(testcase[0] != testcase[1]) {
      return 0;
    }
  }
  return 1;
}
