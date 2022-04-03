/* Check that the struct definition from test.c didn't persist.
 * This would conflict with the following definition.
 */
struct sefunF001 { int i;};

struct sefunF001 x = (<sefunF001> 42);
