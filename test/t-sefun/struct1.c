/* Check that the struct definition from test.c didn't persist.
 * We should get a compile error trying to access it.
 */
struct Other {};

struct sefunF001 x = (<sefunF001>);
