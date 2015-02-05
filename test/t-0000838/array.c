#pragma save_types, strong_types, rtt_checks

struct abc
{};

/* We check whether struct reactivation also
 * catches arrays of structs (which it didn't.)
 */
struct abc* fun() { return ({ (<abc>) }); }
