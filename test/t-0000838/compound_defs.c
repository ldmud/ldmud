#pragma save_types, rtt_checks

/* A structure referred to by another structure. */
struct base
{
};

struct compound
{
    struct base* member;
};

struct sub (base)
{
};

/* After reactivation, <base> will be the old reactivated struct,
 * <compound> and <sub> might not be reactivated and thus have the
 * new <base> as its member resp. base.
 */
