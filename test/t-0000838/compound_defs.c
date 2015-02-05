#pragma save_types, rtt_checks

/* A structure referred to by another structure. */
struct base
{
};

struct compound
{
    struct base* member;
};

/* After reactivation, <base> will be the old reactivated struct,
 * <compound> might not be reactivated and thus have the new <base>
 * as its member
 */
