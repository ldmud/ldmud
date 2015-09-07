/* The compiler should allow struct runtime lookups to
 * members that don't yet exist, but derived structs may have.
 */

struct a_t
{
    mapping m; // compiles without this line
};

struct b_t (a_t)
{
    int i;
};

void fun(struct a_t a, string s)
{
    a->(s) += 42;
}

int run_test()
{
    struct b_t b = (<b_t> ([]));

    fun(b, "i");

    return b->i == 42;
}
