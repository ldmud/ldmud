/* We check whether the struct in tl-private-struct is
 * really private.
 */
public inherit "ti-private-struct";

int run_test()
{
    struct s v = (<s> 42);
    return v.member;
}
