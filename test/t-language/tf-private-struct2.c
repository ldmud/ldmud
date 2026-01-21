/* We check whether the struct in tl-private-struct2 is
 * really private.
 */
public inherit "ti-private-struct2";

int run_test()
{
    struct s v = (<s> 42);
    return v.member2;
}
