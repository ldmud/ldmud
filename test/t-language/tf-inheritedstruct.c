/* The struct is already private in tl-inheritedstruct,
 * so we should not be able to see it.
 */
private structs inherit "tl-inheritedstruct";

int run_test()
{
    struct s v = (<s> 42);
    return v.member;
}
