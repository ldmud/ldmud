/* Even though we inherit private, the struct should be visible here. */
private functions inherit "ti-struct";

int run_test()
{
    struct s v = (<s> 42);
    return v.member;
}
