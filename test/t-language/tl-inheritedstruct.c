// Not for compile_string()
/* Even though we inherit private, the struct should be visible here. */
private structs inherit "ti-struct";

int run_test()
{
    struct s v = (<s> 42);
    return v.member;
}
