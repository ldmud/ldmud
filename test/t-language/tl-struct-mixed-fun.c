/* Make sure we can access 'struct mixed' declared structs.
 */
#pragma save_types

inherit "ti-struct";

struct mixed fun()
{
    return (<s> 42);
}

int run_test()
{
    return fun().member == 42;
}
