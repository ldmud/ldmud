/* Make sure we can access 'struct mixed' declared structs.
 */
inherit "ti-struct";

int run_test()
{
    struct mixed x = (<s> 42);

    return x.member == 42 && x->whatever == 0;
}
