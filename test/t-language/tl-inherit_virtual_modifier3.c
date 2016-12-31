inherit "inh/prog_private";
virtual inherit "inh/prog_visible";

int run_test()
{
    var = 42;
    return fun() == 42;
}
