inherit "inh/prog_protected";
virtual private inherit "inh/prog_visible";

int run_test()
{
    var = 42;
    return fun() == 42;
}
