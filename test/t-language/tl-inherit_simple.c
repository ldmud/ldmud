inherit "inh/a";
inherit "inh/b";
inherit "inh/c";

int run_test()
{
    if (a_var != "a" || b_var != "b" || c_var != "c")
        return 0;

    if (get_a_var() != "a" || get_b_var() != "b" || get_c_var() != "c")
        return 0;

    if (a_fun() != "a" || b_fun() != "b" || c_fun() != "c")
        return 0;

    if (get_a_fun() != "a" || get_b_fun() != "b" || get_c_fun() != "c")
        return 0;

    set_a_var("aa");
    set_b_var("bb");
    set_c_var("cc");

    if (a_var != "aa" || b_var != "bb" || c_var != "cc")
        return 0;

    return 1;
}
