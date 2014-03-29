inherit "inh/i_vavbvc";
inherit "inh/i_vcvbva";

int run_test()
{
    if(abc_fun() != "a_b_c" || cba_fun() != "c_b_a")
        return 0;

    if(abc_var() != "a_b_c" || cba_var() != "c_b_a")
        return 0;

    if(get_abc_var() != "a_b_c" || get_cba_var() != "c_b_a")
        return 0;

    return 1;
}
