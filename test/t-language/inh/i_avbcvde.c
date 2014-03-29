inherit "/inh/a";
virtual inherit "/inh/b";
inherit "/inh/c";
virtual inherit "/inh/d";
inherit "/inh/e";

public string abcde_fun()
{
    return sprintf("%s_%s_%s_%s_%s", a_fun(), b_fun(), c_fun(), d_fun(), e_fun());
}

public string abcde_var()
{
    return sprintf("%s_%s_%s_%s_%s", a_var, b_var, c_var, d_var, e_var);
}

public string get_abcde_var()
{
    return sprintf("%s_%s_%s_%s_%s", get_a_var(), get_b_var(), get_c_var(), get_d_var(), get_e_var());
}
