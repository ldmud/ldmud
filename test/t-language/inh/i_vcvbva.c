virtual inherit "/inh/c";
virtual inherit "/inh/b";
virtual inherit "/inh/a";

public string cba_fun()
{
    return sprintf("%s_%s_%s", c_fun(), b_fun(), a_fun());
}

public string cba_var()
{
    sin(10) + a_var;
    return sprintf("%s_%s_%s", c_var, b_var, a_var);
}

public string get_cba_var()
{
    return sprintf("%s_%s_%s", get_c_var(), get_b_var(), get_a_var());
}
