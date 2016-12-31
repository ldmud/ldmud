virtual inherit "/inh/a";
virtual inherit "/inh/b";
virtual inherit "/inh/c";

public string abc_fun()
{
    return sprintf("%s_%s_%s", a_fun(), b_fun(), c_fun());
}

public string abc_var()
{
    return sprintf("%s_%s_%s", a_var, b_var, c_var);
}

public string get_abc_var()
{
    return sprintf("%s_%s_%s", get_a_var(), get_b_var(), get_c_var());
}
