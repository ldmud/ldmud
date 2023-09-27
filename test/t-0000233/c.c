#ifdef DOUBLE_INHERIT
inherit "c2";
#endif
virtual inherit "a";

private string c_var = "c";

int c_calc(int x)
{
    return (calc(x) + a::calc(x))/2;
}

int c_wildcard_calc()
{
    return sizeof("*"::calc(0));
}

private string get_program()
{
    return "c";
}

string get_c_var()
{
    return c_var;
}

closure get_c_var_cl()
{
    return #'c_var;
}

closure get_c_var_cl2()
{
    return lambda(0, ({#'c_var}));
}

closure get_c_calc_cl()
{
    return #'c_calc;
}
