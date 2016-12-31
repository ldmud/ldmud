virtual inherit "a";

private string b_var = "b";

int b_calc(int x)
{
    return (calc(x) + a::calc(x))/2;
}

private string get_program()
{
    return "b";
}

string get_b_var()
{
    return b_var;
}

closure get_b_var_cl()
{
    return #'b_var;
}

closure get_b_var_cl2()
{
    return lambda(0, ({#'b_var}));
}
