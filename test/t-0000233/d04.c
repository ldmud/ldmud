virtual inherit "a";
#ifdef MIDDLE_INHERITS
inherit "m1";
inherit "m2";
#endif
TEST_VIRTUAL inherit "b";

private string c_var = "c";

int c_calc(int x)
{
    return (calc(x) + a::calc(x))/2;
}

int c_wildcard_calc()
{
    return sizeof("*"::calc(0));
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

private string get_program()
{
    return "d";
}

int count_both()
{
    b::get_count();

    return a::get_count();
}


int check_locality()
{
    return b::get_local_program() == "b" && a::get_local_program() == "d";
}
