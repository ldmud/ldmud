#ifdef SUB_INHERIT
virtual inherit "i";
#endif

#ifdef NEW_VARIABLES
string dummy = "Hello, World!"; // Just to change the variable indices...
#endif

int num;
protected string a_var;

void dummy_fun() { raise_error("Please don't call me!\n"); }

private int internal_calc(int x)
{
    return x+12;
}

int calc(int x)
{
    return internal_calc(x+8);
}

string fun_a2()
{
    return "a2";
}

int get_count()
{
    return ++num;
}

string get_program();
string get_local_program()
{
    return get_program();
}

closure get_a_var_cl()
{
    a_var = "a";
    return #'a_var;
}

closure get_a_var_cl2()
{
    return lambda(0, ({#'a_var}));
}

closure get_a_var_cl3()
{
    return symbol_variable("a_var");
}

closure get_a_var_cl4()
{
    return lambda(0, ({symbol_variable("a_var")}));
}

closure get_a_calc_cl()
{
    return #'calc;
}
