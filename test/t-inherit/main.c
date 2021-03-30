inherit "regular";
virtual inherit "virtual1";

string main_var = "main";

closure get_main_lambda()
{
    return lambda(0, ({#'main_var}));
}
