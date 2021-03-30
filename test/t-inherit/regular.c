virtual inherit "virtual1";
virtual inherit "virtual2";

string regular_var = "regular";

closure get_regular_lambda()
{
    return lambda(0, ({#'regular_var}));
}
