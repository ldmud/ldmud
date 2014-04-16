virtual inherit "a";

int b_calc(int x)
{
    return (calc(x) + a::calc(x))/2;
}

private string get_program()
{
    return "b";
}
