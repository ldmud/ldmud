virtual inherit "a";
inherit "b";

int c_calc(int x)
{
    return (calc(x) + a::calc(x))/2;
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
