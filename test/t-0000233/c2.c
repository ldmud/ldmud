private functions virtual inherit "a";

private string get_program()
{
    return "c2";
}

string get_c2_program()
{
    return get_local_program();
}

int get_c2_count()
{
    return a::get_count();
}
