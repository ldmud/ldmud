private functions virtual inherit "a";

private string get_program()
{
    return "b2";
}

string get_b2_program()
{
    return get_local_program();
}

int get_b2_count()
{
    return a::get_count();
}
