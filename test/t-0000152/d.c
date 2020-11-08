private TEST_VIRTUAL_D_A inherit "a";
private TEST_VIRTUAL_D_B inherit "b";

void create()
{
    "a"::set_var("d_a");
    "b"::set_var("d_b");
}

closure get_lfun_cl()
{
    return "a"::get_lfun_cl();
}

closure get_var_cl()
{
    return "a"::get_var_cl();
}

int test()
{
    return "a"::get_text() == "A" && "b"::get_text() == "B" &&
           "a"::get_var() == "d_a" && "b"::get_var() == "d_b";
}
