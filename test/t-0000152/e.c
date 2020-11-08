private TEST_VIRTUAL_E_C inherit "c";
private TEST_VIRTUAL_E_D inherit "d";

void create()
{
    "*"::create();
    replace_program("d");
}

int test()
{
    return "d"::test();
}

closure get_lfun_cl()
{
    return "d"::get_lfun_cl();
}

closure get_var_cl()
{
    return "d"::get_var_cl();
}
