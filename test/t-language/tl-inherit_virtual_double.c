inherit "inh/i_va1";
inherit "inh/i_va2";

int run_test()
{
    if ("i_va1"::get_a_var() != "a" || "i_va2"::get_a_var() != "a")
        return 0;

    if ("i_va1"::a_fun() != "a" || "i_va2"::a_fun() != "a")
        return 0;

    if ("i_va1"::get_a_fun() != "a" || "i_va2"::get_a_fun() != "a")
        return 0;

    "i_va1"::set_a_var("aa");

    if ("i_va1"::get_a_var() != "aa" || "i_va2"::get_a_var() != "aa")
        return 0;

    "i_va2"::set_a_var("bb");

    if ("i_va1"::get_a_var() != "bb" || "i_va2"::get_a_var() != "bb")
        return 0;

    return 1;
}
