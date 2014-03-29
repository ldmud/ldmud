inherit "inh/i_a1";
inherit "inh/i_a2";

int run_test()
{
    if ("i_a1"::get_a_var() != "a" || "i_a2"::get_a_var() != "a")
        return 0;

    if ("i_a1"::a_fun() != "a" || "i_a2"::a_fun() != "a")
        return 0;

    if ("i_a1"::get_a_fun() != "a" || "i_a2"::get_a_fun() != "a")
        return 0;

    "i_a1"::set_a_var("aa");
    "i_a2"::set_a_var("bb");

    if ("i_a1"::get_a_var() != "aa" || "i_a2"::get_a_var() != "bb")
        return 0;

    return 1;
}
