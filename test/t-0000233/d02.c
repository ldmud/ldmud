TEST_VIRTUAL inherit "c";
#ifdef MIDDLE_INHERITS
inherit "m1";
inherit "m2";
#endif
TEST_VIRTUAL inherit "b";

int count_both()
{
    b::get_count();

    return c::get_count();
}

int check_locality()
{
    return b::get_local_program() == "b" && c::get_local_program() == "c";
}
