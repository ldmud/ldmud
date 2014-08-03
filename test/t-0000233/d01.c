TEST_VIRTUAL inherit "b";
TEST_VIRTUAL inherit "c";

int count_both()
{
    b::get_count();

    return c::get_count();
}

int check_locality()
{
    return b::get_local_program() == "b" && c::get_local_program() == "c";
}
