TEST_VIRTUAL inherit "b";
TEST_VIRTUAL inherit "d";

int count_both()
{
    b::get_count();

    return d::get_count();
}

int check_locality()
{
    return b::get_local_program() == "b" && d::get_local_program() == "c";
}
