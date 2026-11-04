void create()
{
    write_file("/compile_check_side_effect", "child create\n");
}

void reset()
{
    write_file("/compile_check_side_effect", "child reset\n");
}
