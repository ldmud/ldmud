void create()
{
    write_file("/compile_check_side_effect", "top create\n");
}

void reset()
{
    write_file("/compile_check_side_effect", "top reset\n");
}
