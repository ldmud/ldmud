private TEST_VIRTUAL_C_A inherit "a";
private TEST_VIRTUAL_C_B inherit "b";
#ifdef PRELOAD_D
private TEST_VIRTUAL_E_D inherit "d";
#endif

void create()
{
    "a"::set_var("c_a");
    "b"::set_var("c_b");
}
