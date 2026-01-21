// We test calling a function with the same name as an inherited fun.
// This shouldn't crash the compiler.
inherit "ti-argtypes";

int run_test()
{
    int fun;
    ::fun("A",this_object());
    fun(0,0);

    return 1;
}
