int x = 42;

int fun1()
{
    int x = x - 1;
    return x;
}

closure fun2()
{
    int a = 1;
    closure cl = function int() : int a = &a { return a; };

    a = 2;
    return cl;
}

int run_test()
{
    return fun1() == 41 && funcall(fun2()) == 2;
}
