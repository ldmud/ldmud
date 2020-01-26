int check()
{
    // This should compile and execute without error
    <int|string>* args = ({"A", "", 1, 2});

    return python_typecheck(args...) == "A";
}
