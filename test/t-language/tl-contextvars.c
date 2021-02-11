int run_test()
{
    int result = 10;

    return funcall(function int()
    {
        return funcall(function int() : int result = result+1
        {
            return result;
        });
    }) == 11;
}
