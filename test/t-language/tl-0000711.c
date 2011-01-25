closure test()
{
    // The compiler erroneously takes the type of the
    // context variable as the type of the first parameter.
    return function void(string s) : float val;
    {
        // So, when this happens, the following will throw an error like
        // 'Bad arg 1 type to capitalize(): got float, expected string.'
        capitalize(s);

    };
}

int run_test()
{
    return closurep(test());
}
