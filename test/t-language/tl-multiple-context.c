#pragma strong_types

int fun(closure cb)
{
    return funcall(function int(int value)
    {
        /* We have an inner closure, that references variables from
         * two different outer scopes. The driver should not overwrite
         * the type information.
         */
        return funcall(function int()
        {
            if (value)
            {
                funcall(cb);
                return value;
            }
        });
    }, 1);
}

int run_test()
{
    // If it loads, everything is okay.
    return 1;
}