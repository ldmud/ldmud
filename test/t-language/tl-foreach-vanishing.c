int run_test()
{
    mapping m = (["A": 10, "B": 20 ]);
    int result;

    /* We execute for one entry, then remove all keys.
     * foreach() should skip over the other entry.
     */
    foreach(string name, int value: m)
    {
        result += value;
        m -= ({"A", "B"});
    }

    return result == 10 || result ==  20;
}
