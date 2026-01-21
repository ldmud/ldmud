/* We check that two private structs don't conflict with each other.
 * And we can do compile-time lookups.
 */
public inherit "ti-private-struct";
public inherit "ti-private-struct2";

int run_test1()
{
#if __EFUN_DEFINED__(last_instructions)
    string *instrs;

    if (sv.member != 42)
        return 0;

    instrs = last_instructions(20, 0);
    reverse(&instrs);

    foreach (int i: sizeof(instrs))
    {
        if ("  .  " in instrs[i])
        {
            /* -1 as an argument to index operation
             * would mean runtime lookup.
             */
            return !("  nconst1 " in instrs[i+1]);
        }
    }

    /* Instruction not found? */
    return 0;
#else
    return sv.member == 42;
#endif
}

int run_test2()
{
#if __EFUN_DEFINED__(last_instructions)
    string *instrs;

    if (sv2.member2 != 42)
        return 0;

    instrs = last_instructions(20, 0);
    reverse(&instrs);

    foreach (int i: sizeof(instrs))
    {
        if ("  .  " in instrs[i])
        {
            /* -1 as an argument to index operation
             * would mean runtime lookup.
             */
            return !("  nconst1 " in instrs[i+1]);
        }
    }

    /* Instruction not found? */
    return 0;
#else
    return sv2.member2 == 42;
#endif
}

int run_test()
{
    return run_test1() && run_test2();
}
