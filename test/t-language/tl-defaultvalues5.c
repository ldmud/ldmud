public mixed prototype(string arg1, mixed arg2, int opt_arg = 42);

int run_test()
{
    // This should throw an undefined function error.
    // But most importantly, it should not crash.
    string err = catch(prototype("ABC", "123", 789); publish);
    return sizeof(err) && "Undefined" in err;
}
