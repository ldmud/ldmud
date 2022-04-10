/* We define our own struct, check that we can use both variants. */
#pragma strict_types, rtt_checks

struct SomeStruct
{
    closure cl1, cl2;
};

int check_struct_mine(struct SomeStruct x, closure cl1)
{
    return x.cl1 == cl1;
}

int check_struct_sefun(struct sefun::SomeStruct x, string str)
{
    return x.text == str;
}

int run_test()
{
    struct SomeStruct mine1 = (<SomeStruct> #'funcall, #'apply);
    struct ::SomeStruct mine2 = (<::SomeStruct> #'this_object, #'previous_object);
    struct sefun::SomeStruct sefun = (<sefun::SomeStruct> "Test");

    mixed mine_var = mine1;
    mixed sefun_var = sefun;

    /* RTTC should catch calling wrong functions. */
    return check_struct_mine(mine1, #'funcall)
        && check_struct_mine(mine2, #'this_object)
        && check_struct_sefun(sefun, "Test")
        && catch(check_struct_mine(sefun_var, 0))
        && catch(check_struct_sefun(mine_var, 0))
        && 1;
}
