#ifdef SUB_INHERIT
virtual inherit "i";
#endif

#ifdef OLD_VARIABLES
mapping d1 = ([]), d2 = ([]), d3 = ([]);
#endif

int num;

private int internal_calc(int x)
{
    return x+5;
}

int calc(int x)
{
    return internal_calc(x+5);
}

string fun_a1()
{
    return "a1";
}

int get_count()
{
    return ++num;
}

string get_program();
string get_local_program()
{
    return get_program();
}
