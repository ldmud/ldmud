#pragma save_types, rtt_checks, lightweight, clone, save_local_names

nosave protected int|float testvar = 42;

string var_testob = "v_testob";
string fun_testob()
{
    return "f_testob";
}

int loaded()
{
    return 1;
}

void create()
{
}

nomask protected int testfun(<int|float> value, varargs string* args)
{
    return sizeof(args);
}

async string* testcoroutine(varargs string* args)
{
    string local = "X";
    return args + ({ yield(sizeof(args)), local });
}
