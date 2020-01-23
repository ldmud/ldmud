#pragma save_types, rtt_check

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
