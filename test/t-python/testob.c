#pragma save_types

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
