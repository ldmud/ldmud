#pragma strict_types

int testfun(int i)
{
    return i;
}

mixed fun()
{
    return testfun(this_object()->test());
}
