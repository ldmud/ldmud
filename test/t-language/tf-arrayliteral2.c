#pragma strict_types

void afun(int *a)
{
}

void fun()
{
    // The compiler should disallow that.
    afun(({ 1, "2" }));
}
