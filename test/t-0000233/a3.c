// This inherit is just to confuse the compiler.
private int internal_calc(int x)
{
    return x+20;
}

int calc(int x)
{
    return internal_calc(x+20);
}
