/* The index operator should not be callable as an efun. */
void fun()
{
    mixed a, b;

    index(a, b);
}
