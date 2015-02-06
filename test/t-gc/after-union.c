int run_test()
{
    return this_object()->fun1() + this_object()->fun2() + 1;
}

/* If <float|int> was entered twice (see before-union.c) then
 * searching the list for <float|symbol> or <int|symbol> will
 * result in an endless loop.
 */
float|symbol x;
int|symbol y;
