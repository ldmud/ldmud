/* We omit the type on several declarations.
 * Should not load, but also not crash the driver.
 */
a;

int b = a->test;

c, d;

void fun1(int e)
{
    funcall(function int(e) { return e->test; });
}

fun2(f)
{
    f->test;
}

