inherit "ti-virtual-b";

// fun() is private in B (but public in virtual inherit A),
// and therefore should not be callable.

void test()
{
    ::fun();
}
