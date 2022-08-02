#pragma strict_types

void fun()
{
    // The compiler should disallow that.
    symbol** a;

    &a = ({ ({'sym}), ({"A"}) });
}
