#pragma strict_types

void fun()
{
    // The compiler should disallow that.
    int* a = ({ 1, "2" });
}
