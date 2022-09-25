// This is the same as ti-private-struct.c, but to use it in a
// separate test to check ref-counting.
private struct s
{
    int member;
};

public struct s sv = (<s> 42);
