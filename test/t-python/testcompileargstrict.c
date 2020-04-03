#pragma strict_types

int check()
{
    // This should give a compile error
    return python_typecheck(this_object()->test(), this_object()->test());
}
