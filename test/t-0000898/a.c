// Prototype with varargs.
varargs string sub();

// Definition missing varargs. Should evoke compile error on line 5.
string sub()
{
    return "a";
}
