// prototype with varargs.
varargs string sub();

// definition missing varargs. Should evoke warning on L5.
string sub()
{
    return "a";
}
