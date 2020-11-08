int i = 42;
string text = "A";
string var;

void set_var(string str)
{
    var = str;
}

string get_var()
{
    return var;
}

string get_text()
{
    return text;
}

closure get_lfun_cl()
{
    return #'get_text;
}

closure get_var_cl()
{
    return #'text;
}
