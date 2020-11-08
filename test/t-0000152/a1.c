string text = "A";
string var;

string get_text()
{
    return text;
}

void set_var(string str)
{
    var = str;
}

string get_var()
{
    return var;
}

closure get_lfun_cl()
{
    return #'get_text;
}

closure get_var_cl()
{
    return #'text;
}
