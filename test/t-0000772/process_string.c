string destruct_me()
{
    destruct(this_object());
    return "success";
}

string okay()
{
    return "!";
}

string do_process_string()
{
    return process_string("@@destruct_me@@@@okay@@");
}
