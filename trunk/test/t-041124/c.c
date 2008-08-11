inherit "/a";
inherit "/b";

#include "/inc/msg.inc"

string sub()
{
    return "c";
}

int check_closure(string file, string name, closure cl)
{
    int errors;
    
    msg("Checking: get_type_info(%s, 3) == \"/%s\":\t\t\t", name, file);
    if(get_type_info(cl, 3) == "/"+file)
	msg("Success.\n");
    else
    {
	errors++;
	msg("FAILURE.\n");
    }

    msg("Checking: funcall(%s) == \"%s\":\t\t\t\t", name, file);
    if(funcall(cl) == file)
	msg("Success.\n");
    else
    {
	errors++;
	msg("FAILURE.\n");
    }

    msg("Checking: funcall(lambda(0, ({%s}))) == \"%s\": \t\t", name, file);
    if(funcall(lambda(0, ({cl}))) == file)
	msg("Success.\n");
    else
    {
	errors++;
	msg("FAILURE.\n");
    }
    
    msg("Checking: restore_value(save_value(%s)) == %s:    \t", name, name);
    if(restore_value(save_value(cl)) == cl)
	msg("Success.\n");
    else
    {
	errors++;
	msg("FAILURE.\n");
    }

    msg("Checking: funcall(restore_value(save_value(%s))) == \"%s\":\t", name, file);
    if(funcall(restore_value(save_value(cl))) == file)
	msg("Success.\n");
    else
    {
	errors++;
	msg("FAILURE.\n");
    }
    
    return errors;
}

int run_test()
{
    int errors;

    errors += check_closure("a", "#'a::sub", #'a::sub);
    errors += check_closure("b", "#'b::sub", #'b::sub);
    errors += check_closure("a", "#'::sub", #'::sub);
    errors += check_closure("c", "#'sub", #'sub);

    msg("Checking: a::sub() == \"a\":\t\t\t\t\t");
    if(a::sub() == "a")
	msg("Success.\n");
    else
    {
	errors++;
	msg("FAILURE.\n");
    }
    msg("Checking: b::sub() == \"b\":\t\t\t\t\t");
    if(b::sub() == "b")
	msg("Success.\n");
    else
    {
	errors++;
	msg("FAILURE.\n");
    }
    msg("Checking: sub() == \"c\":\t\t\t\t\t\t");
    if(sub() == "c")
	msg("Success.\n");
    else
    {
	errors++;
	msg("FAILURE.\n");
    }

    return errors;
}
