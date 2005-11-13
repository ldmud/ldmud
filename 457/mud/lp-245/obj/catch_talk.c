object next;
string super;
string match, fun, ob_str, type;

/*
short() { return type + "#" + match; }
*/

link(ob) {
    next = ob;
}

/*
 * The function to call.
 */
set_type(t) {
    type = t;
}

/*
 * The string to match.
 */
set_match(str) {
    match = str;
}

/*
 * The function to call.
 */
set_function(f) {
    fun = f;
}

/*
 * The object to call.
 */
set_object(ob) {	/* NOTE: a string */
    ob_str = ob;
}

test_match(str) {
    string who,str1;

    if(sscanf(str,"%s " + type + match + " %s\n",who,str1) == 2 ||
       sscanf(str,"%s " + type + match + "\n",who) == 1 ||
       sscanf(str,"%s " + type + match + "%s\n",who,str1) == 2 ||
       sscanf(str,"%s " + type + " " + match + "\n",who) == 1 ||
       sscanf(str,"%s " + type + " " + match + " %s\n",who,str1) == 2)
    {
	    return call_other(ob_str, fun, str);
    }
    if (next)
	return next->test_match(str);
    else
	return 0;
}

remove_match(str) {
    if (str == match) {
	destruct(this_object());
	return next;
    }
    if (next)
	next = next->remove_match(str);
    return this_object();
}

collaps()
{
    if(next)
	next->collaps();
    destruct(this_object());
}

drop() { return 1; }
