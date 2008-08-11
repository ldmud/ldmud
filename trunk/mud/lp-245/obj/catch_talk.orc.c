object next;
string super;
string match, fun, ob_str, type;

void link(object ob) {
    next = ob;
}

/*
 * The function to call.
 */
void set_type(string t) {
    type = t;
}

/*
 * The string to match.
 */
void set_match(string str) {
    match = str;
}

/*
 * The function to call.
 */
void set_function(string f) {
    fun = f;
}

/*
 * The object to call.
 */
void set_object(string ob) {	/* NOTE: a string */
    ob_str = ob;
}

int test_match(string str) {
    string who,str1;

    if(sscanf(str,"%s " + type + " %s\n",who,str1) == 2){
	if (str1 == match)
	    return call_other(ob_str, fun, str);
    }
    if (next)
	return next->test_match(str);
    else
	return 0;
}

object remove_match(string str) {
    if (str == match) {
	destruct(this_object());
	return next;
    }
    if (next)
	next = next->remove_match(str);
    return this_object();
}

void collaps()
{
    if(next)
	next->collaps();
    destruct(this_object());
}

