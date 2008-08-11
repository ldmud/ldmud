#include "msg.inc"

static void start_gc(closure callback)
{
    garbage_collection("/gc.log");
    call_out("check_gc", 1, 0, callback);
}

static void check_gc(int nr, closure callback)
{
    string s = read_file("/gc.log");
    int error = 1;
    
    if(!stringp(s))
    {
	if(!nr)
	{
	    /* Wait one more cycle. */
	    call_out("check_gc", 1, 1, callback);
	    return;
	}
	msg("FAILURE: Could not read the garbage collection file.\n");
    }
    else
    {
	string *text = explode(s, "\n");
	
	msg(s);
	if(!sizeof(regexp(text, "freeing.*block")))
	{
	    msg("Success: No lost block.\n");
	    error = 0;
	}
	else
	    msg("FAILURE: Found lost block during garbage collection.\n");
    }
    
    rm("/gc.log");
    funcall(callback, error);
}
