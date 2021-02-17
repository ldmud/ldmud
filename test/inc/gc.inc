#include "msg.inc"

static void start_gc(closure callback)
{
    garbage_collection(__MASTER_OBJECT__ ".gc.log");
    call_out("check_gc", 1, 0, callback);
}

static void check_gc(int nr, closure callback)
{
    string s = read_file(__MASTER_OBJECT__ ".gc.log");
    int error = 1;
    
    if(!stringp(s))
    {
	if(!nr)
	{
	    /* Wait one more cycle. */
	    call_out("check_gc", __ALARM_TIME__, 1, callback);
	    return;
	}
	msg("Could not read the garbage collection file.\n"
	    "The allocator has probably no suppport for garbage collection.\n");
	error = 0;
    }
    else
    {
	string *text = explode(s, "\n");
	
	msg("%s\n", s);
	if(!sizeof(regexp(text, "freeing.*block|tabled string.*was left unreferenced")))
	{
	    msg("Success: No lost block.\n");
	    error = 0;
	}
	else
	    msg("FAILURE: Found lost block during garbage collection.\n");
    }
    
    rm(__MASTER_OBJECT__ ".gc.log");
    funcall(callback, error);
}
