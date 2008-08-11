// Indices into the array of a test.
#define T_NAME          0       // The name of the test
#define T_FLAGS         1       // Some Flags
#define T_CLOSURE       2       // The function to be evaluated

// Flags
#define TF_ERROR        1       // The function should generate an error.

void run_array(mixed* testarray, closure callback)
{
    int errors;
    
    // If there's an error:
    call_out(callback, 0, 1);
    
    foreach(mixed test: testarray)
    {
        msg("Running Test %s...", test[T_NAME]);

        if(test[T_FLAGS]&TF_ERROR)
        {
            if(!catch(funcall(test[T_CLOSURE]);nolog))
	    {
		errors++;
                msg(" FAILURE! (There was no error.)\n");
	    }
            else
                msg(" Success.\n");
        }
        else
        {
            if(funcall(test[T_CLOSURE]))
                msg(" Success.\n");
            else
	    {
		errors++;
                msg(" FAILURE! (Wrong result.)\n");
	    }
        }
    }
    
    remove_call_out(callback);

    funcall(callback, errors && 1);
}
