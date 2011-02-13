// Indices into the array of a test.
#define TI_NAME          0       // The name of the test
#define TI_FLAGS         1       // Some Flags
#define TI_CLOSURE       2       // The function to be evaluated

// Flags
#define TF_ERROR          1       // The function should generate an error.
#define TF_DONTCHECKERROR 2       // Doesn't matter if error or not, as long as it does not crash.

int run_array_without_callback(mixed* testarray)
{
    int errors;
    foreach(mixed test: testarray)
    {
        msg("Running Test %s...", test[TI_NAME]);

        if(test[TI_FLAGS]&TF_ERROR)
        {
            if(!catch(funcall(test[TI_CLOSURE]);nolog))
            {
                errors++;
                msg(" FAILURE! (There was no error.)\n");
            }
            else
                msg(" Success.\n");
        }
        else if (test[TI_FLAGS]&TF_DONTCHECKERROR)
        {
            catch(funcall(test[TI_CLOSURE]);nolog);
            // if we surived the test: great
            msg(" Success.\n");
        }
        else
        {
            if(funcall(test[TI_CLOSURE]))
                msg(" Success.\n");
            else
            {
                errors++;
                msg(" FAILURE! (Wrong result.)\n");
            }
        }
    }
    return errors;
}

void run_array(mixed* testarray, closure callback)
{
    int errors;

    // If there's an error:
    call_out(callback, 0, 1);

    errors = run_array_without_callback(testarray);

    remove_call_out(callback);

    funcall(callback, errors && 1);
}
