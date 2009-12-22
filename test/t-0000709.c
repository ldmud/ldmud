#include "/inc/base.inc"

int success;

void testcall(mixed a, mixed b, mixed c)
{
    /* Verify that we've been called with all arguments intact. */
    success = (a==14 && !b && c==this_object());
}

int test32()
{
    /* Bytecode:
     *   F_SAVE_ARG_FRAME
     *   F_CSTRING0 00 ("t-0000709.c")
     *   F_CSTRING0 01 ("testcall")
     *   F_CLIT     14 (It is important, that 14 == F_NUMBER)
     *   F_CONST0      (last_expression will point to this instruction)
     *   F_THIS_OBJECT (will decrement last_expression on defective drivers
     *                  so it will point to 14 (F_NUMBER))
     *   F_CALL_OTHER
     *   F_RESTORE_ARG_FRAME
     *   F_POP_VALUE   (will look at last_expression (14 = F_NUMBER), the
     *                  distance between F_POP_VALUE and 14 must be
     *                  1 + sizeof(p_int) to trigger this bug.)
     */
    __FILE__->testcall(14, 0, this_object());
    return 1;
}

int test64()
{
    /* Bytecode is the same as in test32(), except that we use symbolp()
     * to fill up some space to get the distance right for ia64 machines.
     */
    __FILE__->testcall(14, symbolp(symbolp(symbolp(symbolp(0)))), this_object());
    return 1;
}

void run_test()
{
    msg("\nRunning test for #0000709:\n"
          "--------------------------\n");

    call_out(#'shutdown, 0, 1); // Just to make sure.

    test32();
    if(!success)
    {
        shutdown(1);
        return;
    }
    else
        success = 0;

    test64();
    shutdown(!success);

    remove_call_out(#'shutdown);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
