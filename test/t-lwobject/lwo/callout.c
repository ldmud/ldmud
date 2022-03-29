#pragma save_types, strong_types, rtt_checks, lightweight

/* Just call the given closure every second.
 *
 * We test here constructor arguments and lightweight objects
 * that live only in the callout structure.
 */
void check(closure callback)
{
    call_out(#'check, 1, callback);
    funcall(callback);
}

void new(closure callback)
{
    call_out(#'check, 0, callback);
}
