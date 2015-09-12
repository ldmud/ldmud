/* Define a function with types.
 */

#pragma strong_types, save_types, rtt_checks

inherit "ti-rttc";

/* We make a prototype, but never define this function.
 * RTTC at runtime should check a call against the
 * types of the called site and report an error.
 */
varargs void rttc_fun(mapping|symbol str);

int run_test()
{
    // Check direct call and call_other
    return catch(rttc_fun(([]));publish) &&
           catch(this_object()->rttc_fun(([]));publish) &&
           1;
}
