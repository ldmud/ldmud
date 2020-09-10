/* Test default values for functions.
 */

#pragma strong_types, save_types, rtt_checks

int ifun(int a, int b = 100*a, int c = 2000)
{
    return a + b + c;
}
