/* function closure test 1
 * 
 * Test redeclaring a closure argument variabel in the closure context.
 */

closure t()
{
    return function void(mixed a) : mixed a = 42; { };
}
