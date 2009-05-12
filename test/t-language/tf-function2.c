/* function closure test 2
 * 
 * Test accessing a closure argument from within the context of a closure.
 */

closure t()
{
    return function void(mixed a) : mixed b = a; { };
}
