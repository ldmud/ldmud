/* Test default values for functions.
 */

#include "/inc/msg.inc"

int fun1(int a = 10);
int fun2(int a = 20);
int fun1(int a = 30);
int fun2(int a = 40);
int fun1(int a = 50) { return a; }
int fun2(int a = 60) { return a; }

int run_test()
{
    return fun1() == 50 && fun2() == 60;
}
