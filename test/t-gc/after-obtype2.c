/* Now we test, whether the types used in before-obtype2.c still work.
 */
#pragma rtt_checks

void fun1(object "/before-obtype2" ob) {}
void fun2(object "/after-obtype2" ob) {}
void fun3(object "/abc" ob3) {}

int run_test()
{
    return catch(fun1(this_object())) /* Should throw an error */
       && !catch(fun2(this_object())) /* Should be fine        */
       &&  catch(fun3(this_object())) /* Again an error.       */
       &&  1;
}