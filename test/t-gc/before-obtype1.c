/* We create some named object types and then destruct ourselves
 * to test the automatic removal of those types from the type list.
 */
object "/before-obtype1" ob1;
object "/after-obtype1" ob2;
object "/abc" ob3;

void run_test()
{
    destruct(this_object());
}