/* This is for a test to check, whether the indices
 * are adjusted after a replace_program().
 */
inherit "inh";    /* Has one function and variable. */
inherit "testob"; /* The final program. */

void create()
{
    replace_program("testob");
}
