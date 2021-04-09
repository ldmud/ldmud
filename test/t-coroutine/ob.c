#pragma save_types, strong_types, rtt_checks

async void sleeping_fun(closure cb)
{
    await(sleep1(0)); /* Start with the next backend loop. */

    int start = time();
    await(sleep1(__ALARM_TIME__));

    int middle = time();
    await(sleep2(2 * __ALARM_TIME__));

    funcall(cb, middle - start == __ALARM_TIME__ && time() - middle == 2 * __ALARM_TIME__);
}
