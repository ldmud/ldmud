/* Tests the checks for named object types.
 */

#pragma strong_types, save_types, rtt_checks
inherit "ti-rttc-a";

#include "/sys/driver_info.h"

void rttc_fun_other(object "/abc.c" var) {}
void rttc_fun_me1(object __FILE__ var) {}
void rttc_fun_me2(object "/tl-rttc-ob" var) {}
void rttc_fun_me3(object "///tl-rttc-ob" var) {}
void rttc_fun_me4(mapping|object "tl-rttc-ob" var) {}
void rttc_fun_a(object "/ti-rttc-a" var) {}
void rttc_fun_b(object "/ti-rttc-b" var) {}
void rttc_fun_c(object "/ti-rttc-c" var) {}

int run_test()
{
    if (!catch(rttc_fun_other(this_object());publish) ||
        !catch(this_object()->rttc_fun_other(this_object());publish))
        return 0;

    if (catch(rttc_fun_me1(this_object());publish) ||
        catch(rttc_fun_me2(this_object());publish) ||
        catch(rttc_fun_me3(this_object());publish) ||
        catch(rttc_fun_me4(this_object());publish))
        return 0;

    if (catch(rttc_fun_a(this_object());publish) ||        // Normally inherited
        !catch(rttc_fun_b(this_object());publish) ||       // Privately inherited
        catch(rttc_fun_c(this_object());publish))          // Publicly inhherited
        return 0;

    /* We have 5 distinct named object types mentioned above.
     * The table should be at least half as big.
     * A table entry needs at least 4 bytes.
     */
    if (driver_info(DI_NUM_NAMED_OBJECT_TYPES) < 5 ||
        driver_info(DI_NUM_NAMED_OBJECT_TYPES_TABLE_SLOTS) < 3 ||
        driver_info(DI_SIZE_NAMED_OBJECT_TYPES_TABLE) < 12)
        return 0;

    return 1;
}
