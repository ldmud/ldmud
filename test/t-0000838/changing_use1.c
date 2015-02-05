#pragma save_types, rtt_checks

inherit "changing_def";

struct mystruct get_data()
{
    return (<mystruct> 1, 42);
}
