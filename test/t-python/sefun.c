#pragma save_types, rtt_checks, lightweight, clone, save_local_names

nomask string* testsefun(int value)
{
    return ({ to_string(value), program_name(previous_object()) });
}
