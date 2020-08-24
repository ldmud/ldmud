#pragma save_types, rtt_checks

inherit "changing_def";

int read_data_ct(struct mystruct data)
{
    return data.b == 42 && !data.c;
}

int read_data_rt(struct mystruct data)
{
    return data.("b") == 42 && !data.("c");
}

int write_data_ct(struct mystruct data)
{
    data.b = 55;
    if(data.b != 55)
        return 0;

    /* Access to c shall yield an error. */
    return catch(data.c = "Hi!") && 1;
}

int write_data_rt(struct mystruct data)
{
    data.("b") = 66;
    if(data.("b") != 66)
        return 0;

    /* Access to c shall yield an error. */
    return catch(data.("c") = "Hi!") && 1;
}

int baseof_super(struct mystruct data)
{
    return baseof(data, (<mystruct>));
}

int baseof_sub(struct mystruct data)
{
    return baseof(data, (<mysubstruct>));
}

struct mystruct conv_data_to_super(struct mystruct data)
{
    return to_struct(data, (<mystruct>));
}

struct mysubstruct conv_data_to_sub(struct mystruct data)
{
    return to_struct(data, (<mysubstruct>));
}

int restore(mixed data)
{
    struct mystruct d = restore_value(save_value(data));
    return d.b == data.b;
}
