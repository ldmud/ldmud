#pragma save_types, rtt_checks

inherit "changing_def";

int read_data(struct mystruct data)
{
    return data->b == 42 && !data->c;
}

int write_data(struct mystruct data)
{
    data->b = 55;
    if(data->b != 55)
        return 0;

    /* Access to c shall yield an error. */
    return catch(data->c = "Hi!") && 1;
}
