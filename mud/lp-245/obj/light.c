/* Replacement for the old set_light() efun. */

private nosave int own_light;

protected int set_light(int n)
{
    own_light += n;

    object env = (all_environment() || ({ this_object() }))[<1];
    int result;

    foreach(object ob: ({env}) + deep_inventory(env))
        result += ob->query_own_light();

    return result;
}

public int query_own_light() { return own_light; }
