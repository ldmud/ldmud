int shadow_action(string str)
{
    return 1;
}

int start(object ob)
{
    if (!shadow(ob))
        return 0;

    add_action("shadow_action", "shadowstrtest");
    return 1;
}

void stop()
{
    unshadow();
}
