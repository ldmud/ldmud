int destruct_me()
{
    destruct(this_object());
    return 0;
}

int var = destruct_me();

void create()
{
}
