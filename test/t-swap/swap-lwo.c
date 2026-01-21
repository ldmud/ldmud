/* Check whether we keep our LWO and can call it afterwards. */

lwobject lwo = new_lwobject("/lwo/stack");

void prepare()
{
    lwo.push(12345);
}

int check()
{
    foreach(int i: 2)
        lwo.push(i);
    return lwo.pop() == 1 && lwo.pop() == 0 && lwo.top() == 12345;
}
