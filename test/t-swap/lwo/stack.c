#pragma save_types, strong_types, rtt_checks, lightweight

mixed* stack = ({});

int empty()
{
    return sizeof(stack) == 0;
}

void push(mixed val)
{
    stack += ({ val });
}

mixed pop()
{
    mixed result;

    if (!sizeof(stack))
        raise_error("stack is empty.\n");

    result = stack[<1];
    stack = stack[..<2];
    return result;
}

mixed top()
{
    if (!sizeof(stack))
        raise_error("stack is empty.\n");

    return stack[<1];
}
