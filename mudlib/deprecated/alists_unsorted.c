/*
   simul-efun replacement for alists
   by Coogan@Tubmud & Invisible@Beutelland

   IMPORTANT:
   This implementation does NOT sort the keys in alists. In the old
   implementation the keys were ordered by driver-internal criteria
   which did not guarantee a useful order for the user.

   Some Libs rely on the fact that integer keys happened to be in
   ascending order though. If yours does, use alists_sorted.c instead!
 */

#pragma rtt_checks

#ifndef __ALISTS__

/*
   Helper function used in assoc(), order_alist((), and insert_alist()

   check alist for consistency, raise error if not properly formed
 */
private varargs void
assert_alist(mixed * alist, string context = "")
{
    if (!efun::sizeof(alist) || !efun::pointerp(alist[0]))
        efun::raise_error(context + "Not an alist.\n");

    int keynum = efun::sizeof(alist[0]);
    for (int i = efun::sizeof(alist); i-->1;)
    {
        if (!efun::pointerp(alist[i]) || efun::sizeof(alist[i]) != keynum)
            efun::raise_error(context + "Type or size mismatch of the data arrays.\n");
    }
}


/*
   key or data retrieval

   case 1: int   assoc(mixed key, mixed *keys)
   case 2: mixed assoc(mixed key, mixed *alist [, mixed fail])
   case 3: mixed assoc(mixed key, mixed *keys, mixed *data [, mixed fail])
 */
varargs mixed
assoc(mixed key, mixed * keys_or_alist, mixed data_or_fail, mixed fail)
{
    // case 3: mixed assoc(mixed key, mixed *keys, mixed *data [, mixed fail])
    if (efun::pointerp(data_or_fail))
    {
        if (efun::sizeof(keys_or_alist) != efun::sizeof(data_or_fail))
            efun::raise_error("Number of keys and values differ.\n");

        int idx = efun::member(keys_or_alist, key);
        return idx < 0 ? fail : data_or_fail[idx];
    }

    // case 2: mixed assoc(mixed key, mixed *alist [, mixed fail])
    if (efun::sizeof(keys_or_alist) && efun::pointerp(keys_or_alist[0]))
    {
        assert_alist(keys_or_alist, "Bad argument 2 to assoc(): ");

        int idx = efun::member(keys_or_alist[0], key);
        return idx < 0 ? data_or_fail : keys_or_alist[1][idx];
    }

    // case 1: int assoc(mixed key, mixed *keys)
    if (data_or_fail)
        efun::raise_error("Bad number of arguments to assoc().\n");

    return efun::member(keys_or_alist, key);
}

/*
   key or data insertion

   case 1: mixed * insert_alist(mixed key, mixed data..., mixed * alist)
   case 2: int     insert_alist(mixed key, mixed * keys)
 */
mixed
insert_alist(mixed key, varargs mixed * args)
{
    if (!efun::sizeof(args))
        raise_error("Missing argument to insert_alist().\n");

    // case 2: int insert_alist(mixed key, mixed * keys)
    if (efun::sizeof(args) == 1)
    {
        if (!efun::pointerp(args[0])
                || (efun::sizeof(args[0])
                    && !efun::intp(args[0][0])
                    && !efun::stringp(args[0][0])))
            efun::raise_error("Bad argument 2 to insert_alist().\n");

        int idx = efun::member(args[0], key);

        if (idx >= 0) return idx; // existing key

        // add new key, if key array was given by reference
        args[0] += ({ key });
        return efun::sizeof(args[0]) - 1;
    }

    // case 1: mixed * insert_alist(mixed key, mixed data..., mixed * alist)
    assert_alist(args[<1],
            "Bad argument " + efun::sizeof(args) + " to insert_alist(): ");

    mixed * alist = efun::deep_copy(args[<1]);

    int idx = efun::member(alist[0], key);
    if (idx >= 0)
    {
        // existing key
        for (int i = efun::sizeof(alist); i-->1;)
        {
            alist[i][idx] = args[i-1];
        }
    }
    else
    {
        // new key
        alist[0] += ({ key });

        for (int i = efun::sizeof(alist); i-->1;)
        {
            alist[i] += ({ args[i-1] });
        }
    }

    return alist;
}

/*
   for unsorted keys this is a no-op
 */
mixed *
order_alist(mixed * keys, varargs mixed * data)
{
    // whole alist given -> return
    if (!efun::sizeof(data))
    {
        assert_alist(keys, "Bad argument 1 to order_alist(): ");
        return keys;
    }

    mixed * alist = ({ keys }) + data;
    assert_alist(alist, "Bad argument(s) to order_alist(): ");
    return alist;
}

/*
   intersect two lists, preserving the order of the first
 */
mixed *
intersect_alist(mixed * list1, mixed * list2)
{
    return list1 & list2;
}

#endif
