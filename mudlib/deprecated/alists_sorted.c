/*
   simul-efun replacement for alists
   by Coogan@Tubmud & Invisible@Beutelland

   IMPORTANT:
   This implementation uses sorted key arrays. For this reason it only
   supports integer and string keys, and all keys in an alist must be of
   the same type!

   This is intended for Libs that rely on integer keys being in ascending
   order (which was coincidentally the case in the old driver implementation;
   it did not hold true for other types).

   If you don't rely on sorted keys you can use alists_unsorted.c instead
   which offers much better performance for some operations.

   IMPORTANT NOTE on insert_alist():
   Contrary to the original documentation, the actual efun _never_ updated
   the key array in-place. This simul-efun checks if the key or value array
   was passed by reference and does an in-place update in that case.
   If you need the function to behave _strictly_ as originally implemented,
   please remove the respective sections (marked as OPTIONAL) below.
 */

#ifndef __ALISTS__
/*
   Helper function used by insert_alist()

   returns index of key if found, or -index-1 where key should be inserted
 */
private int
alist_lookup_key(mixed key, mixed * keys)
{
    /* simple approach using member()
       this is slightly slower than a binary search for very large alists
       but doesn't really make a huge difference in practice */

    int idx = efun::member(keys, key);
    if (idx >= 0) return idx;

    // no need to search if key is outside existing range
    if (key < keys[0]) return -1;
    if (key > keys[<1]) return -efun::sizeof(keys) - 1;

    return -efun::member(efun::sort_array(keys + ({ key }), #'>), key) - 1;
}

/*
   Helper function used in assoc(), order_alist(), and insert_alist()

   check alist for consistency, raise error if not properly formed
 */
private varargs void
assert_alist(mixed * alist, string context = "")
{
    if (!efun::sizeof(alist) || !efun::pointerp(alist[0]))
        efun::raise_error(context + "Missing key array.\n");

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
    if (!efun::pointerp(keys_or_alist))
        raise_error("Bad argument 2 to assoc().\n");

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

        int idx = alist_lookup_key(key, args[0]);

        if (idx >= 0) return idx; // existing key

        // new key
        idx = -idx - 1;

        // OPTIONAL: update arg if passed by reference
        if (referencep(&(args[0])))
            args[0][idx..idx-1] = ({ key });
        // *******************************************

        return idx;
    }

    // case 1: mixed * insert_alist(mixed key, mixed data..., mixed * alist)
    assert_alist(args[<1],
            "Bad argument " + efun::sizeof(args) + " to insert_alist(): ");

    mixed * alist = efun::deep_copy(args[<1]);

    int idx = alist_lookup_key(key, alist[0]);
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
        idx = -idx - 1;
        alist[0][idx..idx-1] = ({ key });

        for (int i = efun::sizeof(alist); i-->1;)
        {
            alist[i][idx..idx-1] = ({ args[i-1] });
        }
    }

    // OPTIONAL: update arg if passed by reference
    if (referencep(&(args[<1])))
        args[<1] = alist;
    // *******************************************

    return alist;
}

/*
   sort keys and apply same permutation to the associated values
 */
mixed *
order_alist(mixed * keys, varargs mixed * data)
{
    if (!efun::pointerp(keys))
        raise_error("Bad argument 1 to order_alist().\n");

    // whole alist given -> split into components
    if (!efun::sizeof(data))
    {
        return order_alist(keys...);
    }

    mixed * alist = ({ keys }) + data;
    assert_alist(alist, "Bad argument(s) to order_alist(): ");

    return efun::transpose_array(
            efun::sort_array(
                efun::transpose_array(alist),
                (: $1[0] > $2[0] :)
                )
            );
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
