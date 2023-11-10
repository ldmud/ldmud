/* This sefun is to provide a replacement for the efun mapping_contains().
 * Feel free to add it to your mudlibs, if you have much code relying on that.
 */

#if ! __EFUN_DEFINED__(mapping_contains)

int mapping_contains(varargs mixed* args)
{
    mapping m;
    string key;

    if (sizeof(args) < 2)
        raise_error(sprintf("Not enough args for mapping_contains: got %d, expected 2.\n", sizeof(args)));

    m = args[<2];
    key = args[<1];

    return m_contains(&(args[..<3])..., m, key);
}

#endif
