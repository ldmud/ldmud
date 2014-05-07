/* This sefun is meant to replace the deprecated efun cat().
 * Feel free to add it to your mudlibs, if you have much code using cat() or
 * want to use cat().
 *
 * NOTE: the efun:: qualifiers are there in case you have sefuns shadowing
 *       these efuns. They can't be called with set_this_object() in effect.
 *       If you need to call your sefuns, please move the plain calls to
 *       read_file() with the set_this_object() into a separate private
 *       function.
 */
#define CAT_MAX_LINES 50

varargs int cat(string file, int start, int num)
{
    if (extern_call())
        efun::set_this_object(efun::previous_object());

    int more;

    if (num < 0 || !efun::this_player())
        return 0;

    if (!start)
        start = 1;

    if (!num || num > CAT_MAX_LINES) {
        num = CAT_MAX_LINES;
        more = efun::sizeof(efun::read_file(file, start+num, 1));
    }

    string txt = efun::read_file(file, start, num);
    if (!txt)
        return 0;

    efun::tell_object(efun::this_player(), txt);

    if (more)
        efun::tell_object(efun::this_player(), "*****TRUNCATED****\n");

    return efun::sizeof(txt & "\n");
}

