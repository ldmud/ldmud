/* This sefun is meant to replace the deprecated efun cat().
 * Feel free to add it to your mudlibs, if you have much code using cat() or
 * want to use cat(). 
 */
#define CAT_MAX_LINES 50

varargs int cat(string file, int start, int num)
{
    if (extern_call())
        set_this_object(previous_object());

    int more;

    if (num < 0 || !this_player())
        return 0;

    if (!start)
        start = 1;

    if (!num || num > CAT_MAX_LINES) {
        num = CAT_MAX_LINES;
        more = strlen(read_file(file, start+num, 1));
    }

    string txt = read_file(file, start, num);
    if (!txt)
        return 0;

    tell_object(this_player(), txt);

    if (more)
        tell_object(this_player(), "*****TRUNCATED****\n");

    return strlen(txt & "\n");
}

