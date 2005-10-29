/*
 * This is a mudlib file. Copy it to /obj/simul_efun.c, or
 * wherever the get_simul_efun() in master.c says.
 * The functions defined in this file should only be replacements of efuns
 * no longer supported. Don't use these functions any longer, use the
 * replacement instead.
 */

#pragma strict_types
#pragma save_types

/*
 * The ls() function is no longer needed, as get_dir() can do the same
 * work.
 */
void ls(string path) {
    int max, i, len, tmp;
    status trunc_flag;
    string *dir;
#ifndef COMPAT_FLAG
    seteuid(geteuid(previous_object()));
#endif
    dir = get_dir (path);
#if defined(COMPAT_FLAG) && 0
    if (path[0] == '/')
	path = extract(path, 1);
    if (path != "")
	path += "/";
#else
    if (path != "/")
	path += "/";
#endif
    if (!dir) {
        write("No such directory.\n");
        return;
    }
    if (sizeof(dir) > 999)
    {
        dir = dir[0..998];
        trunc_flag = 1;
    }
    max = (int)("/obj/master"->test_dir(dir,path));
    if (max > 79)
        max = 79;
    for (i=0; i < sizeof(dir); i++) {
	string name;
            name = dir[i];
	tmp = strlen(name);
	if (len + tmp > 79) {
	    len = 0;
	    write("\n");
	}
	write(name);
        if (len + max > 79) {
            write("\n");
            len = 0;
        } else {
            write(extract(
"                                                                                ",
                80-max+tmp));
            len += max;
        }
    }
    write("\n");
    if (trunc_flag) write("***TRUNCATED***\n");
}

/*
 * The old 'slice_array' is no longer needed. Use range argument inside
 * a pair of brackets instead.
 */
mixed *slice_array(mixed *arr, int from, int to) {
    return arr[from..to];
}
#if 0
/*
 * filter_objects() has been renamed to filter_array().
 */
mixed *filter_objects(mixed *list, string str, object ob, mixed extra) {
    return filter_array(list, str, ob, extra);
}

#endif
