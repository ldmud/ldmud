#include "/sys/regexp.h"

#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

void run_test()
{
    msg("\nRunning tests for #0000432:\n"
          "---------------------------\n");

    run_array(({
        ({ "File encoding 1", 0,
            (:
                object ob = load_object("/iso8859-7.c");
                return ob->"\u03c1\u03c9\u03c4\u03ae\u03c3\u03c4\u03b5\u005f\u03c7\u03b1\u03b9\u03c1\u03b5\u03c4\u03b9\u03c3\u03bc\u03cc"() == "\u039a\u03b1\u03bb\u03ae\u0020\u03bc\u03ad\u03c1\u03b1\u0021";
            :)
        }),
        ({ "File encoding 2", 0,
            (:
                object ob = load_object("/utf-8.c");
                return ob->"\u03c1\u03c9\u03c4\u03ae\u03c3\u03c4\u03b5\u005f\u03c7\u03b1\u03b9\u03c1\u03b5\u03c4\u03b9\u03c3\u03bc\u03cc"() == "\u039a\u03b1\u03bb\u03ae\u0020\u03bc\u03ad\u03c1\u03b1\u0021";
            :)
        }),
    }),
    (:
        if($1)
            shutdown(1);
        else
            start_gc(#'shutdown);
    :));
}

private string get_file_encoding(string filename)
{
    string lines = read_file(filename, 0, 2);
    if (!lines)
        return "UTF-8";

    string* enc = regmatch(lines, "(^|\n)[ \t]*/[/*].*coding[:=][ \t]*([-_.a-zA-Z0-9]+)", RE_MATCH_SUBS);
    if (!enc)
        return "UTF-8";

    return enc[2];
}

string *epilog(int eflag)
{
    set_driver_hook(H_FILE_ENCODING, #'get_file_encoding);

    run_test();
    return 0;
}
