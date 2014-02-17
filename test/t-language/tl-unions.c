/* Tests for union types (#721).
 *
 * We're only checking things, that should work.
 * Because the strength of type error detection might
 * vary in the future.
 */

#pragma strong_types
#pragma rtt_checks

string          g_str;
int             g_int;
int|string      g_int_str;
<int|string>*   g_int_str_arr;
<int|string>**  g_int_str_arrr;
int*|string*    g_intarr_strarr;
<int*|string*>* g_intarr_strarr_arr;
int|float       g_nr;

string          f_return_str              (string val)          { return val; }
int             f_return_int              (int val)             { return val; }
int|string      f_return_int_str          (int|string val)      { return val; }
<int|string>*   f_return_int_str_arr      (<int|string>* val)   { return val; }
<int|string>**  f_return_int_str_arrr     (<int|string>** val)  { return val; }
int*|string*    f_return_intarr_strarr    (int*|string* val)    { return val; }
<int*|string*>* f_return_intarr_strarr_arr(<int*|string*>* val) { return val; }
int|float       f_return_nr               (int|float val)       { return val; }

int|string|mixed* multiply(int|string|mixed* arg, int factor)   {return arg*factor;}

int run_test()
{
    /* Initialize variables of different types with zero.
     */
    g_str = g_int = g_int_str_arr = 0;

    /* Initialize them with values of their own kind.
     */
    g_str = "Hello!";
    g_int = 42;
    g_int_str = "Nobody here!";
    g_int_str_arr = ({ 1,2,3,"me" });
    g_int_str_arrr = ({ g_int_str_arr, ({}) });
    g_intarr_strarr = ({ 1, 1, 2, 3, 5, 8, 13 });
    g_intarr_strarr_arr = ({ g_intarr_strarr, ({ 3, 1, 4, 1 }), ({ "you" }), ({}) });
    g_nr = -1.5;

    /* Pass the values through a function.
     */
    g_str               = f_return_str(g_str);
    g_int               = f_return_int(g_int);
    g_int_str           = f_return_int_str(g_int_str);
    g_int_str_arr       = f_return_int_str_arr(g_int_str_arr);
    g_int_str_arrr      = f_return_int_str_arrr(g_int_str_arrr);
    g_intarr_strarr     = f_return_intarr_strarr(g_intarr_strarr);
    g_intarr_strarr_arr = f_return_intarr_strarr_arr(g_intarr_strarr_arr);
    g_nr                = f_return_nr(g_nr);

    /* Mix compatible types. */
    g_str = g_int_str; /* We had a string assigned to g_int_str above. */
    g_int_str = g_int;
    g_int_str_arr = g_intarr_strarr;
    g_intarr_strarr = ({int*|string*})g_int_str_arr;
                      /* Would work without a cast, just for testing. */

    /* Try some selected operations */
    g_nr += 10;
    g_int_str = g_str + g_int;

    g_int = multiply(g_int, -1);
    g_str = multiply(g_str, 1);
    g_intarr_strarr = multiply(g_intarr_strarr, 5);

    g_int_str = g_str & "o";
    g_intarr_strarr = g_intarr_strarr & (g_int_str_arr || ({}));

    /* Array indexing. */
    g_int_str = g_intarr_strarr[1];
    g_intarr_strarr = g_int_str_arr[1..5];
    g_int_str = 3;
    g_int_str = g_intarr_strarr[g_int_str];
    
    /* Refcounting. */
    {
        mapping ** vals = ({});
        foreach(mapping * val: vals) {}
    }

    return 1;
}
