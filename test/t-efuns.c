#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/inc/deep_eq.inc"
#include "/sys/tls.h"
#include "/sys/configuration.h"
#include "/sys/functionlist.h"
#include "/sys/lpctypes.h"
#include "/sys/regexp.h"
#include "/sys/struct_info.h"

#define TESTFILE "/log/testfile"

struct test_struct
{
    int* arg;
};

mapping json_testdata = ([ "test 1": 42, "test 2": 42.0,
                          "test 3": "hello world\n",
                          "test 4": ({1,2,3,4,5,42.0,"teststring"}),
                          "test 5": (["a": 26, "b": 27.0, "c": "letter",
                                      "d": (["x":1,"y":2,"z":3]),
                                      "e": ({10,11,12,13}) ])
                          ]);
string json_teststring = "{ \"test 5\": { \"d\": { \"z\": 3, \"y\": 2, \"x\": 1 }, \"e\": [ 10, 11, 12, 13 ], \"b\": 27.000000, \"a\": 26, \"c\": \"letter\" }, \"test 1\": 42, \"test 3\": \"hello world\\n\", \"test 4\": [ 1, 2, 3, 4, 5, 42.000000, \"teststring\" ], \"test 2\": 42.000000 }";

nomask bytes b = b"\x00";

string dhe_testdata =
  "-----BEGIN DH PARAMETERS-----\n"
  "MIIBCAKCAQEAnE/wdy2KvsDtoGcoeth2e1CceYOoiEoLTwTumYD3L2kmavYtCM5l\n"
  "Z9dUHoOZXKOvBtHUh4N5yld1AuEC6tE3a+Hr4TIkSCaRXUJhNh5kyebkxWM6zlJx\n"
  "hGTxDd6WJk1eeWwKa8KFgoEh2WHqNwuBWeSdoAHmw0iVSjbj2lpb/XIVJJQSX8HT\n"
  "mWUPIuRaKQmExS4F25dALeFXXYz0bX72FnPgab/fjBNVBbZksV++Plui7NLzn5q+\n"
  "gSJfIqbdAdQr7v25rrFowz/ClEMRH0IXM10h8shzr3Cx4e552Z2saV9SRPOgrlcD\n"
  "VxyEwepMIUNDCOCPNP2nwwBXav10bGmZ0wIBBQ==\n"
  "-----END DH PARAMETERS-----\n";

int f(int arg);
object clone = clonep() ? 0 : clone_object(this_object());

mixed *tests = ({
    // TODO: Add cases for indexing at string end ("abc"[3])
    ({ "[ 1", 0,        (: ({1,2,3,4,5})[4] == 5 :) }),
    ({ "[ 2", TF_ERROR, (: ({1,2,3,4,5})[5] :) }),
    ({ "[ 3", TF_ERROR, (: ({1,2,3,4,5})[-1] :) }),
    ({ "[ 4", TF_ERROR, (: copy(({}))[0] :) }),
    ({ "[ 5", 0,        (: "Bla, bla, blup."[0] == 'B' :) }),
    ({ "[ 6", TF_ERROR, (: "Bla."[5] :) }),
    ({ "[ 7", TF_ERROR, (: "Bla."[-1] :) }),
    ({ "[< 1", 0,        (: ({1,2,3,4,5})[<4] == 2 :) }),
    ({ "[< 2", TF_ERROR, (: ({1,2,3,4,5})[<6] :) }),
    ({ "[< 3", TF_ERROR, (: ({1,2,3,4,5})[<-1] :) }),
    ({ "[< 4", TF_ERROR, (: ({1,2,3,4,5})[<0] :) }),
    ({ "[< 5", TF_ERROR, (: copy(({}))[<1] :) }),
    ({ "[< 6", 0,        (: "Bla, bla, blup."[<1] == '.' :) }),
    ({ "[< 7", TF_ERROR, (: "Bla."[<-1] :) }),
    // TODO: #'[..], #'[<..], #'[..<], #'[<..<]
    ({ "abs 1", 0,        (: abs(20) == 20 :) }),
    ({ "abs 2", 0,        (: abs(-20) == 20 :) }),
    ({ "abs 3", 0,        (: abs(__INT_MAX__) == __INT_MAX__ :) }),
    ({ "abs 4", 0,        (: abs(-__INT_MAX__) == __INT_MAX__ :) }),
    ({ "abs 5", TF_ERROR, (: funcall(#'abs,"20") :) }),
    ({ "abs 6", TF_ERROR, (: funcall(#'abs, 1, 2) :) }),
    ({ "abs 7", TF_ERROR, (: funcall(#'abs) :) }),
    ({ "abs 8", 0,        (: abs(20.3) == 20.3 :) }),
    ({ "abs 9", 0,        (: abs(-20.3) == 20.3 :) }),
    ({ "abs 10", 0,        (: abs(__FLOAT_MAX__) == __FLOAT_MAX__ :) }),
    ({ "abs 11", 0,        (: abs(-__FLOAT_MAX__) == __FLOAT_MAX__ :) }),
    ({ "abs 12", TF_ERROR, (: funcall(#'abs, 1.0, 2) :) }),
    ({ "acos 1", 0,        (: acos(1.0) == 0 :) }),
    ({ "acos 2", TF_ERROR, (: funcall(#'acos, "1.0") :) }),
    ({ "acos 3", TF_ERROR, (: acos(1.1) :) }),
    ({ "acos 4", TF_ERROR, (: acos(-1.1) :) }),
    ({ "all_environment 1", 0,
	(:
	    object o = clone_object(this_object());
	    mixed res;
            set_environment(o, this_object());
	    res = all_environment(o);
	    destruct(o);
	    return sizeof(res) == 1 && res[0] == this_object();
	:)
    }),
/* Doku - Korrektur: all_environment liefert bei zerstoerten Objekten einen Fehler und keine 0
    ({ "all_environment", 0, 
	(:
	    object o = clone_object(this_object());
            mixed arr = ({o});
            set_environment(o, this_object());
	    destruct(o);
	    return !all_environment(arr[0]);
	:)
    }),
*/
    ({ "all_environment 2", 0, (: all_environment(this_object()) == 0 :) }),
    ({ "all_inventory 1", 0,
	(:
	    mixed res = all_inventory(this_object());
	    return pointerp(res) && !sizeof(res);
	:)
    }),
    ({ "all_inventory 2", 0,
	(:
	    object o = clone_object(this_object());
	    mixed inv, res;

            set_environment(o, this_object());
	    inv = all_inventory(this_object());
	    res = sizeof(inv)==1 && clonep(inv[0]);
	    if(res)
		destruct(inv[0]);
	    return res;
	:)
    }),
    ({ "all_inventory 3", 0, 
	(:
	    object o = clone_object(this_object());
	    set_environment(o, this_object());
	    destruct(o);
	    return sizeof(all_inventory(this_object())) == 0;
	:)
    }),
    ({ "allocate 1", 0,    (: deep_eq(allocate(0, 10), ({})) :) }),
    ({ "allocate 2", 0,    (: deep_eq(allocate(5, 2), ({2, 2, 2, 2, 2})) :) }),
    ({ "allocate 3", 0,    (: deep_eq(allocate(({1,2,3}), 4), ({ ({ ({4, 4, 4}), ({4, 4, 4}) }) })) :) }),
    ({ "allocate 4", 0,
        (:
            mixed* a = allocate(2, ({1}) );
            a[0][0] = -1;

            return deep_eq(a, ({ ({-1}), ({1}) }));
        :)
    }),
    ({ "allocate 5", 0,
        (:
            mixed* a = allocate(({3,2}), ({1}) );
            a[0][0][0] = -1;

            return deep_eq(a, ({ ({ ({ -1 }), ({ 1 }) }), ({ ({ 1 }), ({ 1 }) }), ({ ({ 1 }), ({ 1 }) }) }));
        :)
    }),
#if 0
    /* Out of memory errors can't be caught, therefore these
     * can't be a part of the regular test array. But they
     * should not crash and should not leak any memory.
     */
    ({ "allocate 6a", TF_ERROR, (: allocate(0x1000000, ({ 1 })) :) }),
    ({ "allocate 6b", TF_ERROR, (: allocate( ({0x100, 0x100, 0x100}), ({ 1 })); :) }),
#endif
    ({ "asin 1", 0,        (: asin(0.0) == 0 :) }),
    ({ "asin 2", TF_ERROR, (: funcall(#'asin,"1.0") :) }),
    ({ "asin 3", TF_ERROR, (: asin(1.1) :) }),
    ({ "asin 4", TF_ERROR, (: asin(-1.1) :) }),
    ({ "call_other 1",           0,        (: call_other(clone,                     "f", 10, ({ 20 })) == 11 :) }),
    ({ "call_other 2",           0,        (: call_other(object_name(clone),        "f", 10, ({ 20 })) == 11 :) }),
    ({ "call_other 3",           0,        (: call_other(clone,                     "g", 10, ({ 20 })) == 12 :) }),
    ({ "call_other 4",           0,        (: call_other(object_name(clone),        "g", 10, ({ 20 })) == 12 :) }),
    ({ "call_other 5",           0,        (: call_other(clone,                     "h", 10, ({ 20 })) == 0  :) }),
    ({ "call_other 6",           0,        (: call_other(object_name(clone),        "h", 10, ({ 20 })) == 0  :) }),
    ({ "call_other array 1",     0,        (: deep_eq(call_other(({clone,clone,object_name(clone),this_object(),0}), "f", 10, ({ 20 })), ({ 11, 11, 11, 11, 0})) :) }),
    ({ "call_other array 2",     0,        (: deep_eq(call_other(({clone,clone,object_name(clone),this_object(),0}), "g", 10, ({ 20 })), ({ 12, 12, 12,  0, 0})) :) }),
    ({ "call_other array 3",     0,        (: deep_eq(call_other(({clone,clone,object_name(clone),this_object(),0}), "h", 10, ({ 20 })), ({  0,  0,  0,  0, 0})) :) }),
    ({ "call_other operator 1",  0,        (: clone->f(10, ({ 20 })) == 11 :) }),
    ({ "call_other operator 2",  0,        (: clone->g(10, ({ 20 })) == 12 :) }),
    ({ "call_other operator 3",  0,        (: clone->h(10, ({ 20 })) == 0  :) }),
    ({ "call_other operator 4",  0,        (: string cname = object_name(clone); return cname->f(10, ({ 20 })) == 11; :) }),
    ({ "call_other operator 5",  0,        (: string cname = object_name(clone); return cname->g(10, ({ 20 })) == 12; :) }),
    ({ "call_other operator 6",  0,        (: string cname = object_name(clone); return cname->h(10, ({ 20 })) == 0;  :) }),
    ({ "call_direct 1",          0,        (: call_direct(clone,              "f", 10, ({ 20 })) == 11 :) }),
    ({ "call_direct 2",          0,        (: call_direct(object_name(clone), "f", 10, ({ 20 })) == 11 :) }),
    ({ "call_direct 3",          0,        (: call_direct(clone,              "g", 10, ({ 20 })) == 0  :) }),
    ({ "call_direct 4",          0,        (: call_direct(object_name(clone), "g", 10, ({ 20 })) == 0  :) }),
    ({ "call_direct 5",          0,        (: call_direct(clone,              "h", 10, ({ 20 })) == 0  :) }),
    ({ "call_direct 6",          0,        (: call_direct(object_name(clone), "h", 10, ({ 20 })) == 0  :) }),
    ({ "call_direct in lambda",  0,        (: funcall(lambda(({'a}), ({#'call_direct, clone, "f", 10, '({ 20 })})), 100, 200) == 11 :) }),
    ({ "call_direct array 1",    0,        (: deep_eq(call_direct(({clone,clone,object_name(clone),this_object(),0}), "f", 10, ({ 20 })), ({ 11, 11, 11, 11, 0})) :) }),
    ({ "call_direct array 2",    0,        (: deep_eq(call_direct(({clone,clone,object_name(clone),this_object(),0}), "g", 10, ({ 20 })), ({  0,  0,  0,  0, 0})) :) }),
    ({ "call_direct array 3",    0,        (: deep_eq(call_direct(({clone,clone,object_name(clone),this_object(),0}), "h", 10, ({ 20 })), ({  0,  0,  0,  0, 0})) :) }),
    ({ "call_strict 1",          0,        (: call_strict(clone,                     "f", 10, ({ 20 })) == 11 :) }),
    ({ "call_strict 2",          0,        (: call_strict(object_name(clone),        "f", 10, ({ 20 })) == 11 :) }),
    ({ "call_strict 3",          0,        (: call_strict(clone,                     "g", 10, ({ 20 })) == 12 :) }),
    ({ "call_strict 4",          0,        (: call_strict(object_name(clone),        "g", 10, ({ 20 })) == 12 :) }),
    ({ "call_strict 5",          TF_ERROR, (: call_strict(clone,                     "h", 10, ({ 20 })) :) }),
    ({ "call_strict 6",          TF_ERROR, (: call_strict(object_name(clone),        "h", 10, ({ 20 })) :) }),
    ({ "call_strict array 1a",   0,        (: deep_eq(call_strict(({clone,clone,object_name(clone),this_object()}),   "f", 10, ({ 20 })), ({ 11, 11, 11, 11})) :) }),
    ({ "call_strict array 1b",   TF_ERROR, (:         call_strict(({clone,clone,object_name(clone),this_object(),0}), "f", 10, ({ 20 }))                       :) }),
    ({ "call_strict array 2a",   0,        (: deep_eq(call_strict(({clone,clone,object_name(clone)}),                 "g", 10, ({ 20 })), ({ 12, 12, 12    })) :) }),
    ({ "call_strict array 2b",   TF_ERROR, (:         call_strict(({clone,clone,object_name(clone),this_object()}),   "g", 10, ({ 20 }))                       :) }),
    ({ "call_strict array 3",    TF_ERROR, (:         call_strict(({clone,clone,object_name(clone)}),                 "h", 10, ({ 20 }))                       :) }),
    ({ "call_strict operator 1", 0,        (: clone.f(10, ({ 20 })) == 11 :) }),
    ({ "call_strict operator 2", 0,        (: clone.g(10, ({ 20 })) == 12 :) }),
    ({ "call_strict operator 3", TF_ERROR, (: clone.h(10, ({ 20 })) :) }),
    ({ "call_strict operator 4", 0,        (: string cname = object_name(clone); return cname.f(10, ({ 20 })) == 11; :) }),
    ({ "call_strict operator 5", 0,        (: string cname = object_name(clone); return cname.g(10, ({ 20 })) == 12; :) }),
    ({ "call_strict operator 6", TF_ERROR, (: string cname = object_name(clone); return cname.h(10, ({ 20 })); :) }),
    ({ "call_direct_strict 1",   0,        (: call_direct_strict(clone,              "f", 10, ({ 20 })) == 11 :) }),
    ({ "call_direct_strict 2",   0,        (: call_direct_strict(object_name(clone), "f", 10, ({ 20 })) == 11 :) }),
    ({ "call_direct_strict 3",   TF_ERROR, (: call_direct_strict(clone,              "g", 10, ({ 20 })) :) }),
    ({ "call_direct_strict 4",   TF_ERROR, (: call_direct_strict(object_name(clone), "g", 10, ({ 20 })) :) }),
    ({ "call_direct_strict 5",   TF_ERROR, (: call_direct_strict(clone,              "h", 10, ({ 20 })) :) }),
    ({ "call_direct_strict 6",   TF_ERROR, (: call_direct_strict(object_name(clone), "h", 10, ({ 20 })) :) }),
    ({ "call_direct_strict array 1a", 0,        (: deep_eq(call_direct_strict(({clone,clone,object_name(clone),this_object()}),   "f", 10, ({ 20 })), ({ 11, 11, 11, 11})) :) }),
    ({ "call_direct_strict array 1b", TF_ERROR, (:         call_direct_strict(({clone,clone,object_name(clone),this_object(),0}), "f", 10, ({ 20 }))                       :) }),
    ({ "call_direct_strict array 2",  TF_ERROR, (:         call_direct_strict(({clone,clone,object_name(clone)}),                 "g", 10, ({ 20 }))                       :) }),
    ({ "call_direct_strict array 3",  TF_ERROR, (:         call_direct_strict(({clone,clone,object_name(clone)}),                 "h", 10, ({ 20 }))                       :) }),
    ({ "call_resolved 1",        0,        (: int result; return  call_resolved(&result, clone,                     "f", 10, ({ 20 })) && result == 11; :) }),
    ({ "call_resolved 2",        0,        (: int result; return  call_resolved(&result, object_name(clone),        "f", 10, ({ 20 })) && result == 11; :) }),
    ({ "call_resolved 3",        0,        (: int result; return  call_resolved(&result, clone,                     "g", 10, ({ 20 })) && result == 12; :) }),
    ({ "call_resolved 4",        0,        (: int result; return  call_resolved(&result, object_name(clone),        "g", 10, ({ 20 })) && result == 12; :) }),
    ({ "call_resolved 5",        0,        (: int result; return !call_resolved(&result, clone,                     "h", 10, ({ 20 })); :) }),
    ({ "call_resolved 6",        0,        (: int result; return !call_resolved(&result, object_name(clone),        "h", 10, ({ 20 })); :) }),
    ({ "call_resolved array 1",  0,        (: int* result; return deep_eq(call_resolved(&result, ({clone,clone,object_name(clone),this_object(),0}), "f", 10, ({ 20 })), ({ 1, 1, 1, 1, 0})) && deep_eq(result, ({ 11, 11, 11, 11, 0})); :) }),
    ({ "call_resolved array 2",  0,        (: int* result; return deep_eq(call_resolved(&result, ({clone,clone,object_name(clone),this_object(),0}), "g", 10, ({ 20 })), ({-1,-1,-1, 0, 0})) && deep_eq(result, ({ 12, 12, 12,  0, 0})); :) }),
    ({ "call_resolved array 3",  0,        (: int* result; return deep_eq(call_resolved(&result, ({clone,clone,object_name(clone),this_object(),0}), "h", 10, ({ 20 })), ({ 0, 0, 0, 0, 0})) && deep_eq(result, ({  0,  0,  0,  0, 0})); :) }),
    ({ "call_direct_resolved 1", 0,        (: int result; return  call_direct_resolved(&result, clone,              "f", 10, ({ 20 })) && result == 11; :) }),
    ({ "call_direct_resolved 2", 0,        (: int result; return  call_direct_resolved(&result, object_name(clone), "f", 10, ({ 20 })) && result == 11; :) }),
    ({ "call_direct_resolved 3", 0,        (: int result; return !call_direct_resolved(&result, clone,              "g", 10, ({ 20 })); :) }),
    ({ "call_direct_resolved 4", 0,        (: int result; return !call_direct_resolved(&result, object_name(clone), "g", 10, ({ 20 })); :) }),
    ({ "call_direct_resolved 5", 0,        (: int result; return !call_direct_resolved(&result, clone,              "h", 10, ({ 20 })); :) }),
    ({ "call_direct_resolved 6", 0,        (: int result; return !call_direct_resolved(&result, object_name(clone), "h", 10, ({ 20 })); :) }),
    ({ "call_direct_resolved array 1",  0, (: int* result; return deep_eq(call_direct_resolved(&result, ({clone,clone,object_name(clone),this_object(),0}), "f", 10, ({ 20 })), ({1, 1, 1, 1, 0})) && deep_eq(result, ({ 11, 11, 11, 11, 0})); :) }),
    ({ "call_direct_resolved array 2",  0, (: int* result; return deep_eq(call_direct_resolved(&result, ({clone,clone,object_name(clone),this_object(),0}), "g", 10, ({ 20 })), ({0, 0, 0, 0, 0})) && deep_eq(result, ({  0,  0,  0,  0, 0})); :) }),
    ({ "call_direct_resolved array 3",  0, (: int* result; return deep_eq(call_direct_resolved(&result, ({clone,clone,object_name(clone),this_object(),0}), "h", 10, ({ 20 })), ({0, 0, 0, 0, 0})) && deep_eq(result, ({  0,  0,  0,  0, 0})); :) }),
    ({ "crypt", TF_ERROR,  (: crypt("ABC", "$$") :) }),
    ({ "ctime", TF_DONTCHECKERROR,  (: ctime(-1) :) }), /* This must be the first ctime call of this test suite. */
    ({ "get_type_info(int,0)", 0, (: get_type_info(10,              0) == T_NUMBER       :) }),
    ({ "get_type_info(str,0)", 0, (: get_type_info("10",            0) == T_STRING       :) }),
    ({ "get_type_info(str,1)", 0, (: get_type_info("10",            1) == 0              :) }), /* Shared string */
    ({ "get_type_info(vec,0)", 0, (: get_type_info(({10}),          0) == T_POINTER      :) }),
    ({ "get_type_info(obj,0)", 0, (: get_type_info(this_object(),   0) == T_OBJECT       :) }),
    ({ "get_type_info(map,0)", 0, (: get_type_info(([10:2;3]),      0) == T_MAPPING      :) }),
    ({ "get_type_info(map,1)", 0, (: get_type_info(([10:2;3]),      1) == 2              :) }), /* Mapping width */
    ({ "get_type_info(fpn,0)", 0, (: get_type_info(10.0,            0) == T_FLOAT        :) }),
    ({ "get_type_info(cls,0)", 0, (: get_type_info(#'f,             0) == T_CLOSURE      :) }),
    ({ "get_type_info(cls,1)", 0, (: get_type_info(#'f,             1) == CLOSURE_LFUN   :) }), /* Closure type */
    ({ "get_type_info(cls,2)", 0, (: get_type_info(#'f,             2) == this_object()  :) }), /* Lfun object */
    ({ "get_type_info(cls,3)", 0, (: get_type_info(#'f,             3) == load_name()    :) }), /* Lfun program */
    ({ "get_type_info(cls,4)", 0, (: get_type_info(#'f,             4) == "f"            :) }), /* Lfun name */
    ({ "get_type_info(sym,0)", 0, (: get_type_info('''f,            0) == T_SYMBOL       :) }),
    ({ "get_type_info(sym,1)", 0, (: get_type_info('''f,            1) == 3              :) }), /* Number of quotes */
    ({ "get_type_info(qar,0)", 0, (: get_type_info('''({10}),       0) == T_QUOTED_ARRAY :) }),
    ({ "get_type_info(qar,1)", 0, (: get_type_info('''({10}),       1) == 3              :) }), /* Number of quotes */
    ({ "get_type_info(str,0)", 0, (: get_type_info((<test_struct>), 0) == T_STRUCT       :) }),
    ({ "get_type_info(str,1)", 0, (: get_type_info((<test_struct>), 1) == "test_struct"  :) }), /* Struct name */
    ({ "get_type_info(byt,0)", 0, (: get_type_info(b"10",           0) == T_BYTES        :) }),
    ({ "save_object 1", 0, (: stringp(save_object()) :) }), /* Bug #594 */
    ({ "strstr 01", 0, (: strstr("","") == 0 :) }), /* Bug #536 */
    ({ "strstr 02", 0, (: strstr("","", 1) == -1 :) }),
    ({ "strstr 03", 0, (: strstr("abc","") == 0 :) }),
    ({ "strstr 04", 0, (: strstr("abc","", 3) == 3 :) }),
    ({ "strstr 05", 0, (: strstr("abc","", 4) == -1 :) }),
    ({ "strstr 06", 0, (: strstr("abcdefa","a") == 0 :) }),
    ({ "strstr 07", 0, (: strstr("abcdefa","a", 1) == 6 :) }),
    ({ "strstr 08", 0, (: strstr("abcdefa","a", 6) == 6 :) }),
    ({ "strstr 09", 0, (: strstr("abcdefa","a", 7) == -1 :) }),
    ({ "strstr 10", 0, (: strstr("abcdefabc","a", 7) == -1 :) }),
    ({ "strstr 11", 0, (: strstr("abcdefabc","c") == 2 :) }),
    ({ "text_width 01", 0, (: text_width("LDMud")                            == 5 :) }),
    ({ "text_width 02", 0, (: text_width("Longest\nLine")                    == 7 :) }),
    ({ "text_width 03", 0, (: text_width("Longest\nLine\n")                  == 7 :) }),
    ({ "text_width 04", 0, (: text_width("\tB")                              == 9 :) }),
    ({ "text_width 05", 0, (: text_width("A\tB")                             == 9 :) }),
    ({ "text_width 06", 0, (: text_width("ABCDEFG\tB")                       == 9 :) }),
    ({ "text_width 07", 0, (: text_width("\e[1mHi\e[0m")                     == 2 :) }),
    ({ "text_width 08", 0, (: text_width("A\u0308")                          == 1 :) }),
    ({ "text_width 09", 0, (: text_width("\U0001f600\ufe0f")                 == 2 :) }),
    ({ "text_width 10", 0, (: text_width("\U0001f3f3\ufe0f\u200d\U0001f308") == 2 :) }),
    ({ "text_width 11", 0, (: text_width("\U0001F1E9\U0001F1EA")             == 2 :) }),
    ({ "text_width 12", 0, (: text_width("\u1100\u1161\u11ab")               == 2 :) }),
    ({ "this_object", 0, (: this_object(({})...) == this_object() :) }),
    ({ "this_player", 0, (: this_player(({})...) == 0 :) }),
    ({ "hash string (MD5)", 0, (:
                         hash(TLS_HASH_MD5, "line 13: Warning: Missing "
                              "'return <value>' statement") ==
                         "ccbece3d9a751067260ed4d8537e5b49" :)
    }),
    ({ "hash string (SHA1)", 0, (:
                         hash(TLS_HASH_SHA1, "line 13: Warning: Missing "
                              "'return <value>' statement") ==
                         "6c17f98a279bb0f586862f7de4dd9b39c6517e42" :)
    }),
#if defined(__GCRYPT__) || defined(__OPENSSL__)
    ({ "hash string (SHA224)", 0, (:
                         hash(TLS_HASH_SHA224, "line 13: Warning: Missing "
                              "'return <value>' statement") ==
                         "063cb8892f917de2bda98b96e3fc8af3f31fe0091c79bd920fade895" :)
    }),
    ({ "hash string (SHA256)", 0, (:
                         hash(TLS_HASH_SHA256, "line 13: Warning: Missing "
                              "'return <value>' statement") ==
                         "b18a6bffef432bba5d97f3910f209446923a4bf71331f8e1a592e72b85a1a8be" :)
    }),
    ({ "hash string (SHA384)", 0, (:
                         hash(TLS_HASH_SHA384, "line 13: Warning: Missing "
                              "'return <value>' statement") ==
                         "ba3f63b41e9321757704fbe84904b08b3ddf91a82c694a96df38317b938431f51ef073dc71fe8d8c3a09c028cb2fd82c" :)
    }),
    ({ "hash string (SHA512)", 0, (:
                         hash(TLS_HASH_SHA512, "line 13: Warning: Missing "
                              "'return <value>' statement") ==
                         "902588f647e39421f274f94303e7d397bda55cd12b562d36e51ffb37f59d17e63edc77443dfb3a0e92996215b82ae067b1a700f3356baeb513cd619ce05b9a39" :)
    }),
    ({ "hash string (RIPEMD160)", 0, (:
                         hash(TLS_HASH_RIPEMD160, "line 13: Warning: Missing "
                              "'return <value>' statement") ==
                         "fe9de95923c1200b31db7905d997a81e121c7640" :)
    }),
#endif
    ({ "write_file 1", 0, 
        (:
            int res;
            rm(TESTFILE);
            write_file(TESTFILE, "abc");
            res = read_file(TESTFILE)=="abc";
            rm(TESTFILE);
            return res;
        :)
    }),
    ({ "write_file 2", 0,
        (:
            int res;
            rm(TESTFILE);
            write_file(TESTFILE, "abc");
            write_file(TESTFILE, "abc");
            res = read_file(TESTFILE)=="abcabc";
            rm(TESTFILE);
            return res;
        :)
    }),
    ({ "write_file 3", 0,
        (:
            int res;
            write_file(TESTFILE, "abc");
            write_file(TESTFILE, "abc", 1);
            res = read_file(TESTFILE)=="abc";
            rm(TESTFILE);
            return res;
        :)
    }),
    ({ "write_file 4", 0, /* Bug #512 */
        (:
            int res;
            rm(TESTFILE);
            write_file(TESTFILE, "abc", 1);
            res = read_file(TESTFILE)=="abc";
            rm(TESTFILE);
            return res;
        :)
    }),
    ({ "filter array", TF_ERROR, (: filter(({1,2,3}), unbound_lambda(0,0), ({4,5,6})) :) }),
    ({ "filter mapping", TF_ERROR, (: filter(([1,2,3]), unbound_lambda(0,0), ([4,5,6])) :) }),
    ({ "map string 1", 0, (: map("abc", (['a':'x'])) == "xbc" :) }),
    ({ "map string 2", 0, (: map("abc", (['a':'x';'y']), 1) == "ybc" :) }),
    ({ "map string 3", TF_ERROR, (: map("abc", (['a']), 0) == "abc" :) }),
    ({ "map string 4", 0, (: map("abc", #'+, 1) == "bcd" :) }),
    ({ "map string 5", 0, (: map("abc", "f") == "bcd" :) }),
    ({ "map string 6", TF_ERROR, (: map("abc", unbound_lambda(0,0), ({1,2,3})) :) }),
    ({ "map string x", TF_ERROR, (: map("abc", (['a':'x']), 2) :) }),
    ({ "map array 1", 0, (: deep_eq(map(({1,2,3}), ([1:5])), ({5,2,3})) :) }),
    ({ "map array 2", 0, (: deep_eq(map(({1,2,3}), ([1:5;6]), 1),({6,2,3})) :) }),
    ({ "map array 3", TF_ERROR, (: map(({1,2,3}), ([1]), 0) :) }),
    ({ "map array 4", 0, (: deep_eq(map(({1,2,3}), #'+, 1), ({2,3,4})) :) }),
    ({ "map array 5", 0, (: deep_eq(map(({1,2,3}), "f"), ({2,3,4})) :) }),
    ({ "map array 6", TF_ERROR, (: map(({0}), unbound_lambda(0,0), ({1,2,3})) :) }),
    ({ "map mapping 1", TF_ERROR, (: deep_eq(map(([1,2,3]), ([1:5])), ({5,2,3})) :) }),
    ({ "map mapping 2", TF_ERROR, (: deep_eq(map(([1,2,3]), ([1:5;6]), 1),({6,2,3})) :) }),
    ({ "map mapping 3", TF_ERROR, (: map(([1,2,3]), ([1]), 0) :) }),
    ({ "map mapping 4", 0, (: deep_eq(map(([1,2,3]), (: $1 + $3 :), 1), ([1:2,2:3,3:4])) :) }),
    ({ "map mapping 5", 0, (: deep_eq(map(([1,2,3]), "f"), ([1:2,2:3,3:4])) :) }),
    ({ "map mapping 6", TF_ERROR, (: map(([]), unbound_lambda(0,0), ([1,2,3])) :) }),

    ({ "load_object 1", 0, (: load_object(__FILE__) == this_object() :) }),
    ({ "load_object 2", 0, (: load_object("/" __FILE__) == this_object() :) }),
    ({ "load_object 3", 0, (: load_object("./" __FILE__) == this_object() :) }),
    ({ "load_object 4", 0, (: load_object("/./" __FILE__) == this_object() :) }),

    ({ "regmatch 1", 0, (: regmatch("abcd", "abc") == "abc" :) }),
    ({ "regmatch 2", 0, (: regmatch("abcd", "abcdef") == 0 :) }),
    ({ "regmatch 3", 0, (: regmatch("abcd", "^$") == 0 :) }),
    ({ "regmatch 4", 0, (: regmatch("", "^$") == "" :) }),
    ({ "regmatch 5", 0, (: regmatch("", "(a|)") == "" :) }),
    ({ "regmatch 6", 0, (: deep_eq(regmatch("abcd", "abc", RE_MATCH_SUBS), ({"abc", 3})) :) }),
    ({ "regmatch 7", 0, (: regmatch("abcd", "abcdef", RE_MATCH_SUBS) == 0 :) }),
    ({ "regmatch 8", 0, (: regmatch("abcd", "^$", RE_MATCH_SUBS) == 0 :) }),
    ({ "regmatch 9", 0, (: deep_eq(regmatch("", "^$", RE_MATCH_SUBS), ({"", 1})) :) }),
    ({ "regmatch 10", 0, (: deep_eq(regmatch("", "(a|)", RE_MATCH_SUBS), ({ "", "", 1})) :) }),
    ({ "regmatch 11", 0, (: regmatch("--A--B--", "A.*.B") == "A--B" :) }),

    ({ "sscanf 1", 0, (: sscanf("A10", "A%~d") == 1 :) }),
    ({ "sscanf 2", 0, (: sscanf("B10", "A%~d") == 0 :) }),
    ({ "sscanf 3", 0, (: sscanf("A10", "A%!d") == 0 :) }),
    ({ "sscanf 4", 0, (: int x; return sscanf("A10", "A%d", x) == 1 && x == 10; :) }),
    ({ "sscanf 5", 0, (: int x; return sscanf("B10", "A%d", x) == 0 && x == 0; :) }),
    ({ "sscanf 6", 0, (: int x; return sscanf("A-10", "A%d", x) == 1 && x == -10; :) }),
    ({ "sscanf 7", 0, (: int x; return sscanf(to_string(__INT_MAX__), "%d", x) == 1 && x == __INT_MAX__; :) }),
    ({ "sscanf 8", 0, (: int x; return sscanf(to_string(__INT_MIN__), "%d", x) == 1 && x == __INT_MIN__; :) }),
    ({ "sscanf overflow 1", 0, (: int x; return sscanf(to_string(__INT_MIN__)-"-", "%d", x) == 0; :) }),
    ({ "sscanf overflow 2", 0, (: int x;
                                  string v = to_string(__INT_MIN__);
                                  for (int i = sizeof(v)-1; i>=0; i--) /* Increment the value. */
                                  {
                                      if (!i)
                                      {
                                          v[0..0] = "-1";
                                          break;
                                      }
                                      if (v[i] != '9')
                                      {
                                         v[i]++;
                                         break;
                                      }
                                      v[i] = '0';
                                  }
                                  return sscanf(v, "%d", x) == 0;
                               :) }),
    ({ "sscanf percent 1", 0, (: string x, y; return sscanf("te%st",     "%s%%%s",   x, y) == 2 && x == "te" && y == "st"; :) }),
    ({ "sscanf percent 2", 0, (: string x, y; return sscanf("te%%st",    "%s%%%%%s", x, y) == 2 && x == "te" && y == "st"; :) }),
    ({ "sscanf percent 3", 0, (: string x, y; return sscanf("%%s",       "%s%%%s",   x, y) == 2 && x == ""   && y == "%s"; :) }),
    ({ "sscanf percent 4", 0, (: string x;    return sscanf("s%%%%%t%%", "%s%%t",    x) == 1    && x == "s%%%%";           :) }),
    ({ "sscanf percent 5", 0, (: string x;    return sscanf("s%",        "%s%%",     x) == 1    && x == "s";               :) }),
    ({ "sscanf percent 6", 0, (: string x;    return sscanf("s%%",       "%s%%%%",   x) == 1    && x == "s";               :) }),
    ({ "sscanf percent 7", 0, (: string x;    return sscanf("s%%",       "%s%%%%%%", x) == 0;                              :) }),
    ({ "sscanf percent 8", 0, (: string x;    return sscanf("%%s",       "%%%s",     x) == 1    && x == "%s";              :) }),
    ({ "sscanf percent 9", 0, (: string x;    return sscanf("%s",        "%%%s",     x) == 1    && x == "s";               :) }),

    ({ "sprintf 1", 0, (: sprintf("%=-4s\n", "A B C\n") == "A B\nC\n" :) }),
    ({ "sprintf 2", 0, (: sprintf("%=-4s\n%s", "A B C\n", "X\n") == "A B\nC\nX\n" :) }),
    ({ "sprintf 3", 0, (: sprintf("%=-4s %=-4s\n%s", "A B C\n", "1 2 3\n", "X\n") == "A B  1 2\nC    3\nX\n" :) }),
    ({ "sprintf 4", 0, (: sprintf("abc%cdef", 0) == "abc\x00def" :) }),
    ({ "sprintf 5", 0, (: sprintf("abc\x00def") == "abc\x00def" :) }),
    ({ "sprintf 6", 0, (: sprintf("%s", "abc\x00def") == "abc\x00def" :) }),
    ({ "sprintf 7", 0, (: sprintf("%-6s", "abc\x00def") == "abc\x00def" :) }),
    ({ "sprintf 8", 0, (: sprintf("%-=6s", "abc\x00def") == "abc\x00def" :) }),
    ({ "sprintf 9", 0, (: sprintf("%-#6.1s", "abc\x00def") == "abc\x00def" :) }),
    ({ "sprintf 10", 0, (: sprintf("%-'\x00'6s", "abc") == "abc\x00\x00\x00" :) }),

    ({ "sprintf doc01", 0, (: sprintf("foo")                     == "foo"           :) }),
    ({ "sprintf doc02", 0, (: sprintf("%s","foo")                == "foo"           :) }),
    ({ "sprintf doc03", 0, (: sprintf("%7s","foo")               == "    foo"       :) }),
    ({ "sprintf doc04", 0, (: sprintf("%-7s","foo")              == "foo    "       :) }),
    ({ "sprintf doc05", 0, (: sprintf("%|7s","foo")              == "  foo  "       :) }),
    ({ "sprintf doc06", 0, (: sprintf("%7'.'s","foo")            == "....foo"       :) }),
    ({ "sprintf doc07", 0, (: sprintf("%-7'+-'s","foo")          == "foo+-+-"       :) }),
    ({ "sprintf doc08", 0, (: sprintf("%|9'-+'s","foo")          == "-+-foo-+-"     :) }),
    ({ "sprintf doc09", 0, (: sprintf("%3s","foobarbloh")        == "foobarbloh"    :) }),
    ({ "sprintf doc10", 0, (: sprintf("%3.6s","foobarbloh")      == "foobar"        :) }),
    ({ "sprintf doc11", 0, (: sprintf("%6.3s","foobarbloh")      == "   foo"        :) }),
    ({ "sprintf doc12", 0, (: sprintf("%:6s","foobarbloh")       == "foobar"        :) }),
    ({ "sprintf doc13", 0, (: sprintf("%:3s","foobarbloh")       == "foo"           :) }),
    ({ "sprintf doc14", 0, (: sprintf("%*.*s",-7,2,"foobarbloh") == "fo     "       :) }),
    ({ "sprintf doc15", 0, (: sprintf("%=12s","this is a very long sentence\n")
                              == "   this is a\n   very long\n    sentence\n"       :) }),
    ({ "sprintf doc16", 0, (: sprintf("%=-12s","this is a very long sentence\n")
                              == "this is a\nvery long\nsentence\n"                 :) }),
    ({ "sprintf doc17", 0, (: sprintf("%=|12s","this is a very long sentence\n")
                              == "  this is a\n  very long\n  sentence\n"           :) }),
    ({ "sprintf doc18", 0, (: sprintf("%=10.6s","this is a very long sentence\n")
                              == "      this\n      is a\n      very\n"
                                 "      long\n    senten\n        ce\n"             :) }),
    ({ "sprintf doc19", 0, (: sprintf("%#-40.3s\n", "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\n")
                              == "one          five         nine\n"
                                 "two          six          ten\n"
                                 "three        seven        \n"
                                 "four         eight        \n"                     :) }),
    ({ "sprintf doc20", 0, (: sprintf("%#-40s\n", "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\nnine\nten\n")
                              == "one     three   five    seven   nine\n"
                                 "two     four    six     eight   ten\n"            :) }),
    ({ "sprintf doc21", 0, (: sprintf("%@-5s",({"foo","bar","bloh"}))
                              == "foo  bar  bloh "                                  :) }),
    ({ "sprintf doc22", 0, (: sprintf("%d",123)                  == "123"           :) }),
    ({ "sprintf doc23", 0, (: sprintf("%7d",123)                 == "    123"       :) }),
    ({ "sprintf doc24", 0, (: sprintf("%-7d",123)                == "123    "       :) }),
    ({ "sprintf doc25", 0, (: sprintf("%d/%d",123,-123)          == "123/-123"      :) }),
    ({ "sprintf doc26", 0, (: sprintf("% d/% d",123,-123)        == " 123/-123"     :) }),
    ({ "sprintf doc27", 0, (: sprintf("%+d/%+d",123,-123)        == "+123/-123"     :) }),
    ({ "sprintf doc28", 0, (: sprintf("%+5d/%5d",123,123)        == " +123/  123"   :) }),
    ({ "sprintf doc29", 0, (: sprintf("%|6d",123)                == "  123 "        :) }),
    ({ "sprintf doc30", 0, (: sprintf("%|10d",123)               == "    123   "    :) }),
    ({ "sprintf doc31", 0, (: sprintf("%|10d%3s",123,"foo")      == "    123   foo" :) }),
    ({ "sprintf doc32", 0, (: sprintf("%o",16)                   == "20"            :) }),
    ({ "sprintf doc33", 0, (: sprintf("%'0'3o",8)                == "010"           :) }),
    ({ "sprintf doc34", 0, (: sprintf("%x",123)                  == "7b"            :) }),
    ({ "sprintf doc35", 0, (: sprintf("%X",123)                  == "7B"            :) }),
    ({ "sprintf doc36", 0, (: sprintf("%f",123.5)                == "124"           :) }),
    ({ "sprintf doc37", 0, (: sprintf("%8.3f",123.5)             == " 123.500"      :) }),
    ({ "sprintf doc38", 0, (: sprintf("%E",123.5)                == "1E+02"         :) }),
    ({ "sprintf doc39", 0, (: sprintf("%12.4e",123.5)            == "  1.2350e+02"  :) }),
    ({ "sprintf doc40", 0, (: sprintf("%g",123.5)                == "1e+02"         :) }),
    ({ "sprintf doc41", 0, (: sprintf("%8.3G",123.5)             == "     124"      :) }),
    ({ "sprintf doc42", 0, (: sprintf("%8.6g",123.5)             == "   123.5"      :) }),

    ({ "to_text 1", 0, (: deep_eq(to_array(to_text( ({}) )), ({}) ) :) }),
    ({ "to_text 2", 0, (: deep_eq(to_array(to_text( ({0, 65, 66, 67}) )), ({0, 65, 66, 67}) ) :) }),
    ({ "to_text 3", 0, (: deep_eq(to_array(to_text( copy(({65, 66, 67, "ABC"})) )), ({65, 66, 67}) ) :) }),
    ({ "to_text 4", 0, (: deep_eq(to_array(to_text( to_text(({65, 66, 67})) )), ({65, 66, 67}) ) :) }),
    ({ "to_text 5", 0, (: deep_eq(to_array(to_text( "ABC" )), ({65, 66, 67}) ) :) }),
    ({ "to_text 6", 0, (: deep_eq(to_array(to_text( ({ 195, 132, 195, 150, 195, 156 }), "utf-8" )), ({196, 214, 220}) ) :) }),
    ({ "to_text 7", 0, (: deep_eq(to_array(to_text( ({ 226, 152, 186, 226, 152, 185 }), "utf-8" )), ({9786, 9785}) ) :) }),
    ({ "to_text 8", 0, (: deep_eq(to_array(to_text( ({ 240, 159, 130, 178, 240, 159, 130, 179, 240, 159, 130, 180, 240, 159, 130, 181, 240, 159, 130, 182, 240, 159, 130, 183, 240, 159, 130, 184, 240, 159, 130, 185, 240, 159, 130, 186, 240, 159, 130, 187, 240, 159, 130, 188, 240, 159, 130, 189, 240, 159, 130, 190, 240, 159, 130, 177}), "utf-8")),
                                   ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153}) )
                        :) }),
    ({ "to_text 9", 0, (: deep_eq(to_array(to_text(
                                   ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153}) )),
                                   ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153}) )
                        :) }),
    ({ "to_text 10", 0, (: deep_eq(to_array(to_text(({65, 192, 9786, 127154}))), ({65, 192, 9786, 127154}))
                        :) }),

    ({ "to_bytes 1", 0, (: deep_eq(to_array(to_bytes( ({}) )), ({}) ) :) }),
    ({ "to_bytes 2", 0, (: deep_eq(to_array(to_bytes( ({0, 130, 150, 200, 255}) )), ({0, 130, 150, 200, 255}) ) :) }),
    ({ "to_bytes 3", 0, (: deep_eq(to_array(to_bytes( copy(({1, 2, 3, "ABC"})) )), ({1, 2, 3}) ) :) }),
    ({ "to_bytes 4", 0, (: deep_eq(to_array(to_bytes( to_bytes(({1,2,3})) )), ({1, 2, 3}) ) :) }),
    ({ "to_bytes 5", 0, (: deep_eq(to_array(to_bytes( "ABC", "ascii" )), ({65, 66, 67}) ) :) }),
    ({ "to_bytes 6", 0, (: deep_eq(to_array(to_bytes( ({196, 214, 220}), "utf-8" )), ({ 195, 132, 195, 150, 195, 156 }) ) :) }),
    ({ "to_bytes 7", 0, (: deep_eq(to_array(to_bytes( ({9786, 9785}), "utf-8" )), ({ 226, 152, 186, 226, 152, 185 }) ) :) }),
    ({ "to_bytes 8", 0, (: deep_eq(to_array(to_bytes( ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153}), "utf-8")),
                                   ({ 240, 159, 130, 178, 240, 159, 130, 179, 240, 159, 130, 180, 240, 159, 130, 181, 240, 159, 130, 182, 240, 159, 130, 183, 240, 159, 130, 184, 240, 159, 130, 185, 240, 159, 130, 186, 240, 159, 130, 187, 240, 159, 130, 188, 240, 159, 130, 189, 240, 159, 130, 190, 240, 159, 130, 177}) )
                        :) }),
    ({ "to_bytes 9", 0, (: deep_eq(to_array(to_bytes(({65, 196, 9786, 127154}), "utf-8")),
                                   ({65, 195, 132, 226, 152, 186, 240, 159, 130, 178}))
                        :) }),

    ({ "to_text/to_bytes 1", 0, (: deep_eq(to_array(to_text(to_bytes(({65, 196, 9786, 127154}), "utf-8"),    "utf-8")),    ({65, 196, 9786, 127154})) :) }),
    ({ "to_text/to_bytes 2", 0, (: deep_eq(to_array(to_text(to_bytes(({65, 196, 9786, 127154}), "utf-16le"), "utf-16le")), ({65, 196, 9786, 127154})) :) }),
    ({ "to_text/to_bytes 3", 0, (: deep_eq(to_array(to_text(to_bytes(({65, 196, 9786, 127154}), "utf-32le"), "utf-32le")), ({65, 196, 9786, 127154})) :) }),
    ({ "to_text/to_bytes 4", 0, (: deep_eq(to_array(to_text(to_bytes( ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153}), "utf-8"), "utf-8")),
                                                                      ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153})) :) }),
    ({ "to_text/to_bytes 5", 0, (: deep_eq(to_array(to_text(to_bytes( ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153}), "utf-16be"), "utf-16be")),
                                                                      ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153})) :) }),
    ({ "to_text/to_bytes 6", 0, (: deep_eq(to_array(to_text(to_bytes( ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153}), "utf-32be"), "utf-32be")),
                                                                      ({127154, 127155, 127156, 127157, 127158, 127159, 127160, 127161, 127162, 127163, 127164, 127165, 127166, 127153})) :) }),

    ({ "to_bytes/to_text 1", 0, (: deep_eq(to_array(to_bytes(to_text(({65, 195, 132, 226, 152, 186, 240, 159, 130, 178}), "utf-8"),    "utf-8")),    ({65, 195, 132, 226, 152, 186, 240, 159, 130, 178})) :) }),
    ({ "to_bytes/to_text 2", 0, (: deep_eq(to_array(to_bytes(to_text(({65, 0, 196, 0, 58, 38, 60, 216, 178, 220}), "utf-16le"), "utf-16le")), ({65, 0, 196, 0, 58, 38, 60, 216, 178, 220})) :) }),
    ({ "to_bytes/to_text 3", 0, (: deep_eq(to_array(to_bytes(to_text(({65, 0, 0, 0, 196, 0, 0, 0, 58, 38, 0, 0, 178, 240, 1, 0}), "utf-32le"), "utf-32le")), ({65, 0, 0, 0, 196, 0, 0, 0, 58, 38, 0, 0, 178, 240, 1, 0})) :) }),
    ({ "to_bytes/to_text 4", 0, (: deep_eq(to_array(to_bytes(to_text(
                                   ({ 240, 159, 130, 178, 240, 159, 130, 179, 240, 159, 130, 180, 240, 159, 130, 181, 240, 159, 130, 182, 240, 159, 130, 183, 240, 159, 130, 184, 240, 159, 130, 185, 240, 159, 130, 186, 240, 159, 130, 187, 240, 159, 130, 188, 240, 159, 130, 189, 240, 159, 130, 190, 240, 159, 130, 177}), "utf-8"), "utf-8")),
                                   ({ 240, 159, 130, 178, 240, 159, 130, 179, 240, 159, 130, 180, 240, 159, 130, 181, 240, 159, 130, 182, 240, 159, 130, 183, 240, 159, 130, 184, 240, 159, 130, 185, 240, 159, 130, 186, 240, 159, 130, 187, 240, 159, 130, 188, 240, 159, 130, 189, 240, 159, 130, 190, 240, 159, 130, 177})) :) }),
    ({ "to_bytes/to_text 5", 0, (: deep_eq(to_array(to_bytes(to_text(
                                   ({216, 60, 220, 178, 216, 60, 220, 179, 216, 60, 220, 180, 216, 60, 220, 181, 216, 60, 220, 182, 216, 60, 220, 183, 216, 60, 220, 184, 216, 60, 220, 185, 216, 60, 220, 186, 216, 60, 220, 187, 216, 60, 220, 188, 216, 60, 220, 189, 216, 60, 220, 190, 216, 60, 220, 177}), "utf-16be"), "utf-16be")),
                                   ({216, 60, 220, 178, 216, 60, 220, 179, 216, 60, 220, 180, 216, 60, 220, 181, 216, 60, 220, 182, 216, 60, 220, 183, 216, 60, 220, 184, 216, 60, 220, 185, 216, 60, 220, 186, 216, 60, 220, 187, 216, 60, 220, 188, 216, 60, 220, 189, 216, 60, 220, 190, 216, 60, 220, 177})) :) }),
    ({ "to_bytes/to_text 6", 0, (: deep_eq(to_array(to_bytes(to_text(
                                   ({0, 1, 240, 178, 0, 1, 240, 179, 0, 1, 240, 180, 0, 1, 240, 181, 0, 1, 240, 182, 0, 1, 240, 183, 0, 1, 240, 184, 0, 1, 240, 185, 0, 1, 240, 186, 0, 1, 240, 187, 0, 1, 240, 188, 0, 1, 240, 189, 0, 1, 240, 190, 0, 1, 240, 177}), "utf-32be"), "utf-32be")),
                                   ({0, 1, 240, 178, 0, 1, 240, 179, 0, 1, 240, 180, 0, 1, 240, 181, 0, 1, 240, 182, 0, 1, 240, 183, 0, 1, 240, 184, 0, 1, 240, 185, 0, 1, 240, 186, 0, 1, 240, 187, 0, 1, 240, 188, 0, 1, 240, 189, 0, 1, 240, 190, 0, 1, 240, 177})) :) }),

    ({ "to_struct anonymous from mapping", 0, (: mixed s = to_struct((["A": 10, "B": 20])); return sizeof(s) == 2 && s.("A") == 10 && s.("B") == 20; :) }),
    ({ "to_struct templated from mapping 1", 0, (: deep_eq(to_struct((["arg": ({10, 20}),]), (<test_struct>)), (<test_struct> ({10,20}))) :) }),
    ({ "to_struct templated from mapping 2", 0, (: deep_eq(to_struct((["arg": ({10, 20}), "superfluous": ({30, 40})]), (<test_struct>)), (<test_struct> ({10,20}))) :) }),
    ({ "to_struct templated from mapping 3", 0, (: mapping* m = ({ (["arg": ({10, 20}) ]) }) * 10; return deep_eq(to_struct(m[0], (<test_struct>)), (<test_struct> ({10,20}))); :) }),
    ({ "to_struct anonymous from wide mapping", 0, (: mixed s = to_struct((["A": 10; 11, "B": 20; 21])); return sizeof(s) == 2 && deep_eq(s.("A"), ({10, 11})) && deep_eq(s.("B"), ({20, 21})); :) }),
    ({ "to_struct templated from wide mapping 1", 0, (: deep_eq(to_struct((["arg": 10; 20,]), (<test_struct>)), (<test_struct> ({10,20}))) :) }),
    ({ "to_struct templated from wide mapping 2", 0, (: deep_eq(to_struct((["arg": 10; 20, "superfluous": 30; 40]), (<test_struct>)), (<test_struct> ({10,20}))) :) }),
    ({ "to_struct templated from wide mapping 3", 0, (: mapping* m = ({ (["arg": 10; 20 ]) }) * 10; return deep_eq(to_struct(m[0], (<test_struct>)), (<test_struct> ({10,20}))); :) }),
    ({ "to_struct anonymous from array", 0, (: mixed s = to_struct(({10, 20})); return sizeof(s) == 2 && s.(0) == 10 && s.(1) == 20; :) }),
    ({ "to_struct templated from array 1", 0, (: deep_eq(to_struct(({({10, 20})}), (<test_struct>)), (<test_struct> ({10,20}))) :) }),
    ({ "to_struct templated from array 2", 0, (: deep_eq(to_struct(({({10, 20}), ({30, 40})}), (<test_struct>)), (<test_struct> ({10,20}))) :) }),

    ({ "get_type_info with temporary anonymous struct 1", 0, (: deep_eq(get_type_info(to_struct((["A": 10]))), ({ T_STRUCT, "anonymous" })) :) }),
    ({ "get_type_info with temporary anonymous struct 2", 0, (: !strstr(get_type_info(to_struct((["A": 10])), 2), "anonymous ") :) }),
    ({ "struct_info of regular struct 1", 0, (: mixed info = struct_info((<test_struct> ({10,20})), SINFO_FLAT);
                                                return info[SI_NAME] == "test_struct" && ("/"+info[SI_PROG_NAME]) == program_name() && sizeof(info) == SI_MEMBER+1 &&
                                                       info[SI_MEMBER+0][SIM_NAME] == "arg" && info[SI_MEMBER+0][SIM_TYPE] == (TYPE_MOD_POINTER | TYPE_NUMBER);
                                             :) }),
    ({ "struct_info of regular struct 1", 0, (: mixed info = struct_info((<test_struct> ({10,20})), SINFO_NESTED);
                                                return info[SI_NAME] == "test_struct" && "/"+info[SI_PROG_NAME] == program_name() && sizeof(info) == SI_MEMBER+1 &&
                                                       info[SI_MEMBER+0][SIM_NAME] == "arg" && info[SI_MEMBER+0][SIM_TYPE] == (TYPE_MOD_POINTER | TYPE_NUMBER);
                                             :) }),
    ({ "struct_info of anonymous struct 1", 0, (: mixed info = struct_info(to_struct((["A": 10, "B": 20])), SINFO_FLAT);
                                                  return !strstr(info[SI_NAME], "anonymous") && info[SI_PROG_NAME] == "anonymous" && sizeof(info) == SI_MEMBER+2 &&
                                                         member((["A", "B"]), info[SI_MEMBER+0][SIM_NAME]) && member((["A", "B"]), info[SI_MEMBER+1][SIM_NAME]);
                                               :) }),
    ({ "struct_info of anonymous struct 2", 0, (: mixed info = struct_info(to_struct((["A": 10, "B": 20])), SINFO_NESTED);
                                                  return !strstr(info[SI_NAME], "anonymous") && info[SI_PROG_NAME] == "anonymous" && sizeof(info) == SI_MEMBER+2 &&
                                                         member((["A", "B"]), info[SI_MEMBER+0][SIM_NAME]) && member((["A", "B"]), info[SI_MEMBER+1][SIM_NAME]);
                                               :) }),

    ({ "transpose_array 1", 0, (: deep_eq(transpose_array(({})), ({})) :) }),
    ({ "transpose_array 2", 0, (: deep_eq(transpose_array(({ ({}), ({}), ({}) })), ({})) :) }),
    ({ "transpose_array 3", 0, (: deep_eq(transpose_array(({ ({1,2,3}), ({"A","B","C"}) })), ({ ({1, "A"}), ({2, "B"}), ({3, "C"}) })) :) }),
    ({ "transpose_array 4", 0, (: deep_eq(transpose_array(({ ({1}),     ({"A","B","C"}) })), ({ ({1, "A"}), ({0, "B"}), ({0, "C"}) })) :) }),
    ({ "transpose_array 5", 0, (: deep_eq(transpose_array(({ ({}),      ({"A","B","C"}) })), ({ ({0, "A"}), ({0, "B"}), ({0, "C"}) })) :) }),
    ({ "transpose_array 6", 0, (: deep_eq(transpose_array(({ ({1,2,3}), ({"A"})         })), ({ ({1, "A"}), ({2, 0}),   ({3, 0})   })) :) }),
    ({ "transpose_array 7", 0, (: deep_eq(transpose_array(({ ({1,2,3}), ({})            })), ({ ({1, 0}),   ({2, 0}),   ({3, 0})   })) :) }),
    ({ "transpose_array 8", 0, (: deep_eq(transpose_array(({ ({1,}), ({"A","B"}), ({'x,'y,'z}), ({-0.1, -0.2, -0.3, -0.4}) })), ({ ({1,"A",'x,-0.1}), ({0,"B",'y,-0.2}), ({0,0,'z,-0.3}), ({0,0,0,-0.4}) })) :) }),
    ({ "transpose_array 9", 0, (: deep_eq(transpose_array(({ ({1,2,3,4}), ({"A","B","C"}), ({'x,'y}), ({-0.1}) })), ({ ({1,"A",'x,-0.1}), ({2,"B",'y,0}), ({3,"C",0,0}), ({4,0,0,0}) })) :) }),

    ({ "explode 1", 0, (: deep_eq(explode("",""), ({""})) :) }),
    ({ "explode 2", 0, (: deep_eq(explode("","anything"), ({""})) :) }),
    ({ "explode 3", 0, (: deep_eq(explode("abc",""), ({"a","b","c"})) :) }),
    ({ "explode 4", 0, (: deep_eq(explode("abc","xyz"), ({"abc"})) :) }),
    ({ "explode 5", 0, (: deep_eq(explode("abc","abc"), ({"",""})) :) }),
    ({ "explode 6", 0, (: deep_eq(explode(" ab cd ef ", " "), 
                                  ({ "", "ab", "cd", "ef", "" })) :) }),
    ({ "implode 1", 0, (: implode(({ "foo", "bar", "" }), "*") == "foo*bar*":) }),
    ({ "implode 2", 0, (: implode(({ "a", 2, this_object(), "c" }), "b") == "abc" :) }),
    ({ "implode 3", 0, (: implode(({ "", "" }), "") == "":) }),
    ({ "implode 4", 0, (: implode(({ b"" }), b"abc") == b"" :) }),
    ({ "random", 0,
        (:
            /* Let's check every 8 bits for some randomness. */
            int shift = 0;
            for (int remaining = __INT_MAX__; remaining; remaining >>= 8)
            {
                mapping result = ([:0]);
                foreach (int i: 2048)
                    m_add(result, (random(__INT_MAX__) >> shift) & 255);

                /* We expect 80% of the range to be filled. */
                if (sizeof(result) < (remaining & 255) * 4 / 5)
                    return 0;

                shift += 8;
            }

            return 1;
        :)
    }),
    ({ "save_/restore_value", 0,
        (:
            mixed qa = '''({ 42 });

            foreach(mixed val:
            ({
                __FLOAT_MIN__,
                __FLOAT_MAX__,
                0.1,
                10.2,
                -0.5,
                -100.1,

                __INT_MIN__,
                __INT_MAX__,
                0,
                1,
                -10,

                "ABC",
                save_value(({""})),
                "\"",
                "\0\0",

                quote("Hello"),
                quote(","),

                #'copy,
                #',,
                #'f,
                #'tests,

                ({}),
                ({-1})*100,

                '({}),
                ''({1,2,3}),
                ({ qa, qa }),

                ([:0]),
                ([:1]),
                ([:2]),
                (["a":1;2;3, "b":4;5;6]),
                ([1:"a";"b", 2:"c";"d"]),
                to_struct((["a":1, "b": 4, "c": 16])),
                to_struct(({1,2,3})),
            }))
            {
                if(!deep_eq(val, restore_value(save_value(val))))
                    return 0;

                if(!deep_eq(({val}), restore_value(save_value(({val})))))
                    return 0;

                if(member(([T_POINTER, T_MAPPING, T_QUOTED_ARRAY, T_STRUCT]), get_type_info(val)[0]))
                {
                    /* We just check, that it works without throwing an error. */
                    restore_value(save_value(([val])));
                    restore_value(save_value(([val:1;2])));
                    restore_value(save_value((["a":val;2])));
                }
                else
                {
                    if(!deep_eq(([val]), restore_value(save_value(([val])))))
                        return 0;

                    if (floatp(val))
                    {
                        /* Check support for the old format.
                         * We just check the absence of errors,
                         * because it's imprecise.
                         */
                        restore_value(save_value(val, 0));
                        restore_value(save_value(({val}), 0));
                        restore_value(save_value(([val]), 0));
                    }
                }
            }

            return 1;
        :)
    }),
    ({ "sort_array 1", 0, (: deep_eq(sort_array(({4,5,2,6,1,3,0}),#'>),
                                     ({0,1,2,3,4,5,6})) :) }),
    ({ "sort_array 2", 0, // sort in-place
        (:
           int *a = ({4,5,2,6,1,3,0});
           int *b = a;
           sort_array(&a, #'>);
           return deep_eq(a, b) && deep_eq(a, ({0,1,2,3,4,5,6}) );
        :)
    }),

    ({ "variable_list 1", 0, (: deep_eq(variable_list(this_object()),                        ({ "last_rt_warning",            "json_testdata", "json_teststring", "b",              "dhe_testdata", "clone",     "tests"                   })) :) }),
    ({ "variable_list 2", 0, (: deep_eq(map(variable_list(this_object(), RETURN_FUNCTION_FLAGS), #'&, NAME_INHERITED|TYPE_MOD_NOSAVE|TYPE_MOD_PRIVATE|TYPE_MOD_PROTECTED|TYPE_MOD_VIRTUAL|TYPE_MOD_NO_MASK|TYPE_MOD_PUBLIC),
                                                                                             ({ 0,                            0,               0,                 TYPE_MOD_NO_MASK, 0,              0,           0                         })) :) }),
    ({ "variable_list 3", 0, (: deep_eq(variable_list(this_object(), RETURN_FUNCTION_TYPE),  ({ TYPE_MOD_POINTER|TYPE_STRING, TYPE_MAPPING,    TYPE_STRING,       TYPE_BYTES,       TYPE_STRING,    TYPE_OBJECT, TYPE_MOD_POINTER|TYPE_ANY })) :) }),
    ({ "variable_list 4", 0, (: deep_eq(variable_list(this_object(), RETURN_FUNCTION_NAME | RETURN_FUNCTION_TYPE), 
                                ({ "last_rt_warning", TYPE_MOD_POINTER|TYPE_STRING, "json_testdata", TYPE_MAPPING, "json_teststring", TYPE_STRING, "b", TYPE_BYTES, "dhe_testdata", TYPE_STRING, "clone", TYPE_OBJECT, "tests", TYPE_MOD_POINTER|TYPE_ANY })) :) }),
    ({ "variable_list 5", 0, (: variable_list(this_object(), RETURN_VARIABLE_VALUE)[3] == b"\x00" :) }),

#ifdef __JSON__
    ({ "json_parse/_serialize 1", 0,
        (: json_parse(json_serialize(1) ) == 1:) }),
    ({ "json_parse/_serialize 2", 0,
        (: json_parse(json_serialize(42.0) ) == 42.0:) }),
    ({ "json_parse/_serialize 3", 0,
        (: json_parse(json_serialize("hello world\n") ) == "hello world\n":) }),
    ({ "json_parse/_serialize 4", 0,
        (: json_parse(json_serialize( ({}) ) ) == ({}) :) }),
    ({ "json_parse/_serialize 5", 0,
        (: deep_eq(json_parse(json_serialize( ({1,2,3,4,5,6}) ) ), ({1,2,3,4,5,6}) ) :) }),
    ({ "json_parse/_serialize 6", 0,
        (: deep_eq(json_parse(json_serialize( ([]) ) ), ([]) ) :) }),
    ({ "json_parse/_serialize 7", 0,
        (: deep_eq(json_parse(json_serialize( json_testdata ) ), json_testdata) :) }),
    ({ "json_parse/_serialize 8", 0,
        (: json_parse(json_serialize(__FLOAT_MAX__) ) == __FLOAT_MAX__ :) }),
    ({ "json_parse/_serialize 9", 0,
        (: json_parse(json_serialize(__FLOAT_MIN__) ) == __FLOAT_MIN__ :) }),
    ({ "json_parse/_serialize 10", 0,
        (: json_parse(json_serialize(__INT_MAX__) ) == __INT_MAX__:) }),
    ({ "json_parse/_serialize 11", 0,
        (: json_parse(json_serialize(__INT_MIN__) ) == __INT_MIN__ :) }),
    ({ "json_parse 1", 0,
        (: json_parse("true") == 1 :) }),
    ({ "json_parse 2", 0,
        (: json_parse("false") == 0 :) }),
    ({ "json_parse 3", 0,
        (: deep_eq(json_parse(json_teststring), json_testdata) :) }),
    ({ "json_serialize + json_parse 1", 0,
        (: deep_eq(json_parse(json_serialize(json_testdata)), json_testdata) 
         :) }),
#endif // __JSON__
#ifdef __TLS__
}) + (tls_available() ? ({
    ({ "configure_driver DHE 1 (testdata)", 0,
        (: configure_driver(DC_TLS_DHE_PARAMETER, dhe_testdata);
           return 1; :) }),
    ({ "configure_driver DHE 2 (defaults)", 0,
        (: configure_driver(DC_TLS_DHE_PARAMETER, 0);
           return 1; :) }),
    ({ "configure_driver DHE 3 arg error", TF_ERROR,
        (: configure_driver(DC_TLS_DHE_PARAMETER, 42) :) }),
    ({ "configure_driver DHE 4 arg error", TF_ERROR,
        (: configure_driver(DC_TLS_DHE_PARAMETER, "abcdef42") :) }),
    ({ "configure_driver DHE 4 arg error", TF_ERROR,
        (: configure_driver(DC_TLS_DHE_PARAMETER, ({1}) ) :) }),
    ({ "configure_driver DC_TLS_CIPHERLIST 1 default", 0,
        (: configure_driver(DC_TLS_CIPHERLIST, 0);
           return 1; :) }),
    ({ "configure_driver DC_TLS_CIPHERLIST 2", TF_ERROR,
        (: configure_driver(DC_TLS_CIPHERLIST, "__never_a_cipher_");
           return 0; :) }),
    ({ "configure_driver DC_TLS_CIPHERLIST 3", 0,
#if defined(__GNUTLS__)
        (: configure_driver(DC_TLS_CIPHERLIST, "NORMAL");
#elif defined (__OPENSSL__)
        (: configure_driver(DC_TLS_CIPHERLIST, "DEFAULT");
#endif
           return 1; :) }),
}) : ({
    // TLS is not available, check that is doesn't crash.
    ({ "configure_driver DHE 1 (testdata) - not available", TF_ERROR,
        (: configure_driver(DC_TLS_DHE_PARAMETER, dhe_testdata);
           return 1; :) }),
    ({ "configure_driver DHE (default) - not available", TF_ERROR,
        (: configure_driver(DC_TLS_DHE_PARAMETER, 0);
           return 1; :) }),
    ({ "configure_driver DC_TLS_CIPHERLIST - not available", TF_ERROR,
        (: configure_driver(DC_TLS_CIPHERLIST, 0);
           return 1; :) }),
})) + ({
#endif // __TLS__
});

void run_test()
{
    msg("\nRunning test suite for efuns:\n"
          "-----------------------------\n");

    run_array(tests,
        (:
            if($1)
                shutdown(1);
            else
                start_gc(#'shutdown);

            return 0;
        :));
}

string *epilog(int eflag)
{
    configure_driver(DC_MEMORY_LIMIT, ({ 0x10000000, 0x20000000 }));

    set_driver_hook(H_DEFAULT_METHOD, function int(mixed result, object ob, string fun, varargs mixed* args)
    {
        if (fun == "g")
        {
            result = ob->f(args...) + 1;
            return 1;
        }

        return 0;
    });

    run_test();
    return 0;
}

int f(int arg)
{
    return arg + 1;
}
