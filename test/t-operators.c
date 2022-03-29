#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/inc/deep_eq.inc"

mixed *tests = ({
    ({ "int + float", 0,          (: float val = 1 + 2.5; return val >  3.4999 && val <  3.5001; :) }),
    ({ "int - float", 0,          (: float val = 1 - 2.5; return val > -1.5001 && val < -1.4999; :) }),
    ({ "int * float", 0,          (: float val = 1 * 2.5; return val >  2.4999 && val <  2.5001; :) }),
    ({ "int / float", 0,          (: float val = 1 / 2.5; return val >  0.3999 && val <  0.4001; :) }),

    ({ "int += float", 0,          (: int|float val = 1; val += 2.5; return val >  3.4999 && val <  3.5001; :) }),
    ({ "int -= float", 0,          (: int|float val = 1; val -= 2.5; return val > -1.5001 && val < -1.4999; :) }),
    ({ "int *= float", 0,          (: int|float val = 1; val *= 2.5; return val >  2.4999 && val <  2.5001; :) }),
    ({ "int /= float", 0,          (: int|float val = 1; val /= 2.5; return val >  0.3999 && val <  0.4001; :) }),

    ({ "int* + int*", 0,          (: deep_eq(({0,1,2,3}) + ({4,2,1}), ({0,1,2,3,4,2,1})) :) }),
    ({ "int* - int*", 0,          (: deep_eq(({0,1,2,3}) - ({4,2,1}), ({0,3}))           :) }),
    ({ "int* & int*", 0,          (: deep_eq(({0,1,2,3}) & ({4,2,1}), ({1,2}))           :) }),
    ({ "int* | int*", 0,          (: deep_eq(({0,1,2,3}) | ({4,2,1}), ({0,1,2,3,4}))     :) }),
    ({ "int* ^ int*", 0,          (: deep_eq(({0,1,2,3}) ^ ({4,2,1}), ({0,3,4}))         :) }),

    ({ "int* += int*", 0,         (: int *val = ({0,1,2,3}); val += ({4,2,1}); return deep_eq(val, ({0,1,2,3,4,2,1})); :) }),
    ({ "int* -= int*", 0,         (: int *val = ({0,1,2,3}); val -= ({4,2,1}); return deep_eq(val, ({0,3}));           :) }),
    ({ "int* &= int*", 0,         (: int *val = ({0,1,2,3}); val &= ({4,2,1}); return deep_eq(val, ({1,2}));           :) }),
    ({ "int* |= int*", 0,         (: int *val = ({0,1,2,3}); val |= ({4,2,1}); return deep_eq(val, ({0,1,2,3,4}));     :) }),
    ({ "int* ^= int*", 0,         (: int *val = ({0,1,2,3}); val ^= ({4,2,1}); return deep_eq(val, ({0,3,4}));         :) }),

    ({ "mapping + mapping 1", 0,  (: deep_eq(([0,1,2,3])                                + ([4,2,1]),                        ([0,1,2,3,4]))                                                  :) }),
    ({ "mapping + mapping 2", 0,  (: deep_eq(([0:"a", 1:"b", 2:"c", 3:"d"])             + ([4:"x", 2:"y", 1:"z"]),          ([0:"a", 1:"z", 2:"y", 3:"d", 4: "x"]))                         :) }),
    ({ "mapping + mapping 3", 0,  (: deep_eq((['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]) + (['d:4;"x", 'e:2;"y", 'f:1;"z"]), (['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:4;"x", 'e:2;"y", 'f:1;"z"])) :) }),
    ({ "mapping - mapping 1", 0,  (: deep_eq(([0,1,2,3])                                - ([4,2,1]),                        ([0,3]))                                                        :) }),
    ({ "mapping - mapping 2", 0,  (: deep_eq(([0:"a", 1:"b", 2:"c", 3:"d"])             - ([4:"x", 2:"y", 1:"z"]),          ([0:"a", 3:"d"]))                                               :) }),
    ({ "mapping - mapping 3", 0,  (: deep_eq((['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]) - (['d:4;"x", 'e:2;"y", 'f:1;"z"]), (['a:0;"a", 'b:1;"b", 'c:2;"c"]))                               :) }),
    ({ "mapping & mapping 1", 0,  (: deep_eq(([0,1,2,3])                                & ([4,2,1]),                        ([1,2]))                                                        :) }),
    ({ "mapping & mapping 2", 0,  (: deep_eq(([0:"a", 1:"b", 2:"c", 3:"d"])             & ([4:"x", 2:"y", 1:"z"]),          ([1:"b", 2:"c"]))                                               :) }),
    ({ "mapping & mapping 3", 0,  (: deep_eq((['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]) & (['d:4;"x", 'e:2;"y", 'f:1;"z"]), (['d:3;"d"]))                                                   :) }),
    ({ "mapping & mapping 4", 0,  (: deep_eq((['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]) & (['d, 'e, 'f]),                   (['d:3;"d"]))                                                   :) }),

    ({ "mapping += mapping 1", 0, (: mapping val = ([0,1,2,3]);                                val += ([4,2,1]);                        return deep_eq(val, ([0,1,2,3,4]));                                                  :) }),
    ({ "mapping += mapping 2", 0, (: mapping val = ([0:"a", 1:"b", 2:"c", 3:"d"]);             val += ([4:"x", 2:"y", 1:"z"]);          return deep_eq(val, ([0:"a", 1:"z", 2:"y", 3:"d", 4: "x"]));                         :) }),
    ({ "mapping += mapping 3", 0, (: mapping val = (['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]); val += (['d:4;"x", 'e:2;"y", 'f:1;"z"]); return deep_eq(val, (['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:4;"x", 'e:2;"y", 'f:1;"z"])); :) }),
    ({ "mapping -= mapping 1", 0, (: mapping val = ([0,1,2,3]);                                val -= ([4,2,1]);                        return deep_eq(val, ([0,3]));                                                        :) }),
    ({ "mapping -= mapping 2", 0, (: mapping val = ([0:"a", 1:"b", 2:"c", 3:"d"]);             val -= ([4:"x", 2:"y", 1:"z"]);          return deep_eq(val, ([0:"a", 3:"d"]));                                               :) }),
    ({ "mapping -= mapping 3", 0, (: mapping val = (['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]); val -= (['d:4;"x", 'e:2;"y", 'f:1;"z"]); return deep_eq(val, (['a:0;"a", 'b:1;"b", 'c:2;"c"]));                               :) }),
    ({ "mapping &= mapping 1", 0, (: mapping val = ([0,1,2,3]);                                val &= ([4,2,1]);                        return deep_eq(val, ([1,2]));                                                        :) }),
    ({ "mapping &= mapping 2", 0, (: mapping val = ([0:"a", 1:"b", 2:"c", 3:"d"]);             val &= ([4:"x", 2:"y", 1:"z"]);          return deep_eq(val, ([1:"b", 2:"c"]));                                               :) }),
    ({ "mapping &= mapping 3", 0, (: mapping val = (['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]); val &= (['d:4;"x", 'e:2;"y", 'f:1;"z"]); return deep_eq(val, (['d:3;"d"]));                                                   :) }),
    ({ "mapping &= mapping 4", 0, (: mapping val = (['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]); val &= (['d, 'e, 'f]);                   return deep_eq(val, (['d:3;"d"]));                                                   :) }),

    ({ "int* & mapping",   0,     (: deep_eq(({0,1,2,3})                                & ([4,2,1]),    ({1,2}))                          :) }),
    ({ "mapping & int* 1", 0,     (: deep_eq(([0,1,2,3])                                & ({4,2,1}),    ([1,2]))                          :) }),
    ({ "mapping & int* 2", 0,     (: deep_eq(([0:"a", 1:"b", 2:"c", 3:"d"])             & ({4,2,1}),    ([1:"b", 2:"c"]))                 :) }),
    ({ "mapping & int* 3", 0,     (: deep_eq((['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]) & ({'d,'e,'f}), (['d:3;"d"]))                     :) }),
    ({ "int* - mapping",   0,     (: deep_eq(({0,1,2,3})                                - ([4,2,1]),    ({0,3}))                          :) }),
    ({ "mapping - int* 1", 0,     (: deep_eq(([0,1,2,3])                                - ({4,2,1}),    ([0,3]))                          :) }),
    ({ "mapping - int* 2", 0,     (: deep_eq(([0:"a", 1:"b", 2:"c", 3:"d"])             - ({4,2,1}),    ([0:"a", 3:"d"]))                 :) }),
    ({ "mapping - int* 3", 0,     (: deep_eq((['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]) - ({'d,'e,'f}), (['a:0;"a", 'b:1;"b", 'c:2;"c"])) :) }),

    ({ "int* &= mapping",   0,    (: int *val    = ({0,1,2,3});                                val &= ([4,2,1]);    return deep_eq(val, ({1,2}));                          :) }),
    ({ "mapping &= int* 1", 0,    (: mapping val = ([0,1,2,3]);                                val &= ({4,2,1});    return deep_eq(val, ([1,2]));                          :) }),
    ({ "mapping &= int* 2", 0,    (: mapping val = ([0:"a", 1:"b", 2:"c", 3:"d"]);             val &= ({4,2,1});    return deep_eq(val, ([1:"b", 2:"c"]));                 :) }),
    ({ "mapping &= int* 3", 0,    (: mapping val = (['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]); val &= ({'d,'e,'f}); return deep_eq(val, (['d:3;"d"]));                     :) }),
    ({ "int* -= mapping",   0,    (: int *val    = ({0,1,2,3});                                val -= ([4,2,1]);    return deep_eq(val, ({0,3}));                          :) }),
    ({ "mapping -= int* 1", 0,    (: mapping val = ([0,1,2,3]);                                val -= ({4,2,1});    return deep_eq(val, ([0,3]));                          :) }),
    ({ "mapping -= int* 2", 0,    (: mapping val = ([0:"a", 1:"b", 2:"c", 3:"d"]);             val -= ({4,2,1});    return deep_eq(val, ([0:"a", 3:"d"]));                 :) }),
    ({ "mapping -= int* 3", 0,    (: mapping val = (['a:0;"a", 'b:1;"b", 'c:2;"c", 'd:3;"d"]); val -= ({'d,'e,'f}); return deep_eq(val, (['a:0;"a", 'b:1;"b", 'c:2;"c"])); :) }),

    ({ "int in array 1",               0, (:   3      in ({1,2,3})  :) }),
    ({ "int in array 2",               0, (: !(5      in ({1,2,3})) :) }),
    ({ "int in mapping 1",             0, (:   3      in ([1,2,3])  :) }),
    ({ "int in mapping 2",             0, (: !(5      in ([1,2,3])) :) }),

    ({ "int in string 1",              0, (:   'a'    in "Hallo"    :) }),
    ({ "int in string 2",              0, (: !('A'    in "Hallo")   :) }),
    ({ "int in string 3",              0, (:   0x263b in "\u263a\u263b\u263b\u263a"  :) }),
    ({ "int in string 4",              0, (: !(0x2639 in "\u263a\u263b\u263b\u263a") :) }),
    ({ "string in string 1",           0, (:   "all"  in "Hallo"    :) }),
    ({ "string in string 2",           0, (: !("I"    in "Team")    :) }),
    ({ "string in string 3",           0, (:   "\U0001f0b1\U0001f0c1" in "\U0001f0a1\U0001f0b1\U0001f0c1\U0001f0d1"  :) }),
    ({ "string in string 4",           0, (: !("\U0001f0d1\U0001f0d2" in "\U0001f0a1\U0001f0b1\U0001f0c1\U0001f0d1") :) }),
    ({ "int in &(string[]) 1",         0, (: string str = "Some String"; return   'S' in &(str[5..7])  &&   't' in &(str[5..7])  &&    'r' in &(str[5..7]);  :) }),
    ({ "int in &(string[]) 2",         0, (: string str = "Some String"; return !('o' in &(str[5..7])) && !(' ' in &(str[5..7])) && ! ('i' in &(str[5..7])); :) }),
    ({ "int in &(string[]) 3",         0, (: string str = "\U0001f1e6\U0001f1e7\U0001f1e8\U0001f1e9\U0001f1ea\U0001f1eb"; return   0x1f1e8 in &(str[2..3])  &&   0x1f1e9 in &(str[2..3]);  :) }),
    ({ "int in &(string[]) 4",         0, (: string str = "\U0001f1e6\U0001f1e7\U0001f1e8\U0001f1e9\U0001f1ea\U0001f1eb"; return !(0x1f1e7 in &(str[2..3])) && !(0x1f1ea in &(str[2..3])); :) }),
    ({ "string in &(string[]) 1",      0, (: string str = "Some String"; return   "Str"  in &(str[5..7])  &&   "St"     in &(str[5..7])  &&   "tr"   in &(str[5..7]);  :) }),
    ({ "string in &(string[]) 2",      0, (: string str = "Some String"; return !("Some" in &(str[5..7])) && !("String" in &(str[5..7])) && !(" Str" in &(str[5..7])); :) }),
    ({ "string in &(string[]) 3",      0, (: string str = "\U0001f1e6\U0001f1e7\U0001f1e8\U0001f1e9\U0001f1ea\U0001f1eb"; return   "\U0001f1e8" in &(str[2..3])  &&   "\U0001f1e8\U0001f1e9" in &(str[2..3])  &&   "\U0001f1e9"           in &(str[2..3]);  :) }),
    ({ "string in &(string[]) 4",      0, (: string str = "\U0001f1e6\U0001f1e7\U0001f1e8\U0001f1e9\U0001f1ea\U0001f1eb"; return !("\U0001f1e7" in &(str[2..3])) && !("\U0001f1e7\U0001f1e8" in &(str[2..3])) && !("\U0001f1e9\U0001f1ea" in &(str[2..3])); :) }),
    ({ "&(string[]) in &(string[]) 1", 0, (: string str = "Some String"; return   &(str[5..7]) in &(str[5..7])  &&   &(str[5..5]) in &(str[5..7])  &&   &(str[0..0]) in &(str[5..7]);  :) }),
    ({ "&(string[]) in &(string[]) 2", 0, (: string str = "Some String"; return !(&(str[0..4]) in &(str[5..7])) && !(&(str[4..5]) in &(str[5..7])) && !(&(str[5..8]) in &(str[5..7])); :) }),
    ({ "&(string[]) in &(string[]) 3", 0, (: string str = "\U0001f1e6\U0001f1e7\U0001f1e8\U0001f1e9\U0001f1ea\U0001f1eb"; return   &(str[2..3]) in &(str[2..3])  &&   &(str[2..2]) in &(str[2..3])  &&   &(str[3..3]) in &(str[2..3]);  :) }),
    ({ "&(string[]) in &(string[]) 4", 0, (: string str = "\U0001f1e6\U0001f1e7\U0001f1e8\U0001f1e9\U0001f1ea\U0001f1eb"; return !(&(str[1..2]) in &(str[2..3])) && !(&(str[2..4]) in &(str[2..3])) && !(&(str[3..4]) in &(str[2..3])); :) }),

    ({ "int in bytes 1",               0, (:   'a'    in b"Hallo"   :) }),
    ({ "int in bytes 2",               0, (: !('A'    in b"Hallo")  :) }),
    ({ "bytes in bytes 1",             0, (:   b"all" in b"Hallo"    :) }),
    ({ "bytes in bytes 2",             0, (: !(b"I"   in b"Team")    :) }),
    ({ "int in &(bytes[]) 1",          0, (: bytes str = b"Some String"; return   'S' in &(str[5..7])  &&   't' in &(str[5..7])  &&    'r' in &(str[5..7]);  :) }),
    ({ "int in &(bytes[]) 2",          0, (: bytes str = b"Some String"; return !('o' in &(str[5..7])) && !(' ' in &(str[5..7])) && ! ('i' in &(str[5..7])); :) }),
    ({ "bytes in &(bytes[]) 1",        0, (: bytes str = b"Some String"; return   b"Str"  in &(str[5..7])  &&   b"St"     in &(str[5..7])  &&   b"tr"   in &(str[5..7]);  :) }),
    ({ "bytes in &(bytes[]) 2",        0, (: bytes str = b"Some String"; return !(b"Some" in &(str[5..7])) && !(b"String" in &(str[5..7])) && !(b" Str" in &(str[5..7])); :) }),
    ({ "&(bytes[]) in &(bytes[]) 1",   0, (: bytes str = b"Some String"; return   &(str[5..7]) in &(str[5..7])  &&   &(str[5..5]) in &(str[5..7])  &&   &(str[0..0]) in &(str[5..7]);  :) }),
    ({ "&(bytes[]) in &(bytes[]) 2",   0, (: bytes str = b"Some String"; return !(&(str[0..4]) in &(str[5..7])) && !(&(str[4..5]) in &(str[5..7])) && !(&(str[5..8]) in &(str[5..7])); :) }),

});

void run_test()
{
    msg("\nRunning test suite for operators:\n"
          "---------------------------------\n");

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
    run_test();
    return 0;
}
