#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/inc/deep_eq.inc"

mixed *tests = ({
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
