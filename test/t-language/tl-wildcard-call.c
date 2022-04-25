inherit "ti-wildcard-call-1";
inherit "ti-wildcard-call-2";
inherit "ti-wildcard-call-3";

#include "/inc/deep_eq.inc"

int run_test()
{
    return deep_eq("*"::funA(), ({1,2}))
        && deep_eq("*2"::funA(), ({2}))
        && deep_eq("*"::funB(10), ({11,12,13}))
        && deep_eq("*3"::funB(10), ({13}))
        && deep_eq("*"::funC(10,100,1000), ({1111, 1112, 1113}))
        && deep_eq("*1"::funC(10,100,1000), ({1111}))
        && deep_eq("*"::funD(10,100,1000,10000), ({}));
}
