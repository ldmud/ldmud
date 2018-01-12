#include "inc/deep_eq.inc"
#include "inc/msg.inc"

/* Here are the values that shall be swapped out and back in. */
int intval = 42;

int lval1 = 100;
int lval2 = &lval1;

string strval = "Hello!";

int* vec1 = ({});
int* vec2 = ({1,2,3});
int* vec3 = vec2;

object ob = find_object(__MASTER_OBJECT__);

mapping map1 = ([:0]);
mapping map2 = (["a":0;1, "b":10;11, "c": "hundert";"hunderteins" ]);

float floatval = 2.718281828459045;

closure cl1 = (: -1 :);
closure cl2 = lambda(0, ({#'return, -2 }));

symbol symb = 'abc;

mixed qarr = '''({5});

struct SomeStruct
{
    int val;
};

struct SomeStruct somestructval = (<SomeStruct> 150);

/* Shall return 1 for success, 0 for error. */
int check()
{
    if (intval != 42)
    {
        msg("intval contains wrong value.\n");
        return 0;
    }

    lval1--;
    if (lval2 != 99)
    {
        msg("lvalue reference doesn't work anymore.\n");
        return 0;
    }

    if (strval != "Hello!")
    {
        msg("strval contains wrong value.\n");
        return 0;
    }

    if (!deep_eq(vec1, ({})))
    {
        msg("vec1 contains wrong value.\n");
        return 0;
    }

    if (!deep_eq(vec2, ({1,2,3})))
    {
        msg("vec2 contains wrong value.\n");
        return 0;
    }

    vec2[1] = 22;
    if (!deep_eq(vec3, ({1,22,3})))
    {
        msg("vec3 contains wrong value.\n");
        return 0;
    }

    if (ob != previous_object())
    {
        msg("ob contains wrong value.\n");
        return 0;
    }

    if (!deep_eq(map1, ([:0])))
    {
        msg("map1 contains wrong value.\n");
        return 0;
    }

    if (!deep_eq(map2, (["a":0;1, "b":10;11, "c": "hundert";"hunderteins" ])))
    {
        msg("map2 contains wrong value.\n");
        return 0;
    }

    if (floatval != 2.718281828459045)
    {
        msg("floatval contains wrong value.\n");
        return 0;
    }

    if (!closurep(cl1) || funcall(cl1) != -1)
    {
        msg("cl1 contains wrong value.\n");
        return 0;
    }

    if (!closurep(cl2) || funcall(cl2) != -2)
    {
        msg("cl2 contains wrong value.\n");
        return 0;
    }

    if (symb != 'abc)
    {
        msg("floatval contains wrong value.\n");
        return 0;
    }

    if (!deep_eq(qarr, '''({5})))
    {
        msg("qarr contains wrong value.\n");
        return 0;
    }

    if (!deep_eq(somestructval, (<SomeStruct> 150)))
    {
        msg("somestructval contains wrong value.\n");
        return 0;
    }

    return 1;
}
