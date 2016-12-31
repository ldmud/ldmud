inherit "inh/trampoline-user1";
inherit "inh/trampoline-user2";

int run_test()
{
    return "trampoline-user1"::f() == "user1" &&
           "trampoline-user2"::f() == "user2";
}
