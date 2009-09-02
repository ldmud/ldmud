inherit "inh";

#include "inc/msg.inc"

string geteuid()
{
    return "lfun";
}

#define CHECK_PREFIX(fun, result)                                                   \
    msg("Checking fun() == \""+result+"\":\t\t\t\t");                               \
    if(fun() == result)                                                             \
        msg("Success.\n");                                                          \
    else                                                                            \
    {                                                                               \
        errors++;                                                                   \
        msg("FAILURE.\n");                                                          \
    }                                                                               \
                                                                                    \
    msg("Checking funcall(#'fun) == \""+result+"\":\t\t\t");                        \
    if(funcall(#'fun) == result)                                                    \
        msg("Success.\n");                                                          \
    else                                                                            \
    {                                                                               \
        errors++;                                                                   \
        msg("FAILURE.\n");                                                          \
    }                                                                               \
                                                                                    \
    if(result == "lfun")                                                            \
    {                                                                               \
        msg("Checking funcall(symbol_function(\"fun\", TO)) == \""+result+"\":\t"); \
        if(funcall(symbol_function("fun", this_object())) == result)                \
            msg("Success.\n");                                                      \
        else                                                                        \
        {                                                                           \
            errors++;                                                               \
            msg("FAILURE.\n");                                                      \
        }                                                                           \
    }                                                                               \
    else if(result != "inh")                                                        \
    {                                                                               \
        msg("Checking funcall(symbol_function(\"fun\")) == \""+result+"\":\t");     \
        if(funcall(symbol_function("fun")) == result)                               \
            msg("Success.\n");                                                      \
        else                                                                        \
        {                                                                           \
            errors++;                                                               \
            msg("FAILURE.\n");                                                      \
        }                                                                           \
    }

int run_test()
{
    int errors;
    seteuid("efun");

    CHECK_PREFIX(geteuid, "lfun");
    CHECK_PREFIX(inh::geteuid, "inh");
    CHECK_PREFIX(efun::geteuid, "efun");
    CHECK_PREFIX(sefun::geteuid, "sefun");

    return errors;
}
