inherit "inh";

#include "inc/msg.inc"
#include "sys/configuration.h"

string geteuid = "var";

string geteuid()
{
    return "lfun";
}

#define CHECK_PREFIX_DIRECT(fun, result)                                            \
    msg("Checking fun() == \""+result+"\":\t\t\t\t");                               \
    if(fun() == result)                                                             \
        msg("Success.\n");                                                          \
    else                                                                            \
    {                                                                               \
        errors++;                                                                   \
        msg("FAILURE.\n");                                                          \
    }

#define CHECK_PREFIX_CLOSURE(fun, result)                                           \
    msg("Checking funcall(#'fun) == \""+result+"\":\t\t\t");                        \
    if(funcall(#'fun) == result)                                                    \
        msg("Success.\n");                                                          \
    else                                                                            \
    {                                                                               \
        errors++;                                                                   \
        msg("FAILURE.\n");                                                          \
    }

#define CHECK_PREFIX_SYMBOL_FUNCTION_OB(fun, result)                                \
    msg("Checking funcall(symbol_function(\"fun\", TO)) == \""+result+"\":\t");     \
    if(funcall(symbol_function("fun", this_object())) == result)                    \
        msg("Success.\n");                                                          \
    else                                                                            \
    {                                                                               \
        errors++;                                                                   \
        msg("FAILURE.\n");                                                          \
    }                                                                               \

#define CHECK_PREFIX_SYMBOL_FUNCTION(fun, result)                                   \
    msg("Checking funcall(symbol_function(\"fun\")) == \""+result+"\":\t");         \
    if(funcall(symbol_function("fun")) == result)                                   \
        msg("Success.\n");                                                          \
    else                                                                            \
    {                                                                               \
        errors++;                                                                   \
        msg("FAILURE.\n");                                                          \
    }

#define CHECK_PREFIX(fun, result)                                                   \
    CHECK_PREFIX_DIRECT(fun, result);                                               \
    CHECK_PREFIX_CLOSURE(fun, result);                                              \
    if(result == "lfun")                                                            \
    {                                                                               \
        CHECK_PREFIX_SYMBOL_FUNCTION_OB(fun, result);                               \
        res = "sefun";                                                              \
    }                                                                               \
    else                                                                            \
        res = result;                                                               \
    if(result != "inh")                                                             \
    {                                                                               \
        CHECK_PREFIX_SYMBOL_FUNCTION(fun, res);                                     \
    }

int run_test()
{
    int errors;
    string res;

    configure_object(this_object(), OC_EUID, "efun");

    CHECK_PREFIX(geteuid, "lfun");
    CHECK_PREFIX(inh::geteuid, "inh");
    CHECK_PREFIX(efun::geteuid, "efun");
    CHECK_PREFIX(sefun::geteuid, "sefun");

    CHECK_PREFIX_CLOSURE(lfun::geteuid, "lfun");
    CHECK_PREFIX_SYMBOL_FUNCTION_OB(lfun::geteuid, "lfun");

    CHECK_PREFIX_CLOSURE(var::geteuid, "var");

    return errors;
}
