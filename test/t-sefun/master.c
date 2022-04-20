#define OWN_PRIVILEGE_VIOLATION
#include "/inc/base.inc"

int num;

string get_simul_efun()
{
    num++;
    load_object("/sefun"+num);
    return "/sefun"+num;
}

string *epilog(int eflag)
{
    /* Reload Sefun. */
    closure sefun = symbol_function("sefun0000");
    destruct(find_object("/sefun1"));
    funcall(sefun);

    "/test"->run_test();
    return 0;
}

int privilege_violation(string op, string|object who, mixed arg, mixed arg2)
{
    switch (op)
    {
        case "pragma no_simul_efuns":
            if ("unprivileged" in who)
                return -1;
            else
                return 1;

        default:
            return 1;
    }
}

int is_old_master()
{
    return 1;
}
