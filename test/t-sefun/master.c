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
