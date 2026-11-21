/* Regression tests for PostgreSQL LISTEN/NOTIFY efuns (pg_listen / pg_unlisten).
 */
#pragma save_types, rtt_checks, clone

#define OWN_PRIVILEGE_VIOLATION

#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"

string last_privi_op;
mixed last_privi_who;
mixed last_privi_arg;
int allow_privi = 1;

int privilege_violation(string op, mixed who, varargs mixed* args)
{
    last_privi_op = op;
    last_privi_who = who;
    last_privi_arg = sizeof(args) ? args[0] : 0;
    return allow_privi;
}

void notify_cb(string channel, mixed payload)
{
}

#ifdef __PGSQL__
string try_listen_fn(string ch, string fn)
{
    return catch(pg_listen(ch, fn));
}

string try_listen_cl(string ch)
{
    return catch(pg_listen(ch, function void(string c, mixed p) {}));
}

string try_unlisten(string ch)
{
    return catch(pg_unlisten(ch));
}

mixed *tests = (this_object() == blueprint()) && ({
    ({ "pg_listen as master without connection bypasses privilege_violation", 0,
        (:
            last_privi_op = 0;
            string err = catch(pg_listen("test_chan", "notify_cb"));
            return stringp(err)
                && strstr(err, "not connected") >= 0
                && last_privi_op == 0;
        :)
    }),
    ({ "pg_listen as clone without connection (function name) triggers privilege check & error", 0,
        (:
            object cl = clone_object(this_object());
            allow_privi = 1;
            last_privi_op = 0;
            last_privi_who = 0;
            last_privi_arg = 0;
            string err = cl->try_listen_fn("test_chan", "notify_cb");
            int ok = stringp(err)
                  && strstr(err, "not connected") >= 0
                  && last_privi_op == "pgsql"
                  && last_privi_who == cl
                  && last_privi_arg == "pg_listen";
            destruct(cl);
            return ok;
        :)
    }),
    ({ "pg_listen as clone without connection (closure) triggers privilege check & error", 0,
        (:
            object cl = clone_object(this_object());
            allow_privi = 1;
            last_privi_op = 0;
            last_privi_who = 0;
            last_privi_arg = 0;
            string err = cl->try_listen_cl("test_chan");
            int ok = stringp(err)
                  && strstr(err, "not connected") >= 0
                  && last_privi_op == "pgsql"
                  && last_privi_who == cl
                  && last_privi_arg == "pg_listen";
            destruct(cl);
            return ok;
        :)
    }),
    ({ "pg_unlisten as clone without connection triggers privilege check & error", 0,
        (:
            object cl = clone_object(this_object());
            allow_privi = 1;
            last_privi_op = 0;
            last_privi_who = 0;
            last_privi_arg = 0;
            string err = cl->try_unlisten("test_chan");
            int ok = stringp(err)
                  && strstr(err, "not connected") >= 0
                  && last_privi_op == "pgsql"
                  && last_privi_who == cl
                  && last_privi_arg == "pg_unlisten";
            destruct(cl);
            return ok;
        :)
    }),
    ({ "pg_listen privilege denied fails before connection check", 0,
        (:
            object cl = clone_object(this_object());
            allow_privi = 0;
            last_privi_op = 0;
            last_privi_who = 0;
            last_privi_arg = 0;
            string err = cl->try_listen_fn("test_chan", "notify_cb");
            int ok = stringp(err)
                  && strstr(err, "not connected") < 0
                  && last_privi_op == "pgsql"
                  && last_privi_who == cl
                  && last_privi_arg == "pg_listen";
            destruct(cl);
            return ok;
        :)
    }),
    ({ "pg_unlisten privilege denied fails before connection check", 0,
        (:
            object cl = clone_object(this_object());
            allow_privi = 0;
            last_privi_op = 0;
            last_privi_who = 0;
            last_privi_arg = 0;
            string err = cl->try_unlisten("test_chan");
            int ok = stringp(err)
                  && strstr(err, "not connected") < 0
                  && last_privi_op == "pgsql"
                  && last_privi_who == cl
                  && last_privi_arg == "pg_unlisten";
            destruct(cl);
            return ok;
        :)
    }),
    ({ "pg_listen invalid arg 1", TF_ERROR,
        (:
            funcall(#'pg_listen, 42, "notify_cb");
            return 0;
        :)
    }),
    ({ "pg_listen invalid arg 2", TF_ERROR,
        (:
            funcall(#'pg_listen, "test_chan", 42);
            return 0;
        :)
    }),
    ({ "pg_unlisten invalid arg 1", TF_ERROR,
        (:
            funcall(#'pg_unlisten, 42);
            return 0;
        :)
    }),
});
#endif

void run_test()
{
    msg("\nRunning tests for pg_listen / pg_unlisten:\n"
          "------------------------------------------\n");

#ifdef __PGSQL__
    run_array(tests,
        (:
            if ($1)
                shutdown(1);
            else
                start_gc(#'shutdown);

            return 0;
        :));
#else
    msg("PostgreSQL support not enabled, skipping test.\n");
    start_gc(#'shutdown);
#endif
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
