#define OWN_INAUGURATE_MASTER

#include "/inc/base.inc"
#include "/inc/sefun.inc"
#include "/sys/files.h"

/* This test checks that the supplied simul-efun implementations
 * still compile. Also any old functionality tests for the removed
 * efuns are moved here.
 */

void run_test()
{
    msg("\nRunning test for deprecared simul-efun implementations:\n"
          "-------------------------------------------------------\n");

    if (!find_object("/sefun"))
    {
        shutdown(1);
        return;
    }

    // In case of errors.
    call_out(#'shutdown, 1, 1);

    if("/tests"->run_test())
        shutdown(1);
    else
    {
        remove_call_out(#'shutdown);
        shutdown(0);
    }
}

void inaugurate_master (int arg)
{
    set_driver_hook(H_LOAD_UIDS, unbound_lambda(({}), "uid"));
    set_driver_hook(H_CLONE_UIDS, unbound_lambda(({}), "uid"));
    set_driver_hook(H_NOTIFY_FAIL, "");
    set_driver_hook(H_INCLUDE_DIRS, ({"/deprecated/", "/sys/"}));
    set_driver_hook(H_AUTO_INCLUDE,
        function string(string base_file, string current_file, int sys_include)
        {
            // For the simul-efun inherit all files in /deprecated.
            if (base_file == "sefun.c" && !current_file)
            {
                return implode(map(get_dir("/deprecated/*.c", GETDIR_NAMES),
                    function string(string fname)
                    {
                        return "inherit \"/deprecated/" + fname[0..<3] + "\";\n";
                    }), "");
            }

            return "";
        }
    );
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
