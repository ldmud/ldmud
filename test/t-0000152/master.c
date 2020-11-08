/* Here we check that replace_program() will work with several combinations
 * of virtual inherits. We have the following inherit structure:
 *   f
 *   +-- e
 *       +-- c
 *       |   +-- a
 *       |   +-- b
 *       |   +-- d (optional)
 *       +-- d
 *           +-- a (newer program)
 *           +-- b
 *
 * We try each inherit statement with or without the virtual modifier.
 * The program 'e' will execute a replace program to 'd' and then
 * check that the correct variables survived.
 */

#include "/inc/base.inc"

int testnum;
object* obs;
closure* lfuns;
closure* vars;

object compile_object(string filename)
{
    object ob;

    filename = filename[0..<3]; /* strip ending. */
    copy_file(filename + (find_object("b") ? "2" : "1") + ".c", filename + ".c");
    ob = load_object(filename);
    rm(filename + ".c");

    return ob;
}

void check_objects()
{
    int errors = 0;

    foreach (int i: sizeof(obs))
    {
        msg("Checking object %03d... ", i);
        if (obs[i].test())
            msg("Success.\n");
        else
        {
            errors++;
            msg("FAILURE.\n");
        }

        msg("Checking lfun closure %03d... ", i);
        if (funcall(lfuns[i]) == "A")
            msg("Success.\n");
        else
        {
            errors++;
            msg("FAILURE.\n");
        }

        msg("Checking variable closure %03d... ", i);
        if (funcall(vars[i]) == "A")
            msg("Success.\n");
        else
        {
            errors++;
            msg("FAILURE.\n");
        }
    }

    shutdown(errors && 1);
    return 0;
}

void run_test()
{
    int errors;

    msg("\nRunning test for #0000152:\n"
          "--------------------------\n");

    /* We load all the objects and then wait a backend cycle
     * so the replace_program() can happen.
     */
    msg("Loading objects...\n");
    obs = ({0}) * 256;
    lfuns = ({0}) * 256;
    vars = ({0}) * 256;

    foreach(testnum: 256)
    {
        object d;

        foreach(string ob: ({"c","d","e"}))
            destruct(find_object(ob));

        load_object("c");
        destruct(find_object("a"));
        d = find_object("d");
        if (d)
            destruct(d);

        object ob = load_object("f");
        rename_object(ob, sprintf("f%03d", testnum));
        ob.create();
        obs[testnum] = ob;
        lfuns[testnum] = ob.get_lfun_cl();
        vars[testnum] = ob.get_var_cl();
    }

    call_out(#'check_objects, 0);

}

string *epilog(int eflag)
{
    set_driver_hook(H_AUTO_INCLUDE,
        function string(string base_file, string current_file, int sys_include) :
            string* defines = ({ "TEST_VIRTUAL_C_A", "TEST_VIRTUAL_C_B", "TEST_VIRTUAL_D_A", "TEST_VIRTUAL_D_B", "TEST_VIRTUAL_E_C", "TEST_VIRTUAL_E_D", "TEST_VIRTUAL_F_E" })
        {
            string str = "#pragma strong_types, save_types\n";
            int i = 1;

            foreach (string def: defines)
            {
                str += sprintf("#define %s %s\n", def, (testnum & i) ? "virtual": "");
                i <<= 1;
            }

            /* If TEST_VIRTUAL_E_C is active, then the additional 'd' inherit
             * will be chosen by replace_program(). We don't want that.
             */
            if ((testnum & i) && !(testnum & 16))
                str += "#define PRELOAD_D\n";

            return str;
        });

    run_test();

    return 0;
}
