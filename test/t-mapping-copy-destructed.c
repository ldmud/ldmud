#include "/inc/base.inc"

/* Copying a mapping whose condensed part holds a key referencing
 * a destructed object must not leave a deleted key in the condensed
 * part of the copy. Otherwise copying that copy again leaves a slot
 * of the new condensed block uninitialized, which crashes the driver
 * when the mapping is freed.
 */

#define MOB "/dummy-mapping-copy-destructed"

mapping m;
object a, b, c;

void check()
{
    int errors;

    /* m has been compacted, all entries live in the condensed part. */
    destruct(a);

    mapping c1 = copy(m);
    mapping c2 = copy(c1);
    mapping c3 = copy(c2);

    if (sizeof(c2) != 2 || c2[b] != 2 || c2[c] != 3)
    {
        msg("FAILURE: Wrong content after copying twice.\n");
        errors++;
    }

    destruct(c);

    mapping c4 = copy(c3);
    mapping c5 = copy(c4);

    if (sizeof(c5) != 1 || c5[b] != 2)
    {
        msg("FAILURE: Wrong content after destructing a second key.\n");
        errors++;
    }

    c2 += ([ b: 20 ]);
    if (sizeof(c2) != 1 || c2[b] != 20)
    {
        msg("FAILURE: Wrong content after updating the copy.\n");
        errors++;
    }

    m = c1 = c2 = c3 = c4 = c5 = 0;

    destruct(b);
    rm(MOB ".c");

    if (!errors)
        msg("Success.\n");
    shutdown(errors ? 1 : 0);
}

void run_test()
{
    msg("\nRunning test for copying mappings with destructed keys:\n"
          "-------------------------------------------------------\n");

    write_file(MOB ".c", "int x;\n", 1);

    a = clone_object(MOB);
    b = clone_object(MOB);
    c = clone_object(MOB);
    m = ([ a: 1, b: 2, c: 3 ]);

    /* The cleanup at the start of the garbage collection forces the
     * compaction of the mappings held in variables.
     */
    garbage_collection();
    call_out("check", 0);
}

string *epilog(int eflag)
{
    run_test();
    return 0;
}
