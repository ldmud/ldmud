#include "/inc/base.inc"
#include "/inc/gc.inc"

// This test should not cause a crash during the garbage collection.
mapping m;

void finish()
{
      msg("Success. (No Crash)\n");
      shutdown(0);
}

void run_test()
{
    msg("\nRunning test for #0000816...\n"
          "----------------------------\n"
          "Test for crash during compact_mapping()... ");

    mapping m2 = ([clone_object(__FILE__): 1, clone_object(__FILE__)
      :2, 1:2, 2:3, 3:4, 4:5, 5:6, 6:7, 7:8, 8:9, 9:10, 10:11,
      11:12, 12:13,13:14]);
    object* keys = m_indices(m2);
    filter(keys,#'objectp)->remove();
    m=m_reallocate(m2,0);
    efun::garbage_collection();
    call_out(#'finish,3);
}

string *epilog(int eflag)
{
    call_out(#'run_test,0);
    return 0;
}

void remove() {destruct(this_object());}

