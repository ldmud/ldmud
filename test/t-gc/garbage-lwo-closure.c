#pragma lightweight

/* We check that the garbage collector won't crash on debug output,
 * when pursuing references that may point to freed structures.
 */

void run_test()
{
    /* We're building an alternating list of large blocks of various sizes.
     * Half of them will be kept, the other half freed later, so the
     * allocator cannot join them when freeing.
     * We try to provoke the memory allocator to overwrite the freed block
     * with its own memory management structures (AVL tree).
     */
    string *temporary = ({}), *ephemeral = ({});
    foreach (int i: 2000)
    {
        temporary += ({"T"*(i/2)});
        ephemeral += ({"E"*(i/2)});
    }
    set_extra_wizinfo(this_object(), ephemeral);

    /* Create the LWO. */
    new_lwobject(object_name());
    /* Destroy the corresponding program. */
    destruct(this_object());

    /* Clear the trace log of references to the LWO. */
    foreach(int i:10000)
    {
        i+i;
    }
}

closure cl = #'run_test;
int dummy1, dummy2, dummy3, dummy4, dummy5, dummy6; // Make this LWO a large block.
