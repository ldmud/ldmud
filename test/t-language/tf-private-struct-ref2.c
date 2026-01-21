/* We check refcounting of a private struct by causing a compile error.
 * This is the second try, as the inherit might have two references.
 */
inherit "ti-private-struct3";

int fun()
{
    // Here we force the inclusion of the struct def into our program.
    return sv.member;
}

Abort the compilation here.
