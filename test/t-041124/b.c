inherit "/a";
inherit "/c";
#if 1
int sub() {
   debug_message("b::sub()\n"); 
}
#endif
int main() {
    debug_message(sprintf("#'a::sub : %O\n", #'a::sub));
    debug_message(sprintf("#'c::sub : %O\n", #'c::sub));
    debug_message(sprintf("#'::sub : %O\n", #'::sub));
    debug_message(sprintf("#'sub : %O\n", #'sub));
    debug_message("funcall #'a::sub\n");
    funcall(#'a::sub);
    debug_message("funcall #'c::sub\n");
    funcall(#'c::sub);
    debug_message("funcall #'::sub\n");
    funcall(#'::sub);
    debug_message("funcall #'sub\n");
    funcall(#'sub);
    debug_message("\n");
    debug_message("calling a::sub()\n");
    a::sub();
    debug_message("calling c::sub()\n");
    c::sub();
    debug_message("calling sub()\n");
    sub();

    mixed *a = ({ #'a::sub, #'c::sub, #'::sub, #'sub });
    string s = save_value(a);
    mixed *b = restore_value(s);

    debug_message(sprintf("%O -> %O -> %O\n", a, s, b));
}
