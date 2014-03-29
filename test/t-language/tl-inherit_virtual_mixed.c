/* Note that:
 *  - a is inherited non-virtually and virtually
 *    (but first non-virtual in a top-level)
 *    Both variable sets must be distinct.
 *  - Virtual inherit 'd' is both on a top-level
 *    and indirectly inherited.
 *  - 'e' is inherited twice non-virtually,
 *    Both variables must be distinct.
 */

inherit "inh/i_avbcvde";
inherit "inh/a";
virtual inherit "inh/d";
inherit "inh/e";
inherit "inh/i_vcvbva";
inherit "inh/f";

int run_test()
{
    if(abcde_fun() != "a_b_c_d_e" || "a"::a_fun() != "a" || "d"::d_fun() != "d" ||
       "e"::e_fun() != "e" || cba_fun() != "c_b_a" || f_fun() != "f")
        return 0;

    if(abcde_var() != "a_b_c_d_e" || a_var != "a" || d_var != "d" ||
       e_var != "e" || cba_var() != "c_b_a" || f_var != "f")
        return 0;

    if(get_abcde_var() != "a_b_c_d_e" || "a"::get_a_var() != "a" || "d"::get_d_var() != "d" ||
       "e"::get_e_var() != "e" || get_cba_var() != "c_b_a" || get_f_var() != "f")
        return 0;

    "a"::set_a_var("aa");
    "d"::set_d_var("dd");
    "e"::set_e_var("ee");
    "f"::set_f_var("ff");

    if(abcde_var() != "a_b_c_dd_e" || d_var != "dd" || cba_var() != "c_b_a" || f_var != "ff")
        return 0;

    if(get_abcde_var() != "a_b_c_dd_e" || "a"::get_a_var() != "aa"|| "d"::get_d_var() != "dd" ||
       "e"::get_e_var() != "ee" || get_cba_var() != "c_b_a" || get_f_var() != "ff")
        return 0;

    return 1;
}
