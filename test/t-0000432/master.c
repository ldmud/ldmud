#include "/sys/configuration.h"
#include "/sys/regexp.h"
#include "/sys/rtlimits.h"
#include "/sys/strings.h"

#include "/inc/base.inc"
#include "/inc/testarray.inc"
#include "/inc/gc.inc"
#include "/inc/deep_eq.inc"

string* errors = ({});

void run_test()
{
    msg("\nRunning tests for #0000432:\n"
          "---------------------------\n");

    run_array(({
        ({ "Byte literals 1", 0,
            (:
                return b"\x00\x01" b"\x02\x03" == to_bytes(({0,1,2,3}));
            :)
        }),
        ({ "Byte literals 2", 0,
            (:
                return b"\xff\xfe" b"\xfd\xfc" == to_bytes(({255,254,253,252}));
            :)
        }),
        ({ "Character literals 1", 0,
           (:
                return '\u270d' == 0x270d;
            :)
        }),
        ({ "Character literals 2", 0,
           (:
                return '\U0001f609' == 0x1f609;
            :)
        }),
        ({ "File encoding 1", 0,
            (:
                object ob = load_object("/iso8859-7.c");
                if (ob->"\u03c1\u03c9\u03c4\u03ae\u03c3\u03c4\u03b5\u005f\u03c7\u03b1\u03b9\u03c1\u03b5\u03c4\u03b9\u03c3\u03bc\u03cc"() != "\u039a\u03b1\u03bb\u03ae\u0020\u03bc\u03ad\u03c1\u03b1\u0021")
                    return 0;
                if (ob->"\u03c1\u03c9\u03c4\u03ae\u03c3\u03c4\u03b5_\u03c7\u03b1\u03c1\u03b1\u03ba\u03c4\u03ae\u03c1\u03b1"() != 960)
                    return 0;
                return 1;
            :)
        }),
        ({ "File encoding 2", 0,
            (:
                object ob = load_object("/utf-8.c");
                if (ob->"\u03c1\u03c9\u03c4\u03ae\u03c3\u03c4\u03b5\u005f\u03c7\u03b1\u03b9\u03c1\u03b5\u03c4\u03b9\u03c3\u03bc\u03cc"() != "\u039a\u03b1\u03bb\u03ae\u0020\u03bc\u03ad\u03c1\u03b1\u0021")
                    return 0;
                if (ob->"\u03c1\u03c9\u03c4\u03ae\u03c3\u03c4\u03b5_\u03c7\u03b1\u03c1\u03b1\u03ba\u03c4\u03ae\u03c1\u03b1"() != 960)
                    return 0;
                return 1;
            :)
        }),
        ({ "File encoding with BoM", 0,
            (:
                object ob = load_object("/utf-8-bom.c");
                if (ob->"\u03c1\u03c9\u03c4\u03ae\u03c3\u03c4\u03b5\u005f\u03c7\u03b1\u03b9\u03c1\u03b5\u03c4\u03b9\u03c3\u03bc\u03cc"() != "\u039a\u03b1\u03bb\u03ae\u0020\u03bc\u03ad\u03c1\u03b1\u0021")
                    return 0;
                if (ob->"\u03c1\u03c9\u03c4\u03ae\u03c3\u03c4\u03b5_\u03c7\u03b1\u03c1\u03b1\u03ba\u03c4\u03ae\u03c1\u03b1"() != 960)
                    return 0;
                return 1;
            :)
        }),
        ({ "Switch with byte strings 1", 0,
            (:
                switch(to_bytes("\u2694\u2699", "utf-8"))
                {
                    case "\u2694\u2699":
                        // Should not match (byte vs. text string)
                        return 0;
                    default:
                        return 1;
                }
            :)
        }),
        ({ "Switch with byte strings 2", 0,
            (:
                switch(to_bytes("\u2694\u2699", "utf-8"))
                {
                    case b"\xe2\x9a\x94\xe2\x9a\x99":
                        return 1;
                    case b"\xe2\x9a\x94\xe2\x9a\x9a":
                        return 0;
                    case b"\xe2\x9a\x94\xe2\x9a":
                        return 0;
                    default:
                        return 0;
                }
            :)
        }),
        ({ "Switch with unicode strings", 0,
            (:
                switch("\u2694" + "\u2699")
                {
                    case "\u2694\u2699":
                        return 1;
                    default:
                        return 0;
                }
            :)
        }),
        ({ "sizeof() for unicode strings", 0,
            (:
                return sizeof("\U00002328") == 1;
            :)
        }),
        ({ "sizeof() for byte sequences", 0,
            (:
                return sizeof(to_bytes("\U00002328", "UTF-8")) == 3;
            :)
        }),
        ({ "String index", 0,
            (:
                string str = "\u2160\u2161\u2162\u2163\u2164";
                return str[1] == 8545;
            :)
        }),
        ({ "Byte index", 0,
            (:
                bytes str = to_bytes("\u2160\u2161\u2162\u2163\u2164", "utf-8");
                return str[3] == 226;
            :)
        }),
        ({ "String reverse index", 0,
            (:
                string str = "\u2160\u2161\u2162\u2163\u2164";
                return str[<2] == 8547;
            :)
        }),
        ({ "String arithmetic index", 0,
            (:
                string str = "\u2160\u2161\u2162\u2163\u2164";
                return str[>-2] == 8547;
            :)
        }),
        ({ "String range", 0,
            (:
                string str = "\U0001f0a1\U0001f0b1\U0001f0c1\U0001f0d1";
                return str[1..2] == "\U0001f0b1\U0001f0c1";
            :)
        }),
        ({ "Char lvalue 1", 0,
            (:
                string str = "\U0001f60b\U0001f62f\U0001f60e";
                str[1] = '-';
                return str == "\U0001f60b-\U0001f60e";
            :)
        }),
        ({ "Char lvalue 2", 0,
            (:
                string str = "ABC";
                str[1] = 128520;
                return str == "A\U0001f608C";
            :)
        }),
        ({ "Char lvalue 3", 0,
            (:
                string str = " " * 100;
                str[0] = 8230;
                return str == "\u2026" + " " * 99;
            :)
        }),
        ({ "Char lvalue increment", 0,
            (:
                string str = "A\u007fA";
                if (str[1]++ != 0x7f)
                    return 0;
                return str == "A\u0080A";
            :)
        }),
        ({ "Char lvalue decrement", 0,
            (:
                string str = "A\u0080A";
                if (str[1]-- != 0x80)
                    return 0;
                return str == "A\u007fA";
            :)
        }),
        ({ "Char lvalue addition", 0,
            (:
                string str = "ABC";
                str[1] += 9333;
                return str == "A\u24b7C";
            :)
        }),
        ({ "Char lvalue subtraction", 0,
            (:
                string str = "A\u24b7C";
                str[1] -= 9333;
                return str == "ABC";
            :)
        }),
        ({ "String difference", 0,
            (:
                return ("\u2669\u266a\u266b\u2669" - "\u2669") == "\u266a\u266b";
            :)
        }),
        ({ "String intersection", 0,
            (:
                return ("\u2669\u266a\u266b\u2669" & "\u266b\u2669") == "\u2669\u266b\u2669";
            :)
        }),
        ({ "String vs. Byte comparison", 0,
            (:
                mixed abc = "ABC";
                return abc != to_bytes("ABC", "ASCII");
            :)
        }),
        ({ "String range lvalue 1", 0,
            (:
                string str = "\U0001f1e6\U0001f1e7\U0001f1e8\U0001f1e9\U0001f1ea\U0001f1eb";
                str[2..3] = "CD";
                return str == "\U0001f1e6\U0001f1e7CD\U0001f1ea\U0001f1eb";
            :)
        }),
        ({ "String range lvalue 2", 0,
            (:
                string str = "Music";
                str[1..2] = "\u266a\u266b";
                return str == "M\u266a\u266bic";
            :)
        }),
        ({ "String range lvalue 3", 0,
            (:
                string str = " " * 100;
                str[1..2] = "\u266a\u266b";
                return str == " \u266a\u266b" + " " * 97;
            :)
        }),
        ({ "Protected char lvalue 1", 0,
            (:
                string str = "\U0001f60b\U0001f62f\U0001f60e";
                mixed ch = &(str[1]);
                if (ch != 0x1f62f)
                    return 0;

                ch = '-';
                return str == "\U0001f60b-\U0001f60e";
            :)
        }),
        ({ "Protected char lvalue 2", 0,
            (:
                string str = "ABC";
                mixed ch = &(str[1]);
                if (ch != 'B')
                    return 0;

                ch = 128520;
                return str == "A\U0001f608C";
            :)
        }),
        ({ "Protected char lvalue 3", 0,
            (:
                string str = "\U0001f60b\U0001f62f\U0001f60e";
                mixed ch1 = &(str[1]);
                mixed ch2 = &(str[1]);
                ch1 = '-';
                return ch2 == '-' && str == "\U0001f60b-\U0001f60e";
            :)
        }),
        ({ "Protected char lvalue 4", 0,
            (:
                string str = "ABC";
                mixed ch1 = &(str[1]);
                mixed ch2 = &(str[1]);
                ch1 = 128520;
                return ch2 == 128520 && str == "A\U0001f608C";
            :)
        }),
        ({ "Protected char lvalue 5", 0,
            (:
                string str = " " * 100;
                mixed ch1 = &(str[0]);
                mixed ch2 = &(str[0]);
                ch1 = 8230;
                return str == "\u2026" + " " * 99 && ch2 == 8230;
            :)
        }),
        ({ "Protected string range lvalue 1", 0,
            (:
                string str = "\U0001f1e6\U0001f1e7\U0001f1e8\U0001f1e9\U0001f1ea\U0001f1eb";
                string range = &(str[2..3]);
                if (range != "\U0001f1e8\U0001f1e9")
                    return 0;

                range = "CD";
                return str == "\U0001f1e6\U0001f1e7CD\U0001f1ea\U0001f1eb";
            :)
        }),
        ({ "Protected string range lvalue 2", 0,
            (:
                string str = "Music";
                string range = &(str[1..2]);
                if (range != "us")
                    return 0;

                range = "\u266a\u266b";
                return str == "M\u266a\u266bic";
            :)
        }),
        ({ "Protected string range lvalue 3", 0,
            (:
                string str = "\U0001f1e6\U0001f1e7\U0001f1e8\U0001f1e9\U0001f1ea\U0001f1eb";
                string range1 = &(str[2..3]);
                string range2 = &(str[2..3]);
                range1 = "CD";
                return range2 == "CD" && str == "\U0001f1e6\U0001f1e7CD\U0001f1ea\U0001f1eb";
            :)
        }),
        ({ "Protected string range lvalue 4", 0,
            (:
                string str = "Music";
                string range1 = &(str[1..2]);
                string range2 = &(str[1..2]);
                range1 = "\u266a\u266b";
                return range2 == "\u266a\u266b" && str == "M\u266a\u266bic";
            :)
        }),
        ({ "Protected tring range lvalue 5", 0,
            (:
                string str = " " * 100;
                string range1 = &(str[1..2]);
                string range2 = &(str[1..2]);
                range1 = "\u266a\u266b";
                return str == " \u266a\u266b" + " " * 97 && range2 == "\u266a\u266b";
            :)
        }),
        ({ "Foreach over string", 0,
            (:
                int sum, num;
                foreach(int ch: "\u24f5\u24f6\u24f7\u24f8\u24f9")
                {
                    sum+=ch;
                    num++;
                }

                return num == 5 && sum == 47315;
            :)
        }),
        ({ "Foreach over string reference 1", 0,
            (:
                string str = "\u24f5\u24f6\u24f7\u24f8\u24f9";
                foreach(int ch: &str)
                    ch -= 9412;

                return str == "12345";
            :)
        }),
        ({ "Foreach over string reference 2", 0,
            (:
                string str = "12345";
                foreach(int ch: &str)
                    ch += 9412;

                return str == "\u24f5\u24f6\u24f7\u24f8\u24f9";
            :)
        }),
        ({ "Reverse", 0,
            (:
                return reverse("\u24b6\u24b7\u24b8\u24b9\u24ba\u24bb") == "\u24bb\u24ba\u24b9\u24b8\u24b7\u24b6";
            :)
        }),
        ({ "Reverse with reference", 0,
            (:
                string str = "\u24b6\u24b7\u24b8\u24b9\u24ba\u24bb";
                return reverse(&(str[1..3])) == "\u24b9\u24b8\u24b7" && str == "\u24b6\u24b9\u24b8\u24b7\u24ba\u24bb";
            :)
        }),
        ({ "save_value & restore_value with unicode string", 0,
            (:
                return deep_eq(
                    to_array(restore_value(save_value("\U0001f634"))),
                    ({ 128564 }));
            :)
        }),
        ({ "save_value & restore_value with byte string 1", 0,
            (:
                return deep_eq(
                    to_array(restore_value(save_value(to_bytes("\U0001f634", "UTF-8")))),
                    ({ 0xF0, 0x9F, 0x98, 0xB4 }));
            :)
        }),
        ({ "save_value & restore_value with byte string 2", 0,
            (:
                string result = save_value(to_bytes(({0, 34, 92, 0, 240})));
                if (!sizeof(result))    // Should not throw an error.
                    return 0;

                return deep_eq(to_array(restore_value(result)), ({0, 34, 92, 0, 240}));
            :)
        }),
#if 1 // These tests are dependant of the locale.
        ({ "capitalize", 0,
            (:
                return capitalize("\u00e4gyptisch") == "\u00c4gyptisch";
            :)
        }),
        ({ "lower_case", 0,
            (:
                return lower_case("\u00c4\u00d6\u00dc") == "\u00e4\u00f6\u00fc";
            :)
        }),
        ({ "upper_case", 0,
            (:
                return upper_case("\u00e4\u00f6\u00fc") == "\u00c4\u00d6\u00dc";
            :)
        }),
#endif
        ({
            "add_action 1", 0,
            (:
                mixed** result;

                efun::configure_object(this_object(), OC_COMMANDS_ENABLED, 1);
                efun::set_this_player(this_object());

                add_action((: 0 :), "\u03bb\u03b4\u03bc\u03c5\u03b4", -2);
                result = match_command("\u03bb\u03b4", this_object());

                efun::configure_object(this_object(), OC_COMMANDS_ENABLED, 0);

                return sizeof(result) == 1;
            :)
        }),
        ({
            "add_action 2", 0,
            (:
                mixed** result;

                efun::configure_object(this_object(), OC_COMMANDS_ENABLED, 1);
                efun::set_this_player(this_object());

                add_action((: 0 :), "\u03bb\u03b4\u03bc\u03c5\u03b4", -2);
                result = match_command("\u03bb", this_object());

                efun::configure_object(this_object(), OC_COMMANDS_ENABLED, 0);

                return sizeof(result) == 0;
            :)
        }),
        ({
            "add_action 3", 0,
            (:
                string error;

                efun::configure_object(this_object(), OC_COMMANDS_ENABLED, 1);
                efun::set_this_player(this_object());

                error = catch(add_action((: 0 :), "\u03bb\u03b4\u03bc\u03c5\u03b4", -6));

                efun::configure_object(this_object(), OC_COMMANDS_ENABLED, 0);

                return error != 0;
            :)
        }),
        ({ "explode with unicode strings 1", 0,
            (:
                return deep_eq(explode("\u2745\u2744\u2746\u2744\u2745\u2744", "\u2744"),
                    ({ "\u2745", "\u2746", "\u2745", "" }));
            :)
        }),
        ({ "explode with unicode strings 2", 0,
            (:
                return deep_eq(explode("\u2745\u2744\u2746", ""),
                    ({ "\u2745", "\u2744", "\u2746" }));
            :)
        }),
        ({ "explode with bytes", 0,
            (:
                return deep_eq(explode(to_bytes("\u2745\u2744","UTF-8"), to_bytes(({0xe2}))),
                    ({ to_bytes(({})), to_bytes(({0x9d, 0x85})), to_bytes(({0x9d, 0x84})) }));
            :)
        }),
        ({ "implode with unicode strings", 0,
            (:
                return implode(({ "\u2745", "\u2746", "\u2745", "" }), "\u2744") ==
                    "\u2745\u2744\u2746\u2744\u2745\u2744";
            :)
        }),
        ({ "implode with bytes", 0,
            (:
                return implode(({ to_bytes(({})), to_bytes(({0x9d, 0x85})), to_bytes(({0x9d, 0x84})) }), to_bytes(({0xe2}))) ==
                        to_bytes("\u2745\u2744","UTF-8");
            :)
        }),
        ({ "map widening characters 1", 0,
            (:
                string str = map("LDMud", #'+, 0xfee0);
                return sizeof(str) == 5 && str == "\uff2c\uff24\uff2d\uff55\uff44";
            :)
        }),
        ({ "map widening characters 2", 0,
            (:
                string str = map("HL", (['H': 0x210c, 'L': 0x2112]));
                return sizeof(str) == 2 && str == "\u210c\u2112";
            :)
        }),
        ({ "map narrowing characters 1", 0,
            (:
                string str = map("\uff2c\uff24\uff2d\uff55\uff44", #'-, 0xfee0);
                return sizeof(str) == 5 && str == "LDMud";
            :)
        }),
        ({ "map narrowing characters 2", 0,
            (:
                string str = map("\u210c\u2112", ([ 0x210c: 'H', 0x2112: 'L']));
                return sizeof(str) == 2 && str == "HL";
            :)
        }),
        ({ "map with bytes 1", 0,
            (:
                bytes str = map(b"LDMud", #'+, 128);
                return sizeof(str) == 5 && str == b"\xcc\xc4\xcd\xf5\xe4";
            :)
        }),
        ({ "map with bytes 2", 0,
            (:
                bytes str = map(b"\xcc\xc4\xcd\xf5\xe4", #'-, 128);
                return sizeof(str) == 5 && str == b"LDMud";
            :)
        }),
        ({ "map with bytes 3", 0,
            (:
                bytes str = map(b"\x20\x40\x60\x80\xa0\xc0\xe0",
                                ([ 0x20: 0xf0, 0x40: 0xd0, 0x60: 0xb0, 0x80: 0x90, 0xa0: 0x70, 0xc0: 0x50, 0xe0: 0x30 ]));
                return str == b"\xf0\xd0\xb0\x90\x70\x50\x30";
            :)
        }),
        ({ "filter with unicode strings 1", 0,
            (:
                return filter("\u0417\u0434\u0440\u0430\u0432\u0441\u0442\u0432\u0443\u0439, LDMud!", function int(int ch)
                {
                    return ch >= 0x400 && ch < 0x500;
                }) == "\u0417\u0434\u0440\u0430\u0432\u0441\u0442\u0432\u0443\u0439";
            :)
        }),
        ({ "filter with unicode strings 2", 0,
            (:
                return filter("123 \u2460\u2461\u2462", ([ '1', 0x2461 ])) == "1\u2461";
            :)
        }),
        ({ "filter with bytes 1", 0,
            (:
                return filter(b"-> \xcc\xc4\xcd\xf5\xe4 <-", function int(int ch)
                {
                    return ch < 128;
                }) == b"->  <-";
            :)
        }),
        ({ "filter with bytes 2", 0,
            (:
                return filter(b"\x20\x40\x60\x80\xa0\xc0\xe0", ([ 0x20, 0x60, 0xa0, 0xe0 ])) == b"\x20\x60\xa0\xe0";
            :)
        }),
        ({ "member with unicode strings 1", 0,
            (:
                return member("\u2160\u2161\u2162\u2163\u2164\u2165\u2166\u2167\u2168\u2169\u216a\u216b", 8548) == 4;
            :)
        }),
        ({ "member with unicode strings 2", 0,
            (:
                return member("\0\u2160\u2161\u2162\u2163\u2164\u2165\u2166\u2167\u2168\u2169\u216a\u216b", 8548) == 5;
            :)
        }),
        ({ "member with unicode strings 3", 0,
            (:
                return member("\u2160\u2161\u2162\u2163\u2164\u2165\u2166\u2167\u2168X\u216a\u216b", 'X') == 9;
            :)
        }),
        ({ "member with byte strings", 0,
            (:
                return member(to_bytes(({240,241,242,243,244,245,246,247,248,249,250})), 245) == 5;
            :)
        }),
        ({ "rmember with unicode strings 1", 0,
            (:
                return rmember("\u2160\u2161\u2162\u2163\u2164\u2165\u2166\u2167\u2168\u2169\u216a\u216b", 8548) == 4;
            :)
        }),
        ({ "rmember with unicode strings 2", 0,
            (:
                return rmember("\0\u2160\u2161\u2162\u2163\u2164\u2165\u2166\u2167\u2168\u2169\u216a\u216b\0", 8548) == 5;
            :)
        }),
        ({ "rmember with unicode strings 3", 0,
            (:
                return rmember("\u2160\u2161\u2162\u2163\u2164\u2165\u2166\u2167\u2168X\u216a\u216b", 'X') == 9;
            :)
        }),
        ({ "rmember with byte strings", 0,
            (:
                return rmember(to_bytes(({240,241,242,243,244,245,246,247,248,249,250})), 245) == 5;
            :)
        }),
        ({ "regmatch with PCRE and unicode strings 1", 0,
            (:
                return regmatch("Sch\u00f6ne \u00c4ra", "[[:upper:]]ra", RE_PCRE) == "\u00c4ra";
            :)
        }),
        ({ "regmatch with PCRE and unicode strings 2", 0,
            (:
                return regmatch("\uff2c\uff24\uff2d\uff55\uff44", "\uff2c*", RE_PCRE) == "\uff2c";
            :)
        }),
        ({ "regmatch with builtin regexp and unicode strings 1", 0,
            (:
                return regmatch("Sch\u00f6ne \u00c4ra", " \\<.ra", RE_TRADITIONAL) == " \u00c4ra";
            :)
        }),
        ({ "regmatch with builtin regexp and unicode strings 2", 0,
            (:
                return regmatch("Sch\u00f6ne \u00c4ra", "[\u00c4\u00d6\u00dc]ra", RE_TRADITIONAL) == "\u00c4ra";
            :)
        }),
        ({ "regmatch with builtin regexp and unicode strings 3", 0,
            (:
                return regmatch("\uff2c\uff24\uff2d\uff55\uff44", "\uff2c*", RE_TRADITIONAL) == "\uff2c";
            :)
        }),
        ({ "regmatch with builtin regexp and unicode strings 4", 0,
            (:
                return regmatch("\uff2c\uff24\uff2d\uff55\uff44", "\uff2c*\uff2c", RE_TRADITIONAL) == "\uff2c";
            :)
        }),
        ({ "regmatch with builtin regexp and grapheme clusters", 0,
            (:
                foreach(string cluster: ({
                    "A\u0308",
                    "\U0001f600\ufe0f",
                    "\U0001f3f3\ufe0f\u200d\U0001f308",
                    "\U0001F1E9\U0001F1EA",
                    "\u1100\u1161\u11ab",
                    "\u0e01\u0e33",
                }))
                {
                    if (regmatch(3 * cluster, "^\\X", RE_TRADITIONAL) != cluster)
                        return 0;
                }

                return 1;
            :)
        }),
        ({ "regreplace with builtin regexp and unicode strings 1", 0,
            (:
                return regreplace("\u216c\u216e\u216f\u2164\u216e", "(.)", " \\1 ", RE_TRADITIONAL | RE_GLOBAL) == " \u216c  \u216e  \u216f  \u2164  \u216e ";
            :)
        }),
        ({ "regreplace with builtin regexp and unicode strings 2", 0,
            (:
                return regreplace("\u216c\u216e\u216f\u2164\u216e", "", "*", RE_TRADITIONAL | RE_GLOBAL) == "*\u216c*\u216e*\u216f*\u2164*\u216e*";
            :)
        }),
        ({ "regreplace with builtin regexp and unicode strings 3", 0,
            (:
                return regreplace("a\u00e5", "[a\u00e4]*", "", RE_TRADITIONAL | RE_GLOBAL) == "\u00e5";
            :)
        }),
        ({ "regreplace with PCRE and unicode strings 1", 0,
            (:
                return regreplace("\u216c\u216e\u216f\u2164\u216e", "(.)", " \\1 ", RE_PCRE | RE_GLOBAL) == " \u216c  \u216e  \u216f  \u2164  \u216e ";
            :)
        }),
        ({ "regreplace with PCRE and unicode strings 2", 0,
            (:
                return regreplace("\u216c\u216e\u216f\u2164\u216e", "", "*", RE_PCRE | RE_GLOBAL) == "*\u216c*\u216e*\u216f*\u2164*\u216e*";
            :)
        }),
        ({ "regexplode with builtin regexp and unicode strings 1", 0,
            (:
                return deep_eq(regexplode("\u216c\u216e\u216f\u2164\u216e", ".", RE_TRADITIONAL), ({"", "\u216c", "", "\u216e", "", "\u216f", "", "\u2164", "", "\u216e", ""}));
            :)
        }),
        ({ "regexplode with builtin regexp and unicode strings 2", 0,
            (:
                return deep_eq(regexplode("a\u00e5", "[a\u00e4]+", RE_TRADITIONAL), ({"", "a", "\u00e5"}));
            :)
        }),
        ({ "regexplode with builtin regexp and unicode strings 3", 0,
            (:
                return deep_eq(regexplode("Sch\u00f6ne \u00c4ra", "\\<", RE_TRADITIONAL), ({"", "", "Sch\u00f6ne ", "", "\u00c4ra"}));
            :)
        }),
        ({ "regexplode with builtin regexp and unicode strings 4", 0,
            (:
                return deep_eq(regexplode("Sch\u00f6ne \u00c4ra", "\\>", RE_TRADITIONAL), ({"Sch\u00f6ne", "", " \u00c4ra", "", ""}));
            :)
        }),
        ({ "regexplode with builtin regexp and grapheme clusters", 0,
            (:
                foreach(string cluster: ({
                    "A\u0308",
                    "\U0001f600\ufe0f",
                    "\U0001f3f3\ufe0f\u200d\U0001f308",
                    "\U0001F1E9\U0001F1EA",
                    "\u1100\u1161\u11ab",
                    "\u0e01\u0e33",
                }))
                {
                    foreach(int i: 3)
                    {
                        if (!deep_eq(regexplode(i * cluster, "\\X", RE_TRADITIONAL) - ({""}), i*({cluster})))
                            return 0;
                    }
                }

                return 1;
            :)
        }),
        ({ "regexplode with PCRE and unicode strings 1", 0,
            (:
                return deep_eq(regexplode("\u216c\u216e\u216f\u2164\u216e", ".", RE_PCRE), ({"", "\u216c", "", "\u216e", "", "\u216f", "", "\u2164", "", "\u216e", ""}));
            :)
        }),
        ({ "regexplode with PCRE and unicode strings 2", 0,
            (:
                return deep_eq(regexplode("a\u00e5", "[a\u00e4]+", RE_PCRE), ({"", "a", "\u00e5"}));
            :)
        }),
        ({ "regexplode with PCRE and unicode strings 3", 0,
            (:
                return deep_eq(regexplode("Sch\u00f6ne \u00c4ra", "\\b", RE_PCRE), ({"", "", "Sch\u00f6ne", "", " ", "", "\u00c4ra", "", ""}));
            :)
        }),
        ({ "sprintf with unicode string", 0,
            (:
                return sprintf("%Q", "\u2615") == "\"\\u2615\"";
            :)
        }),
        ({ "sprintf with unicode string range", 0,
            (:
                string str = "\u201e\u2135\u201d";
                return sprintf("%Q", &(str[1..1])) == "\"\\u2135\"";
            :)
        }),
        ({ "sprintf with unicode char lvalue", 0,
            (:
                string str = "\u201e\u2135\u201d";
                return sprintf("%Q", &(str[1])) == "'\u2135' (8501)";
            :)
        }),
        ({ "sprintf with bytes", 0,
            (:
                return sprintf("%Q", to_bytes("\u2615", "UTF-8")) == "\"\\xe2\\x98\\x95\"";
            :)
        }),
        ({ "sprintf with byte range", 0,
            (:
                bytes str = to_bytes("\u201e\u2135\u201d", "UTF-8");
                return sprintf("%Q", &(str[3..5])) == "\"\\xe2\\x84\\xb5\"";
            :)
        }),
        ({ "sprintf with byte lvalue", 0,
            (:
                bytes str = to_bytes("\u201e\u2135\u201d", "UTF-8");
                return sprintf("%Q", &(str[3])) == "226";
            :)
        }),
        ({ "sprintf with an unicode character", 0,
            (:
                return sprintf("%c", 9774) == "\u262e";
            :)
        }),
        ({ "sprintf chopping unicode strings", 0,
            (:
                return sprintf("%.10s", "\u266b"*20) == "\u266b"*10;
            :)
        }),
        ({ "sprintf table with unicode strings", 0,
            (:
                return sprintf("%6#s\n", "\u2669\n"*10) == "  \u2669  \u2669\n"*5;
            :)
        }),
        ({ "sprintf wrapping unicode characters", 0,
            (:
                return sprintf("%-=19s\n", "\u266a"*20) == "\u266a"*19 + "\n\u266a\n";
            :)
        }),
        ({ "sprintf chopping unicode grapheme clusters 1", 0,
            (:
                return sprintf("%.10s", "\e[34mA\u0308\e[0m"*20) == "\e[34mA\u0308\e[0m"*10+"\e[34m";
            :)
        }),
        ({ "sprintf chopping unicode grapheme clusters 2", 0,
            (:
                return sprintf("%.5s", "\U0001f600\ufe0f"*20) == "\U0001f600\ufe0f"*2;
            :)
        }),
        ({ "sprintf wrapping unicode grapheme clusters", 0,
            (:
                foreach(string cluster: ({
                    "A\u0308",
                    "\U0001f600\ufe0f",
                    "\U0001f3f3\ufe0f\u200d\U0001f308",
                    "\U0001F1E9\U0001F1EA",
                    "\u1100\u1161\u11ab",
                    "\u0e01\u0e33",
                }))
                {
                    foreach(int i: 3*sizeof(cluster))
                    {
                        // Cluster must stay in one piece
                        if (sizeof(map(explode(sprintf("%-=*s", i+1, 20*cluster), cluster), #'trim) - ({cluster, "\n", ""})))
                            return 0;
                    }
                }
                return 1;
            :)
        }),
        ({ "sprintf correctly calculating column sizes 1", 0,
            (:
                foreach(string cluster: ({
                    "A\u0308",
                    "\U0001f600\ufe0f",
                    "\U0001f3f3\ufe0f\u200d\U0001f308",
                    "\U0001F1E9\U0001F1EA",
                    "\u1100\u1161\u11ab",
                    "\u0e01\u0e33",
                }))
                {
                    if (sprintf("%=-3@s\n", ({ "ABC " + cluster, "123 456" })) != "ABC123\n" + cluster + (" " * (3-text_width(cluster))) + "456\n")
                        return 0;
                }
                return 1;
            :)
        }),
        ({ "sprintf correctly calculating column sizes 2", 0,
            (:
                foreach(int spaces: 14)
                {
                    if (sprintf("%*s%s\n", 3 + spaces, "\e[34m\e[1mABC\e[0m", "123") != spaces * " " + "\e[34m\e[1mABC\e[0m123\n")
                        return 0;
                    if (sprintf("%-*s%s\n", 3 + spaces, "\e[34m\e[1mABC\e[0m", "123") != "\e[34m\e[1mABC\e[0m" + spaces * " " + "123\n")
                        return 0;
                }
                return 1;
            :)
        }),
        ({ "sprintf with unicode padding", 0,
            (:
                return sprintf("%7'\U0001F608's", "X") == "\U0001F608\U0001F608\U0001F608X";
            :)
        }),
        ({ "terminal_colour wrapping unicode characters 1", 0,
            (:
                return terminal_colour("\u266a"*20+"\n", 0, 19, 5) == "\u266a"*19 + "\n     \u266a\n";
            :)
        }),
// Tests for the maximum string size of terminal_colour().
#define TERMINAL_COLOUR_MAX 200000
        ({ "terminal_colour wrapping unicode characters 2", 0,
            (:
                return terminal_colour("\u266b "*TERMINAL_COLOUR_MAX, 0, TERMINAL_COLOUR_MAX/2 - 500, 2000) == "\u266b " * (TERMINAL_COLOUR_MAX/4 - 250);
            :)
        }),
        ({ "terminal_colour wrapping unicode characters 3", 0,
            (:
                return terminal_colour("\u266b "*TERMINAL_COLOUR_MAX, 0, TERMINAL_COLOUR_MAX/4 + 250, 2000) == "\u266b " * (TERMINAL_COLOUR_MAX/8 + 124) + "\u266b\n" + " " * 2000 + "\u266b " * (TERMINAL_COLOUR_MAX/8 - 875);
            :)
        }),
        ({ "terminal_colour trimming unicode characters 1", 0,
            (:
                return sizeof(terminal_colour("%^X%^" + "\u266b"*TERMINAL_COLOUR_MAX, ([0: ""]), 0, 0)) == TERMINAL_COLOUR_MAX/3;
            :)
        }),
        ({ "terminal_colour trimming unicode characters 2", 0,
            (:
                return sizeof(terminal_colour("\u266b"*TERMINAL_COLOUR_MAX, 0, TERMINAL_COLOUR_MAX/4, 0)) == 1 + TERMINAL_COLOUR_MAX/3;
            :)
        }),
        ({ "terminal_colour wrapping unicode grapheme clusters", 0,
            (:
                foreach(string cluster: ({
                    "A\u0308",
                    "\U0001f600\ufe0f",
                    "\U0001f3f3\ufe0f\u200d\U0001f308",
                    "\U0001F1E9\U0001F1EA",
                    "\u1100\u1161\u11ab",
                    "\u0e01\u0e33",
                }))
                {
                    foreach(int i: 3*sizeof(cluster))
                    {
                        // Cluster must stay in one piece
                        if (sizeof(map(explode(terminal_colour(20*cluster, 0, i+1), cluster), #'trim) - ({cluster, "\n", ""})))
                            return 0;
                    }
                }
                return 1;
            :)
        }),
        ({ "strstr 1", 0,
            (:
                return strstr("\u263a\u263b\u263b\u263a", "\u263b", 1) == 1;
            :)
        }),
        ({ "strstr 2", 0,
            (:
                return strstr("\u263a\u263b\u263b\u263a", "\u263b", 2) == 2;
            :)
        }),
        ({ "strrstr 1", 0,
            (:
                return strrstr("\u263a\u263b\u263b\u263a", "\u263b", 1) == 1;
            :)
        }),
        ({ "strrstr 2", 0,
            (:
                return strrstr("\u263a\u263b\u263b\u263a", "\u263b", -1) == 2;
            :)
        }),
        ({ "trim 1", 0,
            (:
                return trim("\u22ef\u22ef\u22f0\u22f1\u22ef\u22ef", TRIM_BOTH, "\u22ef") == "\u22f0\u22f1";
            :)
        }),
        ({ "trim 2", 0,
            (:
                return trim("\u22ef\u22ef\u22f0\u22f1\u22ef\u22ef", TRIM_BOTH, 0x22ef) == "\u22f0\u22f1";
            :)
        }),
        ({ "trim 3", 0,
            (:
                return trim(" \U00010990 ") == "\U00010990";
            :)
        }),
        ({ "regreplace 1", 0,
            (:
                string result = regreplace("*\u00c4*\u00d6*\u00dc*","[[:alpha:]]", "", RE_PCRE|RE_GLOBAL);
                return sizeof(result) == 4 && result == "****";
            :)
        }),
        ({ "regreplace 2", 0,
            (:
                string result = regreplace("*\u00c4*\u00d6*\u00dc*","[^[:alpha:]]", "", RE_PCRE|RE_GLOBAL);
                return sizeof(result) == 3 && result == "\u00c4\u00d6\u00dc";
            :)
        }),
        ({ "regreplace 3", 0,
            (:
                string result = regreplace("*\u00c4*\u00d6*\u00dc*","[\u00c4\u00d6\u00dc]", "", RE_TRADITIONAL|RE_GLOBAL);
                return sizeof(result) == 4 && result == "****";
            :)
        }),
        ({ "regreplace 4", 0,
            (:
                string result = regreplace("*\u00c4*\u00d6*\u00dc*","[^\u00c4\u00d6\u00dc]", "", RE_TRADITIONAL|RE_GLOBAL);
                return sizeof(result) == 3 && result == "\u00c4\u00d6\u00dc";
            :)
        }),
        ({ "read_bytes", 0,
            (:
                return read_bytes("/data.bin", 0, 7) == to_bytes("\u269d\U0001f44d", "UTF-8");
            :)
        }),
        ({ "read_file 1", 0,
            (:
                return read_file("/data.bin", 2, 1, "UTF-8") == "\u2328\u231b\n";
            :)
        }),
        ({ "read_file 2", TF_ERROR,
            (:
                return read_file("/data.bin", 2, 1, "ascii");
            :)
        }),
        ({ "read_file 3", 0,
            (:
                int success;

                write_file("/test.bin", "", 1);
                write_bytes("/test.bin", 0, to_bytes(("\u00a0"*100+"\n")*10, "ISO8859-15"));
                success = limited((:
                    return read_file("/test.bin", 5, 1, "ISO8859-15") == "\u00a0"*100+"\n";
                :), LIMIT_FILE, 110);

                rm("/test.bin");
                return success;
            :)
        }),
        ({ "read_file 4", 0,
            (:
                int success;

                write_file("/test.bin", "", 1);
                write_bytes("/test.bin", 0, to_bytes(("\u00a0"*100+"\n")*10, "ISO8859-15"));
                success = limited((:
                    return read_file("/test.bin", 5, 1, "ISO8859-15") == 0;
                :), LIMIT_FILE, 90);

                rm("/test.bin");
                return success;
            :)
        }),
        ({ "read_file 5", 0,
            (:
                int success;

                write_file("/test.bin", "", 1);
                write_bytes("/test.bin", 0, to_bytes(("@"*100+"\n")*10, "UTF-32"));
                success = limited((:
                    return read_file("/test.bin", 5, 1, "UTF-32") == "@"*100+"\n";
                :), LIMIT_FILE, 110);

                rm("/test.bin");
                return success;
            :)
        }),
        ({ "read_file 6", 0,
            (:
                int success;

                write_file("/test.bin", "", 1);
                write_bytes("/test.bin", 0, to_bytes(("@"*100+"\n")*10, "UTF-32"));
                success = limited((:
                    return read_file("/test.bin", 5, 1, "UTF-32") == 0;
                :), LIMIT_FILE, 90);

                rm("/test.bin");
                return success;
            :)
        }),
        ({ "write_bytes", 0,
            (:
                bytes str;

                write_file("/test.bin", "", 1);
                write_bytes("/test.bin", 0, to_bytes(({0,1,2,3,4,5})));
                str = read_bytes("/test.bin");
                rm("/test.bin");

                return str == to_bytes(({0,1,2,3,4,5}));
            :)
        }),
        ({ "write_file 1", 0,
            (:
                string str;

                write_file("/test.txt", "\u2620\u2623\n", 1, "UTF-8");
                str = read_file("/test.txt", 0, 0, "UTF-8");
                rm("/test.txt");

                return str == "\u2620\u2623\n";
            :)
        }),
        ({ "write_file 2", 0,
            (:
                string str;

                write_file("/test.txt", "\u2620\u2623\n", 1, "UTF-32");
                str = read_file("/test.txt", 0, 0, "UTF-32");
                rm("/test.txt");

                return str == "\u2620\u2623\n";
            :)
        }),
        ({ "write_file 3", 0,
            (:
                string str;

                write_file("/test.txt", "@"*100+"\n", 1, "UTF-32");
                str = read_file("/test.txt", 0, 0, "UTF-32");
                rm("/test.txt");

                return str == "@"*100+"\n";
            :)
        }),
        ({ "write_file 4", 0,
            (:
                string str;

                write_file("/test.txt", "\u00a0"*100+"\n", 1, "ISO8859-15");
                str = read_file("/test.txt", 0, 0, "ISO8859-15");
                rm("/test.txt");

                return str == "\u00a0"*100+"\n";
            :)
        }),
#ifdef __SQLITE__
        ({ "SQLite Unicode", 0,
            (:
                string str = "\u00af\\_(\u30c4)_/\u00af";
                int success;

                sl_open("/test.db");
                sl_exec("CREATE TABLE test(info TEXT)");
                sl_exec("INSERT INTO test(info) VALUES (?)", str);

                success = deep_eq(sl_exec("SELECT * FROM test"), ({ ({ str }) }));
                sl_close();
                rm("/test.db");
                return success;
            :)
        }),
        ({ "SQLite Blob", 0,
            (:
                bytes b = to_bytes(({1,2,3,4,5,6,7,8,9,10}));
                int success;

                sl_open("/test.db");
                sl_exec("CREATE TABLE test(info BLOB)");
                sl_exec("INSERT INTO test(info) VALUES (?)", b);

                success = deep_eq(sl_exec("SELECT * FROM test"), ({ ({ b }) }));
                sl_close();
                rm("/test.db");
                return success;
            :)
        }),
#endif
#ifdef __XML_DOM__
        ({ "XML with Unicode", 0,
            (:
                mixed val = ({ "\u00c4", ([ "\u00e4": "\u266b\u266a" ]), 0 });
                return deep_eq(xml_parse(xml_generate(val)), val);
            :)
        }),
#endif
        ({ "Illegal characters in UTF-8 sequences", TF_ERROR,
            (:
                to_text(({0xf8,0x80,0x80,0x80,0x80}), "UTF-8");
            :)
        }),
        ({ "Illegal characters with to_text(array)", TF_ERROR,
            (:
                to_text(({0x110000}));
            :)
        }),
        ({ "Illegal characters with to_string(array)", TF_ERROR,
            (:
                to_string(({0x110000}));
            :)
        }),
        ({ "Illegal characters with sprintf(\"%c\")", TF_ERROR,
            (:
                sprintf("%c", 0x110000);
            :)
        }),
        ({ "Lexer handling of multi-byte characters.", 0,
            (:
                object ob = load_object("/utf-8-on-boundary");
                return ob->run_test();
            :)
        }),
        ({ "Compile error messages with multi-byte characters.", 0,
            (:
                errors = ({});
                catch(load_object("/utf-8-in-error"));

                foreach(string err: errors)
                    if (catch(sizeof(err)))
                        return 0;

                return 1;
            :)
        }),
    }),
    (:
        if($1)
            shutdown(1);
        else
            start_gc(#'shutdown);
    :));
}

private string get_file_encoding(string filename)
{
    string lines;

    if (read_bytes(filename, 0, 3) == b"\xef\xbb\xbf")
        return "UTF-8";

    lines = read_file(filename, 0, 2, "ascii");
    if (!lines)
        return "UTF-8";

    string* enc = regmatch(lines, "(^|\n)[ \t]*/[/*].*coding[:=][ \t]*([-_.a-zA-Z0-9]+)", RE_MATCH_SUBS);
    if (!enc)
        return "UTF-8";

    return enc[2];
}

void log_error(string file, string err, int warn)
{
    errors += ({err});
}

string *epilog(int eflag)
{
    set_driver_hook(H_FILE_ENCODING, #'get_file_encoding);

    run_test();
    return 0;
}
