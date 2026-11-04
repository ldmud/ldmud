#include "/inc/base.inc"
#include "/inc/testarray.inc"

mixed *diagnostics = ({});

/* Keep the four-argument master callback contract. */
void log_error(string file, string message, int warn, int line)
{
    diagnostics += ({ ({ file, line, warn ? "warning" : "error", message }) });
}

private mixed *load_diagnostics(string file)
{
    mixed err;
    object loaded;

    diagnostics = ({});
    err = catch(loaded = load_object(file); nolog);
    if (loaded)
        destruct(loaded);
    return ({ !err, diagnostics });
}

private int valid_result(mixed result)
{
    if (!pointerp(result) || sizeof(result) != 2 || !intp(result[0]) ||
        !pointerp(result[1]))
        return 0;

    foreach (mixed entry: result[1])
        if (!pointerp(entry) || sizeof(entry) != 4 || !stringp(entry[0]) ||
            !intp(entry[1]) || (entry[2] != "error" && entry[2] != "warning") ||
            !stringp(entry[3]))
            return 0;

    return 1;
}

/* Callback file names omit the leading slash. Rendered locations may use
 * either spelling, but must start with their location.
 */
private int has_context(mixed *result, string file, int line, int column,
                        string severity, string source, int width,
                        string *words)
{
    if (valid_result(result))
    {
        foreach (mixed *entry: result[1])
        {
            string message = entry[3];
            string location = sprintf("%s:%d:%d: %s: ", file, line, column, severity);
            string excerpt = sprintf("%5d | %s\n", line, source);
            string marker = "      | " + " " * (column - 1) + "^";
            int matches = 1;

            if (width)
                marker += "~" * (width - 1) + "\n";

            if ((entry[0] != file && entry[0] != "/" + file) ||
                entry[1] != line || entry[2] != severity ||
                (strstr(message, location) != 0 &&
                 strstr(message, "/" + location) != 0) ||
                strstr(message, excerpt) < 0 || strstr(message, marker) < 0)
                continue;

            foreach (string word: words)
                if (strstr(message, word) < 0)
                    matches = 0;

            if (matches)
                return 1;
        }
    }

    msg("\nExpected %s:%d:%d: %s with source %O; got %O\n",
        file, line, column, severity, source, result);
    return 0;
}

private int has_error(string file, int line, int column, string source,
                      int width, string *words)
{
    mixed *result = load_diagnostics("/" + file);

    return !result[0] && has_context(result, file, line, column,
                                    "error", source, width, words);
}

private string truncation_source(int padding, int aligned)
{
    string source = "#pragma strong_types\n";

    /* Each definition is under the lexer line limit, but its single long
     * grapheme makes eight rendered notes exceed diagnostic byte limits.
     */
    foreach (int index: 8)
    {
        /* Align definitions to refill boundaries so all eight complete
         * original lines are available for the truncation regression.
         */
        if (aligned)
            source += "\n" * (2048 - sizeof(to_bytes(source, "UTF-8")) % 2048);
        source += sprintf("#define M%d %s /* %se%s */\n", index,
            index ? sprintf("M%d", index - 1) : "\"bad\"",
            " " * padding, "\u0301" * 1000);
    }

    return source;
}

private int valid_utf8_diagnostic(mixed message, string surface, int padding)
{
    mixed err;

    if (!stringp(message))
        return 0;

    err = catch(to_text(to_bytes(message, "UTF-8"), "UTF-8"); nolog);
    if (!err)
        err = catch(sizeof(message); nolog);
    if (err)
    {
        msg("\nInvalid UTF-8 diagnostic through %s (padding %d).\n", surface, padding);
        return 0;
    }

    if (strstr(message, "count") >= 0 && strstr(message, "expected int") >= 0 &&
        strstr(message, "got string") >= 0)
        return 1;

    msg("\nPrincipal diagnostic missing through %s (padding %d).\n", surface, padding);
    return 0;
}

private int truncation_file_test()
{
    int success = 1;

    foreach (int padding: 2)
    {
        string file = sprintf("/utf8_truncation_%d.c", padding);
        string source = truncation_source(padding, 1) + "int count = M7;\n";
        mixed *result;

        write_file(file, source, 1, "UTF-8");
        result = load_diagnostics(file);
        rm(file);
        if (!valid_result(result) || result[0] || !sizeof(result[1]))
            success = 0;
        else
            foreach (mixed *entry: result[1])
                if (!valid_utf8_diagnostic(entry[3], "log_error", padding))
                    success = 0;
    }

    return success;
}

void run_test()
{
    msg("\nRunning source diagnostic tests:\n"
          "--------------------------------\n");

    run_array(({
        ({ "macro definition source survives an input refill", 0,
           (:
               string file = "/utf8_refill.c";
               write_file(file, truncation_source(0, 0) + "int count = M7;\n", 1, "UTF-8");
               mixed *result = load_diagnostics(file);
               rm(file);
               if (valid_result(result) && !result[0])
                   foreach (mixed *entry: result[1])
                       if (strstr(entry[3], "#define M1 M0 /* e") >= 0)
                           return 1;
               msg("\nMacro M1 source excerpt was lost across the input refill.\n");
               return 0;
           :)
        }),
        ({ "log_error truncation preserves UTF-8 and principal message", 0,
           (: return truncation_file_test(); :)
        }),
        ({ "compile_string truncation preserves UTF-8 and principal message", 0,
           (:
               int success = 1;
               foreach (int padding: 2)
               {
                   string source = truncation_source(padding, 1) + "int count = M7; return count;";
                   mixed err = catch(compile_string(0, source,
                       (<compile_string_options> compile_block: 1)); nolog);
                   if (!valid_utf8_diagnostic(err, "compile_string", padding))
                       success = 0;
               }
               return success;
           :)
        }),
        ({ "elif rewrite retains the original directive start", 0,
           (:
               return has_error("elif_rewrite.c", 2, 1,
                   "#elif @", 0, ({}));
           :)
        }),
        ({ "overlong refill line does not borrow a previous token caret", 0,
           (:
               mixed *result = load_diagnostics("/refill_line_too_long.c");
               foreach (mixed *entry: result[1])
                   if (entry[0] == "refill_line_too_long.c" && entry[1] == 4 &&
                       entry[2] == "error" &&
                       strstr(lower_case(entry[3]), "line too long") >= 0 &&
                       strstr(entry[3], "^") < 0)
                       return !result[0];
               msg("\nObserved refill diagnostic: %O\n", result);
               return 0;
           :)
        }),
        ({ "endif trailing warning uses its own directive line", 0,
           (:
               mixed *result = load_diagnostics("/directive_warning.c");
               return result[0] && has_context(result,
                   "directive_warning.c", 2, 1, "warning", "#endif extra", 0, ({}));
           :)
        }),
        ({ "overlong first source line still invokes master with its file", 0,
           (:
               mixed *result = load_diagnostics("/first_line_too_long.c");
               foreach (mixed *entry: result[1])
                   if (entry[0] == "first_line_too_long.c" && entry[1] == 1 &&
                       entry[2] == "error" &&
                       strstr(lower_case(entry[3]), "line too long") >= 0)
                       return !result[0];
               msg("\nObserved first-line diagnostic: %O\n", result);
               return 0;
           :)
        }),
        ({ "unsupported source encoding reports the file before lexing", 0,
           (:
               set_driver_hook(H_FILE_ENCODING, "NO_SUCH_ENCODING");
               mixed *result = load_diagnostics("/good.c");
               set_driver_hook(H_FILE_ENCODING, "UTF-8");
               foreach (mixed *entry: result[1])
                   if (entry[0] == "good.c" && entry[1] == 1 &&
                       entry[2] == "error" && strstr(lower_case(entry[3]), "encoding") >= 0)
                       return !result[0];
               msg("\nObserved encoding diagnostic: %O\n", result);
               return 0;
           :)
        }),
        ({ "included encoding error does not reuse the parent directive", 0,
           (:
               set_driver_hook(H_FILE_ENCODING,
                   (: strstr($1, "encoding_include.h") >= 0 ? "NO_SUCH_ENCODING" : "UTF-8" :));
               mixed *result = load_diagnostics("/encoding_parent.c");
               set_driver_hook(H_FILE_ENCODING, "UTF-8");
               foreach (mixed *entry: result[1])
                   if (entry[0] == "encoding_include.h" && entry[1] == 1 &&
                       entry[2] == "error" && strstr(lower_case(entry[3]), "encoding") >= 0 &&
                       strstr(entry[3], "^") < 0)
                       return !result[0];
               msg("\nObserved included encoding diagnostic: %O\n", result);
               return 0;
           :)
        }),
        ({ "nested argument calls retain the outer call name location", 0,
           (:
               return has_error("nested_calls.c", 6, 5,
                   "    take(inner());", 4,
                   ({ "take", "expected int", "got string" }));
           :)
        }),
        ({ "token after object macro has no stale macro origin", 0,
           (:
               mixed *result = load_diagnostics("/after_macro.c");
               if (result[0] || !has_context(result, "after_macro.c", 5, 41,
                   "error", "    int first = GOOD_VALUE; int count = \"three\";", 7,
                   ({ "count", "expected int", "got string" })))
                   return 0;
               foreach (mixed *entry: result[1])
                   if (strstr(entry[3], "note:") >= 0)
                       return 0;
               return 1;
           :)
        }),
        ({ "multiline function macro points to first invocation line", 0,
           (:
               return has_error("macro_multiline.c", 5, 17,
                   "    int count = BAD_CALL(", 0,
                   ({ "BAD_CALL", "macro_multiline.c:2:", "note:" }));
           :)
        }),
        ({ "long source excerpt crops around the actual caret", 0,
           (:
               mixed err = catch(compile_string(0, " " * 1000 + "1 + ;"); nolog);
               if (stringp(err) && strstr(err, "master.c (string):1:1005: error:") >= 0)
               {
                   string *lines = explode(err, "\n");
                   foreach (int pos: sizeof(lines) - 1)
                   {
                       string source = lines[pos];
                       string marker = lines[pos + 1];
                       if (strstr(source, "    1 | ") == 0 && sizeof(source) < 300 &&
                           strstr(source, "1 + ;") >= 0 &&
                           strstr(marker, "      | ") == 0 &&
                           strstr(source, ";") == strstr(marker, "^"))
                           return 1;
                   }
               }
               msg("\nObserved cropped diagnostic: %O\n", err);
               return 0;
           :)
        }),
        ({ "source rendering sanitizes terminal escape and control bytes", 0,
           (:
               mixed err = catch(compile_string(0, "/* \e[31m \x01 \x7f */ 1 + ;"); nolog);
               if (stringp(err) && strstr(err, "master.c (string):1:") >= 0 &&
                   strstr(err, "    1 | ") >= 0 && strstr(err, "1 + ;") >= 0 &&
                   strstr(err, "      | ") >= 0 && strstr(err, "^") >= 0 &&
                   strstr(err, "\e") < 0 && strstr(err, "\x01") < 0 && strstr(err, "\x7f") < 0)
                   return 1;
               msg("\nObserved control-byte diagnostic: %O\n", err);
               return 0;
           :)
        }),
        ({ "source capture exhaustion preserves successful compilation", 0,
           (:
               /* Only token-bearing lines need permanent source snapshots.
                * These comment-bearing lines total more than 1.3 MB.
                */
               string input = ("/* " + "x" * 56 + " */ 0 +\n") * 20000 + "42";
               return funcall(compile_string(0, input)) == 42;
           :)
        }),
        ({ "exhausted source capture reports honest file and line only", 0,
           (:
               /* A larger last line cannot fit a remainder too small for
                * preceding snapshots; a tiny line might still fit there.
                */
               string input = ("/* " + "x" * 56 + " */ 0 +\n") * 20000 +
                   " " * 200 + "1 + ;";
               mixed err = catch(compile_string(0, input); nolog);
               if (stringp(err) &&
                   strstr(err, "master.c (string):20001: error:") >= 0 &&
                   strstr(err, "unexpected") >= 0 && strstr(err, "';'") >= 0 &&
                   strstr(err, " | ") < 0 && strstr(err, "^") < 0)
                   return 1;
               msg("\nObserved exhausted-capture diagnostic: %O\n", err);
               return 0;
           :)
        }),
        ({ "error directive retains its own source line", 0,
           (:
               return has_error("error_directive.c", 3, 1,
                   "#error stop_here", 0, ({ "stop_here" }));
           :)
        }),
        ({ "EOF after actual newline points to the following empty line", 0,
           (:
               return has_error("end_of_file_newline.c", 5, 1,
                   "", 1, ({ "end of file" }));
           :)
        }),
        ({ "concatenated string RHS starts at its first physical token", 0,
           (:
               return has_error("concatenated_rhs.c", 5, 9,
                   "        \"th\"", 4, ({ "expected int", "got string" }));
           :)
        }),
        ({ "local initializer reports value location and named types", 0,
           (:
               return has_error("local_initializer.c", 4, 17,
                   "    int count = \"three\";", 7,
                   ({ "count", "expected int", "got string" }));
           :)
        }),
        ({ "global initializer reports value location and named types", 0,
           (:
               return has_error("global_initializer.c", 2, 13,
                   "int count = \"three\";", 7,
                   ({ "count", "expected int", "got string" }));
           :)
        }),
        ({ "multiline assignment points to its right hand side", 0,
           (:
               return has_error("assignment.c", 6, 9,
                   "        \"three\";", 7,
                   ({ "expected int", "got string" }));
           :)
        }),
        ({ "multiline return points to returned value", 0,
           (:
               return has_error("return_value.c", 5, 9,
                   "        \"three\";", 7,
                   ({ "expected int", "got string" }));
           :)
        }),
        ({ "argument type error points to call on its original line", 0,
           (:
               return has_error("argument_type.c", 5, 5,
                   "    take(", 4,
                   ({ "take", "expected int", "got string" }));
           :)
        }),
        ({ "argument count error points to call name", 0,
           (:
               return has_error("argument_count.c", 5, 5,
                   "    take();", 4, ({ "take", "argument" }));
           :)
        }),
        ({ "syntax error uses a readable unexpected token", 0,
           (:
               return has_error("syntax.c", 4, 17,
                   "    int count = ;", 1,
                   ({ "unexpected", "';'" }));
           :)
        }),
        ({ "nested include error uses the innermost physical source", 0,
           (:
               mixed *result = load_diagnostics("/nested_include.c");
               return !result[0] && has_context(result,
                   "include_inner.h", 2, 13, "error",
                   "int count = \"three\";", 7, ({}));
           :)
        }),
        ({ "nested include error carries parent source provenance", 0,
           (:
               mixed *result = load_diagnostics("/nested_include.c");
               return !result[0] && has_context(result,
                   "include_inner.h", 2, 13, "error",
                   "int count = \"three\";", 7,
                   ({ "include_outer.h:2:", "nested_include.c:2:",
                      "#include \"include_inner.h\"",
                      "#include \"include_outer.h\"" }));
           :)
        }),
        ({ "object macro shows invocation and definition", 0,
           (:
               return has_error("macro_object.c", 5, 17,
                   "    int count = BAD_VALUE;", 0,
                   ({ "BAD_VALUE", "macro_defs.h:1:",
                      "#define BAD_VALUE \"three\"", "note:" }));
           :)
        }),
        ({ "function macro shows invocation and definition", 0,
           (:
               return has_error("macro_function.c", 5, 17,
                   "    int count = BAD_CALL(\"three\");", 0,
                   ({ "BAD_CALL", "macro_defs.h:2:",
                      "#define BAD_CALL(value) value", "note:" }));
           :)
        }),
        ({ "nested macro preserves both expansion definitions", 0,
           (:
               return has_error("macro_nested.c", 5, 17,
                   "    int count = OUTER_VALUE;", 0,
                   ({ "OUTER_VALUE", "BAD_VALUE", "macro_defs.h:1:",
                      "macro_defs.h:3:", "note:" }));
           :)
        }),
        ({ "macro provenance survives undef before expression reduces", 0,
           (:
               return has_error("macro_undef.c", 5, 17,
                   "    int count = BAD_VALUE", 0,
                   ({ "macro_undef.c:2:", "#define BAD_VALUE \"three\"", "note:" }));
           :)
        }),
        ({ "macros do not leak into next compilation", 0,
           (:
               load_diagnostics("/macro_undef.c");
               mixed *result = load_diagnostics("/macro_clean.c");
               return valid_result(result) && result[0] && !sizeof(result[1]);
           :)
        }),
        ({ "tabs use eight-column stops in both source and caret", 0,
           (:
               return has_error("coordinates_tabs.c", 4, 21,
                   "        int count = \"three\";", 7, ({}));
           :)
        }),
        ({ "UTF-8 uses display width including combining characters", 0,
           (:
               return has_error("coordinates_utf8.c", 4, 28,
                   "    /* \u00e9\u6f22e\u0301 */ int count = \"three\";", 7, ({}));
           :)
        }),
        ({ "EOF without newline reports one-past-last-character", 0,
           (:
               return has_error("end_of_file.c", 4, 16,
                   "    int count =", 1, ({ "end of file" }));
           :)
        }),
        ({ "source survives lexer buffer refills", 0,
           (:
               return has_error("buffer_refill.c", 84, 17,
                   "    int count = \"three\";", 7, ({ "count" }));
           :)
        }),
        ({ "line directive rewind preserves the actual source occurrence", 0,
           (:
               return has_error("line_rewind.c", 10, 13,
                   "int count = \"three\";", 7, ({ "count" }));
           :)
        }),
        ({ "warning has source context and compilation still succeeds", 0,
           (:
               mixed *result = load_diagnostics("/warning.c");
               return result[0] && has_context(result,
                   "warning.c", 4, 5, "warning", "    42;", 2, ({}));
           :)
        }),
        ({ "master callback retains four arguments and matching source", 0,
           (:
               mixed *result = load_diagnostics("/local_initializer.c");
               return !result[0] && sizeof(result[1]) &&
                   result[1][0][0] == "local_initializer.c" &&
                   has_context(result, "local_initializer.c", 4, 17,
                       "error", "    int count = \"three\";", 7,
                       ({ "expected int", "got string" }));
           :)
        }),
        ({ "successful compilation clears previous diagnostics", 0,
           (:
               load_diagnostics("/macro_nested.c");
               mixed *result = load_diagnostics("/good.c");
               return valid_result(result) && result[0] && !sizeof(result[1]);
           :)
        }),
        ({ "compile_string syntax error includes its exact input source", 0,
           (:
               diagnostics = ({});
               mixed err = catch(compile_string(0, "1 + ;"); nolog);
               /* String compilation reports through the caught error,
                * without invoking the master callback.
                */
               if (stringp(err) && !sizeof(diagnostics) &&
                   strstr(err, "master.c (string):1:5: error: ") >= 0 &&
                   strstr(err, "    1 | 1 + ;\n") >= 0 &&
                   strstr(err, "      |     ^\n") >= 0 &&
                   strstr(err, "unexpected") >= 0 && strstr(err, "';'") >= 0)
                   return 1;
               msg("\nObserved compile_string error: %O\n", err);
               return 0;
           :)
        }),
        ({ "compile_string expression end detection retains suffix", 0,
           (:
               string expr = "1+1!";
               return funcall(compile_string(0, &expr,
                   (<compile_string_options> detect_end: 1))) == 2 && expr == "!";
           :)
        }),
        ({ "compile_string macro end detection retains suffix", 0,
           (:
               string expr = "#define VALUE 42\nVALUE;";
               return funcall(compile_string(0, &expr,
                   (<compile_string_options> detect_end: 1))) == 42 && expr == ";";
           :)
        }),
        ({ "compile_string block end detection retains suffix", 0,
           (:
               string expr = "{ return 42; }\n@";
               return funcall(compile_string(0, &expr,
                   (<compile_string_options> compile_block: 1, detect_end: 1))) == 42 &&
                   expr == "\n@";
           :)
        }),
    }), #'shutdown);
}

string *epilog(int eflag)
{
    set_driver_hook(H_FILE_ENCODING, "UTF-8");
    run_test();
    return 0;
}
