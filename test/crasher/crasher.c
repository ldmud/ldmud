/*---------------------------------------------------------------------------
 * Fuzzy efun tester
 *
 * Author: Menaures (07.12.2001)
 *---------------------------------------------------------------------------
 * Calls efuns with random arguments.
 *
 * For configuration options see config.h
 *
 * Generate
 * --------
 * Randomly chooses functions to be called and their arguments. These
 * decisions are stored in a savefile under /crash with the current time.
 *
 * After generation the functions will be executed. The efun call result
 * is of no consequence. It's just important, that the driver doesn't crash.
 * The current function and its arguments are stored in /crash/LAST_EXEC
 * and removed upon a succussful call.
 *
 * Replay
 * ------
 * When REPLAY_FILE is defined, this file will be read instead and executed.
 *
 *---------------------------------------------------------------------------
 */

#include "/inc/msg.inc"
#include "/config.h"

struct Foo {
    symbol value;
};

struct Bar(Foo) {
    int num;
};

mixed efuns;
mixed values;

void reset_all()
{
    mixed val;
    string str = "Hallihallo";

    efuns =
    ({
        #'[,
        #'abs,
        #'acos,
        #'add_action,
        #'all_environment,
        #'all_inventory,
        #'allocate,
        #'and_bits,
        #'apply,
        #'asin,
        #'atan,
        #'atan2,
#if __EFUN_DEFINED__(attach_erq_demon)
        #'attach_erq_demon,
#endif
        #'baseof,
        #'binary_message,
        #'bind_lambda,
        #'blueprint,
#if __EFUN_DEFINED__(break_point)
        #'break_point,
#endif
        #'bytesp,
        #'call_coroutine,
        #'call_direct,
        #'call_direct_resolved,
        #'call_direct_strict,
        #'caller_stack,
        #'caller_stack_depth,
        #'call_other,
        #'call_out,
        #'call_out_info,
        #'call_resolved,
        #'call_strict,
        #'capitalize,
        #'catch,
        #'ceil,
        #'check_type,
        #'clear_bit,
        #'clone_object,
        #'clonep,
        #'clones,
        #'closurep,
        #'command,
        #'command_stack,
        #'command_stack_depth,
        #'compile_string,
        #'configure_driver,
        #'configure_interactive,
        #'configure_lwobject,
        #'configure_object,
        #'copy,
        #'copy_bits,
        #'copy_file,
        #'coroutinep,
        #'cos,
        #'count_bits,
#if __EFUN_DEFINED__(creator)
        #'creator,
#endif
        #'crypt,
        #'ctime,
#ifdef __MYSQL__
        #'db_affected_rows,
        #'db_close,
        #'db_coldefs,
        #'db_connect,
        #'db_conv_string,
        #'db_error,
        #'db_exec,
        #'db_fetch,
        #'db_handles,
        #'db_insert_id,
#endif
        #'debug_message,
        #'deep_copy,
        #'deep_inventory,
        #'destruct,
        #'driver_info,
        #'dump_driver_info,
        #'ed,
        #'environment,
        #'exec,
        #'execute_command,
        #'exp,
        #'expand_define,
        #'explode,
        #'extern_call,
        #'file_size,
        #'filter,
        #'filter_indices,
        #'filter_objects,
        #'find_call_out,
        #'find_input_to,
        #'find_object,
        #'first_inventory,
        #'floatp,
        #'floor,
        #'funcall,
        #'function_exists,
        #'functionlist,
        #'garbage_collection,
        #'get_dir,
        #'get_error_file,
        #'geteuid,
#if __EFUN_DEFINED__(getuid)
        #'getuid,
#endif
        #'get_eval_cost,
        #'get_extra_wizinfo,
        #'get_type_info,
        #'getuid,
        #'gmtime,
        #'hash,
        #'heart_beat_info,
        #'hmac,
#ifdef __IDNA__
        #'idna_stringprep,
        #'idna_to_ascii,
        #'idna_to_unicode,
#endif
        #'implode,
        #'include_list,
        #'inherit_list,
        #'input_to,
        #'input_to_info,
        #'interactive,
        #'interactive_info,
        #'intp,
        #'invert_bits,
#ifdef __JSON__
        #'json_parse,
        #'json_serialize,
#endif
        #'lambda,
        #'last_bit,
#if __EFUN_DEFINED__(last_instructions)
        #'last_instructions,
#endif
        #'limited,
        #'living,
        #'load_name,
        #'load_object,
        #'localtime,
        #'log,
        #'lower_case,
        #'lpctypep,
        #'lwobject_info,
        #'lwobjectp,
        #'m_add,
        #'make_shared_string,
        #'m_allocate,
        #'map,
        #'map_indices,
        #'map_objects,
        #'mappingp,
        #'master,
        #'match_command,
        #'max,
        #'m_contains,
        #'md5,
        #'md5_crypt,
        #'m_delete,
        #'member,
        #'m_entry,
        #'min,
        #'m_indices,
        #'mkdir,
        #'mkmapping,
        #'mktime,
        #'move_object,
        #'m_reallocate,
        #'m_values,
        #'negate,
        #'net_connect,
        #'new_lwobject,
        #'next_bit,
        #'next_inventory,
        #'notify_fail,
        #'object_info,
        #'object_name,
        #'objectp,
        #'objects,
        #'object_time,
        #'or_bits,
#if __EFUN_DEFINED__(parse_command)
        #'parse_command,
#endif
#ifdef __PGSQL__
        #'pg_close,
        #'pg_connect,
        #'pg_conv_string,
        #'pg_pending,
        #'pg_query,
#endif
        #'pointerp,
        #'pow,
        #'present,
        #'present_clone,
        #'previous_object,
        #'printf,
#if __EFUN_DEFINED__(process_string)
        #'process_string,
#endif
        #'program_name,
        #'program_time,
        #'query_actions,
        #'query_command,
        #'query_notify_fail,
        #'query_verb,
        #'quote,
        #'raise_error,
        #'random,
        #'read_bytes,
        #'read_file,
        #'referencep,
        #'regexp,
        #'regexplode,
        #'regexp_package,
        #'regmatch,
        #'regreplace,
        #'remove_action,
        #'remove_call_out,
        #'remove_input_to,
        #'remove_interactive,
        #'rename,
        #'rename_object,
        #'replace_program,
        #'restore_object,
        #'restore_value,
        #'reverse,
        #'rm,
        #'rmdir,
        #'rmember,
        #'rusage,
        #'save_object,
        #'save_value,
        #'say,
#if __EFUN_DEFINED__(send_erq)
        #'send_erq,
#endif
        #'send_udp,
        #'set_bit,
        #'set_driver_hook,
        #'set_environment,
        #'set_extra_wizinfo,
        #'set_next_reset,
        #'set_this_object,
        #'set_this_player,
        #'sgn,
        #'sha1,
        #'shadow,
        //#'shutdown,
        #'sin,
        #'sizeof,
#ifdef __SQLITE__
        #'sl_close,
        #'sl_exec,
        #'sl_insert_id,
        #'sl_open,
#endif
        #'snoop,
        #'sort_array,
        #'sprintf,
        #'sqrt,
        #'sscanf,
        #'strftime,
        #'stringp,
        #'strrstr,
        #'strstr,
        #'struct_info,
        #'structp,
#if __EFUN_DEFINED__(swap)
        #'swap,
#endif
        #'symbol_function,
        #'symbolp,
        #'symbol_variable,
        #'tan,
        #'tell_object,
        #'tell_room,
        #'terminal_colour,
        #'test_bit,
        #'text_width,
        #'this_coroutine,
        #'this_interactive,
        #'this_object,
        #'this_player,
        #'throw,
        #'time,
        #'tls_available,
        #'tls_check_certificate,
        #'tls_deinit_connection,
        #'tls_error,
        #'tls_init_connection,
        #'tls_query_connection_info,
        #'tls_query_connection_state,
        #'tls_refresh_certs,
        #'to_array,
        #'to_bytes,
        #'to_float,
        #'to_int,
        #'to_lpctype,
        #'to_object,
        #'to_string,
        #'to_struct,
        #'to_text,
        #'to_type,
        #'trace,
        #'traceprefix,
#if __EFUN_DEFINED__(transfer)
        #'transfer,
#endif
        #'transpose_array,
        #'trim,
        #'typeof,
        #'unbound_lambda,
        #'unique_array,
        #'unmkmapping,
        #'unquote,
        #'unshadow,
        #'upper_case,
        #'users,
        #'utime,
        #'variable_exists,
        #'variable_list,
        #'walk_mapping,
        #'widthof,
        #'wizlist_info,
        #'write,
        #'write_bytes,
        #'write_file,
#ifdef __XML_DOM__
        #'xml_generate,
        #'xml_parse,
#endif
        #'xor_bits,

        #',,
        #'=,
        #'+=,
        #'-=,
        #'&=,
        #'|=,
        #'^=,
        #'<<=,
        #'>>=,
        #'>>>=,
        #'*=,
        #'%=,
        #'/=,
        #'?,
        #'?!,
        #'||,
        #'&&,
        #'|,
        #'^,
        #'&,
        #'==,
        #'!=,
        #'>,
        #'>=,
        #'<,
        #'<=,
        #'<<,
        #'>>,
        #'>>>,
        #'+,
        #'-,
        #'*,
        #'%,
        #'/,
        #'++,
        #'--,
        #'negate,
        #'!,
        #'~,
        #'({,
        #'([,
        #'[,
        #'[<,
        #'[>,
        #'[,],
        #'[,<],
        #'[,>],
        #'[..],
        #'[..<],
        #'[..>],
        #'[<..],
        #'[<..<],
        #'[<..>],
        #'[>..],
        #'[>..<],
        #'[>..>],
        #'[..,
        #'[<..,
        #'[>..,
        #'[,..],
        #'[,..<],
        #'[,..>],
        #'[,<..],
        #'[,<..<],
        #'[,<..>],
        #'[,>..],
        #'[,>..<],
        #'[,>..>],
        #'({,
        #'([,
        #'(<,

        #'reset_all /* Needs to be the last entry. */
    });

    values =
    ({
        /* --- Integers: --- */
        0,
        1,
        -1,
        32,
        -32,
        40,
        -40,
        29292929,
       -5050211,
        __INT_MAX__,
        __INT_MIN__,

        /* --- Floats: --- */
        0.0,
        0.5,
       -0.5,
        55.5,
       -55.5,
        999999999999999999999999999999999999999999999.99,
       -999999999999999999999999999999999999999999999.99,

        /* --- Strings: --- */
        "",
        "foo bar",
        "%d %q %T",
        "0",
        "",
        " ",
        "_",
        "^^^^^^",
        "#^#         #^#",
        "                ^",
        "^                      ",
        "  -   -   -                    - ",
        "? ?    ?    ?  ? ?  ?????   ????  ??",
        "\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0\u00b0",
        "\\/ "*32,
        "    !"*42,

        /* --- Objekte: --- */
        __MASTER_OBJECT__,
        clone_object("/a"),
        new_lwobject("/a"),

        /* --- Closures: --- */
        (: :),
        (: (: 1 :) :),
        #'sizeof,
        #'efun::input_to,
        #'switch,
        #'++,
        symbol_function("create", load_object("/a")),
        lambda( ({'x, 'y}), ({#'?, ({#'>, 'x, 'y}), 'x, 'y}) ),
        unbound_lambda( ({'x}), ({#'==, 0, 'x}) ),

        /* --- Arrays: --- */
        ({ }),
        ({ 0 }),
        ({ "" }),
        ({ ({ }) }),
        ({ ([ ]) }),
        allocate(3000),

        /* --- Mappings: --- */
        ([ ]),
        ([ (: :) ]),
        m_allocate(5,5),
        mkmapping(allocate(30), allocate(30, ({})), allocate(30, (:1:))),

        /* --- Structs: --- */
        (<Foo> 'abc),
        (<Bar> 'xyz, 10),
        (<compile_string_options> use_object_functions: 1, detect_end: 1),
        (<to_type_options> source_encoding: "iso8859-1", target_encoding: "utf-8"),

        /* --- References: --- */
        &val,
        &(str[3..7]),

        /* --- Coroutines: --- */
        async function void() {},

        /* --- LPC types: --- */
        [int],
        [string|bytes*],
        [struct Foo],
    });

    while(remove_call_out("execute_file") != -1);
    while(remove_call_out("do_replay") != -1);
    while(remove_call_out("do_record") != -1);
}

void check_arguments(closure efun, mixed *arguments)
{
    switch (to_string(efun))
    {
        case "#'md5":
        case "#'sha1":
            if (sizeof(arguments) > 1 && intp(arguments[1]) && arguments[1] > 5)
                arguments[1] = 3;
            break;
        case "#'hash":
            if (sizeof(arguments) > 2 && intp(arguments[2]) && arguments[2] > 5)
                arguments[2] = 3;
            break;
    }
}

int execute_file(string file, int show)
{
    int line;
    string str;

    if(!stringp(file) || file_size(file) <= 0)
    {
        msg("ERROR: Cannot execute '%s': Invalid file.\n", file);
        return 0;
    }

    msg("Executing: %s\n", file);

    for(line = 1; (str=read_file(file, line, 2)) && sizeof(str); line += 2)
    {
        mixed task = restore_value(str);
        write_file("/crash/LAST_EXEC", str, 1);

#ifdef TEST_EFUN
        closure efun = task[0] < 0 ? TEST_EFUN : efuns[ task[0] ];
#else
        closure efun = efuns[ task[0] ];
#endif
        mixed arguments = map( task[1], (: values[$1] :));
        check_arguments(efun, arguments);

#ifdef VERBOSE
        msg("File: %s, Line: %d\n", file, line);
#endif
        if (show)
            msg("  %Q %Q\n", efun, arguments);

        catch(funcall(efun, arguments...));

        rm("/crash/LAST_EXEC");
    }

    return 1;
}

int generate_file(string file)
{
    if(!stringp(file) || !write_file(file, "", 1))
    {
        msg("ERROR: Cannot write file: %s\n", file);
        return 0;
    }

    msg("Recording: %s\n", file);

    foreach (int i: CALLS)
    {
#ifdef TEST_EFUN
        int efun_index = -1;
#else
        int efun_index = random(sizeof(efuns)-1);
#endif
        int *value_index = ({});

        if (i && !(i % REINIT_LENGTH))
            write_file(file, save_value( ({ sizeof(efuns)-1, ({}) }) ));

        foreach (int j: random(MAX_ARGS+1))
            value_index += ({ random(sizeof(values)) });

        write_file(file, save_value( ({efun_index, value_index}) ));
    }

    return 1;
}

void replay(string file)
{
    execute_file(file, 1);

    __MASTER_OBJECT__->finish(1);
}

void record(string file)
{
    if(generate_file(file))
        execute_file(file, 0);

    __MASTER_OBJECT__->finish(1);
}

void create()
{
    mkdir("/crash");
    reset_all();

#ifdef REPLAY_FILE
    string str = read_file(REPLAY_FILE);
    if(str)
    {
        replay(REPLAY_FILE);
        return;
    }
#ifndef GENERATE_IF_NO_REPLAY_FILE
    else
    {
        msg("Replay file not found: %s\n", REPLAY_FILE);
        __MASTER_OBJECT__->finish(0);
        return;
    }
#endif
#endif

    int* ut = utime();
    record(sprintf("/crash/%d-%06d", ut[0], ut[1]));
}

/* --- End of file. --- */
