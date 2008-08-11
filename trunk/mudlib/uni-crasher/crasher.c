/*
crasher: Standardversion (Menaures 07.12.01)

Modified: 
  14.07.2004 Menaures: Fuer den 3-3er wieder flottgemacht

ANLEITUNG:

1. Bei dem Define DEBUGGER den eigenen Real-Namen angeben

2. Define VERBOSE auskommentieren, falls nicht bereits so.
   VERBOSE kann genutzt werden, wenn gesicherte Dateien einen
   Crash ausloesen und die genaue Zeile gesucht wird.
   (VERBOSE gibt vor jedem einzelnen Aufruf momentane
    Datei und Zeile per debug_message aus).

3. Wenn bereits aufgenommene Dateien existieren, das EFuns und
   Values-Array  NICHT  modifizieren, bzw. nicht die Positionen
   der einzelnen Elemente aendern - sonst werden die aufgezeichneten
   Dateien unbrauchbar. Ebenso koennen nach solchen Modifikationen
   aufgenommene Dateien nicht mehr von alten Versionen des Crashers
   abgespielt werden.

   Wer andere Values / Closures haben moechte, modifiziere die
   entsprechenden Arrays, speichere den Crasher unter einem anderen
   Filenamen und mache einen eindeutigen Eintrag im Header...

record:

  record(file) speichert Ausfuehrungsablaeufe in Dateien namens
  file_xxxx, wobei xxxx eine Durchlaufnummer ist. Das Objekt
  braucht fuer diese Dateien logischerweise Schreibrecht.

  Die aufgenommenen Dateien werden nach ihrer Erstellung automatisch
  ausgefuehrt. Fuehrt dies zu einem Crash, kann man das ganze einfach
  nochmals abspielen.

replay:

  replay(file) spielt aufgenommene Dateien ab, fuehrt sie also aus.
  Dies ist dazu da, um Crashes, die dieses Objekt erzeugt, zu
  reproduzieren. Statt file wird file_xxxx eingelesen, wobei xxxx
  eine Durchlaufnummer ist.

execute_file:

  Mit execute_file(file) kann man eine Datei gezielt ausfuehren.
  Dazu gibt man den echten Dateinamen der Datei an, inklusive
  Durchlaufnummer.

debug_query:

  Mit debug_query(file, line) kann man abfragen, was in der Datei
  file in Zeile line ausgefuehrt werden wuerde - also welche
  Closure mit welchen Argumenten.

  Hierzu muss der eigene Name als DEBUGGER definiert sein.

debug_exec:

  Mit debug_exec(file, line) wird die angegebene Zeile in der
  Datei ausgefuehrt. Dadurch kann gezielt festgestellt werden,
  ob der einzelne Aufruf den Crash ausgeloest hat.


Crash-Grund finden:

  Bringt der Crasher den Driver zum Absturz, zunaechst den Driver
  neu starten und mit execute_file das zuletzt aufgenommene File
  abspielen.

  Bringt das den Driver zum Crash, den gleichen Vorgang mit
  aktiviertem VERBOSE wiederholen, um die Zeilennummer herauszufinden.

  Hat man erst einmal die Zeilennummer, laesst sich per debug_query
  herausfinden, was ausgefuehrt werden, und mit debug_exec pruefen,
  ob es wirklich der Ausloeser war.

  Kann der Crash auf diese Weise nicht reproduziert werden, kann man
  einmal versuchen, alle aufgenommenen Dateien mit replay abzuspielen;
  fuehrt das zu keinem Crash, wurde der Crash nicht durch einen
  bestimmten Aufruf (oder eine bestimmte Folge selbiger) ausgeloest,
  sondern hat einen anderen Grund, der genauer untersucht werden muss.
*/



#include <math.h>

#define DEBUGGER "menaures"
#include <debug.h>

// #define VERBOSE // Aktivieren, wenn man die crashausloesende Zeile sucht

#define MAX_ARGS    5
#define DELAY       4
#define ITERATIONS  1000

mixed execute;
mixed efuns;
mixed values;
mixed names;

mixed query() { return ({execute,efuns,values,names}); }

void record(string file);

void reset_all()
{
      execute = 0,

      ( efuns =
        ({
            #'Name,
            #'[,
            #'abs,
            #'abs_path,
            #'acos,
            #'add_action,
            #'add_dot_to_msg,
//            #'add_verb,
//            #'add_xverb,
            #'all_environment,
            #'all_inventory,
            #'allocate,
//            #'allocate_mapping,
            #'and_bits,
            #'apply,
            #'arr_delete,
            #'asin,
//            #'assoc,
            #'atan,
            #'atan2,
            #'attach_erq_demon,
            #'auto_owner_search,
            #'binary_message,
            #'bind_lambda,
//            #'break_point,
            #'call_other,
            #'call_out,
            #'call_out_info,
            #'call_resolved,
            #'caller_stack,
            #'caller_stack_depth,
            #'capitalize,
            #'cat,
            #'catch,
            #'ceil,
            #'center,
            #'clear_bit,
            #'clock,
            #'clone_object,
            #'clonep,
//            #'clones,
            #'closurep,
            #'command,
            #'command_stack,
            #'command_stack_depth,
            #'cond_present,
            #'convert_message,
            #'convert_umlaute,
            #'copies,
            #'copy,
            #'copy_bits,
            #'copy_file,
//            #'copy_mapping,
            #'cos,
            #'count_bits,
            #'crypt,
            #'ctime,
//            #'db_affected_rows,
//            #'db_close,
//            #'db_coldefs,
//            #'db_connect,
//            #'db_conv_string,
//            #'db_error,
//            #'db_exec,
//            #'db_fetch,
//            #'db_handles,
//            #'db_insert_id,
            #'debug_info,
            #'debug_message,
            #'deep_copy,
            #'deep_inventory,
            #'deep_present,
            #'dein,
            #'deinem,
            #'deinen,
            #'deines,
            #'dem,
            #'den,
            #'der,
            #'des,
            #'destruct,
            #'diesem,
            #'diesen,
            #'dieser,
            #'dieses,
            #'disable_commands,
            #'domain2map,
            #'ed,
//            #'efun308,
            #'ein,
            #'einem,
            #'einen,
            #'eines,
            #'enable_commands,
            #'environment,
            #'er,
            #'exec,
            #'execute_command,
            #'exp,
            #'expand_define,
            #'explode,
            #'export_uid,
            #'extern_call,
//            #'extract,
//            #'file_name,
            #'file_path,
            #'file_size,
            #'file_time,
            #'filter,
//            #'filter_array,
            #'filter_indices,
//            #'filter_mapping,
            #'filter_objects,
            #'find_call_out,
            #'find_living,
            #'find_object,
            #'find_player,
            #'first_inventory,
            #'floatp,
            #'floor,
            #'format_seconds,
            #'format_vseconds,
            #'funcall,
            #'function_exists,
            #'functionlist,
//            #'garbage_collection,
            #'get_align_string,
            #'get_dir,
            #'get_error_file,
            #'get_eval_cost,
            #'get_extra_wizinfo,
            #'get_genitiv,
            #'get_type_info,
            #'get_unique_string,
            #'geteuid,
            #'getuid,
            #'gmtime,
            #'heart_beat_info,
            #'ihm,
            #'ihn,
            #'ihr,
            #'ihrem,
            #'ihren,
            #'ihres,
            #'implode,
            #'inherit_list,
            #'input_to,
//            #'insert_alist,
            #'interactive,
//            #'intersect_alist,
            #'intp,
            #'invert_bits,
            #'ist,
            #'lambda,
            #'last_bit,
            #'last_instructions,
            #'left,
            #'limited,
            #'liste,
            #'living,
            #'load_name,
            #'load_object,
            #'localtime,
            #'log,
            #'log_file,
            #'lower_case,
            #'m_allocate,
            #'m_contains,
            #'m_delete,
            #'m_indices,
            #'m_reallocate,
//            #'m_sizeof,
            #'m_values,
            #'make_shared_string,
            #'map,
            #'map2domain,
//            #'map_array,
            #'map_indices,
//            #'map_mapping,
            #'map_object,
            #'map_objects,
//            #'mapping_contains,
            #'mappingp,
            #'max,
            #'md5,
            #'member,
//            #'member_array,
            #'min,
            #'mixed2str,
            #'mixed_to_closure,
            #'mkdir,
            #'mkmapping,
            #'move_object,
            #'negate,
            #'next_bit,
            #'next_inventory,
            #'not_alone,
            #'notify_fail,
            #'object_info,
            #'object_name,
            #'object_time,
            #'objectp,
            #'or_bits,
//            #'order_alist,
            #'parse_com,
            #'parse_com_error,
            #'parse_com_error_string,
            #'player_exists,
            #'player_present,
            #'playerp,
            #'plural,
            #'pointerp,
            #'pol2xy,
            #'pow,
            #'present,
            #'present_clone,
            #'previous_object,
            #'printf,
            #'process_string,
            #'program_name,
            #'program_time,
            #'query_actions,
//            #'query_akkusativ,
            #'query_command,
//            #'query_dativ,
            #'query_deklin,
            #'query_deklin_adjektiv,
            #'query_deklin_ein_adjektiv,
            #'query_deklin_name,
//            #'query_dekliniert,
            #'query_editing,
//            #'query_genitiv,
            #'query_idle,
//            #'query_imp_port,
            #'query_input_pending,
            #'query_ip_name,
            #'query_ip_number,
            #'query_limits,
            #'query_living_name,
            #'query_livings,
            #'query_load_average,
            #'query_mud_port,
            #'query_notify_fail,
            #'query_once_interactive,
            #'query_pronom,
            #'query_real_player_level,
            #'query_shadowing,
            #'query_snoop,
            #'query_udp_port,
            #'query_up_time,
            #'query_verb,
            #'quote,
            #'raise_error,
            #'random,
            #'read_bytes,
            #'read_file,
            #'real_time_diff,
            #'referencep,
            #'regexp,
            #'regexplode,
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
            #'right,
            #'rm,
            #'rmdir,
            #'round,
            #'rusage,
            #'save_object,
            #'save_value,
            #'say,
            #'search_object,
            #'sein,
            #'seinem,
            #'seinen,
            #'seines,
            #'send_erq,
//            #'send_imp,
            #'send_udp,
//            #'set_auto_include_string,
            #'set_bit,
            #'set_buffer_size,
            #'set_combine_charset,
            #'set_connection_charset,
            #'set_driver_hook,
            #'set_environment,
            #'set_extra_wizinfo,
            #'set_extra_wizinfo_size,
            #'set_heart_beat,
            #'set_is_wizard,
            #'set_light,
            #'set_limits,
            #'set_living_name,
            #'set_modify_command,
            #'set_next_reset,
            #'set_prompt,
            #'set_this_object,
            #'set_this_player,
            #'seteuid,
            #'sgn,
            #'shadow,
            #'short_format_seconds,
            #'short_format_vseconds,
            #'shorttimestr,
            #'shortvtimestr,
            #'shout,
//            #'shutdown,
            #'sin,
            #'sizeof,
//            #'slice_array,
            #'snoop,
            #'sort_array,
            #'space,
            #'sprintf,
            #'sqrt,
            #'sscanf,
            #'str2int,
            #'string_parser,
            #'stringp,
            #'strip,
            #'strlen,
            #'strstr,
            #'substr,
//            #'swap,
            #'symbol_function,
            #'symbol_variable,
            #'symbolp,
            #'sys_log,
            #'tail,
            #'tan,
            #'tell_object,
            #'tell_room,
            #'terminal_colour,
            #'test_bit,
            #'this_interactive,
            #'this_object,
            #'this_player,
            #'throw,
            #'time,
            #'time_to_vtime,
            #'timestr,
            #'to_array,
            #'to_float,
            #'to_int,
            #'to_object,
            #'to_string,
            #'touch,
            #'trace,
            #'traceprefix,
            #'transpose_array,
            #'trim,
            #'typeof,
            #'unbound_lambda,
            #'unique_array,
            #'unmkmapping,
            #'unshadow,
            #'upper_case,
            #'users,
            #'utime,
            #'vclock,
            #'vtime,
            #'vtime_to_time,
            #'vtimestr,
            #'walk_mapping,
            #'wem,
            #'wen,
            #'wer,
            #'wessen,
            #'widthof,
            #'wizardshellp,
            #'wizlist_info,
            #'wrap,
            #'wrap_say,
            #'write,
            #'write_bytes,
            #'write_file,
            #'xor_bits,
            #'xy2pol,

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
            #'[..],
            #'[..<],
            #'[<..],
            #'[<..<],
            #'[..,
            #'[<..,
            #'({,
            #'([,
      }) ),

      ( values =
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
            "같같같같같같같같같같�",
            "\\/ "*32,
            "    !"*42,

            /* --- Objekte: --- */
            touch("/obj/fackel"),
            touch("/obj/rucksack"),
            touch("/obj/rope"),
            touch("/secure/master"),
//            clone_object("/obj/schiff"),
            clone_object("/obj/player"),
            clone_object("/obj/tisch"),

            /* --- Closures: --- */
            (: :),
            (: (: 1 :) :),
            #'sizeof,
//            #'garbage_collection,
            #'efun::input_to,
            symbol_function("query_long", touch("/i/item")),
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

        }) );


      if(pointerp(values)) {
      values += ({0,0,0});

      apply((: values[<3] = $1 :), &execute);
      apply((: values[<2] = $1 :), &values);
      apply((: values[<1] = $1 :), &efuns);
      }
      while(remove_call_out("execute_file") != -1);
      while(remove_call_out("do_replay") != -1);
      while(remove_call_out("do_record") != -1);

      seteuid(getuid());
}

int execute_file(string file)
{
    int line;
    string str;
    mixed restore;

    if(!stringp(file) || file_size(file) <= 0)
    {
        DEBUG("ERROR: Cannot execute "+file+": Invalid file.");
        return 0;
    }

    DEBUG("Executing: "+file);

    for(line = 1; (str=read_file(file, line, 2)) && strlen(str); line += 2)
    {
//        debug_message(sprintf("%#Q\n",str));
        restore = restore_value(str);
        rm("/save/crash/LAST_EXEC");
        write_file("/save/crash/LAST_EXEC",
          save_value( ({file, line}) ));
#ifdef VERBOSE
        debug_message("File: "+file+" Line: "+line+"\n");
#endif
//        debug_message(sprintf("REPLAY: %#Q %#Q -> %#Q %#Q\n", restore[0], restore[1], efuns[ restore[0] ], map( restore[1], (: values[$1] :) )));
        debug_message(sprintf("exec file %#Q line %#Q\n", file, line));
        catch(
          apply( efuns[ restore[0] ],
                 map( restore[1], (: values[$1] :) ) )
        );
    }

    return 1;
}

mixed query_values() { return values; }

int generate_file(string file)
{
    int i, h, efun_index, * value_index;

    if(!stringp(file) || !write_file(file, ""))
    {
        DEBUG("ERROR: Cannot write file: "+file);
        debug_message(wrap("ERROR: Cannot write file: "+file));

        seteuid(getuid());

        record("/save/crash/"+(mixed2str(utime())-"({,})"));

        return 0;
    }

    DEBUG("Recording: "+file);

    for(i = ITERATIONS; i--; )
    {
        efun_index = random(sizeof(efuns));
        value_index = ({});

        for(h = random(MAX_ARGS+1); h--; )
        {
            value_index += ({ random(sizeof(values)) });
        }

        write_file(file, save_value( ({efun_index, value_index}) ));
    }

    return 1;
}

void replay(string file)
{
    if (!file)
    {
        file = names[0];
        names = names[1..];
    }
    DEBUG("Stopping all actions and starting REPLAY for: "+file);

    reset_all();

    execute = ({ #'call_out, "do_replay", DELAY, file, 1 });

    // Um this_player etc. loszuwerden einen Reset-Hack ;o)
    set_next_reset(DELAY);
}

static void do_replay(string file, int number)
{
    if(execute_file(file + right(number, 5, "_0000")))
    {
        call_out("do_replay", DELAY, file, number+1);
    }
    else call_out("replay", DELAY, 0);
}

void record(string file)
{
    DEBUG("Stopping all actions and starting RECORD for: "+file);

    reset_all();

    execute = ({ #'call_out, "do_record", DELAY, file, 1 });

    // Um this_player etc. loszuwerden einen Reset-Hack ;o)
    set_next_reset(DELAY);
}

static void do_record(string file, int number)
{
    if(generate_file(file+right(number, 5, "_0000")))
    {
        call_out("execute_file", DELAY, file+right(number, 5, "_0000"));
        call_out("do_record", DELAY*2, file, number+1);
    }
}

void reset()
{
    // Um this_player etc. loszuwerden einen Reset-Hack ;o)
    if(execute)
    {
        apply(execute[0], execute[1..]);
        execute = 0;
    }
}

void debug_query(string file, int line)
{
    mixed restore;

    restore = restore_value(read_file(file, line, 2));

    rm("/save/crash/TEMP");
    write_file("/save/crash/TEMP",
               sprintf("apply( %Q, %Q )", efuns[ restore[0] ],
               map(restore[1], (: values[$1] :))));

    ({this_player()})->more("/save/crash/TEMP");
}

void debug_exec(string file, int line)
{
    mixed restore;

    restore = restore_value(read_file(file, line, 2));

    catch(
      apply( efuns[ restore[0] ], map(restore[1], (: values[$1] :)))
    );
}

void create()
{
    string str;
    mixed strs;

    reset_all();

    str = read_file("/save/crash/LAST_EXEC");

    if(str)
    {
        catch( apply( #'debug_query, restore_value(str) ),
               write_file("/save/crash/CRASHERS",
                 "-"*80 + "\n"
                 + read_file("/save/crash/TEMP")+ "\n" ) );
    }

#if 0
    strs = get_dir("/save/crash/1*");

    foreach(str : strs)
    {
        rm("/save/crash/"+str);
    }
#endif

#if 1
    call_out("record", 4, "/save/crash/"+(mixed2str(utime())-"({,})"));
#else
    names = ({
               "/save/crash/crash1"
            });
    call_out("replay", 4, 0);
#endif
}

/* --- End of file. --- */
