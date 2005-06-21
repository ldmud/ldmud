// This is for crash purposes only. Don't run it in a production MUD.
//
// It may still depend on some UNItopia stuff, I haven't tested it with other
// libs. If you're using the UNItopia Mudlib to run it, you have to remove all
// nomask simul efuns (empty the file /secure/simul_efun/secure.inc), and
// unlock all privilege violations in /secure/master/compiler_control.inc by
// adding return 1; of the function.
//
// ------------------------------------------------------------------
// File:        crashleak.c
// Description: Try to crash the driver and/or find memory leaks
//              by calling EFuns and Operators with random arguments.
//              Based on my old crasher.c
// Author:      Menaures@UNItopia (2004-07-17)
//

/* --- Documentation: --- */

/* --- Inherits: --- */

/* --- Includes: --- */
#include <rtlimits.h>

// Replace this with some kind of output if you
// want Debug-Messages
#define DEBUG(x) {}

/* --- Defines: --- */

// Undef the following if you're not interested in Memleaks
#define FIND_MEMORY_LEAKS

// Where shall we write our data to?
#define CRASHLEAK_DIR      "/save/crashleak/"

// Where is the doc/efun directory of the driver package?
#define DOC_EFUN_DIR       CRASHLEAK_DIR"doc/efun/"

#define FILE(x,y)              (CRASHLEAK_DIR+l_time+"_"+sprintf("%'0'6d_%s", (y), (x)))
#define GC_FILE(x)             FILE("GC_"+(x), ++filecounter)
#define RECORD_FILE(x)         FILE("REC_"+(x), ++filecounter)
#define LEAK_FILE(x)           FILE("LEAK_"+(x), ++filecounter)
#define LOG_FILE(x)            FILE("LOG_"+(x), ++filecounter)

// How many calls shall we do per step?
#define CALLS_PER_STEP     1000

// How long to wait between steps?
// Careful, Memleak detection might not work for lower values!
#define STEP_PAUSE         4

// Sometimes objects get removed out of the values array.
// Do you want to reinitialize it every X steps?
// For maximum Memleak Find reliability, set to 1,
// which causes reinitialization in every step (halfing
// the speed)
#define REINIT_EVERY_N_STEPS        10

// This defines minimum and maximum number of arguments per
// function call. Recorded files may still be executed with
// old values if you change this.
#define MIN_PARAMS         0
#define MAX_PARAMS         15

// NOTE:
// When looking for Memleaks, only one step per PAUSE will be made.
// Thus, detecting all Memleaks of one real step will require
// a minimum of CALLS_PER_STEP*STEP_PAUSE time until the next step
// is done with full stress speed again. This is probably a long
// time, however: if there are few leaks, lower CALLS_PER_STEP
// will only add up to the time it takes to find one in the first
// place...

/* --- Global Variables: --- */

int l_time = time();   // Just as a unique number...
mixed * efun;          // EFun- and Operator-Closures. Either #'foo or ({#'foo, min_param, max_param})
mixed * value;         // Value-set which contains all Data Types.
int stepcounter;       // For reinit after N steps.
int filecounter;       // For file naming.
mixed * queue;         // Queue, to have one call-out-loop instead of many
#ifdef FIND_MEMORY_LEAKS
mixed * leakage;       // To track GCs and Execs for Leak detection.
#endif

/* --- Prototypes: --- */

void crash(string file, closure callback);
void leak(closure callback);

#ifdef FIND_MEMORY_LEAKS
void check_for_leak(string file, mixed * exec);
void step_by_step(string gc_file, string file, mixed * exec);
#endif

/* --- Functions: --- */

/* --- Queue --- */

// Basic Queue, mainly to prevent recursion problems.
// And to make call_out loops easier (we have more than one)

mixed * query_queue() { return queue; }

int push(int pos, varargs mixed * what)
{
    if(!pointerp(queue))
    {
        queue = ({});
    }

    if(pos <= 0)
    {
        pos = sizeof(queue) + 1;
    }

    queue = queue[0..pos-2] + ({ what }) + queue[pos-1..];
}

int pop()
{
    mixed q;

    if(pointerp(queue) && sizeof(queue))
    {
        q = queue[0];
        queue = queue[1..];
        apply(q[0],q[1..]);
        return sizeof(queue);
    }
}

void step()
{
    if(set_next_reset(0) > 300)
    {
        set_next_reset(150);
    }

    pop();
    call_out(#'step, STEP_PAUSE);
}

/* --- Crashleak Initialization: --- */

// Just to provide some LFun Closures below...
string foo() { return "foo"; }
mixed bar(mixed x, mixed y, mixed z) { return ({z,y,x}); }

mixed query_efun() { return efun; }

void init_efun()
{
    DEBUG("init_efun()");
    // Generate list of all EFuns. /doc/efun/ is part of the driver package.
    // If it isn't part of your mudlib directory, copy it somewhere in it
    // and modify the path accordingly.
    string * list = get_dir(DOC_EFUN_DIR"*") - ({".","..","[]"});

    // Convert them to EFun closures using a restore_value hack.

    efun = restore_value("#1:0\n({#e:"+implode(list, ",#e:")+",})\n");

    // Kill unrecognized and dangerous EFuns.
    efun -= ({0,
               #'efun::set_driver_hook, // it makes the crasher stop running
               #'efun::limited, // too much fun
               #'efun::set_this_object, // poses problems
              #'efun::garbage_collection, // Can't find no leaks otherwise
              #'efun::shutdown,           // Can't find no crashes otherwise
              #'efun::set_limits});       // Messes things up naturally

    // Add Operator closures. Have not found a way to generate this list
    // automatically yet, so you might want to check if it's still up2date.
    // See lex.c ~ approx line 1000 for a list (this one's of 2004/07/17).
    efun +=
    ({ #'+= ,#'++ ,#'+   ,#'-=   ,#'--   ,#'-     ,#'*= ,#'*   ,#'/=
      ,#'/  ,#'%= ,#'%   ,#',    ,#'^=   ,#'^     ,#'|| ,#'||= ,#'|=
      ,#'|  ,#'&& ,#'&&= ,#'&=   ,#'&    ,#'~     ,#'<= ,#'<<= ,#'<<
      ,#'<  ,#'>= ,#'>>= ,#'>>>= ,#'>>>  ,#'>>    ,#'>  ,#'==  ,#'=
      ,#'!= ,#'!  ,#'?!  ,#'?    ,#'[..] ,#'[..<] ,#'[<..]
      ,#'[<..<]   ,#'[..>]       ,#'[>..]         ,#'[<..>]
      ,#'[>..<]   ,#'[>..>]      ,#'[..           ,#'[<..
      ,#'[>..     ,#'[,]         ,#'[    ,#'[<    ,#'[>
      ,#'({       ,#'([  ,#'->   ,#'(<
    });
}

mixed query_values() { return value; }

    struct Bar {
       mixed four;
    };

    struct Foo {
       int        one, *two;
       struct Bar three;
    };

void init_value()
{
    DEBUG("init_value()");

    // The more values, the more variation, the better.
    // Feel free to add stuff to this list. However, don't modify it
    // on the fly, changing this array will make recorded files useless.

    // Please refrain from using self-referencing structures unless you
    // want to test the crash aspect only. Include self-referencing
    // stuff inside #ifndef FIND_MEMORY_LEAKS #endif blocks only.

    if(pointerp(value))
    {
        // Do some cleanup first:
        map(value, (: objectp($1) && destruct($1) :));
    }

    value = 0;
    value =
    ({
        /* --- Integer: --- */
        -16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,
        0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
        -1000,1000,-50000,50000,-2000000,2000000,
        __INT_MIN__+16,__INT_MAX__-16,__INT_MIN__,__INT_MAX__,

        /* --- Float: --- */
        0.0,0.001,-0.001,-0.5,0.5,0.99,-0.99,1.01,-1.01,
        42.767,-42.767,1.0,2.0,3.0,-1.0,-2.0,-3.0,
        to_float(__INT_MIN__), to_float(__INT_MAX__),
        1.0e+100, -1.0e+100,

        /* --- String: --- */
        "", " ", "  ", "!_!_", "_!_!",
        "foo","bar","foo bar", "%d", "%Q", "0", "^^^^^^",
        "#^#         #^#",  "                ^",
        "^                      ", "  -   -   -                    - ",
        "? ?    ?    ?  ? ?  ?????   ????  ??", "",
        "\\/ "*32, "    !"*42, "/", "/..",
        "/a", "/d/", "/w/..",
        "%#Q%#Q%#Q","%d%-=80s%%",
        save_value(efun), save_object(),

        /* --- Object: --- */

        // This is mudlib specific, please provide some objects;
        // Preferred are Blueprints, Clones, Objects with Environment,
        // and Objects which have other objects inside (rooms).
        //
        // Note that random calls (for example to destruct()) might
        // remove objects; so they should be reloaded here.
        // A proper cleanup of old objects then would probably be good too.
        load_object("/obj/fackel"), clone_object("/obj/fackel"),
        load_object("/obj/player"), clone_object("/obj/player"),
        load_object("/i/item/message"),
        clone_object("/obj/rucksack"), clone_object("/obj/kurstafel"),
        clone_object("/obj/wizard_shell"), clone_object("/secure/obj/login"),
        load_object("/secure/obj/login"), load_object("/apps/error_db"),
        load_object("/apps/plugin"), clone_object("/obj/zauberstab"),
        load_object("/room/church"), load_object("/room/void"),
        load_object("/room/hell"), load_object("/room/death/death"),
        load_object("/room/rathaus/treppe"),
        load_object("/room/rathaus/konferenz"),
        load_object("/room/rathaus/forum"),
        clone_object("/obj/monster"), clone_object("/obj/monster"),
        clone_object("/obj/monster"), clone_object("/obj/monster"),

        /* --- Closure: --- */
        (: :), (: 1 :), (: 0 :), (: $2/$1 :),
        #'efun::max, #'efun::input_to, #'efun::this_player,
        #'efun::this_object, #'efun::call_out,
        symbol_function("query_long", load_object("/i/item")),
        symbol_function("query_real_name", load_object("/i/player/player")),
        #'foo, #'bar,
        function { return 2 * $1; },
        function int (int a) { return 2 * a; },
        function (int a) { return 2 * a; },
        lambda( ({'x, 'y}), ({#'?, ({#'>, 'x, 'y}), 'x, 'y}) ),
        unbound_lambda( ({'x}), ({#'==, 0, 'x}) ),
        (: for(;;); :), (: while($1); :),

        /* --- Array: --- */
        ({}), ({ ({}) }), ({ (: :) }), ({ 0 }), ({ 1 }),
        ({ "" }), ({ ([]) }), ({#'efun::min}),
        allocate(5), allocate(10,1), allocate(query_limits(1)[LIMIT_ARRAY]),
        ({'x, 'y}), ({#'?, ({#'>, 'x, 'y}), 'x, 'y}),
        ({'x}), ({#'==, 0, 'x}),

        /* --- Mapping: --- */
        ([:0]), ([:1]), ([:2]), (["a":"b"]),([([1]):({2,3})]),
        mkmapping( ({1,2,3,4,5,6,7,8,9,10}), ({1,2,3,4,5,6,7,8,9,10}) ),
        mkmapping( ({"1","2","3","4","5","6","7","8","9","10"}) ),
        ([ load_object("/apps/groups") ]),
        ([ #'efun::allocate ]),

        /* --- Symbol / Quoted: --- */
        'a, 'b, 'c, quote(allocate(10)),
        'dont_know_what_this_is_supposed_to_be_it_sure_is_fun,
        'whatever_floats_your_boat,

        /* --- Struct: --- */
    });

    // --- Lets add some special values. ---

    // Container inside Container inside...
    object ob = clone_object("/obj/rucksack");


    for(int i = 10; i--; )
    {
        object ob2 = clone_object("/obj/rucksack");
        ob->move(ob2);
        ob=ob2;
    }

    value += ({ob});

    // Some random strings. Good for crashing, bad for debugging...
    // So to make things reproducible, we generate them only once,
    // and only load them afterwards. Delete the Savefile if you
    // want new random strings.

    string str;

    if(str = read_file(CRASHLEAK_DIR"random_strings.o"))
    {
        value += restore_value(str);
    }

    else
    {
        string * random_strings = ({});

        for(int i = 10; i--; )
        {
            int * arr;

            arr = map(allocate(random(100)), (: random(256) :));
            random_strings += ({ to_string(arr) });
        }

        // Save these strings so that we can reuse them next time.
        write_file(CRASHLEAK_DIR"random_strings.o",
                   save_value(random_strings));

        // Don't forget to add them to the values array...
        value += random_strings;
    }

    // Structs
    struct Bar foo = (<Bar> 10);
    struct Foo bar = (<Foo> 1, ({"2",3}), (<Bar> 5));

    value += ({foo,bar})*20;
}

mixed do_plot_efun(closure cl)
{
    // DEBUG(sprintf("do_plot_efun(%#Q)",cl));

    object ob = this_object();

    int min_p = MIN_PARAMS;
    int max_p = MAX_PARAMS;

    for(int p = MAX_PARAMS; p >= MIN_PARAMS; p--)
    {


        string str = catch(apply(cl, allocate(p)); nolog);

        // DEBUG(sprintf("  str -> %#Q", str));

        if(str && strstr(str, "many arguments") != -1)
        {
            max_p--;
        }

        else if(str && strstr(str, "few arguments") != -1)
        {
            min_p++;
        }
    }

    set_this_object(ob);
    seteuid(0);
    seteuid(getuid());

    if(min_p != MIN_PARAMS || max_p != MAX_PARAMS)
    {
        return ({cl, min_p, max_p});
    }

    return cl;
}

void plot_efun()
{
    DEBUG("plot_efun()");

    efun = filter(efun, #'closurep);
    efun = limited(#'map, ({10000000}), efun, #'do_plot_efun);

    garbage_collection(GC_FILE("plot_efun"));
}

void init_crashleak(closure callback)
{
    DEBUG("init_crashleak()");

    // Sometimes init fails. Recall.
    push(1, #'init_crashleak, callback);

    string str;

    if(str = catch(limited(#'init_efun, ({1000000})); publish))
    {
        debug_message("CRASHLEAK: init_efun failed:\n"+str+"\n");
        return;
    }

    if(str = catch(limited(#'init_value, ({1000000})); publish))
    {
        debug_message("CRASHLEAK: init_value failed:\n"+str+"\n");
    }

    garbage_collection(GC_FILE("AFTER_INIT"));

    // Del the old entry
    queue = queue[1..];
    push(0, callback);

    // Determine EFun parameter counts next step.
    push(1, #'plot_efun);
}

void crashleak()
{
    DEBUG("crashleak()");

    if(stepcounter++ == REINIT_EVERY_N_STEPS)
    {
        init_crashleak(#'crashleak);
        stepcounter=0;
        return;
    }


#ifdef FIND_MEMORY_LEAKS
    push(0, #'crash, RECORD_FILE("cl"), #'check_for_leak);
#else
    push(0, #'crash, RECORD_FILE("cl"), #'crashleak);
#endif
}

/* --- Crash Execution: --- */

mixed * pick()
{
    DEBUG("pick()");

    mixed * erg = ({});

    int f, p;

    for(int i = CALLS_PER_STEP; i--; )
    {
        f = random(sizeof(efun));

        if(closurep(efun[f]))
        {
            p = MIN_PARAMS + random(MAX_PARAMS-MIN_PARAMS);
        }

        else
        {
            p = efun[f][1] + random(efun[f][2]-efun[f][1]);
        }


        erg +=
        ({
               // Pick EFun index.
            ({ random(sizeof(efun)),
               // Pick p parameter indices.
               map(allocate(p), (: random(sizeof(value)) :)),
               // Will be replaced with a string later:
               // (debugmessage of what was executed)
               0
            })
        });
    }

    return erg;
}

varargs void exec(mixed * exec, int quiet)
{
    DEBUG("exec()");

    string file;
    string text = "";
    object ob = this_object();

    debug_message("start_of_exec\n");

    foreach(mixed * x : exec)
    {
        mixed f = efun[x[0]];
        if(pointerp(f)) f=f[0];
        mixed * v = map(x[1], (: value[$1] :));
        string str = sprintf("%d %#Q\n", x[0],x[1]);

        // Note: This chance affects arrays by reference. :-)
        x[2] = str;

        debug_message(sprintf("exec: %s",str));

        set_this_object(ob);
        seteuid(0);
        seteuid(getuid());
        catch(apply(f, v); reserve (max(get_eval_cost()/2,100000)), nolog);

        if(get_eval_cost() < 100000)
        {
            break;
        }
    }

    set_this_object(ob);

    debug_message("end_of_exec\n");;
}

void crash(string file, closure callback)
{
    DEBUG("crash()");

    mixed * exec;

    /* --- Pick what we will execute in this run: --- */
    exec = limited(#'pick, ({1000000}));

    /* --- Record it to the file. --- */
    write_file(file, save_value(exec));

    /* --- Execute it. --- */
    limited(#'exec, ({1000000}), exec);

    funcall(callback, file, exec);
}

/* --- Leak detection: --- */

int analyze(string gc_file)
{
    DEBUG("analyze("+gc_file+")");

    string s = read_file(gc_file);

    if(!stringp(s))
    {
        debug_message(sprintf("CRASHLEAK: Could not read file %#Q (%#Q)!\n",gc_file,s));
        return 0;
    }

    string * str = explode(s, "\n");

    return sizeof(regexp(str, "freeing.*block"));
}

void leak_gc(string file, mixed * exec)
{
    DEBUG("leak_gc( ["+sizeof(exec)+"] )");

    string sbs = GC_FILE("sbs");

    // GC.
    garbage_collection(sbs);
    // Note this file will be created later.

    // Push.
    push(1, #'step_by_step, sbs, file, exec);

    // Execute.
    limited(#'exec, ({1000000}), exec, 1);
}

void step_by_step(string gc_file, string file, mixed * exec)
{
    DEBUG("step_by_step()");

// Okay. Binary search.

//  Was there a leak?
    if(analyze(gc_file))
    {
//  Whee! There was one. Let's see what we do.

//  How many execs are there which might have caused the leak?
        if(sizeof(exec) == 1)
        {
//  Only one? Okay, this is it then.
            DEBUG("LEAK: "+exec[0][2]+"\n");
            write_file(LEAK_FILE(explode(gc_file,"/")[<1]), exec[0][2]+"\n");

            // We haven't done a GC, so we can pop:
            call_out(#'pop, 0);
            return;
        }

//  Okay. Do the first half now...
        leak_gc(file, exec[..(sizeof(exec)/2)-1]);
//  ...push the second half into the first queue, coz we can't do
//     to garbage_collection() at once.
        push(1, #'leak_gc, file, exec[(sizeof(exec)/2)..]);
    }

    else
    {
        // We haven't done a GC, so we can pop:
        call_out(#'pop, 0);
    }

// And wait...
}

void check_for_leak(string file, mixed * exec)
{
    DEBUG("check_for_leak()");

    string gc_file = GC_FILE("check_for_leak");

    // Warning: This is actually executed at the end of the cycle,
    //          and not right now.
    garbage_collection(gc_file);
    // This means gc_file does not exist now yet.

    // Check previous leakage (their gc_file exists now)
    if(leakage && analyze(leakage[0]))
    {
        // We need to do a step by step further investigation
        // of this exec to find which call exactly has caused
        // the leak.
        push(0, #'step_by_step, leakage[0], leakage[1], leakage[2]);

        // Continue Crashing afterwards.
        push(0, #'crashleak);
    }

    else
    {
        // nothing to do. continue crashing.
        crashleak();
    }

    // Remember this leakage and test it in the next run.
    leakage = ({gc_file,file,exec});
}

/* --- Applied LFuns: --- */

void reset()
{
    if(sizeof(queue) <= 0)
    {
        push(0, #'init_crashleak, #'crashleak);
    }

    if(find_call_out(#'step) <= 0)
    {
        call_out(#'step, STEP_PAUSE);
    }
}

void create()
{
    DEBUG("create()");

    garbage_collection(GC_FILE("AFTER_CREATE"));

    reset();
}

/* --- End of file. --- */
