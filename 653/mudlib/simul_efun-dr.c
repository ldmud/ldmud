/* This is an addition to the simul_efun file for a fast migration from
 * the -DR parser. Only the ls() function of the original simul_efun need to
 * be commented out
 */
#include "/sys/lpctypes.h"
#define ATTRIBUTE_INDEX 2
#define TEXT_WIDTH 70

/*
 * check_ref() is only a fake function.
 * missing: query_arg/this_arg(), remove_action()
 *
 * The following defines might be appropriate when you need to stay with
 * old code and want speed. Put them in an appropriate header file or in
 * the auto_include string.
 *
 * #define caller() previous_object()
 * if using OLD_PREVIOUS_OBJECT_BEHAVIOUR :
 * #define caller() previous_object(0) // always works, but slower
 *
 * #define contents(ob) all_inventory((ob))
 * #define deletem(m, d) efun::m_delete((m), (d))
 * #define this_verb() query_verb()
 * #define atoi(str) to_int(str)
 * #define keys(m) m_indices(m)
 * #define mapp(m) mappingp(m)
 *
 * The following defines have a problem: a parameter is used twice.
 * #define deletea(a, start, end) ((a)[0..(start)-1] + (a)[(end)+1..])
 * #define index(a, d, start)   member((start) ? (a)[(start)..] : (a) , (d))
 * #define searcha(a, d, start) member((start) ? (a)[(start)..] : (a) , (d))
 */

static void start_simul_efun_dr() {
    mixed *info;

    info = get_extra_wizinfo(0);
    if (!info[ATTRIBUTE_INDEX])
	info[ATTRIBUTE_INDEX] = allocate_mapping(0, 1);
    if (find_call_out("clean_simul_efun_dr") < 0)
	call_out("clean_simul_efun_dr", 900);
}

static void clean_simul_efun_dr() {
    /* There might be destructed objects as keys. */
    m_indices(get_extra_wizinfo(0)[ATTRIBUTE_INDEX]);
    remove_call_out("clean_simul_efun_dr");
    call_out("clean_simul_efun_dr", 3600);
}

varargs void add_attribute(string name, mixed value, object ob) {
    mapping attributes;

    if (!ob) {
	if (objectp(value)) {
	    ob = value;
	    value = 1;
	} else {
	    ob = previous_object();
	}
    }
    attributes =
      (attributes = get_extra_wizinfo(0)[ATTRIBUTE_INDEX])[ob] ||
      (attributes[ob] = allocate_mapping(1, 1));
    if (member(attributes, quote(name))) {
	attributes[quote(name)] = value;
	return;
    }
    attributes[name] = value;
}

varargs void add_prot_attribute(string name, mixed value, object ob) {
    mapping attributes;

    if (!ob) {
	if (objectp(value)) {
	    ob = value;
	    value = 1;
	} else {
	    ob = previous_object();
	}
    }
    efun::m_delete(
      ( (attributes = get_extra_wizinfo(0)[ATTRIBUTE_INDEX])[ob] ||
        (attributes[ob] = allocate_mapping(1, 1)) )
      , name)[quote(name)] = value;
}

#ifndef caller
object caller() {
    return previous_object(1);
}
#endif /* caller */

/* int */ mixed check_ref() {
    /* There are other ways to debug ref counts. Although a simul_efun is
     * possible, I doubt that it would be worth the trouble
     */
    return 42.42;
}

object *contents(object ob) {
    return all_inventory(ob);
}

varargs void delete_attribute(string name, object ob) {
    mapping attributes;

    if (attributes =
	  get_extra_wizinfo(0)[ATTRIBUTE_INDEX][ob || previous_object()])
    {
	if (member(attributes, quote(name)))
	    raise_error(
	      sprintf("Error, cannot delete protected attribute %s.\n", name));
	if ( !sizeof(efun::m_delete(attributes, name)) )
	    efun::m_delete(
	      get_extra_wizinfo(0)[ATTRIBUTE_INDEX],
	      ob || previous_object()
	    );
    }
}

varargs mixed deletea(mixed a, int start, int end)
{
    if (!end)			/* This isn't really exact; it should */
      end = start;		/* only be done if end is not given.  */

    if (end == sizeof(a) - 1)	/* Avoid "out of bounds" . */
      return a[0..start - 1];

    return a[0..start-1] + a[end+1..];
}

mapping deletem(mapping m, mixed d) {
    return efun::m_delete(m, d);
}

void dumpobjs(int start, int end) {
    object ob;

    rm("/OBJDUMP");
    if (end <= 0) {
	printf("Dumping all objects starting with %d.\n", start);
	end = 0x7fffffff;
    } else {
	printf("Dumping objects %d through %d.\n", start, end);
    }
    write_file("/OBJDUMP",
	sprintf("Object                                  Location\n"));
    for (ob = debug_info(start); ob && start <= end;
					ob = debug_info(2, ob), start++)
       write_file("/OBJDUMP", sprintf("%-40s %s\n", file_name(ob),
               (environment(ob)?file_name(environment(ob)):"None")));
    write("Done.\n");
}

int exists(string file) {
    return file_size(file) != -1;
}

string *files(string path) {
    mixed *a;
    string *b;
    int i, j;

    set_this_object(previous_object());
    a = get_dir(path, 3);
    /* Don't call this locally, unless you add code to set this_object() back */
    b = allocate(j = (i = sizeof(a)) >> 1);
    while (i)
	b[--j] = a[--i] < 0 ? a[--i] + "/" : a[--i];
    return b;
}

string format(string text, int width) {
    if (width < 0)
	raise_error("Bad width given to format(); must be >= 0.\n");
    return sprintf("%*-=s", width || TEXT_WIDTH, text);
}

varargs mixed get_attribute(mixed m, object ob) {
    mapping attributes;

    if (!stringp(m))
	return
	  transpose_array( ({
	    m_indices(
	      attributes =
		get_extra_wizinfo(0)
		  [ATTRIBUTE_INDEX][m || previous_object()] ||
		allocate_mapping(0,1)),
	    m_values(attributes)
	  }) ) ;
    if ( ( attributes =
	     get_extra_wizinfo(0)[ATTRIBUTE_INDEX][ob || previous_object()]
	 ) && (mapping_contains(&m, attributes, m) ||
	       mapping_contains(&m, attributes, quote(m)))
    )
	return m;
    return -1;
}

varargs string *grab_file(string path, int start, int len) {
    set_this_object(previous_object());
    return explode(read_file(path, start, len), "\n");
    /* Don't call this locally, unless you add code to set this_object() back */
}

int index(mixed a, mixed d, int start) {
    return member(start ? a[start..] : a , d);
}

string pad(mixed str, int len, int padchar) {
    string sp_format;

    str = to_string(str);
    if (len < 0) {
	if (padchar) {
	    sp_format = "%' '*.*s";
	    sp_format[2] = padchar;
	    return sprintf(sp_format, -len, -len, str);
	}
	return sprintf("%*.*s", -len, -len, str);
    }
    if (padchar) {
	sp_format = "%' '-*.*s%s";
	sp_format[2] = padchar;
	return sprintf(sp_format, len, len, str, "");
    }
    return sprintf("%-*.*s%s", len, len, str, "");
}

varargs void print(string text, int width) {
    if (width > TEXT_WIDTH)
	raise_error("Invalid size for argument 2 to print()\n");
    if (width < 0)
	raise_error("Bad width given to print(); must be >= 0.\n");
    write(sprintf("%*-=s", width || TEXT_WIDTH, text));
}

varargs mixed query_attribute(string m, object ob) {
    mapping attributes;

    return
      ( attributes =
	    get_extra_wizinfo(0)[ATTRIBUTE_INDEX][ob || previous_object()]
      ) && (member(attributes, m) || member(attributes, quote(m)));
}

static int remote_command_flag;
int command(string str, object ob) {
    /* Don't call this locally, unless you add code to set this_object() back */
    int result;
    object me;

    if (!ob) {
	set_this_object(previous_object());
	return efun::command(str);
    }
    if (ob == previous_object()) {
	set_this_object(previous_object());
	return efun::command(str);
    }
    remote_command_flag++;
    me = this_object();
    set_this_object(previous_object());
    result = efun::command(str, ob);
    set_this_object(me);
    remote_command_flag = 0; /* global variable... */
    return result;
}
int remote_command() { return remote_command_flag; }

int root(object ob) {
    return member(file_name(ob || previous_object()), '#') == -1;
}

int searcha(mixed a, mixed d, int start) {
    return member(start ? a[start..] : a , d);
}

static private string *strsub_formats = ({0, "\\<%s\\B","\\B%s\\>","\\<%s\\>"});
string strsub(string input,string *patterns, int mode) {
    int i;
    string format;

    if ( (i = sizeof(patterns)) & 1)
	raise_error("Odd length array given to strsub.\n");
    if (!mode) {
	while (i) {
	    input = implode(explode(input, patterns[<i--]), patterns[<i--]);
	}
	return input;
    }
    format = strsub_formats[mode];
    while (i) {
	input =
	  implode(
	    regexplode(input, sprintf(format, patterns[<i--])),
	    patterns[<i--]
	  );
    }
    return input;
}

string this_verb() { return query_verb(); }


int atoi(string str) { return to_int(str); }

int cp(string from, string to) {
    string str;

    set_this_object(previous_object());
    rm(to);
    str = read_file(from);
    write_file(to, str);
    /* Don't call this locally, unless you add code to set this_object() back */
}

int in_editor(object ob) { return query_editing(ob) && 1; }

mixed *keys(mapping m) { return m_indices(m); }

int mapp(mixed m) { return mappingp(m); }

int memused() {
    return rusage()[2];
}

object remove_shadow(object ob) {
    if (!(ob))
	if ( !(ob = previous_object()) )
	    return 0;
    /* we are missing a test here to test for actual shadowing. */
    funcall(bind_lambda(#'unshadow, ob || previous_object()));
    return ob;
}

mixed *reallocate(mixed *a, int size) {
    if (size > sizeof(a))
	return a + allocate(size - sizeof(a));
    return a[0..size-1];
}

string typeof(mixed m) {
    switch(efun::typeof(m)) {
      case T_OBJECT:
	return "object";
      case T_NUMBER:
	return "int";
      case T_POINTER:
	return "vector";
      case T_STRING:
	return "string";
      case T_MAPPING:
	return "mapping";
      default:
	return "";
    }
}

static int last_destruct;
static private mapping destructing = ([]);
varargs void destruct(object ob, int flag) {
    if (flag && !member(destructing, ob)) {
	if (last_destruct != time())
	    destructing = allocate_mapping(0,0);
	destructing += ([ob]);
	ob->destructor();
    }
    efun::destruct(ob);
}

/*
 * The ls() function is no longer needed, as get_dir() can do the same
 * work.
 */
void ls(string path) {
    int max, i, len, tmp;
    status trunc_flag;
    mixed *dir;
    set_this_object(previous_object());
    if (!path)
	path = ".";
    dir = get_dir (path,3);
#if defined(COMPAT_FLAG) && 0
    if (path[0] == '/')
	path = extract(path, 1);
    if (path != "")
	path += "/";
#else
    if (path != "/")
	path += "/";
#endif
    if (!dir) {
	write("No such directory.\n");
	return;
    }
    if (sizeof(dir) > 999)
    {
	dir = dir[0..998];
	trunc_flag = 1;
    }
    for(i = sizeof(dir); i--; ) {
	if(dir[i--] == -2)
	    dir[i]+="/";
	len = strlen(dir[i]);
	if (len > max)
	    max = len;
    }
    ++max;
    if (max > 79)
	max = 79;
    for (i=0; i < sizeof(dir); i+=2) {
	string name;
	    name = dir[i];
	tmp = strlen(name);
	if (len + tmp > 79) {
	    len = 0;
	    write("\n");
	}
	write(name);
	if (len + max > 79) {
	    write("\n");
	    len = 0;
	} else {
	    write(extract(
"                                                                                ",
		80-max+tmp));
	    len += max;
	}
    }
    write("\n");
    if (trunc_flag) write("***TRUNCATED***\n");
}

#if 0 /* This is probably too costly; just replace this_player(1) with
         this_interactive() */
/*
 * Not accurate.
 * Original behaviour: if arg _supplied_ return current_interactive.
 */

varargs object this_player(int x)
{
    return x ? this_interactive() : efun::this_player();
}
#endif
