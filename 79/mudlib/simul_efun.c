/*
 * This is a mudlib file. Copy it to /obj/simul_efun.c, or
 * wherever the get_simul_efun() in master.c says.
 * The functions defined in this file should only be replacements of efuns
 * no longer supported. Don't use these functions any longer, use the
 * replacement instead.
 */

/*
  simul_efun.c
*/
#define MAX_LOG_SIZE 50000

#define BACKBONE_WIZINFO_SIZE 5

#define LIVING_NAME 3
#define NAME_LIVING 4

#include "/sys/wizlist.h"
#include "/sys/erq.h"
#if 0
#include "/secure/std.h"
#endif

#pragma strong_types
#pragma save_types
#ifdef COMPAT_FLAG
#define MASTER "obj/master"
#else
#define MASTER "secure/master"
#endif

/*
 * The ls() function is no longer needed, as get_dir() can do the same
 * work.
 */
void ls(string path) {
    int max, i, len, tmp;
    status trunc_flag;
    mixed *dir;
    set_this_object(previous_object());
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

int file_time(string path)
{
    mixed *v;

    set_this_object(previous_object());
    if (sizeof(v=get_dir(path,4))) return v[0];
}

string create_wizard(string owner, string domain)
{
    mixed result;

    set_this_object(previous_object());
    result =
      (mixed)MASTER->master_create_wizard(owner, domain, previous_object());
    if (stringp(result)) return result;
    return 0;
}

void log_file(string file,string str)
{
    string file_name;
    int *st;

    file_name = "/log/" + file;
#ifdef COMPAT_FLAG
    if (sizeof(regexp(({file}), "/")) || file[0] == '.' || strlen(file) > 30 )
    {
        write("Illegal file name to log_file("+file+")\n");
        return;
    }
#endif
    if ( sizeof(st = get_dir(file_name,2) ) && st[0] > MAX_LOG_SIZE) {
	catch(rename(file_name, file_name + ".old")); /* No panic if failure */
    }
    set_this_object(previous_object());
    write_file(file_name, str);
}

void localcmd() {
    string *verbs;
    int i,j;

    verbs = query_actions(this_player());
    for (i=0, j = sizeof(verbs); --j >= 0; i++) {
	write(verbs[i]+" ");
    }
    write("\n");
}

mixed *unique_array(mixed *arr,string func,mixed skipnum) {
    mixed *al, last;
    int i, j, k, *ordinals;

    if (sizeof(arr) < 32) return efun::unique_array(arr, func, skipnum);
    for (ordinals = allocate(i = sizeof(arr)); i--; )
	ordinals[i] = i;
    al = order_alist(map_objects(arr, func), ordinals, (ordinals=0,arr));
    arr = al[2];
    ordinals = al[1];
    al = al[0];
    if (k = i = sizeof(al)) {
	for (last = al[j = --i]; i--; ) {
	    if (al[i] != last) {
		if (last != skipnum) {
		    arr[--k] = arr[i+1..j];
		    ordinals[k] = ordinals[j];
		}
		last = al[j = i];
	    }
	}
	if (last != skipnum) {
	    arr[--k] = arr[0..j];
	    ordinals[k] = ordinals[j];
	}
    }
    return order_alist(ordinals[k..], arr[k..])[1];
}

#ifdef COMPAT_FLAG
mixed snoop(mixed snoopee) {
    int result;

    if (snoopee && query_snoop(snoopee)) {
        write("Busy.\n");
        return 0;
    }
    result = snoopee ? efun::snoop(this_player(), snoopee)
                     : efun::snoop(this_player());
    switch (result) {
	case -1:
	    write("Busy.\n");
	case  0:
	    write("Failed.\n");
	case  1:
	    write("Ok.\n");
    }
    if (result > 0) return snoopee;
}
#endif

/* You can remove this simul_efun if you have no objects in your mudlib that
 * use process_string in notify_fail, or if you change them to use closures.
 */
void notify_fail(mixed message) {
    if ( !(stringp(message) && strstr(message, "@@") < 0) ) {
	efun::notify_fail(message);
	return;
    }
    efun::notify_fail(
      funcall(
        bind_lambda(#'lambda, previous_object()),
        0, ({#'process_string, message})
      )
    );
}

string version() {
    return __VERSION__;
}

string query_host_name() {
    return __HOST_NAME__;
}

#ifndef NO_MAPPINGS
mapping m_delete(mapping m, mixed key) {
    return efun::m_delete(copy_mapping(m), key);
}
#endif

nomask void efun308() {}

nomask void set_this_player() {}

void add_worth(int value, object ob) {
    mixed old;
#ifdef COMPAT_FLAG
    switch (explode(file_name(previous_object()), "/")[0]) {
#else
    switch (explode(file_name(previous_object()), "/")[1]) {
#endif
      default:
	raise_error("Illegal call of add_worth.\n");
      case "obj":
      case "std":
      case "room":
    }
    if (!ob) {
	if ( !(ob = previous_object(1)) )
	    return;
    }
    if (intp(old = get_extra_wizinfo(ob)))
        set_extra_wizinfo(ob, old + value);
}

void wizlist(string name) {
    int i, pos, total_cmd;
    int *cmds;
    mixed *a;
    mixed *b;

    if (!name) {
        name = this_player()->query_real_name();
        if (!name)
            return;
    }
    a = transpose_array(wizlist_info());
    cmds = a[WL_COMMANDS];
    a[WL_COMMANDS] = a[0];
    a[0] = cmds;
    a = order_alist(a);
    cmds = a[0];
    a[0] = a[WL_COMMANDS];
    a[WL_COMMANDS] = cmds;
    if ((pos = member(a[WL_NAME], name)) < 0 && name != "ALL")
        return;
    b = allocate(sizeof(cmds));
    for (i = sizeof(cmds); i;) {
        b[<i] = i;
        total_cmd += cmds[--i];
    }
    a = transpose_array(a + ({b}) );
    if (name != "ALL") {
        if (pos + 18 < sizeof(cmds)) {
            a = a[pos-2..pos+2]+a[<15..];
        } else if (pos < sizeof(cmds) - 13) {
            a = a[pos-2..];
        } else {
            a = a[<15..];
        }
    }
    write("\nWizard top score list\n\n");
    if (total_cmd == 0)
        total_cmd = 1;
    for (i = sizeof(a); i; ) {
        b = a[<i--];
        printf("%-15s %5d %2d%% (%d)\t[%4dk,%5d] %6d %d\n",
          b[WL_NAME], b[WL_COMMANDS], b[WL_COMMANDS] * 100 / total_cmd, b[<1],
          b[WL_EVAL_COST] / 1000,
          b[WL_HEART_BEATS], b[WL_EXTRA], b[WL_ARRAY_TOTAL]
        );
    }
    printf("\nTotal         %7d     (%d)\n\n", total_cmd, sizeof(cmds));
}

void shout(string s) {
    filter_array(users(), lambda(({'u}),({#'&&,
      ({#'environment, 'u}),
      ({#'!=, 'u, ({#'this_player})}),
      ({#'tell_object, 'u, to_string(s)})
    })));
}

mapping living_name_m, name_living_m;

void start_simul_efun() {
    mixed *info;

    if ( !(info = get_extra_wizinfo(0)) )
	set_extra_wizinfo(0, info = allocate(BACKBONE_WIZINFO_SIZE));
    if (!(living_name_m = info[LIVING_NAME]))
	living_name_m = info[LIVING_NAME] = allocate_mapping(0, 1);
    if (!(name_living_m = info[NAME_LIVING]))
	name_living_m = info[NAME_LIVING] = allocate_mapping(0, 1);
    if (find_call_out("clean_simul_efun") < 0)
	call_out("clean_simul_efun", 1800);
    this_object()->start_simul_efun_dr();
}

static void clean_name_living_m(string *keys, int left) {
    int i, j;
    mixed a;

    if (left) {
	if (pointerp(a = name_living_m[keys[--left]]) && member(a, 0)>= 0) {
	    i = sizeof(a);
	    do {
		if (a[--i])
		    a[<++j] = a[i];
	    } while (i);
	    name_living_m[keys[left]] = a = j > 1 ? a[<j..] : a[<1];
	}
	if (!a)
	    efun::m_delete(name_living_m, keys[left]);
	call_out("clean_name_living_m", 1, keys, left);
    }
}

static void clean_simul_efun() {
    /* There might be destructed objects as keys. */
    m_indices(living_name_m);
    remove_call_out("clean_simul_efun");
    if (find_call_out("clean_name_living_m") < 0) {
	call_out(
	  "clean_name_living_m",
	  1,
	  m_indices(name_living_m),
	  sizeof(name_living_m)
	);
    }
    call_out("clean_simul_efun", 3600);
}

/* disable symbol_function('set_living_name, SIMUL_EFUN_OBJECT) */
protected void set_living_name(string name) {
    string old;
    mixed a;
    int i;

    if (old = living_name_m[previous_object()]) {
	if (pointerp(a = name_living_m[old])) {
	    a[member(a, previous_object())] = 0;
	} else {
	    efun::m_delete(name_living_m, old);
	}
    }
    living_name_m[previous_object()] = name;
    if (a = name_living_m[name]) {
	if (!pointerp(a)) {
	    name_living_m[name] = ({a, previous_object()});
	    return;
	}
	/* Try to reallocate entry from destructed object */
	if ((i = member(a, 0)) >= 0) {
	    a[i] = previous_object();
	    return;
	}
	name_living_m[name] = a + ({previous_object()});
	return;
    }
    name_living_m[name] = previous_object();
}

object find_living(string name) {
    mixed *a, r;
    int i;

    if (pointerp(r = name_living_m[name])) {
	if ( !living(r = (a = r)[0])) {
	    for (i = sizeof(a); --i;) {
		if (living(a[<i])) {
		    r = a[<i];
		    a[<i] = a[0];
		    return a[0] = r;
		}
	    }
	}
	return r;
    }
    return living(r) && r;
}

object find_player(string name) {
    mixed *a, r;
    int i;

    if (pointerp(r = name_living_m[name])) {
	if ( !(r = (a = r)[0]) || !query_once_interactive(r)) {
	    for (i = sizeof(a); --i;) {
		if (a[<i] && query_once_interactive(a[<i])) {
		    r = a[<i];
		    a[<i] = a[0];
		    return a[0] = r;
		}
	    }
	    return 0;
	}
	return r;
    }
    return r && query_once_interactive(r) && r;
}

void cindent(string name)
{
    string valid;

    valid = funcall(
	bind_lambda(#'call_other, previous_object()),
	__MASTER_OBJECT__,
	"valid_write",
	name,
#if __EFUN_DEFINED__(geteuid)
	geteuid(previous_object()),
#else
	0,
#endif
	"cindent", previous_object()
    );
    if (valid)
    send_erq(ERQ_FORK, "indent_wrapper " + (stringp(valid) ? valid : name));
}

/*
 * Function name: exclude_array
 * Description:   Deletes a section of an array
 * Arguments:     arr: The array
 *		  from: Index from which to delete elements
 *		  to: Last index to be deleted.
 * Returns:
 */
public mixed *
exclude_array(mixed *arr, int from, int to)
{
    return arr[0..from-1] + arr[to+1..sizeof(arr)];
}

/*
 * Function name: exclude_element
 * Description:   deletes an element from an array
 * Arguments:     arr: the array
 *                from: index of the element to remove from the array
 * Returns:       first element of the array, or -1 if failed.
 * Warning:       elements in the array will have been reshuffled somewhat.
 */
public mixed * exclude_element (mixed *arr, int index) {
        arr[index] = arr[0];
        return slice_array (arr, 1, sizeof (arr));
}


/*************************************************************************/

/* Efun: all_environment() */

/*
 * Function name: all_environment
 * Description:   Gives an array of all containers which an object is in, i.e.
 *		  match in matchbox in bigbox in chest in room, would for the
 *		  match give: matchbox, bigbox, chest, room
 * Arguments:     ob: The object
 * Returns:       The array of containers.
 */
public object *
all_environment(object ob)
{
  object *r;

  if (!ob || !environment(ob)) return 0;
  if (!environment(environment(ob)))
      return ({ environment(ob) });
  r = ({ ob = environment(ob) });
  while (environment(ob))
      r = r + ({ ob = environment(ob) });
  return r;
}

/*************************************************************************/

/* Efuns: break_string() and update_actions()
*/

static int gCol;

/*
 * Function name: break_string
 * Description:   Breaks a continous string without newlines into a string
 *		  with newlines inserted at regular intervalls replacing spaces
 *		  Each newline separeted string can be indented with a given
 *		  number of spaces.
 * Arguments:     str: Original message
 *		  width: The total maximum width of each line.
 *		  indent: (optional) How many spaces to indent with.
 * Returns:       A string with newline separated strings
 */
public varargs string
break_string(string str, int width, int indent)
{
    mixed *brkarg;
    string indstr;

    if (indent)
	indstr = extract("                                                                                ", 0, indent-1);
    else
	indstr = "";
    gCol = indent;
    brkarg = ({ width, indstr });

    return indstr +
	implode(map_array(explode(str+" "," "),"brk",this_object(),brkarg)," ");
}

static string brk(string word, mixed *width_indstr)
{
    int width;
    string indstr;

    width = width_indstr[0];
    indstr = width_indstr[1];
    if ((strlen(word)+gCol) > width) {
	gCol = strlen(indstr) + strlen(word) + 1;
	return "\n" + indstr + word;
    }
    else {
	gCol += strlen(word) + 1;
	return word;
    }
}

/*
 * Function name:   update_actions
 * Description:     Updates this_objects actions in all living objects
 */
public void
update_actions()
{
  int objs;

  if (environment(previous_object()))
    previous_object()->move(environment(previous_object()),1);
  filter_array(all_inventory(previous_object()),"move_inv",previous_object());
}

static void move_inv(object ob) { if (living(ob)) ob->move(previous_object(),1); }

/*************************************************************************/


/* Functions below are useful but should probably not be made into efuns
*/

#if 0
string creator(object ob) { return (string)SECURITY->creator_object(ob); }
#endif
#if 0
string domain(object ob) { return (string)SECURITY->domain_object(ob); }
#endif


static void dump_elem(mixed sak, string tab);

/*
 * Function name: dump_array
 * Description:   Dumps a variable with write() for debugging purposes.
 * Arguments:     a: Anything including an array
 */
public varargs void
dump_array(mixed *a, string tab)
{
  int n,m;

  if (!tab) tab = "";
  if (!pointerp(a)) {
    write(a); return;
  }
  else write("\n");
  m=sizeof(a);
  n=0;
  while (n<m) {
    write(tab+"["+n+"] = "); dump_elem(a[n],tab); n+=1;
  }
}

status is_clone(mixed ob) {
    return objectp(ob) && member(file_name(ob), '#') >= 0;
}

static void dump_elem(mixed sak, string tab)
{
  if (pointerp(sak)) {
    write("Array: \n"); dump_array(sak,tab+"   ");
  }
  else write(sak);
  write("\n");
}

#if 0
/*
 * The old 'slice_array' is no longer needed. Use range argument inside
 * a pair of brackets instead.
 */
mixed *slice_array(mixed *arr, int from, int to) {
    return arr[from..to];
}

/*
 * filter_objects() has been renamed to filter_array().
 */
mixed *filter_objects(mixed *list, string str, object ob, mixed extra) {
    return filter_array(list, str, ob, extra);
}

#endif
