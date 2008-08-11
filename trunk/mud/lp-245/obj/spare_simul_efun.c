#pragma strong_types
#pragma save_types

/* obj/simul_efun.c
 *
 * The simul-efun object provides functions which can be accessed as
 * if they were true efuns. This is most useful to protect sensitive
 * efuns from clumsy fingers, but is also used to simulate functions
 * which formerly were implemented by the driver.
 */

#define MAX_LOG_SIZE 50000

#define BACKBONE_WIZINFO_SIZE 5

#define LIVING_NAME 3
#define NAME_LIVING 4

#include "/sys/wizlist.h"
#include "/sys/erq.h"
#include "/sys/files.h"

mapping living_name_m, name_living_m;
  /* Living -> Name and Name -> Living mappings.
   */

//---------------------------------------------------------------------------
void start_simul_efun()

/* Activate the simul-efun object.
 */

{
    mixed *info;

    if ( !(info = get_extra_wizinfo(0)) )
	set_extra_wizinfo(0, info = allocate(BACKBONE_WIZINFO_SIZE));
    if (!(living_name_m = info[LIVING_NAME]))
	living_name_m = info[LIVING_NAME] = m_allocate(0, 1);
    if (!(name_living_m = info[NAME_LIVING]))
	name_living_m = info[NAME_LIVING] = m_allocate(0, 1);
}

//---------------------------------------------------------------------------
void ls (string path)

/* Print the directory listing of <path>, like the unix command.
 */

{
    int max, i, len, tmp;
    status trunc_flag;
    mixed *dir;
    set_this_object(previous_object());
    dir = get_dir (path, GETDIR_NAMES|GETDIR_SIZES);
    if (path != "/")
	path += "/";
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
            write("                                                                                "[80-max+tmp..]);
            len += max;
        }
    }
    write("\n");
    if (trunc_flag) write("***TRUNCATED***\n");
}

//---------------------------------------------------------------------------
string create_wizard(string owner, string domain)
{
    mixed result;

    set_this_object(previous_object());
    result =
      (mixed)__MASTER_OBJECT__->master_create_wizard(owner, domain, previous_object());
    if (stringp(result)) return result;
    return 0;
}

//---------------------------------------------------------------------------
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

//---------------------------------------------------------------------------
void localcmd()
{
    string *verbs;
    int i,j;

    verbs = query_actions(this_player());
    for (i=0, j = sizeof(verbs); --j >= 0; i++) {
	write(verbs[i]+" ");
    }
    write("\n");
}

//---------------------------------------------------------------------------
mixed *unique_array(mixed *arr,string func,mixed skipnum)
{
    mixed *al, last;
    mapping m;
    int i, j, k, *ordinals;

    if (sizeof(arr) < 32)
        return efun::unique_array(arr, func, skipnum);
    for (ordinals = allocate(i = sizeof(arr)); i--; )
	    ordinals[i] = i;
    m = mkmapping(map_objects(arr, func), ordinals, arr);
    al = m_indices(m);
    ordinals = m_values(m, 0);
    arr = m_values(m, 1);
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
    return m_values(mkmapping(ordinals[k..], arr[k..]),0);
}

//---------------------------------------------------------------------------
varargs mixed snoop(mixed snoopee)
{
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
	    break;
	case  0:
	    write("Failed.\n");
	    break;
	case  1:
	    write("Ok.\n");
	    break;
    }
    if (result > 0) return snoopee;
}

//---------------------------------------------------------------------------
void notify_fail(mixed message)
{
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

//---------------------------------------------------------------------------
string version() {
    return __VERSION__;
}

//---------------------------------------------------------------------------
string query_host_name() {
    return __HOST_NAME__;
}

//---------------------------------------------------------------------------
nomask void set_environment() {}

nomask void set_this_player() {}

//---------------------------------------------------------------------------
varargs void add_worth(int value, object ob)
{
    mixed old;
#ifdef __COMPAT_MODE__
    switch (explode(object_name(previous_object()), "/")[0]) {
#else
    switch (explode(object_name(previous_object()), "/")[1]) {
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

//---------------------------------------------------------------------------
varargs void wizlist(string name)
{
    int i, pos, total_cmd;
    int *cmds;
    mixed *a;
    mixed *b;

    if (!name) {
        name = this_player()->query_real_name();
        if (!name)
        {
            write("Need to provide a name or 'ALL' to the wizlist function.\n");
            return;
        }
    }
    a = transpose_array(wizlist_info());
    cmds = a[WL_COMMANDS];
    a[WL_COMMANDS] = a[0];
    a[0] = cmds;

    a = unmkmapping(apply(#'mkmapping, a));
    cmds = a[0];
    a[0] = a[WL_COMMANDS];
    a[WL_COMMANDS] = cmds;

    if ((pos = member(a[WL_NAME], name)) < 0 && name != "ALL")
    {
        write("No wizlist info for '"+name+"' found.\n");
        return;
    }
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
        if (b[WL_GIGACOST] > 1000)
            printf("%-15s %5d %2d%% (%d)\t[%d%4dk,%5d] %6d %d\n",
              b[WL_NAME], b[WL_COMMANDS],
              b[WL_COMMANDS] * 100 / total_cmd, b[<1],
              b[WL_GIGACOST] / 1000,
              b[WL_COST] / 1000 + (b[WL_GIGACOST] % 1000) * 1000000000,
              b[WL_HEART_BEATS], b[WL_EXTRA], b[WL_ARRAY_TOTAL]
            );
        else
            printf("%-15s %5d %2d%% (%d)\t[%4dk,%5d] %6d %d\n",
              b[WL_NAME], b[WL_COMMANDS],
              b[WL_COMMANDS] * 100 / total_cmd, b[<1],
              b[WL_COST] / 1000 + (b[WL_GIGACOST] % 1000) * 1000000000,
              b[WL_HEART_BEATS], b[WL_EXTRA], b[WL_ARRAY_TOTAL]
            );
    }
    printf("\nTotal         %7d     (%d)\n\n", total_cmd, sizeof(cmds));
}

//---------------------------------------------------------------------------
void shout(string s)
{
    filter(users(), lambda(({'u}),({#'&&,
      ({#'environment, 'u}),
      ({#'!=, 'u, ({#'this_player})}),
      ({#'tell_object, 'u, to_string(s)})
    })));
}

//---------------------------------------------------------------------------
void set_living_name(string name)
{
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

//---------------------------------------------------------------------------
object find_living(string name)
{
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

//---------------------------------------------------------------------------
object find_player(string name)
{
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

/*===========================================================================
 * The following functions provide the necessary compatibility of this
 * compat-mode mudlib with a plain driver.
 * Just the parse_command() efun is not simulated.
 */

#ifndef __COMPAT_MODE__
//---------------------------------------------------------------------------
string function_exists (string str, object ob)
{
    string rc;

    rc = efun::function_exists(str, ob);
    return stringp(rc) ? rc[1..] : 0;
}

//---------------------------------------------------------------------------
string object_name(object ob)
{
    string rc;

    rc = efun::object_name(ob);
    return stringp(rc) ? rc[1..] : 0;
}

//---------------------------------------------------------------------------
string program_name(object ob)
{
    string rc;

    rc = efun::program_name(ob);
    return stringp(rc) ? rc[1..] : 0;
}

//---------------------------------------------------------------------------
string* inherit_list(object ob)
{
    string *rc;
    int i;

    rc = efun::inherit_list(ob);
    for (i = sizeof(rc); i-- > 0; )
        rc[i] = rc[i][1..];
    return rc;
}

//---------------------------------------------------------------------------
string to_string(mixed arg)
{
    string rc;

    rc = efun::to_string(arg);
    return objectp(arg) ? rc[1..] : rc;
}

//---------------------------------------------------------------------------
string creator(object ob)
{
    return getuid(ob);
}

//---------------------------------------------------------------------------
varargs void add_action(string fun, string cmd, int flag)
{
    if (fun == "exit")
        raise_error("Illegal to define a command to the exit() function.\n");

    efun::set_this_object(previous_object());
    if (cmd)
        efun::add_action(fun, cmd, flag);
}

//---------------------------------------------------------------------------
object present_clone (mixed obj, object env)
{
  if (stringp(obj) && '/' != obj[0])
      obj = "/"+obj;
  return efun::present_clone(obj, env);
}

//---------------------------------------------------------------------------
#endif /* __COMPAT_MODE__ */

#if !__EFUN_DEFINED__(present)
//---------------------------------------------------------------------------
varargs object present(mixed ob, object env)
{
    int specific, num, i;
    object found;
    string str;

    if (!env)
    {
        env = previous_object();
        specific = 0;
    }
    else
        specific = 1;

    if (objectp(ob))
    {
        /* Quick check: is ob there or not? */

        if (specific)
            return environment(ob) == env ? ob : 0;
        if (environment(ob) == env
         || (environment(env) && environment(ob) == environment(env))
           )
            return ob;
        return 0;
    }

    /* Search by name. Prepare the search parameters */
    if (2 != sscanf(ob, "%s %d", str, num))
    {
        num = 1;
        str = ob;
    }

    /* First, search in env by name */
    for (found = first_inventory(env), i = 0
        ; found
        ; found = next_inventory(env))
    {
        if (found->id(str) && ++i == num)
            break;
        if (!found)  /* may happen */
            break;
    }

    if (found || specific)
        return found;

    /* If not found, search in environment(env) by name */
    env = environment(env);
    if (!env)
        return 0;
    if (env->id(ob))
        return env;
    if (!env) /* the id() may have destructed env */
        return 0;

    for (found = first_inventory(env), i = 0
        ; found
        ; found = next_inventory(env))
    {
        if (found->id(str) && ++i == num)
            break;
        if (!found)  /* may happen */
            break;
    }

    return found;
}

#endif /* !efun_defined(present) */

#if !__EFUN_DEFINED__(transfer)
//---------------------------------------------------------------------------
/*
 * Transfer an object from an object to an object.
 * Call add_weight(), drop(), get(), prevent_insert(), add_weight(),
 * and can_put_and_get() where needed.
 * Return 0 on success, and special code on failure:
 *
 * 1: To heavy for destination.
 * 2: Can't be dropped.
 * 3: Can't take it out of it's container.
 * 4: The object can't be inserted into bags etc.
 * 5: The destination doesn't allow insertions of objects.
 * 6: The object can't be picked up.
 */
int transfer(object item, object dest)
{
    int weight;
    object from;

    efun::set_this_object(previous_object());

    weight = item->query_weight();
    if (!item)
        return 3;

    from = environment(item);
    if (from)
    {
        /*
         * If the original place of the object is a living object,
         * then we must call drop() to check that the object can be dropped.
         */
        if (living(from))
        {
            if (item->drop() || !item)
                return 2;
        }
        /*
         * If 'from' is not a room and not a player, check that we may
         * remove things out of it.
         */
        else if (environment(from))
        {
            if (!from->can_put_and_get() || !from)
                return 3;
        }
    }

    /*
     * If the destination is not a room, and not a player,
     * Then we must test 'prevent_insert', and 'can_put_and_get'.
     */
    if (environment(dest) && !living(dest))
    {
        if (item->prevent_insert())
            return 4;
        if (!dest->can_put_and_get() || !dest)
            return 5;
    }

    if (living(dest))
    {
        if (!item->get() || !item)
            return 6;
    }

    /*
     * If it is not a room, correct the total weight in the destination.
     */
    if (environment(dest) && weight)
    {
        if (!dest->add_weight(weight) || !dest)
            return 1;
    }

    /*
     * If it is not a room, correct the weight in the 'from' object.
     */
    if (from && environment(from) && weight)
    {
        from->add_weight(-weight);
    }

    move_object(item, dest);

    return 0;
}

#endif /* !efun_define(transfer) */

#if !__EFUN_DEFINED__(extract)
//---------------------------------------------------------------------------
mixed extract (mixed data, varargs mixed*from_to)

{
    int from, to;

    if (!stringp(data) && !pointerp(data))
    {
        raise_error("Illegal type for extract(): must be string or array.\n");
        return 0;
    }

    switch(sizeof(from_to))
    {
    case 0: return data;
    case 1:
        if (!intp(from_to[0]))
        {
            raise_error("Illegal 'from' index for extract(): must be a number.\n");
            return 0;
        }
        from = from_to[0];
        if (from >= 0)
            return data[from..];
        return data[<-from..];
    case 2:
        if (!intp(from_to[0]) || !intp(from_to[1]))
        {
            raise_error("Illegal index for extract(): must be a number.\n");
            return 0;
        }
        from = from_to[0];
        to = from_to[1];
        if (from >= 0)
        {
            if (to >= 0)
                return data[from..to];
            return data[<from..<-to];
        }
        if (to >= 0)
            return data[<-from..to];
        return data[<from..<-to];
    }

    raise_error("Illegal number of arguments for extract().\n");
    return 0;
}

#endif /* !efun_defined(extract) */

/*************************************************************************/
