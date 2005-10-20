#pragma strict_types
#pragma strong_types

/* obj/simul_efun_add.c
 *
 * This addition to the normal simul_efun object of the DT2 lib provides
 * the simpler adaptions of the LDMud driver for this mudlib.
 */

#include <debug_info.h>
#include <lpctypes.h>

#define MAX_LOG_SIZE 50000
 
//---------------------------------------------------------------------------
object * contents (object ob)
{
    return all_inventory(ob || previous_object());
} /* contents() */

//---------------------------------------------------------------------------
int cp (string from, string to)
{
    int rc;

    return  (catch(rc = copy_file(from, to)) || rc) ? 0 : 1;
} /* cp() */

//---------------------------------------------------------------------------
int destruct (object ob)
{
    efun::destruct(ob);
    return 1;
} /* destruct() */

//---------------------------------------------------------------------------
varargs int index (mixed arr, mixed item, int from)
{
    if (from)
        return member(arr[from..], item);
    return member(arr, item);
} /* index() */

//---------------------------------------------------------------------------
int in_editor (object ob)
{
    return query_editing(ob);
} /* in_editor() */

//---------------------------------------------------------------------------
int localcmd()
{
    string *verbs;
    int i,j;

    verbs = query_actions(this_player());
    for (i=0, j = sizeof(verbs); --j >= 0; i++) {
	write(verbs[i]+" ");
    }
    write("\n");

    return 0;
} /* localcmd() */

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

    return file_name;
} /* log_file() */

//---------------------------------------------------------------------------
int rename (string from, string to)
{
    int rc;

    return  (catch(rc = rename(from, to)) || rc) ? 0 : 1;
} /* rename() */

//---------------------------------------------------------------------------
int query_malloced()

{
   mixed * data;

   data = debug_info(DINFO_DATA, DID_MEMORY);

   if (data[DID_MEM_NAME] == "smalloc")
       return data[DID_MEM_LARGE_SIZE] - data[DID_MEM_SFREE_SIZE]
                                       - data[DID_MEM_UNUSED]
       ;

   return 4242; /* don't know*/
} /* query_malloced() */

//---------------------------------------------------------------------------
void set_ansi (object ob)

{
    /* This efun doesn't do anything */
} /* set_ansi() */

#if __EFUN_DEFINED__(swap)
//---------------------------------------------------------------------------
nomask void swap (object ob)
{
    if (query_su_level(previous_object()) < 10000)
        raise_error("swap(ob) - no permission.\n");
    efun::swap(ob);
}
#endif

//---------------------------------------------------------------------------
varargs void save_object (string file, object ob)
{
    efun::set_this_object(ob || previous_object());
    save_object(file);
} /* save_object() */

//---------------------------------------------------------------------------
varargs int restore_object (string file, object ob)
{
    if (ob && !interactive(ob) && -1 != strstr(file, "secure"))
        raise_error("Restore_object(\""+file
                   +"\") - not allowed to restore living!\n");
    efun::set_this_object(ob || previous_object());
    return restore_object(file);
} /* restore_object() */

//---------------------------------------------------------------------------
int query_su_level (object ob)
{
    return 0;
} /* query_su_level() */

//---------------------------------------------------------------------------
string query_su_name (object ob)
{
    if (load_name(ob)[0..8] == "/players/")
        return explode(load_name(ob), "/")[2];
    return 0;
} /* query_su_level() */

//---------------------------------------------------------------------------
int query_valid_file (string a, string b)
{
    return 0;
} /* query_valid_file() */

//---------------------------------------------------------------------------
int set_uid_file (string f, int m)
{
    return 0;
} /* set_uid_file() */

//---------------------------------------------------------------------------
varargs int set_uid_me (object ob, int m, string s)
{
    return 0;
} /* set_uid_me() */

//---------------------------------------------------------------------------
string typeof (mixed v)
{
    switch (efun::typeof(v))
    {
    case T_NUMBER:  return "int";
    case T_OBJECT:  return "object";
    case T_STRING:  return "string";
    case T_POINTER: return "array";
    }

    return "unknown";
} /* typeof() */

/*************************************************************************/

