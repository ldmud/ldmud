/*---------------------------------------------------------------------------
 * Wizard List
 *
 *---------------------------------------------------------------------------
 * The Wizard List ("wizlist") started out as a means to determine a high
 * score list of the most popular castles. Over the time, this list
 * was generalized into a simple user-id management with driver-internal
 * purposes.
 *
 * A user-id ("wizard name", "creator") is a unique string, and every object
 * is associated with a uid through the master-apply get_wiz_name(object-name).
 * The reverse doesn't hold true: the mudlib can use uids which are not
 * associated with any object.
 *
 * A special uid is the number 0: this is denotes the 'default' wizard,
 * ie. the system itself. Together with the .extra information feature,
 * the wizlist entry for uid 0 can be used to store information "on driver
 * level".
 *
 * Every uid represented with a wizlist entry of the following structure.
 * Object structures even contain a '.user' entry pointing directly
 * to the associated wizlist entry.
 *
 *   struct wiz_list_s {
 *      wiz_list_t *next;
 *      string_t *name;
 *      int32  score;
 *      int32  cost;
 *      int32  gigacost;
 *      int32  total_cost;
 *      int32  total_gigacost;
 *      int32  heart_beats;
 *      mp_int size_array;
 *      mp_int mapping_total;
#ifdef USE_STRUCTS
 *      mp_int struct_total;
#endif
 *      svalue_t extra;
 *      int32  last_call_out;
 *      int32  call_out_cost;
 *      string_t *file_name;
 *      string_t *error_message;
 *      int32 line_number;
 *  };
 *
 * .name is the tabled uid string of this wizlist entry.
 * .next is the pointer to the next entry in the wizlist.
 *
 * .score, .(total_)cost+.(total_)gigacost, .heart_beats, .size_array and
 * .mapping_total collect statistics about the objects for this uid/wizard.
 * .score is the number of action functions executed (weighted over time),
 * .cost+.gigacost the eval ticks spent (weighted over time),
 * .total_cost+.total_gigacost the eval ticks spent (total), .heart_beats the
 * number of heart_beat() calls (weighted over time).  .size_array and
 * .mapping_total give the number of values held in arrays and mappings for
 * this wizard.
 *
 * .extra offer space for one svalue which can be used by the mudlib
 * for its own purposes. The driver can be instructed to fill the .extra
 * member every newly created entry with an empty array of a specific
 * size. Especially the .extra member for uid 0 can be used to store
 * data persistent to the whole driver run - it even survives reloads
 * of the master object.
 *
 * .last_call_out and .call_out_cost are used to manage the call_out.
 * .call_out_cost is the collected execution cost of all call_outs
 * executed for time .last_call_out for this .user. When executing
 * call_outs, this value is used to prime the eval_cost counter, and
 * thus prevents call_outs from hogging all the cpu.
 *
 * .file_name, .error_message and .line_number are used to communicate
 * errors from the driver to the mudlib. Bit 30 (0x40000000) in line_number
 * is the "forget flag": it is set when the mudlib queries the error
 * information. This way the mudlib can distinguish old from new
 * errors.
 *
 *
 * Part of the wizard list information is stored in the toplevel file
 * "WIZLIST". The file is organized in lines, one for every wizard.
 * Each line must at least contain:
 *
 *    <name> <score>
 *
 * No leading space, and <name> and <score> must be separated by one or
 * more spaces. If existing, this information is read by the driver at
 * startup and added(!) to the already existing wizlist entries.
 *
 * The mudlib is free to add more information after the <score> and read
 * it itself. For the same reason it is the task of the mudlib to write
 * the WIZLIST file.
 *---------------------------------------------------------------------------
 */

#include "driver.h"
#include "typedefs.h"

#include "my-alloca.h"

#include <stdio.h>

#include "wiz_list.h"
#include "../mudlib/sys/wizlist.h"

#include "array.h"
#include "backend.h"
#include "gcollect.h"
#include "interpret.h"
#include "main.h"
#include "mapping.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"
#include "stdstrings.h"
#include "svalue.h"
#include "xalloc.h"

/*-------------------------------------------------------------------------*/
wiz_list_t default_wizlist_entry
  /* The default wizlist entry which is used for all "system" purposes.
   */
  = { NULL          /* next */
    , NULL          /* name */
    , 0             /* score */
    , 0             /* cost */
    , 0             /* gigacost */
    , 0             /* total_cost */
    , 0             /* total_gigacost */
    , 0             /* heart_beats */
    , 0             /* size_array */
    , 0             /* mapping_total */
#ifdef USE_STRUCTS
    , 0             /* struct_total */
#endif
    , { T_NUMBER }  /* extra */
    , 0             /* last_call_out */
    , 0             /* call_out_cost */
    , NULL          /* error file_name */
    , NULL          /* error_message */
    , 0             /* error line_number */
  };

wiz_list_t *all_wiz = NULL;
  /* The list of all wizard entries, sorted by the numeric value
   * of <name>. If the names are queried equally often, this even yields
   * an average O(1) complexity.
   */

static int wiz_info_extra_size = -1;
  /* Default size of the .extra field, -1 if unspecified.
   */

static int number_of_wiz = 0;
  /* Number of entries in the list.
   */

char wizlist_name[MAXPATHLEN+1] = "";
  /* Name of the wizlist file, relative to the mudlib directory.
   */

/*-------------------------------------------------------------------------*/
void
name_wizlist_file (const char *name)

/* Set the swap file name to a copy of <name>.
 */

{
    /* Skip leading '/' */
    while (*name == '/') name++;

    xstrncpy(wizlist_name, name, sizeof wizlist_name);
    wizlist_name[sizeof wizlist_name - 1] = '\0';
} /* name_wizlist_file()*/

/*-------------------------------------------------------------------------*/
size_t
wiz_list_size (void)

/* Return the allocated size of the wiz_list.
 */

{
    /* The extra wizinfo has been counted with the arrays */
    return sizeof(wiz_list_t) * number_of_wiz;
} /* wiz_list_size() */

/*-------------------------------------------------------------------------*/
static wiz_list_t *
find_wiz (string_t *name)

/* Find the entry for the user 'name' and return it's pointer.
 * Return NULL if it can't be found.
 */

{
    wiz_list_t *wl;

    if ( !( name = find_tabled(name) ) )
        return NULL;

    for (wl = all_wiz; wl; wl = wl->next)
    {
        int rc = mstrcmp(wl->name, name);

        if (!rc)
            return wl;
        if (rc > 0)
            break;
    }
    return NULL;
} /* find_wiz() */

/*-------------------------------------------------------------------------*/
wiz_list_t *
add_name (string_t * str)

/* Check if an entry for wizard <str> exists; add it, if it doesn't.
 * Return the pointer to the wiz_list entry.
 * Must not change refcount of <str>.
 */

{
    wiz_list_t *wl;
    wiz_list_t *prev, *this;

    wl = find_wiz(str);
    if (wl)
        return wl;

    number_of_wiz++;
    wl = xalloc(sizeof (wiz_list_t));

    str = make_tabled_from(str);

    wl->next           = NULL;
    wl->name           = str;
    wl->score          = 0;
    wl->cost           = 0;
    wl->gigacost       = 0;
    wl->total_cost     = 0;
    wl->total_gigacost = 0;
    wl->heart_beats    = 0;
    wl->size_array     = 0;
    wl->mapping_total  = 0;
#ifdef USE_STRUCTS
    wl->struct_total   = 0;
#endif /* USE_STRUCTS */
#if 0
    wl->quota_allowance = 0;
    wl->quota_usage   = 0;
#endif
    wl->file_name     = NULL;
    wl->error_message = NULL;
    if (wiz_info_extra_size >= 0)
        put_array(&(wl->extra), allocate_array(wiz_info_extra_size));
    else
        wl->extra = const0;
    wl->last_call_out = 0;

    /* Find the insertion point and insert the new entry */
    for ( prev = NULL, this = all_wiz
        ; this && mstrcmp(this->name, str) < 0
        ; prev = this, this = this->next
        )
    { NOOP; }

    if (!prev)
    {
        wl->next = all_wiz;
        all_wiz = wl;
    }
    else
    {
        wl->next = this;
        prev->next = wl;
    }

    return wl;
} /* add_name() */

/*-------------------------------------------------------------------------*/
void
wiz_decay (void)

/* Called after every complete walkaround of the reset, this 'decays'
 * the score of every wizard once per hour.
 * Together with the decay, the wizlist is checked for destructed extra
 * data.
 */

{
    wiz_list_t *wl;
    static int next_time;

    /* Only once per hour */
    if (next_time > current_time)
        return;

    next_time = current_time + 60 * 60;
    for (wl = all_wiz; wl; wl = wl->next)
    {
        wl->score = wl->score * 99 / 100;
        wl->cost = wl->cost * .9;
        wl->gigacost = wl->gigacost * .9;
        wl->heart_beats = wl->heart_beats * 9 / 10;
    }

    check_wizlist_for_destr();
} /* wiz_decay() */

/*-------------------------------------------------------------------------*/
static string_t *
get_wiz_name (const char *file)

/* For the filename <file> return the name of the wizard responsible
 * for it. This actual query is done with a master apply.
 *
 * Result is NULL or an uncounted string reference.
 */

{
    svalue_t *ret;

    /* Don't call the master if it isn't loaded! */
    if (!master_ob)
        return NULL;

    push_c_string(inter_sp, file);
    ret = apply_master(STR_GET_WNAME, 1);
    if (ret == 0 || ret->type != T_STRING)
        return NULL;
    return ret->u.str;
} /* get_wiz_name() */

/*-------------------------------------------------------------------------*/
void
load_wiz_file (void)

/* Load the old wizlist from the wizlist file and add it's data to
 * the wizlist already in memory.
 *
 * This function is called at driver start up.
 * TODO: Since the wizlist is saved from the mudlib, this function
 * TODO:: should be implemented on mudlib level, too.
 */

{
    char buff[1000];
    FILE *f;

    if (wizlist_name[0] == '\0')
        return;

    f = fopen(wizlist_name, "r");
    if (f == NULL)
        return;

    while (fgets(buff, sizeof buff, f) != NULL)
    {
        char *p;
        uint32 score;

        p = strchr(buff, ' ');
        if (p == 0)
        {
            fprintf(stderr, "%s Bad WIZLIST file '%s'.\n"
                          , time_stamp(), wizlist_name);
            break;
        }
        *p = '\0';
        p++;
        if (*p == '\0')
        {
            fprintf(stderr, "%s Bad WIZLIST file '%s'.\n"
                          , time_stamp(), wizlist_name);
            break;
        }
        score = atoi(p);
        if (score > 0)
        {
            string_t * tmp;

            tmp = new_mstring(buff);
            add_name(tmp)->score += score;
            free_mstring(tmp);
        }
    }
    fclose(f);
} /* load_wiz_file() */

/*-------------------------------------------------------------------------*/
void
remove_wiz_list (void)

/* Remove all memory allocated by the wizlist.
 *
 * Called from simulate::shutdowngame().
 */

{
    wiz_list_t *wl, *w;

    for (w = all_wiz; w; w = wl)
    {
        free_mstring(w->name);
        if (w->file_name)
            free_mstring(w->file_name);
        if (w->error_message)
            free_mstring(w->error_message);
        wl = w->next;
        xfree(w);
    }
} /* remove_wiz_list() */

/*-------------------------------------------------------------------------*/
void
save_error (const char *msg, const char *file, int line)

/* A runtime error <msg> occured for object <file> in line number <line>.
 * Store this information in the wizlist so that the mudlib can handle
 * it later with the efun get_error_file().
 * TODO: A proper runtime error handling could put this into the mudlib
 * TODO:: completely.
 */

{
    wiz_list_t *wl;
    char *copy, *p;
    string_t *name;
    size_t len;

    /* Get the wizard name and the wizlist entry. */
    name = get_wiz_name(file);
    if (!name)
        return;
    wl = add_name(name);

    /* Set the file_name */
    if (wl->file_name)
        free_mstring(wl->file_name);

    len = strlen(file);
    copy = alloca(len + 4); /* May add .c plus the null byte, and / */
    *copy = '/';
    strcpy(copy+1, file);

    /* If it is a cloned object, we have to find out what the file
     * name is, and add '.c'.
     */
    if ( NULL != (p = strrchr(copy, '#'))
     || ((p = copy+len), *p++ != 'c') || p[-2] != '.' )
    {
        p[0] = '.';
        p[1] = 'c';
        p[2] = '\0';
    }
    wl->file_name = new_mstring(copy);

    /* Set the error_message */
    if (wl->error_message)
        free_mstring(wl->error_message);
    wl->error_message = new_mstring(msg);

    /* Set the line_number */
    wl->line_number = line;
} /* save_error() */

/*=========================================================================*/

/*                            EFUNS                                        */

/*-------------------------------------------------------------------------*/
svalue_t *
f_wizlist_info (svalue_t *sp)

/* EFUN wizlist_info()
 *
 *    mixed *wizlist_info()
 *
 * Returns an array with the interesting entries of the wizlist.
 * Raises a privilege_violation (wizlist_info, this_object(), 0).
 *
 * The result is an array with one entry for every wizard (uid).
 * Every entry is an array itself:
 *
 *   string w[WL_NAME]        = Name of the wizard.
 *   int    w[WL_COMMANDS]    = Weighted number of commands execute by objects
 *                              of this wizard.
 *   int    w[WL_COST] and  w[WL_GIGACOST] = Weighted sum of eval_costs.
 *   int    w[WL_TOTAL_COST] and  w[WL_TOTAL_GIGACOST] = Total sum of
 *                              eval_costs.
 *   int    w[WL_HEART_BEATS] = Weighted count of heart_beats.
 *   int    w[WL_CALL_OUT]    = Reserved for call_out() (unused yet).
 *   int    w[WL_ARRAY_TOTAL] = Total size of arrays in elements.
 *   int    w[WL_MAPPING_TOTAL] = Total size of mappings in elements.
#ifdef USE_STRUCTS
 *   int    w[WL_STRUCT_TOTAL] = Total size of mappings in elements.
#endif
 *   mixed  w[WL_EXTRA]       = Extra wizlist-info if set.
 */

{
    vector_t *all, *entry;
    svalue_t *wsvp, *svp;
    wiz_list_t *w;

    if (!privilege_violation(STR_WIZLIST_INFO, &const0, sp))
    {
        all = allocate_array(0);
    }
    else
    {
        all = allocate_array(number_of_wiz);
        wsvp = all->item;
        for (w = all_wiz; w; w = w->next)
        {
            entry = allocate_array(WL_SIZE);
            put_array(wsvp, entry);
            wsvp++;
            svp = entry->item;
            put_ref_string(&(svp[WL_NAME]), w->name);
            put_number(&(svp[WL_COMMANDS]), w->score);
            put_number(&(svp[WL_COST]), w->cost);
            put_number(&(svp[WL_GIGACOST]), w->gigacost);
            put_number(&(svp[WL_TOTAL_COST]), w->total_cost);
            put_number(&(svp[WL_TOTAL_GIGACOST]), w->total_gigacost);
            put_number(&(svp[WL_HEART_BEATS]), w->heart_beats);
            put_number(&(svp[WL_CALL_OUT]), 0); /* TODO: Implement me */
            put_number(&(svp[WL_ARRAY_TOTAL]), w->size_array);
            put_number(&(svp[WL_MAPPING_TOTAL]), w->mapping_total);
#ifdef USE_STRUCTS
            put_number(&(svp[WL_STRUCT_TOTAL]), w->struct_total);
#else
            put_number(&(svp[WL_STRUCT_TOTAL]), 0);
#endif /* USE_STRUCTS */
            if (w->extra.type == T_POINTER)
            {
                vector_t *v = w->extra.u.vec;
                put_array(&(svp[WL_EXTRA]), slice_array(v, 0, VEC_SIZE(v) - 1));
            }
            else
                assign_svalue_no_free(&(svp[WL_EXTRA]), &w->extra);
        } /* end for */
    } /* end if */

    push_array(sp, all);
    return sp;
} /* f_wizlist_info() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_extra_wizinfo (svalue_t *sp)

/* EFUN set_extra_wizinfo()
 *
 *   void set_extra_wizinfo (object wiz, mixed extra)
 *   void set_extra_wizinfo (string wiz, mixed extra)
 *   void set_extra_wizinfo (int    wiz, mixed extra)
 *
 * Set the value <extra> as the 'extra' information for the wizlist
 * entry of <wiz>.
 *
 * If <wiz> is an object, the entry of its creator (uid) is used.
 * If <wiz> is a string (a creator aka uid), it names the entry
 * to use.
 * If <wiz> is the number 0, the data is set in the default wizlist
 * entry. It can be used to store data for the lifetime of this
 * driver run, like the time of the last reboot.
 *
 * <extra> can be any value.
 *
 * The function causes a privilege violation
 * ("set_extra_wizinfo", this_object(), <wiz>).
 */

{
    wiz_list_t *user;
    short type;

    if ((type = sp[-1].type) == T_OBJECT)
    {
        user = sp[-1].u.ob->user;
    }
    else if (type != T_STRING || !(user = find_wiz(sp[-1].u.str)))
    {
        if (type == T_NUMBER && sp[-1].u.number == 0)
            user = NULL;
        else
            efun_gen_arg_error(1, sp->type, sp);
    }

    if (!privilege_violation(STR_SET_EXTRA_WIZINFO, sp-1, sp))
        free_svalue(sp);
    else
        transfer_svalue(user ? &user->extra : &default_wizlist_entry.extra, sp);

    free_svalue(sp-1);

    return sp - 2;
} /* f_set_extra_wizinfo() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_get_extra_wizinfo (svalue_t *sp)

/* EFUN get_extra_wizinfo()
 *
 *   mixed get_extra_wizinfo (object wiz)
 *   mixed get_extra_wizinfo (string wiz)
 *   mixed get_extra_wizinfo (int    wiz)
 *
 * Returns the 'extra' information that was set for the given
 * wizard <wiz> in the wizlist.
 *
 * If <wiz> is an object, the entry of its creator (uid) is used.
 * If <wiz> is a string (a creator aka uid), it names the entry
 * to use.
 * If <wiz> is the number 0, the data is get from the default wizlist
 * entry.
 *
 * The function causes a privilege violation
 * ("get_extra_wizinfo", this_object(), <wiz>).
 */

{
    wiz_list_t *user;
    short type;

    if ((type = sp->type) == T_OBJECT)
    {
        user = sp->u.ob->user;
    }
    else if (type != T_STRING || !(user = find_wiz(sp->u.str)))
    {
        if (type == T_NUMBER && sp->u.number == 0)
            user = NULL;
        else
            errorf("Bad arg 1 to get_extra_wizinfo(): no valid uid given.\n");
    }

    if (!privilege_violation(STR_GET_EXTRA_WIZINFO, sp, sp))
        errorf("Error in get_extra_wizinfo(): privilege violation.\n");

    assign_svalue(sp, user ? &user->extra : &default_wizlist_entry.extra);

    return sp;
} /* get_extra_wizlist_info() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_set_extra_wizinfo_size (svalue_t *sp)

/* EFUN set_extra_wizinfo_size()
 *
 *   void set_extra_wizinfo_size(int size)
 *
 * Indicate that the wizlist should contain an array of this size
 * with extra info foreach wizard. A negative size is used to
 * indicate non-array 'extra' information.
 *
 * Causes the privilege violation
 * ("set_extra_wizinfo_size", this_object(), size).
 *
 * The value is only used to allocate a proper empty 'extra' value
 * for newly created wizlist entries.
 *
 * TODO: The extra_wizinfo idea could be applied to single objects
 * TODO:: and - ta da! - we have driver supported properties.
 * TODO:: Which then could be used to implement uids/euids etc.
 */

{
    if (!privilege_violation(STR_SET_EXTRA_WIZINFO_SIZE, &const0, sp))
        wiz_info_extra_size = sp->u.number;

    sp--;

    return sp;
} /* f_set_extra_wizinfo_size() */

/*-------------------------------------------------------------------------*/
svalue_t *
f_get_error_file (svalue_t *sp)

/* EFUN get_error_file()
 *
 *   mixed * get_error_file(string name, int set_forget_flag)
 *
 * Return information about the last error which occured for
 * <name> (where <name> is a valid name from the wiz list).
 *
 * Result is an array of four elements: the filename of the
 * program where the error occured, the linenumber in the
 * program, the error message (runtime error messages usually
 * start with a '*'), and a numerical flag (the 'forget flag') if
 * the error information has been queried already.
 *
 * If there is no error stored for the given <name>, 0 is
 * returned.
 *
 * If <set_forget_flag> is non-zero, the 'forget' flag is set
 * for the error message after it has been returned.
 */

{
    string_t *name;
    int forget;
    wiz_list_t *wl;
    vector_t *vec;
    svalue_t *v;
#   define FORGET_FLAG 0x4000000 /* 0x80...0 would be the sign! */

    /* Get the function arguments */
    name = sp[-1].u.str;
    forget = sp->u.number;
    wl = find_wiz(name);
    sp--;
    free_string_svalue(sp);

    /* The error_message is used as a flag if there has been any error.
     */
    if (!wl || !wl->error_message)
    {
        put_number(sp, 0);
        return sp;
    }

    vec = allocate_array(4);
    v = vec->item;
    put_ref_string(v, wl->file_name);
    put_number(v+1, wl->line_number & ~0x40000000);
    put_ref_string(v+2, wl->error_message);
    put_number(v+3, (wl->line_number & 0x40000000) != 0);

    if (forget)
        wl->line_number |= 0x40000000;

    put_array(sp, vec);
    return sp;

#   undef FORGET_FLAG
} /* f_get_error_file() */

/*=========================================================================*/

/*-------------------------------------------------------------------------*/
void
check_wizlist_for_destr (void)

/* Check the 'extra' info in all wizinfo and remove destructed objects
 * and closures.
 */

{
    wiz_list_t *wl;

    for (wl = &default_wizlist_entry; wl; )
    {
        size_t num;
        svalue_t *item;

        if (wl->extra.type == T_POINTER)
        {
            num = VEC_SIZE(wl->extra.u.vec);
            item = &(wl->extra.u.vec->item[0]);
        }
        else
        {
            num = 1;
            item = &(wl->extra);
        }

        for ( ; num != 0 ; item++, num--)
        {
            switch(item->type)
            {
            case T_POINTER:
                check_for_destr(item->u.vec);
                break;
            case T_MAPPING:
                check_map_for_destr(item->u.map);
                break;
            case T_OBJECT:
            case T_CLOSURE:
                if (destructed_object_ref(item))
                    assign_svalue(item, &const0);
                break;
            default:
                NOOP;
                break;
            }
        }

        if (wl == &default_wizlist_entry)
            wl = all_wiz;
        else
            wl = wl->next;
    }
} /* check_wizlist_for_destr() */

/*-------------------------------------------------------------------------*/
#ifdef GC_SUPPORT

void
clear_ref_from_wiz_list (void)

/* GC support: Clear the refs for the wiz_list memory.
 */

{
    wiz_list_t *w;

    for (w = all_wiz; w; w = w->next)
    {
        clear_ref_in_vector(&w->extra, 1);
        if(w->file_name)
            clear_string_ref(w->file_name);
        if(w->error_message)
            clear_string_ref(w->error_message);
    }
    clear_ref_in_vector(&default_wizlist_entry.extra, 1);
} /* clear_ref_from_wiz_list() */

/*-------------------------------------------------------------------------*/
void
count_ref_from_wiz_list (void)

/* GC support: Count the refs for the wiz_list memory.
 */

{
    wiz_list_t *w;

    for (w = all_wiz; w; w = w->next) {
        count_ref_from_string(w->name);
        count_ref_in_vector(&w->extra, 1);
        if(w->file_name)
            count_ref_from_string(w->file_name);
        if (w->error_message)
            count_ref_from_string(w->error_message);
        note_malloced_block_ref((char *)w);
    }
    count_ref_in_vector(&default_wizlist_entry.extra, 1);
} /* count_ref_from_wiz_list() */

#endif /* GC_SUPPORT */

/*-------------------------------------------------------------------------*/
#ifdef DEBUG

void
count_extra_ref_from_wiz_list (void)

/* DEBUG support: Count the extra refs for the wiz_list memory.
 */

{
    wiz_list_t *w;

    for (w = all_wiz; w; w = w->next) {
        count_extra_ref_in_vector(&w->extra, 1);
    }
    count_extra_ref_in_vector(&default_wizlist_entry.extra, 1);
} /* count_extra_ref_from_wiz_list() */

#endif

/***************************************************************************/

