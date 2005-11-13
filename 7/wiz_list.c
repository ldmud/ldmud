#include <stdio.h>
#include "config.h"
#include "lint.h"
#include "interpret.h"
#include "object.h"
#include "wiz_list.h"
#include "stralloc.h"

/*
 * Maintain the wizards high score list about most popular castle.
 */

extern char *string_copy PROT((char *));

struct wiz_list *all_wiz = 0;
static int number_of_wiz = 0;
int wiz_info_extra_size = -1;

/*
 * Find the data, if it exists.
 */
static struct wiz_list *find_wiz(name)
    char *name;
{
    struct wiz_list *wl;

    if ( !( name = findstring(name) ) )
	return 0;
    for (wl = all_wiz; wl; wl = wl->next)
        if (wl->name == name)
	    return wl;
    return 0;
}

/*
 * Check that a name exists. Add it, if it doesn't.
 */
struct wiz_list *add_name(str)
    char *str;
{
    struct wiz_list *wl;

    wl = find_wiz(str);
    if (wl)
        return wl;
    number_of_wiz++;
    wl = (struct wiz_list *)xalloc(sizeof (struct wiz_list));
    str = make_shared_string(str);
    wl->name = str;
    wl->length = strlen(str);
    wl->score = 0;
    wl->cost = 0;
    wl->heart_beats = 0;
    wl->next = all_wiz;
    wl->size_array = 0;
    wl->mapping_total = 0;
    wl->quota_allowance = 0;
    wl->quota_usage = 0;
    wl->file_name = 0;
    wl->error_message = 0;
    if (wiz_info_extra_size >= 0) {
	wl->extra.type  = T_POINTER;
	wl->extra.u.vec = allocate_array(wiz_info_extra_size);
    } else {
	extern struct svalue const0;

	wl->extra = const0;
    }
    wl->last_call_out = 0;
    all_wiz = wl;
    return wl;
}

#if 0
/*
 * Add score to an existing name.
 */
void add_score(name, score)
    char *name;
    int score;
{
    struct wiz_list *wl;

    wl = find_wiz(name);
    if (!wl)
        fatal("Add_score: could not find wizard %s\n", name);
    wl->score += score;
}
#endif

/*
 * This one is called at every complete walkaround of reset.
 */
void wiz_decay() {
    struct wiz_list *wl;
    static int next_time;
    extern int current_time;

    /* Perform this once every hour. */
    if (next_time > current_time)
	return;
    next_time = current_time + 60 * 60;
    for (wl = all_wiz; wl; wl = wl->next) {
        wl->score = wl->score * 99 / 100;
	wl->cost = wl->cost * .9;  /* integer is prone to overflow */
	wl->heart_beats = wl->heart_beats * 9 / 10;
    }
}

/*
 * Load the wizlist file.
 */
void load_wiz_file()
{
    char buff[1000];		/* I hate not knowing how much I need. */
    FILE *f;

    f = fopen("WIZLIST", "r");
    if (f == NULL)
        return;
    while(fgets(buff, sizeof buff, f) != NULL) {
        char *p;
	int score;

	p = strchr(buff, ' ');
	if (p == 0) {
	    fprintf(stderr, "Bad WIZLIST file.\n");
	    break;
	}
	*p = '\0';
	p++;
	if (*p == '\0') {
	    fprintf(stderr, "Bad WIZLIST file.\n");
	    break;
	}
	score = atoi(p);
	if (score > 0) {
	    add_name(buff)->score += score;
	}
    }
    fclose(f);
}

struct svalue *f_wizlist_info(sp)
    struct svalue *sp;
{
    struct vector *all, *entry;
    struct svalue *wsvp, *svp;
    struct wiz_list *w;
    extern struct svalue const0;

    if (_privilege_violation("wizlist_info", &const0, sp) <= 0) {
	all = allocate_array(0);
    } else {
	all = allocate_array(number_of_wiz);
	wsvp = all->item;
	for (w = all_wiz; w; w = w->next) {
	    entry = allocate_array(7);
            wsvp->type = T_POINTER;
            wsvp->u.vec = entry;
            wsvp++;
            svp = entry->item;
	    svp->type          = T_STRING;
	    svp->x.string_type = STRING_SHARED;
	    svp->u.string      = w->name;
	    increment_string_ref(w->name);
	    svp++;
	    svp->u.number      = w->score;
	    svp++;
	    svp->u.number      = w->cost;
	    svp++;
	    svp->u.number      = w->heart_beats;
	    svp++;
	    /* reserved for call_out */
	    svp++;
	    svp->u.number      = w->size_array;
	    svp++;
	    if (w->extra.type == T_POINTER) {
		struct vector *v = w->extra.u.vec;
		svp->type  = T_POINTER;
		svp->u.vec = slice_array(v, 0, VEC_SIZE(v) - 1);
	    } else
		assign_svalue_no_free(svp, &w->extra);
	} /* end for */
    } /* end if */
    sp++;
    sp->type = T_POINTER;
    sp->u.vec = all;
    return sp;
}

struct wiz_list default_wizlist_entry = {
0, /* name */
0, /* length */
0, /* next */
0, /* score */
0, /* cost */
0, /* heart_beats */
0, /* size_array */
0, /* mapping_total */
{ T_NUMBER }, /* extra */
0, /* quota_allowance */
0, /* quota_usage */
0, /* error file_name */
0, /* error_message */
0, /* error linue_number */
0, /* last_call_out */
0, /* int call_out_cost */
0, /* quota_state */
};

#ifdef MALLOC_smalloc
void clear_ref_from_wiz_list()
{
    struct wiz_list *w;

    for (w = all_wiz; w; w = w->next) {
	clear_ref_in_vector(&w->extra, 1);
    }
    clear_ref_in_vector(&default_wizlist_entry.extra, 1);
}

void count_ref_from_wiz_list()
{
    struct wiz_list *w;

    for (w = all_wiz; w; w = w->next) {
	count_ref_from_string(w->name);
	count_ref_in_vector(&w->extra, 1);
	if(w->file_name)
	    note_malloced_block_ref(w->file_name);
	if (w->error_message)
	    note_malloced_block_ref(w->error_message);
	note_malloced_block_ref((char *)w);
    }
    count_ref_in_vector(&default_wizlist_entry.extra, 1);
}
#endif /* MALLOC_SMALLOC */

#ifdef DEBUG
void count_extra_ref_from_wiz_list()
{
    struct wiz_list *w;

    for (w = all_wiz; w; w = w->next) {
	count_extra_ref_in_vector(&w->extra, 1);
    }
    count_extra_ref_in_vector(&default_wizlist_entry.extra, 1);
}
#endif

struct svalue *f_set_extra_wizinfo(sp)
    struct svalue *sp;
{
    struct wiz_list *user;
    short type;

    if ((type = sp[-1].type) == T_OBJECT) {
	user = sp[-1].u.ob->user;
    } else if (type != T_STRING || !(user = find_wiz(sp[-1].u.string)))
    {
	if (type == T_NUMBER && sp[-1].u.number == 0)
	    user = 0;
	else
	    bad_xefun_arg(1, sp);
    }
    if (_privilege_violation("set_extra_wizinfo", sp-1, sp) <= 0) {
	free_svalue(sp);
    } else {
	transfer_svalue(user ? &user->extra : &default_wizlist_entry.extra, sp);
    }
    free_svalue(sp-1);
    return sp - 2;
}

struct svalue *f_get_extra_wizinfo(sp)
    struct svalue *sp;
{
    struct wiz_list *user;
    short type;

    if ((type = sp->type) == T_OBJECT) {
	user = sp->u.ob->user;
    } else if (type != T_STRING || !(user = find_wiz(sp->u.string))) {
	if (type == T_NUMBER && sp->u.number == 0)
	    user = 0;
	else
	    bad_xefun_arg(1, sp);
    }
    if (_privilege_violation("get_extra_wizinfo", sp, sp) <= 0)
	bad_xefun_arg(1, sp);
    assign_svalue(sp, user ? &user->extra : &default_wizlist_entry.extra);
    return sp;
}

void remove_wiz_list() {
    struct wiz_list *wl, *w;

    for (w = all_wiz; w; w = wl) {
	free_string(w->name);
	wl = w->next;
	xfree((char *)w);
    }
}

void save_error(msg, file, line)
    char *msg;
    char *file;
    int line;
{
    struct wiz_list *wl;
    char name[100];
    char *copy, *p;
    int len;

    p = get_wiz_name(file);
    if(!p)
	return;
    strcpy(name, p);
    wl = add_name(name);
    if (wl->file_name)
	xfree(wl->file_name);
    len = strlen(file);
    copy = xalloc(len + 4); /* May add .c plus the null byte, and / */
    *copy = '/';
    strcpy(copy+1, file);
    /*
     * If it is a cloned object, we have to find out what the file
     * name is, and add '.c'.
     */
    if ( (p = strrchr(copy, '#')) ||
        ((p = copy+len), *p++ != 'c') || p[-2] != '.' ) {
	p[0] = '.';
	p[1] = 'c';
	p[2] = '\0';
    }
    wl->file_name = copy;
    if (wl->error_message)
	xfree(wl->error_message);
    wl->error_message = string_copy(msg);
    wl->line_number = line;
}

struct svalue *f_get_error_file(sp)
    struct svalue *sp;
{
    char *name;
    int forget;
    struct wiz_list *wl;
    struct vector *vec;
    struct svalue *v;

    if (sp[-1].type != T_STRING)
	bad_xefun_arg(1, sp);
    if (sp->type != T_NUMBER)
	bad_xefun_arg(2, sp);
    name = sp[-1].u.string;
    forget = sp->u.number;
    wl = find_wiz(name);
    sp--;
    free_string_svalue(sp);
    /*
     * The error_message is used as a flag if there has been any error.
     */
    if (!wl || !wl->error_message) {
	sp->type = T_NUMBER;
	sp->u.number = 0;
	return sp;
    }
    vec = allocate_array(4);
    v = vec->item;
    v[0].type = T_STRING;
    v[0].x.string_type = STRING_MALLOC;
    v[0].u.string = string_copy(wl->file_name);
    v[1].u.number = wl->line_number & ~0x40000000;
    v[2].type = T_STRING;
    v[2].x.string_type = STRING_MALLOC;
    v[2].u.string = string_copy(wl->error_message);
    v[3].u.number = (wl->line_number & 0x40000000) != 0;
    if (forget)
	wl->line_number |= 0x40000000;
    sp->type = T_POINTER;
    sp->u.vec = vec;
    return sp;
}

/*
 * Argument is a file name, which we want to get the owner of.
 * Ask the master.c object !
 */
char *get_wiz_name(file)
    char *file;
{
    struct svalue *ret;
    static char buff[50];

    push_volatile_string(file);
    ret = apply_master_ob(STR_GET_WNAME, 1);
    if (ret == 0 || ret->type != T_STRING)
	return 0;
    strncpy(buff, ret->u.string, sizeof buff - 1);
    return buff;
}
