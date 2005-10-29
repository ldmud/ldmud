#include <stdio.h>
#include <string.h>
#include "wiz_list.h"

/*
 * Maintain the wizards high score list about most popular castle.
 */

extern char *string_copy();

struct wiz_list *all_wiz;

void fatal(), add_message();

/*
 * Sort the wiz list in ascending order.
 */

static struct wiz_list *insert(w, wl)
    struct wiz_list *w, *wl;
{
    if (wl == 0) {
	w->next = 0;
	return w;
    }
    if (w->score > wl->score) {
	wl->next = insert(w, wl->next);
	return wl;
    }
    w->next = wl;
    return w;
}

static void rebuild_list() {
    struct wiz_list *wl, *w, *new_list = 0;

    for (w = all_wiz; w; w = wl) {
	wl = w->next;
	new_list = insert(w, new_list);
    }
    all_wiz = new_list;
}

/*
 * Find the data, if it exists.
 */
static struct wiz_list *find_wiz(name)
    char *name;
{
    int length;
    struct wiz_list *wl;

    length = strlen(name);
    for (wl = all_wiz; wl; wl = wl->next)
        if (wl->length == length && strcmp(wl->name, name) == 0)
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
    wl = (struct wiz_list *)xalloc(sizeof (struct wiz_list));
    str = string_copy(str);
    wl->name = str;
    wl->length = strlen(str);
    wl->score = 0;
    wl->cost = 0;
    wl->heart_beats = 0;
    wl->total_worth = 0;
    wl->next = all_wiz;
    all_wiz = wl;
    return wl;
}

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

/*
 * This one is called at every complete walkaround of reset.
 */
void wiz_decay() {
    struct wiz_list *wl;

    for (wl = all_wiz; wl; wl = wl->next) {
        wl->score = wl->score * 99 / 100;
	wl->total_worth = wl->total_worth * 99 / 100;
	wl->cost = wl->cost * 9 / 10;
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
    while(fgets(buff, sizeof buff, f)) {
        char *p;
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
	(void)add_name(buff);
	add_score(buff, atoi(p));
    }
    fclose(f);
}

/*
 * Save the wizlist file.
 */
void save_wiz_file()
{
    struct wiz_list *wl;
    FILE *f;

    f = fopen("WIZLIST", "w");
    if (f == NULL) {
        fprintf(stderr, "Could not open WIZLIST for write\n");
        return;
    }
    for (wl = all_wiz; wl; wl = wl->next)
        fprintf(f, "%s %d %d\n", wl->name, wl->score, wl->total_worth);
    fclose(f);
}

/* Print out the wizlist file */

void wizlist() {
    struct wiz_list *wl;
    int total = 0, num = 0;

    rebuild_list();
    for (wl = all_wiz; wl; wl = wl->next) {
        total += wl->score;
	num++;
    }
    add_message("\nWizard top score list\n\n");
    for (wl = all_wiz; wl; wl = wl->next) {
	if(wl->score)
	    add_message("%-15s %5d %2d%% (%d)\t[%3dk,%5d] %d\n", wl->name,
			wl->score, wl->score * 100 / total, num,
			wl->cost / 1000,
			wl->heart_beats, wl->total_worth);
	num--;
    }
    add_message("\nTotal         %7d\n\n", total);
}
