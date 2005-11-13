#include <sys/types.h>
#include <sys/time.h>
#include "lint.h"
#include "config.h"
#include "interpret.h"
#include "object.h"
#include "exec.h"
#include "sent.h"
#include "comm.h"
#include "smalloc.h"
#include "instrs.h"
#include "lang.h"
#include "wiz_list.h"
#include "rxcache.h"
#define NO_INCREMENT_STRING_REF
#include "stralloc.h"
extern void dprintf1 PROT((int, char *, p_int));

/* The referencing code for dynamic data should mirror the destructor code,
 * thus, memory leaks can show up as soon as the memory has allocated.
 */


#ifdef MAPPINGS
void walk_mapping PROT((
        struct mapping *,
        void (*)(struct svalue *, struct svalue *, char *),
        char *
));
extern mp_int num_dirty_mappings;
extern void compact_mappings PROT((mp_int));
#endif

extern void free_all_sent();
extern void remove_destructed_objects();

#if defined(MALLOC_smalloc)

#define	STRING_REFS(str)	(*(unsigned short *)((char *) (str)\
						   - sizeof(unsigned short)))
#define MARK_STRING_REF(str) ((void)(\
    STRING_REFS(str)++ || (\
	CHECK_REF( (str)-sizeof(short)-sizeof(char *) ) ||\
	    /* reached max ref count, which is given as 0... */ \
	    STRING_REFS(str)--\
    ))\
)

extern int d_flag;
int gcollect_outfd = 2;
#define gout gcollect_outfd

/* Time of last gc, used by the backend in times of memory shortage */
time_t time_last_gc = 0;

/* Are the ref counts unusable, i.e. is phase 2 or two 3 in progress ? */
int garbage_collection_in_progress = 0;

#ifdef SMALLOC_TRACE
extern void writex PROT((int, int));
extern void writed PROT((int, int));
extern int is_freed PROT((char *, p_uint));
#define WRITES(d, s) write((d), (s), strlen(s))
#define WRITE_SMALLOC_TRACE(p)	(WRITES(gout, ((char **)(p))[-3]), \
	WRITES(gout, " "), \
	((p_uint (*)PROT((int, int)))writed)(gout, ((p_uint *)(p))[-2]), \
	WRITES(gout, "\n") ),
#else
#define WRITE_SMALLOC_TRACE(p)
#endif

#define CLEAR_REF(p) ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] &= ~M_REF )

#define MARK_REF(p) ( ((p_uint *)(p))[-SMALLOC_OVERHEAD] |= M_REF )

#define TEST_REF(p) ( !( ((p_uint *)(p))[-SMALLOC_OVERHEAD] & M_REF ) )

#define CHECK_REF(p) ( TEST_REF(p) && ( MARK_REF(p),MY_TRUE ) )

extern long time_to_swap_variables;

#ifdef __GNUC__
/* typecast would strip NORETURN */
#define ifatal(s) (fatal(s),0)
#else
#define ifatal(s) (((p_uint (*)PROT((char *)))fatal)(s))
#endif
#define NOTE_REF(p) \
    ( \
	TEST_REF(p) ? \
	    MARK_REF(p) \
	: time_to_swap_variables + 1 == 0 && \
	  ( WRITE_SMALLOC_TRACE(p) \
	    ifatal("memory block referenced twice\n") ) \
    )

void clear_memory_reference(p)
    char *p;
{
    CLEAR_REF(p);
}

void remove_unreferenced_string(start, string)
    char *start, *string;
{
    if (TEST_REF(start)) {
	dprintf1(gout, "\
'%s' was left unreferenced in the shared string table, freeing now.\n",
	  (p_int)string
	);

	MARK_REF(start);
	STRING_REFS(string)++;
	free_string(string);
    }
}

struct object *gc_obj_list_destructed;
struct lambda *stale_lambda_closures, *stale_misc_closures;
extern struct mapping *stale_mappings;

void clear_inherit_ref(p)
    struct program *p;
{
    int i;

    for (i=0; i< p->num_inherited; i++) {
	/* Inherited programs are never swapped. Only programs with blueprints
	 * are swapped, and a blueprint and one inheritance makes two refs.
	 */
	struct program *p2;

	p2 = p->inherit[i].prog;
	if (p2->ref) {
	    p2->ref = 0;
	    clear_inherit_ref(p2);
	}
    }
}

void mark_program_ref(p)
    struct program *p;
{
    if (CHECK_REF(p)) {
        int i;

	unsigned char *program = p->program;
        uint32 *functions = p->functions;
	char **strings;
	struct variable *variable_names;

	if (p->ref++)
	    fatal("First reference to program, but ref count != 0\n");
	if (p->swap_num != -1 && p->line_numbers) {
	    NOTE_REF(p->line_numbers);
	}
        for (i = p->num_functions; --i >= 0; ) {
            if ( !(functions[i] & NAME_INHERITED) ) {
                char *name;

                memcpy(
                  (char *)&name,
                  program + (functions[i] & FUNSTART_MASK) - 1 - sizeof name,
                  sizeof name
                );
                MARK_STRING_REF(name);
            }
        }
	strings = p->strings;
	for (i = p->num_strings; --i >= 0; ) {
	    char *str = *strings++;
	    MARK_STRING_REF(str);
	}
	variable_names = p->variable_names;
	for (i = p->num_variables; --i >= 0; variable_names++)
	    MARK_STRING_REF(variable_names->name);
	for (i=0; i< p->num_inherited; i++) {
	    mark_program_ref(p->inherit[i].prog);
	}
	NOTE_REF(p->name);
    } else {
	if (!p->ref++)
	    fatal("Program block referenced as something else\n");
    }
}

static void note_sentence_ref(p)
struct sentence *p;
{
    do {
	if (p->function) MARK_STRING_REF(p->function);
	if (p->verb) MARK_STRING_REF(p->verb);
	NOTE_REF(p);
    } while (p = p->next);
}

void clear_map_ref_filter PROT((struct svalue *, struct svalue *, char *));

void reference_destructed_object(ob)
    struct object *ob;
{
    if (TEST_REF(ob)) {
	if (ob->ref)
	    fatal("First reference to destructed object, but ref count != 0\n");
	/* Destructed objects are not swapped */
	ob->next_all = gc_obj_list_destructed;
	gc_obj_list_destructed = ob;
	MARK_REF(ob);
	mark_program_ref(ob->prog);
	NOTE_REF(ob->name);
	ob->ref++;
    } else {
	if (!ob->ref) {
	    WRITE_SMALLOC_TRACE(ob)
	    ifatal("Destructed object referenced as something else\n");
	}
    }
}

void note_malloced_block_ref(p)
    char *p;
{
    NOTE_REF(p);
}

static void count_ref_in_closure(csvp)
    struct svalue *csvp;
{
    extern struct object *master_ob;

    struct lambda *l = csvp->u.lambda;
    ph_int type = csvp->x.closure_type;

    if (!l->ref) {
	/* This closure was bound to a destructed object, and has been
	 * encountered before.
	 */
	l->ref--; /* Undo ref increment that was done by the caller */
	if (type == CLOSURE_BOUND_LAMBDA) {
	    csvp->x.closure_type = CLOSURE_UNBOUND_LAMBDA;
	    (csvp->u.lambda = l->function.lambda)->ref++;
	} else {
	    csvp->x.closure_type = F_UNDEF-F_OFFSET+CLOSURE_EFUN;
	    csvp->u.ob = master_ob;
	    master_ob->ref++;
	}
	return;
    }
    if (type != CLOSURE_UNBOUND_LAMBDA) {
	struct object *ob;

	ob = l->ob;
	if (ob->flags & O_DESTRUCTED ||
	    ( type == CLOSURE_ALIEN_LFUN &&
	      l->function.alien.ob->flags & O_DESTRUCTED) )
	{
	    l->ref = -1;
	    if (type == CLOSURE_LAMBDA) {
		l->ob = (struct object *)stale_lambda_closures;
		stale_lambda_closures = l;
	    } else {
		l->ob = (struct object *)stale_misc_closures;
		stale_misc_closures = l;
		if (type == CLOSURE_ALIEN_LFUN) {
		    if (l->function.alien.ob->flags & O_DESTRUCTED)
			reference_destructed_object(l->function.alien.ob);
		}
	    }
	    if (ob->flags & O_DESTRUCTED)
		reference_destructed_object(ob);
	    if (type == CLOSURE_BOUND_LAMBDA) {
		csvp->x.closure_type = CLOSURE_UNBOUND_LAMBDA;
		csvp->u.lambda = l->function.lambda;
	    } else {
		csvp->x.closure_type = F_UNDEF-F_OFFSET+CLOSURE_EFUN;
		csvp->u.ob = master_ob;
		master_ob->ref++;
	    }
	} else {
	    ob->ref++;
	    if (type == CLOSURE_ALIEN_LFUN)
		l->function.alien.ob->ref++;
	}
    }
    if (CLOSURE_HAS_CODE(type)) {
	mp_int num_values;
	struct svalue *svp;

	svp = (struct svalue *)l;
	if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
	    num_values = svp[-0xff].u.number;
	svp -= num_values;
	NOTE_REF(svp);
	count_ref_in_vector(svp, num_values);
    } else {
	NOTE_REF(l);
	if (type == CLOSURE_BOUND_LAMBDA)
	{
	    struct lambda *l2 = l->function.lambda;

	    if (!l2->ref++) {
		struct svalue sv;

		sv.type = T_CLOSURE;
		sv.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
		sv.u.lambda = l2;
		count_ref_in_closure(&sv);
	    }
	}
    }
}

static void clear_ref_in_closure(l, type)
    struct lambda *l;
    ph_int type;
{
    if (CLOSURE_HAS_CODE(type)) {
	mp_int num_values;
	struct svalue *svp;

	svp = (struct svalue *)l;
	if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
	    num_values = svp[-0xff].u.number;
	svp -= num_values;
	clear_ref_in_vector(svp, num_values);
    } else if (type == CLOSURE_BOUND_LAMBDA) {
	struct lambda *l2 = l->function.lambda;

	if (l2->ref) {
	    l2->ref = 0;
	    clear_ref_in_closure(l2, CLOSURE_UNBOUND_LAMBDA);
	}
    }
    if (type != CLOSURE_UNBOUND_LAMBDA && l->ob->flags & O_DESTRUCTED &&
	l->ob->ref /* block against bad efficency due to multiple refs */ )
    {
	l->ob->ref = 0;
	l->ob->prog->ref = 0;
	clear_inherit_ref(l->ob->prog);
    }
    if (type == CLOSURE_ALIEN_LFUN &&
	l->function.alien.ob->flags & O_DESTRUCTED &&
	l->function.alien.ob->ref)
    {
	l->function.alien.ob->ref = 0;
	l->function.alien.ob->prog->ref = 0;
	clear_inherit_ref(l->function.alien.ob->prog);
    }
}

void count_ref_from_string(p)
   char *p;
{
   MARK_STRING_REF(p);
}

void count_ref_in_vector(svp, num)
    struct svalue *svp;
    int num;
{
    struct svalue *p;

    for (p = svp; p < svp+num; p++) {
	switch(p->type) {
	  case T_OBJECT:
	  {
	    struct object *ob;

	    ob = p->u.ob;
	    if (ob->flags & O_DESTRUCTED) {
		p->type = T_NUMBER;
		p->u.number = 0;
		reference_destructed_object(ob);
	    } else {
		ob->ref++;
	    }
	    continue;
	  }
	  case T_POINTER:
	  case T_QUOTED_ARRAY:
	    /* Don't use CHECK_REF on the null vector */
	    if (p->u.vec != &null_vector && CHECK_REF(p->u.vec)) {
		count_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
	    }
	    p->u.vec->ref++;
	    continue;
#ifdef MAPPINGS
	  case T_MAPPING:
	    if (CHECK_REF(p->u.map)) {
		extern void count_ref_in_mapping PROT((struct mapping *));

		struct mapping *m;
		struct condensed_mapping *cm;
		int num_values;

		m = p->u.map;
		cm = m->condensed;
		num_values = m->num_values;
		NOTE_REF((char *)CM_MISC(cm) - cm->misc_size *(num_values + 1));
		/* hash mappings have been eleminated at the start */
		count_ref_in_mapping(m);
	    }
	    p->u.map->ref++;
	    continue;
#endif /* MAPPINGS */
	  case T_STRING:
	    switch(p->x.string_type) {
	      case STRING_MALLOC:
		NOTE_REF(p->u.string);
		break;
	      case STRING_SHARED:
		MARK_STRING_REF(p->u.string);
		break;
	    }
	    continue;
	  case T_CLOSURE:
	    if (CLOSURE_MALLOCED(p->x.closure_type)) {
		if (p->u.lambda->ref++ <= 0) {
		    count_ref_in_closure(p);
		}
	    } else {
		extern struct object *master_ob;

		struct object *ob;

		ob = p->u.ob;
		if (ob->flags & O_DESTRUCTED) {
		    p->x.closure_type = F_UNDEF-F_OFFSET+CLOSURE_EFUN;
		    p->u.ob = master_ob;
		    master_ob->ref++;
		    reference_destructed_object(ob);
		} else {
		    ob->ref++;
		}
	    }
	    continue;
	  case T_SYMBOL:
	    MARK_STRING_REF(p->u.string);
	    continue;
	}
    }
}

void clear_map_ref_filter(key, data, extra)
    struct svalue *key, *data;
    char *extra;
{
    clear_ref_in_vector(key, 1);
    clear_ref_in_vector(data, (p_int)extra);
}

/*
 * Clear the ref count for vectors
 */
void clear_ref_in_vector(svp, num)
    struct svalue *svp;
    int num;
{
    struct svalue *p;

    for (p = svp; p < svp+num; p++) {
	switch(p->type) {
	  case T_OBJECT:
	    /* this might be a destructed object, which has it's ref not
	     * cleared by the obj_list because it is no longer a member
	     * Alas, swapped objects must not have prog->ref cleared.
	     */
	    if (p->u.ob->flags & O_DESTRUCTED && p->u.ob->ref) {
		p->u.ob->ref = 0;
		p->u.ob->prog->ref = 0;
		clear_inherit_ref(p->u.ob->prog);
	    }
	    continue;
	  case T_POINTER:
	  case T_QUOTED_ARRAY:
	    if (!p->u.vec->ref) continue;
	    p->u.vec->ref = 0;
	    clear_ref_in_vector(&p->u.vec->item[0], VEC_SIZE(p->u.vec));
	    continue;
#ifdef MAPPINGS
	  case T_MAPPING:
	    if (p->u.map->ref) {
		struct mapping *m;
		p_int num_values;

		m = p->u.map;
		m->ref = 0;
		num_values = m->num_values;
		walk_mapping(m, clear_map_ref_filter, (char *)num_values );
	    }
	    continue;
#endif /* MAPPINGS */
	  case T_CLOSURE:
	    if (CLOSURE_MALLOCED(p->x.closure_type)) {
		struct lambda *l;

		l = p->u.lambda;
		if (l->ref) {
		    l->ref = 0;
		    clear_ref_in_closure(l, p->x.closure_type);
		}
	    } else if (p->u.ob->flags & O_DESTRUCTED && p->u.ob->ref) {
		p->u.ob->ref = 0;
		p->u.ob->prog->ref = 0;
		clear_inherit_ref(p->u.ob->prog);
	    }
	    continue;
	}
    }
}

/*
 * Loop through every object and variable in the game and check
 * all reference counts. This will surely take some time, and should
 * only be used for debugging.
 */
void garbage_collection()
{
    extern struct object *master_ob;
    extern void clear_M_REF_flags();
    extern void clear_shared_string_refs();
    extern void walk_shared_strings PROT(( void (*)(char *, char *) ));
    extern void reallocate_reserved_areas();

    extern struct interactive *all_players[MAX_PLAYERS];
    extern char *last_insert_alist_shared_string;
    extern int malloc_privilege;
    extern int32 initial_eval_cost, eval_cost, assigned_eval_cost;
    extern int out_of_memory;
    extern char *reserved_user_area, *reserved_master_area,
	        *reserved_system_area;
    extern char *mud_lib;

    struct object *ob, *next_ob;
    struct lambda *l, *next_l;
    int i;

    /*
     * Pass 0: dispose of some unnecessary stuff.
     */
    malloc_privilege = MALLOC_MASTER;
    CLEAR_EVAL_COST;
    out_of_memory = 0;
    assert_master_ob_loaded();
    malloc_privilege = MALLOC_SYSTEM;
    remove_destructed_objects();
    free_all_sent();
    compact_mappings(num_dirty_mappings);
    free_interpreter_temporaries();
    remove_stale_call_outs();
    free_defines();
    free_all_local_names();
    remove_unknown_identifier();
    free_notifys();

    /*
     * Pass 1: clear the M_REF flag in all malloced blocks.
     */
    clear_M_REF_flags();

    /*
     * Pass 2: clear the ref counts.
     */
    garbage_collection_in_progress = 2;
    if (d_flag > 3) {
        debug_message("start of garbage_collection\n");
    }
    /*
     * List of all objects.
     */
    for (ob=obj_list; ob; ob = ob->next_all) {
        int was_swapped;

	if (d_flag > 4) {
	    debug_message("clearing refs for object '%s'\n",ob->name);
	}
	was_swapped = 0;
	if (ob->flags & O_SWAPPED &&
	    (was_swapped = load_ob_from_swap(ob)) & 1)
	{
	    /* don't clear the program ref count. It is 1 */
	} else {
	    /* Take special care of inherited programs, the associated
	     * objects might me destructed.
	     */
	    ob->prog->ref = 0;
	}
	clear_inherit_ref(ob->prog);
	ob->ref = 0;
	clear_ref_in_vector(ob->variables, ob->prog->num_variables);
	if (ob->flags & O_SHADOW) {
	    struct ed_buffer *buf;

	    if (buf = O_GET_SHADOW(ob)->ed_buffer) {
		clear_ed_buffer_refs(buf);
	    } /* end of ed-buffer processing */
	}
	if (was_swapped) swap(ob, was_swapped);
    }
    if (d_flag > 3) {
        debug_message("ref counts referenced by obj_list cleared\n");
    }

    /* The interactives */
    for(i = 0 ; i < MAX_PLAYERS; i++) {
	struct input_to * it;

	if (all_players[i] == 0)
	    continue;
	if (it = all_players[i]->input_to) {
	    clear_ref_in_vector(it->arg, it->num_arg);
	    if (it->ob->flags & O_DESTRUCTED && it->ob->ref) {
		it->ob->ref = 0;
		it->ob->prog->ref = 0;
		clear_inherit_ref(it->ob->prog);
	    }
	}
	clear_ref_in_vector(&all_players[i]->default_err_message, 1);
	clear_ref_in_vector(&all_players[i]->prompt, 1);
	if (ob = all_players[i]->snoop_by) {
	    if (!O_GET_INTERACTIVE(ob) ||
		O_GET_INTERACTIVE(ob)->sent.type != SENT_INTERACTIVE)
	    {
		/* snooping monster */
		if (ob->flags & O_DESTRUCTED && ob->ref) {
		    ob->ref = 0;
		    ob->prog->ref = 0;
		    clear_inherit_ref(ob->prog);
	        }
	    }
	} /* end of snoop-processing */
	if (ob = all_players[i]->modify_command) {
	    if (ob->flags & O_DESTRUCTED && ob->ref) {
		ob->ref = 0;
		ob->prog->ref = 0;
		clear_inherit_ref(ob->prog);
	    }
	}
    }
    for (i = NUM_CLOSURE_HOOKS; --i >= 0; ) {
	if (closure_hook[i].type == T_CLOSURE &&
	    closure_hook[i].x.closure_type == CLOSURE_LAMBDA)
	{
	    closure_hook[i].x.closure_type = CLOSURE_UNBOUND_LAMBDA;
	}
    }
    clear_ref_in_vector(closure_hook, NUM_CLOSURE_HOOKS);

    /*
     * The shared string table.
     */   
    clear_shared_string_refs();

    /* The wizlist */
    clear_ref_from_wiz_list();

    /* call_outs */
    clear_ref_from_call_outs();

    clear_simul_efun_refs();
    clear_interpreter_refs();
    clear_comm_refs();
    clear_rxcache_refs();

    null_vector.ref = 0;

    /*
     * Pass 3: Compute the ref counts, and set M_REF where appropriate.
     */
    garbage_collection_in_progress = 3;

    gc_obj_list_destructed = 0;
    stale_lambda_closures = 0;
    stale_misc_closures = 0;
    stale_mappings = 0;

    /*
     * List of all objects.
     */
    for (ob=obj_list; ob; ob = ob->next_all) {
        int was_swapped;

	was_swapped = 0;
	if (ob->flags & O_SWAPPED) {
	    was_swapped = load_ob_from_swap(ob);
	    if (was_swapped & 1) {
		CLEAR_REF(ob->prog);
		ob->prog->ref = 0;
	    }
	}
	ob->ref++;
	NOTE_REF(ob);
	if (ob->prog->num_variables)
	    NOTE_REF(ob->variables);
	mark_program_ref(ob->prog);
	count_ref_in_vector(ob->variables, ob->prog->num_variables);
	if (ob->sent) {
	    struct sentence *sent;
	    struct ed_buffer *buf;

	    sent = ob->sent;
	    if (ob->flags & O_SHADOW) {
		NOTE_REF(sent);
		if (buf = ((struct shadow_sentence *)sent)->ed_buffer) {
		    NOTE_REF(buf);
		    count_ed_buffer_refs(buf);
		} /* end of ed-buffer processing */
		sent = sent->next;
	    }
	    if (sent) note_sentence_ref(sent);
	}
	NOTE_REF(ob->name);
	if (was_swapped) {
	    swap(ob, was_swapped);
	}
    }
    if (d_flag > 3) {
        debug_message("obj_list evaluated\n");
    }

    /*
     * The interactives.
     */   
    for(i = 0 ; i < MAX_PLAYERS; i++) {
	struct input_to * it;

	if (all_players[i] == 0)
	    continue;
	NOTE_REF(all_players[i]);
	/* There are no destructed interactives */
	all_players[i]->ob->ref++;
	if (ob = all_players[i]->snoop_by) {
	    if (!O_GET_INTERACTIVE(ob) ||
		O_GET_INTERACTIVE(ob)->sent.type != SENT_INTERACTIVE)
	    {
		/* snooping monster */
		if (ob->flags & O_DESTRUCTED) {
		    all_players[i]->snoop_by = 0;
		    reference_destructed_object(ob);
		} else {
		    ob->ref++;
	        }
	    }
	} /* end of snoop-processing */
	if (it = all_players[i]->input_to) {
	    /* To avoid calling too high-level functions, we want the
	     * struct input_to not to be freed by now.
	     * Thus, we reference the object even if it is destructed.
	     */
	    NOTE_REF(it);
	    ob = it->ob;
	    if (!ob->ref) {
		/* destructed */
		NOTE_REF(ob);
		mark_program_ref(ob->prog);
		NOTE_REF(ob->name);
	    }
	    ob->ref++;
	    MARK_STRING_REF(it->function);
	    count_ref_in_vector(it->arg, it->num_arg);
	} /* end of input_to processing */
	if (ob = all_players[i]->modify_command) {
	    if (ob->flags & O_DESTRUCTED) {
		all_players[i]->modify_command = 0;
		reference_destructed_object(ob);
	    } else {
		ob->ref++;
	    }
	}
	count_ref_in_vector(&all_players[i]->default_err_message, 1);
	count_ref_in_vector(&all_players[i]->prompt, 1);
        if (all_players[i]->trace_prefix) {
            count_ref_from_string(all_players[i]->trace_prefix);
        }
    }

    /* The wizlist */
    count_ref_from_wiz_list();

    /* call_outs */
    count_ref_from_call_outs();

    if (master_ob) master_ob->ref++;
    else fatal("No master object\n");
    if (last_insert_alist_shared_string) {
	MARK_STRING_REF(last_insert_alist_shared_string);
    }

    count_lex_refs();
    count_compiler_refs();

    count_simul_efun_refs();
    note_shared_string_table_ref();
    note_otable_ref();
    count_comm_refs();
    count_interpreter_refs();
    count_heart_beat_refs();
#ifdef RXCACHE_TABLE
    count_rxcache_refs();
#endif
    if (reserved_user_area)
	NOTE_REF(reserved_user_area);
    if (reserved_master_area)
	NOTE_REF(reserved_master_area);
    if (reserved_system_area)
	NOTE_REF(reserved_system_area);
    NOTE_REF(mud_lib);
    null_vector.ref++;

    count_ref_in_vector(closure_hook, NUM_CLOSURE_HOOKS);
    for (i = NUM_CLOSURE_HOOKS; --i >= 0; ) {
	if (closure_hook[i].type == T_CLOSURE &&
	    closure_hook[i].x.closure_type == CLOSURE_UNBOUND_LAMBDA)
	{
	    closure_hook[i].x.closure_type = CLOSURE_LAMBDA;
	}
    }

    garbage_collection_in_progress = 0;
    /*
     * Pass 4: remove stralloced strings with M_REF cleared.
     */
    walk_shared_strings(remove_unreferenced_string);

    /*
     * Pass 5: Release all destructed objects.
     */
    /* It is vital that all information freed here is already known
     * as referenced, so we won't free it a second time in pass 6.
     */
    for (ob = gc_obj_list_destructed; ob; ob = next_ob) {
	next_ob = ob->next_all;
	free_object(ob, "garbage collection");
    }
    for (l = stale_lambda_closures; l; ) {
	struct svalue sv;

	next_l = (struct lambda *)l->ob;
	l->ref = 1;
	sv.type = T_CLOSURE;
	sv.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
	sv.u.lambda = l;
	l = (struct lambda *)l->ob;
	free_closure(&sv);
    }
    for (l = stale_misc_closures; l; l = next_l) {
	next_l = (struct lambda *)l->ob;
	xfree((char *)l);
    }
    clean_stale_mappings();
    /*
     * Pass 6: Release all unused memory.
     */
    free_unreferenced_memory();
    reallocate_reserved_areas();
    if (!reserved_user_area) {
	void remove_uids PROT((int));

	struct svalue *res = 0;
	if (reserved_system_area) {
	    CLEAR_EVAL_COST;
	    malloc_privilege = MALLOC_MASTER;
	    res = apply_master_ob(STR_QUOTA_DEMON, 0);
	}
	remove_uids(res && (res->type != T_NUMBER || res->u.number) );
    }
    reallocate_reserved_areas();
    time_last_gc = time(NULL);
}

void remove_uids(smart)
    int smart;
{
}

#else /* not MALLOC_smalloc :-( */

void garbage_collection()
{
    assert_master_ob_loaded();
    remove_destructed_objects();
    free_all_sent();
    compact_mappings(num_dirty_mappings);
    free_interpreter_temporaries();
    remove_stale_call_outs();
    free_defines();
    free_all_local_names();
    remove_unknown_identifier();
    reallocate_reserved_areas();
}
#endif /* MALLOC_smalloc */

static void clear_program_id(p)
    struct program *p;
{
    int i;

    if (!p->id_number)
	return;
    p->id_number = 0;
    for (i=0; i< p->num_inherited; i++) {
	clear_program_id(p->inherit[i].prog);
    }
}

static void renumber_program(p)
    struct program *p;
{
    extern int32 current_id_number;

    int i;

    if (p->id_number)
	return;
    p->id_number = ++current_id_number;
    for (i=0; i< p->num_inherited; i++) {
	renumber_program(p->inherit[i].prog);
    }
}

int32 renumber_programs() {
    extern int32 current_id_number;
    extern void invalidate_apply_low_cache();

    struct object *ob;

    current_id_number = 0;
    for (ob = obj_list; ob; ob = ob->next_all) {
	if (ob->flags & O_DESTRUCTED)
	    continue;
	if ( !O_PROG_SWAPPED(ob) )
	    clear_program_id(ob->prog);
    }
    for (ob = obj_list; ob; ob = ob->next_all) {
	if (ob->flags & O_DESTRUCTED)
	    continue;
	if ( !O_PROG_SWAPPED(ob) )
	    renumber_program(ob->prog);
    }
    invalidate_apply_low_cache();
    return ++current_id_number;
}

#if defined(SMALLOC_TRACE) && defined(MALLOC_smalloc)
static void show_string(d, block, depth)
    int d;
    char *block;
    int depth;
{
    mp_int len;

    WRITES(d, "\"");
    if ((len = strlen(block)) < 70) {
	write(d, block, len);
	WRITES(d, "\"");
    } else {
	write(d, block, 50);
	WRITES(d, "\" (truncated, length ");writed(d, len);WRITES(d, ")");
    }
}

static void show_added_string(d, block, depth)
    int d;
    char *block;
    int depth;
{
    WRITES(d, "Added string: ");
    show_string(d, block, 0);
    WRITES(d, "\n");
}

static void show_object(d, block, depth)
    int d;
    char *block;
    int depth;
{
    struct object *ob;

    ob = (struct object *)block;
    if (depth) {
	struct object *o;

	for (o = obj_list; o && o != ob; o = o->next_all);
	if (!o || o->flags & O_DESTRUCTED) {
	    WRITES(d, "Destructed object in block 0x");
	    writex(d, (unsigned)((unsigned *)block - SMALLOC_OVERHEAD));
	    WRITES(d, "\n");
	    return;
	}
    }
    WRITES(d, "Object: ");
    show_string(d, ob->name, 0);
    WRITES(d, ", uid: ");
    show_string(d, ob->user->name ? ob->user->name : "0", 0);
    WRITES(d, "\n");
}

static void show_array(d, block, depth)
    int d;
    char *block;
    int depth;
{
    struct vector *a;
    mp_int i, j;
    struct svalue *svp;
    struct wiz_list *user;
    mp_int a_size;

    a = (struct vector *)block;
    a_size = VEC_SIZE(a);
    if (depth && a != &null_vector) {
	extern struct wiz_list *all_wiz;

	int freed;
	struct wiz_list *wl;

	freed = is_freed(block, sizeof(struct vector) );
	if (!freed) {
	    user = a->user;
	    wl = all_wiz;
	    if (user)
		for ( ; wl && wl != user; wl = wl->next);
	}
	if (freed || !wl || a_size <= 0 || a_size > MAX_ARRAY_SIZE ||
	    malloced_size((char *)a) - SMALLOC_OVERHEAD << 2 !=
	      sizeof(struct vector) + sizeof(struct svalue) * (a_size - 1) )
	{
	    WRITES(d, "Array in freed block 0x");
	    writex(d, (unsigned)((unsigned *)block - SMALLOC_OVERHEAD));
	    WRITES(d, "\n");
	    return;
	}
    } else {
	user = a->user;
    }
    WRITES(d, "Array size ");writed(d, a_size);
    WRITES(d, ", uid: ");show_string(d, user ? user->name : "0", 0);
    WRITES(d, "\n");
    if (depth > 2)
	return;
    i = 32 >> depth;
    if (i > a_size)
	i = a_size;
    for (svp = a->item; --i >= 0; svp++) {
	for (j = depth + 1; --j >= 0;) WRITES(d, " ");
	switch(svp->type) {
	  case T_POINTER:
	    show_array(d, (char *)svp->u.vec, depth+1);
	    break;
	  case T_NUMBER:
	    writed(d, svp->u.number);
	    WRITES(d, "\n");
	    break;
	  case T_STRING:
	    if (svp->x.string_type == STRING_MALLOC &&
		  is_freed(svp->u.string, 1) )
	    {
		WRITES(d, "Malloced string in freed block 0x");
		writex(d, (unsigned)((unsigned *)block - SMALLOC_OVERHEAD));
		WRITES(d, "\n");
		break;
	    }
	    if (svp->x.string_type == STRING_SHARED &&
		is_freed(SHSTR_BLOCK(svp->u.string),
		         sizeof(char *) + sizeof(short) + 1) )
	    {

		WRITES(d, "Shared string in freed block 0x");
		writex(d, (unsigned)(
		  (unsigned *)(block-sizeof(char *)-sizeof(short))
		  - SMALLOC_OVERHEAD
		));
		WRITES(d, "\n");
		break;
	    }
	    WRITES(d, "String: ");
	    show_string(d, svp->u.string, 0);
	    WRITES(d, "\n");
	    break;
	  case T_OBJECT:
	    show_object(d, (char *)svp->u.ob, 1);
	    break;
	  default:
	    WRITES(d, "Svalue type ");writed(d, svp->type);WRITES(d, "\n");
	    break;
	}
    }
}

void setup_print_block_dispatcher()
/* This is here because I like to avoid smalloc calling closures.
 * gcollect.c is already notorious for including almost every header file.
 */
{
    extern struct object *master_ob;
    extern struct svalue *inter_sp;

    extern void store_print_block_dispatch_info
				PROT((char *, void (*)(int, char *, int)));

    struct svalue tmp_closure;
    struct vector *a, *b;

    assert_master_ob_loaded();
    tmp_closure.type = T_CLOSURE;
    tmp_closure.x.closure_type = CLOSURE_EFUN + F_ADD - F_OFFSET;
    tmp_closure.u.ob = master_ob;
    push_constant_string("");
    push_constant_string("");
    call_lambda(&tmp_closure, 2);
    store_print_block_dispatch_info(inter_sp->u.string, show_added_string);
    free_svalue(inter_sp--);
    a = allocate_array(1);
    store_print_block_dispatch_info((char *)a, show_array);
    b = slice_array(a, 0, 0);
    store_print_block_dispatch_info((char *)b, show_array);
    free_vector(a);
    free_vector(b);
    store_print_block_dispatch_info((char *)master_ob, show_object);
}
#else
void setup_print_block_dispatcher() {}
#endif /* SMALLOC_TRACE */
