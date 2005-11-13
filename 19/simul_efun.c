#include "driver.h"

#include "my-alloca.h"
#include <stdio.h>

#include "simul_efun.h"

#include "array.h"
#include "exec.h"
#include "gcollect.h"
#include "interpret.h"
#include "instrs.h"
#include "lex.h"
#include "object.h"
#include "parse.h"
#include "prolang.h"
#include "simulate.h"
#include "stralloc.h"
#include "swap.h"

struct function *simul_efunp = 0;
static int num_simul_efun    = 0;
static int total_simul_efun  = 0;

/* Don't export this pointer ever. It is used elsewhere. */
static char *simul_efun_file_name = 0;

struct object *simul_efun_object  = 0;
static struct program *simul_efun_program= 0;
struct vector *simul_efun_vector  = 0;

struct simul_efun_table_s simul_efun_table[256];

static struct ident *all_simul_efuns = 0;
static short all_discarded_simul_efun = -1;

/*
 * If there is a simul_efun file, then take care of it and extract all
 * information we need.
 */
struct object *get_simul_efun_object()
{
    struct svalue *svp;
    struct object *ob;
    struct program *progp;
    char *name;
    int i, j, num_fun;
    struct ident *id;
    struct simul_efun_table_s *entry;
    char *visible;

    for(entry = simul_efun_table, i=256; --i >= 0; ) {
	entry->funstart = 0;
	entry++;
    }
    free_defines(); /* to prevent #defines hideing places for globals */
    for(id = all_efuns; id; id = id->next_all) {
        id->u.global.sim_efun |= -0x8000;
    }
    for(id = all_simul_efuns; id; id = id->next_all) {
        id->u.global.sim_efun |= -0x8000;
    }
    if (simul_efun_program) {
	free_prog(simul_efun_program, 1);
	simul_efun_program = 0;
    }
    if (simul_efun_vector) {
	free_vector(simul_efun_vector);
	simul_efun_vector = 0;
    }
    svp = apply_master_ob(STR_GET_SEFUN, 0);
    if (svp == 0) {
	fprintf(stderr, "No simul_efun\n");
	return 0;
    }
    if (svp->type == T_POINTER) {
	simul_efun_vector = svp->u.vec;
	svp->type = T_NUMBER;
	if (VEC_SIZE(svp->u.vec))
	    svp = svp->u.vec->item;
    }
    if (svp->type != T_STRING) {
	fprintf(stderr, "No simul_efun\n");
	return 0;
    }
    name = svp->u.string;
    while (*name == '/') name++;
    if (simul_efun_file_name) free_string(simul_efun_file_name);
    simul_efun_file_name = make_shared_string(name);

    ob = find_object(simul_efun_file_name);

    if (ob == 0) {
	fprintf(stderr, "The simul_efun file %s was not loaded.\n",
		simul_efun_file_name);
	fprintf(stderr, "The function get_simul_efun in master.c must load it.\n");
	exit(1);
    }
    if (O_PROG_SWAPPED(ob) && load_ob_from_swap(ob) < 0) {
	fprintf(stderr, "Out of memory ==> No simul_efun\n");
	return 0;
    }
    reference_prog( (simul_efun_program = ob->prog), "get_simul_efun");
    num_fun = ob->prog->num_function_names;
    if (num_fun == 0)
	return 0;
    if (!simul_efunp) {
	simul_efunp = (struct function *)
	  xalloc(sizeof (struct function) * num_fun);
    } else num_fun = total_simul_efun;
    free_defines(); /* to prevent #defines hideing places for globals */
    /* locals and defines are freed now. There are still reserved words,
     * but it is impossible to define a function with the name being
     * a reserved word, thus, there will be no clashes with higher-priority
     * shared identifiers.
     */
    progp = ob->prog;
    visible = alloca(i = ob->prog->num_functions);
    bzero(visible, i);
    i = ob->prog->num_function_names;
    while(--i >= 0)
	visible[progp->function_names[i]] = 1;
    for (i=0; i < ob->prog->num_functions; i++) {
	int ix;
	uint32 flags, flags2;
	unsigned char *funstart;
	mp_int fun_ix_offs, var_ix_offs;
	struct program *inherit_progp;

	if (!visible[i])
	    continue;
	ix = i;
	flags2 = flags = progp->functions[ix];
	flags &= ~FUNSTART_MASK;
	fun_ix_offs = ix;
	var_ix_offs = 0;
	inherit_progp = progp;
	while (flags2 & NAME_INHERITED) {
	    struct inherit *inheritp;

	    inheritp = &inherit_progp->inherit[flags2 & INHERIT_MASK];
	    ix -= inheritp->function_index_offset;
	    var_ix_offs += inheritp->variable_index_offset;
	    inherit_progp = inheritp->prog;
	    flags2 = inherit_progp->functions[ix];
	}
	fun_ix_offs -= ix;
	funstart = inherit_progp->program + (flags2 & FUNSTART_MASK);
	if (funstart[2] == F_ESCAPE-F_OFFSET &&
	    funstart[3] == F_UNDEF - F_OFFSET - 0x100)
	{
	    flags |= NAME_UNDEFINED;
	}
	if ( !(flags & (TYPE_MOD_STATIC|TYPE_MOD_PRIVATE|NAME_UNDEFINED)) )
	{
	    char *function_name;
	    struct ident *p;
	    unsigned char type, num_arg, num_locals;

	    memcpy(
	      (char *)&function_name,
	      funstart - 1 - sizeof function_name,
	      sizeof function_name
	    );
	    type = funstart[-1];
	    num_arg = (funstart[0] & 0x7f);
	    num_locals = funstart[1];
	    p = make_shared_identifier(function_name, I_TYPE_GLOBAL);
	    if (p->type == I_TYPE_UNKNOWN) {
		p->type = I_TYPE_GLOBAL;
		p->u.global.function = -2;
		p->u.global.variable = -2;
		p->u.global.efun     = -1;
		p->u.global.sim_efun = -1;
		p->next_all = all_simul_efuns;
		all_simul_efuns = p;
	    }
	    if (flags & TYPE_MOD_VARARGS) num_arg = -1;
	    switch(0) { default:
		if ((j=p->u.global.sim_efun) != -1) {
		    j &= ~-0x8000;
		    if (simul_efunp[j].num_arg != num_arg) {
			int last;

			simul_efunp[j].offset.func = all_discarded_simul_efun;
			all_discarded_simul_efun = j;
			while ( (j = simul_efunp[last=j].offset.func) >= 0) {
			    if (num_arg != simul_efunp[j].num_arg)
				continue;
			    if (strcmp(function_name, simul_efunp[j].name))
				continue;
			    simul_efunp[last].offset.func =
			      simul_efunp[j ].offset.func;
			    break;
			}
		        if (j >= 0) break; /* switch */
		    } else break;          /* switch */
		}
		increment_string_ref(function_name);
		j = num_simul_efun++;
		if (num_simul_efun > num_fun) {
		    num_fun = num_simul_efun + 12;
		    simul_efunp = (struct function *) rexalloc(
	  		(char *)simul_efunp,
	  		sizeof (struct function) * num_fun
	  	    );
		}
	        simul_efunp[j].num_arg = num_arg;
	    }
	    p->u.global.sim_efun = j;
	    simul_efunp[j].name    = function_name;
	    simul_efunp[j].flags   = flags;
	    simul_efunp[j].type    = type;

	    if ((size_t)j < (sizeof simul_efun_table / sizeof simul_efun_table[0])) {
		simul_efun_table[j].funstart = funstart;
		simul_efun_table[j].program = inherit_progp;
		simul_efun_table[j].function_index_offset = fun_ix_offs;
		simul_efun_table[j].variable_index_offset = var_ix_offs;
	    }
	}
    }
    total_simul_efun = num_fun;
    simul_efun_object = ob;
    return ob;
}

#if 0
/*
 * Test if 'name' is a simul_efun. The string pointer MUST be a pointer to
 * a shared string.
 */
struct function *find_simul_efun(name)
    char *name;
{
    int i;
    for (i=0; i < num_simul_efun; i++) {
	if (name == simul_efunp[i].name)
	    return &simul_efunp[i];
    }
    return 0;
}
#endif

char *query_simul_efun_file_name() {
#ifdef DEBUG
    if (simul_efunp == 0)
	fatal("query_simul_efun_file_name called when non exists!\n");
#endif
    return simul_efun_file_name;
}

#ifdef MALLOC_smalloc

void clear_simul_efun_refs() {
    if (simul_efun_vector && simul_efun_vector->ref) {
	simul_efun_vector->ref = 0;
	clear_ref_in_vector(
	  simul_efun_vector->item,
	  VEC_SIZE(simul_efun_vector)
	);
    }
    if (simul_efun_program)
	simul_efun_program->ref = 0;
#ifdef SUPPLY_PARSE_COMMAND
    clear_ref_in_vector(
      find_living_closures,
      sizeof find_living_closures / sizeof(struct svalue)
    );
#endif
}

void count_simul_efun_refs() {
    if (simul_efun_file_name)
	count_ref_from_string(simul_efun_file_name);
    if (simul_efunp) {
	int i;

	note_malloced_block_ref((char *)simul_efunp);
	for (i = num_simul_efun; --i >= 0; )
	    count_ref_from_string(simul_efunp[i].name);
    }
    if (simul_efun_vector && !simul_efun_vector->ref++) {
	note_malloced_block_ref((char *)simul_efun_vector);
	count_ref_in_vector(
	  simul_efun_vector->item,
	  VEC_SIZE(simul_efun_vector)
	);
    }
    if (simul_efun_program)
	mark_program_ref(simul_efun_program);
#ifdef SUPPLY_PARSE_COMMAND
    count_ref_in_vector(
      find_living_closures,
      sizeof find_living_closures / sizeof(struct svalue)
    );
#endif
}

#endif /* MALLOC_smalloc */

#ifdef DEBUG
void count_simul_efun_extra_refs() {
    if (simul_efun_vector) {
	simul_efun_vector->extra_ref++;
        if ( !register_pointer( (char *)(simul_efun_vector) ) )
	    count_extra_ref_in_vector(
	      simul_efun_vector->item,
	      VEC_SIZE(simul_efun_vector)
	    );
    }
    if (simul_efun_program) {
	simul_efun_program->extra_ref++;
	if (register_pointer((char *)simul_efun_program))
	    return;
	simul_efun_program->extra_ref = 1;
	count_inherits(simul_efun_program);
    }
}
#endif
