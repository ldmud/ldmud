#ifdef __STDC__
#include <stdarg.h>
#endif
#include "lint.h"
#include "lex.h"
#include "exec.h"
#include "interpret.h"
#include "object.h"
#include "lang.h"
#include "instrs.h"
#include "switch.h"
#include "stralloc.h"

/* maximum recursion depth for compile_value */
#define MAX_LAMBDA_LEVELS 0x8000;

#define SYMTAB_START_SIZE 16
#define CODE_BUFFER_START_SIZE 1024
#define VALUE_START_MAX 0x20

#define ZERO_ACCEPTED	0x01 /* a return value of zero need not be coded */
#define VOID_ACCEPTED	0x02 /* any return value can be left out */
#define VOID_GIVEN	0x04
#define NEGATE_ACCEPTED	0x08
#define NEGATE_GIVEN	0x10
#define REF_REJECTED	0x20

#define USE_INDEX_LVALUE	0x1
#define PROTECT_LVALUE		0x2

#define VOID_WANTED (ZERO_ACCEPTED | VOID_ACCEPTED | NEGATE_ACCEPTED)

#define UNIMPLEMENTED \
	lambda_error("Unimplemented - contact amylaar@mail.cs.tu-berlin.de\n");

static void insert_value_push PROT((struct svalue *));

static INLINE int function_cmp(name, prog, ix)
    char *name;
    struct program *prog;
    int ix;
{
    int32 flags;

    ix = prog->function_names[ix];
    flags = prog->functions[ix];
    while (flags & NAME_INHERITED) {
	struct inherit *inheritp;

	inheritp = &prog->inherit[flags & INHERIT_MASK];
	prog = inheritp->prog;
	ix -= inheritp->function_index_offset;
	flags = prog->functions[ix];
    }
#ifdef ALIGN_FUNCTIONS
    return
      *(char **)(&prog->program[flags & FUNSTART_MASK] - 1 - sizeof name) -
      name;
#else
    return memcmp(
      &name,
      &prog->program[flags & FUNSTART_MASK] - 1 - sizeof name,
      sizeof name
    );
#endif
}

int find_function(name, prog)
    char *name;
    struct program *prog;
{
    int i, o, d;
    int size;

    if ( !(size = prog->num_function_names) ) return -1;
    i = size >> 1;
    o = (i+2) >> 1;
    for (;;) {
	d = function_cmp(name, prog, i);
	if (d<0) {
	    i -= o;
	    if (i<0) {
		i = 0;
	    }
	} else if (d>0) {
	    i += o;
	    if (i >= size) {
		i = size-1;
	    }
	} else {
	    return prog->function_names[i];
	}
	if (o<=1) {
	    if (function_cmp(name, prog, i)) return -1;
	    return prog->function_names[i];
	}
	o = (o+1) >> 1;
    }
}

struct s_case_state case_state;

static int switch_initialized;

static struct case_list_entry *save_case_free_block, *save_case_next_free,
		*save_case_list0, *save_case_list1;

extern char *current_file; /* used to detect when the compiler is running */

static struct work_area {
    struct symbol **symbols;
    mp_int symbol_max, symbol_mask, symbols_left;
    unsigned char *code, *codep;
    mp_int code_max, code_left;
    struct svalue *values, *valuep;
    mp_int value_max, values_left;
    mp_int num_locals;
    mp_int levels_left;
    struct work_area *last;
    struct object *lambda_origin;
    int break_stack, max_break_stack;
} current = { 0, 0, 0, 0, 0, 0 };

struct case_list_entry *case_blocks;

struct lambda_replace_program_protector {
    struct svalue l;
    struct lambda_replace_program_protector *next;
    p_int size;
    struct vector *args;
    struct svalue block;
};

int lambda_ref_replace_program(l, type, size, args, block)
    struct lambda *l;
    int type;
    p_int size;
    struct vector *args;
    struct svalue *block;
{
    struct replace_ob *r_ob;

    for (r_ob = obj_list_replace; r_ob; r_ob = r_ob->next) {
	if (r_ob->ob == current_object) {
	    struct lambda_replace_program_protector *lrpp;

	    l->ref++;
	    lrpp = (struct lambda_replace_program_protector *)
		xalloc(sizeof *lrpp);
	    lrpp->l.u.lambda = l;
	    lrpp->l.x.closure_type = type;
	    lrpp->next = r_ob->lambda_rpp;
	    r_ob->lambda_rpp = lrpp;
	    if (size) {
		lrpp->size = size;
		args->ref++;
		lrpp->args = args;
		assign_svalue_no_free(&lrpp->block, block);
	    }
	    return 1;
	}
    }
    return 0;
}

void set_closure_user(svp, owner)
    struct svalue *svp;
    struct object *owner;
{
    int type;

    if ( !CLOSURE_MALLOCED(type = svp->x.closure_type) ) {
	free_object(svp->u.ob, "set_closure_user");
	svp->u.ob = owner;
    } else if (type == CLOSURE_PRELIMINARY) {
	int ix;
	struct lambda *l;
	int32 flags;
	struct program *prog;

	prog = owner->prog;
	l = svp->u.lambda;
	ix = l->function.index;
	if ( !(prog->flags & P_REPLACE_ACTIVE) ||
	     !lambda_ref_replace_program(
		l,
		ix >= CLOSURE_IDENTIFIER_OFFS ?
		  CLOSURE_IDENTIFIER :
		  CLOSURE_LFUN,
		0, 0, 0) )
	{
	    owner->flags |= O_LAMBDA_REFERENCED;
	}
	if (ix >= CLOSURE_IDENTIFIER_OFFS) {
	    ix -= CLOSURE_IDENTIFIER_OFFS;
	    svp->x.closure_type = CLOSURE_IDENTIFIER;
	} else {
	    flags = prog->functions[ix];
	    if (flags & NAME_CROSS_DEFINED) {
		ix += (flags & INHERIT_MASK) - (INHERIT_MASK + 1 >> 1);
	    }
	    svp->x.closure_type = CLOSURE_LFUN;
	}
	free_object(l->ob, "closure");
	l->function.index = ix;
	l->ob = owner;
    }
    add_ref(owner, "set_closure_user");
}

void replace_program_lambda_adjust(r_ob, old_prog)
    struct replace_ob *r_ob;
    struct program *old_prog;
{
    extern struct object *master_ob;

    static struct lambda_replace_program_protector *current_lrpp;

    struct lambda_replace_program_protector *lrpp, *next_lrpp;
    struct error_recovery_info error_recovery_info;

    lrpp = r_ob->lambda_rpp;
    /* Adjust lfuns first, because these are possible building blocks */
    do {
	if ( !CLOSURE_HAS_CODE(lrpp->l.x.closure_type) ) {
	    if (lrpp->l.x.closure_type == CLOSURE_LFUN) {
		struct lambda *l;
		int i;

		l = lrpp->l.u.lambda;
		i = l->function.index -= r_ob->fun_offset;
		if (i < 0 || i >= r_ob->new_prog->num_functions) {
		    assert_master_ob_loaded();
		    free_object(l->ob, "replace_program_lambda_adjust");
		    add_ref(
		      l->ob = master_ob, "replace_program_lambda_adjust");
		    i = find_function(
			findstring("dangling_lfun_closure"),
			master_ob->prog
		    );
		    l->function.index = i < 0 ? 0 : i;
		}
	    } else if (lrpp->l.x.closure_type == CLOSURE_ALIEN_LFUN) {
		struct lambda *l;
		int i;

		l = lrpp->l.u.lambda;
		i = l->function.alien.index -= r_ob->fun_offset;
		if (i < 0 || i >= r_ob->new_prog->num_functions) {
		    assert_master_ob_loaded();
		    free_object(
		      l->function.alien.ob,
		      "replace_program_lambda_adjust"
		    );
		    add_ref(
		      l->function.alien.ob = master_ob,
		      "replace_program_lambda_adjust"
		    );
		    i = find_function(
			findstring("dangling_lfun_closure"),
			master_ob->prog
		    );
		    l->function.index = i < 0 ? 0 : i;
		}
	    } else /* CLOSURE_IDENTIFIER */ {
		struct lambda *l;
		int i;

		l = lrpp->l.u.lambda;
		i = l->function.index -= r_ob->var_offset;
		if (i >= r_ob->new_prog->num_variables) {
		    l->function.index = -1;
		}
	    }
	}
    } while (lrpp = lrpp->next);
    lrpp = r_ob->lambda_rpp;
    error_recovery_info.last = error_recovery_pointer;
    error_recovery_info.type = ERROR_RECOVERY_BACKEND;
    error_recovery_pointer = &error_recovery_info;
    if (setjmp(error_recovery_info.con.text)) {
	unsigned char *p;

	lrpp = current_lrpp;
	p = (unsigned char *)lrpp->l.u.lambda->function.code;
	p[3] = F_ESCAPE - F_OFFSET;
	p[4] = F_UNDEF - F_OFFSET - 0x100;
	free_vector(lrpp->args);
	free_svalue(&lrpp->block);
	free_closure(&lrpp->l);
	next_lrpp = lrpp->next;
	xfree((char*)lrpp);
	lrpp = next_lrpp;
    }
    if (lrpp) do {
	if (lrpp->l.x.closure_type == CLOSURE_LAMBDA) {
	    struct lambda *l, *l2;
	    struct svalue *svp, *svp2;
	    mp_int num_values, num_values2, code_size2;

	    current_lrpp = lrpp;
	    l = lrpp->l.u.lambda;
	    l2 = lambda(lrpp->args, &lrpp->block, l->ob);
	    svp = (struct svalue *)l;
	    if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
		num_values = svp[-0xff].u.number;
	    svp2 = (struct svalue *)l2;
	    if ( (num_values2 = EXTRACT_UCHAR(l2->function.code)) == 0xff)
		num_values2 = svp2[-0xff].u.number;
	    code_size2 = current.code_max - current.code_left;
	    if (num_values != num_values2 || lrpp->size != code_size2) {
		free_svalue(&lrpp->block);
		/* lrpp->block will be freed after the error */
		lrpp->block.type = T_CLOSURE;
		lrpp->block.x.closure_type = CLOSURE_UNBOUND_LAMBDA;
		lrpp->block.u.lambda = l2;
		error("Cannot adjust lambda closure after replace_program()\n");
	    }
	    while (--num_values >= 0)
		transfer_svalue(--svp, --svp2);
	    memcpy(l->function.code, l2->function.code, code_size2);
	    xfree((char *)svp2);
	    free_vector(lrpp->args);
	    free_svalue(&lrpp->block);
	}
	free_closure(&lrpp->l);
	next_lrpp = lrpp->next;
	xfree((char*)lrpp);
    } while (lrpp = next_lrpp);
    error_recovery_pointer = error_recovery_info.last;
}

void closure_literal(dest, ix)
    struct svalue *dest;
    int ix;
{
    extern int function_index_offset;

    struct lambda *l;
    int32 flags;
    struct program *prog;

    l = (struct lambda *)
	xalloc(sizeof *l - sizeof l->function + sizeof l->function.index);
    l->ref = 1;
    prog = current_object->prog;
    if ( !(prog->flags & P_REPLACE_ACTIVE) ||
	 !lambda_ref_replace_program(
	    l,
	    ix >= CLOSURE_IDENTIFIER_OFFS ? CLOSURE_IDENTIFIER : CLOSURE_LFUN,
	    0, 0, 0) )
    {
	current_object->flags |= O_LAMBDA_REFERENCED;
    }
    if (ix >= CLOSURE_IDENTIFIER_OFFS) {
	extern struct svalue *current_variables;

	ix +=
	  -CLOSURE_IDENTIFIER_OFFS +
	  (current_variables - current_object->variables);
	dest->x.closure_type = CLOSURE_IDENTIFIER;
    } else {
	ix += function_index_offset;
	flags = prog->functions[ix];
	if (flags & NAME_CROSS_DEFINED) {
	    ix += (flags & INHERIT_MASK) - (INHERIT_MASK + 1 >> 1);
	}
	dest->x.closure_type = CLOSURE_LFUN;
    }
    l->ob = current_object;
    l->function.index = ix;
    add_ref(current_object, "closure");
    dest->type = T_CLOSURE;
    dest->u.lambda = l;
}

struct symbol {
    char *name;
    struct symbol *next;
    struct symbol *next_local;
    int index;
};

/*VARARGS1*/
static void lambda_error
	VARPROT((char *error_str, ...), printf, 1, 2) NORETURN;

void realloc_values() {
    mp_int new_max;
    struct svalue *new_values;

    new_max = current.value_max << 1;
    new_values = (struct svalue *)xalloc(new_max * sizeof new_values[0]);
    if (!new_values)
	lambda_error("Out of memory\n");
    current.values_left += current.value_max;
    memcpy(
	(current.valuep = new_values + current.value_max),
	current.values,
	current.value_max * sizeof new_values[0]
    );
    xfree((char *)current.values);
    current.values = new_values;
    current.value_max = new_max;
}

void realloc_code() {
    mp_int new_max;
    unsigned char *new_code;

    new_max = current.code_max << 1;
    new_code = rexalloc(current.code, new_max);
    if (!new_code)
	lambda_error("Out of memory\n");
    current.code_left += current.code_max;
    current.code_max = new_max;
    current.codep += new_code - current.code;
    current.code = new_code;
}

static void free_symbols();

#ifdef __STDC__
static void lambda_error(char *error_str, ...)
{
    va_list va;
#else
static void lambda_error(error_str, a1)
    char *error_str;
    char *a1;
{
#endif
    for (;;) {
	free_symbols();
	if (current.code)
	    xfree(current.code);
	if (current.values) {
	    mp_int num_values = current.value_max - current.values_left;
	    struct svalue *svp;

	    for (svp = current.valuep; --num_values >= 0; )
		free_svalue(svp++);
	    xfree((char *)current.values);
	}
	if (!current.last) break;
	current = *current.last;
    }
#ifdef __STDC__
    va_start(va, error_str);
    error(error_str, va_arg(va, char *)); /* One arg or nothing :-) */
    va_end(va);
#else
    error(error_str, a1);
#endif
}

void lambda_cerror(s)
    char *s;
{
    lambda_error("%s\n", s);
}

void lambda_cerrorl(s1, s2, line1, line2)
    char *s1, *s2;
    int line1, line2;
{
    lambda_error(s1, "\n");
}

char *lambda_get_space(size)
    p_int size;
{
    while (current.code_left < size)
	realloc_code();
    current.code_left -= size;
    current.codep += size;
    return (char*)current.codep - size;
}

void lambda_move_switch_instructions(len, blocklen)
    int len;
    p_int blocklen;
{
    while (current.code_left < len)
	realloc_code();
    current.code_left -= len;
    current.codep += len;
    move_memory(
      current.codep - blocklen,
      current.codep - blocklen - len,
      blocklen
    );
}

static void free_symbols()
{
    p_int i;
    struct symbol **symp, *sym, *next;

    i = current.symbol_max;
    symp = current.symbols;
    do {
	for (sym = *symp++; sym; sym = next) {
	    next = sym->next;
	    xfree((char *)sym);
	}
    } while (i -= sizeof sym);
    xfree((char *)current.symbols);
    if (switch_initialized) {
	if (current_file) {
	    case_state.free_block = save_case_free_block;
	    case_state.next_free  = save_case_next_free;
	    case_state.list0 = save_case_list0;
	    case_state.list1 = save_case_list1;
	} else {
	    while (case_blocks) {
		struct case_list_entry *tmp;

		tmp = case_blocks;
		case_blocks = tmp->next;
		xfree(tmp);
	    }
	}
    }
}

struct symbol *make_symbol(name)
    char *name;
{
    p_int h;
    struct symbol *sym, **symp;

    h = (p_int)name;
    h ^= h >> 16;
    h ^= h >> 8;
    h ^= h >> 4;
    h &= current.symbol_mask;
    symp = (struct symbol **)((char *)current.symbols + h);
    for (sym = *symp; sym; sym = sym->next) {
	if (sym->name == name)
	    return sym;
    }
    sym = (struct symbol *)xalloc(sizeof *sym);
    if (!sym)
	lambda_error("Out of memory\n");
    sym->name = name;
    sym->index = -1;
    sym->next = *symp;
    *symp = sym;
    if ( !(current.symbols_left -= sizeof sym) ) {
	struct symbol **newtab, *sym2;
	p_int i;

	sym2 = sym;
	current.symbols_left = current.symbol_max;
	current.symbol_max <<= 1;
	symp = newtab = (struct symbol **)xalloc(current.symbol_max);
	if (!symp) {
	    current.symbol_max >>= 1;
	    xfree((char*)sym);
	    lambda_error("Out of memory\n");
	}
	current.symbol_mask = i = current.symbol_max - sizeof sym;
	do {
	    *symp++ = 0;
	} while ((i -= sizeof sym) >= 0);
	i = current.symbols_left - sizeof sym;
	do {
	    struct symbol *next;

	    for (sym = *(struct symbol **)((char *)current.symbols+i);
		 sym; sym = next)
	    {
		next = sym->next;
		h = (p_int)sym->name;
		h ^= h >> 16;
		h ^= h >> 8;
		h ^= h >> 4;
		h &= current.symbol_mask;
		symp = (struct symbol **)((char *)newtab + h);
		sym->next = *symp;
		*symp = sym;
	    }
	} while ((i -= sizeof sym) >= 0);
	xfree((char *)current.symbols);
	current.symbols = newtab;
	return sym2;
    }
    return sym;
}

/* compile_lvalue does not only supply an lvalue, but also 1 byte space to
 * store the assignment code
 */
void compile_lvalue PROT((struct svalue *, int));

int compile_value(value, opt_flags)
    struct svalue *value;
    int opt_flags;
{
    extern struct svalue const0, const1;

    if (!--current.levels_left)
	lambda_error("Too deep recursion inside lambda()\n");
    switch(value->type) {
      case T_POINTER:
      {
	struct vector *block;
	struct svalue *argp;
	ph_int type;
    
	block = value->u.vec;
	argp = block->item;
	if (block == &null_vector || argp->type != T_CLOSURE) {
	    lambda_error("Missing function\n");
	}
	if ( (type = argp->x.closure_type) < (ph_int)CLOSURE_SIMUL_EFUN) {
	    if (type < (ph_int)CLOSURE_EFUN) {
		/* operator */
		mp_int block_size;

		block_size = VEC_SIZE(block);
		switch(type - CLOSURE_OPERATOR) {
		  default:
		    lambda_error("Unimplemented operator %s for lambda()\n",
		      instrs[type - CLOSURE_OPERATOR].name);
		  case F_LOR-F_OFFSET:
		  case F_LAND-F_OFFSET:
		  {
		    mp_int *branchp;
		    mp_int i, start, end;
		    int code = type - CLOSURE_OPERATOR;
		    int void_given;

		    if (opt_flags & VOID_ACCEPTED) {
			code =
			  code == F_LAND-F_OFFSET ?
			    F_BRANCH_WHEN_ZERO - F_OFFSET :
			    F_BRANCH_WHEN_NON_ZERO - F_OFFSET;
			opt_flags |= VOID_GIVEN;
		    }
		    i = block_size - 1;
		    branchp = alloca(i * sizeof *branchp);
		    while (--i > 0) {
			compile_value(++argp, REF_REJECTED);
			if (current.code_left < 2)
			    realloc_code();
			*branchp++ = current.code_max - current.code_left;
			current.code_left -= 2;
			*current.codep = code;
			current.codep += 2;
		    }
		    void_given = compile_value(
		      i ?
			(code == F_LAND-F_OFFSET ? &const1 : &const0) :
			++argp,
		      opt_flags & (VOID_ACCEPTED|REF_REJECTED)
		    );
		    if (opt_flags & VOID_ACCEPTED && !(void_given & VOID_GIVEN))
		    {
			if (current.code_left < 1)
			    realloc_code();
			current.code_left--;
			*current.codep++ = F_POP_VALUE-F_OFFSET;
		    }
		    i = block_size - 1;
		    end = current.code_max - current.code_left;
		    while (--i > 0) {
			mp_int offset;

			start = *--branchp;
			offset = end - start - 2;
			if (offset <= 0xff) {
			    current.code[start+1] = offset;
			    continue;
			} else {
			    mp_int growth;
			    int growth_factor;
			    mp_int j;
			    char *p, *q;

			    if (opt_flags & VOID_ACCEPTED) {
				growth = i;
				growth_factor = 1;
				code +=
				  F_LBRANCH_WHEN_ZERO - F_BRANCH_WHEN_ZERO;
			    } else {
				growth = i * 3;
				growth_factor = 3;
				code += F_LBRANCH_WHEN_ZERO - F_LAND;
			    }
			    if (current.code_left < growth)
				realloc_code();
			    current.code_left -= growth;
			    current.codep += growth;
			    p = current.code + end;
			    q = p + growth;
			    /* - 1 is precompensation for leading branch code */
			    end += growth_factor - 1;
			    if ( !(opt_flags & VOID_ACCEPTED) )
				/* offset precompensation for leading F_DUP */
				end--;
			    branchp++;
			    do {
				char tmp_short[2];
				start = *--branchp;
				offset = end - start;
				end += growth_factor;
				if (offset > 0x7fff)
				    UNIMPLEMENTED
				*(short *)tmp_short = offset;
				j = p - (char *)&current.code[start+2];
				do {
				    *--q = *--p;
				} while (--j);
				if (opt_flags & VOID_ACCEPTED) {
				    *--q = tmp_short[1];
				    *--q = tmp_short[0];
				    *--q = code;
				} else {
				    *--q = F_POP_VALUE - F_OFFSET;
				    *--q = tmp_short[1];
				    *--q = tmp_short[0];
				    *--q = code;
				    *--q = F_DUP - F_OFFSET;
				}
				p -= 2;
			    } while (--i > 0);
			    break;
			}
		    }
		    break;
		  }
		  case F_BRANCH_WHEN_ZERO-F_OFFSET:
		  case F_BRANCH_WHEN_NON_ZERO-F_OFFSET:
		  {
		    mp_int *branchp;
		    mp_int i, start, end, void_dest, non_void_dest;
		    int code = type - CLOSURE_OPERATOR;
		    int opt_used, all_void;
		    mp_int last_branch;

		    if ( !(block_size & 1) &&
			 opt_flags & (VOID_ACCEPTED|ZERO_ACCEPTED) &&
			 ( opt_flags & VOID_ACCEPTED ?
			   argp[block_size-1].type != T_POINTER
				/* no side effect */ :
			   argp[block_size-1].type == T_NUMBER &&
				!argp[block_size-1].u.number
			 ) )
		    {
			/* ignore default, it is equivalent to 0. */
			block_size--;
		    }
		    i = block_size;
		    branchp = alloca(i * sizeof *branchp);
		    all_void = VOID_GIVEN;
		    while ( (i -= 2) > 0) {
			mp_int offset;

			opt_used = compile_value(++argp, NEGATE_ACCEPTED);
			if (current.code_left < 2)
			    realloc_code();
			last_branch = current.code_max - current.code_left;
			current.code_left -= 2;
			*current.codep = opt_used & NEGATE_GIVEN ?
			    (code == F_BRANCH_WHEN_NON_ZERO-F_OFFSET ?
				F_BRANCH_WHEN_ZERO-F_OFFSET :
				F_BRANCH_WHEN_NON_ZERO-F_OFFSET) :
			    code;
			current.codep += 2;
			++argp;
			opt_used =
			  compile_value(
			    argp,
			    i == 1 && !all_void ?
				opt_flags & REF_REJECTED :
				opt_flags &
				  (VOID_ACCEPTED|ZERO_ACCEPTED|REF_REJECTED)
			  );
			all_void &= opt_used;
			if (current.code_left < 4)
			    realloc_code();
			offset =
			  current.code_max - current.code_left - last_branch;
			/* Allow the offset to be incremented
			 * by one afterwards.
			 */
			if (offset > 0xfe) {
			    char *p, tmp_short[2];
			    mp_int j;

			    p = (char*)current.codep++;
			    j = offset - 2;
			    if (offset > 0x7ffd)
				UNIMPLEMENTED
			    do {
				p--;
				p[1] = *p;
			    } while (--j);
			    current.code_left--;
			    *((short *)tmp_short) = offset + 2;
			    current.code[last_branch] +=
			      F_LBRANCH_WHEN_ZERO - F_BRANCH_WHEN_ZERO;
			    current.code[last_branch+1] = tmp_short[0];
			    current.code[last_branch+2] = tmp_short[1];
			} else {
			    current.code[last_branch+1] = offset;
			}
			*branchp++ = current.code_max - current.code_left;
			*branchp++ = last_branch;
			current.code_left -= 2;
			*current.codep++ = F_BRANCH-F_OFFSET;
			*current.codep++ = opt_used;
		    }
		    if ( i /* no default */ &&
			 ( opt_flags & VOID_ACCEPTED ||
			   (all_void && opt_flags & ZERO_ACCEPTED)
			 ) )
		    {
			mp_int offset;

			opt_flags |= VOID_GIVEN;
			if (all_void) {
			    if (block_size < 2) {
				break;
			    }
			    offset = -2;
			    void_dest =
			      current.code_max - current.code_left - 2;
			} else {
			    /* Terminating void after non-void is avoided */
			    current.codep[-2] = F_POP_VALUE - F_OFFSET;
			    offset = -1;
			    non_void_dest =
			      current.code_max - current.code_left - 2;
			    void_dest = non_void_dest + 1;
			}
			start = *--branchp;
			code = current.code[start];
			if (code == F_LBRANCH_WHEN_ZERO-F_OFFSET ||
			    code == F_LBRANCH_WHEN_NON_ZERO-F_OFFSET)
			{
			    char tmp_short[2];

			    tmp_short[0] = current.code[start+1];
			    tmp_short[1] = current.code[start+2];
			    (*(short *)tmp_short) += offset;
			    current.code[start+1] = tmp_short[0];
			    current.code[start+2] = tmp_short[1];
			} else {
			    current.code[start+1] += offset;
			}
			current.codep += offset;
			current.code_left -= offset;
			branchp--;
			i = block_size - 2;
		    } else {
			/* the following assignment is only valid if
			 *                no V default
			 * if ( !all_void && i &&
			 *   ( (opt_flags & (VOID_ACCEPTED|ZERO_ACCEPTED)) ==
			 *     ZERO_ACCEPTED) )
			 * is met, and it is only needed when there is at
			 * least one void expression, too.
			 * However, it's easier to do the assignment
			 * all the time, and it does no harm here.
			 */
			void_dest = current.code_max - current.code_left;

			opt_used = compile_value(
			  i ? &const0 : ++argp,
			  opt_flags &
			    ( all_void ?
			      (VOID_ACCEPTED|ZERO_ACCEPTED|REF_REJECTED) :
			      REF_REJECTED
			    )
			);
			non_void_dest =
			  current.code_max - current.code_left;
			if (opt_used & VOID_GIVEN) {
			    void_dest = non_void_dest;
			    opt_flags |= VOID_GIVEN;
			} else if (opt_flags & VOID_ACCEPTED) {
			    opt_flags |= VOID_GIVEN;
			    if (current.code_left < 1)
				realloc_code();
			    current.code_left--;
			    *current.codep++ = F_POP_VALUE-F_OFFSET;
			    opt_used = VOID_GIVEN;
			    void_dest = non_void_dest + 1;
			} else if (all_void && block_size > 2) {
			    if (current.code_left < 3)
				realloc_code();
			    if (block_size > 4 ||
				branchp[-2] - branchp[-1] > 0xfd)
			    {
				void_dest = non_void_dest + 2;
				current.code_left -= 3;
				*current.codep++ = F_BRANCH-F_OFFSET;
				*current.codep++ = 1;
				*current.codep++ = F_CONST0-F_OFFSET;
			    } else {
				current.code_left--;
				start = branchp[-2];
				move_memory(
				  &current.code[start+1],
				  &current.code[start],
				  non_void_dest - start
				);
				current.codep++;
				current.code[start] = F_CONST0-F_OFFSET;
				/* void_dest = start; */
				current.code[start+2] = 0; /* not void */
				branchp[-2] = start+1;
				current.code[branchp[-1]+1]++;
				non_void_dest++;
				/* all_void isn't used any more, else we'd
				 * need to zero it now.
				 */
			    }
			} else if (!i && !all_void &&
				   opt_flags & ZERO_ACCEPTED)
			{
			    mp_int *branchp2, j;

			    branchp2 = branchp;
			    for (j = block_size;  (j -= 2) > 0; ) {
				start = *(branchp2 -= 2);
				if (current.code[start+1] & VOID_GIVEN) {
				    void_dest = non_void_dest + 2;
				    non_void_dest += 3;
				    if (current.code_left < 3)
					realloc_code();
				    current.code_left -= 3;
				    *current.codep++ = F_BRANCH-F_OFFSET;
				    *current.codep++ = 1;
				    *current.codep++ = F_CONST0-F_OFFSET;
				    break;
				}
			    }
			}
			i = block_size;
		    }
		    end = current.code_max - current.code_left;
		    while ( (i -= 2) > 0) {
			mp_int offset;

			start = *(branchp -= 2);
			offset = current.code[start+1] & VOID_GIVEN ?
			    void_dest - start - 2:
			    non_void_dest - start - 2;
			if (offset <= 0xff) {
			    current.code[start+1] = offset;
			    continue;
			} else {
			    mp_int growth;
			    mp_int j;
			    unsigned char *p, *q;

			    growth = i+1 >> 1;
			    if (current.code_left < growth)
				realloc_code();
			    current.code_left -= growth;
			    current.codep += growth;
			    p = current.code + end;
			    q = p + growth;
			    branchp +=2;
			    do {
				char tmp_short[2];

				start = *--branchp;
				code = current.code[start];
				if (code == F_LBRANCH_WHEN_ZERO-F_OFFSET ||
				    code == F_LBRANCH_WHEN_NON_ZERO-F_OFFSET)
				{
				    tmp_short[0] = current.code[start+1];
				    tmp_short[1] = current.code[start+2];
				    (*(short *)tmp_short)++;
				    current.code[start+1] = tmp_short[0];
				    current.code[start+2] = tmp_short[1];
				} else {
				    current.code[start+1]++;
				}
				start = *--branchp;
				offset = current.code[start+1] & VOID_GIVEN ?
				    void_dest - start - 1:
				    non_void_dest - start - 1;
				end++;
				void_dest++;
				non_void_dest++;
				if (offset > 0x7fff)
				    UNIMPLEMENTED
				*(short *)tmp_short = offset;
				j = (p - (current.code + start)) - 2;
				do {
				    *--q = *--p;
				} while (--j);
				*--q = tmp_short[1];
				*--q = tmp_short[0];
				*--q = *(p-=2) +
				    (F_LBRANCH_WHEN_ZERO - F_BRANCH_WHEN_ZERO);
			    } while ( (i -= 2) > 0);
			    break;
			}
		    }
		    break;
		  }
		  case F_POP_VALUE-F_OFFSET:
		  {
		    mp_int i;
		    int void_given;

		    for (i = block_size - 1; --i > 0; ) {
			void_given = compile_value(++argp, VOID_WANTED);
			if ( !(void_given & VOID_GIVEN) ) {
			    if (current.code_left < 1)
				realloc_code();
			    current.code_left--;
			    *current.codep++ = F_POP_VALUE - F_OFFSET;
			}
		    }
		    opt_flags = compile_value(i ? &const0 : ++argp, opt_flags);
		    break;
		  }
		  case F_ASSIGN-F_OFFSET:
		  {
		    mp_int i;

		    /* There must be at least one assignment in order to get
		     * a return value.
		     */
		    if ( !(i = block_size - 1) || (i & 1) )
			lambda_error("Missing value in assignment\n");
		    argp++;
		    for (; (i -= 2) >= 0; argp+=2) {
			compile_value(argp+1, REF_REJECTED);
			compile_lvalue(argp, USE_INDEX_LVALUE);
			if (!i) {
			    if (opt_flags & VOID_ACCEPTED) {
				opt_flags = VOID_GIVEN;
				*current.codep++ = F_VOID_ASSIGN - F_OFFSET;
			    } else {
				*current.codep++ = F_ASSIGN - F_OFFSET;
			    }
			} else {
			    *current.codep++ = F_VOID_ASSIGN - F_OFFSET;
			}
		    }
		    break;
		  }
		  case F_ADD_EQ-F_OFFSET:
		    if (block_size != 3)
			goto generic_assign_error;
		    if (argp[2].type == T_NUMBER && argp[2].u.number == 1) {
			if (opt_flags & VOID_ACCEPTED) {
			    opt_flags = VOID_GIVEN;
			    type = F_INC - F_OFFSET;
			} else {
			    type = F_PRE_INC - F_OFFSET;
			}
			goto generic_modify;
		    }
		    if (opt_flags & VOID_ACCEPTED) {
			opt_flags = VOID_GIVEN;
			type = F_VOID_ADD_EQ - F_OFFSET;
		    }
		    goto generic_assign;
		  case F_SUB_EQ-F_OFFSET:
		    if (block_size != 3)
			goto generic_assign_error;
		    if (argp[2].type == T_NUMBER && argp[2].u.number == 1) {
			if (opt_flags & VOID_ACCEPTED) {
			    opt_flags = VOID_GIVEN;
			    type = F_DEC - F_OFFSET;
			} else {
			    type = F_PRE_DEC - F_OFFSET;
			}
			goto generic_modify;
		    }
		    goto generic_assign;
		  case F_MULT_EQ-F_OFFSET:
		  case F_AND_EQ-F_OFFSET:
		  case F_OR_EQ-F_OFFSET:
		  case F_XOR_EQ-F_OFFSET:
		  case F_LSH_EQ-F_OFFSET:
		  case F_RSH_EQ-F_OFFSET:
		  case F_DIV_EQ-F_OFFSET:
		  case F_MOD_EQ-F_OFFSET:
		    if (block_size != 3) {
		  generic_assign_error:
			lambda_error(
			  "Bad number of arguments to #'%s\n",
			  instrs[type - CLOSURE_OPERATOR].name
			);
		    }
		  generic_assign:
		    compile_value(argp+2, REF_REJECTED);
		  generic_modify:
		    compile_lvalue(argp+1, USE_INDEX_LVALUE);
		    *current.codep++ = type;
		    break;
		  case F_POST_INC:
		  case F_POST_DEC:
		    if (block_size != 2)
			goto generic_assign_error;
		    if (opt_flags & VOID_ACCEPTED) {
			opt_flags = VOID_GIVEN;
			type -= F_POST_INC - F_INC;
		    }
		    goto generic_modify;
		  case F_BBRANCH_WHEN_NON_ZERO-F_OFFSET: /* #'do */
		  {
		    mp_int i;
		    int void_given;
		    mp_int offset;

		    i = block_size - 3;
		    if (i < 0)
			lambda_error("Missing argument(s) to #'do\n");
		    offset = current.code_left - current.code_max;
		    if (i) do {
			void_given = compile_value(++argp, VOID_WANTED);
			if ( !(void_given & VOID_GIVEN) ) {
			    if (current.code_left < 1)
				realloc_code();
			    current.code_left--;
			    *current.codep++ = F_POP_VALUE - F_OFFSET;
			}
		    } while(--i);
		    void_given =
		      compile_value(++argp, NEGATE_ACCEPTED);
		    offset += current.code_max - current.code_left + 1;
		    if (current.code_left < 3)
			realloc_code();
		    if (offset > 0xff) {
			char tmp_short[2];

			if (offset > 0x8000)
			    UNIMPLEMENTED
			current.code_left -= 3;
			*((short *)tmp_short) = -offset;
			*current.codep++ = void_given & NEGATE_GIVEN ?
			    F_LBRANCH_WHEN_ZERO - F_OFFSET		 :
			    F_LBRANCH_WHEN_NON_ZERO - F_OFFSET;
			*current.codep++ = tmp_short[0];
			*current.codep++ = tmp_short[1];
		    } else {
			current.code_left -= 2;
			*current.codep++ = void_given & NEGATE_GIVEN ?
			    F_BBRANCH_WHEN_ZERO - F_OFFSET		 :
			    F_BBRANCH_WHEN_NON_ZERO - F_OFFSET;
			*current.codep++ = offset;
		    }
		    opt_flags = compile_value(++argp, opt_flags);
		    break;
		  }
		  case F_BBRANCH_WHEN_ZERO-F_OFFSET: /* #'while */
		  {
		    mp_int i;
		    int void_given;
		    mp_int start_branch;
		    mp_int offset;

		    if (current.code_left < 2)
			realloc_code();
		    current.code_left -= 2;
		    start_branch = current.code_max - current.code_left;
		    *current.codep = F_BRANCH - F_OFFSET;
		    current.codep += 2;
		    i = block_size - 3;
		    if (i < 0)
			lambda_error("Missing argument(s) to #'while\n");
		    offset = current.code_left - current.code_max;
		    argp += 2;
		    if (i) do {
			void_given = compile_value(++argp, VOID_WANTED);
			if ( !(void_given & VOID_GIVEN) ) {
			    if (current.code_left < 2)
				realloc_code();
			    current.code_left--;
			    *current.codep++ = F_POP_VALUE - F_OFFSET;
			}
		    } while(--i);
		    offset =
		      current.code_max - current.code_left - start_branch;
		    if (offset > 0xff) {
			char *p, tmp_short[2];

			if (offset > 0x7ffd)
			    UNIMPLEMENTED
			if (current.code_left < 1)
			    realloc_code();
			current.code_left--;
			p = (char*)current.codep++;
			i = offset;
			do {
			    p--;
			    p[1] = *p;
			} while (--i);
			*((short *)tmp_short) = offset + 2;
			current.code[start_branch-2] = F_LBRANCH - F_OFFSET;
			current.code[start_branch-1] = tmp_short[0];
			current.code[start_branch-0] = tmp_short[1];
			start_branch++;
		    } else {
			current.code[start_branch-1] = offset;
		    }
		    argp = block->item;
		    void_given =
		      compile_value(++argp, NEGATE_ACCEPTED);
		    if (current.code_left < 3)
			realloc_code();
		    offset =
		      current.code_max - current.code_left - start_branch + 1;
		    if (offset > 0xff) {
			char tmp_short[2];

			if (offset > 0x8000)
			    UNIMPLEMENTED
			current.code_left -= 3;
			*((short *)tmp_short) = -offset;
			*current.codep++ = void_given & NEGATE_GIVEN ?
			    F_LBRANCH_WHEN_ZERO - F_OFFSET		 :
			    F_LBRANCH_WHEN_NON_ZERO - F_OFFSET;
			*current.codep++ = tmp_short[0];
			*current.codep++ = tmp_short[1];
		    } else {
			current.code_left -= 2;
			*current.codep++ = void_given & NEGATE_GIVEN ?
			    F_BBRANCH_WHEN_ZERO - F_OFFSET		 :
			    F_BBRANCH_WHEN_NON_ZERO - F_OFFSET;
			*current.codep++ = offset;
		    }
		    opt_flags = compile_value(++argp, opt_flags);
		    break;
		  }
		  case F_CATCH-F_OFFSET:
		  {
		    mp_int start, offset;
		    int void_given;

		    if (block_size != 2)
			lambda_error("Wrong number of arguments to #'catch\n");
		    if (current.code_left < 2)
			realloc_code();
		    current.code_left -= 2;
		    *current.codep++ = F_CATCH - F_OFFSET;
		    *current.codep++ = 0;
		    start = current.code_max - current.code_left;
		    void_given = compile_value(++argp, 0);
		    if (current.code_left < 2)
			realloc_code();
		    current.code_left -= 2;
		    *current.codep++ = F_ESCAPE - F_OFFSET;
		    *current.codep++ = F_END_CATCH - F_OFFSET - 0x100;
		    offset = current.code_max - current.code_left - start;
		    if (offset > 0xff) {
			UNIMPLEMENTED
		    }
		    current.code[start-1] = offset;
		    break;
		  }
		  case F_NOT-F_OFFSET:
		  {
		    if (block_size != 2)
			lambda_error("Wrong number of arguments to #'!\n");
		    opt_flags |=
		      compile_value(++argp, opt_flags & ~ZERO_ACCEPTED);
		    if (opt_flags & (NEGATE_ACCEPTED|VOID_GIVEN) ) {
			opt_flags ^= NEGATE_GIVEN;
		    } else {
			if (current.code_left < 1)
			    realloc_code();
			current.code_left--;
			*current.codep++ = F_NOT - F_OFFSET;
		    }
		    break;
		  }
		  case F_AND-F_OFFSET:
		  {
		    int i;

		    if ( (i = block_size - 2) > 0) {
			compile_value(++argp, 0);
			do {
			    compile_value(++argp, 0);
			    if (current.code_left < 1)
				realloc_code();
			    current.code_left--;
			    *current.codep++ = F_AND - F_OFFSET;
			} while (--i);

		    } else if (!i) {
			if (opt_flags & REF_REJECTED)
			    lambda_error("Reference value in bad position\n");
			compile_lvalue(++argp, PROTECT_LVALUE|USE_INDEX_LVALUE);
			current.code_left++;
		    } else {
			lambda_error("Missing argument(s) to #'&\n");
		    }
		    break;
		  }
		  case F_SSCANF-F_OFFSET:
		  {
		    int lvalues;

		    if ( (lvalues = block_size - 3) < 0)
			lambda_error("Missing argument(s) to #'sscanf\n");
		    if (lvalues > 0xff - 2)
			lambda_error("Too many arguments to #'sscanf\n");
		    compile_value(++argp, 0);
		    compile_value(++argp, 0);
		    while (--lvalues >= 0) {
			compile_lvalue(++argp, PROTECT_LVALUE|USE_INDEX_LVALUE);
			current.code_left++;
		    }
		    if (current.code_left < 2)
			realloc_code();
		    current.code_left -= 2;
		    *current.codep++ = F_SSCANF - F_OFFSET;
		    *current.codep++ = block_size - 1;
		    break;
		  }
		  case F_AGGREGATE-F_OFFSET:
		  {
		    int i;
		    char size[2];

		    i = block_size - 1;
		    *(short *)size = i;
		    while (--i >= 0) {
			compile_value(++argp, REF_REJECTED);
		    }
		    if (current.code_left < 3)
			realloc_code();
		    current.code_left -= 3;
		    *current.codep++ = F_AGGREGATE-F_OFFSET;
		    *current.codep++ = size[0];
		    *current.codep++ = size[1];
		    break;
		  }
		  case F_M_CAGGREGATE-F_OFFSET:
		  {
		    int i, j, num_keys, num_values;

		    num_values = 1;
		    i = block_size;
		    num_keys = i - 1;
		    for (i = block_size; --i;) {
			struct svalue *element;

			if ( (++argp)->type != T_POINTER )
			    lambda_error("Bad argument to #'([\n");
			element = argp->u.vec->item;
			j = VEC_SIZE(argp->u.vec);
			if (j != num_values) {
			    if (!j)
				lambda_error("#'([ : Missing key.\n");
			    if (i != num_keys)
				lambda_error(
				  "#'([ : Inconsistent value count.\n");
			    num_values = j;
			}
			while (--j >= 0) {
			    compile_value(element++, REF_REJECTED);
			}
		    }
		    if (current.code_left < 5)
			realloc_code();
                    num_values--; /* one item of each subarray is the key */
		    if ( (num_keys | num_values) & ~0xff) {
			char size[2];

			current.code_left -= 5;
			*current.codep++ = F_M_AGGREGATE-F_OFFSET;
			*(short *)size = num_keys;
			*current.codep++ = size[0];
			*current.codep++ = size[1];
			*(short *)size = num_values;
			*current.codep++ = size[0];
			*current.codep++ = size[1];
		    } else {
			current.code_left -= 3;
			*current.codep++ = F_M_CAGGREGATE-F_OFFSET;
			*current.codep++ = num_keys;
			*current.codep++ = num_values;
		    }
		    break;
		  }
		  case F_RETURN-F_OFFSET:
		  {
		    if (block_size != 2) {
			if (block_size > 1)
			    lambda_error("Too many arguments to #'return\n");
			opt_flags = VOID_GIVEN;
		    } else {
			opt_flags =
			  compile_value(++argp, ZERO_ACCEPTED|REF_REJECTED);
		    }
		    if (current.code_left < 1)
			realloc_code();
		    current.code_left--;
		    *current.codep++ =
		      opt_flags & VOID_GIVEN ?
			F_RETURN0 - F_OFFSET :
			F_RETURN - F_OFFSET;
		    break;
		  }
		  case F_EXTRACT_LVALUE-F_OFFSET:
		  {
		    if (block_size != 3)
			lambda_error("Bad number of arguments to #'[<..\n");
		    compile_value(++argp, REF_REJECTED);
		    if ((++argp)->type == T_NUMBER) {
			static struct svalue tmp_svalue = { T_NUMBER };

			tmp_svalue.u.number = - argp->u.number;
			compile_value(&tmp_svalue, 0);
		    } else {
			compile_value(argp, REF_REJECTED);
			if (current.code_left < 1)
			    realloc_code();
			current.code_left--;
			*current.codep++ = F_NEGATE - F_OFFSET;
		    }
		    if (current.code_left < 1)
			realloc_code();
		    current.code_left--;
		    *current.codep++ = F_EXTRACT2 - F_OFFSET;
		    break;
		  }
		  case F_SWITCH-F_OFFSET:
		  {
		    mp_int num_blocks, i, switch_pc, default_addr = 0;
		    int some_numeric = 0, no_string = 1;
		    struct case_list_entry *zero = 0;
		    struct case_list_entry *save_free_block, *save_next_free,
			*save_list0, *save_list1;

		    if (!switch_initialized) {
			switch_initialized = 1;
			if (current_file) {
			    save_case_free_block = case_state.free_block;
			    save_case_next_free  = case_state.next_free;
			    save_case_list0 = case_state.list0;
			    save_case_list1 = case_state.list1;
			} else {
			    case_blocks = 0;
			    case_state.free_block = (struct case_list_entry *)(
			      ((PTRTYPE)(&case_blocks))-
			      ((PTRTYPE)(&((struct case_list_entry*)0)->next)-
					(PTRTYPE) 0)
			    );
			    case_state.next_free = case_state.free_block + 1;
			}
		    }
		    num_blocks = (block_size) / 3;
		    if (block_size != 2 + num_blocks*3)
			lambda_error("Bad number of arguments to #'switch\n");
		    compile_value(++argp, REF_REJECTED);
		    if (current.code_left < 3)
			realloc_code();
		    current.code_left -= 3;
		    *current.codep = F_SWITCH - F_OFFSET;
		    current.codep += 3;
		    switch_pc = current.code_max - current.code_left - 2;
		    if (++current.break_stack > current.max_break_stack)
			current.max_break_stack = current.break_stack;
		    save_free_block = case_state.free_block;
		    save_next_free  = case_state.next_free;
		    save_list0 = case_state.list0;
		    save_list1 = case_state.list1;
		    case_state.list0 = case_state.list1 = 0;
		    for (i = num_blocks; --i >= 0;) {
			struct svalue *labels;
			mp_int j;
			struct case_list_entry *l;
			int opt_used;

			++argp;
			if (argp->type == T_POINTER) {
			    labels = argp->u.vec->item;
			    j = VEC_SIZE(argp->u.vec);
			} else {
			    labels = argp;
			    j = 1;
			}
			for (; j--; labels++) {
			    l = new_case_entry();
			    l->addr =
			      current.code_max - current.code_left - switch_pc;
			    l->line = 1;
			    if (j && labels[1].type == T_CLOSURE &&
				  labels[1].x.closure_type ==
				  F_RANGE -F_OFFSET+CLOSURE_EFUN )
			    {
				if (j < 2) {
				    lambda_error(
				      "case label range lacks end\n"
				    );
				}
				if (labels[0].type != T_NUMBER ||
				    labels[2].type != T_NUMBER   )
				{
				    lambda_error(
				      "case label range must be numeric\n"
				    );
				}
				if (!no_string)
				    lambda_error(
				      "mixed case label lists not supported\n"
				    );
				some_numeric = 1;
				l->key = labels->u.number;
				j -= 2;
				labels += 2;
				if (labels[-2].u.number == labels->u.number)
				    continue;
				if (labels[-2].u.number > labels->u.number)
				    goto reuse_list_entry;
				l->addr = 1;
				l = new_case_entry();
				l->addr =
				  current.code_max - current.code_left -
				    switch_pc;
				l->line = 0;
				l->key = labels->u.number;
			    } else if (labels->type == T_STRING) {
				struct svalue stmp;

				if (some_numeric)
				    lambda_error(
				      "mixed case label lists not supported\n"
				    );
				if (--current.values_left < 0)
				    realloc_values();
				no_string = 0;
				stmp.type = T_STRING;
				stmp.x.string_type = STRING_SHARED;
				stmp.u.string =
				  make_shared_string(labels->u.string);
				if (!stmp.u.string)
				    lambda_error("Out of memory\n");
				*--current.valuep = stmp;
				l->key = (p_int)stmp.u.string;
			    } else if (labels->type == T_NUMBER) {
				if (l->key = labels->u.number) {
				    if (!no_string)
					lambda_error(
"mixed case label lists not supported\n"
					);
				    some_numeric = 1;
				} else {
				    zero = l;
				}
			    } else if (labels->type == T_CLOSURE &&
				  labels->x.closure_type ==
				  F_CSTRING0 -F_OFFSET+CLOSURE_OPERATOR)
			    {
				if (default_addr)
				    lambda_error("duplicate default\n");
				default_addr = l->addr;
			  reuse_list_entry:
				case_state.list0 = case_state.list1;
				case_state.list1 = l->next;
				case_state.next_free++;
				continue;
			    } else {
				lambda_error("bad type of case label\n");
			    }
			}
			argp++;
			opt_used = compile_value(
			  argp,
			  argp[1].x.closure_type ==
			  F_POP_VALUE - F_OFFSET+CLOSURE_OPERATOR ?
			    REF_REJECTED | VOID_ACCEPTED :
			    REF_REJECTED
			);
			if ((++argp)->type != T_CLOSURE ||
			    ( argp->x.closure_type !=
				F_BREAK - F_OFFSET+CLOSURE_OPERATOR &&
			      (!i || argp->x.closure_type !=
				F_POP_VALUE - F_OFFSET+CLOSURE_OPERATOR)) )
			{
			    lambda_error("Bad delimiter in #'switch\n");
			}
			if ( !(opt_used & VOID_GIVEN) ) {
			    if (current.code_left < 1)
				realloc_code();
			    current.code_left--;
			    *current.codep++ = argp->x.closure_type;
			}
		    }
		    if (!default_addr) {
			default_addr =
			  current.code_max - current.code_left - switch_pc;
			if (current.code_left < 2)
			    realloc_code();
			current.code_left -= 2;
			*current.codep++ = F_CONST0 - F_OFFSET;
			*current.codep++ = F_BREAK -  F_OFFSET;
		    }
		    store_case_labels(
		      current.code_max - current.code_left - switch_pc,
		      default_addr,
		      some_numeric|no_string, zero,
		      lambda_get_space, lambda_move_switch_instructions,
		      lambda_cerror, lambda_cerrorl
		    );
		    case_state.free_block = save_free_block;
		    case_state.next_free  = save_next_free;
		    case_state.list0 = save_list0;
		    case_state.list1 = save_list1;
		    current.break_stack--;
		    break;
		  }
		}
	    } else {
		/* efun */
		mp_int i;
		char *p;
		int f;
		int num_arg, min, max, def;
	
		num_arg = VEC_SIZE(block) - 1;
		for (i = num_arg; --i >= 0; ) {
		    compile_value(++argp, 0);
		}
		argp = block->item;
		if (current.code_left < 5)
		    realloc_code();
		f = type - CLOSURE_EFUN;
		min = instrs[f].min_arg;
		max = instrs[f].max_arg;
		p = current.codep;
		if (num_arg < min) {
		    extern int proxy_efun PROT((int, int));
	
		    int g;
	
		    if (num_arg == min-1 && (def = instrs[f].Default)) {
			*p++ = def - F_OFFSET;
			current.code_left--;
			max--;
			min--;
		    } else if ( (g = proxy_efun(f, num_arg)) < 0 ||
							    (f = g,MY_FALSE) )
			lambda_error("Too few arguments to %s\n", instrs[f].name);
		} else if (num_arg > max && max != -1) {
		    if (f == F_INDEX - F_OFFSET && num_arg == 3) {
			f = F_MAP_INDEX - F_OFFSET;
		    } else {
			lambda_error(
			  "Too many arguments to %s\n",
			  instrs[f].name
			);
		    }
		}
		if (f > 0xff) {
		    *p++ = f >> F_ESCAPE_BITS;
		    current.code_left--;
		}
		*p++ = f;
		current.code_left--;
		if (min != max) {
		    *p++ = num_arg;
		    if (num_arg > 0xff)
			lambda_error("Too many arguments to efun closure\n");
		    current.code_left--;
		}
		if ( instrs[f].ret_type == TYPE_VOID ) {
		    if (opt_flags & (ZERO_ACCEPTED|VOID_ACCEPTED)) {
			opt_flags = VOID_GIVEN;
		    } else {
			*p++ = F_CONST0-F_OFFSET;
			current.code_left--;
		    }
		}
		current.codep = (unsigned char*)p;
		break;
	    }
	} else switch (type) {
	  default:
	  {
	    /* simul_efun */
	    extern struct function *simul_efunp;
	    int simul_efun;
	    int num_arg;
	    int i;

	    simul_efun = type - CLOSURE_SIMUL_EFUN;
	    if (simul_efun > 0xff) {
		static struct svalue string_sv = { T_STRING };

		string_sv.x.string_type = STRING_SHARED;
		string_sv.u.string = query_simul_efun_file_name();
		compile_value(&string_sv, 0);
		string_sv.u.string =
		  simul_efunp[simul_efun].name;
		compile_value(&string_sv, 0);
	    }
	    num_arg = VEC_SIZE(block) - 1;
	    for (i = num_arg; --i >= 0; ) {
		compile_value(++argp, 0);
	    }
	    if (current.code_left < 3)
		realloc_code();
	    if (simul_efun > 0xff) {
		current.code_left -= 2;
		*current.codep++ = F_CALL_OTHER - F_OFFSET;
		*current.codep++ = num_arg + 2;
		if (num_arg + 2 > 0xff)
		    lambda_error("Argument number overflow\n");
	    } else {
		struct function *funp;

		funp = &simul_efunp[simul_efun];
		if (num_arg > funp->num_arg)
		    lambda_error(
		      "Too many arguments to simul_efun %s\n", funp->name
		    );
		if (funp->num_arg != 0xff) {

		    i = funp->num_arg - num_arg;
		    if (i > 1 && current.code_left < i + 2)
			realloc_code();
		    current.code_left -= i;
		    while ( --i >= 0 ) {
			*current.codep++ = F_CONST0 - F_OFFSET;
		    }
		}
		*current.codep++ = F_SIMUL_EFUN - F_OFFSET;
		*current.codep++ = simul_efun;
		if (funp->num_arg == 0xff) {
		    *current.codep++ = num_arg;
		    current.code_left -= 3;
		} else
		    current.code_left -= 2;
		break;
	    }
	  }
	  case CLOSURE_UNBOUND_LAMBDA:
	  case CLOSURE_BOUND_LAMBDA:
	  case CLOSURE_LAMBDA:
	  case CLOSURE_PRELIMINARY:
	    lambda_error("Unimplemented closure type for lambda()\n");
	  case CLOSURE_ALIEN_LFUN:
	  {
	    mp_int i;
	    struct lambda *l;
	    mp_int block_size;

	    block_size = VEC_SIZE(block);
	    l = argp->u.lambda;
	    insert_value_push(argp);
	    for (i = block_size; --i; ) {
		compile_value(++argp, 0);
	    }
	    if (current.code_left < 2)
	    realloc_code();
	    current.code_left -= 2;
	    *current.codep++ = F_FUNCALL - F_OFFSET;
	    *current.codep++ = block_size;
	    break;
	  }
	  case CLOSURE_LFUN:
	  {
	    mp_int i;
	    struct lambda *l;
	    mp_int block_size;

	    block_size = VEC_SIZE(block);
	    l = argp->u.lambda;
	    if (l->ob != current.lambda_origin) {
		insert_value_push(argp);
		for (i = block_size; --i; ) {
		    compile_value(++argp, 0);
		}
		if (current.code_left < 2)
		    realloc_code();
		current.code_left -= 2;
		*current.codep++ = F_FUNCALL - F_OFFSET;
		*current.codep++ = block_size;
	    } else {
		for (i = block_size; --i; ) {
		    compile_value(++argp, 0);
		}
		if (current.code_left < 4)
		    realloc_code();
		current.code_left -= 4;
		*current.codep++ = F_CALL_FUNCTION_BY_ADDRESS - F_OFFSET;
		*current.codep++ = ((char *)&l->function.index)[0];
		*current.codep++ = ((char *)&l->function.index)[1];
		*current.codep++ = block_size - 1;
		if (block_size > 0x100)
		    lambda_error("Too many arguments to lfun closure\n");
	    }
	    break;
	  }
	  case CLOSURE_IDENTIFIER:
	  {
	    struct lambda *l;

	    l = argp->u.lambda;
	    if (VEC_SIZE(block) != 1)
		lambda_error("Argument to variable\n");
	    if (l->ob != current.lambda_origin) {
		insert_value_push(argp);
		if (current.code_left < 2)
		    realloc_code();
		current.code_left -= 2;
		*current.codep++ = F_FUNCALL - F_OFFSET;
		*current.codep++ = 1;
	    } else {
		if (current.code_left < 2)
		    realloc_code();
		current.code_left -= 2;
		if ((short)l->function.index < 0)
		    lambda_error("Variable not inherited\n");
		*current.codep++ = F_IDENTIFIER - F_OFFSET;
		*current.codep++ = l->function.index;
	    }
	    break;
	  }
	} /* end of switch on closure_type */
	break;
      } /* end of case T_POINTER (block compiling code) */
      case T_QUOTED_ARRAY:
	insert_value_push(value);
	if (!--current.valuep->x.quotes)
	    current.valuep->type = T_POINTER;
	break;
      case T_SYMBOL:
	if (value->x.quotes > 1) {
	    insert_value_push(value);
	    --current.valuep->x.quotes;
	} else {
	    struct symbol *sym;

	    sym = make_symbol(value->u.string);
	    if (sym->index < 0)
		lambda_error("Symbol '%s' not bound\n", sym->name);
	    if (current.code_left < 2)
		realloc_code();
	    *current.codep++ = F_LOCAL - F_OFFSET;
	    *current.codep++ = sym->index;
	    current.code_left -= 2;
	}
	break;
      case T_NUMBER:
      {
	mp_int i;

	if ( (i = value->u.number) >= 0) {
	    if (i < 0x100) {
		if (current.code_left < 2)
		    realloc_code();
		if (!i) {
		    if (opt_flags & (VOID_ACCEPTED|ZERO_ACCEPTED)) {
			opt_flags = VOID_GIVEN;
			break;
		    }
		    *current.codep++ = F_CONST0 - F_OFFSET;
		    current.code_left--;
		    break;
		} else if (i == 1) {
		    *current.codep++ = F_CONST1 - F_OFFSET;
		    current.code_left--;
		    break;
		}
		*current.codep++ = F_CLIT - F_OFFSET;
		*current.codep++ = i;
		current.code_left -= 2;
		break;
	    }
	} else if (i > -0x100) {
	    if (current.code_left < 2)
		realloc_code();
	    *current.codep++ = F_NCLIT - F_OFFSET;
	    *current.codep++ = -i;
	    current.code_left -= 2;
	    break;
	}
	/* else fall through */
      }
      default:
	insert_value_push(value);
	break;
    }
    current.levels_left++;
    return opt_flags;
}

int is_lvalue(argp, index_lvalue)
    struct svalue *argp;
    int index_lvalue;
{
    switch(argp->type) {
      case T_SYMBOL:
	return argp->x.quotes == 1;
      case T_POINTER:
	if (index_lvalue) {
	    struct vector *block;

	    block = argp->u.vec;
	    if (VEC_SIZE(block) != 3)
		break;
	    argp = block->item;
	    if (argp->type != T_CLOSURE)
	    {
		break;
	    }
	    switch (argp->x.closure_type) {
	      case F_INDEX -F_OFFSET+CLOSURE_EFUN:
	      case F_RINDEX-F_OFFSET+CLOSURE_EFUN:
	      case CLOSURE_IDENTIFIER:
		return 1;
	    }
	}
	break;
      case T_CLOSURE:
	if (argp->x.closure_type == CLOSURE_IDENTIFIER)
	    return 1;
	break;
    }
    return 0;
}

void compile_lvalue(argp, flags)
    struct svalue *argp;
    int flags;
{
    switch(argp->type) {
      case T_SYMBOL:
      {
	struct symbol *sym;

	if (argp->x.quotes > 1)
	    break;
	sym = make_symbol(argp->u.string);
	if (sym->index < 0)
	    sym->index = current.num_locals++;
	if (current.code_left < 3)
	    realloc_code();
	current.code_left -= 3;
	*current.codep++ = F_PUSH_LOCAL_VARIABLE_LVALUE - F_OFFSET;
	*current.codep++ = sym->index;
	return;
      }
      case T_POINTER:
      {
	struct vector *block;

	block = argp->u.vec;
	if (block != &null_vector && (argp = block->item)->type == T_CLOSURE)
	{
	    switch (argp->x.closure_type) {
	      case F_INDEX -F_OFFSET+CLOSURE_EFUN:
	      case F_RINDEX-F_OFFSET+CLOSURE_EFUN:
		if (VEC_SIZE(block) == 3) {
		    if (is_lvalue(argp+1, flags & USE_INDEX_LVALUE)) {
			compile_value(argp+2, 0);
			compile_lvalue(argp+1, flags & PROTECT_LVALUE);
			if (current.code_left < 2)
			    realloc_code();
			if (flags & PROTECT_LVALUE) {
			    current.code_left -= 2;
			    *current.codep++ = F_ESCAPE - F_OFFSET;
			    *current.codep++ =
			      argp->x.closure_type == F_RINDEX-F_OFFSET ?
				F_PROTECTED_RINDEX_LVALUE - F_OFFSET - 0x100 :
				F_PROTECTED_INDEX_LVALUE  - F_OFFSET - 0x100;
			} else {
			    current.code_left--;
			    *current.codep++ =
			      argp->x.closure_type == F_RINDEX-F_OFFSET ?
				F_RINDEX_LVALUE - F_OFFSET :
				F_INDEX_LVALUE - F_OFFSET;
			}
			return;
		    }
		    compile_value(argp+1, 0);
		    compile_value(argp+2, 0);
		    if (current.code_left < 3)
			realloc_code();
		    if (flags & PROTECT_LVALUE) {
			current.code_left -= 3;
			*current.codep++ = F_ESCAPE - F_OFFSET;
			*current.codep++ =
			  argp->x.closure_type == F_RINDEX-F_OFFSET ?
			    F_PUSH_PROTECTED_RINDEXED_LVALUE - F_OFFSET - 0x100 :
			    F_PUSH_PROTECTED_INDEXED_LVALUE  - F_OFFSET - 0x100;
		    } else {
			current.code_left -= 2;
			*current.codep++ =
			  argp->x.closure_type == F_RINDEX-F_OFFSET ?
			    F_PUSH_RINDEXED_LVALUE - F_OFFSET :
			    F_PUSH_INDEXED_LVALUE - F_OFFSET;
		    }
		    return;
		}
		if (VEC_SIZE(block) == 4 &&
		    argp->x.closure_type == F_INDEX -F_OFFSET+CLOSURE_EFUN)
		{
		    compile_value(argp+1, 0);
		    compile_value(argp+2, 0);
		    compile_value(argp+3, 0);
		    if (current.code_left < 3)
			realloc_code();
		    if (flags & PROTECT_LVALUE) {
			current.code_left -= 3;
			*current.codep++ = F_ESCAPE - F_OFFSET;
			*current.codep++ =
			  F_PUSH_PROTECTED_INDEXED_MAP_LVALUE -F_OFFSET -0x100;
		    } else {
			current.code_left -= 2;
			*current.codep++ =
			    F_PUSH_INDEXED_MAP_LVALUE - F_OFFSET;
		    }
		    return;
		}
		break;
	      case F_RANGE -F_OFFSET+CLOSURE_EFUN:
		if (VEC_SIZE(block) != 4)
		    break;
		compile_value(argp += 2, 0);
		compile_value(++argp, 0);
		compile_lvalue(argp - 2, flags & PROTECT_LVALUE);
		if (current.code_left < 2)
		    realloc_code();
		if (flags & PROTECT_LVALUE) {
		    current.code_left -= 2;
		    *current.codep++ = F_ESCAPE - F_OFFSET;
		    *current.codep++ =
		      F_PROTECTED_RANGE_LVALUE - F_OFFSET - 0x100;
		} else {
		    current.code_left--;
		    *current.codep++ = F_RANGE_LVALUE - F_OFFSET;
		}
		return;
	      case F_NR_RANGE -F_OFFSET+CLOSURE_EFUN:
	      case F_RN_RANGE -F_OFFSET+CLOSURE_EFUN:
	      case F_RR_RANGE -F_OFFSET+CLOSURE_EFUN:
		if (VEC_SIZE(block) != 4)
		    break;
		compile_value(argp += 2, 0);
		compile_value(++argp, 0);
		compile_lvalue(argp - 2, flags & PROTECT_LVALUE);
		if (current.code_left < 2)
		    realloc_code();
		current.code_left -= 2;
		*current.codep++ = F_ESCAPE - F_OFFSET;
		if (flags & PROTECT_LVALUE) {
		    *current.codep++ = argp->x.closure_type - CLOSURE_EFUN -
				F_NR_RANGE +  F_PROTECTED_NR_RANGE_LVALUE;
		} else {
		    *current.codep++ = argp->x.closure_type - CLOSURE_EFUN -
					F_NR_RANGE +  F_NR_RANGE_LVALUE;
		}
		return;
	      case F_MAP_INDEX -F_OFFSET+CLOSURE_EFUN:
		if (VEC_SIZE(block) != 4)
		    break;
		compile_value(++argp, 0);
		compile_value(++argp, 0);
		compile_value(++argp, 0);
		if (current.code_left < 3)
		    realloc_code();
		if (flags & PROTECT_LVALUE) {
		    current.code_left -= 3;
		    *current.codep++ = F_ESCAPE - F_OFFSET;
		    *current.codep++ =
		      F_PUSH_PROTECTED_INDEXED_MAP_LVALUE - F_OFFSET - 0x100;
		} else {
		    current.code_left -= 2;
		    *current.codep++ = F_PUSH_INDEXED_MAP_LVALUE - F_OFFSET;
		}
		return;
	      case CLOSURE_IDENTIFIER:
	      {
		struct lambda *l;

		if (VEC_SIZE(block) != 1)
		    break;
		l = argp->u.lambda;
		if (l->ob != current.lambda_origin)
		    break;
		if (current.code_left < 3)
		    realloc_code();
		current.code_left -= 3;
		if ((short)l->function.index < 0)
		    lambda_error("Variable not inherited\n");
		*current.codep++ = F_PUSH_IDENTIFIER_LVALUE - F_OFFSET;
		*current.codep++ = l->function.index;
		return;
	      }
	    }
	}
	break;
      }
      case T_CLOSURE:
      {
	switch (argp->x.closure_type) {
	  case CLOSURE_IDENTIFIER:
	  {
	    struct lambda *l;

	    l = argp->u.lambda;
	    if (l->ob != current.lambda_origin)
		break;
	    if (current.code_left < 3)
		realloc_code();
	    current.code_left -= 3;
	    if ((short)l->function.index < 0)
		lambda_error("Variable not inherited\n");
	    *current.codep++ = F_PUSH_IDENTIFIER_LVALUE - F_OFFSET;
	    *current.codep++ = l->function.index;
	    return;
	  }
	}
	break;
      }
    }
    lambda_error("Illegal lvalue\n");
}

struct lambda *lambda(args, block, origin)
    struct vector *args;
    struct svalue *block;
    struct object *origin;
{
    mp_int i, j;
    struct svalue *argp;
    mp_int num_values, values_size, code_size;
    char *l0;
    struct lambda *l;
    int void_given;

    current.symbols_left = current.symbol_max =
	sizeof current.symbols[0] * SYMTAB_START_SIZE;
    current.symbol_mask = current.symbol_max- sizeof(struct symbol *);
    current.last = 0;
    current.code = 0;
    current.values = 0;
    current.symbols = (struct symbol **)xalloc(current.symbol_max);
    i = SYMTAB_START_SIZE - 1;
    do {
	current.symbols[i] = 0;
    } while (--i >= 0);
    switch_initialized = 0;
    argp = args->item;
    j = VEC_SIZE(args);
    for (i = 0; i < j; i++, argp++) {
	struct symbol *sym;

	if (argp->type != T_SYMBOL) {
	    lambda_error("Illegal argument type to lambda()\n");
	}
	sym = make_symbol(argp->u.string);
	if (sym->index >= 0)
	    lambda_error("Double symbol name in lambda arguments\n");
	sym->index = i;
    }
    current.num_locals = i;
    current.break_stack = current.max_break_stack = 0;
    current.code_max = CODE_BUFFER_START_SIZE;
    current.code_left = CODE_BUFFER_START_SIZE-3;
    current.levels_left = MAX_LAMBDA_LEVELS;
    if ( !(current.code = current.codep = xalloc(current.code_max)) )
	lambda_error("Out of memory\n");
    *current.codep++ = 0;		   /* dummy for num values */
    *current.codep++ = current.num_locals; /* num arguments */
    *current.codep++ = 0;		   /* dummy for num variables */
    current.value_max = current.values_left = VALUE_START_MAX;
    if ( !(current.values =
	xalloc(current.value_max * sizeof current.values[0])) )
    {
	lambda_error("Out of memory\n");
    }
    current.valuep = current.values + current.value_max;
    current.lambda_origin = origin;

    void_given = compile_value(block, ZERO_ACCEPTED|REF_REJECTED);

    if (current.code_left < 1)
	realloc_code();
    current.code_left -= 1;
    *current.codep++ =
      void_given & VOID_GIVEN ? F_RETURN0 - F_OFFSET : F_RETURN - F_OFFSET;
    num_values = current.value_max - current.values_left;
    values_size = num_values * sizeof (struct svalue);
    code_size = current.code_max - current.code_left;
    l0 = xalloc(values_size + sizeof *l - sizeof l->function + code_size);
    memcpy(l0, (char *)current.valuep, values_size);
    l0 += values_size;
    l = (struct lambda *)l0;
    l->ref = 1;
    memcpy(l->function.code, current.code, code_size);
    /* fix number of constant values */
    if (num_values > 0xff) {
	((struct svalue *)l->function.code)[-0xff].u.number = num_values;
	l->function.code[0] = 0xff;
    } else {
	l->function.code[0] = num_values;
    }
    /* fix number of variables */
    l->function.code[2] = current.num_locals + current.max_break_stack;
    free_symbols();
    xfree(current.code);
    xfree(current.values);
    if (origin && ( !(origin->prog->flags & P_REPLACE_ACTIVE) ||
	 !lambda_ref_replace_program(l, CLOSURE_LAMBDA, code_size, args, block)
    ) )
    {
	origin->flags |= O_LAMBDA_REFERENCED;
    }
    return l;
}

static void insert_value_push(value)
    struct svalue *value;
{
    mp_int offset;

    if (current.code_left < 3)
	realloc_code();
    offset = current.value_max - current.values_left;
    if (offset < 0xff) {
	current.code_left -= 2;
	*current.codep++ = F_LAMBDA_CCONSTANT - F_OFFSET;
	*current.codep++ = offset;
    } else {
	if (offset == 0xff) {
	    current.values_left--;
	    offset++;
	    (--current.valuep)->type = T_INVALID;
	}
	current.code_left -= 3;
	*current.codep++ = F_LAMBDA_CONSTANT - F_OFFSET;
	*current.codep++ = offset >> 8;
	*current.codep++ = offset;
    }
    if (--current.values_left < 0)
	realloc_values();
    assign_svalue_no_free(--current.valuep, value);
}

void free_closure(svp)
    struct svalue *svp;
{
    struct lambda *l;
    int type;

    if (!CLOSURE_MALLOCED(type = svp->x.closure_type)) {
	free_object(svp->u.ob, "free_closure");
	return;
    }
    l = svp->u.lambda;
    if (--l->ref) return;
    if (CLOSURE_HAS_CODE(type)) {
	mp_int num_values;

	if (type != CLOSURE_UNBOUND_LAMBDA)
	    free_object(l->ob, "free_closure");
	svp = (struct svalue *)l;
	if ( (num_values = EXTRACT_UCHAR(l->function.code)) == 0xff)
	    num_values = svp[-0xff].u.number;
	while (--num_values >= 0)
	    free_svalue(--svp);
	xfree((char *)svp);
	return;
    }
    free_object(l->ob, "free_closure");
    if (type == CLOSURE_BOUND_LAMBDA) {
	mp_int num_values;
	struct lambda *l2;

	l2 = l->function.lambda;
	xfree((char *)l);
	if (--l2->ref) return;
	svp = (struct svalue *)l2;
	if ( (num_values = EXTRACT_UCHAR(l2->function.code)) == 0xff)
	    num_values = svp[-0xff].u.number;
	while (--num_values >= 0)
	    free_svalue(--svp);
	xfree((char *)svp);
	return;
    }
    if (type == CLOSURE_ALIEN_LFUN) {
	free_object(l->function.alien.ob, "free_closure");
    }
    /* CLOSURE_LFUN || CLOSURE_IDENTIFIER || CLOSURE_PRELIMINARY */
    xfree((char *)l);
}

int symbol_operator(symbol, endp)
char *symbol, **endp;
{
    char c;
    int ret;

    switch(*symbol) {
      case '+':
	c = symbol[1];
	if (c == '=') {
	    symbol++;
	    ret = F_ADD_EQ-F_OFFSET;
	    break;
	} else if (c == '+') {
	    symbol++;
	    ret = F_POST_INC-F_OFFSET;
	    break;
	}
	ret = F_ADD-F_OFFSET;
	break;
      case '-':
	c = symbol[1];
	if (c == '=') {
	    symbol++;
	    ret = F_SUB_EQ-F_OFFSET;
	    break;
	} else if (c == '-') {
	    symbol++;
	    ret = F_POST_DEC-F_OFFSET;
	    break;
	}
	ret = F_SUBTRACT-F_OFFSET;
	break;
      case '*':
	if (symbol[1] == '=') {
	    symbol++;
	    ret = F_MULT_EQ-F_OFFSET;
	    break;
	}
	ret = F_MULTIPLY-F_OFFSET;
	break;
      case '/':
	if (symbol[1] == '=') {
	    symbol++;
	    ret = F_DIV_EQ-F_OFFSET;
	    break;
	}
	ret = F_DIVIDE-F_OFFSET;
	break;
      case '%':
	if (symbol[1] == '=') {
	    symbol++;
	    ret = F_MOD_EQ-F_OFFSET;
	    break;
	}
	ret = F_MOD-F_OFFSET;
	break;
      case ',':
	ret = F_POP_VALUE-F_OFFSET;
	break;
      case '^':
	if (symbol[1] == '=') {
	    symbol++;
	    ret = F_XOR_EQ-F_OFFSET;
	    break;
	}
	ret = F_XOR-F_OFFSET;
	break;
      case '|':
	c = *++symbol;
	if (c == '|') {
	    ret = F_LOR-F_OFFSET;
	    break;
	} else if (c == '=') {
	    ret = F_OR_EQ-F_OFFSET;
	    break;
	}
	symbol--;
	ret = F_OR-F_OFFSET;
	break;
      case '&':
	c = *++symbol;
	if (c == '&') {
	    ret = F_LAND-F_OFFSET;
	    break;
	} else if (c == '=') {
	    ret = F_AND_EQ-F_OFFSET;
	    break;
	}
	symbol--;
	ret = F_AND-F_OFFSET;
	break;
      case '~':
	ret = F_COMPL-F_OFFSET;
	break;
      case '<':
	c = *++symbol;
	if (c == '=') {
	    ret = F_LE-F_OFFSET;
	    break;
	} else if (c == '<') {
	    if (symbol[1] == '=') {
		symbol++;
		ret = F_LSH_EQ-F_OFFSET;
		break;
	    }
	    ret = F_LSH-F_OFFSET;
	    break;
	}
	symbol--;
	ret = F_LT-F_OFFSET;
	break;
      case '>':
	c = *++symbol;
	if (c == '=') {
	    ret = F_GE-F_OFFSET;
	    break;
	} else if (c == '>') {
	    if (symbol[1] == '=') {
		symbol++;
		ret = F_RSH_EQ-F_OFFSET;
		break;
	    }
	    ret = F_RSH-F_OFFSET;
	    break;
	}
	symbol--;
	ret = F_GT-F_OFFSET;
	break;
      case '=':
	if (symbol[1] == '=') {
	    symbol++;
	    ret = F_EQ-F_OFFSET;
	    break;
	}
	ret = F_ASSIGN-F_OFFSET;
	break;
      case '!':
	if (symbol[1] == '=') {
	    symbol++;
	    ret = F_NE-F_OFFSET;
	    break;
	}
	ret = F_NOT-F_OFFSET;
	break;
      case '?':
	if (symbol[1] == '!') {
	    symbol++;
	    ret = F_BRANCH_WHEN_NON_ZERO-F_OFFSET;
	    break;
	}
	ret = F_BRANCH_WHEN_ZERO-F_OFFSET;
	break;
      case '[':
	c = *++symbol;
	if (c == '<') {
	    if (symbol[1] == '.' && symbol[2] == '.') {
		c = *(symbol+=3);
		if (c == ']') {
		    ret = F_RN_RANGE-F_OFFSET;
		    break;
		} else if (c == '<' && symbol[1] == ']') {
		    symbol++;
		    ret = F_RR_RANGE-F_OFFSET;
		    break;
		}
		symbol--;
		/* We will have to compile F_NEGATE , F_EXTRACT2 */
		ret = F_EXTRACT_LVALUE-F_OFFSET;
		break;
	    }
	    ret = F_RINDEX-F_OFFSET;
	    break;
	} else if (c == '.' && symbol[1] == '.') {
	    c = *(symbol+=2);
	    if (c == ']') {
		ret = F_RANGE-F_OFFSET;
		break;
	    } else if (c == '<' && symbol[1] == ']') {
		symbol++;
		ret = F_NR_RANGE-F_OFFSET;
		break;
	    }
	    symbol--;
	    ret = F_EXTRACT2-F_OFFSET;
	    break;
	} else if (c == ',' && symbol[1] == ']') {
	    symbol++;
	    ret = F_MAP_INDEX - F_OFFSET;
	    break;
	}
	symbol--;
	ret = F_INDEX-F_OFFSET;
	break;
      case '(':
	c = *++symbol;
	if (c == '{') {
	    ret = F_AGGREGATE-F_OFFSET;
	    break;
	} else if (c == '[') {
	    ret = F_M_CAGGREGATE-F_OFFSET;
	    break;
	}
	symbol--;
      /* fall through */
      default:
	ret = -1;
	symbol--;
    }
    *endp = symbol+1;
    return ret;
}

void symbol_efun(sp)
    struct svalue *sp;
{
    extern struct svalue *inter_sp;

    int efun_override = 0;
    char *str;

    str = sp->u.string;
    if (isalunum(*str)) {
	extern struct function *simul_efunp;

	struct ident *p;

	if ( !strncmp(str, "efun::", 6) ) {
	    str += 6;
	    efun_override = 1;
	}
	if ( !(p = make_shared_identifier(str, I_TYPE_GLOBAL)) ) {
	    inter_sp = sp;
	    error("Out of memory\n");
	}
	while (p->type > I_TYPE_GLOBAL) {
	    if (p->type == I_TYPE_RESWORD) {
		int code;
	
		switch(code = p->u.code) {
		  default:
		    if (p = p->inferior)
			continue;
		    goto undefined_function;
		  case F_IF:
		    code = F_BRANCH_WHEN_ZERO-F_OFFSET;
		    break;
		  case F_DO:
		    code = F_BBRANCH_WHEN_NON_ZERO-F_OFFSET;
		    break;
		  case F_WHILE:
		    /* the politically correct code   /
		    /  was already taken, see above. */
		    code = F_BBRANCH_WHEN_ZERO-F_OFFSET;
		    break;
		  case F_CONTINUE:
		    code = F_BRANCH-F_OFFSET;
		    break;
		  case F_DEFAULT:
		    code = F_CSTRING0-F_OFFSET;
		    break;
		  case F_SWITCH:
		  case F_BREAK:
		  case F_RETURN:
		  case F_SSCANF:
		  case F_CATCH:
		    code -= F_OFFSET;
		    break;
		}
		free_string_svalue(sp);
		sp->type = T_CLOSURE;
		sp->x.closure_type = code + CLOSURE_OPERATOR;
		add_ref(sp->u.ob = current_object, "symbol_efun");
	        return;
	    }
	    if ( !(p = p->inferior) )
		break;
	}
	if (!p || p->type < I_TYPE_GLOBAL ||
	    ( efun_override || p->u.global.sim_efun < 0 ) &&
	      p->u.global.efun < 0 )
	{
	    if (p && p->type == I_TYPE_UNKNOWN)
		free_shared_identifier(p);
undefined_function:
#if 0
	    inter_sp = sp;
	    error("Undefined function: %s\n", str);
#else
	    free_string_svalue(sp);
	    sp->type = T_NUMBER;
	    sp->u.number = 0;
	    return;
#endif
	}
	if (efun_override && p->u.global.sim_efun >= 0 &&
	      simul_efunp[p->u.global.sim_efun].flags & TYPE_MOD_NO_MASK)
	{
	    struct svalue *res;
	
	    inter_sp = sp;
	    push_constant_string("nomask simul_efun");
	    push_object(current_object);
	    push_shared_string(p->name);
	    res = apply_master_ob(STR_PRIVILEGE, 3);
	    if (!res || res->type != T_NUMBER || res->u.number < 0)
	    {
		error(
		  "Privilege violation: nomask simul_efun %s\n",
		  p->name
		);
	    } else if (!res->u.number) {
		efun_override = 0;
	    }
	}
	free_string_svalue(sp);
	sp->type = T_CLOSURE;
	if (!efun_override && p->u.global.sim_efun >= 0) {
	    sp->x.closure_type = p->u.global.sim_efun + CLOSURE_SIMUL_EFUN;
	    add_ref(sp->u.ob = current_object, "symbol_efun");
	    return;
	}
	/* p->u.global.efun >= 0 */
	sp->x.closure_type = p->u.global.efun + CLOSURE_EFUN;
	if (sp->x.closure_type > LAST_INSTRUCTION_CODE + CLOSURE_EFUN)
	    sp->x.closure_type = CLOSURE_EFUN +
	      efun_aliases[
		sp->x.closure_type - CLOSURE_EFUN - LAST_INSTRUCTION_CODE - 1];
	add_ref(sp->u.ob = current_object, "symbol_efun");
    } else {
	int i;
	char *end;

	i = symbol_operator(str, &end);
	/* If there was a valid operator with trailing junk, *end, but i >= 0.
	 * On the other hand, if we passed the empty string, i < 0, but !*end.
	 * Thus, we have to test for (*end || i < 0) .
	 */
#if 0
	if (*end || i < 0) {
	    inter_sp = sp;
	    error("Unknown operator\n");
	}
	free_string_svalue(sp);
#else
	free_string_svalue(sp);
	if (*end || i < 0) {
	    sp->type = T_NUMBER;
	    sp->u.number = 0;
	    return;
	}
#endif
	sp->type = T_CLOSURE;
	if (instrs[i].Default == -1) {
	    sp->x.closure_type = i + CLOSURE_OPERATOR;
	} else {
	    sp->x.closure_type = i + CLOSURE_EFUN;
	}
	add_ref(sp->u.ob = current_object, "symbol_efun");
    }
}

struct svalue *f_unbound_lambda(sp)
    struct svalue *sp;
{
    extern struct svalue *inter_sp;

    struct lambda *l;
    struct vector *args;

    if (sp[-1].type != T_POINTER) {
	if (sp[-1].type != T_NUMBER || sp[-1].u.number)
	    bad_xefun_arg(1, sp);
	(args = &null_vector)->ref++;
    } else {
	args = sp[-1].u.vec;
    }
    inter_sp = sp;
    l = lambda(args, sp, 0);
    l->ob = 0;
    free_svalue(sp--);
    free_vector(args);
    sp->type = T_CLOSURE;
    sp->x.closure_type = CLOSURE_UNBOUND_LAMBDA;
    sp->u.lambda = l;
    return sp;
}

struct svalue *f_symbol_variable(sp)
    struct svalue *sp;
{
    extern struct svalue *current_variables;

    char *str;
    struct object *ob;
    int n;
    struct lambda *l;

    str = sp->u.string;
    ob = current_object;
    if (current_variables < ob->variables ||
	current_variables >= ob->variables + ob->prog->num_variables)
    {
	/* efun closures are called without setting current_prog nor
	 * current_variables. This keeps the program scope for variables
	 * for calls inside this_object(), but would give trouble with
	 * calling from other ones if it were not for this test.
	 */
	current_prog = ob->prog;
	current_variables = ob->variables;
    }
    switch(sp->type) {
      default:
	bad_xefun_arg(1, sp);
      case T_NUMBER:
	n = (p_int)str;
	if (n < 0 || n >= current_prog->num_variables) {
	    sp->u.number = 0;
	    return sp;
	}
	if (current_prog->variable_names[n].flags & NAME_HIDDEN) {
	    if (_privilege_violation("symbol_variable", sp, sp) <= 0) {
		sp->u.number = 0;
		return sp;
	    }
	}
	break;
      case T_STRING:
	if (sp->x.string_type != STRING_SHARED)
	    str = make_shared_string(str);
	    if (sp->x.string_type == STRING_MALLOC)
		xfree(sp->u.string);
	/* fall through */
      case T_SYMBOL:
      {
	struct variable *var;
	struct program *prog;
	int num_var;

	prog = current_prog;
	var = prog->variable_names;
	num_var = prog->num_variables;
	for (n = num_var; --n >= 0; var++) {
	    if (var->name == str && !(var->flags & NAME_HIDDEN))
		break;
	}
	free_string(str);
	if (n < 0) {
	    sp->type = T_NUMBER;
	    sp->u.number = 0;
	    return sp;
	}
	n = num_var - n - 1;
      }
    }
    l = (struct lambda *)
        xalloc(sizeof *l - sizeof l->function + sizeof l->function.index);
    if (!l) {
	extern struct svalue *inter_sp;

	inter_sp = sp - 1;
	error("Out of memory\n");
    }
    add_ref(l->ob = current_object, "symbol_variable");
    l->ref = 1;
    l->function.index = n + (current_variables - current_object->variables);
    sp->type = T_CLOSURE;
    sp->x.closure_type = CLOSURE_IDENTIFIER;
    sp->u.lambda = l;
    return sp;
}

/* allocate case_list_entrys in contigous blocks to increase locality of
 * reference
 */
struct case_list_entry *new_case_entry() {
    struct case_list_entry *ret;
    ret = --case_state.next_free;
    if (ret == case_state.free_block) {
	struct case_list_entry *next;

	if ( !(next = case_state.free_block->next) ) {
	    next = (struct case_list_entry*)
	      xalloc(sizeof(struct case_list_entry[CASE_BLOCKING_FACTOR]));
	    next->next = 0;
	    case_state.free_block->next = next;
	}
	case_state.free_block = next;
	case_state.next_free = ret = next + CASE_BLOCKING_FACTOR - 1;
    }
    case_state.next_free->next = case_state.list1;
    ret->next = case_state.list1;
    case_state.list1 = case_state.list0;
    case_state.list0 = ret;
    return ret;
}

void store_case_labels(
    total_length, default_addr, numeric, zero,
    get_space, move_instructions, cerror, cerrorl
)
    struct case_list_entry *zero;
    p_int total_length;
    p_int default_addr;
    int numeric;
    char *(*get_space) PROT((p_int));
    void (*move_instructions) PROT((int, p_int));
    void (*cerror) PROT((char *));
    void (*cerrorl) PROT((char *, char*, int, int));
{
    struct case_list_entry *list0, *list1;
    int type;
    mp_int runlength, key_num;
    int len, i,o;
    p_int maxspan;
    mp_int current_key,last_key;
    mp_int current_addr,last_addr;
    char tmp_short[2];
    unsigned char *p;
    mp_int tablen;
    int i0;

    list0 = case_state.list0;
    list1 = case_state.list1;
    if (numeric) {
	type = 0;
    } else {
	type = 0x20;
	if (zero) {
	    zero->key = (p_int)ZERO_AS_STR_CASE_LABEL;
	}
    }
    /* length(list0) >= length(list1) */
    if (!list0) (*cerror)("switch without case not supported");
    for (runlength = 1; list1; runlength <<= 1) {
	struct case_list_entry *out_hook0, *out_hook1, **out0, **out1;
	mp_int count0, count1;

	out0 = &out_hook0;
	out1 = &out_hook1;
	while (list1) {
	    count0 = count1 = runlength;
	    while (1) {
		if (list1->key < list0->key)
		{
		    *out0 = list1;
		    out0 = &list1->next;
		    list1 = *out0;
		    if (!--count1 || !list1) {
			*out0 = list0;
			do {
			    out0 = &list0->next;
			    list0 = *out0;
			} while (--count0 && list0);
			break;
		    }
		} else {
		    *out0 = list0;
		    out0 = &list0->next;
		    list0 = *out0;
		    if (!--count0 || !list0) {
			*out0 = list1;
			do {
			    out0 = &list1->next;
			    list1 = *out0;
			} while (--count1 && list1);
			break;
		    }
		}
	    }
	    {
		struct case_list_entry **temp;

		temp = out0;
		out0 = out1;
		out1 = temp;
	    }
	}
	*out0 = list0;
	*out1 = 0;
	list0 = out_hook0;
	list1 = out_hook1;
    }
    /* list0 now contains all entries, sorted. Scan the list for ranges. */
    key_num = 0;
    if (numeric) {
	struct case_list_entry *table_start, *max_gain_end;
	p_int keys, max_gain, cutoff;

	for(last_addr=0xffffff, list1=list0; list1; list1 = list1->next) {
	    int curr_line,last_line;
	    struct case_list_entry *range_start;

	    key_num++;
	    current_key = list1->key ;
	    curr_line = list1->line ;
	    current_addr = list1->addr ;
	    if ( current_key == last_key && list1 != list0) {
		(*cerrorl)("Duplicate case%s", " in line %d and %d",
		    last_line, curr_line);
	    }
	    /* range ends are left in the list without checks. */
	    if (curr_line) {
		if (last_addr == 1) {
		    (*cerrorl)(
		      "Discontinued case label list range%s",
		      ", line %d by line %d",
		      last_line, curr_line);
		} else if (current_key == last_key + 1) {
		    if (current_addr == last_addr) {
			/* range continuation with single value */
			if (list1 != range_start->next) {
			    range_start->addr = 1;
			    range_start->next = list1;
			    /* lookup table building uses !end->line */
			    list1->line = 0;
			    key_num--;
			}
		    } else if (current_addr == 1 &&
			       list1->next->addr == last_addr)
		    {
			/* range continuation with range start */

			key_num -= 1 + (list1 != range_start->next);
			range_start->addr = 1;
			range_start->next = list1->next;
			/* list1->next was range end before, thus
			 * range_start->next->line == 0 .
			 */
			list1 = range_start;
		    } else {
			range_start = list1;
		    }
		} else {
		    range_start = list1;
		}
	    }
	    last_key = current_key;
	    last_line = curr_line;
	    last_addr = current_addr;
	}
	if (	!( total_length + key_num*(sizeof(p_int)+1)     & ~0xff) ) {
	    len = 1;
	    maxspan = MAXINT/len;
	} else if ( !( total_length + key_num*(sizeof(p_int)+2) + 1 & ~0xffff) )
	{
	    len = 2;
	    maxspan = MAXINT/len;
	} else if ( !( total_length + key_num*(sizeof(p_int)+3) + 2 & ~0xffffff) )
	{
	    len = 3;
	    maxspan = MAXINT/len;
	} else {
	    (*cerror)("offset overflow");
	    return;
	}
	if (len > 1) {
	    (*move_instructions)(len-1, total_length);
	    total_length += len-1;
	    default_addr += len-1;
	}
	cutoff = sizeof(p_int)*2 + len*2;
	list1 = list0;
	table_start = list1;
	for (max_gain = keys = 0; list1; list1 = list1->next) {
	    p_int span, gain;

	    struct case_list_entry *previous;
	    keys++;
	    if (list1->addr == 1) {
		previous = list1;
		continue;
	    }
	    list1->addr += len-1;
	    span = list1->key - table_start->key + 1;
	    if ((p_uint)span >= maxspan)
		gain = -1;
	    else
		gain = keys * sizeof(p_int) - (span - keys)* len;
	    if (max_gain - gain > cutoff && max_gain >= cutoff) {
		struct case_list_entry *tmp;
		p_int key, addr, size;
		unsigned char *p0;

		/* write table from table_start to  max_gain_end */
		span = max_gain_end->key - table_start->key + 1;
		size = span * len;
		p0 = (unsigned char*)(*get_space)(size);
		tmp = table_start;
		key = tmp->key;
		if (tmp->addr == 1) {
		    key_num--;
		    tmp = tmp->next;
		}
		do {
		    if (tmp->key < key) {
			key_num--;
			tmp = tmp->next;
			if (tmp->addr == 1) {
			    key_num--;
			    tmp = tmp->next;
			}
		    }
		    addr = default_addr;
		    if (key == tmp->key  || !tmp->line)
			addr = tmp->addr;
		    p0 += len;
		    p0[-1] = addr;
		    if (len >= 2) {
			p0[-2] = addr >> 8;
			if (len > 2) {
			    p0[-3] = addr >> 16;
			}
		    }
		} while (++key <= max_gain_end->key);
		key_num += 1;
		max_gain_end->addr = total_length;
		total_length += size;
		table_start->addr = 0;
		table_start->next = max_gain_end;

		gain = -1;
	    }
	    if (gain < 0) {
		if (list1->line) {
		    table_start = list1;
		    keys = 1;
		} else {
		    table_start = previous;
		    keys = 2;
		}
		table_start = list1->line ? list1 : previous;
		max_gain = 0;
	    } else if (gain > max_gain) {
		max_gain = gain;
		max_gain_end = list1;
	    }
	}
    } else {
	/* string case: neither ordinary nor lookup table ranges are viable.
	 * Thus, don't spend unnecesarily time with calculating them.
	 * Also, a more accurate calculation of len is possible.
	 */
	for (list1 = list0; list1; list1 = list1->next) {
	    int curr_line,last_line;

	    key_num++;
	    current_key = list1->key ;
	    curr_line = list1->line ;
	    if ( current_key == last_key && list1 != list0) {
		(*cerrorl)("Duplicate case%s", " in line %d and %d",
		    last_line, curr_line);
	    }
	    last_key = current_key;
	    last_line = curr_line;
	}
	if (        !( (total_length   | key_num*sizeof(p_int)) & ~0xff) ) {
	    len = 1;
	} else if ( !( (total_length+1 | key_num*sizeof(p_int)) & ~0xffff) ) {
	    len = 2;
	} else if ( !( (total_length+2 | key_num*sizeof(p_int)) & ~0xffffff) ) {
	    len = 3;
	} else {
	    (*cerror)("offset overflow");
	    return;
	}
	if (len > 1) {
	    (*move_instructions)(len-1, total_length);
	    total_length += len-1;
	    default_addr += len-1;
	    for (list1 = list0; list1; list1 = list1->next) {
		list1->addr += len-1;
	    }
	}
    }
    /* calculate starting index for iterative search at execution time */
    for(i=0,o=2;o <= key_num; )
	i++,o<<=1;
    /* and store it */
    type |= i | len << 6;
    tablen = key_num * sizeof(p_int);
    p = (unsigned char *)get_space(
	tablen + key_num * len + 2 + len + sizeof(p_int) - 4);
    p[-total_length] = tablen;
    p[-total_length+1] = type;
    i0 = p[-total_length+1+len];
    p[-total_length+2] = total_length;
    if (len >= 2) {
	*p++ = tablen >> 8;
	p[-total_length+2] = total_length >> 8;
	if (len > 2) {
	    *p++ = tablen >> 16;
	    p[-total_length+2] = total_length >> 16;
	}
    }
    *(short*)tmp_short = default_addr;
    *p++ = tmp_short[0];
    *p++ = tmp_short[1];
    *p++ = i0;
    p += sizeof(p_int) - 4;
    for (list1 = list0; list1; list1 = list1->next) {
	memcpy(p, &list1->key, sizeof(list1->key));
	p += sizeof(list1->key);
    }
    for (list1 = list0; list1; list1 = list1->next) {
	p += len;
	p[-1] = list1->addr;
	if (len >= 2) {
	    p[-2] = list1->addr >> 8;
	    if (len > 2) {
		p[-3] = list1->addr >> 16;
	    }
	}
    }
    if (len > 2)
	*(*get_space)(1) = default_addr >> 16;
}

void align_switch(pc)
    unsigned char *pc;
{
    int len;
    int32 tablen, offset, size;
    unsigned char a2, abuf[sizeof(p_int)-1], *startu, *starta;

    tablen = pc[0];
    a2 = pc[1];
    len = a2 >> 6;
    pc[0] |= len;
    pc[1] = offset = pc[2];
    if (len >=2) {
	offset += (pc[2] = pc[3]) << 8;
	if (len > 2) {
	    offset += (pc[3] = pc[4]) << 16;
	}
    }
    if (len >=2) {
	tablen += pc[offset] << 8;
	if (len > 2) {
	    tablen += pc[offset+1] << 16;
	}
    }
    memcpy(abuf, pc+offset+len-1, 2);
    pc[len+1] = pc[offset+len+1];
    pc[offset+len+1] = abuf[2] = a2;
    startu = pc+offset+len+2 + sizeof(p_int) - 4;
    starta = (unsigned char *)((p_int)startu & ~(sizeof(char *)-1));
    size = tablen + tablen / sizeof(char*) * len;
    move_memory(starta, startu, size);
    move_memory(starta+size, abuf + sizeof abuf - (startu-starta), startu-starta);
}
