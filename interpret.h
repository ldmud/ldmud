union u {
    char *string;
    int number;
    struct object *ob;
    struct vector *vec;
    struct svalue *lvalue;
};

/*
 * The value stack element.
 * If it is a string, then the way that the string has been allocated differ,
 * wich will affect how it should be freed.
 */
struct svalue {
    short type;
    short string_type;
    union u u;
};

#define T_INVALID	0x0
#define T_LVALUE	0x1
#define T_NUMBER	0x2
#define T_STRING	0x4
#define T_POINTER	0x8
#define T_OBJECT	0x10

#define STRING_MALLOC	0	/* Allocated by malloc() */
#define STRING_CONSTANT	1	/* Do not has to be freed at all */
#define STRING_SHARED	2	/* Allocated by the shared string library */

struct vector {
    short size;
    short ref;
#ifdef DEBUG
    int extra_ref;
#endif
    struct wiz_list *user;	/* Save who made the vector */
    struct svalue item[1];
};

#define ALLOC_VECTOR(nelem) \
    (struct vector *)xalloc(sizeof (struct vector) + \
			    sizeof(struct svalue) * (nelem - 1))

struct lnode_def;
void free_vector PROT((struct vector *)), free_all_values();

/*
 * Control stack element.
 * 'prog' is usually same as 'ob->prog' (current_object), except when
 * when the current function is defined by inheritance.
 * The pointer, csp, will point to the values that will be used at return.
 */
struct control_stack {
    struct object *ob;		/* Current object */
    struct object *prev_ob;	/* Save previous object */
    struct program *prog;	/* Current program */
    int num_local_variables;	/* Local + arguments */
    char *pc;
    struct svalue *fp;
    int extern_call;		/* Flag if evaluator should return */
    struct function *funp;	/* Only used for tracebacks */
    int function_index_offset;	/* Used when executing functions in inherited
				   programs */
    int variable_index_offset;	/* Same */
    short *break_sp;
};

