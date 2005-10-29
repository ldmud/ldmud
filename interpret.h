struct value {
    int type;
    struct value *next;		/* Linked list of values. */
    union {
	char *string;
	int number;
	struct lnode *ref;
	struct object *ob;
    } u;
};

#define T_INVALID	0
#define T_NUMBER	1
#define T_STRING	2
#define T_REFERENCE	3
#define T_OBJECT	4

struct value *alloc_value(), *find_value(), *apply();

extern struct value const0, const1;
