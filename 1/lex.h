#define MLEN 4096
#define NSIZE 256

struct lpc_predef_s {
    char *flag;
    struct lpc_predef_s *next;
};

extern struct lpc_predef_s * lpc_predefs;

#define NELEM(a) (sizeof (a) / sizeof (a)[0])

#define lookup_predef(p) (p->type == I_TYPE_GLOBAL ? p->u.global.efun : -1)

#define I_TYPE_UNKNOWN	0
#define I_TYPE_GLOBAL	2 /* function, variable AND efuns/simul_efuns */
#define I_TYPE_LOCAL	3
#define I_TYPE_RESWORD	4
#define I_TYPE_DEFINE	5

struct defn {
    union {
	char *str;
	char *(*fun)();
    } exps;
    short nargs;
    char permanent;
    char special;
};

struct s_reswords {
    char *name;
    int code;
};
struct ident {
    char *name;
    short type;
    short hash;
    struct ident *next; /* next in hash chain */
    struct ident *inferior;
    union {
        struct defn define;
        int code;
        struct {
            short function, variable, efun, sim_efun;
        } global;
        int local;
    } u;
    struct ident *next_all;
};

struct efun_shadow {
    struct ident *shadow;
    struct efun_shadow *next;
};

extern struct ident *make_shared_identifier PROT((char *, int));
extern void free_shared_identifier PROT((struct ident*));
