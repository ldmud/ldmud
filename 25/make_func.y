%{
#define MAKE_FUNC
#include "driver.h"

#include "my-alloca.h"
#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>

#include "exec.h"
#include "hash.h"
#include "interpret.h"

#define FUNC_SPEC 	"func_spec"
#define FUNC_TOKENS 	"efun_tokens.y"
#define CONFIG		"config.h"
#define PRO_LANG        "prolang.y"
#define THE_LANG        "lang.y"
#define THE_INSTRS	"instrs.h"
/* Amylaar:
   make_func has little problems with low memory. Therefore, I prefer to
   be genorous with the maximum allowed line length and simply leave out
   handling of overflow ( which can cause unpredictable behaviour this way...
   but the same is true for some text-editors when faced with so long lines.)
 */
#define MAKE_FUNC_MAXLINE		4096
#define MAKE_FUNC_BUFSIZ 		1024
#define NELEMS(arr) 	(sizeof arr / sizeof arr[0])

#define MAX_FUNC  	2048  /* If we need more than this we're in trouble! */
#define MAX_TOKENS	2048  /* If we need more than this we're in trouble! */
#undef malloc
#undef realloc
#undef free

#if defined(AMIGA)
#define isascii(c) ((c) >= 0 && (c) <= 255)
#endif
#undef isalunum
#define isalunum(c) (isascii((unsigned char)c) && (isalnum((unsigned char)c) || (c) == '_' ))
#define lexwhite(c) (isascii((unsigned char)c) && isspace((unsigned char)c) && (c) != '\n')
#undef lexdigit
#define lexdigit(c) (isascii((unsigned char)c) && isdigit((unsigned char)c))

#define MF_TYPE_MOD_POINTER   0x10000
#define MF_TYPE_MOD_REFERENCE 0x20000

#define C_TOKEN 0
#define C_CODE  1
#define C_EFUN  2
#define C_XCODE 3
#define C_XEFUN 4
#define C_TCODE 5
#define C_TEFUN 6
#define C_VCODE 7
#define C_VEFUN 8
#define C_ALIAS 9
#define C_TOTAL 10

#define C_IS_CODE(x) \
 ((x) == C_CODE || (x) == C_XCODE || (x) == C_TCODE || (x) == C_VCODE)
#define C_IS_EFUN(x) \
 ((x) == C_EFUN || (x) == C_XEFUN || (x) == C_TEFUN || (x) == C_VEFUN)

int num_buff = 0, num_instr[C_TOTAL] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
char *token_f_names[MAX_TOKENS];
/* For quick sort purposes : */
char *f_names[MAX_FUNC], *key[MAX_FUNC], *buf[MAX_FUNC], code_class[MAX_FUNC];

int min_arg = -1, limit_max = 0;

/*
 * arg_types is the types of all arguments. A 0 is used as a delimiter,
 * marking next argument. An argument can have several types.
 */
int arg_types[500], last_current_type;
/*
 * Store the types of the current efun. They will be copied into the
 * arg_types list if they were not already there (to save memory).
 */
int curr_arg_types[MAX_LOCAL], curr_arg_type_size;

void yyerror PROT((char *));
int yylex();
int yyparse();
int ungetc PROT((int c, FILE *f));
char *type_str PROT((int)), *etype PROT((int)), *etype1 PROT((int)),
   *ctype PROT((int));
#ifndef toupper
int toupper PROT((int));
#endif
int fatal PROT((char *str));

int current_code_class;

/* ultrix 4.2 doesn't know strdup ?!? */
char *mystrdup(str)
char *str;
{
    char *copy = malloc(strlen(str)+1);
    if (!copy) fatal("strdup failed\n");
    strcpy(copy, str);
    return copy;
}

int fatal(str)
    char *str;
{
    fprintf(stderr, "%s", str);
    fflush(stdout);
    exit(1);
    return 0;
}

char *make_f_name(str)
    char *str;
{
    char f_name[500];
    size_t i, len;

    if (strlen(str) + 1 + 2 > sizeof f_name)
	fatal("A local buffer was too small!(1)\n");
    sprintf(f_name, "F_%s", str);
    len = strlen(f_name);
    for (i=0; i < len; i++) {
	if (islower((unsigned char)f_name[i]))
	    f_name[i] = toupper(f_name[i]);
    }
    return mystrdup(f_name);
}

%}
%union {
    int number;
    char *string;
}

%token NAME ID

%token VOID INT STRING OBJECT MAPPING FLOAT CLOSURE SYMBOL QUOTED_ARRAY
%token MIXED UNKNOWN

%token DEFAULT

%token TOKENS CODES EFUNS XCODES XEFUNS TCODES TEFUNS END

%type <number> type VOID MIXED UNKNOWN
%type <number> INT STRING OBJECT MAPPING FLOAT CLOSURE SYMBOL QUOTED_ARRAY
%type <number> arg_list basic typel arg_type typel2 opt_star opt_ref

%type <string> ID optional_ID optional_default NAME optional_name

%%

all: tokens END codes END efuns END xcodes END xefuns END tcodes END tefuns;

tokens:	  TOKENS
	| tokens token;

codes:	  CODES
	| codes code;

xcodes:	  XCODES
	| xcodes code;

tcodes:	  TCODES
	| tcodes code;

efuns:	EFUNS  funcs;

xefuns:	XEFUNS funcs;

tefuns:	TEFUNS funcs;

token:	ID
    {
	token_f_names[num_instr[0]++] = make_f_name($1);
    }

optional_name: /* empty */ { $$ = 0; }
	| NAME;

code:	optional_name ID
    {
	char *f_name, buff[500];

	if (!$1) $1 = mystrdup($2);
	f_name = make_f_name($2);
	code_class[num_buff] = current_code_class;
	num_instr[current_code_class]++;
	sprintf(buff, "{0,0,{0,0},-1,0,0,\"%s\"},\n",$1);
        if (strlen(buff) > sizeof buff)
     	    fatal("Local buffer overwritten !\n");
        f_names[num_buff] = f_name;
        key[num_buff] = mystrdup($2);
	buf[num_buff] = mystrdup(buff);
        num_buff++;
	free($1);
    }

funcs: /* empty */ | funcs func ;

optional_ID: ID | /* empty */ { $$ = ""; } ;

optional_default: DEFAULT ':' ID { $$ = $3; } | /* empty */ { $$="0"; } ;

func: type ID optional_ID '(' arg_list optional_default ')' ';'
    {
	char buff[500];
	char *f_name;
	int i;
	if (min_arg == -1)
	    min_arg = $5;
	if ($3[0] == '\0') {
	    f_name = make_f_name($2);
	    num_instr[
	      (int)(code_class[num_buff] =
		limit_max && current_code_class == C_TEFUN ?
		  C_VEFUN : current_code_class)
	    ]++;
	} else {
	    f_name = mystrdup($3);
	    code_class[num_buff] = C_ALIAS;
	    num_instr[C_ALIAS]++;
	}
	for(i=0; i < last_current_type; i++) {
	    int j;
	    for (j = 0; j+i<last_current_type && j < curr_arg_type_size; j++)
	    {
		if (curr_arg_types[j] != arg_types[i+j])
		    break;
	    }
	    if (j == curr_arg_type_size)
		break;
	}
	if (i == last_current_type) {
	    int j;
	    for (j=0; j < curr_arg_type_size; j++) {
		arg_types[last_current_type++] = curr_arg_types[j];
		if (last_current_type == NELEMS(arg_types))
		    yyerror("Array 'arg_types' is too small");
	    }
	}
	sprintf(buff, "{%d,%d,{%s,%s},%s,%s,%d,\"%s\"},\n",
		limit_max ? -1 : $5, min_arg, etype(0), etype(1),
		$6, ctype($1), i, $2);
        if (strlen(buff) > sizeof buff)
     	    fatal("Local buffer overwritten !\n");
        f_names[num_buff] = f_name;
        key[num_buff] = mystrdup($2);
	buf[num_buff] = mystrdup(buff);
        num_buff++;
	min_arg = -1;
	limit_max = 0;
	curr_arg_type_size = 0;
    } ;

type: basic opt_star opt_ref { $$ = $1 | $2 | $3; };

basic: VOID | INT | STRING | MAPPING | FLOAT | MIXED | OBJECT | CLOSURE |
	UNKNOWN | SYMBOL | QUOTED_ARRAY ;

opt_star : '*' { $$ = MF_TYPE_MOD_POINTER; }
	|      { $$ = 0;                   } ;

opt_ref : '&' { $$ = MF_TYPE_MOD_REFERENCE; }
	|     { $$ = 0;                     } ;

arg_list: /* empty */		{ $$ = 0; }
	| typel2			{ $$ = 1; if ($1) min_arg = 0; }
	| arg_list ',' typel2 	{ $$ = $1 + 1; if ($3) min_arg = $$ - 1; } ;

typel2: typel
    {
	$$ = $1;
	curr_arg_types[curr_arg_type_size++] = 0;
	if (curr_arg_type_size == NELEMS(curr_arg_types))
	    yyerror("Too many arguments");
    } ;

arg_type: type
    {
	if ($1 != VOID) {
	    curr_arg_types[curr_arg_type_size++] = $1;
	    if (curr_arg_type_size == NELEMS(curr_arg_types))
		yyerror("Too many arguments");
	}
	$$ = $1;
    } ;

typel: arg_type			{ $$ = ($1 == VOID && min_arg == -1); }
     | typel '|' arg_type 	{ $$ = (min_arg == -1 && ($1 || $3 == VOID));}
     | '.' '.' '.'		{ $$ = min_arg == -1 ; limit_max = 1; } ;

%%

struct type {
    char *name;
    int num;
} types[] = {
{ "void", VOID },
{ "int", INT },
{ "string", STRING },
{ "object", OBJECT },
{ "mapping", MAPPING },
{ "float", FLOAT },
{ "closure", CLOSURE },
{ "symbol", SYMBOL },
{ "quoted_array", QUOTED_ARRAY },
{ "mixed", MIXED },
{ "unknown", UNKNOWN }
};

int current_line;
char *current_file;
int last_line;

#define MAKE_FUNC_DEFHASH 101
#define defhash(str) (whashstr((str), 12) % MAKE_FUNC_DEFHASH)

struct defn {
    char *name;
    char *exps;
    int  num_arg;
    struct defn *next;
};

static struct defn *deftab[MAKE_FUNC_DEFHASH];

static char *outp;

static int
mygetc()
{
    return *outp++;
}

static void
myungetc(c)
    int c;
{
    *--outp = c;
}

static void
add_input(p)
    char *p;
{
    int l;

    l = strlen(p);
    outp -= l;
    strncpy(outp, p, l);
}

static void add_define(name, num_arg, exps)
    char *name;
    int num_arg;
    char *exps;
{
    int i;
    struct defn *ndef;

    i = defhash(name);
    ndef = (struct defn *)malloc(sizeof(struct defn));
    if (!ndef) {
	abort();
    }
    ndef->next    = deftab[i];
    ndef->exps    = mystrdup(exps);
    ndef->num_arg = num_arg;
    ndef->name    = mystrdup(name);
    deftab[i]     = ndef;
}

static struct defn *
lookup_define(s)
char * s;
{
    struct defn *curr, *prev;
    int h;

    h = defhash(s);

    curr = deftab[h];
    prev = 0;
    while (curr) {
	if (!strcmp(curr->name, s)) { /* found it */
	    if (prev) { /* not at head of list */
		prev->next = curr->next;
		curr->next = deftab[h];
		deftab[h] = curr;
	    }
	    return curr;
	}
	prev = curr;
	curr = curr->next;
    } /* not found */
    return 0;
}

static int
expand_define(s)
    char *s;
{
    struct defn *p;

    p = lookup_define(s);
    if (!p)
	return 0;
    if (p->num_arg < 0) {
	add_input(p->exps);
    } else {
	return -1;
    }
    return 1;

}

static char *nextword(str)
    char *str;
{
    char *cp;

    while(!lexwhite(*str)) str++;
    while( lexwhite(*str)) str++;
    for(cp = str; isalunum(*cp); ) cp++;
    *cp = '\0';
    return str;
}

static struct ifstate {
    struct ifstate *next;
    int state;
} *iftop = 0;

#define EXPECT_ELSE 1
#define EXPECT_ENDIF 2

FILE *fpr, *fpw;

static int
skip_to(mark, token, atoken)
char mark, *token, *atoken;
{
    char b[20], *p;
    int c;
    int nest;
    FILE *fp = fpr;

    for(nest = 0;;) {
	current_line++;
	c = fgetc(fp);
	if (c == mark) {
	    do {
		c = fgetc(fp);
	    } while(lexwhite(c));
	    for(p = b; c != '\n' && c != EOF; ) {
		if (p < b+sizeof b-1)
		    *p++ = c;
		c = fgetc(fp);
	    }
	    *p++ = 0;
	    for(p = b; *p && !lexwhite(*p); p++)
		;
	    *p = 0;
	    if (strcmp(b, "if") == 0 || strcmp(b, "ifdef") == 0 ||
		strcmp(b, "ifndef") == 0) {
		nest++;
	    } else if (nest > 0) {
		if (strcmp(b, "endif") == 0)
		    nest--;
	    } else {
		if (strcmp(b, token) == 0)
		    return 1;
		else if (atoken && strcmp(b, atoken) == 0)
		    return 0;
	    }
	} else {
            while (c != '\n' && c != EOF) {
		c = fgetc(fp);
	    } 
	    if (c == EOF) {
		fprintf(stderr, "Unexpected end of file while skipping");
		abort();
	    }
	}
    }
}

static void compensate_lines() {
    for (; last_line <= current_line; last_line++)
        fputc('\n', fpw);
}

static void
handle_cond(mark, c)
char mark;
int c;
{
    struct ifstate *p;

    if (c || skip_to(mark, "else", "endif")) {
	p = (struct ifstate *)malloc(sizeof(struct ifstate));
	p->next = iftop;
	iftop = p;
	p->state = c ? EXPECT_ELSE : EXPECT_ENDIF;
    }
    if (mark == '%')
	compensate_lines();
}

static int cond_get_exp();

static void handle_if(mark, str)
    char mark, *str;
{
    int cond;

    add_input(str);
    cond = cond_get_exp(0);
    if (mygetc() != '\n') {
	yyerror("Condition too complex in #/%if");
	fflush(stdout);
	if (mark == '%') exit(1);
	while(mygetc() != '\n') NOOP;
    } else
	handle_cond(mark, cond );
}

static void handle_else(mark)
    char mark;
{
    if (iftop && iftop->state == EXPECT_ELSE) {
	struct ifstate *p = iftop;

	iftop = p->next;
	free((char *)p);
	skip_to(mark, "endif", (char *)0);
    } else {
	fprintf(stderr, "Unexpected #/%%else line %d\n", current_line);
	abort();
    }
}

static void handle_endif()
{
    if (iftop && (iftop->state == EXPECT_ENDIF ||
	iftop->state == EXPECT_ELSE)) {
	struct ifstate *p = iftop;

	iftop = p->next;
	free((char *)p);
    } else {
	fprintf(stderr, "Unexpected #/%%endif line %d\n", current_line);
	abort();
    }
}

static int name_to_type(name)
    char *name;
{
    while (isspace((unsigned char)*name))
	name++;
    if ( strncmp(name, "TYPE_", 5) )
	return -1;
    name += 5;
    if ( !strcmp(name, "ANY") )
	return TYPE_ANY;
    if ( !strcmp(name, "NUMBER") )
	return TYPE_NUMBER;
    if ( !strcmp(name, "FLOAT") )
	return TYPE_FLOAT;
    if ( !strcmp(name, "OBJECT") )
	return TYPE_OBJECT;
    if ( !strcmp(name, "MAPPING") )
	return TYPE_MAPPING;
    if ( !strcmp(name, "CLOSURE") )
	return TYPE_CLOSURE;
    return -1;
}

static int name_to_hook(name)
    char *name;
{
    while (isspace((unsigned char)*name))
	name++;
    if ( strncmp(name, "H_", 2) )
	return -1;
    name += 2;
    if ( !strcmp(name, "MOVE_OBJECT0") )
	return H_MOVE_OBJECT0;
    if ( !strcmp(name, "MOVE_OBJECT1") )
	return H_MOVE_OBJECT1;
    if ( !strcmp(name, "LOAD_UIDS") )
	return H_LOAD_UIDS;
    if ( !strcmp(name, "CLONE_UIDS") )
	return H_CLONE_UIDS;
    if ( !strcmp(name, "CREATE_SUPER") )
	return H_CREATE_SUPER;
    if ( !strcmp(name, "CREATE_OB") )
	return H_CREATE_OB;
    if ( !strcmp(name, "CREATE_CLONE") )
	return H_CREATE_CLONE;
    if ( !strcmp(name, "RESET") )
	return H_RESET;
    if ( !strcmp(name, "CLEAN_UP") )
	return H_CLEAN_UP;
    if ( !strcmp(name, "MODIFY_COMMAND") )
	return H_MODIFY_COMMAND;
    if ( !strcmp(name, "NOTIFY_FAIL") )
	return H_NOTIFY_FAIL;
    if ( !strcmp(name, "NO_IPC_SLOT") )
	return H_NO_IPC_SLOT;
    if ( !strcmp(name, "INCLUDE_DIRS") )
	return H_INCLUDE_DIRS;
    if ( !strcmp(name, "TELNET_NEG") )
	return H_TELNET_NEG;
    if ( !strcmp(name, "NOECHO") )
	return H_NOECHO;
    if ( !strcmp(name, "ERQ_STOP") )
	return H_ERQ_STOP;
    if ( !strcmp(name, "MODIFY_COMMAND_FNAME") )
	return H_MODIFY_COMMAND_FNAME;
    return -1;
}

static void handle_map(str, size, name_to_index)
    char *str;
    int size;
    int (*name_to_index) PROT((char *));
{
    char **map, deflt[256];
    char *del = 0, *val;
    int i;
    char *output_del = "";

    map = (char **)alloca(size * sizeof *map);
    strcpy(deflt, "0");
    for (i = 0; i < size; i++) {
	map[i] = deflt;
    }
    do {
	while (isspace((unsigned char)*str))
	    str++;
	if (*str == '\\') {
	    str = alloca(MAKE_FUNC_MAXLINE + 1);
	    if (!fgets(str, MAKE_FUNC_MAXLINE, fpr))
		break;
	    if (del) {
		output_del = "\n";
	    }
	}
	del = strchr(str, ':');
	if (!del)
	    break;
	*del = '\0';
	val = del+1;
	del = strchr(val, ',');
	if (!del) {
	    del = strchr(val, '\n');
	    if (del) {
		*del = '\0';
		del = 0;
	    }
	} else {
	    *del = '\0';
	}
	if ( !strcmp(str, "default") ) {
	    strncpy(deflt, val, sizeof(deflt));
	    deflt[sizeof deflt - 1] = '\0';
	} else {
	    i = (*name_to_index)(str);
	    if (i < 0)
		exit(-1);
	    map[i] = val;
	}
	str = del+1;
    } while (del);
    fprintf(fpw, "{");
    fprintf(fpw, output_del);
    for (i = 0; i < size; i++) {
	fprintf(fpw, "%s,", map[i]);
	fprintf(fpw, output_del);
    }
    fprintf(fpw, "};\n");
}

static int exgetc() {
    register unsigned char c;

    c= (unsigned char) mygetc();
    while ( isalpha(c) || c=='_' ) {
	char word[512], *p;
	int space_left;

	p = word;
	space_left = sizeof(word);
	do {
	    *p++ = c;
	    c=mygetc();
	} while ( isalunum(c) && --space_left);
	if (!space_left) fatal("Too long word.\n");
	myungetc(c);
	*p='\0';
	if (strcmp(word, "defined") == 0) {
	    /* handle the defined "function" in #if */
	    do c = mygetc(); while(lexwhite(c));
	    if (c != '(') {
		yyerror("Missing ( in defined");
		continue;
	    }
	    do c = mygetc(); while(lexwhite(c));
	    p = word;
	    space_left = sizeof(word);
	    while ( isalunum(c) && --space_left) {
		*p++ = c;
		c=mygetc();
	    }
	    *p='\0';
	    while(lexwhite(c)) c = mygetc();
	    if (c != ')') {
		yyerror("Missing ) in defined");
		continue;
	    }
	    if (lookup_define(word))
	    	add_input(" 1 ");
	    else
		add_input(" 0 ");
	} else {
	    int res;

	    res = expand_define(word);
	    if (res < 0) {
		yyerror("Unimplemented macro expansion");
		return 0;
	    }
	    if (!res) add_input(" 0 ");
	}
	c=mygetc();
    }
    return c;
}

#define BNOT   1
#define LNOT   2
#define UMINUS 3
#define UPLUS  4

#define MULT   1
#define DIV    2
#define MOD    3
#define BPLUS  4
#define BMINUS 5
#define LSHIFT 6
#define RSHIFT 7
#define LESS   8
#define LEQ    9
#define GREAT 10
#define GEQ   11
#define EQ    12
#define NEQ   13
#define BAND  14
#define XOR   15
#define BOR   16
#define LAND  17
#define LOR   18
#define QMARK 19

static char _optab[]=
{0,4,0,0,0,26,56,0,0,0,18,14,0,10,0,22,0,0,0,0,0,0,0,0,0,0,0,0,30,50,40,74,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,70,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63,0,1};
static char optab2[]=
{BNOT,0,0,LNOT,'=',NEQ,7,0,0,UMINUS,0,BMINUS,10,UPLUS,0,BPLUS,10,
0,0,MULT,11,0,0,DIV,11,0,0,MOD,11,
0,'<',LSHIFT,9,'=',LEQ,8,0,LESS,8,0,'>',RSHIFT,9,'=',GEQ,8,0,GREAT,8,
0,'=',EQ,7,0,0,0,'&',LAND,3,0,BAND,6,0,'|',LOR,2,0,BOR,4,
0,0,XOR,5,0,0,QMARK,1};
#define optab1 (_optab-' ')

static int cond_get_exp(priority)
int priority;
{
  int c;
  int value,value2,x;

  do c=exgetc(); while ( lexwhite(c) );
  if ( c=='(' ) {

    value=cond_get_exp(0);
    do c=exgetc(); while ( lexwhite(c) );
    if ( c!=')' ) {
      yyerror("bracket not paired in #if");
      if (!c) myungetc('\0');
    }
  } else if ( ispunct(c) ) {
    x=optab1[c];
    if (!x) {
      yyerror("illegal character in #if");
      return 0;
    }
    value=cond_get_exp(12);
    switch ( optab2[x-1] ) {
      case BNOT  : value = ~value; break;
      case LNOT  : value = !value; break;
      case UMINUS: value = -value; break;
      case UPLUS : value =  value; break;
      default :
	yyerror("illegal unary operator in #if");
	return 0;
    }
  } else {
    int base;

    if ( !lexdigit(c) ) {
      if (!c) {
	yyerror("missing expression in #if");
	myungetc('\0');
      } else yyerror("illegal character in #if");
      return 0;
    }
    value=0;
    if ( c!='0' ) base=10;
    else {
      c=mygetc();
      if ( c=='x' || c=='X' ) {
	base=16;
	c=mygetc();
      } else base=8;
    }
    for(;;) {
      if ( isdigit(c) ) x = -'0';
      else if ( isupper(c) ) x = -'A'+10;
      else if ( islower(c) ) x = -'a'+10;
      else break;
      x+=c;
      if ( x > base ) break;
      value=value*base+x;
      c=mygetc();
    }
    myungetc(c);
  }
  for (;;) {
    do c=exgetc(); while ( lexwhite(c) );
    if ( !ispunct(c) ) break;
    x=optab1[c];
    if (!x) break;
    value2=mygetc();
    for(;;x+=3) {
      if ( !optab2[x] ) {
	myungetc(value2);
	if ( !optab2[x+1] ) {
	  yyerror("illegal operator use in #if");
	  return 0;
	}
	break;
      }
      if ( value2==optab2[x] ) break;
    }
    if ( priority >= optab2[x+2] ) {
      if( optab2[x] ) myungetc(value2);
      break;
    }
    value2=cond_get_exp(optab2[x+2]);
    switch ( optab2[x+1] ) {
      case MULT : value *= value2;	break;
      case DIV  : value /= value2;	break;
      case MOD  : value %= value2;	break;
      case BPLUS  : value += value2;	break;
      case BMINUS : value -= value2;	break;
      case LSHIFT : value <<= value2;	break;
      case RSHIFT : value >>= value2;	break;
      case LESS   : value = value <  value2;	break;
      case LEQ    : value = value <= value2;	break;
      case GREAT  : value = value >  value2;	break;
      case GEQ    : value = value >= value2;	break;
      case EQ     : value = value == value2;	break;
      case NEQ    : value = value != value2;	break;
      case BAND   : value &= value2;	break;
      case XOR    : value ^= value2;	break;
      case BOR    : value |= value2;	break;
      case LAND   : value = value && value2;	break;
      case LOR    : value = value || value2;	break;
      case QMARK  :
	do c=exgetc(); while( lexwhite(c) );
	if ( c!=':' ) {
	  yyerror("'?' without ':' in #if");
	  myungetc(c);
	  return 0;
	}
	if ( value ) {
	  cond_get_exp(1);
	  value=value2;
	}
	else value=cond_get_exp(1);
	break;
    }
  }
  myungetc(c);
  return value;
}

int make_func_isescaped(c)
char c;
{
    switch(c) {
      case '\007':
      case '\b'  :
      case '\t'  :
      case '\n'  :
      case '\013':
      case '\014':
      case '\r'  :
        return 1;
    }
    if (c == '\\' || c == '\"') return 1;
    return 0;
}

static int efuncmp(i,j)
    int i, j;
{
    int result;

    result = code_class[i] - code_class[j];
    if ( result )
	return result;
    if (C_IS_CODE(code_class[i])) return 0;
    return strcmp(key[i], key[j]);
}

int main (argc, argv)
    int argc UNUSED;
    char ** argv UNUSED;
{
    int i, j, k;
    int match_tmp;
    unsigned char c;
    char line_buffer[MAKE_FUNC_MAXLINE + 1];
    char defbuf[MAKE_FUNC_MAXLINE + 1];

#ifdef AMIGA
    add_define("AMIGA",-1,"");
#endif
#ifdef HAVE_GETRUSAGE
    add_define("HAVE_GETRUSAGE",-1,"");
#endif
#ifdef TRACE_CODE
    add_define("TRACE_CODE",-1,"");
#endif
    if ((fpw = fopen(THE_LANG, "w")) == 0) {
       perror(THE_LANG);
       exit(1);
    }
    fprintf(fpw, "%s", "\
%union{ int i; char *p; }\n\
%type <p> all\n\
%%\n\
all: { $<p>$ = 0; } 'a'\n\
%%\n\
");
    fclose(fpw);
    sprintf(line_buffer, "%s %s", YACC, THE_LANG);
    printf("#if 0\n");
    fflush(stdout);
    fprintf(stderr, "checking default & anonymous rules in %s\n", YACC);
    if (system(line_buffer)) {
	fprintf(
	  stderr,
"It seems to have trouble with this combination, I'll avoid the latter.\n"
	);
	add_define("YACC_CANNOT_MIX_ANONYMOUS_WITH_DEFAULT", -1, "");
    }
    printf("#endif\n\n");
    outp = defbuf + sizeof(defbuf) - 1;
    /* read in the defines of the configuration file */
    if ((fpr = fopen(CONFIG, "r")) == 0) {
       perror(CONFIG);
       fflush(stdout);
       exit(1);
    }
    current_line = 0;
    current_file = CONFIG;

#define MATCH(str) (isspace((unsigned char)line_buffer[1+(match_tmp=strlen(str))]) &&\
			strncmp(str, line_buffer+1, match_tmp) == 0)

    while (fgets(line_buffer, MAKE_FUNC_MAXLINE, fpr)) {
	char *name;

	current_line++;
	if ( *line_buffer != '#' )
	    continue;
	if MATCH("if") {
	    handle_if('#', line_buffer+4);
	    continue;
	}
	if MATCH("ifdef") {
	    handle_cond('#', lookup_define(nextword(line_buffer)) != 0);
	    continue;
	}
	if MATCH("ifndef") {
	    handle_cond('#', lookup_define(nextword(line_buffer)) == 0);
	    continue;
	}
	if MATCH("else") {
	    handle_else('#');
	    continue;
	}
	if MATCH("endif") {
	    handle_endif();
	    continue;
	}
	if MATCH("undef") {
	    struct defn *old_def;
	    old_def = lookup_define(nextword(line_buffer));
	    if (old_def)
		old_def->name[0] = '\0';
	    continue;
	}
	if ( !MATCH("define") ) {
	    continue;
	}
	/* CONFIG is trusted to be syntactically correct. After all, it was
	 * included by the source of this program.
	 */
	{
	    char *cp, *exps;
	    int num_arg;

	    cp = line_buffer+8;
	    while( isspace((unsigned char)*cp)) cp++;
	    name = cp;
	    while(isalunum((unsigned char)*cp)) cp++;
	    num_arg = *cp == '(' ? 0 : -1;
	    if (*cp == '\n') {
		exps = cp;
		*cp = '\0';
	    } else {
		*cp++ = '\0';
		while( isspace((unsigned char)*cp)) cp++;
		exps = cp;
		while(*cp != '\n') cp++;
		*cp = '\0';
	    }
	    add_define(name, num_arg, exps);
	}
    }
    fclose(fpr);

    if ((fpr = fopen(FUNC_SPEC, "r")) == NULL) { 
	perror(FUNC_SPEC);
	exit(1);
    }
    current_line = 1;
    current_file = FUNC_SPEC;
    yyparse();
    fclose(fpr);
    fprintf(
	stderr, "\
Primary codes: %d Secondary codes: %d\n\
Tabled codes: %d Tabled varargs codes: %d\n",
	num_instr[C_CODE]  + num_instr[C_EFUN],
	num_instr[C_XCODE] + num_instr[C_XEFUN],
	num_instr[C_TCODE] + num_instr[C_TEFUN],
	num_instr[C_VCODE] + num_instr[C_VEFUN]
    );
    if ( (num_instr[C_CODE] + num_instr[C_EFUN]) & ~0xff ||
         ( (num_instr[C_XCODE]+ num_instr[C_XEFUN]) |
           (num_instr[C_TCODE]+ num_instr[C_TEFUN]) |
           (num_instr[C_VCODE]+ num_instr[C_VEFUN]) ) & ~0x7f)
    {
	fatal("Codes exhausted!");
    }
    /* Now sort the main_list */
    for (i = num_buff; --i >= 0; ) {
       for (j = 0; j < i; j++)
	   if (efuncmp(i,j) < 0) {
	      char *tmp;
	      int tmpi;
	      tmp = f_names[i]; f_names[i] = f_names[j]; f_names[j] = tmp;
	      tmp = key[i]; key[i] = key[j]; key[j] = tmp;
	      tmp = buf[i]; buf[i] = buf[j]; buf[j] = tmp;
	      tmpi = code_class[i];
	      code_class[i] = code_class[j]; code_class[j] = tmpi;
           }
    }
    /* Now display it... */
    printf("#include \"exec.h\" /* for size of struct instr */\n\n");
    printf("struct instr instrs[]={\n");
    j = num_instr[C_CODE] + num_instr[C_EFUN];
    for (i = 0; i < j; i++) {
	printf(" %s", buf[i]);
    }
    for (i -= 256 ; ++i <= 0; ) {
	printf(" {0,0,{0,0},-1,0,0,0},\n");
    }
    i = j;
    j += num_instr[C_XCODE] + num_instr[C_XEFUN];
    for (; i < j; i++) {
	printf(" %s", buf[i]);
    }
    for (i = 128 - num_instr[C_XCODE] - num_instr[C_XEFUN]; --i >= 0; ) {
	printf(" {0,0,{0,0},-1,0,0,0},\n");
    }
    i = j;
    j += num_instr[C_TCODE] + num_instr[C_TEFUN];
    for (; i < j; i++) {
	printf(" %s", buf[i]);
    }
    for (i = 128 - num_instr[C_TCODE] - num_instr[C_TEFUN]; --i >= 0; ) {
	printf(" {0,0,{0,0},-1,0,0,0},\n");
    }
    i = j;
    for (; i < num_buff; i++) {
	printf(" %s", buf[i]);
    }
    printf("};\nshort efun_aliases[]={\n");
    for (i = 0; i < num_buff; i++) {
	if (code_class[i] == C_ALIAS)
	    printf(" %s-F_OFFSET,\n", f_names[i]);
    }
    printf("};\nint efun_arg_types[] = {\n ");
    for (i=0; i < last_current_type; i++) {
	if (arg_types[i] == 0)
	    printf("0,\n ");
	else
	    printf("%s,", ctype(arg_types[i]));
    }
    printf("};\n\n");
    for(j = 1, i = 0; j < C_TCODE; j++)
	i += num_instr[j];
    j = i + num_instr[C_TCODE] + num_instr[C_TEFUN];
    k = i;
    while(k < j) {
	printf("struct svalue *f_%s PROT((struct svalue *));\n", key[k++]);
    }
    printf("\nstruct svalue *(*efun_table[]) PROT((struct svalue *)) = {\n");
    while(i < j) {
	printf(" f_%s,\n", key[i++]);
    }
    printf("};\n\n");
    j = i + num_instr[C_VCODE] + num_instr[C_VEFUN];
    k = i;
    while(k < j) {
	printf("struct svalue *f_%s PROT((struct svalue *, int));\n", key[k++]);
    }
    printf("\nstruct svalue *(*vefun_table[]) PROT((struct svalue *, int)) = {\n");
    while(i < j) {
	printf(" f_%s,\n", key[i++]);
    }
    printf("};\n\n");
    /* Make our own character type classification. This is for two reasons:
     * i)  The standard isXXX macros are only defined on ascii values.
     *     there might be non-ascii characters in the compiled files.
     * ii) We actually need some nonstandard classifications, and the lexical
     *     scanner can be costly in terms of cpu usage.
     */

	printf("#define lexwhite(c) (_my_ctype[(unsigned char)(c)]&%d)\n",_MCTs);
	printf("#define leXdigit(c) (_my_ctype[(unsigned char)(c)]&%d)\n",_MCTx);
	printf("unsigned char _my_ctype[]={");
	c = '\0';
	do {
	    if (!(c & 0xf)) printf("\n    ");
	    printf("%d,", !isascii(c) ? 0 :
	        ( make_func_isescaped(c)   ? _MCTe : 0 ) |
	        ( isdigit (c)		   ? _MCTd : 0 ) |
	        ( isspace (c) && c != '\n' ? _MCTs : 0 ) |
	        ( isxdigit(c)		   ? _MCTx : 0 ) |
	        ( isalnum (c) || c == '_'  ? _MCTa : 0 ) );
	    c++;
	} while (c != '\0');
	printf("};\n");

    if ((fpw = fopen(THE_INSTRS, "w")) == 0) {
       perror(THE_INSTRS);
       exit(1);
    }
    fprintf(fpw, "#include \"exec.h\" /* for size of struct instr */\n\n");
    fprintf(fpw, "#define F_OFFSET %s\n", f_names[0]);
    fprintf(fpw, "#define LAST_INSTRUCTION_CODE %d\n",
	256+128+128+num_instr[C_VCODE]+num_instr[C_VEFUN] - 1
    );
    fprintf(fpw, "\nextern struct instr instrs[%d];\n",
	256+128+128+num_instr[C_VCODE]+num_instr[C_VEFUN]+num_instr[C_ALIAS]
    );
    fprintf(fpw, "extern short efun_aliases[%d];\n\n", num_instr[C_ALIAS]);
    for (i = j = num_instr[C_CODE]; i < num_buff; i++) {
	if (j == num_instr[C_CODE] + num_instr[C_EFUN]) {
	    j = 256;
	}
	if (j == 256 + num_instr[C_XCODE] + num_instr[C_XEFUN]) {
	    j = 256+128;
	}
	if (j == 256 + 128 + num_instr[C_TCODE] + num_instr[C_TEFUN]) {
	    j = 256+128+128;
	}
	if (code_class[i] != C_ALIAS) {
	    fprintf(fpw, "#define %s (F_OFFSET+%d)\n", make_f_name(key[i]), j);
	    j++;
	}
    }
    fclose(fpw);
    /*
     * Write out the tokens that the compiler needs. Don't copy all the efuns,
     * lest yacc might choke on the number of terminal symbols .
     * Do this by copying the
     * pre-include portion of lang.y to lang.y, appending
     * this information, then appending the post-include
     * portion of lang.y.  It's done this way because I don't
     * know how to get YACC to #include %token files.  *grin*
     */
    if ((fpr = fopen(PRO_LANG, "r")) == 0) {
       perror(PRO_LANG);
       exit(1);
    }
    if ((fpw = fopen(THE_LANG, "w")) == 0) {
       perror(THE_LANG);
       exit(1);
    }
    current_line = 0;
    while (fgets(line_buffer, MAKE_FUNC_MAXLINE, fpr)) {
	current_line++;
	if (*line_buffer == '%') {
	    if MATCH("efuns") {
		for (i = 0; i < num_instr[C_TOKEN]; i++) {
		    fprintf(fpw, "%%token %s\n", token_f_names[i]);
		}
		for (i = 0; i < num_instr[C_CODE]; i++) {
		    if (code_class[i] != C_ALIAS) {
			fprintf(fpw , "%%token %s\n", make_f_name(key[i]));
		    }
		}
		continue;
	    }
	    last_line = current_line;
	    if MATCH("if") {
		handle_if('%', line_buffer+4);
		continue;
	    }
	    if MATCH("ifdef") {
		handle_cond('%', lookup_define(nextword(line_buffer)) != 0);
		continue;
	    }
	    if MATCH("ifndef") {
		handle_cond('%', lookup_define(nextword(line_buffer)) == 0);
		continue;
	    }
	    if MATCH("else") {
	        handle_else('%');
	        compensate_lines();
		continue;
	    }
	    if MATCH("endif") {
	        handle_endif();
	        compensate_lines();
		continue;
	    }
	    if MATCH("line") {
		fprintf(fpw, "#line %d \"%s\"\n", current_line+1, PRO_LANG);
		continue;
	    }
	    if MATCH("typemap") {
		handle_map(line_buffer+9, TYPEMAP_SIZE, name_to_type);
		continue;
	    }
	    if MATCH("hookmap") {
		handle_map(line_buffer+9, NUM_CLOSURE_HOOKS, name_to_hook);
		continue;
	    }
	    if MATCH("//") {
		/* c++ - resembling comment */
		fputs("", fpw);
		continue;
	    }
        }
        fputs(line_buffer, fpw);
    }
    fclose(fpr), fclose(fpw);
    return 0;
}

#undef MATCH

void yyerror(str)
    char *str;
{
    fprintf(stderr, "%s:%d: %s\n", current_file, current_line, str);
    exit(1);
}

int ident(c)
    int c;
{
    char buff[100];
    size_t len, i;

    for (len=0; isalunum(c); c = getc(fpr)) {
	if (len == sizeof buff - 1) {
	    yyerror("Too long indentifier");
	    do c = getc(fpr); while (isalunum(c));
	    break;
	}
	buff[len++] = c;
    }
    (void)ungetc(c, fpr);
    buff[len] = '\0';
    if ( C_IS_EFUN(current_code_class) ) {
	for (i=0; i < NELEMS(types); i++) {
	    if (strcmp(buff, types[i].name) == 0) {
		yylval.number = types[i].num;
		return types[i].num;
	    }
	}
	if (strcmp(buff, "default") == 0)
	    return DEFAULT;
    }
    yylval.string = mystrdup(buff);
    return ID;
}

char *type_str(n)
    int n;
{
    int type = n & 0xffff;
    size_t i;

    for (i=0; i < NELEMS(types); i++) {
	if (types[i].num == type) {
	    if (n & MF_TYPE_MOD_REFERENCE) {
		static char buff[100];
		char *str;

		str = type_str(n & ~MF_TYPE_MOD_REFERENCE);
		if (strlen(str) + 3 > sizeof buff)
		    fatal("Local buffer too small in type_str()!\n");
		sprintf(buff, "%s &", str);
		return buff;
	    }
	    if (n & MF_TYPE_MOD_POINTER) {
		static char buff[100];

		if (strlen(types[i].name) + 3 > sizeof buff)
		    fatal("Local buffer too small in type_str()!\n");
		sprintf(buff, "%s *", types[i].name);
		return buff;
	    }
	    return types[i].name;
	}
    }
    return "What ?";
}

static void
skip_comment()
{
    int c;

    for(;;) {
	while((c = getc(fpr)) != '*') {
	    if (c == EOF) {
	        yyerror("End of file in a comment");
		return;
	    }
	    if (c == '\n') {
		current_line++;
	    }
	}
	do {
	    if ((c = getc(fpr)) == '/')
		return;
	    if (c == '\n') {
		current_line++;
	    }
	} while(c == '*');
    }
}

int yylex1() {
    register int c;
    
    for(;;) {
	int match_tmp;
#define MATCH(str) (isspace((unsigned char)line_buffer[match_tmp=strlen(str)]) &&\
			strncmp(str, line_buffer, match_tmp) == 0)

	char line_buffer[MAKE_FUNC_MAXLINE+1];

	switch(c = getc(fpr)) {
	  case ' ':
	  case '\t':
	    continue;
	  case '#':
	  {
	    int line;
	    char file[2048]; /* does any operating system support
				longer pathnames? */
	    fgets(line_buffer, MAKE_FUNC_MAXLINE, fpr);
	    if ( sscanf(line_buffer, "%d \"%s\"",&line,file ) == 2 ) {
		current_line = line+1;
		continue;
	    }
	    current_line++;
	    if MATCH("if") {
		handle_if('#', line_buffer+3);
	    } else if MATCH("ifdef") {
		handle_cond('#', lookup_define(nextword(line_buffer)) != 0);
	    } else if MATCH("ifndef") {
		handle_cond('#', lookup_define(nextword(line_buffer)) == 0);
		continue;
	    } else if MATCH("else") {
		handle_else('#');
	    } else if MATCH("endif") {
		handle_endif();
	    } else {
		yyerror("unrecognised '#' directive\n");
	    }
	    continue;
	  }
	  case '%':
	  {
	    static int send_end = 0;

	    if (send_end) {
		send_end = 0;
		ungetc('%', fpr);
		return END;
	    }
	    send_end = 1;
	    fgets(line_buffer, MAKE_FUNC_MAXLINE, fpr);
	    current_line++;
	    if (MATCH("tokens")) { current_code_class=C_TOKEN; return TOKENS; }
	    if (MATCH("codes"))  { current_code_class=C_CODE;  return CODES;  }
	    if (MATCH("efuns"))  { current_code_class=C_EFUN;  return EFUNS;  }
	    if (MATCH("xcodes")) { current_code_class=C_XCODE; return XCODES; }
	    if (MATCH("xefuns")) { current_code_class=C_XEFUN; return XEFUNS; }
	    if (MATCH("tcodes")) { current_code_class=C_TCODE; return TCODES; }
	    if (MATCH("tefuns")) { current_code_class=C_TEFUN; return TEFUNS; }
	    return '%';

#undef MATCH
	  }
	  case '\"':
	  {
	    char buff[100];
	    int len;

	    for (len=0; c = getc(fpr),c != '\"';) {
		if (len == sizeof buff - 1) {
		    yyerror("Too long name");
		    do
			c = getc(fpr);
		    while (c != '\"' && c != '\n' && c != EOF);
		    (void)ungetc(c, fpr);
		    break;
		}
		if (c == '\n' || c == EOF) {
		    yyerror("unterminated name");
		    (void)ungetc(c, fpr);
		    break;
		}
		buff[len++] = c;
	    }
	    buff[len] = '\0';
	    yylval.string = mystrdup(buff);
	    return NAME;
	  }
	  case '\n':
	    current_line++;
	    continue;
	  case EOF:
	    return -1;
	  case '/':
	    if ( (c=getc(fpr)) == '*') {
		skip_comment();
		continue;
	    } else {
		(void)ungetc(c, fpr);
		return '/';
	    }
	  default:
	    if (isalpha(c))
		return ident(c);
	    return c;
	}
    }
}

int yylex() {
    int res;

    res = yylex1();
#if 0
    fprintf(stderr,"yylex returns %d '%c'\n",res,res);
#endif
    return res;
}

char *etype1(n)
    int n;
{
    if (n & MF_TYPE_MOD_REFERENCE)
	return "T_LVALUE";
    if (n & MF_TYPE_MOD_POINTER)
	return "T_POINTER";
    switch(n) {
    case INT:
	return "T_NUMBER";
    case OBJECT:
	return "T_OBJECT";
    case STRING:
	return "T_STRING";
    case MAPPING:
	return "T_MAPPING";
    case FLOAT:
	return "T_FLOAT";
    case CLOSURE:
	return "T_CLOSURE";
    case SYMBOL:
	return "T_SYMBOL";
    case QUOTED_ARRAY:
	return "T_QUOTED_ARRAY";
    case MIXED:
	return "0";	/* 0 means any type */
    default:
	yyerror("Illegal type for argument");
    }
    return "What ?";
}

char *etype(n)
    int n;
{
    int i;
    size_t local_size = 100;
    char *buff = malloc(local_size);

    for (i=0; i < curr_arg_type_size; i++) {
	if (n == 0)
	    break;
	if (curr_arg_types[i] == 0)
	    n--;
    }
    if (i == curr_arg_type_size || !curr_arg_types[i])
	return "0";
    buff[0] = '\0';
    for(; curr_arg_types[i] != 0; i++) {
	char *p;
	if (curr_arg_types[i] == VOID)
	    continue;
	if (buff[0] != '\0')
	    strcat(buff, "|");
	p = etype1(curr_arg_types[i]);
	/*
	 * The number 2 below is to include the zero-byte and the next
	 * '|' (which may not come).
	 */
	if (strlen(p) + strlen(buff) + 2 > local_size) {
	    fprintf(stderr, "Buffer overflow!\n");
	    exit(1);
	}
	strcat(buff, etype1(curr_arg_types[i]));
    }
    return buff;
}

char *ctype(n)
    int n;
{
    static char buff[100];	/* 100 is such a comfortable size :-) */
    char *p;

    buff[0] = '\0';
    if (n & MF_TYPE_MOD_REFERENCE)
	strcat(buff, "TYPE_MOD_REFERENCE|");
    if (n & MF_TYPE_MOD_POINTER)
	strcat(buff, "TYPE_MOD_POINTER|");
    n &= ~(MF_TYPE_MOD_REFERENCE|MF_TYPE_MOD_POINTER);
    switch(n) {
      case VOID:    p = "TYPE_VOID";    break;
      case STRING:  p = "TYPE_STRING";  break;
      case INT:     p = "TYPE_NUMBER";  break;
      case OBJECT:  p = "TYPE_OBJECT";  break;
      case MAPPING: p = "TYPE_MAPPING"; break;
      case FLOAT:   p = "TYPE_FLOAT";   break;
      case CLOSURE: p = "TYPE_CLOSURE"; break;
      case SYMBOL:  p = "TYPE_SYMBOL";  break;
      case MIXED:   p = "TYPE_ANY";     break;
      case UNKNOWN: p = "TYPE_UNKNOWN"; break;
      case QUOTED_ARRAY:
	p = "TYPE_QUOTED_ARRAY"; break;
    default: yyerror("Bad type!"); return 0;
    }
    strcat(buff, p);
    if (strlen(buff) + 1 > sizeof buff)
	fatal("Local buffer overwritten in ctype()");
    return buff;
}
