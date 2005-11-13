#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>

#include "lint.h"

#ifdef OS2
#include <io.h>
#endif

#include "interpret.h" /* we have to place it before lang.h
			  to define struct svalue */
#include "lang.h"
#include "string.h"
#include "config.h"
#include "exec.h"
#include "lex.h"
#include "instrs.h"
#include "patchlevel.h"
#include "stralloc.h"

#ifdef AMIGA
#include "hosts/amiga/socket.h"
#endif

#if defined(hpux) && !defined(__GNUC__)
/* This compilers handling of (char) is broken */
#define CHAR_EOF EOF
#else
#define CHAR_EOF ((char)EOF)
#endif

int current_line;
int total_lines;	/* Used to compute average compiled lines/s */
char *current_file;
int pragma_strict_types;	/* Force usage of strict types. */
int pragma_save_types;		/* Save argument types after compilation */
int pragma_combine_strings;	/* perform addition of constant strings at
				 * compile time.			 */
int pragma_verbose_errors;	/* give info on the context of an error */
char *last_lex_string;
struct lpc_predef_s *lpc_predefs=NULL;
static INLINE int number PROT((int));
static INLINE int string PROT((char *));
static void handle_define PROT((char *, int));
static void add_define PROT((char *, int, char *));
#ifdef __GNUC__ /* old gcc versions, like on a NeXT, want it like this... */
static void add_permanent_define PROT((char *, int, char *, int));
#else /* other compilers, like DICE, insist on this... */
static void add_permanent_define PROT((char *, int, char *, char));
#endif
static int expand_define PROT((void));
static int _expand_define PROT((struct defn*));
static void add_input PROT((char *));
static INLINE void myungetc PROT((int));
static int cond_get_exp PROT((int, struct svalue *));
static int exgetc();
static char *get_current_file(), *get_current_line(), *get_version(),
	*get_hostname(), *get_domainname();
static void efun_defined PROT((char **));
extern char *get_host_ip_number();
static int yyin_des;
static char *linebufstart;
static char *linebufend;
static char saved_char; /* the one that gets overwritten at the start of
			 * the last line in the buffer by  the '\0'	.*/
static int lex_fatal;
static struct svalue *inc_list;
static int inc_list_size;
static mp_int inc_list_maxlen;
static char *auto_include_string = (char *)0;
static int auto_include_start;

#define EXPANDMAX 25000
static int nexpands;

#ifndef tolower
extern int tolower PROT((int));
#endif

static void lexerror PROT((char *));

#define MAXLINE 2048
#define MAX_ANSI_CONCAT 4096
static char yytext[MAXLINE];

#define INC_OPEN_BUFSIZE 1024

static struct ident *lookup_define();

static struct ifstate {
    struct ifstate *next;
    int state;
} *iftop = 0;

#define EXPECT_ELSE 1
#define EXPECT_ENDIF 2

static struct incstate {
    struct incstate *next;
    int yyin_des;
    int line;
    char *file;
    long linebufoffset;
    int pragma_strict_types;
    char saved_char;
} *inctop = 0;

#ifndef DEFMAX
#define DEFMAX 12000
#endif

#define DEFBUF_1STLEN (DEFMAX+MAXLINE+1)/* allow DEFMAX + an input line + 0 */
#define MAX_TOTAL_BUF 400000
static char *defbuf = 0;
static unsigned long defbuf_len = 0;
static char *outp;

static struct s_reswords reswords[] = {
{ "break",		F_BREAK, },
{ "case",		F_CASE, },
{ "catch",		F_CATCH, },
{ "closure",		F_CLOSURE_DECL, },
{ "continue",		F_CONTINUE, },
{ "default",		F_DEFAULT, },
{ "do",			F_DO, },
{ "else",		F_ELSE, },
{ "float",		F_FLOAT_DECL, },
{ "for",		F_FOR, },
{ "if",			F_IF, },
{ "inherit",		F_INHERIT, },
{ "int",		F_INT, },
{ "mapping",		F_MAPPING, },
{ "mixed",		F_MIXED, },
{ "nomask",		F_NO_MASK, },
{ "object",		F_OBJECT, },
#ifdef SUPPLY_PARSE_COMMAND
{ "parse_command",	F_PARSE_COMMAND, },
#endif
{ "private",		F_PRIVATE, },
{ "protected",		F_PROTECTED, },
{ "public",		F_PUBLIC, },
{ "return",		F_RETURN, },
{ "sscanf",		F_SSCANF, },
{ "static",		F_STATIC, },
{ "status",		F_STATUS, },
{ "string",		F_STRING_DECL, },
{ "switch",		F_SWITCH, },
{ "symbol",		F_SYMBOL_DECL, },
{ "varargs",		F_VARARGS, },
{ "virtual",		F_VIRTUAL, },
{ "void",		F_VOID, },
{ "while",		F_WHILE, },
};

struct ident *ident_table[ITABLE_SIZE];
#if ITABLE_SIZE == 256
#define identhash(s) chashstr((s), 12)
#else
#define identhash(s) (whashstr((s), 12) % ITABLE_SIZE)
#endif

/*
 * The number of arguments stated below, are used by the compiler.
 * If min == max, then no information has to be coded about the
 * actual number of arguments. Otherwise, the actual number of arguments
 * will be stored in the byte after the instruction.
 * A maximum value of -1 means unlimited maximum value.
 *
 * If an argument has type 0 (T_INVALID) specified, then no checks will
 * be done at run time.
 *
 * The argument types are checked by the compiler if type checking is enabled,
 * and always at runtime.
 */
#include "efun_defs.c"

struct ident *make_shared_identifier(s, n)
char * s;
int n;
{
    struct ident *curr, *prev;
    int h;
    char *str;

#if defined(LEXDEBUG)
    printf("make_shared_identifier called:%s\n",s);
#endif
    h = identhash(s);

    curr = ident_table[h];
    prev = 0;
    while (curr) {
#if defined(LEXDEBUG)
	printf("checking %s.\n", curr->name);
#endif
	if (!strcmp(curr->name, s)) { /* found it */
#if defined(LEXDEBUG)
	    printf("found.\n");
#endif
	    if (prev) { /* not at head of list */
		prev->next = curr->next;
		curr->next = ident_table[h];
		ident_table[h] = curr;
	    }
	    if (n > curr->type) {
	        struct ident *inferior=curr;

#if defined(LEXDEBUG)
    		printf("shifting down inferior.\n");
#endif
    		if (curr = (struct ident*)xalloc(sizeof *curr)) {
		    curr->name = inferior->name;
    		    curr->next = inferior->next;
    		    curr->type = I_TYPE_UNKNOWN;
    		    curr->inferior = inferior;
    		    curr->hash = h;
		    ident_table[h] = curr;
		}
	    }
	    return curr;
	}
	prev = curr;
	curr = curr->next;
    } /* not found, create new one */
    str = make_shared_string(s);
    if (!str) return 0;
    curr = (struct ident*)xalloc(sizeof *curr);
    if (!curr) {
	free_string(str);
	return 0;
    }
    curr->name = str;
    curr->next = ident_table[h];
    curr->type = I_TYPE_UNKNOWN;
    curr->inferior = 0;
    curr->hash = h;
    ident_table[h] = curr;
    return curr;
}

void free_shared_identifier(p)
struct ident * p;
{
    struct ident *curr, **q;
    int h;
    char *s;

    h = p->hash;

    q = &ident_table[h];
    curr = *q;
    s = p->name;
#if defined(LEXDEBUG)
printf("freeing '%s'\n",s);
fflush(stdout);
#endif
#ifdef DEBUG
    while (curr) {
	if (!strcmp(curr->name, s)) {	/* found matching name */
#else
    for(;;) {
	if (curr->name == s) {		/* found matching name */
#endif
	    struct ident *first = curr;

#ifdef DEBUG
	    while (curr) {
#else
    	    for(;;) {
#endif
	        if (curr == p) { /* this is the right one */
	            if (first == curr) {
	                if (curr->inferior) {
	                    curr->inferior->next = curr->next;
	                    *q = curr->inferior;
	            	    xfree((char *)curr);
	            	    return; /* success */
	                } else {
	                    *q = curr->next;
	                    free_string(curr->name);
	            	    xfree((char *)curr);
	            	    return; /* success */
	            	}
	            } else {
	                *q = curr->inferior;
	                xfree((char *)curr);
	                return; /* success */
	            }
	        }
	        q = &curr->inferior;
	        curr = *q;
	    }
#ifdef DEBUG
    	    fatal("free_shared_identifier: type not found!\n");
#endif
	}
	q = &curr->next;
	curr = *q;
    } /* not found */
#ifdef DEBUG
    fatal("free_shared_identifier: name not found!\n");
#endif
}

void merge(name, namelen, deststart)
    char *name, *deststart;
    mp_int namelen;
{
    char *from, *dest;

    from = name;
    if (*from == '/') {
	/* absolute path */
	dest = deststart;
	do from++; while (*from == '/');
    } else {
	char *cp, *dp;

	dest = (dp = deststart) - 1;
	for (cp = current_file; *cp; *dp++ = *cp++) {
	    if (*cp == '/')
		dest = dp;
	}
	dest++;
    }
    if ((dest - deststart) + namelen >= INC_OPEN_BUFSIZE) {
	*deststart = 0;
	return;
    }

    for (;;) {
	if (*from == '.') {
	    if (from[1] == '.' && from[2] == '/') {
		if (dest == deststart) {
		    /* including from above mudlib is NOT allowed */
		    *deststart = 0;
		    return;
		}
		for (--dest;;) {
		    if (*--dest == '/') {
			dest++;
			break;
		    }
		    if (dest == deststart)
			break;
		}
		from += 3;
		continue;
	    } else if (from[1] == '/') {
		from += 2;
		continue;
	    }
	}
	{
	    char c;

	    do {
		c = *from++;
		*dest++ = c;
		if (!c) return;
	    } while (c != '/');
	    while (*from == '/')
		from++;
	}
    }
}

#if 1 && defined(atarist) && defined(__GNUC_INLINE__) /* this is a speed hack */
/* always read '\r', this doesn't matter. */
#undef getc
#define CR_IN_INPUT
#define getc(__fp) \
	( (--__fp->_cnt >= 0) ? (*__fp->_ptr++) : _filbuf(__fp) )
#endif

#define myfilbuf() (*outp?0:_myfilbuf())

static /* NO inline */
char *
_myfilbuf()
{
    int i;
    char *p;

    *outp = saved_char;
    if (linebufend - outp)
	memcpy(outp-MAXLINE, outp, linebufend  - outp);
    outp -= MAXLINE;
    *(outp-1) = '\n';
    p = linebufstart; /* == linebufend - MAXLINE */
    i = read(yyin_des, p, MAXLINE);
    if (i < MAXLINE) {
        if (i < 0) {
            i = 0;
        }
    	p += i;
	if (p - outp ? p[-1] != '\n' : current_line == 1)
            *p++ = '\n';
        *p++ = EOF;
        return outp;
    }
    p += i;
    while (*--p != '\n'); /* find last newline */
    if (p < linebufstart) {
	lexerror("line too long");
	*(p = linebufend-1) = '\n';
    }
    p++;
    saved_char = *p;
    *p = 0;
    return outp;
}

static INLINE char
mygetc()
{
#if 0
    fprintf(stderr, "c='%c' %x", *outp, *outp);
#endif
#if defined(LEXDEBUG)
    putc(*outp, stderr);
#if 1
    fflush(stdout);
#endif
#endif
    return *outp++;
}

static INLINE void
myungetc(c)
int c;
{
    *--outp = c;
}

static INLINE int
gobble(c)
char c;
{
    if (c ==  mygetc())
	return 1;
    --outp;
    return 0;
}

static void
lexerror(s)
char *s;
{
    yyerror(s);
    lex_fatal++;
}

static int
skip_to(token, atoken)
char *token, *atoken;
{
    char *p, *q;
    char c;
    char nl = '\n';
    int nest;

    p = outp;
    for(nest = 0;;) {
#if 0 /* we don't compile code here */
	store_line_number_info();
#endif
	current_line++;
	total_lines++;
	c = *p++;
	if (c == '#') {
	    while(lexwhite(*p++));
	    q = --p;
	    while (isalunum(*p++));
	    c = *--p;
	    *p = 0;
	    if (c != nl) {
		while (*++p != nl);
	    }
	    p++;
/*fprintf(stderr, "skip checks %s\n", q);*/
	    if (strcmp(q, "if") == 0 || strcmp(q, "ifdef") == 0 ||
		strcmp(q, "ifndef") == 0) {
		nest++;
	    } else if (nest > 0) {
		if (strcmp(q, "endif") == 0)
		    nest--;
	    } else {
		if (strcmp(q, token) == 0) {
		    *(p-1) = nl;
	    	    outp = p;
		    if (!*p) {
	    		_myfilbuf();
	    	    }
		    return 1;
		} else if (atoken) {
		    if (strcmp(q, atoken) == 0) {
			*(p-1) = nl;
			outp = p;
			if (!*p) {
			    _myfilbuf();
			}
			return 0;
		    } else if (strcmp(q, "elif") == 0) {
			current_line--;
			total_lines--;
			q[0] = nl;
			q[1] = '#';
			q[4] = c;
			outp = q+1;
			return 0;
		    }
		}
	    }
	} else {
/*fprintf(stderr, "skipping (%d) %c", c, c);*/
            if ( c == CHAR_EOF) {
		outp = p - 2;
		current_line--;
		total_lines--;
		lexerror("Unexpected end of file while skipping");
		return 1;
            }
            while (c != nl) c = *p++;
	}
	if (!*p) {
	    outp = p;
	    p = _myfilbuf();
	}
    }
}

static void
handle_cond(c)
int c;
{
    struct ifstate *p;

/*fprintf(stderr, "cond %d\n", c);*/
    if (c || skip_to("else", "endif")) {
	p = (struct ifstate *)xalloc(sizeof(struct ifstate));
	p->next = iftop;
	iftop = p;
	p->state = c ? EXPECT_ELSE : EXPECT_ENDIF;
    }
}

static int
inc_open(buf, name, namelen, delim)
    char *buf, *name, delim;
    mp_int namelen;
{
    int fd;
    int i;
    char *p;

    if (delim == '"') {
	merge(name, namelen, buf);
	if ((fd = ixopen(buf, O_RDONLY|O_BINARY)) >= 0)
	    return fd;
	if (errno == EMFILE)
	    lexerror("File descriptors exhausted");
#ifdef ENFILE
	if (errno == ENFILE)
	    lexerror("File table overflow");
#endif
    }
    if (closure_hook[H_INCLUDE_DIRS].type == T_POINTER) {
	if (namelen + inc_list_maxlen >= INC_OPEN_BUFSIZE) {
	    yyerror("Include name too long.");
	    return -1;
	}
	/*
	 * Search all include dirs specified.
	 */
	for (p=strchr(name, '.'); p; p = strchr(p+1, '.')) {
	    if (p[1] == '.')
		return -1;
	}
	for (i=0; i < inc_list_size; i++) {
	    sprintf(buf, "%s%s", inc_list[i].u.string, name);
	    fd = ixopen(buf, O_RDONLY|O_BINARY);
	    if (fd >= 0)
		return fd;
	    if (errno == EMFILE) lexerror("File descriptors exhausted");
#if ENFILE
	    if (errno == ENFILE) lexerror("File table overflow");
#endif
	}
    } else if (closure_hook[H_INCLUDE_DIRS].type == T_CLOSURE) {
	extern struct object *current_object;
	struct svalue *svp;

	push_string_malloced(name);
	push_volatile_string(current_file);
	if (closure_hook[H_INCLUDE_DIRS].x.closure_type == CLOSURE_LAMBDA)
	    closure_hook[H_INCLUDE_DIRS].u.lambda->ob = current_object;
	svp = secure_call_lambda(&closure_hook[H_INCLUDE_DIRS], 2);
	if (svp && svp->type == T_STRING &&
	    strlen(svp->u.string) < INC_OPEN_BUFSIZE)
	{
	    strcpy(buf, svp->u.string);
	    if (legal_path(buf)) {
		fd = ixopen(buf, O_RDONLY|O_BINARY);
		if (fd >= 0)
		    return fd;
		if (errno == EMFILE) lexerror("File descriptors exhausted");
#if ENFILE
		if (errno == ENFILE) lexerror("File table overflow");
#endif
	    }
	}
    }
    return -1;
}

void free_defbuf(void) {
    if (defbuf_len) {
	xfree(defbuf);
	defbuf_len = 0;
    }
}

static void realloc_defbuf() {
    char * old_defbuf = defbuf;
    long old_defbuf_len = defbuf_len;
    char * old_outp = outp;

    if (MAX_TOTAL_BUF <= defbuf_len)
      return;
    outp -= &defbuf[defbuf_len] - (char*)0;
    if (defbuf_len > (MAX_TOTAL_BUF >> 1) ) {
	defbuf_len = MAX_TOTAL_BUF;
    } else {
	defbuf_len <<= 1;
    }
    fprintf(stderr, "reallocating defbuf from %ld (%ld left) to %ld.\n"
           , old_defbuf_len, old_outp-defbuf, defbuf_len);
    defbuf = xalloc(defbuf_len);
    memcpy(defbuf+defbuf_len-old_defbuf_len, old_defbuf, old_defbuf_len);
    xfree(old_defbuf);
    outp += &defbuf[defbuf_len] - (char*)0;
}

static INLINE void
handle_include(name)
char *name;
{
    char *p;
    char buf[INC_OPEN_BUFSIZE];
    int fd;
    struct incstate *is;
    int delim;
    long linebufoffset;
    char *old_outp;
    int in_buffer = 0;

/*fprintf(stderr, "handle include '%s'\n", name);*/
#if 0
    if (nbuf) {
	lexerror("Internal preprocessor error");
	return;
    }
#endif
    old_outp = outp;
    while (*name != '"' && *name != '<') {
	char c;
	struct ident *d;

	for (p = name; isalunum(*p); p++);
	c = *p;
	*p = 0;
	d = lookup_define(name);
	*p = c;
	if (in_buffer) {
	    outp = p;
	} else {
	    myungetc('\n');
	    add_input(p);
	    in_buffer = 1;
	}
	if (!d || !_expand_define(&d->u.define) ) {
	    yyerror("Missing leading \" or < in #include");
	    return;
	}
	name = outp;
	while (lexwhite(*name))
	    name++;
    }
    delim = *name++ == '"' ? '"' : '>';
    for(p = name; *p && *p != delim; p++)
	;
    if (!*p) {
	yyerror("Missing trailing \" or > in #include");
	outp = old_outp;
	return;
    }
    *p = 0;
    if (delim == '"') {
	char *q;

	q = p + 1;
	for (;;) {
	    while(lexwhite(*q))
		q++;
	    if (!*q || *q == '\n')
		break;
	    while (*q != delim) {
		char *r, c;
		struct ident *d;

		for (r = q; isalunum(*r); r++);
		c = *r;
		*r = 0;
		d = lookup_define(q);
		*r = c;
		if (in_buffer) {
		    outp = r;
		    if (name != yytext) {
			if ( (p - name) >= MAXLINE - 1) {
			    yyerror("Include name too long.");
			    outp = old_outp;
			    return;
			}
			*p = 0;
			strcpy(yytext, name);
			p += yytext - name;
			name = yytext;
		    }
		} else {
		    myungetc('\n');
		    add_input(r);
		    in_buffer = 1;
		}
		if (!d || !_expand_define(&d->u.define) ) {
		    yyerror("Missing leading \" in #include");
		    outp = old_outp;
		    return;
		}
		q = outp;
		while (lexwhite(*q))
		    q++;
	    }
	    while(*++q && *q != delim) {
		if ( (p - name) >= MAXLINE - 1) {
		    yyerror("Include name too long.");
		    outp = old_outp;
		    return;
		}
		*p++ = *q;
	    }
	    if (!*q++) {
		yyerror("Missing trailing \" in #include");
		outp = old_outp;
		return;
	    }
	}
    }
    outp = old_outp;
    *p = 0;
    linebufoffset = linebufstart - &defbuf[defbuf_len];
    if (outp - defbuf < 3*MAXLINE) {
	realloc_defbuf();
	if (outp - defbuf < 2*MAXLINE) {
	    lexerror("Maximum total buffer size exceeded");
	    return;
	}
    }
    if ((fd = inc_open(buf, name, p - name, delim)) >= 0) {
	store_include_info(name);
	is = (struct incstate *)xalloc(sizeof(struct incstate));
	if (!is) {
	    lexerror("Out of memory");
	    return;
	}
	is->yyin_des = yyin_des;
	is->line = current_line;
	is->file = current_file;
	is->linebufoffset = linebufoffset;
	is->saved_char = saved_char;
	is->next = inctop;
	is->pragma_strict_types = pragma_strict_types;
	current_file = xalloc(strlen(buf)+1);
	if (!current_file) {
	    current_file = is->file;
	    xfree((char*)is);
	    lexerror("Out of memory");
	    return;
	}
	strcpy(current_file, buf);
	pragma_strict_types = 0;
	instrs[F_CALL_OTHER-F_OFFSET].ret_type = TYPE_ANY;
	inctop = is;
	current_line = 1;
	linebufend   = outp - 1; /* allow trailing zero */
	linebufstart = linebufend - MAXLINE;
	*(outp = linebufend) = 0;
	yyin_des = fd;
	_myfilbuf();
/*fprintf(stderr, "pushed to %s\n", buf);*/
    } else {
	sprintf(buf, "Cannot #include %s", name);
	yyerror(buf);
    }
}

static void
skip_comment()
{
    register char c, *p;

    p = outp;
    for(;;) {
	while((c =  *p++) != '*') {
	    if (c == '\n') {
		store_line_number_info();
		nexpands=0;
		if ((c = *p) == CHAR_EOF) {
		    outp = p - 1;
	            lexerror("End of file in a comment");
		    return;
		}
		current_line++;
		if (!c) {
		    outp = p;
		    p = _myfilbuf();
		}
	    }
	}
	do {
	    if ((c = *p++) == '/') {
		outp = p;
		return;
	    }
	    if (c == '\n') {
		store_line_number_info();
		nexpands=0;
		if ((c = *p) == CHAR_EOF) {
		    outp = p - 1;
	            lexerror("End of file in a comment");
		    return;
		}
		current_line++;
		if (!c) {
		    outp = p;
		    p = _myfilbuf();
		}
		c = '\0';
	    }
	} while(c == '*');
    }
}

static char *
skip_pp_comment(p)
    char *p;
{
    char c;

    for (;;) {
	c = *p++;
	if (c == '\n') {
	    store_line_number_info();
	    current_line++;
	    if (p[-2] == '\\') {
		if (!*p) {
		    outp = p;
		    p = _myfilbuf();
		}
		continue;
	    }
	    nexpands=0;
	    if (!*p) {
		outp = p;
		p = _myfilbuf();
	    }
	    return p;
	}
    }
}

#define TRY(c, t) if (*yyp == (c)) {yyp++; outp = yyp; return t;}

static void
deltrail(sp)
char *sp;
{
    char *p;
    p = sp;
    if (!*p) {
	lexerror("Illegal # command");
    } else {
	while(*p && !isspace(*p))
	    p++;
	*p = 0;
    }
}

#define SAVEC \
    if (yyp < yytext+MAXLINE-5)\
       *yyp++ = c;\
    else {\
       lexerror("Line too long");\
       break;\
    }

static void handle_pragma(str)
    char *str;
{
#if defined(LEXDEBUG)
    printf("handle pragma:'%s'\n",str);
#endif
    if (strcmp(str, "strict_types") == 0) {
	pragma_strict_types = 2;
	instrs[F_CALL_OTHER-F_OFFSET].ret_type = TYPE_UNKNOWN;
    } else if (strcmp(str, "save_types") == 0) {
	pragma_save_types = 1;
    } else if (strcmp(str, "combine_strings") == 0) {
	pragma_combine_strings = 1;
    } else if (strcmp(str, "no_combine_strings") == 0) {
	pragma_combine_strings = 0;
    } else if (strcmp(str, "strong_types") == 0) {
	pragma_strict_types = 1;
	instrs[F_CALL_OTHER-F_OFFSET].ret_type = TYPE_ANY;
    } else if (strcmp(str, "verbose_errors") == 0) {
	pragma_verbose_errors = 1;
#if defined( DEBUG ) && defined ( TRACE_CODE )
    } else if (strcmp(str, "set_code_window") == 0) {
	extern void set_code_window();

	set_code_window();
    } else if (strcmp(str, "show_code_window") == 0) {
	extern void show_code_window();

	show_code_window();
#endif
    }
}
static struct ident *all_defines = 0, *permanent_defines = 0,
	*undefined_permanent_defines = 0;

static INLINE int number(i)
    int i;
{
#ifdef LEXDEBUG
    printf("returning number %d.\n", i);
#endif
    yylval.number = i;
    return F_NUMBER;
}

static void add_lex_string(str)
    char *str;
{
    mp_int len1, len3;
    char *tmp;

    len1 = strlen(last_lex_string);
    len3 = len1 + strlen(str) + 1;
    if (len3 > MAX_ANSI_CONCAT) {
	/* Without this test, compilation would still terminate eventually,
	 * thus it would still be 'correct', but it could take several hours.
	 */
	lexerror("Too long ansi style string concatenation");
	/* leave the old string, ignore the new addition */
	return;
    }
    tmp = alloca(len3);
    strcpy(tmp, last_lex_string);
    strcpy(tmp + len1, str);
    free_string(last_lex_string);
    last_lex_string = make_shared_string(tmp);
}

int yylex();

static INLINE int string(str)
    char *str;
{
    if (last_lex_string) {
	add_lex_string(str);
	return yylex();
    } else {
	last_lex_string = make_shared_string(str);
    }
    return F_STRING;
}

static INLINE int
yylex1()
{
  register char *yyp;
  register char c;

  yyp = outp;
#if 0
  if (lex_fatal) {
    return -1;
  }
#endif
  for(;;) {
    switch(c = *yyp++) {
    case CHAR_EOF:
	if (inctop) {
	    static char call_other_return_types[] =
		{ TYPE_ANY, TYPE_ANY, TYPE_UNKNOWN };
	    struct incstate *p;
	    p = inctop;
	    close(yyin_des);
/*fprintf(stderr, "popping to %s\n", p->file);*/
	    xfree(current_file);
	    nexpands=0;
	    current_file = p->file;
	    current_line = p->line + 1;
	    pragma_strict_types = p->pragma_strict_types;
	    instrs[F_CALL_OTHER-F_OFFSET].ret_type = 
		call_other_return_types[pragma_strict_types];
	    yyin_des = p->yyin_des;
	    saved_char = p->saved_char;
	    inctop = p->next;
	    *linebufend = '\n';
	    yyp = linebufend + 1;
	    linebufstart = &defbuf[defbuf_len] + p->linebufoffset;
	    linebufend   = linebufstart + MAXLINE;
	    xfree((char *)p);
	    if (!*yyp) {
	        outp = yyp;
	        yyp = _myfilbuf();
	    }
	    store_include_end();
	    break;
	}
	if (iftop) {
	    struct ifstate *p = iftop;
	    yyerror(p->state == EXPECT_ENDIF ? "Missing #endif" : "Missing #else");
	    while(iftop) {
		p = iftop;
		iftop = p->next;
		xfree((char *)p);
	    }
	}
	outp = yyp-1;
	return -1;
    case '\n':
	{
	    store_line_number_info();
	    nexpands=0;
	    current_line++;
	    total_lines++;
	    if (!*yyp) {
	        outp = yyp;
	        yyp = _myfilbuf();
	    }
	}
	    break;
    case 0x1a: /* Used by some MSDOS editors as EOF */
    case '\r':
        *(yyp-1) = *(yyp-2);
        break;
    case ' ':
    case '\t':
    case '\f':
    case '\v':
	break;
    case '+':
	switch(c=*yyp++) {
	case '+': outp=yyp;
		  return F_INC;
	case '=': yylval.number = F_ADD_EQ-F_OFFSET;
		  outp=yyp;
		  return F_ASSIGN;
	default:  yyp--;
	}
	outp = yyp;
	return '+';
    case '-':
    	switch(c=*yyp++) {
    	case '>': outp=yyp;
    		  return F_ARROW;
    	case '-': outp=yyp;
    		  return F_DEC;
	case '=': yylval.number = F_SUB_EQ-F_OFFSET;
		  outp=yyp;
		  return F_ASSIGN;
	default:  yyp--;
	}
	outp = yyp;
	return '-';
    case '&':
    	switch(c=*yyp++) {
    	case '&': outp=yyp;
    		  return F_LAND;
	case '=': yylval.number = F_AND_EQ-F_OFFSET;
		  outp=yyp;
		  return F_ASSIGN;
	default:  yyp--;
	}
	outp = yyp;
	return '&';
    case '|':
    	switch(c=*yyp++) {
    	case '|': outp=yyp;
    		  return F_LOR;
	case '=': yylval.number = F_OR_EQ-F_OFFSET;
		  outp=yyp;
		  return F_ASSIGN;
	default:  yyp--;
	}
	outp = yyp;
	return '|';
    case '^':
	if (*yyp == '=') {
	    yyp++;
	    yylval.number = F_XOR_EQ-F_OFFSET;
	    outp=yyp;
	    return F_ASSIGN;
	}
	outp = yyp;
	return '^';
    case '<':
	c = *yyp++;;
	if (c == '<') {
	    if (*yyp == '=') {
	        yyp++;
	        yylval.number = F_LSH_EQ-F_OFFSET;
	        outp=yyp;
	        return F_ASSIGN;
	    }
	    outp=yyp;
	    return F_LSH;
	}
	if (c == '=') {
	    outp=yyp;
	    return F_LE;
	}
	yyp--;
	outp=yyp;
	return '<';
    case '>':
	c = *yyp++;
	if (c == '>') {
	    if (*yyp == '=') {
	        yyp++;
	        yylval.number = F_RSH_EQ-F_OFFSET;
	        outp=yyp;
	        return F_ASSIGN;
	    }
	    outp=yyp;
	    return F_RSH;
	}
	if (c == '=') {
	    outp=yyp;
	    return F_GE;
	}
	yyp--;
	outp=yyp;
	return '>';
    case '*':
	if (*yyp == '=') {
	    yyp++;
	    yylval.number = F_MULT_EQ-F_OFFSET;
	    outp=yyp;
	    return F_ASSIGN;
	}
	outp=yyp;
	return '*';
    case '%':
	if (*yyp == '=') {
	    yyp++;
	    yylval.number = F_MOD_EQ-F_OFFSET;
	    outp=yyp;
	    return F_ASSIGN;
	}
	outp=yyp;
	return '%';
    case '/':
	c = *yyp++;
	if (c == '*') {
	    outp=yyp;
	    skip_comment();
	    yyp=outp;
    	    if (lex_fatal) {
		return -1;
    	    }
	    break;
	}
	if (c == '/') {
	    yyp = skip_pp_comment(yyp);
	    break;
	}
	if (c == '=') {
	    yylval.number = F_DIV_EQ-F_OFFSET;
	    outp=yyp;
	    return F_ASSIGN;
	}
	yyp--;
	outp=yyp;
	return '/';
    case '=':
	TRY('=', F_EQ);
	yylval.number = F_ASSIGN-F_OFFSET;
	outp=yyp;
	return F_ASSIGN;
    case ';':
    case '(':
    case ')':
    case ',':
    case '{':
    case '}':
    case '~':
    case '[':
    case ']':
    case '?':
	outp=yyp;
	return c;
    case '!':
	TRY('=', F_NE);
	outp=yyp;
	return F_NOT;
    case ':':
	TRY(':', F_COLON_COLON);
	outp=yyp;
	return ':';
    case '.':
	TRY('.',F_RANGE);
	goto badlex;
    case '#':
	if (*yyp == '\'') {
	    extern struct function *simul_efunp;
	    extern struct object *master_ob;

	    struct ident *p;
	    char *wordstart = ++yyp;
	    int efun_override;

	    do c=*yyp++;
	    while (isalunum(c));
	    c = *--yyp;
	    /* the assignment is good for the data flow analysis :-} */
	    if (yyp == wordstart) {
		extern int symbol_operator PROT((char *, char **));
		int i;

		if ((i = symbol_operator(yyp, &outp)) < 0)
		    yyerror("Missing function name after #'");
		yylval.closure.number = i + CLOSURE_EFUN_OFFS;
		return F_CLOSURE;
	    }
	    efun_override = 0;
	    if (yyp - wordstart == 4 && !strncmp(wordstart, "efun::", 6) ) {
		efun_override = 1;
		wordstart = yyp += 2;
		do c=*yyp++;
		while (isalunum(c));
		c = *--yyp;
	    }
	    outp = yyp;
	    *yyp = 0;
	    p = make_shared_identifier(wordstart, I_TYPE_GLOBAL);
	    *yyp = c;
	    if (!p) {
		lexerror("Out of memory");
		return 0;
	    }
	    while (p->type > I_TYPE_GLOBAL) {
		if (p->type == I_TYPE_RESWORD) {
		    int code;

		    switch(code = p->u.code) {
		      default:
			/* There aren't efuns with reswords as names, and
			 * it is impossible to define local / global vars
			 * or functions with such a name. Thus, !p->inferior .
			 */
			yyerrorf(
			  "No closure associated with reserved word '%s'",
			  p->name
			);
			code = CLOSURE_EFUN_OFFS;
			break;
		      case F_IF:
			code = F_BRANCH_WHEN_ZERO-F_OFFSET+CLOSURE_EFUN_OFFS;
			break;
		      case F_DO:
			code =
			  F_BBRANCH_WHEN_NON_ZERO-F_OFFSET+CLOSURE_EFUN_OFFS;
			break;
		      case F_WHILE:
			/* the politically correct code   /
			/  was already taken, see above. */
			code = F_BBRANCH_WHEN_ZERO-F_OFFSET+CLOSURE_EFUN_OFFS;
			break;
		      case F_CONTINUE:
			code = F_BRANCH-F_OFFSET+CLOSURE_EFUN_OFFS;
			break;
		      case F_DEFAULT:
			code = F_CSTRING0-F_OFFSET+CLOSURE_EFUN_OFFS;
			/* as bogus as we can possibliy get :-) */
			break;
		      case F_BREAK:
		      case F_RETURN:
		      case F_SSCANF:
		      case F_CATCH:
		      case F_SWITCH:
			code += -F_OFFSET + CLOSURE_EFUN_OFFS;
			break;
		    }
		    yylval.closure.number = code;
		    return F_CLOSURE;
		}
		if ( !(p = p->inferior) )
		    break;
	    }
	    if (!p || p->type < I_TYPE_GLOBAL) {
		if (p && p->type == I_TYPE_UNKNOWN)
		    free_shared_identifier(p);
		c = *yyp;
		*yyp = 0;
		yyerrorf("Undefined function: %s", wordstart);
		*yyp = c;
		yylval.closure.number = CLOSURE_EFUN_OFFS;
		return F_CLOSURE;
	    }
	    if (efun_override && p->u.global.sim_efun >= 0 &&
		simul_efunp[p->u.global.sim_efun].flags & TYPE_MOD_NO_MASK &&
		p->u.global.efun >= 0 &&
		master_ob)
	    {
		struct svalue *res;

		push_constant_string("nomask simul_efun");
		push_volatile_string(current_file);
		push_shared_string(p->name);
		res = apply_master_ob("privilege_violation", 3);
		if (!res || res->type != T_NUMBER || res->u.number < 0)
		{
		    yyerrorf(
		      "Privilege violation: nomask simul_efun %s",
		      p->name
		    );
		    efun_override = 0;
		} else if (!res->u.number) {
		    efun_override = 0;
		}
	    }
	    switch(0) { default:
		if (!efun_override) {
		    if (p->u.global.function >= 0) {
			int i;

			i = p->u.global.function;
			yylval.closure.number = i;
			if (i >= CLOSURE_IDENTIFIER_OFFS)
			    yyerrorf(
			      "Too high function index of %s for #'",
			      p->name
			    );
			break;
		    }
		    if (p->u.global.sim_efun >= 0) {
			yylval.closure.number =
			  p->u.global.sim_efun + CLOSURE_SIMUL_EFUN_OFFS;
			break;
		    }
		}
		if (p->u.global.efun >= 0) {
		    yylval.closure.number =
		      p->u.global.efun + CLOSURE_EFUN_OFFS;
		    if (yylval.closure.number >
			LAST_INSTRUCTION_CODE + CLOSURE_EFUN_OFFS)
		    {
			yylval.closure.number =
			  efun_aliases[
			    yylval.closure.number - CLOSURE_EFUN_OFFS
			      - LAST_INSTRUCTION_CODE - 1
			  ] + CLOSURE_EFUN_OFFS;
		    }
		    break;
		}
		if (p->u.global.variable >= 0) {
		    extern int num_virtual_variables;

		    if (p->u.global.variable & VIRTUAL_VAR_TAG) {
			/* Handling this would require an extra coding of
			 * this closure type, and special treatment in
			 * replace_program_lambda_adjust() .
			 */
			yyerrorf("closure of virtual variable");
			yylval.closure.number = CLOSURE_IDENTIFIER_OFFS;
			break;
		    }
		    yylval.closure.number =
		      p->u.global.variable + num_virtual_variables +
		      CLOSURE_IDENTIFIER_OFFS;
		    break;
		}
		c = *yyp;
		*yyp = 0;
		yyerrorf("Undefined function: %s", wordstart);
		*yyp = c;
		yylval.closure.number = CLOSURE_EFUN_OFFS;
		break;
	    }
	    return F_CLOSURE;
	} else if (*(yyp-2) == '\n' && !nexpands) {
	    char *sp = 0;
	    int quote, last;

	    outp=yyp;
	    yyp = yytext;
	    do {
		c = mygetc();
	    } while (lexwhite(c));
	    for(quote = 0, last = 0;;) {

		while(!quote && c == '/') { /*gc - handle comments cpp-like! 1.6.91 @@@*/
		    char c2;

		    if ( (c2 = mygetc()) == '*') {
			skip_comment();
			c=mygetc();
		    } else if (c2 == '/') {
			outp = skip_pp_comment(outp);
			current_line--;
			c = '\n';
		    } else {
			--outp;
			break;
		    }
		}

		if (last == '\\')
		    last = 0;
		else if (c == '"')
		    quote ^= 1;
		else
		    last = c;

		if (!sp && lexwhite(c))
		    sp = yyp;
		if (c == '\n') {
		    break;
		}
		SAVEC;
		c = mygetc();
	    }
	    if (sp) {
		*sp++ = 0;
		while(lexwhite(*sp))
		    sp++;
	    } else {
		sp = yyp;
	    }
	    *yyp = 0;
	    if (strcmp("include", yytext) == 0) {
                handle_include(sp);
	    } else {myfilbuf();if (strcmp("define", yytext) == 0) {
		handle_define(sp, quote);
	    } else if (strcmp("if", yytext) == 0) {
		int cond;
		struct svalue sv;

		myungetc('\n');
		add_input(sp);
		cond = cond_get_exp(0, &sv);
		free_svalue(&sv);
		if (mygetc() != '\n') {
		    yyerror("Condition too complex in #if");
		    while (mygetc() != '\n');
		} else
		    handle_cond(cond);
	    } else if (strcmp("ifdef", yytext) == 0) {
		deltrail(sp);
		handle_cond(lookup_define(sp) != 0);
	    } else if (strcmp("ifndef", yytext) == 0) {
		deltrail(sp);
		handle_cond(lookup_define(sp) == 0);
	    } else if (*yytext == 'e' && yytext[1] == 'l' &&
			( (yytext[2] == 's' && yytext[3] == 'e') ||
			  (yytext[2] == 'i' && yytext[3] == 'f') ) &&
			!yytext[4])
		{
		if (iftop && iftop->state == EXPECT_ELSE) {
		    struct ifstate *p = iftop;

/*fprintf(stderr, "found else\n");*/
		    iftop = p->next;
		    xfree((char *)p);
		    skip_to("endif", (char *)0);
		} else {
		    yyerror("Unexpected #else");
		}
	    } else if (strcmp("endif", yytext) == 0) {
		if (iftop && (iftop->state == EXPECT_ENDIF ||
			      iftop->state == EXPECT_ELSE)) {
		    struct ifstate *p = iftop;

/*fprintf(stderr, "found endif\n");*/
		    iftop = p->next;
		    xfree((char *)p);
		} else {
		    yyerror("Unexpected #endif");
		}
	    } else if (strcmp("undef", yytext) == 0) {
		struct ident *p, **q;
		int h;

		deltrail(sp);
#if 0
    fprintf(stderr, "#undef '%s'\n", sp);
#endif
    		h = identhash(sp);
		for(q = &ident_table[h]; p=*q; q=&p->next) {
		    if (strcmp(sp, p->name)) continue;
		    if (p->type != I_TYPE_DEFINE) break; /* failure */
		    if (!p->u.define.permanent) {
#if defined(LEXDEBUG)
			fprintf(stderr, 
			  "#undef define '%s' %d '%s'\n",
			  p->name,
			  p->u.define.nargs,
			  p->u.define.exps.str);
			fflush(stderr);
#endif
			if (p->inferior) {
		            p->inferior->next = p->next;
		            *q = p->inferior;
			} else {
			    *q = p->next;
		    	    free_string(p->name);
			}
			xfree(p->u.define.exps.str);
			p->name = 0; /* mark for later freeing by all_defines */
			/* success */
			break;
		    } else {
			if (p->inferior) {
		            p->inferior->next = p->next;
		            *q = p->inferior;
			    increment_string_ref(p->name);
			} else {
			    *q = p->next;
			}
			p->next = undefined_permanent_defines;
			undefined_permanent_defines = p;
			/* success */
			break;
		    }
		}
	    } else if (strcmp("echo", yytext) == 0) {
		fprintf(stderr, "%s\n", sp);
	    } else if (strcmp("pragma", yytext) == 0) {
		deltrail(sp);
		handle_pragma(sp);
	    } else {
		yyerror("Unrecognised # directive");
	    }}
	    store_line_number_info();
	    nexpands=0;
	    current_line++;
	    total_lines++;
	    yyp = outp;
    	    if (lex_fatal) {
		return -1;
    	    }
	    break;
	} else
	    goto badlex;
    case '\'':
	c = *yyp++;
	if (c == '\\') {
	    switch(c = *yyp++) {
		case '\n':
		case CHAR_EOF:
		    yyp--; /* this will be noted as error below */
		    break;
		case 'a': c = '\007'; break;
		case 'b': c = '\b';   break;
		case 'e': c = '\033'; break;
		case 'f': c = '\014'; break;
		case 'n': c = '\n';   break;
		case 'r': c = '\r';   break;
		case 't': c = '\t';   break;
	    }
	    if (*yyp++ != '\'') {
		yyp--;
		yyerror("Illegal character constant");
	    }
	} else if (*yyp++ != '\'' ||
		   c == '\'' &&
			(*yyp == '(' || isalunum(*yyp) || *yyp == '\'') )
	{
	    char *wordstart;
	    int quotes = 1;

	    yyp -= 2;
	    while (*yyp == '\'') {
		quotes++;
		yyp++;
	    }
	    wordstart = yyp;
	    if (!isalpha(*yyp)) {
		if (*yyp == '(' && yyp[1] == '{') {
		    outp = yyp + 2;
		    yylval.number = quotes;
		    return F_QUOTED_AGGREGATE;
		}
		yyerror("Illegal character constant");
		outp = yyp;
		return F_NUMBER;
	    }
	    while(isalunum(*++yyp));
	    c = *yyp;
	    *yyp = 0;
	    yylval.symbol.name = make_shared_string(wordstart);
	    *yyp = c;
	    yylval.symbol.quotes = quotes;
	    outp = yyp;
	    return F_SYMBOL;
	}
	yylval.number = c;
	outp = yyp;
	return F_NUMBER;
    case '"':
    {
	char *p = yyp;

	yyp = yytext;
	for(;;) {
	    c = *p++;
	    if (c == '\n') {
 		outp = p-1;
 		/* myfilbuf(); not needed */
 		lexerror("Newline in string");
		return string("");
	    }
	    SAVEC;
	    if (c == '"') {
		*--yyp = 0;
		break;
	    }
	    if (c == '\\') {
		yyp--;
		switch(c = *p++) {
		case '\r':
		    if (*p++ != '\n') {
			p--;
			*yyp++ = c;
			break;
		    }
		case '\n':
		    store_line_number_info();
		    current_line++;
		    total_lines++;
		    if (*p == CHAR_EOF ) {
			outp = p;
 		        lexerror("End of file in string");
		        return string("");
		    }
		    if (!*p) {
		        outp = p;
		        p = _myfilbuf();
		    }
		    if (*p++ != '\r') p--;
		    break;
		case 'a': *yyp++ = '\007'; break;
		case 'b': *yyp++ = '\b';   break;
		case 'e': *yyp++ = '\033'; break;
		case 'f': *yyp++ = '\014'; break;
		case 'n': *yyp++ = '\n'; break;
		case 'r': *yyp++ = '\r'; break;
		case 't': *yyp++ = '\t'; break;
		default : *yyp++ = c;
		}
	    }
	}
	outp = p;
	return string(yytext);

    }
    case '0':
	c = *yyp++;
	if ( c == 'X' || c == 'x' ) {
	    unsigned long l;

	    /* strtol() gets the sign bit wrong,
	       strtoul() isn't portable enough. */
	    l = 0;
	    --yyp;
	    while(leXdigit(c = *++yyp)) {
		if (c > '9')
		    c = (c & 0xf) + ( '9' + 1 - ('a' & 0xf) );
		l <<= 4;
		l += c - '0';
	    }
	    outp=yyp;
	    return number(l);
	}
	yyp--;
	if (!lexdigit(c) && (c != '.' || yyp[1] == '.') ) {
	    outp=yyp;
	    return number(0);
	}
	c = '0';
	/* fall through */
             case '1':case '2':case '3':case '4':
    case '5':case '6':case '7':case '8':case '9':
    {
	char *numstart=yyp;
	long l;

	l = c - '0';
	while(lexdigit(c = *yyp++)) l = (((l << 2)+l) << 1) + 
#if defined(atarist) || defined(sun) || defined(MSDOS)
/* everybody with ascii is invited to join in... */
		(c & 0xf ); /* can be done in the same step as the type conversion */
#else
		(c - '0');
#endif

#ifdef FLOATS
	if (c == '.' && *yyp != '.') {
	    while(lexdigit(*yyp++));
	    c = *--yyp;
	    *yyp = 0;
	    yylval.float_number = atof(numstart-1);
	    *yyp = c;
	    outp=yyp;
	    return F_FLOAT;
	}
#endif /* FLOATS */
	--yyp;
	outp = yyp;
	return number(l);
    }
    case 'A':case 'B':case 'C':case 'D':case 'E':case 'F':case 'G':case 'H':
    case 'I':case 'J':case 'K':case 'L':case 'M':case 'N':case 'O':case 'P':
    case 'Q':case 'R':case 'S':case 'T':case 'U':case 'V':case 'W':case 'X':
    case 'Y':case 'Z':case 'a':case 'b':case 'c':case 'd':case 'e':case 'f':
    case 'g':case 'h':case 'i':case 'j':case 'k':case 'l':case 'm':case 'n':
    case 'o':case 'p':case 'q':case 'r':case 's':case 't':case 'u':case 'v':
    case 'w':case 'x':case 'y':case 'z':case '_':
    	{
	    struct ident *p;
	    char *wordstart = yyp-1;

	    do c=*yyp++;
	    while (isalunum(c));
	    c = *--yyp;
	    /* the assignment is good for the data flow analysis :-} */
	    *yyp = 0;

	    p = make_shared_identifier(wordstart, I_TYPE_UNKNOWN);
	    *yyp = c;
	    if (!p) {
		lexerror("Out of memory");
		return 0;
	    }
#if 0
	    printf("ident type is %d\n",p->type);
#endif
	    switch(p->type) {
	      case I_TYPE_DEFINE:
		outp=yyp;
		_expand_define(&p->u.define);
		if (lex_fatal) {
		    return -1;
		}
		yyp=outp;
		continue;
	      case I_TYPE_RESWORD:
		outp = yyp;
		return p->u.code;
	      case I_TYPE_LOCAL:
		yylval.number = p->u.local;
		outp = yyp;
		return F_LOCAL;
	      default:
		yylval.ident = p;
		outp = yyp;
		return F_IDENTIFIER;
	    }
	}
    default:
	goto badlex;
    }
  }
 badlex:
  if (lex_fatal) return -1;
  { char buff[100]; sprintf(buff, "Illegal character (hex %02x) '%c'", c, c);
    yyerror(buff); outp = yyp; return ' '; }
}

int
yylex()
{
    int r;

#ifdef LEXDEBUG
    yytext[0] = 0;
#endif
    r = yylex1();
#ifdef LEXDEBUG
    fprintf(stderr, "lex=%d(%s) ", r, yytext);
#endif
    return r;
}

extern YYSTYPE yylval;

void end_new_file()
{
    while (inctop) {
	struct incstate *p;
	p = inctop;
	close(yyin_des);
	xfree(current_file);
	current_file = p->file;
	yyin_des = p->yyin_des;
	inctop = p->next;
	xfree((char *)p);
    }
    while(iftop) {
	struct ifstate *p;

	p = iftop;
	iftop = p->next;
	xfree((char *)p);
    }
#if 1
    if (defbuf_len > DEFBUF_1STLEN) {
	xfree(defbuf);
	defbuf_len = 0;
    }
#endif
    if (last_lex_string) {
	free_string(last_lex_string);
	last_lex_string = 0;
    }
}

void lex_close(msg)
    char *msg;
{
    int i;
    struct incstate *p;
    static char buf[] =
	"File descriptors exhausted, include nesting: 12345678";

    for (i = 0, p = inctop; p; p = p->next)
	i++;
    if (!msg) {
	/* skip back terminating \0 and 8 digits */
	sprintf(buf + sizeof buf - 9, "%d", i);
	msg = buf;
    }
    end_new_file();
    outp = ("##")+1;
    lexerror(msg);
}

void start_new_file(fd)
    int fd;
{
    free_defines();
    yyin_des = fd;
    if (!defbuf_len) {
	defbuf = xalloc(DEFBUF_1STLEN);
	defbuf_len = DEFBUF_1STLEN;
    }
    *(outp = linebufend = (linebufstart = defbuf + DEFMAX) + MAXLINE) = 0;
    current_line = 1; /* already used in _myfilbuf() */
    _myfilbuf();
    lex_fatal = 0;
    if (auto_include_string) {
	add_input(auto_include_string);
	current_line = auto_include_start;
    }
    pragma_strict_types = 0;		/* I would prefer !o_flag   /Lars */
    instrs[F_CALL_OTHER-F_OFFSET].ret_type = TYPE_ANY;
    pragma_save_types = 0;
    pragma_verbose_errors = 0;
    nexpands = 0;
}

struct ident *all_efuns = 0;

void init_num_args()
{
    extern char master_name[];
    int i, n;
    struct lpc_predef_s *tmpf;
    char mtext[MLEN];
    static short binary_operators[] = {
	F_ADD, F_SUBTRACT, F_MULTIPLY, F_DIVIDE, F_MOD,
	F_LT, F_GT, F_EQ, F_GE, F_LE, F_NE,
	F_OR, F_XOR, F_LSH, F_RSH,
	F_INDEX, F_RINDEX, F_EXTRACT2,
    };
    static short ternary_operators[] = {
	F_RANGE, F_NR_RANGE, F_RR_RANGE, F_RN_RANGE,
	F_MAP_INDEX,
    };

    for (i=0; i<ITABLE_SIZE; i++)
	ident_table[i] = 0;
    for(n=0; n<NELEM(instrs); n++) {
	struct ident *p;

	if (instrs[n].Default == -1) continue;
	p = make_shared_identifier(instrs[n].name, I_TYPE_GLOBAL);
	if (!p)
	    fatal("Out of memory\n");
	p->type = I_TYPE_GLOBAL;
	p->u.global.efun     =  n;
	p->u.global.sim_efun = -1;
	p->u.global.function = -2;
	p->u.global.variable = -2;
	p->next_all = all_efuns;
	all_efuns = p;
    }
    for (i=0; i<NELEM(reswords); i++) {
        struct ident *p;

        p = make_shared_identifier(reswords[i].name, I_TYPE_RESWORD);
	if (!p)
	    fatal("Out of memory\n");
        p->type = I_TYPE_RESWORD;
        p->u.code = reswords[i].code;
    }
    for (i=0; i<NELEM(binary_operators); i++) {
	n = binary_operators[i] - F_OFFSET;
	instrs[n].min_arg = instrs[n].max_arg = 2;
	instrs[n].Default = 0;
	instrs[n].ret_type = TYPE_ANY;
    }
    for (i=0; i<NELEM(ternary_operators); i++) {
	n = ternary_operators[i] - F_OFFSET;
	instrs[n].min_arg = instrs[n].max_arg = 3;
	instrs[n].Default = 0;
	instrs[n].ret_type = TYPE_ANY;
    }
    n = F_AND - F_OFFSET;
	instrs[n].min_arg = instrs[n].max_arg = 2;
	instrs[n].ret_type = TYPE_ANY;
    n = F_COMPL - F_OFFSET;
	instrs[n].min_arg = instrs[n].max_arg = 1;
	instrs[n].Default = 0;
	instrs[n].ret_type = TYPE_ANY;
    n = F_NOT - F_OFFSET;
	instrs[n].min_arg = instrs[n].max_arg = 1;
	instrs[n].ret_type = TYPE_ANY;
    add_permanent_define("LPC3", -1, string_copy(""), 0);
#ifdef MSDOS
	add_permanent_define("MSDOS", -1, string_copy(""), 0);
#endif /* MSDOS */   
#ifdef COMPAT_MODE
	add_permanent_define("COMPAT_FLAG", -1, string_copy(""), 0);
#endif
    mtext[0] = '"';
    mtext[1] = '/';
    strcpy(mtext+2, master_name);
    strcat(mtext+2, "\"");
    add_permanent_define("__MASTER_OBJECT__", -1, string_copy(mtext), 0);
    add_permanent_define("__FILE__", -1, (char *)get_current_file, 1);
    add_permanent_define("__LINE__", -1, (char *)get_current_line, 1);
    add_permanent_define("__VERSION__", -1, (char *)get_version, 1);
    add_permanent_define("__HOST_NAME__", -1, (char *)get_hostname, 1);
    add_permanent_define("__DOMAIN_NAME__", -1, (char *)get_domainname, 1);
    add_permanent_define(
      "__HOST_IP_NUMBER__", -1, (char *)get_host_ip_number, 1);
    sprintf(mtext, "%d", MAX_USER_TRACE);
    add_permanent_define("__MAX_RECURSION__", -1, string_copy(mtext), 0);
    add_permanent_define("__EFUN_DEFINED__", 1, (char *)efun_defined, 1);
    for (tmpf=lpc_predefs; tmpf; tmpf=tmpf->next) {
	char namebuf[NSIZE];

	*mtext='\0';
	sscanf(tmpf->flag,"%[^=]=%[ -~=]",namebuf,mtext);
	if ( strlen(namebuf) >= NSIZE ) fatal("NSIZE exceeded\n");
	if ( strlen(mtext) >= MLEN ) fatal("MLEN exceeded\n");
	add_permanent_define(namebuf,-1,string_copy(mtext), 0);
    }
}

char *get_f_name(n)
    int n;
{
    if (instrs[n].name)
	return instrs[n].name;
    else {
	static char buf[30];
	sprintf(buf, "<OTHER %d>", n);
	return buf;
    }
}

#define NARGS 25
#define MARKS '@'

#define SKIPWHITE while(lexwhite(*p)) p++
#define GETALPHA(p, q, m) \
    while(isalunum(*p)) {\
	*q = *p++;\
	if (q < (m))\
	    q++;\
	else {\
	    lexerror("Name too long");\
	    return;\
	}\
    }\
    *q++ = 0

static char
cmygetc()
{
    int c;

    for(;;) {
	c = mygetc();
	if (c == '/') {
	    if (gobble('*'))
		skip_comment();
	    else if (gobble('/')) {
		outp = skip_pp_comment(outp);
		current_line--;
		return '\n';
	    } else
		return c;
	} else
	    return c;
    }
}

static int
refill(quote)
int quote;
{
    char *p;
    int c;
    int last = 0;

    p = yytext;
        do {
	    c = mygetc();
	    if (c == '/' && !quote) {
		last = 0;
		if (gobble('*')) {
		    skip_comment();
		    continue;
		}
		else if (gobble('/')) {
		    outp = skip_pp_comment(outp);
		    current_line--;
		    c = '\n';
		}
	    } else if (last == '\\') {
		last = 0;
	    } else if (c == '"')
		quote ^= 1;
	    else
		last = c;
	    if (p < yytext+MAXLINE-5)
	        *p++ = c;
	    else {
	        lexerror("Line too long");
	        break;
	    }
        } while(c != '\n');
        myfilbuf();
        p[-1] = ' ';
    *p = 0;
    nexpands=0;
    current_line++;
    store_line_number_info();
    return quote;
}

static void
handle_define(yyt, quote)
char *yyt;
int quote;
{
    char namebuf[NSIZE];
    char args[NARGS][NSIZE];
    char mtext[MLEN];
    char *p, *q;

    p = yyt;
    strcat(p, " ");
    q = namebuf;
    GETALPHA(p, q, namebuf+NSIZE-1);
    if (*p == '(') {		/* if "function macro" */
	int arg;
	int inid;
	char *ids;
	p++;			/* skip '(' */
	SKIPWHITE;
	if (*p == ')') {
	    arg = 0;
	} else {
	    for(arg = 0; arg < NARGS; ) {
		q = args[arg];
		GETALPHA(p, q, &args[arg][NSIZE-1]);
		arg++;
		SKIPWHITE;
		if (*p == ')')
		    break;
		if (*p++ != ',') {
		    yyerror("Missing ',' in #define parameter list");
		    return;
		}
		SKIPWHITE;
	    }
	    if (arg == NARGS) {
		lexerror("Too many macro arguments");
		return;
	    }
	}
	p++;			/* skip ')' */
	for(inid = 0, q = mtext; *p; ) {
	    if (isalunum(*p)) {
		if (!inid) {
		    inid++;
		    ids = p;
		}
	    } else {
		if (inid) {
		    int idlen = p - ids;
		    int n, l;
		    for(n = 0; n < arg; n++) {
			l = strlen(args[n]);
			if (l == idlen && strncmp(args[n], ids, l) == 0) {
			    q -= idlen;
			    *q++ = MARKS;
			    *q++ = n+MARKS+1;
			    break;
			}
		    }
		    inid = 0;
		}
	    }
	    *q = *p;
	    if (*p++ == MARKS)
		*++q = MARKS;
	    if (q < mtext+MLEN-2)
		q++;
	    else {
		lexerror("Macro text too long");
		return;
	    }
	    if (!*p) {
	      if (p[-2] == '\\') {
		q -= 2;
		quote = refill(quote);
		p = yytext;
	      } else if (p[-2] == '\r' && p[-3] == '\\' ) {
		q -= 3;
		quote = refill(quote);
		p = yytext;
	      }
	    }
	}
	*--q = 0;
	add_define(namebuf, arg, mtext);
    } else {
	for(q = mtext; *p; ) {
	    *q = *p++;
	    if (q < mtext+MLEN-2)
		q++;
	    else {
		lexerror("Macro text too long");
		return;
	    }
	    if (!*p) {
	      if (p[-2] == '\\') {
		q -= 2;
		quote = refill(quote);
		p = yytext;
	      } else if (p[-2] == '\r' && p[-3] == '\\' ) {
		q -= 3;
		quote = refill(quote);
		p = yytext;
	      }
	    }
	}
	*--q = 0;
	add_define(namebuf, -1, mtext);
    }
    return;
}

static void
add_input(p)
char *p;
{
    int l = strlen(p);

#if defined(LEXDEBUG)
if (l > 0)
fprintf(stderr, "add '%s'\n", p);
#endif
    outp -= l;
    if (outp < &defbuf[10]) {
        outp += l;
	lexerror("Macro expansion buffer overflow");
	return;
    }
    strncpy(outp, p, l);
}

static void
add_define(name, nargs, exps)
char *name, *exps;
int nargs;
{
    struct ident *p;

    p = make_shared_identifier(name, I_TYPE_DEFINE);
    if (!p) {
	lexerror("Out of memory");
	return;
    }
    if (p->type != I_TYPE_UNKNOWN) {
	if (nargs != p->u.define.nargs ||
	    p->u.define.special ||
	    strcmp(exps,p->u.define.exps.str) != 0)
	{
	    char buf[200+NSIZE];
	    sprintf(buf, "Redefinition of #define %s", name);
	    yyerror(buf);
	}
	return;
    }
    p->type = I_TYPE_DEFINE;
    p->u.define.nargs = nargs;
    p->u.define.permanent = 0;
    p->u.define.special = 0;
    if ( !(p->u.define.exps.str = xalloc(strlen(exps)+1)) ) {
	free_shared_identifier(p);
	lexerror("Out of memory");
	return;
    }
    strcpy(p->u.define.exps.str, exps);
    p->next_all = all_defines;
    all_defines = p;
#if defined(LEXDEBUG)
    fprintf(stderr, "define '%s' %d '%s'\n", name, nargs, exps);
#endif
}

static void
add_permanent_define(name, nargs, exps, special)
char *name, *exps, special;
int nargs;
{
    struct ident *p;

    p = make_shared_identifier(name, I_TYPE_DEFINE);
    if (!p) {
	error("Out of memory\n");
    }
    if (p->type != I_TYPE_UNKNOWN) {
	if (nargs != p->u.define.nargs ||
	    p->u.define.special ||
	    strcmp(exps,p->u.define.exps.str) != 0)
	{
	    error("Redefinition of #define %s", name);
	}
	return;
    }
    p->type = I_TYPE_DEFINE;
    p->u.define.nargs = nargs;
    p->u.define.permanent = 1;
    p->u.define.special = special;
    p->u.define.exps.str = exps;
    p->next_all = permanent_defines;
    permanent_defines = p;
}

void
free_defines()
{
    struct ident *p, *q;

    for(p = all_defines; p; p = q) {
	q = p->next_all;
	if (p->name) {
	    xfree(p->u.define.exps.str);
	    free_shared_identifier(p);
	} else { /* has been undef'd. */
	    xfree((char *)p);
	}
    }
    all_defines = 0;
    for (p = undefined_permanent_defines; p; p = q) {
	struct ident *curr, **prev;

	q = p->next;
	p->next = 0;
	prev = &ident_table[p->hash];
	while (curr = *prev) {
	    if (curr->name == p->name) { /* found it */
		p->next = curr->next;
		free_string(p->name);
		break;
	    }
	    prev = &curr->next;
	} /* not found, create new one */
	p->inferior = curr;
	*prev = p;
    }
    undefined_permanent_defines = 0;
    nexpands = 0;
}

static
struct ident *
lookup_define(s)
char * s;
{
    struct ident *curr, *prev;
    int h;

    h = identhash(s);

    curr = ident_table[h];
    prev = 0;
    while (curr) {
	if (!strcmp(curr->name, s)) { /* found it */
	    if (prev) { /* not at head of list */
		prev->next = curr->next;
		curr->next = ident_table[h];
		ident_table[h] = curr;
	    }
	    if (curr->type == I_TYPE_DEFINE)
		return curr;
	    return 0;
	}
	prev = curr;
	curr = curr->next;
    } /* not found */
    return 0;
}

#define SKIPW \
    for(;;) {\
        do {\
	    c = cmygetc();\
	} while(lexwhite(c));\
	if (c == '\n') {\
	    myfilbuf();\
	    store_line_number_info();\
	    current_line++;\
	    total_lines++;\
	} else break;\
    }\


/* Check if yytext is a macro and expand if it is. */

static int
expand_define()
{
    struct ident *p;

    p = lookup_define(yytext);
    if (!p) {
	return 0;
    }
    return _expand_define(&p->u.define);
}

static int
_expand_define(p)
    struct defn *p;
{
    char expbuf[DEFMAX];
    char *args[NARGS];
    char buf[DEFMAX];
    char *q, *e, *b;
	char *r;

    if (nexpands++ > EXPANDMAX) {
	lexerror("Too many macro expansions");
	return 0;
    }
    if (p->nargs == -1) {
	if (!p->special) {
	    add_input(p->exps.str);
	} else {
	    e = (*p->exps.fun)();
	    if (!e) {
		lexerror("Out of memory");
		return 0;
	    }
	    add_input(e);
	    xfree(e);
	}
    } else {
	int c, parcnt = 0, dquote = 0, squote = 0;
	int n;
	SKIPW;
	if (c != '(') {
	    yyerror("Missing '(' in macro call");
	    return 0;
	}
	SKIPW;
	if (c == ')')
	    n = 0;
	else {
	    r = outp;

	    *--r = c;
	    q = expbuf;
	    args[0] = q;
	    for(n = 0;;) {
		if (q >= expbuf + DEFMAX - 5) {
		    lexerror("Macro argument overflow");
		    return 0;
		}
		switch(c = *r++) {
		  case '"' :
		    if (!squote) dquote ^= 1;
		    *q++ = c;
		    continue;
		  case '#':
		    *q++ = c;
		    if (!squote && !dquote && *r == '\'') {
			r++;
			*q++ = '\'';
			if (isalunum(c = *r)) {
			    do {
				*q++ = c;
				++r;
			    } while (isalunum(c = *r));
			} else {
			    extern int symbol_operator PROT((char *, char **));

			    char *end;

			    if (symbol_operator(r, &end) < 0) {
				yyerror("Missing function name after #'");
			    }
			    strncpy(q, r, end - r);
			    q += end - r;
			    r = end;
			}
		    }
		    continue;
		  case '\'':
		    if ( !dquote &&
			 (!isalunum(*r) || r[1] == '\'') &&
			 (*r != '(' || r[1] != '{') )
		    {
			squote ^= 1;
		    }
		    *q++ = c;
		    continue;
		  case '(' :
		    if (!squote && !dquote) parcnt++;
		    *q++ = c;
		    continue;
		  case ')' :
		    if (!squote && !dquote) {
		        parcnt--;
			if (parcnt < 0) {
			    *q++ = 0;
			    n++;
			    break;
			}
		    }
		    *q++ = c;
		    continue;
		  case '\\':
		    *q++ = c;
		    if (squote || dquote) {
			c = *r++;
			if (c == '\r')
			    c = *r++;
			if (c == '\n') { /* nope! This wracks consistency! */
			    store_line_number_info();
			    current_line++;
			    total_lines++;
			    if (!*r) {
				outp = r;
				r = _myfilbuf();
			    }
			    q--;	/* alas, long strings should work. */
			    continue;
			}
			if (c == CHAR_EOF) { /* can't quote THAT */
			    r--;
			    continue;
			}
			*q++ = c;
		    }
		    continue;

		  case '\n':
		    store_line_number_info();
		    current_line++;
		    total_lines++;
		    *q++ = ' ';
		    if (!*r) {
			outp = r;
			r = _myfilbuf();
		    }
		    if (squote || dquote) {
			lexerror("Newline in string");
			return 0;
		    }
		    continue;
		  case ',':
		    if (!parcnt && !dquote && !squote) {
			*q++ = 0;
			args[++n] = q;
			if (n == NARGS - 1) {
			    lexerror("Maximum macro argument count exceeded");
			    return 0;
			}
			continue;
		    }
		    *q++ = c;
		    continue;
		  case CHAR_EOF:
			lexerror("Unexpected end of file");
			return 0;
		  case '/':
		    if (!squote && !dquote) {
			if ( (c = *r++) == '*') {
			    outp = r;
			    skip_comment();
			    r = outp;
			} else {
			    --r;
			    *q++ = '/';
			}
			continue;
		    }
		  default:
		    *q++ = c;
		    continue;
		} /* end switch */
		break;
	    } /* end for */
	    outp = r;
	}
	if (n != p->nargs) {
	    yyerror("Wrong number of macro arguments");
	    return 0;
	}
	/* Do expansion */
	if (p->special) {
	    (*p->exps.fun)(args);
	    return 1;
	}
	b = buf;
	e = p->exps.str;
	while(*e) {
	    if (*e == MARKS) {
		if (*++e == MARKS)
		    *b++ = *e++;
		else {
		    for(q = args[*e++ - MARKS - 1]; *q; ) {
			*b++ = *q++;
			if (b >= buf+DEFMAX) {
			    lexerror("Macro expansion overflow");
			    return 0;
			}
		    }
		}
	    } else {
		*b++ = *e++;
		if (b >= buf+DEFMAX) {
		    lexerror("Macro expansion overflow");
		    return 0;
		}
	    }
	}
	*b++ = 0;
	add_input(buf);
    }
    return 1;
}

/* Stuff to evaluate expression.  I havn't really checked it. /LA
** Written by "J\"orn Rennecke" <amylaar@cs.tu-berlin.de>
*/
#define SKPW 	do c = mygetc(); while(lexwhite(c)); myungetc(c)

static int exgetc() {
    register char c,*yyp;

    c=mygetc();
    for (;;) {
	if ( isalpha(c) || c=='_' ) {
	    yyp=yytext;
	    do {
		SAVEC;
		c=mygetc();
	    } while ( isalunum(c) );
	    myungetc(c);
	    *yyp='\0';
	    if (strcmp(yytext, "defined") == 0) {
		/* handle the defined "function" in #if */
		do c = mygetc(); while(lexwhite(c));
		if (c != '(') {
		    yyerror("Missing ( in defined");
		    continue;
		}
		do c = mygetc(); while(lexwhite(c));
		yyp=yytext;
		while ( isalunum(c) ) {
		    SAVEC;
		    c=mygetc();
		}
		*yyp='\0';
		while(lexwhite(c)) c = mygetc();
		if (c != ')') {
		    yyerror("Missing ) in defined");
		    continue;
		}
		SKPW;
		if (lookup_define(yytext))
		    add_input(" 1 ");
		else
		    add_input(" 0 ");
	    } else {
		if (!expand_define()) add_input(" 0 ");
	    }
	    c=mygetc();
	} else if (c == '\\' && *outp == '\n') {
	    int quote;

	    outp++;
	    yyp = yytext;
	    for(quote = 0;;) {
		c = mygetc();
		if (c == '"')
	    	quote ^= 1;
		while(!quote && c == '/') { /* handle comments cpp-like */
		    char c2;
    
		    if ( (c2 = mygetc()) == '*') {
			skip_comment();
			c=mygetc();
		    } else if (c2 == '/') {
			outp = skip_pp_comment(outp);
			current_line--;
			c = '\n';
		    } else {
			--outp;
			break;
		    }
		}
		SAVEC;
		if (c == '\n') {
		    break;
		}
	    }
	    *yyp = '\0';
	    current_line++;
	    total_lines++;
	    add_input(yytext);
	    nexpands = 0;
	    c=mygetc();
	} else {
	    break;
	}
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

/* lookup table for characters >= ' ' and <= '~'.
 * 0 for no operator, else index into optab2.
 */
static char _optab[]=
{0,6,0,0,0,46,50,0,0,0,2,18,0,14,0,10,0,0,0,0,0,0,0,0,0,0,0,0,22,42,32,68,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57,0,1};

/* optab2[index-1] : operation code for unary operator, 0 for none.
 * optab[index+0 .. +3 .. +6 ...] :
 * two character binary operators: second character, operation code, priority
 * one character binary operator & end: 0,           operation code, priority
 * end: 0, 0
 */
static char optab2[]=
{
    BNOT,0,0, MULT,11,
    LNOT,'=',NEQ,7,0,0, DIV,11,
    UMINUS,0,BMINUS,10,
    UPLUS,0,BPLUS,10,
    0,'<',LSHIFT,9,'=',LEQ,8,0,LESS,8,
    0,'>',RSHIFT,9,'=',GEQ,8,0,GREAT,8,
    0,'=',EQ,7,0,0, MOD,11,
    0,'&',LAND,3,0,BAND,6,0,'|',LOR,2,0,BOR,4,
    0,0,XOR,5,0,0,QMARK,1
};
#define optab1 (_optab-' ')

static int cond_get_exp(priority, svp)
    int priority;
    struct svalue *svp;
{
    int c;
    int value,value2,x;
    struct svalue sv2;

    svp->type = T_INVALID;
    do c=exgetc(); while ( lexwhite(c) );
    if ( c=='(' ) {

	value = cond_get_exp(0, svp);
	do c=exgetc(); while ( lexwhite(c) );
	if ( c!=')' ) {
	    yyerror("bracket not paired in #if");
	    if (c == '\n') myungetc('\n');
	}
    } else if ( ispunct(c) ) {
	if (c == '"') {
	    char *p, *q;

	    svp->type = T_STRING;
	    svp->x.string_type = STRING_MALLOC;
	    q = p = outp;
	    for (;;) {
		c = *p++;
		if (c == '"') {
		    break;
		}
		if (c == '\n') {
		    yyerror("unexpected end of string in #if");
		    svp->u.string = string_copy("");
		    return 0;
		}
		if (c == '\\') {
		    c = *p++;
		    if (c == '\n') {
			current_line++;
			*--p = '"';
			break;
		    }
		}
		*q++ = c;
	    }
	    *q = '\0';
	    svp->u.string = string_copy(outp);
	    outp = p;
	} else {
	    x=optab1[c];
	    if (!x) {
		yyerror("illegal character in #if");
		return 0;
	    }
	    value = cond_get_exp(12, svp);
	    switch ( optab2[x-1] ) {
	      case BNOT  : value = ~value; break;
	      case LNOT  : value = !value; break;
	      case UMINUS: value = -value; break;
	      case UPLUS : value =  value; break;
	      default :
		yyerror("illegal unary operator in #if");
		free_svalue(svp);
		svp->type = T_NUMBER;
		return 0;
	    }
	    if (svp->type != T_NUMBER) {
		yyerror("illegal type to unary operator in #if");
		free_svalue(svp);
		svp->type = T_NUMBER;
		return 0;
	    }
	    svp->u.number = value;
	}
    } else {
	int base;

	if ( !lexdigit(c) ) {
	    if (c == '\n') {
		yyerror("missing expression in #if");
		myungetc('\n');
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
	svp->type = T_NUMBER;
	svp->u.number = value;
    }
    for (;;) {
	do c=exgetc(); while ( lexwhite(c) );
	if ( !ispunct(c) ) break;
	if (c == '"') {
	    myungetc('"');
	    c = '+';
	}
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
	value2=cond_get_exp(optab2[x+2], &sv2);
	if (svp->type == T_NUMBER && sv2.type == T_NUMBER) {
	    switch ( optab2[x+1] ) {
	      case MULT : value *= value2;		break;
	      case DIV  : if (!value2) lexerror("Division by zero");
			  else value /= value2; 	break;
	      case MOD  : if (!value2) lexerror("Division by zero");
			  else value %= value2; 	break;
	      case BPLUS  : value += value2;		break;
	      case BMINUS : value -= value2;		break;
	      case LSHIFT : if (value2 > MAX_SHIFT) value = 0;
			    else value <<= value2; break;
	      case RSHIFT : value >>= value2 > MAX_SHIFT ? MAX_SHIFT : value2;
		break;
	      case LESS   : value = value <  value2;	break;
	      case LEQ    : value = value <= value2;	break;
	      case GREAT  : value = value >  value2;	break;
	      case GEQ    : value = value >= value2;	break;
	      case EQ     : value = value == value2;	break;
	      case NEQ    : value = value != value2;	break;
	      case BAND   : value &= value2;		break;
	      case XOR    : value ^= value2;		break;
	      case BOR    : value |= value2;		break;
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
		    *svp = sv2;
		    cond_get_exp(1, &sv2);
		    free_svalue(&sv2);
		    value = value2;
		}
		else value=cond_get_exp(1, svp);
		break;
	    }
	} else if (svp->type == T_STRING && sv2.type == T_STRING) {
	    x = optab2[x+1];
	    if (x == BPLUS) {
		char *str;

		str = xalloc(strlen(svp->u.string) + strlen(sv2.u.string) + 1);
		strcpy(str, svp->u.string);
		strcat(str, sv2.u.string);
		free_string_svalue(svp);
		free_string_svalue(&sv2);
		svp->u.string = str;
	    } else {
		value = strcmp(svp->u.string, sv2.u.string);
		free_string_svalue(svp);
		svp->type = T_NUMBER;
		free_string_svalue(&sv2);
		switch (x) {
		  case LESS   : value = value <  0; break;
		  case LEQ    : value = value <= 0; break;
		  case GREAT  : value = value >  0; break;
		  case GEQ    : value = value >= 0; break;
		  case EQ     : value = value == 0; break;
		  case NEQ    : value = value != 0; break;
		  default:
		    yyerror("illegal operator use in #if");
		    return 0;
		}
		svp->u.number = value;
	    }
	} else {
	    yyerror("operands in #if won't match");
	    free_svalue(svp);
	    svp->type = T_NUMBER;
	    free_svalue(&sv2);
	    return 0;
	}
    }
    myungetc(c);
    return value;
}

void set_inc_list(v)
    struct vector *v;
{
    int i;
    char *p;
    struct svalue *svp;
    mp_int len, max;

    svp = v->item;
    for (i=0, max = 0; i < VEC_SIZE(v); i++, svp++) {
	if (svp->type != T_STRING) {
	    error("H_INCLUDE_DIRS argument has a non-string array element\n");
	}
	p = svp->u.string;
	for(;;) {
	    if (*p == '/')
		p++;
	    else if (*p == '.' && p[1] == '/')
		p += 2;
	    else
		break;
	}
	/*
	 * Even make sure that the game administrator has not made an error.
	 */
	if (!legal_path(p)) {
	    error("H_INCLUDE_DIRS path contains '..'\n");
	}
	if (*p == '.' && !p[1])
	    error("H_INCLUDE_DIRS path is a single prefix dot\n");
	len = strlen(p);
	if (max < len)
	    max = len;
	if (len >= 2 && p[len -1] == '.' && p[len - 2] == '/')
	    error("H_INCLUDE_DIRS path ends in single prefix dot\n");
	p = string_copy(p);
	if (!p)
	    error("Out of memory\n");
	free_svalue(svp);
	svp->x.string_type = STRING_MALLOC;
	svp->u.string = p;
    }
    inc_list = v->item;
    inc_list_size = VEC_SIZE(v);
    inc_list_maxlen = max;
}

void clear_auto_include_string()
{
    if (auto_include_string) {
	xfree(auto_include_string);
	auto_include_string = 0;
    }
}

struct svalue *f_set_auto_include_string(sp)
    struct svalue *sp;
{
    char *s;

    if (sp->type != T_STRING)
	bad_xefun_arg(1, sp);
    if (_privilege_violation("set_auto_include_string", sp, sp) > 0)
    {
	clear_auto_include_string();
	s = sp->u.string;
	auto_include_string = xalloc(strlen(s)+2);
	*auto_include_string = '\n';
	strcpy(auto_include_string+1, s);
	for (auto_include_start = 0; *s; auto_include_start -= *s++ == '\n');
    }
    free_svalue(sp);
    return sp - 1;
}

static char *get_current_file() {
    char *buf;

    buf = xalloc(strlen(current_file)+4);
    if (!buf) return 0;
    sprintf(buf, "\"/%s\"", current_file);
    return buf;
}

static char *get_current_line() {
    char *buf;

    buf = xalloc(12);
    if (!buf) return 0;
    sprintf(buf, "%d", current_line);
    return buf;
}

static char *get_version() {
    char *buf;

    buf = xalloc(8 + strlen(PATCH_LEVEL LOCAL_LEVEL));
    if (!buf) return 0;
    sprintf(buf, "\"%5.5s%s\"", GAME_VERSION, PATCH_LEVEL LOCAL_LEVEL);
    return buf;
}

static char *get_hostname() {
    char *tmp, *buf;

    tmp = query_host_name();
    buf = xalloc(strlen(tmp)+3);
    if (!buf) return 0;
    sprintf(buf, "\"%s\"", tmp);
    return buf;
}

static char *get_domainname() {
    char tmp[256], *buf;

#ifdef HAVE_GETDOMAINNAME
    getdomainname(tmp, sizeof(tmp));
    tmp[sizeof(tmp)-1] = '\0';
#else
    strcpy(tmp, "unknown");
#endif
    buf = xalloc(strlen(tmp)+3);
    if (!buf) return 0;
    sprintf(buf, "\"%s\"", tmp);
    return buf;
}

static void efun_defined(args)
    char **args;
{
    struct ident *p;

    p = make_shared_identifier(args[0], I_TYPE_GLOBAL);
    if (!p) {
	lexerror("Out of memory");
	return;
    }
    while (p->type > I_TYPE_GLOBAL) {
	if ( !(p = p->inferior) )
	    break;
    }
    add_input(
      (p && p->type == I_TYPE_GLOBAL && p->u.global.efun >= 0) ?
	" 1 " : " 0 "
    );
    if (p && p->type == I_TYPE_UNKNOWN)
	free_shared_identifier(p);
}

void remove_unknown_identifier() {
    int i;
    struct ident *id, *next;

    for (i = ITABLE_SIZE; --i >= 0; ) {
	id = ident_table[i];
	for ( ; id; id = next) {
	    next = id->next;
	    if (id->type == I_TYPE_UNKNOWN)
		free_shared_identifier(id);
	}
    }
}

#ifdef MALLOC_smalloc
void count_lex_refs() {
    int i;
    struct ident *id;

    /* Identifier */
    for (i = ITABLE_SIZE; --i >= 0; ) {
	id = ident_table[i];
	for ( ; id; id = id->next) {
	    count_ref_from_string(id->name);
	    note_malloced_block_ref((char *)id);
	}
    }
    for (id = permanent_defines; id; id = id->next_all) {
	if (!id->u.define.special)
	    note_malloced_block_ref(id->u.define.exps.str);
    }

    if (auto_include_string)
	note_malloced_block_ref(auto_include_string);
    if (defbuf_len)
	note_malloced_block_ref(defbuf);
}
#endif /* MALLOC_smalloc */

char *lex_error_context() {
    extern int yychar;

    static char buf[20];
    char *end;
    mp_int len;

    if (!pragma_verbose_errors)
	return "";
    strcpy(buf, yychar == -1 ? (len = 5, "near ") : (len = 7, "before "));
    if (!yychar || !*outp) {
	strcpy(buf+len, "end of line");
    } else {
	strncpy(buf + len, outp, sizeof buf - 1 - len);
	buf[sizeof buf - 1] = '\0';
	if (end = strchr(buf, '\n'))
	    *end = 0;
    }
    return buf;
}

struct svalue *f_expand_define(sp)
    struct svalue *sp;
{
    char *arg, *res, *end;
    struct ident *d;

    if (sp[-1].type != T_STRING)
	bad_xefun_arg(1, sp);
    arg = sp->u.string;
    if (sp->type != T_STRING) {
	if (sp->type != T_NUMBER || sp->u.number)
	    bad_xefun_arg(2, sp);
	arg = "";
    }
    res = 0;
    if (current_file && outp > defbuf && outp <= &defbuf[defbuf_len]) {
	myungetc('\n');
	end = outp;
	add_input(arg);
	d = lookup_define(sp[-1].u.string);
	if (d && _expand_define(&d->u.define) ) {
	    *end = '\0';
	    res = string_copy(outp);
	}
	outp = &end[1];
    }
    free_svalue(sp);
    free_svalue(--sp);
    if (!res) {
	sp->type = T_NUMBER;
	sp->u.number = 0;
    } else {
	sp->x.string_type = STRING_MALLOC;
	sp->u.string = res;
    }
    return sp;
}
