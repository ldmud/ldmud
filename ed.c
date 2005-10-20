/*
 *  ed - standard editor
 *  ~~
 *	Authors: Brian Beattie, Kees Bot, and others
 *
 * Copyright 1987 Brian Beattie Rights Reserved.
 * Permission to copy or distribute granted under the following conditions:
 * 1). No charge may be made other than reasonable charges for reproduction.
 * 2). This notice must remain intact.
 * 3). No further restrictions may be added.
 * 4). Except meaningless ones.
 *
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *  TurboC mods and cleanup 8/17/88 RAMontante.
 *  Further information (posting headers, etc.) at end of file.
 *  RE stuff replaced with Spencerian version, sundry other bugfix+speedups
 *  Ian Phillipps. Version incremented to "5".
 * _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
 *
 *  ********--->> INDENTATION ONLINE !!!! <<----------****************
 *  Indentation added by Ted Gaunt (aka Qixx) paradox@mcs.anl.gov
 *  help files added by Ted Gaunt
 *  '^' added by Ted Gaunt
 *  Note: this version compatible with v.3.0.34  (and probably all others too)
 *  but i've only tested it on v.3 please mail me if it works on your mud
 *  and if you like it!
 * _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
 *
 *  Online indentation algorithm replaced with adapted version from DGD
 *  editor by Dworkin, 920510
 */


#define ED_VERSION 5	/* used only in the "set" function, for i.d. */

#include "config.h"

#include <stdio.h>
#include <sys/types.h>  /* need for netinet */
/* #include <netinet/in.h> Included in comm.h */
#include <ctype.h>
/* Regexp is Henry Spencer's package. WARNING: regsub is modified to return
 * a pointer to the \0 after the destination string, and this program refers
 * to the "private" reganch field in the struct regexp.
 */
#include "lint.h"
#include "regexp.h"
#include "interpret.h"
#include "object.h"
#include "comm.h"

/*
 *	#defines for non-printing ASCII characters
 */
#define NUL	0x00	/* ^@ */
#define EOS	0x00	/* end of string */
#define SOH	0x01	/* ^A */
#define STX	0x02	/* ^B */
#define ETX	0x03	/* ^C */
#define EOT	0x04	/* ^D */
#define ENQ	0x05	/* ^E */
#define ACK	0x06	/* ^F */
#define BEL	0x07	/* ^G */
#define BS	0x08	/* ^H */
#define HT	0x09	/* ^I */
#define LF	0x0a	/* ^J */
#define NL	'\n'
#define VT	0x0b	/* ^K */
#define FF	0x0c	/* ^L */
#define CR	0x0d	/* ^M */
#define SO	0x0e	/* ^N */
#define SI	0x0f	/* ^O */
#define DLE	0x10	/* ^P */
#define DC1	0x11	/* ^Q */
#define DC2	0x12	/* ^R */
#define DC3	0x13	/* ^S */
#define DC4	0x14	/* ^T */
#define NAK	0x15	/* ^U */
#define SYN	0x16	/* ^V */
#define ETB	0x17	/* ^W */
#define CAN	0x18	/* ^X */
#define EM	0x19	/* ^Y */
#define SUB	0x1a	/* ^Z */
#define ESC	0x1b	/* ^[ */
#define FS	0x1c	/* ^\ */
#define GS	0x1d	/* ^] */
/*#define RS	0x1e	   ^^ */
#define US	0x1f	/* ^_ */
#define SP	0x20	/* space */
#define DEL	0x7f	/* DEL*/
#define ESCAPE  '\\'

#define TAB '\t'		/* added by Qixx for indentation */
#define LB '{'
#define RB '}'
#define LC '('
#define RC ')'
#define LS '['
#define RS ']'
#define PP '\"'
#define EOL '\0'


#define TRUE	1
#define FALSE	0
#define ERR		-2
#undef  FATAL		(ERR-1)
#define CHANGED		(ERR-2)
#define SET_FAIL	(ERR-3)
#define SUB_FAIL	(ERR-4)
#define MEM_FAIL	(ERR-5)


#define	BUFFER_SIZE	2048	/* stream-buffer size:  == 1 hd cluster */

#define LINFREE	1	/* entry not in use */
#define LGLOB	2       /* line marked global */

#define MAXLINE	2048	/* max number of chars per line */
#define MAXPAT	 256	/* max number of chars per replacement pattern */
#define MAXFNAME 256	/* max file name size */


/**  Global variables  **/

struct	line {
	int		l_stat;		/* empty, mark */
	struct line	*l_prev;
	struct line	*l_next;
	char		l_buff[1];
};
typedef struct line	LINE;

extern struct object *command_giver;

#ifndef toupper
extern int toupper PROT((int));
#endif

static int doprnt PROT((int, int));
static int ins PROT((char *));
static int deflt PROT((int, int));
static void print_help PROT((int arg));
static void print_help2 PROT((void));
static void count_blanks PROT((int line));
static void _count_blanks PROT((char *str, int blanks));

#define P_DIAG		(ED_BUFFER->diag)
#define P_TRUNCFLG	(ED_BUFFER->truncflg)
#define P_NONASCII	(ED_BUFFER->nonascii)
#define P_NULLCHAR	(ED_BUFFER->nullchar)
#define P_TRUNCATED	(ED_BUFFER->truncated)
#define P_FNAME		(ED_BUFFER->fname)
#define P_FCHANGED	(ED_BUFFER->fchanged)
#define P_NOFNAME	(ED_BUFFER->nofname)
#define P_MARK		(ED_BUFFER->mark)
#define P_OLDPAT	(ED_BUFFER->oldpat)
#define P_LINE0		(ED_BUFFER->Line0)
#define P_LINE0		(ED_BUFFER->Line0)
#define P_CURLN		(ED_BUFFER->CurLn)
#define P_CURPTR	(ED_BUFFER->CurPtr)
#define P_LASTLN	(ED_BUFFER->LastLn)
#define P_LINE1		(ED_BUFFER->Line1)
#define P_LINE2		(ED_BUFFER->Line2)
#define P_NLINES	(ED_BUFFER->nlines)
#define P_SHIFTWIDTH	(ED_BUFFER->shiftwidth)
/* shiftwidth is meant to be a 4-bit-value that can be packed into an int
   along with flags, therefore masks 0x1 ... 0x8 are reserved.           */
#define P_FLAGS 	(ED_BUFFER->flags)
#define NFLG_MASK	0x0010
#define P_NFLG		( P_FLAGS & NFLG_MASK )
#define LFLG_MASK	0x0020
#define P_LFLG		( P_FLAGS & LFLG_MASK )
#define PFLG_MASK	0x0040
#define P_PFLG		( P_FLAGS & PFLG_MASK )
#define EIGHTBIT_MASK	0x0080
#define P_EIGHTBIT	( P_FLAGS & EIGHTBIT_MASK )
#define AUTOINDFLG_MASK	0x0100
#define P_AUTOINDFLG	( P_FLAGS & AUTOINDFLG_MASK )
#define EXCOMPAT_MASK	0x0200
#define P_EXCOMPAT	( P_FLAGS & EXCOMPAT_MASK )
#define TABINDENT_MASK	0x0400
#define P_TABINDENT	( P_FLAGS & TABINDENT_MASK )
#define SMALLNUMBER_MASK	0x0800
#define P_SMALLNUMBER	( P_FLAGS & SMALLNUMBER_MASK )
#define SHIFTWIDTH_MASK	0x000f
#define ALL_FLAGS_MASK	0x0ff0
#define P_APPENDING	(ED_BUFFER->appending)
#define P_MORE		(ED_BUFFER->moring)
#define P_LEADBLANKS	(ED_BUFFER->leading_blanks)
#define P_CUR_AUTOIND   (ED_BUFFER->cur_autoindent)
#define ED_BUFFER       (current_ed_buffer)
#define EXTERN_ED_BUFFER (O_GET_SHADOW(command_giver)->ed_buffer)
static struct ed_buffer *current_ed_buffer;


static char	inlin[MAXLINE];
static char	*inptr;		/* tty input buffer */
struct ed_buffer {
	int	diag;		/* diagnostic-output? flag */
	int	truncflg;	/* truncate long line flag */
	int	nonascii;	/* count of non-ascii chars read */
	int	nullchar;	/* count of null chars read */
	int	truncated;	/* count of lines truncated */
	char	fname[MAXFNAME];
	int	fchanged;	/* file-changed? flag */
	int	nofname;
	int	mark['z'-'a'+1];
	regexp	*oldpat;
	
	LINE	Line0;
	int	CurLn;
	LINE	*CurPtr;	/* CurLn and CurPtr must be kept in step */
	int	LastLn;
	int	Line1, Line2, nlines;
	int	flags;
#if 0
	int	eightbit;	/* save eighth bit */
	int	nflg;		/* print line number flag */
	int	lflg;		/* print line in verbose mode */
	int	pflg;		/* print current line after each command */
#endif
	int	appending;
	int     moring;         /* used for the wait line of help */
	char    *exit_fn;	/* Function to be called when player exits */
	struct object *exit_ob; /* in this object */
	int	shiftwidth;
	int	leading_blanks;
	int	cur_autoindent;
	struct svalue old_prompt;
};

static struct tbl {
	char	*t_str;
	int	t_and_mask;
	int	t_or_mask;
} *t, tbl[] = {
	{  "number",		~FALSE,		NFLG_MASK,	},
	{  "nonumber",		~NFLG_MASK,	FALSE,		},
	{  "list",		~FALSE,		LFLG_MASK,	},
	{  "nolist",		~LFLG_MASK,	FALSE,		},
	{  "print",		~FALSE, 	PFLG_MASK,	},
	{  "noprint",		~PFLG_MASK,	FALSE,		},
	{  "eightbit",		~FALSE,		EIGHTBIT_MASK,	},
	{  "noeightbit",	~EIGHTBIT_MASK,	FALSE,		},
	{  "autoindent",	~FALSE,		AUTOINDFLG_MASK,},
	{  "noautoindent",	~AUTOINDFLG_MASK, FALSE,	},
	{  "excompatible",	~FALSE,		EXCOMPAT_MASK,	},
	{  "noexcompatible",	~EXCOMPAT_MASK,	FALSE,		},
	{  "tabindent", 	~FALSE,		TABINDENT_MASK,	},
	{  "notabindent",	~TABINDENT_MASK,FALSE,		},
	{  "smallnumber", 	~FALSE,		SMALLNUMBER_MASK,},
	{  "nosmallnumber",	~SMALLNUMBER_MASK,FALSE,	},
	{  0							},
};


/*-------------------------------------------------------------------------*/

static  LINE	*getptr();
extern	char	*gettxt();
extern	char	*gettxtl();
extern	char	*catsub();
static  void	prntln(), putcntl();
static regexp	*optpat();


/*________  Macros  ________________________________________________________*/

#ifndef max
#  define max(a,b)	((a) > (b) ? (a) : (b))
#endif

#ifndef min
#  define min(a,b)	((a) < (b) ? (a) : (b))
#endif

#define nextln(l)	((l)+1 > P_LASTLN ? 0 : (l)+1)
#define prevln(l)	((l)-1 < 0 ? P_LASTLN : (l)-1)

#define gettxtl(lin)	((lin)->l_buff)
#define gettxt(num)	(gettxtl( getptr(num) ))

#define getnextptr(p)	((p)->l_next)
#define getprevptr(p)	((p)->l_prev)

#define _setCurLn( lin )	( (P_CURPTR = getptr( (lin) )), P_CURLN = (lin) )
static int ed_tmp;
#define setCurLn( lin )	( (P_CURPTR = getptr( ed_tmp = (lin) )), P_CURLN = ed_tmp )
#define nextCurLn()	( P_CURLN = nextln(P_CURLN), P_CURPTR = getnextptr( P_CURPTR ) )
#define prevCurLn()	( P_CURLN = prevln(P_CURLN), P_CURPTR = getprevptr( P_CURPTR ) )

#define clrbuf()	del(1, P_LASTLN)

#define	Skip_White_Space	{while (*inptr==SP || *inptr==HT) inptr++;}

#define relink(a, x, y, b) { (x)->l_prev = (a); (y)->l_next = (b); }


/*________  functions  ______________________________________________________*/


/*	append.c	*/


static
int append(line, glob)
int	line, glob;
{
	if(glob)
		return(ERR);
	_setCurLn( line );
	P_APPENDING = 1;
	if(P_NFLG)
		add_message(P_SMALLNUMBER ? "%3d " : "%6d. ",P_CURLN+1);
	if (P_CUR_AUTOIND)
	    add_message("%*s",P_LEADBLANKS,"");
	set_prompt("*\b");
	return 0;
}

static INLINE /* only used once */
int more_append(str)
	char *str;
{
	if(str[0] == '.' && str[1] == '\0') {
		P_APPENDING = 0;
		set_prompt(":");
		return(0);
	}
	if(P_NFLG)
		add_message(P_SMALLNUMBER ? "%3d " : "%6d. ",P_CURLN+2);
	if ( P_CUR_AUTOIND )
	{
		int i;
		int less_indent_flag = 0;

		while ( *str=='\004' || *str == '\013' )
		{
			str++;
			P_LEADBLANKS-=P_SHIFTWIDTH;
			if ( P_LEADBLANKS < 0 ) P_LEADBLANKS=0;
			less_indent_flag=1;
		}
		for ( i=0; i < P_LEADBLANKS; ) inlin[i++]=' ';
		strncpy(inlin+P_LEADBLANKS,str,MAXLINE-P_LEADBLANKS);
		inlin[MAXLINE-1]='\0';
		_count_blanks(inlin,0);
		add_message("%*s",P_LEADBLANKS,"");
		if ( !*str && less_indent_flag ) return 0;
		str=inlin;
	}
	if( ins(str) < 0)
		return( MEM_FAIL );
	return 0;
}

void prompt_from_ed_buffer(ip)
    struct interactive *ip;
{
    struct ed_buffer *ed_buffer;

    if (ed_buffer = ip->sent.ed_buffer) {
	transfer_svalue(&ip->prompt, &ed_buffer->old_prompt);
	ed_buffer->old_prompt.type = T_INVALID;
    }
}

void prompt_to_ed_buffer(ip)
    struct interactive *ip;
{
    struct ed_buffer *ed_buffer;

    if (ed_buffer = ip->sent.ed_buffer) {
	transfer_svalue(&ed_buffer->old_prompt, &ip->prompt);
	ip->prompt.type = T_STRING;
	ip->prompt.x.string_type = STRING_CONSTANT;
	ip->prompt.u.string = ed_buffer->appending ? "*\b" : ":";
    }
}

static void count_blanks(line)
	int line;
{
	_count_blanks(gettxtl(getptr(line)), 0);
}

static void _count_blanks(str,blanks)
	char *str;
	int blanks;
{
	for ( ; *str; str++ )
	{
		if ( *str == ' ' ) blanks++;
		else if ( *str == '\t' ) blanks += 8 - blanks % 8 ;
		else break;
	}
	P_LEADBLANKS = blanks<MAXLINE ? blanks : MAXLINE ;
}

/*	ckglob.c	*/

static INLINE /* only used once */
int ckglob()
{
	regexp	*glbpat;
	char	c, delim, *lin;
	int	num;
	LINE	*ptr;

	c = *inptr;

	if(c != 'g' && c != 'v')
		return(0);
	if (deflt(1, P_LASTLN) < 0)
		return(ERR);

	delim = *++inptr;
	if(delim <= ' ')
		return(ERR);

	glbpat = optpat();
	if(*inptr == delim)
		inptr++;
	ptr = getptr(1);
	for (num=1; num<=P_LASTLN; num++) {
		ptr->l_stat &= ~LGLOB;
		if (P_LINE1 <= num && num <= P_LINE2) {
			/* we might have got a NULL pointer if the
			   supplied pattern was invalid		   */
			if (glbpat) {
				lin = gettxtl(ptr);
				if(regexec(glbpat, lin, lin)) {
					if (c=='g') ptr->l_stat |= LGLOB;
				} else {
					if (c=='v') ptr->l_stat |= LGLOB;
				}
			}
		}
		ptr = getnextptr(ptr);
	}
	return(1);
}


/*  deflt.c
 *	Set P_LINE1 & P_LINE2 (the command-range delimiters) if none of them
 *      was supplied with the command; Test whether they have valid values.
 */

static
int deflt(def1, def2)
int	def1, def2;
{
	if(P_NLINES == 0) {
		P_LINE1 = def1;
		P_LINE2 = def2;
	}
	return ( (P_LINE1>P_LINE2 || P_LINE1<=0) ? ERR : 0 );
}


/*	del.c	*/

/* One of the calls to this function tests its return value for an error
 * condition.  But del doesn't return any error value, and it isn't obvious
 * to me what errors might be detectable/reportable.  To silence a warning
 * message, I've added a constant return statement. -- RAM
 * ... It could check to<=P_LASTLN ... igp
 */

static
int del(from, to)
int	from, to;
{
	LINE	*first, *last, *next, *tmp;

	if(from < 1)
		from = 1;
	first = getprevptr( getptr( from ) );
	P_CURLN = prevln(from);
	P_CURPTR = first;
	last = getnextptr( getptr( to ) );
	next = first->l_next;
	while(next != last && next != &P_LINE0) {
		tmp = next->l_next;
		xfree((char *)next);
		next = tmp;
	}
	relink(first, last, first, last);
	P_LASTLN -= (to - from)+1;
	return(0);
}


static INLINE /* only used once */
int dolst(line1, line2)
int line1, line2;
{
	int oldflags=P_FLAGS, p;

	P_FLAGS |= LFLG_MASK;
	p = doprnt(line1, line2);
	P_FLAGS = oldflags;
	return p;
}


#if 0
/*	esc.c
 * Map escape sequences into their equivalent symbols.  Returns the
 * correct ASCII character.  If no escape prefix is present then s
 * is untouched and *s is returned, otherwise **s is advanced to point
 * at the escaped character and the translated character is returned.
 */
int esc(s)
char	**s;
{
	register int	rval;

	if (**s != ESCAPE) {
		rval = **s;
	} else {
		(*s)++;
		switch(islower(**s) ? toupper(**s) : **s) {
		case '\000':
			rval = ESCAPE;	break;
		case 'S':
			rval = ' ';	break;
		case 'N':
			rval = '\n';	break;
		case 'T':
			rval = '\t';	break;
		case 'B':
			rval = '\b';	break;
		case 'R':
			rval = '\r';	break;
		default:
			rval = **s;	break;
		}
	}
	return (rval);
}
#endif /* 0 for unused function esc */


/*	doprnt.c	*/

static
int doprnt(from, to)
int	from, to;
{
	from = (from < 1) ? 1 : from;
	to = (to > P_LASTLN) ? P_LASTLN : to;

	if(to != 0) {
		_setCurLn( from );
		while( P_CURLN <= to ) {
			prntln( gettxtl( P_CURPTR ), P_LFLG, (P_NFLG ? P_CURLN : 0));
			if( P_CURLN == to )
				break;
			nextCurLn();
		}
	}
	return(0);
}


static
void prntln(str, vflg, lin)
char	*str;
int	vflg, lin;
{
	if(lin)
		add_message(P_SMALLNUMBER ? "%3d " : "%7d " ,lin);
	while(*str && *str != NL) {
		if(*str < ' ' || *str >= 0x7f) {
			switch(*str) {
			case '\t':
				if(vflg)
					putcntl(*str);
				else
					add_message("%c", *str);
				break;

			case DEL:
				add_message("^?");
				break;

			default:
				putcntl(*str);
				break;
			}
			str++;
		} else {
			char *start;

			start = str;
			do str++; while(*str >= ' ' && *str < 0x7f);
			if (*str)
			    add_message("%.*s", str - start, start);
			else
			    add_message("%s", start);
		}
	}
	if(vflg)
		add_message("$");
	add_message("\n");
}


static
void putcntl(c)
char	c;
{
	add_message("^%c",(c&31)|'@');
}


/*	egets.c	*/

static INLINE /* only used once */
int egets(str,size,stream)
char	*str;
int	size;
FILE	*stream;
{
	int	c, count;
	char	*cp;

	/* assert(size); if there is any chance of size == 0 */
	count = 0;
	cp = str;
	do {
		c = getc(stream);
		if(c == EOF) {
			*cp = EOS;
			if(count)
				add_message("[Incomplete last line]\n");
			return(count);
		}
		else if(c == NL) {
			*cp = EOS;
			return(++count);
		}
		else if (c == 0)
			P_NULLCHAR++;	/* count nulls */
		else {
			if(c > 127) {
				if(!P_EIGHTBIT)		/* if not saving eighth bit */
					c = c&127;	/* strip eigth bit */
				P_NONASCII++;		/* count it */
			}
			*cp++ = c;	/* not null, keep it */
			count++;
		}
	} while (size > count);
	str[count-1] = EOS;
	if(c != NL) {
		add_message("truncating line\n");
		P_TRUNCATED++;
		while((c = getc(stream)) != EOF)
			if(c == NL)
				break;
	}
	return(count);
}  /* egets */


static int doread(lin, fname)
int	lin;
char	*fname;
{
	FILE	*fp;
	int	err;
	unsigned long	bytes;
	unsigned int	lines;
	char	str[MAXLINE];

	err = 0;
	P_NONASCII = P_NULLCHAR = P_TRUNCATED = 0;

	if (P_DIAG) add_message("\"%s\" ",fname);
	if( (fp = fopen(fname, "r")) == NULL ) {
		add_message(" isn't readable.\n");
		return( ERR );
	}
	_setCurLn( lin );
	for(lines = 0, bytes = 0;(err = egets(str,MAXLINE,fp)) > 0;) {
		bytes += err;
		if(ins(str) < 0) {
			err = MEM_FAIL;
			break;
		}
		lines++;
	}
	fclose(fp);
	if(err < 0)
		return(err);
	if (P_DIAG) {
		add_message("%u lines %lu bytes",lines,bytes);
		if(P_NONASCII)
			add_message(" [%d non-ascii]",P_NONASCII);
		if(P_NULLCHAR)
			add_message(" [%d nul]",P_NULLCHAR);
		if(P_TRUNCATED)
			add_message(" [%d lines truncated]",P_TRUNCATED);
		add_message("\n");
	}
	return( err );
}  /* doread */


static int dowrite(from, to, fname, apflg)
int	from, to;
char	*fname;
int	apflg;
{
	FILE	*fp;
	int	lin, err;
	unsigned int	lines;
	unsigned long	bytes;
	char	*str;
	LINE	*lptr;

	err = 0;
	lines = bytes = 0;

	add_message("\"%s\" ",fname);
	if((fp = fopen(fname,(apflg?"a":"w"))) == NULL) {
		add_message(" can't be opened for writing!\n");
		return( ERR );
	}

	lptr = getptr(from);
	for(lin = from; lin <= to; lin++) {
		str = lptr->l_buff;
		lines++;
		bytes += strlen(str) + 1;	/* str + '\n' */
		if(fputs(str, fp) == EOF) {
			add_message("file write error\n");
			err++;
			break;
		}
		fputc('\n', fp);
		lptr = lptr->l_next;
	}
	add_message("%u lines %lu bytes\n",lines,bytes);
	fclose(fp);
	return( err );
}  /* dowrite */


/*	find.c	*/

static INLINE /* only used once */
int find(pat, dir)
regexp	*pat;
int	dir;
{
	int	i, num;
	LINE	*lin;

    	dir ? nextCurLn() : prevCurLn() ;
	num = P_CURLN;
	lin = P_CURPTR;
	/* if the typed in pattern was invalid we have a NULL pointer! */
	if (!pat) return ( ERR );
	for(i=0; i<P_LASTLN; i++ ) {
		char *line_start = gettxtl( lin );
		if( regexec(pat, line_start, line_start) )
			return(num);
		if( dir )
			num = nextln(num), lin = getnextptr(lin);
		else
			num = prevln(num), lin = getprevptr(lin);
	}
	return ( ERR );
}

#if 0
/*	findg.c by Ted Gaunt for global searches....	much like 'grep' 
	especially useful when line numbering is turned on.
*/
int findg(pat, dir)
regexp	*pat;
int	dir;
{
    int	i, num,count;
    LINE	*lin;
    
    count=0;
    num = P_CURLN;
    lin = P_CURPTR;
    for(i=0; i<P_LASTLN; i++ ) {
	if(regexec( pat, gettxtl( lin ), gettxtl( lin ) ))
	    {prntln( gettxtl( lin ), P_LFLG, (P_NFLG ? P_CURLN : 0));
	     count++;}
	if( dir )
	    num = nextln(num), lin = getnextptr(lin);
	else
	    num = prevln(num), lin = getprevptr(lin);
	nextCurLn();
    }
    if (!count)
	return ( ERR );
    else return (count);
}
#endif /* 0 */

/*	getfn.c	*/

static
char *getfn(writeflg)
int writeflg;
{
    extern int out_of_memory;

    static char	file[MAXFNAME];
    char	*cp;
    char *file2;
    struct svalue *ret;
    
    if(*inptr == NL) {
	P_NOFNAME=TRUE;
	file[0] = '/';
	strcpy(file+1, P_FNAME);
    } else {
	P_NOFNAME=FALSE;
	Skip_White_Space;
	
	cp = file;
	while(*inptr && *inptr != NL && *inptr != SP && *inptr != HT)
	    *cp++ = *inptr++;
	*cp = '\0';
	
    }
    if(strlen(file) == 0) {
	add_message("bad file name\n");
	return( NULL );
    }
    if (file[0] != '/') {
	push_string_malloced(file);
	ret = apply_master_ob("make_path_absolute", 1);
	if (!ret || (ret->type == T_NUMBER && ret->u.number == 0)) {
	    if (out_of_memory)
		error("Out of memory\n");
	    return NULL;
	}
	if (ret->type == T_STRING)
	    strncpy(file, ret->u.string, sizeof file - 1);
    }
    /* add_message() / apply() might have nasty effects */
    if (!command_giver || command_giver->flags & O_DESTRUCTED)
	return NULL;
    file2 = check_valid_path(file, command_giver, "ed_start", writeflg);
	if (!file2)
	    return( NULL );
	strncpy(file, file2, MAXFNAME-1);
	file[MAXFNAME-1] = 0;

	if(strlen(file) == 0) {
		add_message("no file name\n");
		return(NULL);
	}
	return( file );
}  /* getfn */


static
int getnum(first)
int first;
{
	regexp	*srchpat;
	int	num;
	char	c;

	Skip_White_Space;

	if(*inptr >= '0' && *inptr <= '9') {	/* line number */
		for(num = 0; *inptr >= '0' && *inptr <= '9'; ++inptr) {
			num = (num * 10) + (*inptr - '0');
		}
		return num;
	}

	switch(c = *inptr) {
	case '.':
		inptr++;
		return (P_CURLN);

	case '$':
		inptr++;
		return (P_LASTLN);

	case '/':
	case '?':
		srchpat = optpat();
		if(*inptr == c)
			inptr++;
		return(find(srchpat,c == '/'?1:0));

#if 0
      case '^':			/* for grep-like searching */
      case '&':
	srchpat = optpat();
	if(*inptr == c)
	    inptr++;
	return(findg(srchpat,c == '^'?1:0));
#endif
	
	case '-':
	case '+':
		return(first ? P_CURLN : 1);

	case '\'':
		inptr++;
		if (*inptr < 'a' || *inptr > 'z')
			return(EOF);
		return P_MARK[ *inptr++ - 'a' ];

	default:
		return ( first ? EOF : 1 );	/* unknown address */
	}
}  /* getnum */


/*  getone.c
 *	Parse a number (or arithmetic expression) off the command line.
 */
#define FIRST 1
#define NOTFIRST 0

static int getone()
{
	int	c, i, num;

	if((num = getnum(FIRST)) >= 0) {
		for (;;) {
			Skip_White_Space;
			if(*inptr != '+' && *inptr != '-')
				break;	/* exit infinite loop */

                        c = *inptr++;
			if((i = getnum(NOTFIRST)) < 0)
				return ( i );
			if(c == '+')
				num += i;
			else
				num -= i;
		}
	}
	return ( num>P_LASTLN ? ERR : num );
}  /* getone */


static int getlst()
{
	int	num;

	P_LINE2 = 0;
	for(P_NLINES = 0; (num = getone()) >= 0;)
	{
		P_LINE1 = P_LINE2;
		P_LINE2 = num;
		P_NLINES++;
		if(*inptr != ',' && *inptr != ';')
			break;
		if(*inptr == ';')
			_setCurLn( num );
		inptr++;
	}
	P_NLINES = min(P_NLINES, 2);
	if(P_NLINES == 0)
		P_LINE2 = P_CURLN;
	if(P_NLINES <= 1)
		P_LINE1 = P_LINE2;

	return ( (num == ERR) ? num : P_NLINES );
}  /* getlst */


/*	getptr.c	*/

static
LINE *getptr(num)
int	num;
{
    LINE *ptr;
    int j, cur;

    cur = P_CURLN;
    if (num >= cur) {
	if (2*num - cur > P_LASTLN && num <= P_LASTLN) {
	    /* high line numbers */
	    ptr = P_LINE0.l_prev;
	    for (j = P_LASTLN - num; --j >= 0; )
		ptr = ptr->l_prev;
	} else {
	    ptr = P_CURPTR;
	    for (j = num - cur; --j >= 0; )
		ptr = ptr->l_next;
	}
    } else {
	if (2*num <= cur) {
	    /* low line numbers */
		ptr = &P_LINE0;
		for (j = num; --j >= 0; )
			ptr = ptr->l_next;
	} else {
	    ptr = P_CURPTR;
	    for (j = cur - num; --j >= 0; )
		ptr = ptr->l_prev;
	}
    }
    return(ptr);
}


/*	getrhs.c	*/

static INLINE /* only used once */
int getrhs(sub)
char	*sub;
{
	char delim = *inptr++;
	char *outmax = sub + MAXPAT;
	if( delim == NL || *inptr == NL)	/* check for eol */
		return( ERR );
	while( *inptr != delim && *inptr != NL ) {
		if ( sub > outmax )
			return ERR;
		if ( *inptr == ESCAPE ) {
			switch ( *++inptr ) {
			case 'r':
				*sub++ = '\r';
				inptr++;
				break;
#if 0
			case ESCAPE:
				*sub++ = ESCAPE;
				*sub++ = ESCAPE;
				inptr++;
#endif
			case 'n':
				*sub++ = '\n';
				inptr++;
				break;
			case 'b':
				*sub++ = '\b';
				inptr++;
				break;
			case 't':
				*sub++ = '\t';
				inptr++;
				break;
			case '0': {
				int i=3;
				*sub = 0;
				do {
					if (*++inptr<'0' || *inptr >'7')
						break;
					*sub = (*sub<<3) | (*inptr-'0');
				} while (--i!=0);
				sub++;
				} break;
#if 0
			default:
				if ( *inptr != delim )
					*sub++ = ESCAPE;
#else
			case '&':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '\\':
				*sub++ = ESCAPE; /* fall through */
			default:
#endif
				*sub++ = *inptr;
				if ( *inptr != NL )
					inptr++;
			}
		}
		else *sub++ = *inptr++;
	}
	*sub = '\0';

	inptr++;		/* skip over delimter */
	Skip_White_Space;
	if(*inptr == 'g') {
		inptr++;
		return( 1 );
	}
	return( 0 );
}

/*	ins.c	*/

static
int ins(str)
char	*str;
{
	char	*cp;
	LINE	*new, *nxt;
	int	len;

	do {
		for ( cp = str; *cp && *cp != NL; cp++ )
			;
		len = cp - str;
		/* cp now points to end of first or only line */

		if((new = (LINE *)xalloc(sizeof(LINE)+len)) == NULL)
			return( MEM_FAIL ); 	/* no memory */

		new->l_stat=0;
		strncpy(new->l_buff,str,len);	/* build new line */
		new->l_buff[len] = EOS;
		nxt = getnextptr(P_CURPTR);	/* get next line */
		relink(P_CURPTR, new, new, nxt);	/* add to linked list */
		relink(new, nxt, P_CURPTR, new);
		P_LASTLN++;
		P_CURLN++;
		P_CURPTR = new;
		str = cp + 1;
	}
		while( *cp != EOS );
	return 1;
}


/*	join.c	*/

static INLINE /* only used once */
int join(first, last)
int first, last;
{
	char buf[MAXLINE];
	char *cp=buf, *str;
	LINE *lin;
	int num;

	if (first<=0 || first>last || last>P_LASTLN)
		return(ERR);
	if (first==last) {
		_setCurLn( first );
		return 0;
	}
	lin = getptr(first);
	for (num=first; num<=last; num++) {
		str=gettxtl(lin);
		while ( *str ) {
			if (cp >= buf + MAXLINE-1 ) {
				add_message("line too long\n");
				return(ERR);
			}
			*cp++ = *str++;
		}
		lin = getnextptr(lin);
	}
	*cp = EOS;
	del(first, last);
	if( ins(buf) < 0 )
		return MEM_FAIL;
	P_FCHANGED = TRUE;
	return 0;
}


/*  move.c
 *	Unlink the block of lines from P_LINE1 to P_LINE2, and relink them
 *	after line "num".
 */

static INLINE /* only used once */
int move(num)
int	num;
{
	LINE	*dest, *before, *first, *last, *after;

	dest = getptr(num);
	if (num < P_LINE1)
		num += P_LINE2 - P_LINE1 + 1;
	else if (num <= P_LINE2)
		return( ERR );
	first = getptr(P_LINE1);
	before = first->l_prev;
	last = getptr(P_LINE2);
	after = last->l_next;

	relink(before, after, before, after);

	before = dest;
	after = dest->l_next;
	relink(before, first, last, after);
	relink(last, after, before, first);
	P_CURPTR = last;
	P_CURLN = num;
	return( 1 );
}


static INLINE /* only used once */
int transfer(num)
int num;
{
    int count1, count2;
    struct line *ptr;

    /* The caller made sure that (P_LINE1 > 0 && P_LINE1 <= P_LINE2)
     * by calling deflt()
     */

    if (num >= P_LINE1 && num < P_LINE2) {
	count1 = num - P_LINE1; /* loop has one iteration more */
	count2 = P_LINE2 - num;
    } else {
	count1 = P_LINE2 - P_LINE1;
	count2 = 0;
    }
    _setCurLn( num );
    ptr = getptr(P_LINE1);
    do {
	if( ins(gettxtl(ptr)) < 0 )
	    return MEM_FAIL;
	ptr = getnextptr(ptr);
    } while (--count1 >= 0);
    if (count2) {
	ptr = getnextptr(P_CURPTR);
	do {
	    if( ins(gettxtl(ptr)) < 0 )
		return MEM_FAIL;
	    ptr = getnextptr(ptr);
	} while(--count2);
    }
    return 1;
}


/*	optpat.c	*/

static
regexp *optpat()
{
	char	delim, str[MAXPAT], *cp;

	delim = *inptr++;
	if (delim == NL)
	    return P_OLDPAT;
	cp = str;
	while(*inptr != delim && *inptr != NL && *inptr != EOS && cp < str + MAXPAT - 1) {
		if(*inptr == ESCAPE && inptr[1] != NL)
			*cp++ = *inptr++;
		*cp++ = *inptr++;
	}

	*cp = EOS;
	if(*str == EOS)
		return(P_OLDPAT);
	if(P_OLDPAT)
		xfree((char *)P_OLDPAT);
	return P_OLDPAT = regcomp(str,P_EXCOMPAT);
}

/* regerror.c */
void regerror( s )
    char *s;
{
	add_message("ed: %s\n", s );
}


static INLINE /* only used once */
int set()
{
	/* the longest valid set keyword is 14 characters long. Add one char
	 * for EOS, and another to get 4-byte-alignmnt
	 */
	char	word[16];
	int	i;

	if(*(++inptr) != 't') {
		if(*inptr != SP && *inptr != HT && *inptr != NL)
			return(ERR);
	} else
		inptr++;

	if ( (*inptr == NL))
	{
		add_message("ed version %d.%d\n", ED_VERSION/100, ED_VERSION%100);
		for(t = tbl; t->t_str; t+=2) {
			add_message(	"%s:%s ",t->t_str,
				P_FLAGS & t->t_or_mask ?"ON":"OFF");
		}
		add_message("\nshiftwidth:%d\n",P_SHIFTWIDTH);
		return(0);
	}

	Skip_White_Space;
	for(i = 0; *inptr != SP && *inptr != HT && *inptr != NL;) {
	    /* leave space for EOS too */
	    if (i == sizeof word - 2) {
		add_message("Too long argument to 'set'!\n");
		return 0;
	    }
	    word[i++] = *inptr++;
	}
	word[i] = EOS;
	for(t = tbl; t->t_str; t++) {
		if(strcmp(word,t->t_str) == 0) {
			P_FLAGS = P_FLAGS & t->t_and_mask | t->t_or_mask;
			return(0);
		}
	}
	if ( !strcmp(word,"save") ) {
		struct svalue *ret;
		push_object(command_giver);
		push_number( P_SHIFTWIDTH | P_FLAGS );
		ret = apply_master_ob("save_ed_setup",2);
		if ( ret && ret->type==T_NUMBER && ret->u.number > 0 )
			return 0;
	}
	if ( !strcmp(word,"shiftwidth") ) {
		Skip_White_Space;
		if ( isdigit(*inptr) ) {
			P_SHIFTWIDTH = *inptr-'0';
			return 0;
		}
	}
	return SET_FAIL;
}

#ifndef relink
void relink(a, x, y, b)
LINE	*a, *x, *y, *b;
{
	x->l_prev = a;
	y->l_next = b;
}
#endif


static INLINE /* only used once */
void set_ed_buf()
{
	relink(&P_LINE0, &P_LINE0, &P_LINE0, &P_LINE0);
	P_CURLN = P_LASTLN = 0;
	P_CURPTR = &P_LINE0;
}


/*	subst.c	*/

static INLINE /* only used once */
int subst(pat, sub, gflg, pflag)
regexp	*pat;
char	*sub;
int	gflg, pflag;
{
	int	nchngd = 0;
	char	*new, *old, buf[MAXLINE];
	int	still_running = 1;
	LINE	*lastline = getptr( P_LINE2 );

	if(P_LINE1 <= 0)
		return( SUB_FAIL );
	nchngd = 0;		/* reset count of lines changed */

	for( setCurLn( prevln( P_LINE1 ) ); still_running; ) {
		char	*start, *current;
		int	space;			/* amylaar */

		nextCurLn();
		new = buf;
		if ( P_CURPTR == lastline )
			still_running = 0;
		current = start = gettxtl(P_CURPTR);
		if ( regexec(pat, current, start) ) {
			space = MAXLINE;	/* amylaar */
			do
				{
				/* Copy leading text */
				int diff = pat->startp[0] - current;
				if ( (space-=diff) < 0 )	/* amylaar */
					return SUB_FAIL;
				strncpy( new, current, diff );
				new += diff;
				/* Do substitution */
				old = new;
				new = regsub( pat, sub, new, space,0);
				if (!new || (space-= new-old) < 0) /* amylaar */
					return SUB_FAIL;
				if (current == pat->endp[0]) { /* amylaar :
				                       prevent infinite loop */
				    if ( !*current ) break;
				    if (--space < 0) return SUB_FAIL;
				    *new++ = *current++;
				} else
				    current = pat->endp[0];
				}
			while(gflg && !pat->reganch && regexec(pat, current, start));

			/* Copy trailing chars */
			/* amylaar : always check for enough space left
			 * BEFORE altering memory
			 */
			if ( (space-= strlen(current)+1 ) < 0 )
				return SUB_FAIL;
			strcpy(new, current);
			del(P_CURLN,P_CURLN);
			if( ins(buf) < 0 )
				return MEM_FAIL;
			nchngd++;
			if(pflag)
				doprnt(P_CURLN, P_CURLN);
		}
        }
	return (( nchngd == 0 && !gflg ) ? SUB_FAIL : nchngd);
}

/*
 * Indent code from DGD editor (v0.1), adapted.  No attempt has been made to
 * optimize for this editor.   Dworkin 920510
 */
/* closure / symbol support Amylaar 30th Sep 1993 */
# define error(s)		{ add_message(s, lineno); errs++; return; }
# define bool char
static int lineno, errs;
static int shi;		/* the current shift (negative for left shift) */
static int full_shift, small_shift;

/*
 * NAME:	shift(char*)
 * ACTION:	Shift a line left or right according to "shi".
 */
/* amylaar: don't use identifier index, this is reserved in SYS V compatible
 *		environments.
 */
static void shift(text)
register char *text;
{
    register int indent_index;

    /* first determine the number of leading spaces */
    indent_index = 0;
    while (*text == ' ' || *text == '\t') {
	if (*text++ == ' ') {
	    indent_index++;
	} else {
	    indent_index = (indent_index + 8) & ~7;
	}
    }

    if (*text != '\0') { /* don't shift lines with ws only */
	indent_index += shi;
	if (indent_index < MAXLINE) {
	    char buffer[MAXLINE];
	    register char *p;

	    p = buffer;
	    /* fill with leading ws */
	    if (P_TABINDENT) while (indent_index >= 8) {
		*p++ = '\t';
		indent_index -= 8;
	    }
	    while (indent_index > 0) {
		*p++ = ' ';
		--indent_index;
	    }
	    if (p - buffer + strlen(text) < MAXLINE) {
		strcpy(p, text);
		del(lineno, lineno);
		ins(buffer);
		return;
	    }
	}

	error("Result of shift would be too long, line %d\n");
    }
}

# define STACKSZ	1024	/* size of indent stack */

/* token definitions in indent */
# define SEMICOLON	0
# define LBRACKET	1
# define RBRACKET	2
# define LOPERATOR	3
# define ROPERATOR	4
# define LHOOK		5
# define LHOOK2		6
# define RHOOK		7
# define TOKEN		8
# define ELSE		9
# define IF		10
# define FOR		11
# define WHILE		12
# define DO		13
# define XEOT		14

static char *stack, *stackbot;	/* token stack */
static int *ind, *indbot;	/* indent stack */
static char quote;		/* ' or " */
static bool in_ppcontrol, in_comment, after_keyword;	/* status */

/*
 * NAME:	indent(char*)
 * ACTION:	Parse and indent a line of text. This isn't perfect, as
 *		keywords could be defined as macros, comments are very hard to
 *		handle properly, (, [ and ({ will match any of ), ] and }),
 *		and last but not least everyone has his own taste of
 *		indentation.
 */
static void indent(buf)
char *buf;
{
/*                      ;  {  }  (  )  [  ([ ]  tok el if fo whi do xe   */
/*                                     (  ({ )  en  se    r  le     ot   */
    static char f[] = { 7, 1, 7, 1, 2, 1, 1, 6, 4,  2, 6, 7, 7,  2, 0, };
    static char g[] = { 2, 2, 1, 7, 1, 5, 5, 1, 3,  6, 2, 2, 2,  2, 0, };
    char text[MAXLINE], ident[MAXLINE];
    register char *p, *sp;
    register int *ip;
    register long indent_index;
    register int top, token;
    char *start;
    bool do_indent;

    /*
     * Problem: in this editor memory for deleted lines is reclaimed. So
     * we cannot shift the line and then continue processing it, as in
     * DGD ed. Instead make a copy of the line, and process the copy.
     * Dworkin 920510
     */
    strcpy(text, buf);

    do_indent = FALSE;
    indent_index = 0;
    p = text;

    /* process status vars */
    if (quote != '\0') {
	shi = 0;	/* in case a comment starts on this line */
    } else if (in_ppcontrol || *p == '#' && p[1] != '\'') {
	while (*p != '\0') {
	    if (*p == '\\' && *++p == '\0') {
		in_ppcontrol = TRUE;
		return;
	    }
	    p++;
	}
	in_ppcontrol = FALSE;
	return;
    } else {
	/* count leading ws */
	while (*p == ' ' || *p == '\t') {
	    if (*p++ == ' ') {
		indent_index++;
	    } else {
		indent_index = (indent_index + 8) & ~7;
	    }
	}
	if (*p == '\0') {
	    del(lineno, lineno);
	    ins(p);
	    return;
	} else if (in_comment) {
	    shift(text);	/* use previous shi */
	} else {
	    do_indent = TRUE;
	}
    }

    /* process this line */
    start = p;
    while (*p != '\0') {

	/* lexical scanning: find the next token */
	ident[0] = '\0';
	if (in_comment) {
	    /* comment */
	    while (*p != '*') {
		if (*p == '\0') {
		    return;
		}
		p++;
	    }
	    while (*p == '*') {
		p++;
	    }
	    if (*p == '/') {
		in_comment = FALSE;
		p++;
	    }
	    continue;

	} else if (quote != '\0') {
	    /* string or character constant */
	    for (;;) {
		if (*p == quote) {
		    quote = '\0';
		    p++;
		    break;
		} else if (*p == '\0') {
		    error("Unterminated string in line %d\n");
		} else if (*p == '\\' && *++p == '\0') {
		    break;
		}
		p++;
	    }
	    token = TOKEN;

	} else {
	    switch (*p++) {
	    case ' ':	/* white space */
	    case '\t':
		continue;

	    case '\'':
		if (isalunum(*p) && p[1] && p[1] != '\'') {
		    do ++p; while (isalunum(*p));
		    token = TOKEN;
		    break;
		}
		if (*p == '(' && p[1] == '{') {
		    /* treat quoted array like an array */
		    token = TOKEN;
		    break;
		}
		/* fall through */
	    case '"':	/* start of string */
		quote = p[-1];
		continue;

	    case '/':
		if (*p == '*') {	/* start of comment */
		    in_comment = TRUE;
		    if (do_indent) {
			/* this line hasn't been indented yet */
			shi = *ind - indent_index;
			shift(text);
			do_indent = FALSE;
		    } else {
			register char *q;
			register int index2;

			/*
			 * find how much the comment has shifted, so the same
			 * shift can be used if the coment continues on the
			 * next line
			 */
			index2 = *ind;
			for (q = start; q < p - 1;) {
			    if (*q++ == '\t') {
				indent_index = (indent_index + 8) & ~7;
				index2 = (index2 + 8) & ~7;
			    } else {
				indent_index++;
				index2++;
			    }
			}
			shi = index2 - indent_index;
		    }
		    p++;
		    continue;
		}
		if (*p == '/') {	/* start of C++ style comment */
		    p = strchr(p, '\0');
		}
		token = TOKEN;
		break;

	    case '{':
		token = LBRACKET;
		break;

	    case '(':
		if (after_keyword) {
		    /*
		     * LOPERATOR & ROPERATOR are a kludge. The operator
		     * precedence parser that is used could not work if
		     * parenthesis after keywords was not treated specially.
		     */
		    token = LOPERATOR;
		    break;
		}
		if (*p == '{' || *p == '[') {
		    p++;	/* ({ , ([ each are one token */
		    token = LHOOK2;
		    break;
		}
	    case '[':
		token = LHOOK;
		break;

	    case '}':
		if (*p != ')') {
		    token = RBRACKET;
		    break;
		}
		/* }) is one token */
		p++;
		token = RHOOK;
		break;
	    case ']':
		if (*p == ')' &&
		  (*stack == LHOOK2 || (*stack != XEOT &&
		    ( stack[1] == LHOOK2 ||
		      ( stack[1] == ROPERATOR && stack[2] == LHOOK2) ) ) ) )
		{
		    p++;
		}
	    case ')':
		token = RHOOK;
		break;

	    case ';':
		token = SEMICOLON;
		break;

	    case '#':
		if (*p == '\'') {
		    ++p;
		    if (isalunum(*p)) {
			do ++p; while (isalunum(*p));
		    } else {
			extern int symbol_operator PROT((char *, char **));

			char *end;

			if (symbol_operator(p, &end) < 0) {
			    error("Missing function name after #' in line %d\n");
			}
			p = end;
		    }
		    token = TOKEN;
		    break;
		}
	    default:
		if (isalpha(*--p) || *p == '_') {
		    register char *q;

		    /* Identifier. See if it's a keyword. */
		    q = ident;
		    do {
			*q++ = *p++;
		    } while (isalnum(*p) || *p == '_');
		    *q = '\0';

		    if      (strcmp(ident, "if"   ) == 0)	token = IF;
		    else if (strcmp(ident, "else" ) == 0)	token = ELSE;
		    else if (strcmp(ident, "for"  ) == 0)	token = FOR;
		    else if (strcmp(ident, "while") == 0)	token = WHILE;
		    else if (strcmp(ident, "do"   ) == 0)	token = DO;
		    else    /* not a keyword */			token = TOKEN;
		} else {
		    /* anything else is a "token" */
		    p++;
		    token = TOKEN;
		}
		break;
	    }
	}

	/* parse */
	sp = stack;
	ip = ind;
	for (;;) {
	    top = *sp;
	    if (top == LOPERATOR && token == RHOOK) {
		/* ) after LOPERATOR is ROPERATOR */
		token = ROPERATOR;
	    }

	    if (f[top] <= g[token]) {	/* shift the token on the stack */
		register int i;

		if (sp == stackbot) {
		    /* out of stack */
		    error("Nesting too deep in line %d\n");
		}

		/* handle indentation */
		i = *ip;
		/* if needed, reduce indentation prior to shift */
		if ((token == LBRACKET &&
		  (*sp == ROPERATOR || *sp == ELSE || *sp == DO)) ||
		  token == RBRACKET ||
		  (token == IF && *sp == ELSE)) {
		    /* back up */
		    i -= full_shift;
		} else if (token == RHOOK || token == ROPERATOR) {
		    i -= small_shift;
		}
		/* shift the current line, if appropriate */
		if (do_indent) {
		    shi = i - indent_index;
		    if (token == TOKEN && *sp == LBRACKET &&
		      (strcmp(ident, "case") == 0 ||
		      strcmp(ident, "default") == 0)) {
			/* back up if this is a switch label */
			shi -= full_shift;
		    }
		    shift(text);
		    do_indent = FALSE;
		}
		/* change indentation after current token */
		switch (token) {
		  case LBRACKET: case ROPERATOR: case ELSE: case DO:
		  {
		    /* add indentation */
		    i += full_shift;
		    break;
		  }
		  case LOPERATOR: case LHOOK: case LHOOK2:
		  {
		    /* half indent after ( [ ({ ([ */
		    i += small_shift;
		    break;
		  }
		  case SEMICOLON:
		  {
		    /* in case it is followed by a comment */
		    if (*sp == ROPERATOR || *sp == ELSE) {
			i -= full_shift;
		    }
		    break;
		  }
		}

		*--sp = token;
		*--ip = i;
		break;
	    }

	    /* reduce handle */
	    do {
		top = *sp++;
		ip++;
	    } while (f[(int)*sp] >= g[top]);
	}
	stack = sp;
	ind = ip;
	after_keyword = (token >= IF);	/* but not after ELSE */
    }
}

static int indent_code(from, to)
    int from, to;
{
    char s[STACKSZ];
    int i[STACKSZ];

    /* setup stacks */
    stackbot = s;
    indbot = i;
    stack = stackbot + STACKSZ - 1;
    *stack = XEOT;
    ind = indbot + STACKSZ - 1;
    *ind = 0;

    quote = '\0';
    in_ppcontrol = FALSE;
    in_comment = FALSE;

    P_FCHANGED = TRUE;
    errs = 0;
    full_shift = P_SHIFTWIDTH;
    small_shift = full_shift / 2;

    for (lineno = from; lineno <= to; lineno++) {
	_setCurLn(lineno);
	indent(gettxtl(P_CURPTR));
	if (errs != 0) {
	    return ERR;
	}
    }

    return 0;
}

# undef bool
# undef error
/* end of indent code */

/*  docmd.c
 *	Perform the command specified in the input buffer, as pointed to
 *	by inptr.  Actually, this finds the command letter first.
 */

static
int docmd(glob)
int	glob;
{
	static char	rhs[MAXPAT];
	regexp	*subpat;
	int	c, err, line3;
	int	apflg, pflag, gflag;
	int	nchng;
	char	*fptr;

	pflag = FALSE;
	Skip_White_Space;

	c = *inptr++;
	switch(c) {
	case NL:
		if( P_NLINES == 0 && (P_LINE2 = nextln(P_CURLN)) == 0 )
			return(ERR);
		setCurLn( P_LINE2 );
		return (1);

	case '=':
		add_message("%d\n",P_LINE2);
		break;

	case 'a':
	case 'A':
		P_CUR_AUTOIND = c=='a' ? P_AUTOINDFLG : !P_AUTOINDFLG;
		if(*inptr != NL || P_NLINES > 1)
			return(ERR);

		if ( P_CUR_AUTOIND ) count_blanks(P_LINE1);
		if(append(P_LINE1, glob) < 0)
			return(ERR);
		P_FCHANGED = TRUE;
		break;

	case 'c':
		if(*inptr != NL)
			return(ERR);

		if(deflt(P_CURLN, P_CURLN) < 0)
			return(ERR);

		P_CUR_AUTOIND = P_AUTOINDFLG;
		if ( P_AUTOINDFLG ) count_blanks(P_LINE1);
		if(del(P_LINE1, P_LINE2) < 0)
			return(ERR);
		if(append(P_CURLN, glob) < 0)
			return(ERR);
		P_FCHANGED = TRUE;
		break;

	case 'd':
		if(*inptr != NL)
			return(ERR);

		if(deflt(P_CURLN, P_CURLN) < 0)
			return(ERR);

		if(del(P_LINE1, P_LINE2) < 0)
			return(ERR);
		if(nextln(P_CURLN) != 0)
			nextCurLn();
		if (P_PFLG)
			doprnt(P_CURLN, P_CURLN);
		P_FCHANGED = TRUE;
		break;

	case 'e':
		if(P_NLINES > 0)
			return(ERR);
		if(P_FCHANGED)
			return CHANGED;
		/*FALL THROUGH*/
	case 'E':
		if(P_NLINES > 0)
			return(ERR);

		if(*inptr != ' ' && *inptr != HT && *inptr != NL)
			return(ERR);

		if((fptr = getfn(0)) == NULL)
			return(ERR);

		clrbuf();
		(void)doread(0, fptr);

		strcpy(P_FNAME, fptr);
		P_FCHANGED = FALSE;
		break;

	case 'f':
		if(P_NLINES > 0)
			return(ERR);

		if(*inptr != ' ' && *inptr != HT && *inptr != NL)
			return(ERR);

		fptr = getfn(0);

		if (P_NOFNAME)
			add_message("%s\n", P_FNAME);
		else {
			if(fptr == NULL) return(ERR);
			strcpy(P_FNAME, fptr);
		}
		break;

	case 'i':
		if(*inptr != NL || P_NLINES > 1)
			return(ERR);

		P_CUR_AUTOIND = P_AUTOINDFLG;
		if ( P_AUTOINDFLG ) count_blanks(P_LINE1);
		if(append(prevln(P_LINE1), glob) < 0)
			return(ERR);
		P_FCHANGED = TRUE;
		break;

	case 'j':
		if (*inptr != NL || deflt(P_CURLN, P_CURLN+1)<0)
			return(ERR);

		if (join(P_LINE1, P_LINE2) < 0)
			return(ERR);
		break;

	case 'k':
		Skip_White_Space;

		if (*inptr < 'a' || *inptr > 'z')
			return ERR;
		c= *inptr++;

		if(*inptr != ' ' && *inptr != HT && *inptr != NL)
			return(ERR);

		P_MARK[c-'a'] = P_LINE1;
		break;

	case 'l':
		if(*inptr != NL)
			return(ERR);
		if(deflt(P_CURLN,P_CURLN) < 0)
			return(ERR);
		if (dolst(P_LINE1,P_LINE2) < 0)
			return(ERR);
		break;

	case 'm':
		if((line3 = getone()) < 0)
			return(ERR);
		if(deflt(P_CURLN,P_CURLN) < 0)
			return(ERR);
		if(move(line3) < 0)
			return(ERR);
		P_FCHANGED = TRUE;
		break;

      case 'n':
	if (P_NFLG)
	    P_FLAGS &= ~( NFLG_MASK | LFLG_MASK );
	else
	    P_FLAGS |=  ( NFLG_MASK | LFLG_MASK );
	P_DIAG=!P_DIAG;
	add_message(	"number %s, list %s\n",
		    P_NFLG?"ON":"OFF",
		    P_LFLG?"ON":"OFF");
	break;
		
      case 'I':
	if(deflt(1, P_LASTLN) < 0)
	    return(ERR);
	if(*inptr != NL)
	    return(ERR);
	if (!P_NLINES) add_message("Indenting entire code...\n");
	if (indent_code(P_LINE1, P_LINE2))
	    add_message("Indention halted.\n");
	else 
	    add_message("Done indenting.\n");
	break;

      case 'H':
      case 'h': 
	print_help(*(inptr++));
	break;

	case 'P':
	case 'p':
		if(*inptr != NL)
			return(ERR);
		if(deflt(P_CURLN,P_CURLN) < 0)
			return(ERR);
		if(doprnt(P_LINE1,P_LINE2) < 0)
			return(ERR);
		break;

	case 'q':
		if(P_FCHANGED)
			return CHANGED;
		/*FALL THROUGH*/
	case 'Q':
		clrbuf();
		if(*inptr == NL && P_NLINES == 0 && !glob)
			return(EOF);
		else
			return(ERR);

	case 'r':
		if(P_NLINES > 1)
			return(ERR);

		if(P_NLINES == 0)		/* The original code tested */
			P_LINE2 = P_LASTLN;	/*	if(P_NLINES = 0)    */
						/* which looks wrong.  RAM  */

		if(*inptr != ' ' && *inptr != HT && *inptr != NL)
			return(ERR);

		if((fptr = getfn(0)) == NULL)
			return(ERR);

		if((err = doread(P_LINE2, fptr)) < 0)
			return(err);
		P_FCHANGED = TRUE;
		break;

	case 's':
		if(*inptr == 'e')
			return(set());
		Skip_White_Space;
		if((subpat = optpat()) == NULL)
			return(ERR);
		if((gflag = getrhs(rhs)) < 0)
			return(ERR);
		if(*inptr == 'p')
			pflag++;
		if(deflt(P_CURLN, P_CURLN) < 0)
			return(ERR);
		if((nchng = subst(subpat, rhs, gflag, pflag)) < 0)
			return(ERR);
		if(nchng)
			P_FCHANGED = TRUE;
		if ( nchng==1 && P_PFLG ) {
			if(doprnt(P_CURLN, P_CURLN) < 0)
			return(ERR);
		}
		break;

	case 't':
		if((line3 = getone()) < 0)
			return(ERR);
		if(deflt(P_CURLN,P_CURLN) < 0)
			return(ERR);
		if(transfer(line3) < 0)
			return(ERR);
		P_FCHANGED = TRUE;
		break;

	case 'W':
	case 'w':
		apflg = (c=='W');

		if(*inptr != ' ' && *inptr != HT && *inptr != NL)
			return(ERR);

		if((fptr = getfn(1)) == NULL)
			return(ERR);

		if(deflt(1, P_LASTLN) < 0)
			return(ERR);
		if(dowrite(P_LINE1, P_LINE2, fptr, apflg) < 0)
			return(ERR);
		P_FCHANGED = FALSE;
		break;

	case 'x':
		if(*inptr == NL && P_NLINES == 0 && !glob) {
			if((fptr = getfn(1)) == NULL)
				return(ERR);
			if(dowrite(1, P_LASTLN, fptr, 0) >= 0 &&
			   command_giver && command_giver->flags & O_SHADOW)
				return(EOF);
		}
		return(ERR);

	case 'z':
		if(deflt(P_CURLN,P_CURLN) < 0)
			return(ERR);

		switch(*inptr) {
		case '-':
			if(doprnt(P_LINE1-21,P_LINE1) < 0)
				return(ERR);
			break;

		case '.':
			if(doprnt(P_LINE1-11,P_LINE1+10) < 0)
				return(ERR);
			break;

		case '+':
		case '\n':
			if(doprnt(P_LINE1,P_LINE1+21) < 0)
				return(ERR);
			break;
		}
		break;

      case 'Z':
	if(deflt(P_CURLN,P_CURLN) < 0)
	    return(ERR);
	
	switch(*inptr) {
	  case '-':
	    if(doprnt(P_LINE1-41,P_LINE1) < 0)
		return(ERR);
	    break;
	    
	  case '.':
	    if(doprnt(P_LINE1-21,P_LINE1+20) < 0)
		return(ERR);
	    break;
	    
	  case '+':
	  case '\n':
	    if(doprnt(P_LINE1,P_LINE1+41) < 0)
		return(ERR);
	    break;
	}
	break;	
	default:
		return(ERR);
	}

	return (0);
}  /* docmd */


/*	doglob.c	*/
static INLINE /* only used once */
int doglob()
{
	int	lin, status;
	char	*cmd;
	LINE	*ptr;

	cmd = inptr;

	for (;;) {
		ptr = getptr(1);
		for (lin=1; lin<=P_LASTLN; lin++) {
			if (ptr->l_stat & LGLOB)
				break;
			ptr = getnextptr(ptr);
		}
		if (lin > P_LASTLN)
			break;

		ptr->l_stat &= ~LGLOB;
		P_CURLN = lin; P_CURPTR = ptr;
		inptr = cmd;
		if((status = getlst()) < 0)
			return(status);
		if((status = docmd(1)) < 0)
			return(status);
	}
	return(P_CURLN);
}  /* doglob */


/*
 * Start the editor. Because several players can edit simultaneously,
 * they will each need a separate editor data block.
 *
 * If an exit_fn and exit_ob is given, then call exit_ob->exit_fn at
 * exit of editor. The purpose is to make it possible for external LPC
 * code to maintain a list of locked files.
 */
void ed_start(file_arg, exit_fn, exit_ob)
	char *file_arg;
	char *exit_fn;
	struct object *exit_ob;
{
	extern void push_apply_value(), pop_apply_value();
	char *new_path;
	struct svalue *setup, *prompt;
	struct ed_buffer *old_ed_buffer;

	if (!command_giver || !(command_giver->flags & O_SHADOW))
	    error("Tried to start an ed session on a non-interative player.\n");
	if (EXTERN_ED_BUFFER)
		error("Tried to start an ed session, when already active.\n");
	/*
	 * Check for read on startup, since the buffer is read in. But don't
	 * check for write, since we may want to change the file name.
	 */
	new_path = check_valid_path(file_arg, command_giver, "ed_start", 0);
	if (!file_arg && !new_path)
	    return;
	/* Never trust the master... in might be as paranoid as ourselves... */
	/* Starting another ed session in valid_read() looks stupid, but
	 * possible.
	 */
	if (!command_giver ||
	    !(command_giver->flags & O_SHADOW) ||
	    command_giver->flags & O_DESTRUCTED ||
	    EXTERN_ED_BUFFER)
	{
	    return;
	}
	old_ed_buffer = ED_BUFFER;
	EXTERN_ED_BUFFER =
	  ED_BUFFER = (struct ed_buffer *)xalloc(sizeof (struct ed_buffer));
	memset((char *)ED_BUFFER, '\0',
	       sizeof (struct ed_buffer));
	ED_BUFFER->truncflg = 1;
	ED_BUFFER->flags |= EIGHTBIT_MASK | TABINDENT_MASK;
	ED_BUFFER->shiftwidth= 4;
	prompt = query_prompt(command_giver);
	ED_BUFFER->old_prompt = *prompt;
	prompt->type = T_STRING;
	prompt->x.string_type = STRING_CONSTANT;
	prompt->u.string = ":";
	ED_BUFFER->CurPtr =
	    &ED_BUFFER->Line0;
	if (exit_fn) {
	    ED_BUFFER->exit_fn = string_copy(exit_fn);
	    exit_ob->ref ++ ;
	} else {
	    ED_BUFFER->exit_fn = 0;
	}
	ED_BUFFER->exit_ob = exit_ob;
	set_ed_buf();
	push_apply_value();
	push_object(command_giver);
	setup = apply_master_ob("retrieve_ed_setup",1);
	if ( setup && setup->type==T_NUMBER && setup->u.number ) {
		ED_BUFFER->flags      = setup->u.number & ALL_FLAGS_MASK;
		ED_BUFFER->shiftwidth = setup->u.number & SHIFTWIDTH_MASK;
	}
	/* It is possible to toggle P_DIAG in retrieve_ed_setup() , by issueing
	 * an 'n' command(), which will cause add_message() to be called in
	 * do_read(); add_message might in turn call apply() via
	 * shadow_catch_message(), thus new_path needs to stay pushed.
	 */

	if( new_path && !doread(0, new_path)) {
		_setCurLn( 1 );
	}
	if (new_path) {
	    strncpy(P_FNAME, new_path, MAXFNAME-1);
	    P_FNAME[MAXFNAME-1] = 0;
	    add_message("/%s, %d lines\n", new_path, P_LASTLN);
	} else {
	    add_message("No file.\n");
	}
	pop_apply_value();
	ED_BUFFER = old_ed_buffer;
	return;
}

#ifdef MALLOC_smalloc
void clear_ed_buffer_refs(b)
    struct ed_buffer *b;
{
    struct object *ob;

    if (b->exit_fn) {
	if (ob = b->exit_ob) {
	    if (ob->flags & O_DESTRUCTED) {
		reference_destructed_object(ob);
		b->exit_ob = 0;
	    } else {
		ob->ref++;
	    }
	}
    }
    clear_ref_in_vector(&b->old_prompt, 1);
}

void count_ed_buffer_refs(b)
    struct ed_buffer *b;
{
    struct object *ob;
    LINE *line;

    if (b->LastLn) {
	line = b->Line0.l_next;
	while(line != &b->Line0) {
	    note_malloced_block_ref((char *)line);
	    line = line->l_next;
	}
    }
    if (b->exit_fn) {
	note_malloced_block_ref(b->exit_fn);
	if (ob = b->exit_ob) {
	    if (ob->flags & O_DESTRUCTED) {
		reference_destructed_object(ob);
		b->exit_ob = 0;
	    } else {
		ob->ref++;
	    }
	}
    }
    if (b->oldpat)
	note_malloced_block_ref((char *)b->oldpat);
    count_ref_in_vector(&b->old_prompt, 1);
}
#endif /* MALLOC_smalloc */

#ifdef DEBUG
void count_ed_buffer_extra_refs(b)
    struct ed_buffer *b;
{
    struct object *ob;

    if (ob = b->exit_ob)
	ob->extra_ref++;
}
#endif

/* After calling this function, ED_BUFFER won't be referenced unless set anew
 * There must be no errors here, because there might be a call from
 * remove_interactive() .
 */
void free_ed_buffer() {
    char *name;
    struct object *ob;

    ED_BUFFER = EXTERN_ED_BUFFER;
    clrbuf();
    ob   = ED_BUFFER->exit_ob;
    name = ED_BUFFER->exit_fn;
    if (O_GET_INTERACTIVE(command_giver) &&
	O_GET_INTERACTIVE(command_giver)->sent.type == SENT_INTERACTIVE)
    {
	transfer_svalue( query_prompt(command_giver), &ED_BUFFER->old_prompt );
    } else {
	free_svalue(&ED_BUFFER->old_prompt);
    }
    if(P_OLDPAT)
	xfree((char *)P_OLDPAT);
    xfree((char *)ED_BUFFER);
    EXTERN_ED_BUFFER = 0;
    if (name) {
	if (!ob || ob->flags & O_DESTRUCTED) {
	    debug_message("ed: exit_ob destructed at eof.\n");
	} else {
	    struct object *save = current_object;

	    current_object = ob;
	    secure_apply(name, ob, 0); /* might call efun ed,
			         * thus setting (EXTERN_)ED_BUFFER again */
	    current_object = save;
	}
	if (ob)
	    free_object(ob, "ed EOF");
	xfree(name);
    } else {
	add_message("Exit from ed.\n");
    }
    return;
}

void ed_cmd(str)
	char *str;
{
	int status;
	struct ed_buffer *old_ed_buffer;

	old_ed_buffer = ED_BUFFER;
	ED_BUFFER = EXTERN_ED_BUFFER;
	if (P_MORE) {
	    print_help2();
	    ED_BUFFER = old_ed_buffer;
	    return;
	}
	if (P_APPENDING) {
		more_append(str);
		ED_BUFFER = old_ed_buffer;
		return;
	}
	if (strlen(str) < MAXLINE)
	    strcat(str, "\n");
	
	strncpy(inlin, str, MAXLINE-1);
	inlin[MAXLINE-1] = 0;
	inptr = inlin;
	if( (status = getlst()) >= 0)
		if((status = ckglob()) != 0) {
			if(status >= 0 && (status = doglob()) >= 0) {
				_setCurLn( status );
				ED_BUFFER = old_ed_buffer;
				return;
			}
		} else {
			if((status = docmd(0)) >= 0) {
				if(status == 1)
					doprnt(P_CURLN, P_CURLN);
				ED_BUFFER = old_ed_buffer;
				return;
			}
		}
	switch (status) {
	case EOF:
	        free_ed_buffer();
		ED_BUFFER = old_ed_buffer;
		return;
#ifdef FATAL
	case FATAL:
		if (ED_BUFFER->exit_fn) {
		    xfree(ED_BUFFER->exit_fn);
		    free_object(ED_BUFFER->exit_ob, "ed FATAL");
		}
		xfree((char *)ED_BUFFER);
		EXTERN_ED_BUFFER = 0;
		add_message("FATAL ERROR\n");
		set_prompt("> ");
		ED_BUFFER = old_ed_buffer;
		return;
#endif
	case CHANGED:
		add_message("File has been changed.\n");
		break;
	case SET_FAIL:
		add_message("`set' command failed.\n");
		break;
	case SUB_FAIL:
		add_message("string substitution failed.\n");
		break;
	case MEM_FAIL:
		add_message("Out of memory: text may have been lost.\n" );
		break;
	default:
		add_message("Unrecognized or failed command.\n");
		/*  Unrecognized or failed command (this  */
		/*  is SOOOO much better than "?" :-)	  */
	}
	ED_BUFFER = old_ed_buffer;
}

void save_ed_buffer()
{
    struct svalue *stmp;
    char *fname;
    struct interactive *save = O_GET_INTERACTIVE(command_giver);

    ED_BUFFER = EXTERN_ED_BUFFER;
    push_string_shared(P_FNAME);
    stmp = apply_master_ob("get_ed_buffer_save_file_name",1);
    if (save->sent.type == SENT_INTERACTIVE) {
	save->catch_tell_activ = 0;
	command_giver = save->ob;
    }
    if (stmp) {
	if (stmp->type == T_STRING) {
	    fname = stmp->u.string;
	    if (*fname == '/') fname++;
            dowrite(1, P_LASTLN, fname , 0);
	}
    }
    free_ed_buffer();
}

struct svalue *f_query_editing(sp)
    struct svalue *sp;
{
    struct object *ob;
    struct shadow_sentence *sent;

    if (sp->type != T_OBJECT)
	bad_xefun_arg(1, sp);
    ob = sp->u.ob;
    decr_object_ref(ob, "query_editing");
    if (ob->flags & O_SHADOW && (sent = O_GET_SHADOW(ob)) && sent->ed_buffer) {
	if (ob = sent->ed_buffer->exit_ob) {
	    sp->u.ob = ob;
	    add_ref(ob, "query_editing");
	    return sp;
	}
	sp->u.number = 1;
    } else {
	sp->u.number = 0;
    }
    sp->type = T_NUMBER;
    return sp;
}

static void print_help(arg)
    int arg;
{
    switch (arg) {
    case 'I':
	add_message("This command indents your entire file under the\n");
	add_message("assumption that it is LPC code.  It is only useful\n");
	add_message("for files that are not yet indented, since the\n");
	add_message("indentation style is unlikely to satisfy anyone.\n");
	add_message("Originally from DGD ed.\n");
	break;
#if 0
    case '^':
	add_message("Command: ^   Usage: ^pattern\n");
	add_message("This command is similiar to grep, in that it searches the\n");
	add_message("entire file, printing every line that contains the specified\n");
	add_message("pattern.  To get the line numbers of found lines, turn on line\n");
	add_message("number printing with the 'n' command.\n");
	break;
#endif
    case 'n':
	add_message("Command: n   Usage: n\n");
	add_message("This command toggles the internal flag which will cause line\n");
	add_message("numbers to be printed whenever a line is listed.\n");
	break;
    case 'a':
	add_message("Command: a   Usage: a\n");
	add_message("Append causes the editor to enter input mode, inserting all text\n");
	add_message("starting AFTER the current line. Use a '.' on a blank line to exit\n");
	add_message("this mode.\n");
	break;
    case 'A':
        add_message("Command: A   Usage: A\n\
Like the 'a' command, but uses inverse autoindent mode.\n");
	break;
    case 'i':
	add_message("Command: i   Usage: i\n");
	add_message("Insert causes the editor to enter input mode, inserting all text\n");
	add_message("starting BEFORE the current line. Use a '.' on a blank line to exit\n");
	add_message("this mode.\n");
	break;
    case 'c':
	add_message("Command: c   Usage: c\n");
	add_message("Change command causes the current line to be wiped from memory.\n");
	add_message("The editor enters input mode and all text is inserted where the previous\n");
	add_message("line existed.\n");
	break;
    case 'd':
	add_message("Command: d   Usage: d  or [range]d\n");
	add_message("Deletes the current line unless preceeded with a range of lines,\n");
	add_message("then the entire range will be deleted.\n");
	break;
    case 'e':
	add_message("Commmand: e  Usage: e filename\n");
	add_message("Causes the current file to be wiped from memory, and the new file\n");
	add_message("to be loaded in.\n");
	break;      
    case 'E':
	add_message("Commmand: E  Usage: E filename\n");
	add_message("Causes the current file to be wiped from memory, and the new file\n");
	add_message("to be loaded in.  Different from 'e' in the fact that it will wipe\n");
	add_message("the current file even if there are unsaved modifications.\n");
	break;
    case 'f':
	add_message("Command: f  Usage: f  or f filename\n");
	add_message("Display or set the current filename.   If  filename is given as \nan argument, the file (f) command changes the current filename to\nfilename; otherwise, it prints  the current filename.\n");
	break;
    case 'g':
	add_message("Command: g  Usage: g/re/p\n");
	add_message("Search in all lines for expression 're', and print\n");
	add_message("every match. Command 'l' can also be given\n");
	add_message("Unlike in unix ed, you can also supply a range of lines\n");
	add_message("to search in\n");
	add_message("Compare with command 'v'.\n");
	break;
    case 'h':
	add_message("Command: h    Usage:  h  or hc (where c is a command)\n");
	add_message("Help files added by Qixx.\n");
	break;
    case 'j':
	add_message("\
Command: j    Usage: j or [range]j\n\
Join Lines. Remove the NEWLINE character  from  between the  two\n\
addressed lines.  The defaults are the current line and the line\n\
following.  If exactly one address is given,  this  command does\n\
nothing.  The joined line is the resulting current line.\n");
	break;
    case 'k':
	add_message("\
Command: k   Usage: kc  (where c is a character)\n\
Mark the addressed line with the name c,  a  lower-case\n\
letter.   The  address-form,  'c,  addresses  the  line\n\
marked by c.  k accepts one address; the default is the\n\
current line.  The current line is left unchanged.\n");
	break;
    case 'l':
	add_message("\
Command: l   Usage: l  or  [range]l\n\
List the current line or a range of lines in an unambiguous\n\
way such that non-printing characters are represented as\n\
symbols (specifically New-Lines).\n");
	break;
    case 'm':
	add_message("\
Command: m   Usage: mADDRESS or [range]mADDRESS\n\
Move the current line (or range of lines if specified) to a\n\
location just after the specified ADDRESS.  Address 0 is the\n\
beginning of the file and the default destination is the\n\
current line.\n\
	");
	break;
    case 'p':
	add_message("\
Command: p    Usage: p  or  [range]p\n\
Print the current line (or range of lines if specified) to the\n\
screen. See the commands 'n' and 'set' if line numbering is desired.\n");
	break;
    case 'q':
	add_message("\
Command: q    Usage: q\n\
Quit the editor. Note that you can't quit this way if there\n\
are any unsaved changes.  See 'w' for writing changes to file.\n");
	break;
    case 'Q':
	add_message("\
Command: Q    Usage: Q\n\
Force Quit.  Quit the editor even if the buffer contains unsaved\n\
modifications.\n");
	break;
    case 'r':
	add_message("\
Command: r    Usage: r filename\n\
Reads the given filename into the current buffer starting\n\
at the current line.\n");
	break;
    case 't':
	add_message("\
Command: t   Usage: tADDRESS or [range]tADDRESS\n\
Transpose a copy of the current line (or range of lines if specified)\n\
to a location just after the specified ADDRESS.  Address 0 is the\n\
beginning of the file and the default destination\nis the current line.\n");
	break;
    case 'v':
	add_message("\
Command: v   Usage: v/re/p\n\
Search in all lines without expression 're', and print\n\
every match. Other commands than 'p' can also be given\n\
Compare with command 'g'.\n");
	break;
    case 'z':
	add_message("\
Command: z   Usage: z  or  z-  or z.\n\
Displays 20 lines starting at the current line.\n\
If the command is 'z.' then 20 lines are displayed being\n\
centered on the current line. The command 'z-' displays\n\
the 20 lines before the current line.\n");
	break;
    case 'Z':
	add_message("\
Command: Z   Usage: Z  or  Z-  or Z.\n\
Displays 40 lines starting at the current line.\n\
If the command is 'Z.' then 40 lines are displayed being\n\
centered on the current line. The command 'Z-' displays\n\
the 40 lines before the current line.\n");
	break;
    case 'x':
	add_message("\
Command: x   Usage: x\n\
Save file under the current name, and then exit from ed.\n");
	break;
    case 's':
	if ( *inptr=='e' && *(inptr+1)=='t' ) {
	    add_message("\
Without arguments: show current settings.\n\
'set save' will preserve the current settings for subsequent invocations of ed.\n\
Options:\n\
\n\
number	   will print line numbers before printing or inserting a lines\n\
list	   will print control characters in p(rint) and z command like in l(ist)\n\
print	   will show current line after a single substitution or deletion\n\
eightbit\n\
autoindent will preserve current indentation while entering text.\n\
	   use ^D or ^K to get back one step back to the right.\n\
excompatible will exchange the meaning of \\( and ( as well as \\) and )\n\
\n\
An option can be cleared by prepending it with 'no' in the set command, e.g.\n\
'set nolist' to turn off the list option.\n\
\n\
set shiftwidth <digit> will store <digit> in the shiftwidth variable, which\n\
determines how much blanks are removed from the current indentation when\n\
typing ^D or ^K in the autoindent mode.\n");
		break;
	} else {
/* is there anyone who wants to add an exact description for the 's' command? */
	}
    case 'w':
    case 'W':
    case '/':
    case '?':
	add_message("Sorry no help yet for this command. Try again later.\n");
	break;
    default:
	add_message("       Help for Ed  (V 2.0)\n");
	add_message("---------------------------------\n");
	add_message("     by Qixx [Update: 7/10/91]\n");
	add_message("\n\nCommands\n--------\n");
	add_message("/\tsearch forward for pattern\n");
	add_message("?\tsearch backward for a pattern\n");
	/* add_message("^\tglobal search and print for pattern\n"); */
	add_message("=\tshow current line number\n");
	add_message("a\tappend text starting after this line\n");
	add_message("A\tlike 'a' but with inverse autoindent mode\n"),
	add_message("c\tchange current line, query for replacement text\n");
	add_message("d\tdelete line(s)\n");
	add_message("e\treplace this file with another file\n");
	add_message("E\tsame as 'e' but works if file has been modified\n");
	add_message("f\tshow/change current file name\n");
	add_message("g\tSearch and execute command on any matching line.\n");
	add_message("h\thelp file (display this message)\n");
	add_message("i\tinsert text starting before this line\n");
	add_message("I\tindent the entire file (from DGD ed v0.1)\n");
	add_message("\n--Return to continue--");
	P_MORE=1;
	break;
    }
}

static void print_help2() {
    P_MORE=0;
    add_message("\
j\tjoin lines together\n\
k\tmark this line with a character - later referenced as 'a\n\
l\tline line(s) with control characters displayed\n\
m\tmove line(s) to specified line\n\
n\ttoggle line numbering\n\
p\tprint line(s) in range\n\
q\tquit editor\n\
Q\tquit editor even if file modified and not saved\n\
r\tread file into editor at end of file or behind the given line\n\
s\tsearch and replace\n\
set\tquery, change or save option settings\n\
t\tmove copy of line(s) to specified line\n\
v\tSearch and execute command on any non-matching line.\n\
x\tsave file and quit\n\
w\twrite to current file (or specified file)\n\
W\tlike the 'w' command but appends instead\n\
z\tdisplay 20 lines, possible args are . + -\n\
Z\tdisplay 40 lines, possible args are . + -\n\
\n\
For further information type 'hc' where c is the command\n\
that help is desired for.\n");
}
