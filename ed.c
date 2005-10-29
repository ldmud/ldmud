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
 */

int	version = 5;	/* used only in the "set" function, for i.d. */

#include <stdio.h>
#include <string.h>
/* Regexp is Henry Spencer's package. WARNING: regsub is modified to return
 * a pointer to the \0 after the destination string, and this program refers
 * to the "private" reganch field in the struct regexp.
 */
#include "regexp.h"
#include "object.h"
#include "config.h"

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
#define RS	0x1e	/* ^^ */
#define US	0x1f	/* ^_ */
#define SP	0x20	/* space */
#define DEL	0x7f	/* DEL*/
#define ESCAPE  '\\'


#define TRUE	1
#define FALSE	0
#define ERR		-2
#define FATAL		(ERR-1)
#define CHANGED		(ERR-2)
#define SET_FAIL	(ERR-3)
#define SUB_FAIL	(ERR-4)
#define MEM_FAIL	(ERR-5)


#define	BUFFER_SIZE	2048	/* stream-buffer size:  == 1 hd cluster */

#define LINFREE	1	/* entry not in use */
#define LGLOB	2       /* line marked global */

#define MAXLINE	512	/* max number of chars per line */
#define MAXPAT	256	/* max number of chars per replacement pattern */
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
extern void add_message();

#define P_DIAG		(command_giver->ed_buffer->diag)
#define P_TRUNCFLG	(command_giver->ed_buffer->truncflg)
#define P_EIGHTBIT	(command_giver->ed_buffer->eightbit)
#define P_NONASCII	(command_giver->ed_buffer->nonascii)
#define P_NULLCHAR	(command_giver->ed_buffer->nullchar)
#define P_TRUNCATED	(command_giver->ed_buffer->truncated)
#define P_FNAME		(command_giver->ed_buffer->fname)
#define P_FCHANGED	(command_giver->ed_buffer->fchanged)
#define P_NOFNAME	(command_giver->ed_buffer->nofname)
#define P_MARK		(command_giver->ed_buffer->mark)
#define P_OLDPAT	(command_giver->ed_buffer->oldpat)
#define P_LINE0		(command_giver->ed_buffer->Line0)
#define P_LINE0		(command_giver->ed_buffer->Line0)
#define P_CURLN		(command_giver->ed_buffer->CurLn)
#define P_CURPTR	(command_giver->ed_buffer->CurPtr)
#define P_LASTLN	(command_giver->ed_buffer->LastLn)
#define P_LINE1		(command_giver->ed_buffer->Line1)
#define P_LINE2		(command_giver->ed_buffer->Line2)
#define P_NLINES	(command_giver->ed_buffer->nlines)
#define P_PFLAG		(command_giver->ed_buffer->pflag)
#define P_NFLG		(command_giver->ed_buffer->nflg)
#define P_LFLG		(command_giver->ed_buffer->lflg)
#define P_PFLG		(command_giver->ed_buffer->pflg)
#define P_APPENDING	(command_giver->ed_buffer->appending)

char	inlin[MAXLINE];
char	*inptr;		/* tty input buffer */
struct ed_buffer {
	int	diag;		/* diagnostic-output? flag */
	int	truncflg;	/* truncate long line flag */
	int	eightbit;	/* save eighth bit */
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
	int	pflag;
	int	Line1, Line2, nlines;
	int	nflg;		/* print line number flag */
	int	lflg;		/* print line in verbose mode */
	int	pflg;		/* print current line after each command */
	int	appending;
};

#if 0

struct tbl {
	char	*t_str;
	int	*t_ptr;
	int	t_val;
} *t, tbl[] = {
	"number",	&nflg,		TRUE,
	"nonumber",	&nflg,		FALSE,
	"list",		&lflg,		TRUE,
	"nolist",	&lflg,		FALSE,
	"eightbit",	&eightbit,	TRUE,
	"noeightbit",	&eightbit,	FALSE,
	0
};
#endif


/*-------------------------------------------------------------------------*/

extern	char	*strcpy();
extern	char	*xalloc();
extern	LINE	*getptr();
extern	char	*gettxt();
extern	char	*gettxtl();
extern	char	*catsub();
extern	void	prntln(), putcntl(), error();
regexp	*optpat();


/*________  Macros  ________________________________________________________*/

#ifndef max
#  define max(a,b)	((a) > (b) ? (a) : (b))
#endif

#ifndef min
#  define min(a,b)	((a) < (b) ? (a) : (b))
#endif

#ifndef toupper
#  define toupper(c)	((c >= 'a' && c <= 'z') ? c-32 : c )
#endif

#define nextln(l)	((l)+1 > P_LASTLN ? 0 : (l)+1)
#define prevln(l)	((l)-1 < 0 ? P_LASTLN : (l)-1)

#define gettxtl(lin)	((lin)->l_buff)
#define gettxt(num)	(gettxtl( getptr(num) ))

#define getnextptr(p)	((p)->l_next)
#define getprevptr(p)	((p)->l_prev)

#define setCurLn( lin )	( P_CURPTR = getptr( P_CURLN = (lin) ) )
#define nextCurLn()	( P_CURLN = nextln(P_CURLN), P_CURPTR = getnextptr( P_CURPTR ) )
#define prevCurLn()	( P_CURLN = prevln(P_CURLN), P_CURPTR = getprevptr( P_CURPTR ) )

#define clrbuf()	del(1, P_LASTLN)

#define	Skip_White_Space	{while (*inptr==SP || *inptr==HT) inptr++;}

#define relink(a, x, y, b) { (x)->l_prev = (a); (y)->l_next = (b); }


/*________  functions  ______________________________________________________*/


/*	append.c	*/


append(line, glob)
int	line, glob;
{
	if(glob)
		return(ERR);
	setCurLn( line );
	P_APPENDING = 1;
	set_prompt("*\b");
	return 0;
}

more_append(str)
	char *str;
{
	if(P_NFLG)
		add_message("%6d. ",P_CURLN+1);
	if(str[0] == '.' && str[1] == '\0') {
		P_APPENDING = 0;
		set_prompt(":");
		return(0);
	}
	if( ins(str) < 0)
		return( MEM_FAIL );
	return 0;
}

/*	ckglob.c	*/

ckglob()
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
			lin = gettxtl(ptr);
			if(regexec(glbpat, lin )) {
				if (c=='g') ptr->l_stat |= LGLOB;
			} else {
				if (c=='v') ptr->l_stat |= LGLOB;
			}
		ptr = getnextptr(ptr);
		}
	}
	return(1);
}


/*  deflt.c
 *	Set P_LINE1 & P_LINE2 (the command-range delimiters) if the file is
 *	empty; Test whether they have valid values.
 */

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

del(from, to)
int	from, to;
{
	LINE	*first, *last, *next, *tmp;

	if(from < 1)
		from = 1;
	first = getprevptr( getptr( from ) );
	last = getnextptr( getptr( to ) );
	next = first->l_next;
	while(next != last && next != &P_LINE0) {
		tmp = next->l_next;
		free(next);
		next = tmp;
	}
	relink(first, last, first, last);
	P_LASTLN -= (to - from)+1;
	setCurLn( prevln(from) );
	return(0);
}


int dolst(line1, line2)
int line1, line2;
{
	int oldlflg=P_LFLG, p;

	P_LFLG = 1;
	p = doprnt(line1, line2);
	P_LFLG = oldlflg;
	return p;
}


/*	esc.c
 * Map escape sequences into their equivalent symbols.  Returns the
 * correct ASCII character.  If no escape prefix is present then s
 * is untouched and *s is returned, otherwise **s is advanced to point
 * at the escaped character and the translated character is returned.
 */
esc(s)
char	**s;
{
	register int	rval;

	if (**s != ESCAPE) {
		rval = **s;
	} else {
		(*s)++;
		switch(toupper(**s)) {
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


/*	doprnt.c	*/

int doprnt(from, to)
int	from, to;
{
	from = (from < 1) ? 1 : from;
	to = (to > P_LASTLN) ? P_LASTLN : to;

	if(to != 0) {
		setCurLn( from );
		while( P_CURLN <= to ) {
			prntln( gettxtl( P_CURPTR ), P_LFLG, (P_NFLG ? P_CURLN : 0));
			if( P_CURLN == to )
				break;
			nextCurLn();
		}
	}
	return(0);
}


void prntln(str, vflg, lin)
char	*str;
int	vflg, lin;
{
	if(lin)
		add_message("%7d ",lin);
	while(*str && *str != NL) {
		if(*str < ' ' || *str >= 0x7f) {
			switch(*str) {
			case '\t':
				if(vflg)
					putcntl(*str, stdout);
				else
					add_message("%c", *str);
				break;

			case DEL:
				putc('^', stdout);
				putc('?', stdout);
				break;

			default:
				putcntl(*str, stdout);
				break;
			}
		} else
			add_message("%c", *str);
		str++;
	}
	if(vflg)
		add_message("$");
	add_message("\n");
}


void putcntl(c, stream)
char	c;
FILE	*stream;
{
	putc('^', stream);
	putc((c&31)|'@', stream);
}


/*	egets.c	*/

egets(str,size,stream)
char	*str;
int	size;
FILE	*stream;
{
	int	c, count;
	char	*cp;

	for(count = 0, cp = str; size > count;) {
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
	}
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


doread(lin, fname)
int	lin;
char	*fname;
{
	FILE	*fp;
	int	err;
	unsigned long	bytes;
	unsigned int	lines;
	static char	str[MAXLINE];

	err = 0;
	P_NONASCII = P_NULLCHAR = P_TRUNCATED = 0;

	if (P_DIAG) add_message("\"%s\" ",fname);
	if( (fp = fopen(fname, "r")) == NULL ) {
		add_message(" isn't readable.\n");
		return( ERR );
	}
	setCurLn( lin );
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
		add_message("%u lines %u bytes",lines,bytes);
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


int dowrite(from, to, fname, apflg)
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

find(pat, dir)
regexp	*pat;
int	dir;
{
	int	i, num;
	LINE	*lin;

	num = P_CURLN;
	lin = P_CURPTR;
	for(i=0; i<P_LASTLN; i++ ) {
		if(regexec( pat, gettxtl( lin ) ))
			return(num);
		if( dir )
			num = nextln(num), lin = getnextptr(lin);
		else
			num = prevln(num), lin = getprevptr(lin);
	}
	return ( ERR );
}


/*	getfn.c	*/

char *getfn(writeflg)
{
	static char	file[256];
	char	*cp;
	extern char *check_file_name();

	if(*inptr == NL) {
		P_NOFNAME=TRUE;
		strcpy(file, P_FNAME);
	} else {
		char *file2;
		P_NOFNAME=FALSE;
		Skip_White_Space;

		cp = file;
		while(*inptr && *inptr != NL && *inptr != SP && *inptr != HT)
			*cp++ = *inptr++;
		*cp = '\0';

		if(strlen(file) == 0) {
			add_message("bad file name\n");
			return( NULL );
		}
		file2 = check_file_name(file, writeflg);
		if (!file2)
		    return( NULL );
		strcpy(file, file2);
	}

	if(strlen(file) == 0) {
		add_message("no file name\n");
		return(NULL);
	}
	return( file );
}  /* getfn */


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

int getone()
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


getlst()
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
			setCurLn( num );
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

LINE *getptr(num)
int	num;
{
	LINE	*ptr;
	int	j;

	if (2*num>P_LASTLN && num<=P_LASTLN) {	/* high line numbers */
		ptr = P_LINE0.l_prev;
		for (j = P_LASTLN; j>num; j--)
			ptr = ptr->l_prev;
	} else {				/* low line numbers */
		ptr = &P_LINE0;
		for(j = 0; j < num; j++)
			ptr = ptr->l_next;
	}
	return(ptr);
}


/*	getrhs.c	*/

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
			case ESCAPE:
				*sub++ = ESCAPE;
				*sub++ = ESCAPE;
				inptr++;
			case 'n':
				*sub++ = '\n';
				inptr++;
				break;
			case 'b':
				*sub++ = '\b';
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
			default:
				if ( *inptr != delim )
					*sub++ = ESCAPE;
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

ins(str)
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
		setCurLn( first );
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

int move(num)
int	num;
{
	int	range;
	LINE	*before, *first, *last, *after;

	if( P_LINE1 <= num && num <= P_LINE2 )
		return( ERR );
	range = P_LINE2 - P_LINE1 + 1;
	before = getptr(prevln(P_LINE1));
	first = getptr(P_LINE1);
	last = getptr(P_LINE2);
	after = getptr(nextln(P_LINE2));

	relink(before, after, before, after);
	P_LASTLN -= range;	/* per AST's posted patch 2/2/88 */
	if (num > P_LINE1)
		num -= range;

	before = getptr(num);
	after = getptr(nextln(num));
	relink(before, first, last, after);
	relink(last, after, before, first);
	P_LASTLN += range;	/* per AST's posted patch 2/2/88 */
	setCurLn( num + range );
	return( 1 );
}


int transfer(num)
int num;
{
	int mid, lin, ntrans;

	if (P_LINE1<=0 || P_LINE1>P_LINE2)
		return(ERR);

	mid= num<P_LINE2 ? num : P_LINE2;

	setCurLn( num );
	ntrans=0;

	for (lin=P_LINE1; lin<=mid; lin++) {
		if( ins(gettxt(lin)) < 0 )
			return MEM_FAIL;
		ntrans++;
	}
	lin+=ntrans;
	P_LINE2+=ntrans;

	for ( ; lin <= P_LINE2; lin += 2 ) {
		if( ins(gettxt(lin)) < 0 )
			return MEM_FAIL;
		P_LINE2++;
	}
	return(1);
}


/*	optpat.c	*/

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
		free(P_OLDPAT);
	return P_OLDPAT = regcomp(str);
}

/* regerror.c */
void regerror( s )
    char *s;
{
	add_message("ed: %s\n", s );
}


set()
{
	char	word[16];
	int	i;

	if(*(++inptr) != 't') {
		if(*inptr != SP && *inptr != HT && *inptr != NL)
			return(ERR);
	} else
		inptr++;

	if ( (*inptr == NL))
	{
		add_message("ed version %d.%d\n", version/100, version%100);
		add_message(	"number %s, list %s\n",
			P_NFLG?"ON":"OFF",
			P_LFLG?"ON":"OFF");
		return(0);
	}

	Skip_White_Space;
	for(i = 0; *inptr != SP && *inptr != HT && *inptr != NL;)
		word[i++] = *inptr++;
	word[i] = EOS;
#if 0
	for(t = tbl; t->t_str; t++) {
		if(strcmp(word,t->t_str) == 0) {
			*t->t_ptr = t->t_val;
			return(0);
		}
	}
#endif
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


void set_ed_buf()
{
	relink(&P_LINE0, &P_LINE0, &P_LINE0, &P_LINE0);
	P_CURLN = P_LASTLN = 0;
	P_CURPTR = &P_LINE0;
}


/*	subst.c	*/

int subst(pat, sub, gflg, pflag)
regexp	*pat;
char	*sub;
int	gflg, pflag;
{
	int	nchngd = 0;
	char	*txtptr;
	char	*new, buf[MAXLINE];
	int	still_running = 1;
	LINE	*lastline = getptr( P_LINE2 );

	if(P_LINE1 <= 0)
		return( SUB_FAIL );
	nchngd = 0;		/* reset count of lines changed */

	for( setCurLn( prevln( P_LINE1 ) ); still_running; ) {
		nextCurLn();
		new = buf;
		if ( P_CURPTR == lastline )
			still_running = 0;
		if ( regexec( pat, txtptr = gettxtl( P_CURPTR ) ) ) {
			do
				{
				/* Copy leading text */
				int diff = pat->startp[0] - txtptr;
				strncpy( new, txtptr, diff );
				new += diff;
				/* Do substitution */
				new = regsub( pat, sub, new );
				txtptr = pat->endp[0];
				}
			while( gflg && !pat->reganch && regexec( pat, txtptr ));

			/* Copy trailing chars */
			while( *txtptr ) *new++ = *txtptr++;

			if(new >= buf+MAXLINE)
				return( SUB_FAIL );
			*new++ = EOS;
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

/*  docmd.c
 *	Perform the command specified in the input buffer, as pointed to
 *	by inptr.  Actually, this finds the command letter first.
 */

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
		if(*inptr != NL || P_NLINES > 1)
			return(ERR);

		if(append(P_LINE1, glob) < 0)
			return(ERR);
		P_FCHANGED = TRUE;
		break;

	case 'c':
		if(*inptr != NL)
			return(ERR);

		if(deflt(P_CURLN, P_CURLN) < 0)
			return(ERR);

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

		if((fptr = getfn(1)) == NULL)
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

		if((fptr = getfn(1)) == NULL)
			return(ERR);

		if (P_NOFNAME)
			add_message("%s\n", P_FNAME);
		else
			strcpy(P_FNAME, fptr);
		break;

	case 'i':
		if(*inptr != NL || P_NLINES > 1)
			return(ERR);

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

		if(P_NLINES == 0)			/* The original code tested */
			P_LINE2 = P_LASTLN;		/*	if(P_NLINES = 0)	    */
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
			if(dowrite(1, P_LASTLN, fptr, 0) >= 0)
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

	default:
		return(ERR);
	}
	return (0);
}  /* docmd */


/*	doglob.c	*/
doglob()
{
	int	lin, stat;
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
		if((stat = getlst()) < 0)
			return(stat);
		if((stat = docmd(1)) < 0)
			return(stat);
	}
	return(P_CURLN);
}  /* doglob */


void ed_start(file_arg)
	char *file_arg;
{
	if (command_giver->ed_buffer)
		error("Tried to start an ed session, when already active.\n");
	if (command_giver != current_object)
	    	error("Illegal start of ed.\n");
	command_giver->ed_buffer =
		(struct ed_buffer *)xalloc(sizeof (struct ed_buffer));
	memset((char *)command_giver->ed_buffer, '\0',
	       sizeof (struct ed_buffer));
	command_giver->ed_buffer->truncflg = 1;
	command_giver->ed_buffer->eightbit = 1;
	command_giver->ed_buffer->CurPtr = &command_giver->ed_buffer->Line0;
	set_ed_buf();

	if(file_arg && doread(0,file_arg)==0) {
		setCurLn( 1 );
		strcpy(P_FNAME, file_arg);
		set_prompt(":");
		return;
	}
	set_prompt(":");
}

ed_cmd(str)
	char *str;
{
	int stat;

	if (P_APPENDING) {
		more_append(str);
		return;
	}
	strcat(str, "\n");
	
	strcpy(inlin, str);
	inptr = inlin;
	if(getlst() >= 0)
		if((stat = ckglob()) != 0) {
			if(stat >= 0 && (stat = doglob()) >= 0) {
				setCurLn( stat );
				return;
			}
		} else {
			if((stat = docmd(0)) >= 0) {
				if(stat == 1)
					doprnt(P_CURLN, P_CURLN);
				return;
			}
		}
	switch (stat) {
	case EOF:
		free(command_giver->ed_buffer);
		command_giver->ed_buffer = 0;
		add_message("Exit from ed.\n");
		set_prompt("> ");
		return;
	case FATAL:
		free(command_giver->ed_buffer);
		command_giver->ed_buffer = 0;
		add_message("FATAL ERROR\n");
		set_prompt("> ");
		return;
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
}
