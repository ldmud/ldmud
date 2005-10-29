/*
 * webster - look words up in the dictionary
 *
 * This program connects to the Webster server to get definitions of words.
 * Words may be given on the command line, or, if no arguments are given,
 * the program runs interactively.  If -s or -d is given, it puts the program
 * in "spell" or "define" mode, respectively.  "define" is the default.
 *
 * In either mode, a word may include the wildcard characters '%' and '*'.
 * The '%' character matches exactly one character, while the '*' matches
 * zero or more characters.  If wildcards are used, the program will
 * return either "No match" or a list of matching words.  "!word" looks the
 * word up in the alternate mode.
 *
 * In interactive mode only, Tenex-style command completion may also be
 * used.  Typing a '?' following part of a word will cause the program
 * to print all words which begin with the partial word, or the program
 * will beep if nothing matches.  Typing an ESCape character causes the
 * program to attempt to complete the word.  If the word can be completed,
 * the new word is printed; otherwise, the program beeps.  Wildcards
 * may be used to specify the partial words.
 *
 * David A. Curry
 * Purdue University
 * davy@ee.purdue.edu
 * April, 1986
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sgtty.h>
#include <netdb.h>
#include <ctype.h>
#include <stdio.h>

#include <sys/un.h>
#include <arpa/inet.h>

#include "lint.h"
#include "interpret.h"
#include "config.h"
#include "comm.h"
#include "object.h"

#include <errno.h>

#include "webster.h"

#define BACKSPACE	010		/* backspace character       ^H */
#define WORDERASE	027		/* word erase character      ^W */
#define LINEERASE	030		/* line kill character       ^X */
#define LINERTYPE	022		/* line reprint character    ^R */
#define COMPLETE	033		/* word completion character ^[ */
#define ENDINGS		'?'		/* print matches character      */
#define ALTMODE		'!'		/* use other lookup mode	*/
#define THESMODE	'#'		/* lookup in thesaurus (sah)	*/
#define DEFINE		0		/* DEFINE mode			*/
#define SPELL		1		/* SPELL mode			*/
#define	THESAURUS	2		/* THESAURUS mode		*/

struct sgttyb sgttyb;			/* tty modes when interactive	*/
struct sgttyb rsgttyb;			/* original tty modes		*/

extern int errno;

/*
 * added at IU
 * ..sahayman 89/02/28
 */

struct tchars tchars;
struct ltchars ltchars;

int connectup(), socket(), connet(), byebye(), endings(), strlen(), send();
int strncmp(), connect(), getline();
void setitimer(), bzero(), define(), spell(), bcopy(), complete(), help();
void define(), listlines(), putline();
char *strcpy();

int mode = DEFINE;
FILE *WebsterSock;			/* for reading from the server	*/
int interactive = 0;			/* 1 when running interactive	*/
int full_search = 0;
char *pager = NULL;
char *getenv();

extern struct object *command_giver;
struct object c_g;

void
add_message2(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *fmt;
int a1, a2, a3, a4, a5, a6, a7, a8, a9;
{
  if (c_g.flags & O_DESTRUCTED) return;
/*  add_output(c_g.interactive, 0, fmt, a1, a2, a3, a4, a5, a6,
    a7, a8, a9); */
  add_message(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
}

int webster(arg1, arg2)
char *arg1, *arg2;
{
	int didone = 0;
	struct itimerval value;
	void go_out();
	char aa[100], bb[100];

	mode = DEFINE;

	/*
	 * Connect to the server.
	 */

	if (!connectup()) return 0;
	c_g = *command_giver;

        value.it_value.tv_sec = 10;   /* 10 seconds, no more! */
	value.it_interval.tv_sec =10; /* Kickaha!             */
        setitimer(ITIMER_PROF, &value, NULL);
	signal(SIGPROF, go_out);

	bzero(aa, 100);
	bzero(bb, 100);

	if (arg1) sscanf(arg1, "%s %s", aa, bb);

	if (bb[0] == (char)0) strcpy(bb, arg1);

	/*
	 * If we were given command line arguments, just
	 * try to define each word.
	 */
	if (bb != (char *)0) {
			if (*aa == '-') {
				switch (*(aa+1)) {
				case 'd':
					mode = DEFINE;
					break;
				case 's':
					mode = SPELL;
					break;
				case 't':
					mode = THESAURUS;
					break;
				case 'f':
					full_search++;
					break;
				default:
					add_message2("Usage: webster [-d] [-s] [-t] [-f] [word]\n");
                                        value.it_value.tv_sec = 60*5;  /* 5 minutes must be enough */
                                        value.it_interval.tv_sec = 3600;       /* Kickaha ! */
                                        setitimer(ITIMER_PROF, &value, NULL);
					return 1;
				}
			}

			/*
			 * Look up the word.
			 */
			if (mode == DEFINE || mode == THESAURUS)
				define(bb, mode);
			else
				spell(bb);

			didone++;

		if (didone)
		{
                        value.it_value.tv_sec = 60*5;  /* 5 minutes must be enough */
                        value.it_interval.tv_sec = 3600;       /* Kickaha ! */
                        setitimer(ITIMER_PROF, &value, NULL);
			return 0;
                }
	}


	/*
	 * If no arguments were given, set up the
	 * terminal modes and run interactively.
	 */
/*	setup();
	interact(); */
        value.it_value.tv_sec = 60*5;  /* 5 minutes must be enough */
        value.it_interval.tv_sec = 3600;       /* Kickaha ! */
        setitimer(ITIMER_PROF, &value, NULL);
	add_message2("No word specified!\n");
	return 0;
}

void go_out()
{
  struct itimerval value;
  value.it_value.tv_sec = 60*5;  /* 5 minutes must be enough */
  value.it_interval.tv_sec = 3600;       /* Kickaha ! */
  setitimer(ITIMER_PROF, &value, NULL);
  signal(SIGPROF, SIG_DFL);
  add_message2("Couldn't reach server host!\n");
}

/*
 * connectup - connects to the Webster server.
 */
int connectup()
{
	int s;
	struct sockaddr_in sin;
	struct hostent *hp;
	struct servent *sp;
	struct hostent *gethostbyname();
	struct servent *getservbyname();
	extern int byebye();

	char *whichhost, *getenv();

	whichhost = getenv("WEBSTERHOST");
	if ( whichhost == NULL )
		whichhost = WEBSTERHOST;

	/*
	 * Look up the host in the host file.
	 */
	if ((hp = gethostbyname(whichhost)) == NULL) {
		add_message2("webster: %s: unknown host.\n", whichhost);
		return 0;
	}

	bzero(&sin, sizeof(struct sockaddr_in));

	/*
	 * Build the server's address.
	 */
	sin.sin_family = AF_INET;
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);

	if ((sp = getservbyname(WEBSTERNAME, "tcp")) == NULL)
		sin.sin_port = htons(WEBSTERPORT);
	else
		sin.sin_port = sp->s_port;

	/*
	 * Get a TCP socket.
	 */
	if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
/*		perror("webster: socket"); */
		add_message2("Webster couldn't get TCP socket.\n");
		return 0;
	}

	/*
	 * Try to connect.
	 */
/*        signal(SIGINT, byebye); */
	if (connect(s, &sin, sizeof(struct sockaddr_in)) < 0) {
		add_message2("Webster: host %s: ", whichhost);
/*		perror("connect"); */
		add_message2("couldn't connect (try again).\n");
		return 0;
	}
/*        signal(SIGINT, SIG_DFL); */

	/*
	 * Open the socket for stdio.
	 */
	WebsterSock = fdopen(s, "r");
	return 1;
}

/*
 * setup - turns on CBREAK, turns off ECHO.  Also trap signals.
 */
void setup()
{
	extern int byebye();
	extern int suspend();

	interactive = 1;
/*	ioctl(0, TIOCGETP, &sgttyb);
	ioctl(0, TIOCGETC, &tchars);
	ioctl(0, TIOCGLTC, &ltchars);

	rsgttyb = sgttyb;

	signal(SIGINT, byebye);
	signal(SIGQUIT, byebye);
	signal(SIGTSTP, suspend);

	sgttyb.sg_flags |= CBREAK;
	sgttyb.sg_flags &= ~ECHO;
	ioctl(0, TIOCSETP, &sgttyb); */
}

/*
 * interact - interact with the user.
 */
void interact()
{
	/*
	int c;
	*/
	char c;
	char buf[1024];
	char *s, *t;

	/*
	 * Forever...
	 */
	for (;;) {
		/*
		 * Prompt for a word.
		 */
		s = buf;
		write(1, "Word: ", 6);

		/*
		 * Forever... read characters.  We
		 * break out of this from inside.
		 */
		for (;;) {
			/*
			if ( (c = getchar()) < 0 )
			*/
			if (read(0, &c, 1) <= 0)
				byebye();

			/*
			 * Added at IU - use the user's own backspace,
			 * and line erase characters.  Check for EOF too.
			 *
			 * Could also do worderase here if really keen
			 * but would require looking up ltchars
			 * and I'm lazy, and we already have sgttyb sitting
			 * around... Should also probably handle EOF here.
			 * ..sahayman 89/02/28
			 */
			
			if ( c == sgttyb.sg_erase )
				c = BACKSPACE;
			else if ( c == sgttyb.sg_kill )
				c = LINEERASE;
			else if ( c == ltchars.t_werasc )
				c = WORDERASE;
			else if ( c == ltchars.t_rprntc )
				c = LINERTYPE;
			else if ( c == tchars.t_brkc )
				c = COMPLETE;
			else if ( c == tchars.t_eofc )
				byebye();



			switch (c) {
			case BACKSPACE:
				/*
				 * If not at the beginning of a line,
				 * back up one character.
				 */
				if (s > buf) {
					write(1, "\b \b", 3);
					s--;
				}
				continue;
			case WORDERASE:
				/*
				 * Until we hit beginning of line
				 * or beginning of word, back up.
				 */
				while ((s > buf) && (*s != ' ')) {
					write(1, "\b \b", 3);
					s--;
				}
				continue;
			case LINEERASE:
				/*
				 * Until we hit beginning of line,
				 * back up.
				 */
				while (s > buf) {
					write(1, "\b \b", 3);
					s--;
				}
				continue;
			case LINERTYPE:
				/*
				 * Retype the line.
				 */
				write(1, "\r\nWord: ", 8);

				for (t=buf; t < s; t++)
					write(1, t, 1);
				continue;
			case COMPLETE:
				/*
				 * Try to complete what they typed
				 * so far.  Put the pointer at the
				 * end of the new word.
				 */
				*s = NULL;
				complete(buf);
				for (s=buf; *s; s++)
					;
				continue;
			case ENDINGS:
				/*
				 * If it's the first character,
				 * then print some help.  Otherwise,
				 * try to find endings for the word.
				 * endings() returns 1 if no endings
				 * were found, 0 if some were found.
				 * This tells us whether to reprint
				 * the current word or not.
				 */
				if (s == buf) {
					help();
				}
				else {
					*s = NULL;
					if (endings(buf) == 0) {
						write(1, "Word: ", 6);

						for (s=buf; *s; s++)
							write(1, s, 1);
					}

					continue;
				}
				break;
			case '\n':
				/*
				 * If at the start of a word,
				 * newline is exit.
				 */
				if (s == buf)
					byebye();

				/*
				 * Otherwise, try to define
				 * the word.
				 */
				*s = NULL;
				write(1, "\n", 1);

				if (*buf == ALTMODE) {
					if (strlen(buf) == 1)
						break;
					if (mode == DEFINE || mode == THESAURUS)
						spell(buf+1);
					else
						define(buf+1,mode);
				}
				else if ( *buf == THESMODE ) {
					int oldmode = mode;
					if ( strlen(buf) == 1 )
						break;
					mode = THESAURUS;
					define(buf+1,mode);
					mode = oldmode;
				} else {
					if (mode == DEFINE || mode == THESAURUS)
						define(buf,mode);
					else
						spell(buf);
				}
				fflush(stdout);

				break;
			default:
				/*
				 * Echo the character and copy it.
				 */
				write(1, &c, 1);
				*s++ = c;
				continue;
			}

			break;
		}
	}
}

/*
 * define - try to define a word and print its definition.
 */
void define(word, mode2)
char *word;
int mode2;
{
	int c, refs;
	char buf[1024];

	/*
	 * first mode is the normal dictionary so that
	 * we don't send INDEX commands to an old server
	 */
	static int last_mode = DEFINE;
	static int last_full = 0;

	FILE *fpager, *popen();
	/*
	 * Command is "DEFINE<space>word<nl>".
	 */

	/*
	 * Send appropriate INDEX instruction if anything has changed
	 * since last time.
	 */
	if ( mode2 != last_mode || full_search != last_full ) {
	    last_mode = mode2;
	    last_full = full_search;

	    if ( mode2 == DEFINE ) {
		sprintf(buf, "INDEX %s\r\n",
			full_search ? "dictionary-full" : "dictionary" );
	    } else if ( mode2 == THESAURUS ) {
		sprintf(buf, "INDEX thesaurus\r\n");
	    }

	    if ( send(fileno(WebsterSock), buf, strlen(buf), 0) < 0 ) {
/*		perror("webster: send"); */
		add_message2("Webster couldn't send!\n");
		byebye();
	    }
	    /*
	    getline(buf);
	    */
	}

	sprintf(buf, "DEFINE %s\r\n", word);

	/*
	 * Send the command.
	 */
	if (send(fileno(WebsterSock), buf, strlen(buf), 0) < 0) {
/*		perror("webster: send"); */
		add_message2("Webster couldn't send.\n");
		byebye();
	}

	/*
	 * Read the first line back from the server.  This
	 * line tells us what the result of our DEFINE
	 * request was.
	 */
	getline(buf);

	/*
	 * "WILD<space>0<nl>" means they used wild cards and no
	 * matches were found.
	 */
	if (!strncmp(buf, "WILD 0", 6)) {
		add_message2("No match.\n");
		return;
	}

	/*
	 * "WILD<nl>" means that the wildcard matched, so we
	 * print a list of possible matches.
	 */
	if (!strncmp(buf, "WILD", 4)) {
		add_message2("Possible matches are:\n");

		/*
		 * List lines.
		 */
		listlines(0, 1);
		add_message2("\n");
		return;
	}

	/*
	 * "SPELLING<space>0<nl>" means the word is not defined,
	 * and there are no alternate spellings.
	 */
	if (!strncmp(buf, "SPELLING 0", 10)) {
		add_message2("No %s",
			mode2 == THESAURUS ? "thesaurus entry" : "definition ");
                add_message2(" for '%s'.\n", word);
		/*
		 * Everywhere else there's a blank line between words.
		 */
                add_message2("\n");
/*		putchar('\n'); */
		return;
	}

	/*
	 * "SPELLING<nl>" means the word is not defined, but
	 * some alternate spellings were found.  Print
	 * them out.
	 */
	if (!strncmp(buf, "SPELLING", 8)) {
		add_message2("No %s",
			mode2 == THESAURUS ? "thesaurus entry" : "definition ");
                add_message2(" for '%s'. Maybe you mean:\n", word);

		/*
		 * List lines.
		 */
		listlines(0, 1);
/*		putchar('\n'); */
		add_message2("\n");
		return;
	}

	/*
	 * "DEFINITION<space>n<nl>" means the word is defined,
	 * and there are n cross-references.
	 */
	if (!strncmp(buf, "DEFINITION", 10)) {
		/*
		 * Use pager if desired
		 */

		if ( pager ) {
		    fpager = popen(pager, "w");
		    if ( fpager == NULL ) {
			add_message2("Warning: pager program.\n");
/*			perror(pager); */

			fpager = stdout;
		    }
		} else {
		    fpager = stdout;
		}
		sscanf(buf+11, "%d", &refs);

		
		/*
		 * Print any cross references.
		 */
		if (refs > 0) {
			add_message2("Cross references:\n");

			/*
			 * List lines.
			 */
			listlines(refs, 1);
			add_message2(" \n");
		}

		/*
		 * Print the definition.
		 */
		while ((c = getc(WebsterSock)) != EOF) {
			if (c == EOFCH)
				break;

			c &= 0177;
			add_message2("%c", c);
		}

		add_message2("\n");
		if ( fpager != stdout )
		    pclose(fpager);
		return;
	}

	/*
	 * An error message.
	 */
	if (!strncmp(buf, "ERROR ", 6)) {
		if (!strncmp(buf+6, "FATAL", 5)) {
			add_message2("%s\n", buf+11);
			byebye();
		}
		else {
			add_message2("%s\n", buf+17);
			return;
		}
	}

	/*
	 * Should never get here.
	 */
	while (((c = getc(WebsterSock)) != EOF) && (c != EOFCH))
		;
}

/*
 * spell - look up a word and see if it's spelled correctly.
 */
void spell(word)
char *word;
{
	int c;
	char buf[1024];

	/*
	 * Command is "SPELL<space>word<nl>".
	 */
	sprintf(buf, "SPELL %s\r\n", word);

	/*
	 * Send the command.
	 */
	if (send(fileno(WebsterSock), buf, strlen(buf), 0) < 0) {
/*		perror("webster: send"); */
		add_message2("Webster couldn't send!\n");
		byebye();
	}

	/*
	 * Read the first line back from the server.  This
	 * line tells us what the result of our SPELL
	 * request was.
	 */
	getline(buf);

	/*
	 * "SPELLING<space>0<nl>" means the word is not spelled correctly,
	 * and there are no alternate spellings.
	 */
	if (!strncmp(buf, "SPELLING 0", 10)) {
		add_message2("'%s' is not a correct spelling.\n", word);
		return;
	}

	/*
	 * "SPELLING<space>1<nl>" means the word is spelled correctly.
	 */
	if (!strncmp(buf, "SPELLING 1", 10)) {
		add_message2("'%s' is spelled correctly.\n", word);
		return;
	}

	/*
	 * "SPELLING<nl>" means the word is not spelled correctly, but
	 * some alternate spellings were found.  Print them out.
	 */
	if (!strncmp(buf, "SPELLING", 8)) {
		add_message2("No spelling for '%s'.  Maybe you mean:\n", word);

		/*
		 * List lines.
		 */
		listlines(0, 1);
		add_message2("\n");
/*		putchar('\n'); */
		return;
	}

	/*
	 * An error message.
	 */
	if (!strncmp(buf, "ERROR ", 6)) {
		if (!strncmp(buf+6, "FATAL", 5)) {
			add_message2("%s\n", buf+11);
			byebye();
		}
		else {
			add_message2("%s\n", buf+17);
			return;
		}
	}

	/*
	 * Should never get here.
	 */
	while (((c = getc(WebsterSock)) != EOF) && (c != EOFCH))
		;
}

/*
 * complete - try to complete the word.
 */
void complete(word)
char *word;
{
	int c;
	char buf[1024];
	char *s;

	/*
	 * Command is "COMPLETE<space>word<nl>".
	 */
	sprintf(buf, "COMPLETE %s\r\n", word);

	/*
	 * Send the command.
	 */
	if (send(fileno(WebsterSock), buf, strlen(buf), 0) < 0) {
/*		perror("webster: send"); */
		add_message2("Webster couldn't send command.\n");
		byebye();
	}

	/*
	 * Get the first line from the server, which tells
	 * us the reult of our request.
	 */
	getline(buf);

	/*
	 * "AMBIGUOUS<space>n<nl>" means the word is ambiguous,
	 * with n possible matches.  We ignore the n, and just
	 * beep.
	 */
	if (!strncmp(buf, "AMBIGUOUS", 9)) {
		add_message2("");
/*		write(1, "\007", 1); */
		return;
	}

	/*
	 * "COMPLETION<space>full-word<nl>" means the
	 * word was completed.  Erase what they typed
	 * and print the new word over it.  This takes
	 * care of things if they used wildcards.
	 */
	if (!strncmp(buf, "COMPLETION", 10)) {
		for (s=word; *s; s++)
		{
			add_message2("\b");
/*			write(1, "\b", 1); */
                }

		s = buf+11;
		while (((*s & 0177) != '\r') && ((*s & 0177) != '\n') &&
		       ((*s & 0177) != NULL)) {
/*			write(1, s, 1); */
			add_message2("%c", s);
			s++;
		}

		/*
		 * Put the new word back into word.  This
		 * gets rid of the wildcards here.
		 */
		*s = NULL;
		strcpy(word, buf+11);

		return;
	}

	/*
	 * An error message.
	 */
	if (!strncmp(buf, "ERROR ", 6)) {
		if (!strncmp(buf+6, "FATAL", 5)) {
			add_message2("%s\n", buf+11);
			byebye();
		}
		else {
			add_message2("%s\n", buf+17);
			return;
		}
	}

	/*
	 * Should never get here.
	 */
	while (((c = getc(WebsterSock)) != EOF) && (c != EOFCH))
		;
}

/*
 * endings - find possible endings for a word.
 */
int endings(word)
char *word;
{
	int c;
	char buf[1024];

	/*
	 * Command is "ENDINGS<space>word<nl>".
	 */
	sprintf(buf, "ENDINGS %s\r\n", word);

	/*
	 * Send the command.
	 */
	if (send(fileno(WebsterSock), buf, strlen(buf), 0) < 0) {
/*		perror("webster: send"); */
		add_message2("Webster couldn't send command!\n");
		byebye();
	}

	/*
	 * Get the first line from the server, which tells
	 * us the result of the search.
	 */
	getline(buf);

	/*
	 * "MATCHS<space>0<nl>" means nothing matched,
	 * so we beep at them.
	 */
	if (!strncmp(buf, "MATCHS 0", 8)) {
/*		write(1, "\007", 1); */
		add_message2("");
		return(1);
	}

	/*
	 * "MATCHS<nl>" means there were matches, so
	 * print them out.
	 */
	if (!strncmp(buf, "MATCHS", 6)) {
		add_message2("\nMaybe you mean:\n");

		/*
		 * List lines.
		 */
		listlines(0, 0);
		add_message2("\n");
/*		putchar('\n'); */
		return(0);
	}

	/*
	 * An error message.
	 */
	if (!strncmp(buf, "ERROR ", 6)) {
		if (!strncmp(buf+6, "FATAL", 5)) {
			add_message2("%s\n", buf+11);
			byebye();
		}
		else {
			add_message2("%s\n", buf+17);
			return(0);
		}
	}

	/*
	 * Should never get here.
	 */
	while (((c = getc(WebsterSock)) != EOF) && (c != EOFCH))
		;

	return(0);
}

/*
 * getline - read one line from the server and put it in s.
 */
int getline(s)
char *s;
{
	int c;

	/*
	 * Read in chars.  If we hit EOFCH, return
	 * 0.
	 */
	while ((c = getc(WebsterSock)) != EOF) {
		if (c == EOFCH)
			return(0);

		c &= 0177;

		if (c == '\r')
			continue;

		if (c == '\n')
			break;

		*s++ = c;
	}

	*s = NULL;
	return(1);
}

/*
 * listlines - list WILD-style lines on the screen.
 */
void listlines(n, num)
int n;
int num;
{
	char buf[1024];
	int col;

	add_message2(" ");

	/*
	 * If n is non-zero, we only want to list n lines.
	 * Otherwise, we go till we hit EOFCH.  Lines are
	 * printed in four columns.
	 */
	if (n) {
		col = 0;
		while (n-- > 0) {
			getline(buf);
			putline(buf, num);

			if (++col == 3) {
				add_message2("\n ");
				col = 0;
			}
		}
	}
	else {
		col = 0;
		while (getline(buf) > 0) {
			putline(buf, num);

			if (++col == 3) {
				add_message2("\n ");
				col = 0;
			}
		}
	}

	if (col)
	{
/*		putchar('\n'); */
		add_message2("\n");
        }
}

/*
 * putline - put out a line, if num is 0, skip the line number.
 */
void putline(buf, num)
char *buf;
int num;
{
	int lnum;
	char line[1024];

	sscanf(buf, "%d %[^\n]", &lnum, line);

/*	sscanf(line, "%s\n", line);
	line[strlen(line)] = 0; */

	if (num)
	{
		add_message2("%2d. ", lnum);
/*		add_message2("%-22s", line); */
		add_message2("%s", line);
        }
	else
	{
/*		add_message2("%-26s", line); */
		add_message2("%s", line);
        }
}

/*
 * help - print a help message.
 */
void help()
{
	add_message2("\n   Type in the word you want defined, or a blank line to exit. Additionally,\n");
	add_message2("Webster can match words using wildcards.  The character '%%' in a word means\n");
	add_message2("match exactly one character; while the character '*' means match zero or more\n");
	add_message2("characters.  Typing \"!word\" will check the spelling of the word instead\n");
	add_message2("of checking its definition.\n");
	add_message2("   Typing a partial word followed by '?' will print all the words in the\n");
	add_message2("dictionary which match your partial word. Typing a partial word followed by an\n");
	add_message2("ESCape character will try to complete the word for you.  If the partial word\n");
	add_message2("is ambiguous, Webster will beep at you.  Note that you can use the wildcards\n");
	add_message2("along with ESC and ?.  For example (the underlined parts are typed by the\n");
	add_message2("user, the rest by Webster),\n");
	add_message2("\n");
	add_message2("Word: balla?   Maybe you mean:\n");
	add_message2("      ------\n");
	add_message2("  1. ballad           2. ballade          3. baladry         4. ballast\n");
	add_message2("Word: pluria<ESC>xial\n");
	add_message2("      -----------\n");
	add_message2("Word: plu*x<ESC>\n");
	add_message2("      ----------\n");
	add_message2("Word: pluriaxial\n");
	add_message2("\n---- End of Help File ----\n\n");
}

/*
 * byebye - called on exit.
 */
int byebye()
{
	/*
	 * If interactive, reset the tty modes.
	 */
/*	if (interactive)
		ioctl(0, TIOCSETP, &rsgttyb); */

	/*
	 * Close the socket and exit.
	 */
	fclose(WebsterSock);
/*	write(1, "\n", 1); */
	add_message2("\n");
	return 0;
}

/*
 * suspend - reset tty modes and supend ourselves.
 */
void suspend2()
{
	extern int suspend();

	/*
	 * Reset tty modes and suspend.
	 */
/*	ioctl(0, TIOCSETP, &rsgttyb);
	signal(SIGTSTP, SIG_DFL);
	blocked = sigsetmask(0);
	kill(0, SIGTSTP); */

	/*
	 * We come here on SIGCONT.  Reset
	 * the signal mask and tty modes.
	 */
/*	sigsetmask(blocked);
	signal(SIGTSTP, suspend);
	ioctl(0, TIOCSETP, &sgttyb);	 */
}
