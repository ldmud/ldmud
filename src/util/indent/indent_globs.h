/*
 * Copyright (c) 1985 Sun Microsystems, Inc.
 * Copyright (c) 1980 The Regents of the University of California.
 * Copyright (c) 1976 Board of Trustees of the University of Illinois.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley, the University of Illinois,
 * Urbana, and Sun Microsystems, Inc.  The name of either University
 * or Sun Microsystems may not be used to endorse or promote products
 * derived from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)indent_globs.h	5.7 (Berkeley) 9/15/88
 */

#include <stdio.h>
#include <stdlib.h>
/* Do the same thing, but abort with an error if out of memory
   (see globs.c).  */
char *xmalloc ();
char *xrealloc ();

#ifndef M_UNIX
#include <strings.h>
#else
#include <string.h>
#endif

#define BACKSLASH '\\'
#define label_offset 2		/* number of levels a label is placed to left
				 * of code */
/* Initial size of internal buffers (they are extended as necessary).  */
#define bufsize 1000

#define tabsize 8		/* the size of a tab */
#define tabmask 0177770		/* mask used when figuring length of lines
				 * with tabs */

enum codes {code_eof = 0,  /* end of file */
	      newline,
	      lparen, /* '(' or '['.  Also '{' in an initialization.  */
	      rparen, /* ')' or ']'.  Also '}' in an initialization.  */
	      unary_op, binary_op, postop,
	      question, casestmt, colon, semicolon, lbrace, rbrace,
	      ident, /* string or char literal, identifier, number */
	      comma, comment, swstmt,
	      preesc,  /* '#'.  */
	      form_feed, decl,
	      sp_paren, /* if, for, or while token */
	      sp_nparen, ifstmt, whilestmt,
	      forstmt, stmt, stmtl, elselit, dolit, dohead, ifhead,
	      elsehead, period };
	
#define false 0
#define true  1

#ifdef MAIN
#define GLOBAL
#else /* !MAIN */
#define GLOBAL	extern
#endif /* MAIN */

/* Name of input file.  */
extern char *in_name;

GLOBAL char *in_prog; /* pointer to the null-terminated input program */

/* Point to the position in the input program which we are currently
   looking at.  */
GLOBAL char *in_prog_pos;

/* Point to the start of the current line.  */
GLOBAL char *cur_line;

/* Size of the input program, not including the ' \n\0' we add at the end */
GLOBAL int in_prog_size;

GLOBAL FILE       *output;		/* the output file */

/* The following macro employs traditional token-pasting.  If one desires
   to make this thing run without giving gcc the -traditional option
   or it becomes inconvenient, it should be rewritten to use a struct
   for each buffer (along the lines of the 'struct buf' later in this
   file).  */
#ifdef	__STDC__
#define	paste(a, b)	a ## b
#else
#define	paste(a, b)	a/**/b
#endif
#define check_size(name) \
	if (paste(e_,name) >= paste(l_,name)) { \
	    register nsize = paste(l_,name)-paste(s_,name)+400; \
	    paste(name,buf) = (char *) xrealloc(paste(name,buf), nsize); \
	    paste(e_,name) = paste(name,buf) + \
	      (paste(e_,name)-paste(s_,name)) + 1; \
	    paste(l_,name) = paste(name,buf) + nsize - 5; \
	    paste(s_,name) = paste(name,buf) + 1; \
	}

GLOBAL char       *labbuf;		/* buffer for label */
GLOBAL char       *s_lab;		/* start ... */
GLOBAL char       *e_lab;		/* .. and end of stored label */
GLOBAL char       *l_lab;		/* limit of label buffer */

GLOBAL char       *codebuf;		/* buffer for code section */
GLOBAL char       *s_code;		/* start ... */
GLOBAL char       *e_code;		/* .. and end of stored code */
GLOBAL char       *l_code;		/* limit of code section */

GLOBAL char       *combuf;		/* buffer for comments */
GLOBAL char       *s_com;		/* start ... */
GLOBAL char       *e_com;		/* ... and end of stored comments */
GLOBAL char       *l_com;		/* limit of comment buffer */

GLOBAL char       *buf_ptr;		/* ptr to next character to be taken from
					 * in_buffer */
GLOBAL char       *buf_end;		/* ptr to first after last char in in_buffer */

/* pointer to the token that lexi() has just found */
GLOBAL char *token;
/* points to the first char after the end of token */
GLOBAL char *token_end;

/* Used to keep track of buffers.  */
struct buf {
  char *ptr;  /* points to the start of the buffer */
  char *end;  /* points to the character beyond the last one (e.g. is equal
		 to ptr if the buffer is empty).  */
  int size;  /* how many chars are currently allocated.  */
};
/* Insure that BUFSTRUC has at least REQ more chars left, if not extend it.
   Note:  This may change bufstruc.ptr.  */
#define need_chars(bufstruc, req) \
  if ((bufstruc.end - bufstruc.ptr + (req)) >= bufstruc.size) \
    {\
      int cur_chars = bufstruc.end - bufstruc.ptr;\
      bufstruc.size *= 2;\
      bufstruc.ptr = xrealloc(bufstruc.ptr,bufstruc.size);\
      bufstruc.end = bufstruc.ptr + cur_chars;\
    }
/* Initialize BUFSTRUC.  */
#define init_buf(bufstruc) \
  bufstruc.end = bufstruc.ptr = xmalloc(bufsize),\
  bufstruc.size = bufsize

/* Buffer in which to save a comment which occurs between an if(), while(),
   etc., and the statement following it.  Note: the fact that we point
   into this buffer, and that we might realloc() it (via the
   need_chars macro) is a bad thing (since when the buffer is
   realloc'd its address might change, making any pointers into it
   point to garbage), but since the filling of the buffer (hence the
   need_chars) and the using of the buffer (where buf_ptr points into
   it) occur at different times, we can get away with it (it would not
   be trivial to fix).  */
GLOBAL struct buf save_com;

GLOBAL char       *bp_save;		/* saved value of buf_ptr when taking input
					 * from save_com */
GLOBAL char       *be_save;		/* similarly saved value of buf_end */


GLOBAL int         pointer_as_binop;
GLOBAL int         blanklines_after_declarations;
GLOBAL int         blanklines_before_blockcomments;
extern int blanklines_after_procs;
GLOBAL int         blanklines_around_conditional_compilation;
GLOBAL int         swallow_optional_blanklines;
GLOBAL int         n_real_blanklines;
GLOBAL int         prefix_blankline_requested;
GLOBAL int         postfix_blankline_requested;
GLOBAL int         break_comma;	/* when true and not in parens, break after a
				 * comma */

/* number of spaces to indent braces from the suround if, while, etc.
   in -bl (bype_2 == 0) code */
GLOBAL int brace_indent;

GLOBAL int         btype_2;	/* when true, brace should be on same line as
				 * if, while, etc */
/* If true, a space is inserted between if, while, or for, and a semicolon
   for example
   while (*p++ == ' ') ;
   */
GLOBAL int space_sp_semicolon;

/* True if a #else or #endif has been encountered.  */
extern int else_or_endif;

GLOBAL int         case_ind;		/* indentation level to be used for a "case
					 * n:" in spaces */
GLOBAL int         code_lines;		/* count of lines with code */
GLOBAL int         had_eof;		/* set to true when input is exhausted */
GLOBAL int         line_no;		/* the current line number. */
GLOBAL int         max_col;		/* the maximum allowable line length */
GLOBAL int         verbose;		/* when true, non-essential error messages are
					 * printed */
GLOBAL int         cuddle_else;		/* true if else should cuddle up to '}' */
GLOBAL int         star_comment_cont;	/* true iff comment continuation lines should
					 * have stars at the beginning of each line. */
GLOBAL int         comment_delimiter_on_blankline;
GLOBAL int         troff;		/* true iff were generating troff input */
GLOBAL int         procnames_start_line;/* if true, the names of procedures
					 * being defined get placed in column
					 * 1 (ie. a newline is placed between
					 * the type of the procedure and its
					 * name) */
GLOBAL int         proc_calls_space;	/* If true, procedure calls look like:
					 * foo(bar) rather than foo (bar) */
GLOBAL int         format_col1_comments;/* If comments which start in column 1
					 * are to be magically reformatted */
/* If any comments are to be reformatted */
GLOBAL int format_comments;

extern int  suppress_blanklines;/* set iff following blanklines should be
				 * suppressed */
GLOBAL int         continuation_indent;	/* set to the indentation between the edge of
					 * code and continuation lines in spaces */
GLOBAL int         lineup_to_parens;	/* if true, continued code within parens will
					 * be lined up to the open paren */

/* The position that we will line the current line up with when it
   comes time to print it (if we are lining up to parentheses).  */
extern int paren_target;

GLOBAL int         Bill_Shannon;	/* true iff a blank should always be inserted
					 * after sizeof */
GLOBAL int         blanklines_after_declarations_at_proctop;	/* This is vaguely
								 * similar to
								 * blanklines_after_decla
								 * rations except that
								 * it only applies to
								 * the first set of
								 * declarations in a
								 * procedure (just after
								 * the first '{') and it
								 * causes a blank line
								 * to be generated even
								 * if there are no
								 * declarations */
GLOBAL int         block_comment_max_col;
GLOBAL int         extra_expression_indent;	/* True if continuation lines from the
						 * expression part of "if(e)",
						 * "while(e)", "for(e;e;e)" should be
						 * indented an extra tab stop so that
						 * they don't conflict with the code
						 * that follows */

extern int         lpc;

/* The following are all controlled by command line switches
   (as are some of the things above).  */
extern int         leave_comma;	/* if true, never break declarations after
				 * commas */
extern int         decl_com_ind;	/* the column in which comments after
				 * declarations should be put */
extern int         case_indent;	/* The distance to indent case labels from the
				 * switch statement */
extern int         com_ind;	/* the column in which comments to the right
				 * of code should start */
extern int         decl_indent;	/* column to indent declared identifiers to */
extern int         ljust_decl;	/* true if declarations should be left
				 * justified */
extern int         unindent_displace;	/* comments not to the right of code
					 * will be placed this many
					 * indentation levels to the left of
					 * code */
extern int         else_if;	/* True iff else if pairs should be handled
				 * specially */
/* Number of spaces to indent parameters.  */
extern int         indent_parameters;
/* The size of one indentation level in spaces.  */
extern int         ind_size;

/* -troff font state information */

struct fstate {
    char        font[4];
    char        size;
    int         allcaps:1;
};
char       *chfont();

GLOBAL struct fstate
            keywordf,		/* keyword font */
            stringf,		/* string font */
            boxcomf,		/* Box comment font */
            blkcomf,		/* Block comment font */
            scomf,		/* Same line comment font */
            bodyf;		/* major body font */

/* Initial size for the parser's stacks.  (p_stack, il, and cstk).  */
#define INITIAL_STACK_SIZE 2

struct parser_state {
    struct parser_state *next;
    enum codes last_token;
    struct fstate cfont;	/* Current font */

    /* This is the parsers stack, and the current allocated size.  */
    enum codes *p_stack;
    int p_stack_size;

    /* This stack stores indentation levels */
    /* Currently allocated size is stored in p_stack_size.  */
    int *il;

    /* Used to store case stmt indentation levels.  */
    /* Currently allocated size is stored in p_stack_size.  */
    int *cstk;

    /* Pointer to the top of stack of the p_stack, il and cstk arrays. */
    int         tos;

    int         box_com;	/* set to true when we are in a "boxed"
				 * comment. In that case, the first non-blank
				 * char should be lined up with the / in '/ *' */
    int         comment_delta,
                n_comment_delta;
    int         cast_mask;	/* indicates which close parens close off
				 * casts */
    int         sizeof_mask;	/* indicates which close parens close off
				 * sizeof''s */
    int         block_init;	/* true iff inside a block initialization */
    int         block_init_level;	/* The level of brace nesting in an
					 * initialization */
    int         last_nl;	/* this is true if the last thing scanned was
				 * a newline */
    int         in_or_st;	/* Will be true iff there has been a
				 * declarator (e.g. int or char) and no left
				 * paren since the last semicolon. When true,
				 * a '{' is starting a structure definition or
				 * an initialization list */
    int         bl_line;	/* set to 1 by dump_line if the line is blank */
    int         col_1;		/* set to true if the last token started in
				 * column 1 */
    int         com_col;	/* this is the column in which the current
				 * coment should start */
    int         com_lines;	/* the number of lines with comments, set by
				 * dump_line */
    int         dec_nest;	/* current nesting level for structure or init */
    int         decl_on_line;	/* set to true if this line of code has part
				 * of a declaration on it */
    int         i_l_follow;	/* the level in spaces to which ind_level should be set
				 * after the current line is printed */
    int         in_decl;	/* set to true when we are in a declaration
				 * stmt.  The processing of braces is then
				 * slightly different */
    int         in_stmt;	/* set to 1 while in a stmt */
    int         ind_level;	/* the current indentation level in spaces */
    int         ind_stmt;	/* set to 1 if next line should have an extra
				 * indentation level because we are in the
				 * middle of a stmt */
    int         last_u_d;	/* set to true after scanning a token which
				 * forces a following operator to be unary */
    int         out_coms;	/* the number of comments processed, set by
				 * pr_comment */
    int         out_lines;	/* the number of lines written, set by
				 * dump_line */
    int         p_l_follow;	/* used to remember how to indent following
				 * statement */
    int         paren_level;	/* parenthesization level. used to indent
				 * within stmts */
    /* Column positions of paren at each level.  If positive, it
       contains just the number of characters of code on the line up to
       and including the right parenthesis character.  If negative, it
       contains the opposite of the actual level of indentation in
       characters (that is, the indentation of the line has been added
       to the number of characters and the sign has been reversed to
       indicate that this has been done).  */
    short *paren_indents;	/* column positions of each paren */
    int paren_indents_size;  /* Currently allocated size.  */

    int         pcase;		/* set to 1 if the current line label is a
				 * case.  It is printed differently from a
				 * regular label */
    int         search_brace;	/* set to true by parse when it is necessary
				 * to buffer up all info up to the start of a
				 * stmt after an if, while, etc */
    int         use_ff;		/* set to one if the current line should be
				 * terminated with a form feed */
    int         want_blank;	/* set to true when the following token should
				 * be prefixed by a blank. (Said prefixing is
				 * ignored in some cases.) */
    int         its_a_keyword;
    int         sizeof_keyword;
    int         dumped_decl_indent;
    int         in_parameter_declaration;
    char *procname;	/* The name of the current procedure */
    char *procname_end;  /* One char past the last one in procname */
    int         just_saw_decl;
};
/* All manipulations of the parser stack occur at the tos
   (via the macro ps).  The elements of the stack below it are kept in
   a linked list via the next field.  */
extern struct parser_state *parser_state_tos;

/* The column in which comments to the right of #else and #endif should
   start.  */
extern int else_endif_col;
