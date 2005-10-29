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
 */

#ifndef lint
static char sccsid[] = "@(#)lexi.c	5.11 (Berkeley) 9/15/88";
#endif /* not lint */

/*
 * Here we have the token scanner for indent.  It scans off one token and puts
 * it in the global variable "token".  It returns a code, indicating the type
 * of token scanned.
 */

#include "indent_globs.h"
#include "ctype.h"

#define alphanum 1
#define opchar 3

enum rwcodes {
  rw_break,
  rw_switch,
  rw_case,
  rw_struct_like, /* struct, enum, union */
  rw_decl,
  rw_sp_paren, /* if, while, for */
  rw_sp_nparen, /* do, else */
  rw_sizeof
  };

struct templ {
    char       *rwd;
    enum rwcodes rwcode;
};

struct templ *user_specials = 0;
unsigned int user_specials_max, user_specials_idx;
struct templ specials[] =
{
    {"switch", rw_switch},
    {"case", rw_case},
    {"break", rw_break},
    {"struct", rw_struct_like},
    {"union", rw_struct_like},
    {"enum", rw_struct_like},
    {"default", rw_case},
    {"int", rw_decl},
    {"char", rw_decl},
    {"float", rw_decl},
    {"double", rw_decl},
/*    {"long", rw_decl},
    {"short", rw_decl},*/
    {"typdef", rw_decl},
    {"unsigned", rw_decl},
    {"register", rw_decl},
    {"static", rw_decl},
    {"global", rw_decl},
    {"extern", rw_decl},
    {"void", rw_decl},
    {"va_dcl", rw_decl},
    {"goto", rw_break},
    {"return", rw_break},
    {"if", rw_sp_paren},
    {"while", rw_sp_paren},
    {"for", rw_sp_paren},
    {"else", rw_sp_nparen},
    {"do", rw_sp_nparen},
    {"sizeof", rw_sizeof},
    {0, 0}
};

char        chartype[128] =
{				/* this is used to facilitate the decision of
				 * what type (alphanumeric, operator) each
				 * character is */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 3, 0, 0, 1, 3, 3, 0,
    0, 0, 3, 3, 0, 3, 0, 3,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 0, 0, 3, 3, 3, 3,
    0, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 0, 0, 0, 3, 1,
    0, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 0, 3, 0, 3, 0
};




enum codes
lexi()
{
    /* used to walk through the token */
    char *tok;

    int         unary_delim;	/* this is set to 1 if the current token
				 *
				 * forces a following operator to be unary */
    static enum codes last_code;	/* the last token type returned */
    static int  l_struct;	/* set to 1 if the last token was 'struct' */
    int         code;		/* internal code to be returned */
    char        qchar;		/* the delimiter character for a string */

    unary_delim = false;
    parser_state_tos->col_1 = parser_state_tos->last_nl;	/* tell world that this token started in
				 * column 1 iff the last thing scanned was nl */
    parser_state_tos->last_nl = false;

    while (*buf_ptr == ' ' || *buf_ptr == '\t') {	/* get rid of blanks */
	parser_state_tos->col_1 = false;	/* leading blanks imply token is not in column
				 * 1 */
	if (++buf_ptr >= buf_end)
	    fill_buffer();
    }

    token = buf_ptr;

    /* Scan an alphanumeric token */
    if (chartype[*buf_ptr] == alphanum || buf_ptr[0] == '.' && isdigit(buf_ptr[1])) {
	/*
	 * we have a character or number
	 */
	register char *j;	/* used for searching thru list of
				 *
				 * reserved words */
	register struct templ *p;

	if (isdigit(*buf_ptr) || buf_ptr[0] == '.' && isdigit(buf_ptr[1])) {
	    int         seendot = 0,
	                seenexp = 0;
	    if (*buf_ptr == '0' &&
		    (buf_ptr[1] == 'x' || buf_ptr[1] == 'X')) {
	        buf_ptr += 2;
		while (isxdigit(*buf_ptr))
		    buf_ptr++;
	    }
	    else
		while (1) {
		    if (*buf_ptr == '.')
			if (seendot)
			    break;
			else
			    seendot++;
		    buf_ptr++;
		    if (!isdigit(*buf_ptr) && *buf_ptr != '.')
			if ((*buf_ptr != 'E' && *buf_ptr != 'e') || seenexp)
			    break;
			else {
			    seenexp++;
			    seendot++;
			    buf_ptr++;
			    if (*buf_ptr == '+' || *buf_ptr == '-')
				buf_ptr++;
			}
		}
	    if (*buf_ptr == 'L' || *buf_ptr == 'l')
		buf_ptr++;
	}
	else
	    while (chartype[*buf_ptr] == alphanum) {	/* copy it over */
		buf_ptr++;
		if (buf_ptr >= buf_end)
		    fill_buffer();
	    }
	token_end = buf_ptr;
	while (*buf_ptr == ' ' || *buf_ptr == '\t') {	/* get rid of blanks */
	    if (++buf_ptr >= buf_end)
		fill_buffer();
	}
	parser_state_tos->its_a_keyword = false;
	parser_state_tos->sizeof_keyword = false;
	if (l_struct) {		/* if last token was 'struct', then this token
				 * should be treated as a declaration */
	    l_struct = false;
	    last_code = ident;
	    parser_state_tos->last_u_d = true;
	    return (decl);
	}
	parser_state_tos->last_u_d = false;	/* Operator after indentifier is binary */
	last_code = ident;	/* Remember that this is the code we will
				 * return */

	/*
	 * This loop will check if the token is a keyword.
	 */
	for (p = specials; (j = p->rwd) != 0; p++) {
	    tok = token;	/* point at scanned token */
	    if (*j++ != *tok++ || *j++ != *tok++)
		continue;	/* This test depends on the fact that
				 * identifiers are always at least 1 character
				 * long (ie. the first two bytes of the
				 * identifier are always meaningful) */
	    if (tok >= token_end)
		break;		/* If its a 1 or 2 character identifier */
	    while (tok < token_end && *tok++ == *j++)
		if (*j == 0 && tok == token_end)
		    goto found_keyword;	/* I wish that C had a multi-level
					 * break... */
	}
	if (p->rwd) {		/* we have a keyword */
    found_keyword:
	    parser_state_tos->its_a_keyword = true;
	    parser_state_tos->last_u_d = true;
	    switch (p->rwcode) {
	    case rw_switch:		/* it is a switch */
		return (swstmt);
	    case rw_case:		/* a case or default */
		return (casestmt);

	    case rw_struct_like:		/* a "struct" */
		if (parser_state_tos->p_l_follow)
		    break;	/* inside parens: cast */
		l_struct = true;

		/*
		 * Next time around, we will want to know that we have had a
		 * 'struct'
		 */
	    case rw_decl:		/* one of the declaration keywords */
		if (parser_state_tos->p_l_follow) {
		    parser_state_tos->cast_mask |= 1 << parser_state_tos->p_l_follow;
		    break;	/* inside parens: cast */
		}
		last_code = decl;
		return (decl);

	    case rw_sp_paren:		/* if, while, for */
		return (sp_paren);

	    case rw_sp_nparen:		/* do, else */
		return (sp_nparen);

	    case rw_sizeof:
		parser_state_tos->sizeof_keyword = true;
	    default:		/* all others are treated like any other
				 * identifier */
		return (ident);
	    }			/* end of switch */
	}			/* end of if (found_it) */
	if (*buf_ptr == '(' && parser_state_tos->tos <= 1 && parser_state_tos->ind_level == 0) {
	    register char *tp = buf_ptr;
	    while (tp < buf_end)
		if (*tp++ == ')' && *tp == ';')
		    goto not_proc;
	    parser_state_tos->procname = token;
	    parser_state_tos->procname_end = token_end;
	    parser_state_tos->in_parameter_declaration = 1;
    not_proc:;
	}
	/*
	 * The following hack attempts to guess whether or not the current
	 * token is in fact a declaration keyword -- one that has been
	 * typedefd
	 */
	if (((*buf_ptr == '*' && buf_ptr[1] != '=') || isalpha(*buf_ptr) || *buf_ptr == '_')
		&& !parser_state_tos->p_l_follow
	        && !parser_state_tos->block_init
		&& (parser_state_tos->last_token == rparen || parser_state_tos->last_token == semicolon ||
		    parser_state_tos->last_token == decl ||
		    parser_state_tos->last_token == lbrace || parser_state_tos->last_token == rbrace)) {
	    parser_state_tos->its_a_keyword = true;
	    parser_state_tos->last_u_d = true;
	    last_code = decl;
	    return decl;
	}
	if (last_code == decl)	/* if this is a declared variable, then
				 * following sign is unary */
	    parser_state_tos->last_u_d = true;	/* will make "int a -1" work */
	last_code = ident;
	return (ident);		/* the ident is not in the list */
    }				/* end of procesing for alpanum character */
    /* l l l Scan a non-alphanumeric token */

    /* If it is not a one character token, token_end will get changed
       later.  */
    token_end = buf_ptr + 1;

    if (++buf_ptr >= buf_end)
	fill_buffer();

    switch (*token) {
    case '\n':
	unary_delim = parser_state_tos->last_u_d;
	parser_state_tos->last_nl = true;	/* remember that we just had a newline */
	code = (had_eof ? 0 : newline);

	/*
	 * if data has been exausted, the newline is a dummy, and we should
	 * return code to stop
	 */
	break;

    case '\'':			/* start of quoted character */
    case '"':			/* start of string */
	qchar = *token;

	/* Find out how big the literal is so we can set token_end.  */
	
	/* Invariant:  before loop test buf_ptr points to the next */
	/* character that we have not yet checked. */
	while (*buf_ptr != qchar && *buf_ptr != 0 && *buf_ptr != '\n')
	  {
	    if (*buf_ptr == '\\')
	      {
		buf_ptr++;
		if (buf_ptr >= buf_end)
		  fill_buffer ();
		if (*buf_ptr == '\n')
		  ++line_no;
		if (*buf_ptr == 0)
		  break;
	      }
	    buf_ptr++;
	    if (buf_ptr >= buf_end)
	      fill_buffer ();
	  }
	if (*buf_ptr == '\n' || *buf_ptr == 0)
	  {
	    diag (1,
		  qchar == '\''
		    ? "Unterminated character constant"
		    : "Unterminated string constant"
		 );
	  }
	else
	  {
	    /* Advance over end quote char.  */
	    buf_ptr++;
	    if (buf_ptr >= buf_end)
	      fill_buffer ();
	  }

	code = ident;
	break;

    case ('('):
        if (lpc && *buf_ptr == '{') {
	    buf_ptr++;
	}
    case ('['):
	unary_delim = true;
	code = lparen;
	break;

    case (')'):
    case (']'):
	code = rparen;
	break;

    case '#':
	unary_delim = parser_state_tos->last_u_d;
	code = preesc;
	break;

    case '?':
	unary_delim = true;
	code = question;
	break;

    case (':'):
        if (lpc && *buf_ptr == ':') {
	    buf_ptr++;
	    code = unary_op;
	    unary_delim = true;
	    break;
	}
	code = colon;
	unary_delim = true;
	break;

    case (';'):
	unary_delim = true;
	code = semicolon;
	break;

    case ('{'):
	unary_delim = true;

	/* This check is made in the code for '='.  No one who writes
	   initializers without '=' these days deserves to have indent
	   work on their code (besides which, uncommenting this would
	   screw up anything which assumes that parser_state_tos->block_init really
	   means you are in an initializer.  */
	/*
	 * if (parser_state_tos->in_or_st) parser_state_tos->block_init = 1;
	 */

	/* The following neat hack causes the braces in structure
	   initializations to be treated as parentheses, thus causing
	   initializations to line up correctly, e.g.
	   struct foo bar =
	   {{a,
	     b,
	     c},
	    {1,
	     2}};
	   If lparen is returned, token can be used to distinguish
	   between '{' and '(' where necessary.  */

	code = parser_state_tos->block_init ? lparen : lbrace;
	break;

    case ('}'):
        if (lpc && *buf_ptr == ')') {
	    buf_ptr++;
	    code = rparen;
	    break;
	}
	unary_delim = true;
	/* The following neat hack is explained under '{' above.  */
	code = parser_state_tos->block_init ? rparen : rbrace;

	break;

    case 014:			/* a form feed */
	unary_delim = parser_state_tos->last_u_d;
	parser_state_tos->last_nl = true;	/* remember this so we can set 'parser_state_tos->col_1'
				 * right */
	code = form_feed;
	break;

    case (','):
	unary_delim = true;
	code = comma;
	break;

    case '.':
	unary_delim = false;
	code = period;
	break;

    case '-':
    case '+':			/* check for -, +, --, ++ */
	code = (parser_state_tos->last_u_d ? unary_op : binary_op);
	unary_delim = true;

	if (*buf_ptr == token[0]) {
	    /* check for doubled character */
	    buf_ptr++;
	    /* buffer overflow will be checked at end of loop */
	    if (last_code == ident || last_code == rparen) {
		code = (parser_state_tos->last_u_d ? unary_op : postop);
		/* check for following ++ or -- */
		unary_delim = false;
	    }
	}
	else if (*buf_ptr == '=')
	    /* check for operator += */
	    buf_ptr++;
	else if (*buf_ptr == '>') {
	    /* check for operator -> */
	    buf_ptr++;
	    if (!pointer_as_binop) {
		unary_delim = false;
		code = unary_op;
		parser_state_tos->want_blank = false;
	    }
	}
	break;			/* buffer overflow will be checked at end of
				 * switch */

    case '=':
	if (parser_state_tos->in_or_st)
	    parser_state_tos->block_init = 1;

	if (*buf_ptr == '=') /* == */
	    buf_ptr++;

	code = binary_op;
	unary_delim = true;
	break;
	/* can drop thru!!! */

    case '>':
    case '<':
    case '!':			/* ops like <, <<, <=, !=, etc */
	if (*buf_ptr == '>' || *buf_ptr == '<' || *buf_ptr == '=') {
	    if (++buf_ptr >= buf_end)
		fill_buffer();
	}

	code = (parser_state_tos->last_u_d ? unary_op : binary_op);
	unary_delim = true;
	break;

    default:
	if (token[0] == '/' && *buf_ptr == '*') {
	    /* it is start of comment */

	    if (++buf_ptr >= buf_end)
		fill_buffer();

	    code = comment;
	    unary_delim = parser_state_tos->last_u_d;
	    break;
	}
	while (*(buf_ptr - 1) == *buf_ptr || *buf_ptr == '=') {
	    /*
	     * handle ||, &&, etc, and also things as in int *****i
	     */
	    if (++buf_ptr >= buf_end)
		fill_buffer();
	}
	code = (parser_state_tos->last_u_d ? unary_op : binary_op);
	unary_delim = true;


    }				/* end of switch */
    if (code != newline) {
	l_struct = false;
	last_code = code;
    }
    token_end = buf_ptr;
    if (buf_ptr >= buf_end)	/* check for input buffer empty */
	fill_buffer();
    parser_state_tos->last_u_d = unary_delim;

    return (code);
}

/*
 * Add the given keyword to the keyword table, using val as the keyword type
 */
addkey(key, val)
    char       *key;
     enum rwcodes val;
{
    register struct templ *p = specials;
    while (p->rwd)
	if (p->rwd[0] == key[0] && strcmp(p->rwd, key) == 0)
	    return;
	else
	    p++;

    if (user_specials == 0)
      {
	user_specials = (struct templ *) xmalloc (5 * sizeof (struct templ));
	if (user_specials == 0)
	  {
	    fputs ("indent: out of memory\n", stderr);
	    exit (1);
	  }
	user_specials_max = 5;
	user_specials_idx = 0;
      }
    else if (user_specials_idx == user_specials_max)
      {
	user_specials_max += 5;
	user_specials = (struct templ *) xrealloc ((char *) user_specials,
						  user_specials_max
						  * sizeof (struct templ));
      }
    p = &user_specials[user_specials_idx++];

    p->rwd = key;
    p->rwcode = val;
    p[1].rwd = 0;
    p[1].rwcode = 0;
    return;
}
