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
char copyright[] =
"@(#) Copyright (c) 1985 Sun Microsystems, Inc.\n\
 @(#) Copyright (c) 1980 The Regents of the University of California.\n\
 @(#) Copyright (c) 1976 Board of Trustees of the University of Illinois.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)indent.c	5.11 (Berkeley) 9/15/88";
#endif /* not lint */

#define MAIN
#include "indent_globs.h"
#include <ctype.h>

char       *in_name = 0;	/* will always point to name of input
					 * file */
char       *out_name = 0;	/* will always point to name
						 * of output file */

/* The following variables are documented in indent_globs.h.  */
int else_or_endif = false;

main(argc, argv)
    int         argc;
    char      **argv;
{

    extern int  found_err;	/* flag set in diag() on error */
    int         dec_ind;	/* current indentation for declarations */
    int         di_stack[20];	/* a stack of structure indentation levels */
    int         flushed_nl;	/* used when buffering up comments to remember
				 * that a newline was passed over */
    int         force_nl;	/* when true, code must be broken */
    int         hd_type;	/* used to store type of stmt for if (...),
				 * for (...), etc */
    register int i;		/* local loop counter */
    int         scase;		/* set to true when we see a case, so we will
				 * know what to do with the following colon */
    int         sp_sw;		/* when true, we are in the expressin of
				 * if(...), while(...), etc. */

    /* True if we have just encountered the end of an if (...), etc.
       (i.e. the ')' of the if (...) was the last token).  The variable
       is set to 2 in the middle of the main token reading loop and is
       decremented at the beginning of the loop, so it will reach zero
       when the second token after the ')' is read.  */
    int last_token_ends_sp;

    int         squest;		/* when this is positive, we have seen a ?
				 * without the matching : in a <c>?<s>:<s>
				 * construct */
    register char *t_ptr;	/* used for copying tokens */
    enum codes type_code;	/* the type of token, returned by lexi */

    int         last_else = 0;	/* true iff last keyword was an else */


    /*-----------------------------------------------*\
    |		      INITIALIZATION		      |
    \*-----------------------------------------------*/

    parser_state_tos = (struct parser_state *)
      xmalloc(sizeof(struct parser_state));
    parser_state_tos->next = 0;
    /* Allocate initial stacks for the parser.  */
    parser_state_tos->p_stack_size = INITIAL_STACK_SIZE;
    parser_state_tos->p_stack = (enum codes *) xmalloc
      (parser_state_tos->p_stack_size * sizeof (enum codes));
    parser_state_tos->il = (int *) xmalloc
      (parser_state_tos->p_stack_size * sizeof (int));
    parser_state_tos->cstk = (int *) xmalloc
      (parser_state_tos->p_stack_size * sizeof (int));

    parser_state_tos->paren_indents_size = 1;
    parser_state_tos->paren_indents = (short *) xmalloc
      (parser_state_tos->paren_indents_size * sizeof (short));

    parser_state_tos->p_stack[0] = stmt;	/* this is the parser's stack */
    parser_state_tos->last_nl = true;		/* this is true if the last thing scanned was
				 * a newline */
    parser_state_tos->last_token = semicolon;
    combuf = (char *) xmalloc(bufsize);
    labbuf = (char *) xmalloc(bufsize);
    codebuf = (char *) xmalloc(bufsize);
    l_com = combuf + bufsize - 5;
    l_lab = labbuf + bufsize - 5;
    l_code = codebuf + bufsize - 5;
    combuf[0] = codebuf[0] = labbuf[0] = ' ';	/* set up code, label, and
						 * comment buffers */
    combuf[1] = codebuf[1] = labbuf[1] = '\0';
    else_if = 1;		/* Default else-if special processing to on */
    s_lab = e_lab = labbuf + 1;
    s_code = e_code = codebuf + 1;
    s_com = e_com = combuf + 1;

    init_buf(save_com);

    line_no = 1;
    had_eof = parser_state_tos->in_decl = parser_state_tos->decl_on_line = break_comma = false;
    sp_sw = force_nl = false;
    parser_state_tos->in_or_st = false;
    parser_state_tos->bl_line = true;
    dec_ind = 0;
    di_stack[parser_state_tos->dec_nest = 0] = 0;
    parser_state_tos->want_blank = parser_state_tos->in_stmt = parser_state_tos->ind_stmt = false;
    parser_state_tos->procname = parser_state_tos->procname_end = "\0";

    scase = parser_state_tos->pcase = false;
    squest = 0;
    bp_save = 0;
    be_save = 0;

    output = 0;



    /*--------------------------------------------------*\
    |   		COMMAND LINE SCAN		 |
    \*--------------------------------------------------*/

    for (i = 1; i < argc; ++i)
	if (strcmp(argv[i], "-npro") == 0)
	    break;
    set_defaults();
    if (i >= argc)
	set_profile();

    for (i = 1; i < argc; ++i) {

	/*
	 * look thru args (if any) for changes to defaults
	 */

	if (argv[i][0] != '-') {/* no flag on parameter */
	    if (in_name == 0) {	/* we must have the input file */
		in_name = argv[i];	/* remember name of input file */
		read_file(in_name);
		continue;
	    }
	    else if (out_name == 0) {	/* we have the output file */
		out_name = argv[i];	/* remember name of output file */
		continue;
	    }
	    fprintf(stderr, "indent: unknown parameter: %s\n", argv[i]);
	    exit(1);
	}
	else
	    set_option(argv[i],1);
    }				/* end of for */
    if (in_name == 0) {
	fprintf(stderr, "indent: usage: indent file [ outfile ] [ options ]\n");
	exit(1);
    }
    if (out_name)
      {
	if (strcmp(in_name, out_name) == 0) {	/* attempt to overwrite
						 * the file */
	  fprintf(stderr, "indent: input and output files must be different\n");
	  exit(1);
	}
	output = fopen(out_name, "w");
	if (output == 0) {	/* check for create error */
	  fprintf(stderr, "indent: can't create %s\n", argv[i]);
	  exit(1);
	}
      }
    if (output == 0)
      {
	if (troff)
	    output = stdout;
	else {
	    out_name = in_name;
	    bakcopy();
	}
      }
    if (com_ind <= 1)
	com_ind = 2;		/* dont put normal comments before column 2 */
    if (troff) {
	if (bodyf.font[0] == 0)
	    parsefont(&bodyf, "R");
	if (scomf.font[0] == 0)
	    parsefont(&scomf, "I");
	if (blkcomf.font[0] == 0)
	    blkcomf = scomf, blkcomf.size += 2;
	if (boxcomf.font[0] == 0)
	    boxcomf = blkcomf;
	if (stringf.font[0] == 0)
	    parsefont(&stringf, "L");
	if (keywordf.font[0] == 0)
	    parsefont(&keywordf, "B");
	writefdef(&bodyf, 'B');
	writefdef(&scomf, 'C');
	writefdef(&blkcomf, 'L');
	writefdef(&boxcomf, 'X');
	writefdef(&stringf, 'S');
	writefdef(&keywordf, 'K');
    }
    if (lpc) {
	addkey("string", 4);
	addkey("object", 4);
    } else {
	addkey("long", 4);
	addkey("short", 4);
    }
    if (block_comment_max_col <= 0)
	block_comment_max_col = max_col;
    if (decl_com_ind <= 0)	/* if not specified by user, set this */
	decl_com_ind =
	  ljust_decl ? (com_ind <= 10 ? 2 : com_ind - 8) : com_ind;
    if (continuation_indent == 0)
	continuation_indent = ind_size;
    fill_buffer();		/* get first batch of stuff into input buffer */

    parse(semicolon);
    {
	register char *p = buf_ptr;
	register    col = 1;

	while (1) {
	    if (*p == ' ')
		col++;
	    else if (*p == '\t')
		col = ((col - 1) & ~7) + 9;
	    else
		break;
	    p++;
	};
	if (col > ind_size)
	  parser_state_tos->ind_level = parser_state_tos->i_l_follow = col;
    }
    if (troff) {
	register char *p = in_name,
	           *beg = in_name;

	while (*p)
	    if (*p++ == '/')
		beg = p;
	fprintf(output, ".Fn \"%s\"\n", beg);
    }
    /*
     * START OF MAIN LOOP
     */

    while (1) {			/* this is the main loop.  it will go until we
				 * reach eof */
	int         is_procname;

	type_code = lexi();	/* lexi reads one token.  "token" points to
				   the actual characters. lexi
				   returns a code indicating the type of token */

	if (last_token_ends_sp > 0)
	  last_token_ends_sp--;
	is_procname = parser_state_tos->procname[0];

	/*
	 * The following code moves everything following an if (), while (),
	 * else, etc. up to the start of the following stmt to a buffer. This
	 * allows proper handling of both kinds of brace placement.
	 */

	flushed_nl = false;
	while (parser_state_tos->search_brace)
	  {
	    /* After scanning an if(), while (), etc., it might be
               necessary to keep track of the text between the if() and
	       the start of the statement which follows.  Use save_com
	       to do so.  */
	
	    switch (type_code) {
	    case newline:
		++line_no;
		flushed_nl = true;
	    case form_feed:
		break;		/* form feeds and newlines found here will be
				 * ignored */

	    case lbrace:	/* this is a brace that starts the compound
				 * stmt */
		if (save_com.end == save_com.ptr) { /* ignore buffering if a comment wasnt
					    stored up */
		    parser_state_tos->search_brace = false;
		    goto check_type;
		}
		if (btype_2) {
		    save_com.ptr[0] = '{';	/* we either want to put the brace
					 * right after the if */
		    goto sw_buffer;	/* go to common code to get out of
					 * this loop */
		}
	    case comment:	/* we have a comment, so we must copy it into
				 * the buffer */
		if (!flushed_nl || save_com.end != save_com.ptr)
		  {
		    need_chars(save_com,10);
		    if (save_com.end == save_com.ptr) {	/* if this is the first comment, we
					 * must set up the buffer */
			save_com.ptr[0] = save_com.ptr[1] = ' ';
			save_com.end = save_com.ptr + 2;
		    }
		    else {
			*save_com.end++ = '\n';	/* add newline between
						 * comments */
			*save_com.end++ = ' ';
			--line_no;
		    }
		    *save_com.end++ = '/';	/* copy in start of comment */
		    *save_com.end++ = '*';

		    for (;;) {	/* loop until we get to the end of the comment */
		        /* make sure there is room for this character and
			   (while we're at it) the '/' we might add
			   at the end of the loop. */
		        need_chars(save_com,2);
			*save_com.end = *buf_ptr++;
			if (buf_ptr >= buf_end)
			    fill_buffer();

			if (*save_com.end++ == '*' && *buf_ptr == '/')
			    break;	/* we are at end of comment */

		    }
		    *save_com.end++ = '/';	/* add ending slash */
		    if (++buf_ptr >= buf_end)	/* get past / in buffer */
			fill_buffer();
		    break;
		}
	    default:		/* it is the start of a normal statment */
		if (flushed_nl)	/* if we flushed a newline, make sure it is
				 * put back */
		    force_nl = true;
		if (type_code == sp_paren && *token == 'i'
			&& last_else && else_if
			|| type_code == sp_nparen && *token == 'e'
			&& e_code != s_code && e_code[-1] == '}')
		    force_nl = false;

		if (save_com.end == save_com.ptr) {	/* ignore buffering if comment wasnt
					 * saved up */
		    parser_state_tos->search_brace = false;
		    goto check_type;
		}
		if (force_nl) {	/* if we should insert a nl here, put it into
				 * the buffer */
		    force_nl = false;
		    --line_no;	/* this will be re-increased when the nl is
				 * read from the buffer */
		    need_chars(save_com,2);
		    *save_com.end++ = '\n';
		    *save_com.end++ = ' ';
		    if (verbose && !flushed_nl)	/* print error msg if the line
						 * was not already broken */
			diag(0, "Line broken");
		    flushed_nl = false;
		}
		for (t_ptr = token; t_ptr < token_end; ++t_ptr)
		  {
		    need_chars(save_com,1);
		    *save_com.end++ = *t_ptr;	/* copy token into temp buffer */
		  }
		parser_state_tos->procname = "\0";

	sw_buffer:
		parser_state_tos->search_brace = false;	/* stop looking for start of
						 * stmt */
		bp_save = buf_ptr;	/* save current input buffer */
		be_save = buf_end;
		buf_ptr = save_com.ptr;	/* fix so that subsequent calls to
					 * lexi will take tokens out of
					 * save_com */
		need_chars(save_com,1);
		*save_com.end++ = ' ';/* add trailing blank, just in case */
		buf_end = save_com.end;
		save_com.end = save_com.ptr; /* make save_com empty */
		break;
	    }			/* end of switch */
	    if (type_code != 0)	/* we must make this check, just in case there
				 * was an unexpected EOF */
		type_code = lexi();	/* read another token */
	    /* if (parser_state_tos->search_brace) parser_state_tos->procname[0] = 0; */
	    if ((is_procname = parser_state_tos->procname[0]) && flushed_nl
		    && !procnames_start_line && parser_state_tos->in_decl
		    && type_code == ident)
		flushed_nl = 0;
	}			/* end of while (search_brace) */
	last_else = 0;
check_type:
	if (type_code == 0) {	/* we got eof */
	    if (s_lab != e_lab || s_code != e_code
		    || s_com != e_com)	/* must dump end of line */
		dump_line();
	    if (parser_state_tos->tos > 1)	/* check for balanced braces */
		diag(1, "Stuff missing from end of file.");

	    if (verbose) {
		printf("There were %d output lines and %d comments\n",
		       parser_state_tos->out_lines, parser_state_tos->out_coms);
		printf("(Lines with comments)/(Lines with code): %6.3f\n",
		       (1.0 * parser_state_tos->com_lines) / code_lines);
	    }
	    fflush(output);
	    exit(found_err);
	}
	if (
		(type_code != comment) &&
		(type_code != newline) &&
		(type_code != preesc) &&
		(type_code != form_feed)) {
	    if (force_nl &&
		    (type_code != semicolon) &&
		    (type_code != lbrace || !btype_2)) {
		/* we should force a broken line here */
		if (verbose && !flushed_nl)
		    diag(0, "Line broken");
		flushed_nl = false;
		dump_line();
		parser_state_tos->want_blank = false;	/* dont insert blank at line start */
		force_nl = false;
	    }
	    parser_state_tos->in_stmt = true;	/* turn on flag which causes an extra level of
				 * indentation. this is turned off by a ; or
				 * '}' */
	    if (s_com != e_com) {	/* the turkey has embedded a comment
					 * in a line. fix it */
		*e_code++ = ' ';
		for (t_ptr = s_com; *t_ptr; ++t_ptr) {
		    check_size(code);
		    *e_code++ = *t_ptr;
		}
		*e_code++ = ' ';
		*e_code = '\0';	/* null terminate code sect */
		parser_state_tos->want_blank = false;
		e_com = s_com;
	    }
	}
	else if (type_code != comment)	/* preserve force_nl thru a comment */
	    force_nl = false;	/* cancel forced newline after newline, form
				 * feed, etc */



	/*-----------------------------------------------------*\
	|	   do switch on type of token scanned		|
	\*-----------------------------------------------------*/
	check_size(code);
	switch (type_code) {	/* now, decide what to do with the token */

	case form_feed:	/* found a form feed in line */
	    parser_state_tos->use_ff = true;	/* a form feed is treated much like a newline */
	    dump_line();
	    parser_state_tos->want_blank = false;
	    break;

	case newline:
	    if (parser_state_tos->last_token != comma || parser_state_tos->p_l_follow > 0
		    || !leave_comma || parser_state_tos->block_init || !break_comma || s_com != e_com) {
		dump_line();
		parser_state_tos->want_blank = false;
	    }
	    ++line_no;		/* keep track of input line number */
	    break;

	case lparen:
	    /* Count parens so we know how deep we are.  */
	    if (++parser_state_tos->p_l_follow
		>= parser_state_tos->paren_indents_size)
	      {
		parser_state_tos->paren_indents_size *= 2;
		parser_state_tos->paren_indents = (short *)
		  xrealloc (parser_state_tos->paren_indents,
			    parser_state_tos->paren_indents_size
			      * sizeof (short));
	      }
	    if (parser_state_tos->want_blank && *token != '[' &&
		    (parser_state_tos->last_token != ident || proc_calls_space
	      || (parser_state_tos->its_a_keyword && (!parser_state_tos->sizeof_keyword || Bill_Shannon))))
		*e_code++ = ' ';
	    if (parser_state_tos->in_decl && !parser_state_tos->block_init)
		if (troff && !parser_state_tos->dumped_decl_indent && !is_procname && parser_state_tos->last_token == decl) {
		    parser_state_tos->dumped_decl_indent = 1;
		    sprintf (e_code, "\n.Du %dp+\200p \"%.*s\"\n", dec_ind * 7,
			     token_end - token, token);
		    e_code += strlen(e_code);
		}
		else {
		    while ((e_code - s_code) < dec_ind) {
			check_size(code);
			*e_code++ = ' ';
		    }
		    if (token_end > token+1) {
			*e_code++ = token[0];
			*e_code++ = token[1];
		    } else
			*e_code++ = token[0];
		}
	    else {
		if (token_end > token+1) {
		    *e_code++ = token[0];
		    *e_code++ = token[1];
		} else
		    *e_code++ = token[0];
	    }
	    parser_state_tos->paren_indents[parser_state_tos->p_l_follow - 1] = e_code - s_code;
	    if (sp_sw && parser_state_tos->p_l_follow == 1 && extra_expression_indent
		    && parser_state_tos->paren_indents[0] < 2 * ind_size)
		parser_state_tos->paren_indents[0] = 2 * ind_size;
	    parser_state_tos->want_blank = false;
	    if (parser_state_tos->in_or_st && *token == '(' && parser_state_tos->tos <= 2) {
		/*
		 * this is a kluge to make sure that declarations will be
		 * aligned right if proc decl has an explicit type on it, i.e.
		 * "int a(x) {..."
		 */
		parse(semicolon);	/* I said this was a kluge... */
		parser_state_tos->in_or_st = false;	/* turn off flag for structure decl or
					 * initialization */
	    }
	    if (parser_state_tos->sizeof_keyword)
		parser_state_tos->sizeof_mask |= 1 << parser_state_tos->p_l_follow;
	    break;

	case rparen:
	    if (parser_state_tos->cast_mask & (1 << parser_state_tos->p_l_follow) & ~parser_state_tos->sizeof_mask) {
		parser_state_tos->last_u_d = true;
		parser_state_tos->cast_mask &= (1 << parser_state_tos->p_l_follow) - 1;
	    }
	    parser_state_tos->sizeof_mask &= (1 << parser_state_tos->p_l_follow) - 1;
	    if (--parser_state_tos->p_l_follow < 0) {
		parser_state_tos->p_l_follow = 0;
		diag(0, "Extra %c", *token);
	    }
	    if (e_code == s_code)	/* if the paren starts the line */
	      {
		parser_state_tos->paren_level = parser_state_tos->p_l_follow;	/* then indent it */
		paren_target = -parser_state_tos->paren_indents[parser_state_tos->paren_level - 1];
	      }
	    *e_code++ = token[0];
	    if (token_end > token+1)
		*e_code++ = token[1];
	    parser_state_tos->want_blank = true;

	    if (sp_sw && (parser_state_tos->p_l_follow == 0)) {	/* check for end of if
							 * (...), or some such */
	        last_token_ends_sp = 2;
		sp_sw = false;
		force_nl = true;/* must force newline after if */
		parser_state_tos->last_u_d = true;	/* inform lexi that a following
					 * operator is unary */
		parser_state_tos->in_stmt = false;	/* dont use stmt continuation
					 * indentation */

		parse(hd_type);	/* let parser worry about if, or whatever */
	    }
	    parser_state_tos->search_brace = btype_2;	/* this should insure that constructs
					 * such as main(){...} and int[]{...}
					 * have their braces put in the right
					 * place */
	    break;

	case unary_op:		/* this could be any unary operation */
	    if (parser_state_tos->want_blank)
		*e_code++ = ' ';

	    if (troff && !parser_state_tos->dumped_decl_indent && parser_state_tos->in_decl && !is_procname) {
		sprintf(e_code, "\n.Du %dp+\200p \"%.*s\"\n", dec_ind * 7,
			token_end - token, token);
		parser_state_tos->dumped_decl_indent = 1;
		e_code += strlen(e_code);
	    }
	    else {
		char       *res = token;
		char *res_end = token_end;

		if (parser_state_tos->in_decl && !parser_state_tos->block_init) {	/* if this is a unary op
							 * in a declaration, we
							 * should indent this
							 * token */
		    while ((e_code - s_code) < (dec_ind - (token_end-token))) {
			check_size(code);
			*e_code++ = ' ';	/* pad it */
		    }
		}
		if (troff && token[0] == '-' && token[1] == '>')
		  {
		    static char resval[] = "\\(->";
		    res = resval;
		    res_end = res + sizeof(resval);
		  }
		for (t_ptr = res; t_ptr < res_end; ++t_ptr) {
		    check_size(code);
		    *e_code++ = *t_ptr;
		}
	    }
	    parser_state_tos->want_blank = false;
	    break;

	case binary_op:	/* any binary operation */
    do_binary:
	    if (parser_state_tos->want_blank)
		*e_code++ = ' ';
	    {
		char       *res = token;
		char *res_end = token_end;
#define set_res(str) \
	      {\
		static char resval[] = str;\
		res = resval;\
		res_end = res + sizeof(resval);\
	      }

		if (troff)
		    switch (token[0]) {
		    case '<':
			if (token[1] == '=')
			  set_res ("\\(<=");
			break;
		    case '>':
			if (token[1] == '=')
			    set_res ("\\(>=");
			break;
		    case '!':
			if (token[1] == '=')
			    set_res ("\\(!=");
			break;
		    case '|':
			if (token[1] == '|')
			  {
			    set_res ("\\(br\\(br");
			  }
			else if (token[1] == 0)
			    set_res ("\\(br");
			break;
		    }
		for (t_ptr = res; t_ptr < res_end; ++t_ptr) {
		    check_size(code);
		    *e_code++ = *t_ptr;	/* move the operator */
		}
	    }
	    parser_state_tos->want_blank = true;
	    break;

	case postop:		/* got a trailing ++ or -- */
	    *e_code++ = token[0];
	    *e_code++ = token[1];
	    parser_state_tos->want_blank = true;
	    break;

	case question:		/* got a ? */
	    squest++;		/* this will be used when a later colon
				 * appears so we can distinguish the
				 * <c>?<n>:<n> construct */
	    if (parser_state_tos->want_blank)
		*e_code++ = ' ';
	    *e_code++ = '?';
	    parser_state_tos->want_blank = true;
	    break;

	case casestmt:		/* got word 'case' or 'default' */
	    scase = true;	/* so we can process the later colon properly */
	    goto copy_id;

	case colon:		/* got a ':' */
	    if (squest > 0) {	/* it is part of the <c>?<n>: <n> construct */
		--squest;
		if (parser_state_tos->want_blank)
		    *e_code++ = ' ';
		*e_code++ = ':';
		parser_state_tos->want_blank = true;
		break;
	    }
	    if (parser_state_tos->in_decl) {
		*e_code++ = ':';
		parser_state_tos->want_blank = false;
		break;
	    }
	    parser_state_tos->in_stmt = false;	/* seeing a label does not imply we are in a
				 * stmt */
	    for (t_ptr = s_code; *t_ptr; ++t_ptr)
		*e_lab++ = *t_ptr;	/* turn everything so far into a label */
	    e_code = s_code;
	    *e_lab++ = ':';
	    *e_lab++ = ' ';
	    *e_lab = '\0';

	    force_nl = parser_state_tos->pcase = scase;	/* parser_state_tos->pcase will be used by
						 * dump_line to decide how to
						 * indent the label. force_nl
						 * will force a case n: to be
						 * on a line by itself */
	    scase = false;
	    parser_state_tos->want_blank = false;
	    break;

	case semicolon:	/* got a ';' */
	    parser_state_tos->in_or_st = false;/* we are not in an initialization or
				 * structure declaration */
	    scase = false;	/* these will only need resetting in a error */
	    squest = 0;
	    /* The following code doesn't seem to do much good.
	       Just because we've found something like
	       extern int foo();    or
	       int (*foo)();
	       doesn't mean we are out of a declaration.  Now if it was
	       serving some purpose we'll have to address that....
	    if (parser_state_tos->last_token == rparen)
		parser_state_tos->in_parameter_declaration = 0;
	    */
	    parser_state_tos->cast_mask = 0;
	    parser_state_tos->sizeof_mask = 0;
	    parser_state_tos->block_init = 0;
	    parser_state_tos->block_init_level = 0;
	    parser_state_tos->just_saw_decl--;

	    if (parser_state_tos->in_decl && s_code == e_code && !parser_state_tos->block_init)
		while ((e_code - s_code) < (dec_ind - 1)) {
		    check_size(code);
		    *e_code++ = ' ';
		}

	    parser_state_tos->in_decl = (parser_state_tos->dec_nest > 0);	/* if we were in a first level
						 * structure declaration, we
						 * arent any more */

	    if ((!sp_sw || hd_type != forstmt) && parser_state_tos->p_l_follow > 0) {

		/*
		 * This should be true iff there were unbalanced parens in the
		 * stmt.  It is a bit complicated, because the semicolon might
		 * be in a for stmt
		 */
		diag(1, "Unbalanced parens (1)");
		parser_state_tos->p_l_follow = 0;
		if (sp_sw) {	/* this is a check for a if, while, etc. with
				 * unbalanced parens */
		    sp_sw = false;
		    parse(hd_type);	/* dont lose the if, or whatever */
		}
	    }

	    /* If we have a semicolon following an if, while, or for,
	       and the user wants us to, we should insert a space
	       (to show that there is a null statement there).  */
	    if (last_token_ends_sp && space_sp_semicolon)
	      {
		*e_code++ = ' ';
	      }
	    *e_code++ = ';';
	    parser_state_tos->want_blank = true;
	    parser_state_tos->in_stmt = (parser_state_tos->p_l_follow > 0);	/* we are no longer in the
						 * middle of a stmt */

	    if (!sp_sw) {	/* if not if for (;;) */
		parse(semicolon);	/* let parser know about end of stmt */
		force_nl = true;/* force newline after a end of stmt */
	    }
	    break;

	case lbrace:		/* got a '{' */
	    parser_state_tos->in_stmt = false;	/* dont indent the {} */
	    if (!parser_state_tos->block_init)
		force_nl = true;/* force other stuff on same line as '{' onto
				 * new line */
	    else if (parser_state_tos->block_init_level <= 0)
		parser_state_tos->block_init_level = 1;
	    else
		parser_state_tos->block_init_level++;

	    if (s_code != e_code && !parser_state_tos->block_init) {
		if (!btype_2) {
		    dump_line();
		    parser_state_tos->want_blank = false;
		}
		else if (parser_state_tos->in_parameter_declaration && !parser_state_tos->in_or_st) {
		    parser_state_tos->i_l_follow = 0;
		    dump_line();
		    parser_state_tos->want_blank = false;
		}
	    }
	    if (parser_state_tos->in_parameter_declaration)
		prefix_blankline_requested = 0;

	    if (parser_state_tos->p_l_follow > 0) {	/* check for preceeding unbalanced
					 * parens */
		diag(1, "Unbalanced parens (2)");
		parser_state_tos->p_l_follow = 0;
		if (sp_sw) {	/* check for unclosed if, for, etc. */
		    sp_sw = false;
		    parse(hd_type);
		    parser_state_tos->ind_level = parser_state_tos->i_l_follow;
		}
	    }
	    if (s_code == e_code)
		parser_state_tos->ind_stmt = false;	/* dont put extra indentation on line
					 * with '{' */
	    if (parser_state_tos->in_decl && parser_state_tos->in_or_st) {	/* this is either a structure
						 * declaration or an init */
		di_stack[parser_state_tos->dec_nest++] = dec_ind;
		/* ?		dec_ind = 0; */
	    }
	    else {
		parser_state_tos->decl_on_line = false;	/* we cant be in the middle of
						 * a declaration, so dont do
						 * special indentation of
						 * comments */
		if (blanklines_after_declarations_at_proctop
			&& parser_state_tos->in_parameter_declaration)
		    postfix_blankline_requested = 1;
		parser_state_tos->in_parameter_declaration = 0;
	    }
	    dec_ind = 0;
	    parse(lbrace);	/* let parser know about this */
	    if (!(lpc && e_code[-1] == '('))
	      if (parser_state_tos->want_blank)	/* put a blank before '{' if '{' is not at
				 * start of line */
		*e_code++ = ' ';
	    parser_state_tos->want_blank = false;
	    *e_code++ = '{';
	    parser_state_tos->just_saw_decl = 0;
	    break;

	case rbrace:		/* got a '}' */
	    if (parser_state_tos->p_stack[parser_state_tos->tos] == decl && !parser_state_tos->block_init)	/* semicolons can be
								 * omitted in
								 * declarations */
		parse(semicolon);
	    if (parser_state_tos->p_l_follow) {/* check for unclosed if, for, else. */
		diag(1, "Unbalanced parens (3)");
		parser_state_tos->p_l_follow = 0;
		sp_sw = false;
	    }
	    parser_state_tos->just_saw_decl = 0;
	    parser_state_tos->block_init_level--;
	    if (s_code != e_code && !parser_state_tos->block_init) {	/* '}' must be first on
							 * line */
		if (verbose)
		    diag(0, "Line broken");
		dump_line();
	    }
	    *e_code++ = '}';
	    parser_state_tos->want_blank = true;
	    parser_state_tos->in_stmt = parser_state_tos->ind_stmt = false;
	    if (parser_state_tos->dec_nest > 0) {	/* we are in multi-level structure
					 * declaration */
		dec_ind = di_stack[--parser_state_tos->dec_nest];
		if (parser_state_tos->dec_nest == 0 && !parser_state_tos->in_parameter_declaration)
		    parser_state_tos->just_saw_decl = 2;
		parser_state_tos->in_decl = true;
	    }
	    prefix_blankline_requested = 0;
	    parse(rbrace);	/* let parser know about this */
	    parser_state_tos->search_brace = cuddle_else && parser_state_tos->p_stack[parser_state_tos->tos] == ifhead
		&& parser_state_tos->il[parser_state_tos->tos] >= parser_state_tos->ind_level;
	    if (parser_state_tos->tos <= 1 && blanklines_after_procs && parser_state_tos->dec_nest <= 0)
		postfix_blankline_requested = 1;
	    break;

	case swstmt:		/* got keyword "switch" */
	    sp_sw = true;
	    hd_type = swstmt;	/* keep this for when we have seen the
				 * expression */
	    goto copy_id;	/* go move the token into buffer */

	case sp_paren:		/* token is if, while, for */
	    sp_sw = true;	/* the interesting stuff is done after the
				 * expression is scanned */
	    hd_type = (*token == 'i' ? ifstmt :
		       (*token == 'w' ? whilestmt : forstmt));

	    /*
	     * remember the type of header for later use by parser
	     */
	    goto copy_id;	/* copy the token into line */

	case sp_nparen:	/* got else, do */
	    parser_state_tos->in_stmt = false;
	    if (*token == 'e') {
		if (e_code != s_code && (!cuddle_else || e_code[-1] != '}')) {
		    if (verbose)
			diag(0, "Line broken");
		    dump_line();/* make sure this starts a line */
		    parser_state_tos->want_blank = false;
		}
		force_nl = true;/* also, following stuff must go onto new line */
		last_else = 1;
		parse(elselit);
	    }
	    else {
		if (e_code != s_code) {	/* make sure this starts a line */
		    if (verbose)
			diag(0, "Line broken");
		    dump_line();
		    parser_state_tos->want_blank = false;
		}
		force_nl = true;/* also, following stuff must go onto new line */
		last_else = 0;
		parse(dolit);
	    }
	    goto copy_id;	/* move the token into line */

	case decl:		/* we have a declaration type (int, register,
				 * etc.) */
	    parse(decl);	/* let parser worry about indentation */
	    if (parser_state_tos->last_token == rparen && parser_state_tos->tos <= 1) {
		parser_state_tos->in_parameter_declaration = 1;
		if (s_code != e_code) {
		    dump_line();
		    parser_state_tos->want_blank = 0;
		}
	    }
	    if (parser_state_tos->in_parameter_declaration && indent_parameters && parser_state_tos->dec_nest == 0) {
		parser_state_tos->ind_level = parser_state_tos->i_l_follow = indent_parameters;
		parser_state_tos->ind_stmt = 0;
	    }
	    parser_state_tos->in_or_st = true;	/* this might be a structure or initialization
				 * declaration */
	    parser_state_tos->in_decl = parser_state_tos->decl_on_line = true;
	    if ( /* !parser_state_tos->in_or_st && */ parser_state_tos->dec_nest <= 0)
		parser_state_tos->just_saw_decl = 2;
	    prefix_blankline_requested = 0;
	    i = token_end - token + 1;	/* get length of token plus 1 */

	    /*
	     * dec_ind = e_code - s_code + (parser_state_tos->decl_indent>i ? parser_state_tos->decl_indent
	     * : i);
	     */
	    dec_ind = decl_indent > 0 ? decl_indent : i;
	    goto copy_id;

	case ident:		/* got an identifier or constant */
	    if (parser_state_tos->in_decl) {	/* if we are in a declaration, we must indent
				 * identifier */
		if (parser_state_tos->want_blank)
		    *e_code++ = ' ';
		parser_state_tos->want_blank = false;
		if (is_procname == 0 || !procnames_start_line) {
		    if (!parser_state_tos->block_init)
			if (troff && !parser_state_tos->dumped_decl_indent) {
			    sprintf(e_code, "\n.De %dp+\200p\n", dec_ind * 7);
			    parser_state_tos->dumped_decl_indent = 1;
			    e_code += strlen(e_code);
			}
			else
			    while ((e_code - s_code) < dec_ind) {
				check_size(code);
				*e_code++ = ' ';
			    }
		}
		else {
		    if (dec_ind && s_code != e_code)
			dump_line();
		    dec_ind = 0;
		    parser_state_tos->want_blank = false;
		}
	    }
	    else if (sp_sw && parser_state_tos->p_l_follow == 0) {
		sp_sw = false;
		force_nl = true;
		parser_state_tos->last_u_d = true;
		parser_state_tos->in_stmt = false;
		parse(hd_type);
	    }
    copy_id:
	    if (parser_state_tos->want_blank)
		*e_code++ = ' ';
	    if (troff && parser_state_tos->its_a_keyword) {
		e_code = chfont(&bodyf, &keywordf, e_code);
		for (t_ptr = token; t_ptr < token_end; ++t_ptr) {
		    check_size(code);
		    *e_code++ = keywordf.allcaps && islower(*t_ptr)
			? toupper(*t_ptr) : *t_ptr;
		}
		e_code = chfont(&keywordf, &bodyf, e_code);
	    }
	    else
	      {
		/* Troff mode requires that strings be processed specially.  */
		if (troff && (*token == '"' || *token == '\''))
		  {
		    char qchar;

		    qchar = *token;
		    *e_code++ = '`';
		    if (qchar == '"')
		      *e_code++ = '`';
		    e_code = chfont(&bodyf, &stringf, e_code);

		    t_ptr = token + 1;
		    while (t_ptr < token_end)
		      {
			*e_code = *t_ptr++;
			if (*e_code == '\\')
			  {
			    *++e_code = '\\';
			    if (*t_ptr == '\\')
			      *++e_code = '\\';
			    /* Copy char after backslash.  */
			    *++e_code = *t_ptr++;
			    /* Point after the last char we copied.  */
			    e_code++;
			  }
		      }
		    e_code = chfont(&stringf, &bodyf, e_code - 1);
		    if (qchar == '"')
		      *e_code++ = '\'';
		  }
		else
		  for (t_ptr = token; t_ptr < token_end; ++t_ptr)
		    {
		      check_size(code);
		      *e_code++ = *t_ptr;
		    }
		}
	    parser_state_tos->want_blank = true;

	    /* If the token is va_dcl, it appears without a semicolon,
	       so we need to pretend that one was there.  */
	    if ((token_end - token) == 6
		&& strncmp(token,"va_dcl",6) == 0)
	      {
		parser_state_tos->in_or_st = false;
		parser_state_tos->just_saw_decl--;
		parser_state_tos->in_decl = 0;
		parse(semicolon);
		force_nl = true;
	      }
	    break;

	case period:		/* treat a period kind of like a binary
				 * operation */
	    *e_code++ = '.';	/* move the period into line */
	    parser_state_tos->want_blank = false;	/* dont put a blank after a period */
	    break;

	case comma:
	    parser_state_tos->want_blank = (s_code != e_code);	/* only put blank after comma
						 * if comma does not start the
						 * line */
	    if (parser_state_tos->in_decl && is_procname == 0 && !parser_state_tos->block_init)
		while ((e_code - s_code) < (dec_ind - 1)) {
		    check_size(code);
		    *e_code++ = ' ';
		}

	    *e_code++ = ',';
	    if (parser_state_tos->p_l_follow == 0) {
		if (parser_state_tos->block_init_level <= 0)
		    parser_state_tos->block_init = 0;
		if (break_comma && !leave_comma)
		    force_nl = true;
	    }
	    break;

	case preesc:		/* got the character '#' */
	    if ((s_com != e_com) ||
		    (s_lab != e_lab) ||
		    (s_code != e_code))
		dump_line();
	    *e_lab++ = '#';	/* move whole line to 'label' buffer */
	    {
		int         in_comment = 0;
		int         com_start = 0;
		char        quote = 0;
		int         com_end = 0;

		while (*buf_ptr != '\n' || in_comment) {
		    check_size(lab);
		    *e_lab = *buf_ptr++;
		    if (buf_ptr >= buf_end)
			fill_buffer();
		    switch (*e_lab++) {
		    case BACKSLASH:
			if (troff)
			    *e_lab++ = BACKSLASH;
			if (!in_comment) {
			    *e_lab++ = *buf_ptr++;
			    if (buf_ptr >= buf_end)
				fill_buffer();
			}
			break;
		    case '/':
			if (*buf_ptr == '*' && !in_comment && !quote) {
			    in_comment = 1;
			    *e_lab++ = *buf_ptr++;
			    com_start = e_lab - s_lab - 2;
			}
			break;
		    case '"':
			if (quote == '"')
			    quote = 0;
			break;
		    case '\'':
			if (quote == '\'')
			    quote = 0;
			break;
		    case '*':
			if (*buf_ptr == '/' && in_comment) {
			    in_comment = 0;
			    *e_lab++ = *buf_ptr++;
			    com_end = e_lab - s_lab;
			}
			break;
		    }
		}

		while (e_lab > s_lab && (e_lab[-1] == ' ' || e_lab[-1] == '\t'))
		    e_lab--;
		if (e_lab - s_lab == com_end && bp_save == 0) {	/* comment on
								 * preprocessor line */
		    if (save_com.end != save_com.ptr)
		      {
		        need_chars(save_com,2);
			*save_com.end++ = '\n';	/* add newline between
						 * comments */
			*save_com.end++ = ' ';
			--line_no;
		      }
		    need_chars(save_com,com_end - com_start);
		    strncpy (save_com.end, s_lab + com_start,
			     com_end - com_start);
		    save_com.end += com_end - com_start;

		    e_lab = s_lab + com_start;
		    while (e_lab > s_lab && (e_lab[-1] == ' ' || e_lab[-1] == '\t'))
			e_lab--;
		    bp_save = buf_ptr;	/* save current input buffer */
		    be_save = buf_end;
		    buf_ptr = save_com.ptr;	/* fix so that subsequent calls to
					 * lexi will take tokens out of
					 * save_com */
		    need_chars(save_com,1);
		    *save_com.end++ = ' ';	/* add trailing blank, just in case */
		    buf_end = save_com.end;
		    save_com.end = save_com.ptr; /* make save_com empty */
		}
		*e_lab = '\0';	/* null terminate line */
		parser_state_tos->pcase = false;
	    }

	    if (strncmp(s_lab, "#if", 3) == 0) {
		if (blanklines_around_conditional_compilation) {
		    register    c;
		    prefix_blankline_requested++;
		    while ((c = *in_prog_pos++) == '\n');
		    in_prog_pos--;
		}
		{
		  /* Push a copy of the parser_state onto the stack.
		     All manipulations will use the copy at the top of stack,
		     and then we
		     can return to the previous state by popping the
		     stack.  */
		  struct parser_state *new;

		  new = (struct parser_state *)
		    xmalloc(sizeof(struct parser_state));
		  memcpy (new, parser_state_tos, sizeof(struct parser_state));

		  /* We need to copy the dynamically allocated arrays in
		     the struct parser_state too.  */
		  new->p_stack = (enum codes *)
		    xmalloc (parser_state_tos->p_stack_size
			     * sizeof (enum codes));
		  memcpy (new->p_stack, parser_state_tos->p_stack,
			  parser_state_tos->p_stack_size * sizeof (enum codes));

		  new->il = (int *)
		    xmalloc (parser_state_tos->p_stack_size * sizeof (int));
		  memcpy (new->il, parser_state_tos->il,
			  parser_state_tos->p_stack_size * sizeof (int));

		  new->cstk = (int *)
		    xmalloc (parser_state_tos->p_stack_size
			     * sizeof (int));
		  memcpy (new->cstk, parser_state_tos->cstk,
			  parser_state_tos->p_stack_size * sizeof (int));

		  new->paren_indents = (short *)xmalloc
		    (parser_state_tos->paren_indents_size * sizeof (short));
		  memcpy (new->paren_indents, parser_state_tos->paren_indents,
			  parser_state_tos->paren_indents_size * sizeof (short));
		
		  new->next = parser_state_tos;
		  parser_state_tos = new;
		}
	    }
	    else if (strncmp(s_lab, "#else", 5) == 0)
	      {
		/* When we get #else, we want to restore the parser
		   state to what it was before the matching #if, so
		   that things get lined up with the code before the
		   #if.  However, we do not want to pop the stack; we
		   just want to copy the second to top elt of the
		   stack because when we encounter the #endif, it will
		   pop the stack.  */
		else_or_endif = true;
		if (parser_state_tos->next)
		  {
		    /* First save the addresses of the arrays
		       for the top of stack.  */
		    enum codes *tos_p_stack = parser_state_tos->p_stack;
		    int *tos_il = parser_state_tos->il;
		    int *tos_cstk = parser_state_tos->cstk;
		    short *tos_paren_indents =
		      parser_state_tos->paren_indents;
		    struct parser_state *second =
		      parser_state_tos->next;

		    memcpy (parser_state_tos, second,
			    sizeof(struct parser_state));
		    parser_state_tos->next = second;
		
		    /* Now copy the arrays from the second to top of
		       stack to the top of stack.  */
		    /* Since the p_stack, etc. arrays only grow, never
		       shrink, we know that they will be big enough to
		       fit the array from the second to top of stack.  */
		    parser_state_tos->p_stack = tos_p_stack;
		    memcpy (parser_state_tos->p_stack,
			    parser_state_tos->next->p_stack,
			    parser_state_tos->p_stack_size
			      * sizeof (enum codes));

		    parser_state_tos->il = tos_il;
		    memcpy (parser_state_tos->il,
			    parser_state_tos->next->il,
			    parser_state_tos->p_stack_size * sizeof (int));

		    parser_state_tos->cstk = tos_cstk;
		    memcpy (parser_state_tos->cstk,
			    parser_state_tos->next->cstk,
			    parser_state_tos->p_stack_size * sizeof (int));

		    parser_state_tos->paren_indents = tos_paren_indents;
		    memcpy (parser_state_tos->paren_indents,
			    parser_state_tos->next->paren_indents,
			    parser_state_tos->paren_indents_size
			      * sizeof (short));
		  }
		else
		    diag(1, "Unmatched #else");
	      }
	    else if (strncmp(s_lab, "#endif", 6) == 0)
	      {
		else_or_endif = true;
		/* We want to remove the second to top elt on the
		   stack, which was put there by #if and was used to
		   restore the stack at the #else (if there was one).
		   We want to leave the top of stack
		   unmolested so that the state which we have been
		   using is unchanged.  */
		if (parser_state_tos->next)
		  {
		    struct parser_state *second = parser_state_tos->next;

		    parser_state_tos->next = second->next;
		    free (second->p_stack);
		    free (second->il);
		    free (second->cstk);
		    free (second->paren_indents);
		    free (second);
		  }
		else
		    diag(1, "Unmatched #endif");
		if (blanklines_around_conditional_compilation) {
		    postfix_blankline_requested++;
		    n_real_blanklines = 0;
		}
	      }
	    break;		/* subsequent processing of the newline
				 * character will cause the line to be printed */

	case comment:		/* we have gotten a '/ *'  this is a biggie */
    proc_comment:
	    if (flushed_nl) {	/* we should force a broken line here */
		flushed_nl = false;
		dump_line();
		parser_state_tos->want_blank = false;	/* dont insert blank at line start */
		force_nl = false;
	    }
	    pr_comment();
	    break;
	}			/* end of big switch stmt */

	*e_code = '\0';		/* make sure code section is null terminated */
	if (type_code != comment && type_code != newline && type_code != preesc)
	    parser_state_tos->last_token = type_code;

      }				/* end of main while (1) loop */
}

/*
 * copy input file to backup file if in_name is /blah/blah/blah/file, then
 * backup file will be "file.BAK" then make the backup file the input and
 * original input file the output
 */
bakcopy()
{
    /* file descriptor for the output file */
    int bakchn;

    /* Used to walk around file names */
    register char *p;

    /* Buffer for error message.  */
    char *errbuf;
    /* Backup file name.  */
    char *bakfile;

    /* construct name of backup file */
    for (p = in_name; *p; p++);	/* skip to end of string */
    while (p > in_name && *p != '/')	/* find last '/' */
	p--;
    if (*p == '/')
	p++;
    bakfile = xmalloc(40 + strlen(p));
    sprintf(bakfile, "%s.BAK", p);

    errbuf = xmalloc(80 + strlen(p));

    sprintf(errbuf,"indent: %s",bakfile);

    /* copy in_name to backup file */
    bakchn = creat(bakfile, 0600);
    if (bakchn < 0)
      {
	perror(errbuf);
	exit(1);
      }

    if (write(bakchn, in_prog, in_prog_size) != in_prog_size)
      {
	perror(errbuf);
	exit(1);
      }

    close(bakchn);

    /* now the original input file will be the output */
    output = fopen(in_name, "w");
    if (output == 0) {
	fprintf(stderr, "indent: can't create %s\n", in_name);
	unlink(bakfile);
	exit(1);
    }
    free (errbuf);
    free (bakfile);
}
