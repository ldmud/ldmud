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
static char sccsid[] = "@(#)pr_comment.c	5.9 (Berkeley) 9/15/88";
#endif /* not lint */

/*
 * NAME:
 *	pr_comment
 *
 * FUNCTION:
 *	This routine takes care of scanning and printing comments.
 *
 * ALGORITHM:
 *	1) Decide where the comment should be aligned, and if lines should
 *	   be broken.
 *	2) If lines should not be broken and filled, just copy up to end of
 *	   comment.
 *	3) If lines should be filled, then scan thru input_buffer copying
 *	   characters to com_buf.  Remember where the last blank, tab, or
 *	   newline was.  When line is filled, print up to last blank and
 *	   continue copying.
 *
 * HISTORY:
 *	November 1976	D A Willcox of CAC	Initial coding
 *	12/6/76		D A Willcox of CAC	Modification to handle
 *						UNIX-style comments
 *
 */

/*
 * this routine processes comments.  It makes an attempt to keep comments from
 * going over the max line length.  If a line is too long, it moves everything
 * from the last blank to the next comment line.  Blanks and tabs from the
 * beginning of the input line are removed
 */


#include "indent_globs.h"
#include "io.h"

void pr_comment()
{
    int         now_col;	/* column we are in now */
    int         adj_max_col;	/* Adjusted max_col for when we decide to
				 * spill comments over the right margin */
    char       *last_bl;	/* points to the last blank in the output
				 * buffer */
    char       *t_ptr;		/* used for moving string */
    int         unix_comment;	/* tri-state variable used to decide if it is
				 * a unix-style comment. 0 means only blanks
				 * since '/ *', 1 means regular style comment, 2
				 * means unix style comment */
    int         break_delim = comment_delimiter_on_blankline;
    int         l_just_saw_decl = parser_state_tos->just_saw_decl;
#if 0
     int         parser_state_tos->last_nl = 0;	/* true iff the last significant thing
     * weve seen is a newline
     */
#endif
    int         one_liner = 1;	/* true iff this comment is a one-liner */
    adj_max_col = max_col;
    parser_state_tos->just_saw_decl = 0;
    last_bl = 0;		/* no blanks found so far */
    parser_state_tos->box_com = false;		/* at first, assume that we are not in
					 * a boxed comment or some other
					 * comment that should not be touched */
    ++parser_state_tos->out_coms;		/* keep track of number of comments */
    unix_comment = 1;		/* set flag to let us figure out if there is a
				 * unix-style comment ** DISABLED: use 0 to
				 * reenable this hack! */

    /* Figure where to align and how to treat the comment */

    if (parser_state_tos->col_1 && !format_col1_comments) {	/* if comment starts in column
						 * 1 it should not be touched */
	parser_state_tos->box_com = true;
	parser_state_tos->com_col = 1;
    }
    else {
	if (*buf_ptr == '-' || *buf_ptr == '*' || !format_comments) {
	    parser_state_tos->box_com = true;	/* a comment with a '-' or '*' immediately
				 * after the '/ *' is assumed to be a boxed
				 * comment */
	    break_delim = 0;
	}
	if ( /* parser_state_tos->bl_line && */ (s_lab == e_lab) && (s_code == e_code)) {
	    /* klg: check only if this line is blank */
	    /*
	     * If this (*and previous lines are*) blank, dont put comment way
	     * out at left
	     */
	    parser_state_tos->com_col = (parser_state_tos->ind_level - unindent_displace) + 1;
	    adj_max_col = block_comment_max_col;
	    if (parser_state_tos->com_col <= 1)
		parser_state_tos->com_col = 1 + !format_col1_comments;
	}
	else {
	    register int   target_col;
	    break_delim = 0;
	    if (s_code != e_code)
		target_col = count_spaces(compute_code_target(), s_code);
	    else {
		target_col = 1;
		if (s_lab != e_lab)
		    target_col = count_spaces(compute_label_target(), s_lab);
	    }
	    parser_state_tos->com_col = parser_state_tos->decl_on_line || parser_state_tos->ind_level == 0 ? decl_com_ind : com_ind;
	    /* If we are already past the position for the comment,
	       put it at the next tab stop.  */
	    if (parser_state_tos->com_col < target_col)
		parser_state_tos->com_col = ((target_col + 7) & ~7) + 1;
	    if (else_or_endif)
	      {
		parser_state_tos->com_col = else_endif_col;
		else_or_endif = false;
		/* We want the comment to appear one space after the #else
		   or #endif.  */
		if (parser_state_tos->com_col < target_col)
		  parser_state_tos->com_col = target_col + 1;
	      }
	    if (parser_state_tos->com_col + 24 > adj_max_col)
		adj_max_col = parser_state_tos->com_col + 24;
	}
    }
    if (parser_state_tos->box_com) {
	buf_ptr[-2] = 0;
	parser_state_tos->n_comment_delta = 1 - count_spaces(1, cur_line);
	buf_ptr[-2] = '/';
    }
    else {
	parser_state_tos->n_comment_delta = 0;
	while (*buf_ptr == ' ' || *buf_ptr == '\t')
	    buf_ptr++;
    }
    parser_state_tos->comment_delta = 0;
    *e_com++ = '/';		/* put '/ *' into buffer */
    *e_com++ = '*';
    if (*buf_ptr != ' ' && !parser_state_tos->box_com)
	*e_com++ = ' ';

    *e_com = '\0';
    if (troff) {
	now_col = 1;
	adj_max_col = 80;
    }
    else
	now_col = count_spaces(parser_state_tos->com_col, s_com);	/* figure what column we
							 * would be in if we
							 * printed the comment
							 * now */

    /* Start to copy the comment */

    while (1) {			/* this loop will go until the comment is
				 * copied */
	if (*buf_ptr > 040 && *buf_ptr != '*')
	    parser_state_tos->last_nl = 0;
	check_size(com);
	switch (*buf_ptr) {	/* this checks for various spcl cases */
	case 014:		/* check for a form feed */
	    if (!parser_state_tos->box_com) {	/* in a text comment, break the line here */
		parser_state_tos->use_ff = true;
		/* fix so dump_line uses a form feed */
		dump_line();
		last_bl = 0;
		*e_com++ = ' ';
		*e_com++ = '*';
		*e_com++ = ' ';
		while (*++buf_ptr == ' ' || *buf_ptr == '\t');
	    }
	    else {
		if (++buf_ptr >= buf_end)
		    fill_buffer();
		*e_com++ = 014;
	    }
	    break;

	case '\n':
	    if (had_eof) {	/* check for unexpected eof */
		printf("Unterminated comment\n");
		*e_com = '\0';
		dump_line();
		return;
	    }
	    one_liner = 0;
	    if (parser_state_tos->box_com || parser_state_tos->last_nl) {	/* if this is a boxed comment,
						 * we dont ignore the newline */
		if (s_com == e_com) {
		    *e_com++ = ' ';
		    *e_com++ = ' ';
		}
		*e_com = '\0';
		if (!parser_state_tos->box_com && e_com - s_com > 3) {
		    if (break_delim == 1 && s_com[0] == '/'
			    && s_com[1] == '*' && s_com[2] == ' ') {
			char       *t = e_com;
			break_delim = 2;
			e_com = s_com + 2;
			*e_com = 0;
			if (blanklines_before_blockcomments)
			    prefix_blankline_requested = 1;
			dump_line();
			e_com = t;
			s_com[0] = s_com[1] = s_com[2] = ' ';
		    }
		    dump_line();
		    check_size(com);
		    *e_com++ = ' ';
		    *e_com++ = ' ';
		}
		dump_line();
		now_col = parser_state_tos->com_col;
	    }
	    else {
		parser_state_tos->last_nl = 1;
		if (unix_comment != 1) {	/* we not are in unix_style
						 * comment */
		    if (unix_comment == 0 && s_code == e_code) {
			/*
			 * if it is a UNIX-style comment, ignore the
			 * requirement that previous line be blank for
			 * unindention
			 */
			parser_state_tos->com_col = ((parser_state_tos->ind_level - unindent_displace)
				      + ind_size);
			if (parser_state_tos->com_col <= 1)
			    parser_state_tos->com_col = 2;
		    }
		    unix_comment = 2;	/* permanently remember that we are in
					 * this type of comment */
		    dump_line();
		    ++line_no;
		    now_col = parser_state_tos->com_col;
		    *e_com++ = ' ';
		    /*
		     * fix so that the star at the start of the line will line
		     * up
		     */
		    do		/* flush leading white space */
			if (++buf_ptr >= buf_end)
			    fill_buffer();
		    while (*buf_ptr == ' ' || *buf_ptr == '\t');
		    break;
		}
		if (*(e_com - 1) == ' ' || *(e_com - 1) == '\t')
		    last_bl = e_com - 1;
		/*
		 * if there was a space at the end of the last line, remember
		 * where it was
		 */
		else {		/* otherwise, insert one */
		    last_bl = e_com;
		    check_size(com);
		    *e_com++ = ' ';
		    ++now_col;
		}
	    }
	    ++line_no;		/* keep track of input line number */
	    if (!parser_state_tos->box_com) {
		int         nstar = 1;
		do {		/* flush any blanks and/or tabs at start of
				 * next line */
		    if (++buf_ptr >= buf_end)
			fill_buffer();
		    if (*buf_ptr == '*' && --nstar >= 0) {
			if (++buf_ptr >= buf_end)
			    fill_buffer();
			if (*buf_ptr == '/')
			    goto end_of_comment;
		    }
		} while (*buf_ptr == ' ' || *buf_ptr == '\t');
	    }
	    else if (++buf_ptr >= buf_end)
		fill_buffer();
	    break;		/* end of case for newline */

	case '*':		/* must check for possibility of being at end
				 * of comment */
	    if (++buf_ptr >= buf_end)	/* get to next char after * */
		fill_buffer();

	    if (unix_comment == 0)	/* set flag to show we are not in
					 * unix-style comment */
		unix_comment = 1;

	    if (*buf_ptr == '/') {	/* it is the end!!! */
	end_of_comment:
		if (++buf_ptr >= buf_end)
		    fill_buffer();

		if (*(e_com - 1) != ' ' && !parser_state_tos->box_com) {	/* insure blank before
								 * end */
		    *e_com++ = ' ';
		    ++now_col;
		}
		if (break_delim == 1 && !one_liner && s_com[0] == '/'
			&& s_com[1] == '*' && s_com[2] == ' ') {
		    char       *t = e_com;
		    break_delim = 2;
		    e_com = s_com + 2;
		    *e_com = 0;
		    if (blanklines_before_blockcomments)
			prefix_blankline_requested = 1;
		    dump_line();
		    e_com = t;
		    s_com[0] = s_com[1] = s_com[2] = ' ';
		}
		if (break_delim == 2 && e_com > s_com + 3
			 /* now_col > adj_max_col - 2 && !parser_state_tos->box_com */ ) {
		    *e_com = '\0';
		    dump_line();
		    now_col = parser_state_tos->com_col;
		}
		check_size(com);
		*e_com++ = '*';
		*e_com++ = '/';
		*e_com = '\0';
		parser_state_tos->just_saw_decl = l_just_saw_decl;
		return;
	    }
	    else {		/* handle isolated '*' */
		*e_com++ = '*';
		++now_col;
	    }
	    break;
	default:		/* we have a random char */
	    if (unix_comment == 0 && *buf_ptr != ' ' && *buf_ptr != '\t')
		unix_comment = 1;	/* we are not in unix-style comment */

	    *e_com = *buf_ptr++;
	    if (buf_ptr >= buf_end)
		fill_buffer();

	    if (*e_com == '\t')	/* keep track of column */
		now_col = ((now_col - 1) & tabmask) + tabsize + 1;
	    else if (*e_com == '\b')	/* this is a backspace */
		--now_col;
	    else
		++now_col;

	    if (*e_com == ' ' || *e_com == '\t')
		last_bl = e_com;
	    /* remember we saw a blank */

	    ++e_com;
	    if (now_col > adj_max_col && !parser_state_tos->box_com && unix_comment == 1 && e_com[-1] > ' ') {
		/*
		 * the comment is too long, it must be broken up
		 */
		if (break_delim == 1 && s_com[0] == '/'
			&& s_com[1] == '*' && s_com[2] == ' ') {
		    char       *t = e_com;
		    break_delim = 2;
		    e_com = s_com + 2;
		    *e_com = 0;
		    if (blanklines_before_blockcomments)
			prefix_blankline_requested = 1;
		    dump_line();
		    e_com = t;
		    s_com[0] = s_com[1] = s_com[2] = ' ';
		}
		if (last_bl == 0) {	/* we have seen no blanks */
		    last_bl = e_com;	/* fake it */
		    *e_com++ = ' ';
		}
		*e_com = '\0';	/* print what we have */
		*last_bl = '\0';
		while (last_bl > s_com && last_bl[-1] < 040)
		    *--last_bl = 0;
		e_com = last_bl;
		dump_line();

		*e_com++ = ' ';	/* add blanks for continuation */
		*e_com++ = ' ';
		*e_com++ = ' ';

		t_ptr = last_bl + 1;
		last_bl = 0;
		if (t_ptr >= e_com) {
		    while (*t_ptr == ' ' || *t_ptr == '\t')
			t_ptr++;
		    while (*t_ptr != '\0') {	/* move unprinted part of
						 * comment down in buffer */
			if (*t_ptr == ' ' || *t_ptr == '\t')
			    last_bl = e_com;
			*e_com++ = *t_ptr++;
		    }
		}
		*e_com = '\0';
		now_col = count_spaces(parser_state_tos->com_col, s_com);	/* recompute current
								 * position */
	    }
	    break;
	}
    }
}
