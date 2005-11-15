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
static char sccsid[] = "@(#)parse.c	5.8 (Berkeley) 9/15/88";
#endif /* not lint */

#include "indent_globs.h"

struct parser_state *parser_state_tos;

/* like ++parser_state_tos->tos but checks for stack overflow and extends
   stack if necessary.  */
static int
inc_pstack ()
{
  if (++parser_state_tos->tos >= parser_state_tos->p_stack_size)
    {
      parser_state_tos->p_stack_size *= 2;
      parser_state_tos->p_stack = (enum codes *)
	xrealloc (parser_state_tos->p_stack,
		 parser_state_tos->p_stack_size * sizeof (enum codes));
      parser_state_tos->il = (int *)
	xrealloc (parser_state_tos->il,
		  parser_state_tos->p_stack_size * sizeof (int));
      parser_state_tos->cstk = (int *)
	xrealloc (parser_state_tos->cstk,
		  parser_state_tos->p_stack_size * sizeof (int));
    }
  return parser_state_tos->tos;
}

void
parse(tk)
    enum codes tk;		/* the code for the construct scanned */
{
    int         i;

#ifdef debug
    printf("%2d - %s\n", tk, token);
#endif

    while (parser_state_tos->p_stack[parser_state_tos->tos] == ifhead && tk != elselit) {
	/* true if we have an if without an else */
	parser_state_tos->p_stack[parser_state_tos->tos] = stmt;	/* apply the if(..) stmt ::= stmt
					 * reduction */
	reduce();		/* see if this allows any reduction */
    }


    switch (tk) {		/* go on and figure out what to do with the
				 * input */

    case decl:			/* scanned a declaration word */
	parser_state_tos->search_brace = btype_2;
	/* indicate that following brace should be on same line */
	if (parser_state_tos->p_stack[parser_state_tos->tos] != decl) {	/* only put one declaration
						 * onto stack */
	    break_comma = true;	/* while in declaration, newline should be
				 * forced after comma */
	    parser_state_tos->p_stack[inc_pstack()] = decl;
	    parser_state_tos->il[parser_state_tos->tos] = parser_state_tos->i_l_follow;

	    if (ljust_decl) {/* only do if we want left justified
				 * declarations */
		parser_state_tos->ind_level = 0;
		for (i = parser_state_tos->tos - 1; i > 0; --i)
		    if (parser_state_tos->p_stack[i] == decl)
		      /* indentation is number of declaration levels deep
			 we are times spaces per level */
		      parser_state_tos->ind_level += ind_size;
		parser_state_tos->i_l_follow = parser_state_tos->ind_level;
	    }
	}
	break;

    case ifstmt:		/* scanned if (...) */
	if (parser_state_tos->p_stack[parser_state_tos->tos] == elsehead
	    && else_if)	/* "else if ..." */
	    parser_state_tos->i_l_follow = parser_state_tos->il[parser_state_tos->tos];
    case dolit:		/* 'do' */
    case forstmt:		/* for (...) */
	inc_pstack();
	parser_state_tos->p_stack[parser_state_tos->tos] = tk;
	parser_state_tos->il[parser_state_tos->tos] = parser_state_tos->ind_level = parser_state_tos->i_l_follow;
	parser_state_tos->i_l_follow += ind_size;	/* subsequent statements should be indented 1 */
	parser_state_tos->search_brace = btype_2;
	break;

    case lbrace:		/* scanned { */
	break_comma = false;	/* don't break comma in an initial list */
	if (parser_state_tos->p_stack[parser_state_tos->tos] == stmt || parser_state_tos->p_stack[parser_state_tos->tos] == decl
		|| parser_state_tos->p_stack[parser_state_tos->tos] == stmtl)
	  /* it is a random, isolated stmt group or a declaration */
	  parser_state_tos->i_l_follow += ind_size;	
	else {
	    if (s_code == e_code) {
		/*
		 * only do this if there is nothing on the line
		 */
	
		parser_state_tos->ind_level -= ind_size;
		/*
		 * it is a group as part of a while, for, etc.
		 */

		/* For -bl formatting, indent by brace_indent
		   additional spaces
		   e.g.
		   if (foo == bar)
		       {
		   <--> brace_indent spaces (in this example, 4)
		*/
		if (!btype_2)
		  {
		    parser_state_tos->ind_level += brace_indent;
		    parser_state_tos->i_l_follow += brace_indent;
		    if (parser_state_tos->p_stack[parser_state_tos->tos] == swstmt)
		      case_ind += brace_indent;
		  }

		if (parser_state_tos->p_stack[parser_state_tos->tos] == swstmt
		    && case_indent
		    >= ind_size)
		  parser_state_tos->ind_level -= ind_size;
		/*
		 * for a switch, brace should be two levels out from the code
		 */
	    }
	}

	inc_pstack();
	parser_state_tos->p_stack[parser_state_tos->tos] = lbrace;
	parser_state_tos->il[parser_state_tos->tos] = parser_state_tos->ind_level;
	inc_pstack();
	parser_state_tos->p_stack[parser_state_tos->tos] = stmt;
	/* allow null stmt between braces */
	parser_state_tos->il[parser_state_tos->tos] = parser_state_tos->i_l_follow;
	break;

    case whilestmt:		/* scanned while (...) */
	if (parser_state_tos->p_stack[parser_state_tos->tos] == dohead) {
	    /* it is matched with do stmt */
	    parser_state_tos->ind_level = parser_state_tos->i_l_follow = parser_state_tos->il[parser_state_tos->tos];
	    inc_pstack();
	    parser_state_tos->p_stack[parser_state_tos->tos] = whilestmt;
	    parser_state_tos->il[parser_state_tos->tos] = parser_state_tos->ind_level = parser_state_tos->i_l_follow;
	}
	else {			/* it is a while loop */
	  inc_pstack();
	  parser_state_tos->p_stack[parser_state_tos->tos] = whilestmt;
	  parser_state_tos->il[parser_state_tos->tos] = parser_state_tos->i_l_follow;
	  parser_state_tos->i_l_follow += ind_size;
	  parser_state_tos->search_brace = btype_2;
	}

	break;

    case elselit:		/* scanned an else */

	if (parser_state_tos->p_stack[parser_state_tos->tos] != ifhead)
	    diag(1, "Unmatched 'else'");
	else {
	    parser_state_tos->ind_level = parser_state_tos->il[parser_state_tos->tos];	/* indentation for else should
						 * be same as for if */
	    /* everything following should be in 1 level */
	    parser_state_tos->i_l_follow = parser_state_tos->ind_level + ind_size;
	
	    parser_state_tos->p_stack[parser_state_tos->tos] = elsehead;
	    /* remember if with else */
	    parser_state_tos->search_brace = btype_2 | else_if;
	}
	break;

    case rbrace:		/* scanned a } */
	/* stack should have <lbrace> <stmt> or <lbrace> <stmtl> */
	if (parser_state_tos->p_stack[parser_state_tos->tos - 1] == lbrace) {
	    parser_state_tos->ind_level = parser_state_tos->i_l_follow = parser_state_tos->il[--parser_state_tos->tos];
	    parser_state_tos->p_stack[parser_state_tos->tos] = stmt;
	}
	else
	    diag(1, "Stmt nesting error.");
	break;

    case swstmt:		/* had switch (...) */
	inc_pstack();
	parser_state_tos->p_stack[parser_state_tos->tos] = swstmt;
	parser_state_tos->cstk[parser_state_tos->tos] = case_ind;
	/* save current case indent level */
	parser_state_tos->il[parser_state_tos->tos] = parser_state_tos->i_l_follow;
	case_ind = parser_state_tos->i_l_follow + case_indent;	/* cases should be one
							 * level down from
							 * switch */
	/* statements should be two levels in */
	parser_state_tos->i_l_follow += case_indent + ind_size;

	parser_state_tos->search_brace = btype_2;
	break;

    case semicolon:		/* this indicates a simple stmt */
	break_comma = false;	/* turn off flag to break after commas in a
				 * declaration */
	inc_pstack();
	parser_state_tos->p_stack[parser_state_tos->tos] = stmt;
	parser_state_tos->il[parser_state_tos->tos] = parser_state_tos->ind_level;
	break;

    default:			/* this is an error */
	diag(1, "Unknown code to parser");
	return;


    }				/* end of switch */

    reduce();			/* see if any reduction can be done */

#ifdef debug
    for (i = 1; i <= parser_state_tos->tos; ++i)
	printf("(%d %d)", parser_state_tos->p_stack[i], parser_state_tos->il[i]);
    printf("\n");
#endif

    return;
}

/*
 * Copyright (C) 1976 by the Board of Trustees of the University of Illinois
 *
 * All rights reserved
 *
 *
 * NAME: reduce
 *
 * FUNCTION: Implements the reduce part of the parsing algorithm
 *
 * ALGORITHM: The following reductions are done.  Reductions are repeated until
 * no more are possible.
 *
 * Old TOS		New TOS <stmt> <stmt>	<stmtl> <stmtl> <stmt>	<stmtl> do
 * <stmt>	"dostmt" if <stmt>	"ifstmt" switch <stmt>	<stmt> decl
 * <stmt>	<stmt> "ifelse" <stmt>	<stmt> for <stmt>	<stmt> while
 * <stmt>	<stmt> "dostmt" while	<stmt>
 *
 * On each reduction, parser_state_tos->i_l_follow (the indentation for the following line) is
 * set to the indentation level associated with the old TOS.
 *
 * PARAMETERS: None
 *
 * RETURNS: Nothing
 *
 * GLOBALS: parser_state_tos->cstk parser_state_tos->i_l_follow = parser_state_tos->il parser_state_tos->p_stack = parser_state_tos->tos =
 *
 * CALLS: None
 *
 * CALLED BY: parse 
 *
 * HISTORY: initial coding 	November 1976	D A Willcox of CAC
 *
 */
/*----------------------------------------------*\
|   REDUCTION PHASE				    |
\*----------------------------------------------*/
reduce()
{

    register int i;

    for (;;) {			/* keep looping until there is nothing left to
				 * reduce */

	switch (parser_state_tos->p_stack[parser_state_tos->tos]) {

	case stmt:
	    switch (parser_state_tos->p_stack[parser_state_tos->tos - 1]) {

	    case stmt:
	    case stmtl:
		/* stmtl stmt or stmt stmt */
		parser_state_tos->p_stack[--parser_state_tos->tos] = stmtl;
		break;

	    case dolit:	/* <do> <stmt> */
		parser_state_tos->p_stack[--parser_state_tos->tos] = dohead;
		parser_state_tos->i_l_follow = parser_state_tos->il[parser_state_tos->tos];
		break;

	    case ifstmt:
		/* <if> <stmt> */
		parser_state_tos->p_stack[--parser_state_tos->tos] = ifhead;
		for (i = parser_state_tos->tos - 1;
			(
			 parser_state_tos->p_stack[i] != stmt
			 &&
			 parser_state_tos->p_stack[i] != stmtl
			 &&
			 parser_state_tos->p_stack[i] != lbrace
			 );
			--i);
		parser_state_tos->i_l_follow = parser_state_tos->il[i];
		/*
		 * for the time being, we will assume that there is no else on
		 * this if, and set the indentation level accordingly. If an
		 * else is scanned, it will be fixed up later
		 */
		break;

	    case swstmt:
		/* <switch> <stmt> */
		case_ind = parser_state_tos->cstk[parser_state_tos->tos - 1];

	    case decl:		/* finish of a declaration */
	    case elsehead:
		/* <<if> <stmt> else> <stmt> */
	    case forstmt:
		/* <for> <stmt> */
	    case whilestmt:
		/* <while> <stmt> */
		parser_state_tos->p_stack[--parser_state_tos->tos] = stmt;
		parser_state_tos->i_l_follow = parser_state_tos->il[parser_state_tos->tos];
		break;

	    default:		/* <anything else> <stmt> */
		return;

	    }			/* end of section for <stmt> on top of stack */
	    break;

	case whilestmt:	/* while (...) on top */
	    if (parser_state_tos->p_stack[parser_state_tos->tos - 1] == dohead) {
		/* it is termination of a do while */
		parser_state_tos->p_stack[--parser_state_tos->tos] = stmt;
		break;
	    }
	    else
		return;

	default:		/* anything else on top */
	    return;

	}
    }
}
