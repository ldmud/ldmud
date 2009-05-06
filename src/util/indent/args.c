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
static char sccsid[] = "@(#)args.c	5.6 (Berkeley) 9/15/88";
#endif /* not lint */

/*
 * Argument scanning and profile reading code.  Default parameters are set
 * here as well.
 */

#include "indent_globs.h"
#include <ctype.h>
#include "version.h"

int else_endif_col;

extern char *in_name;

char       *getenv();

/* profile types */
enum profile {PRO_BOOL, /* boolean */
	    PRO_INT, /* integer */
	    PRO_FONT, /* troff font */
	    PRO_IGN, /* ignore it */
	    PRO_STDIN, /* -st switch */
	    PRO_KEY, /* -T switch */
	    PRO_SETTINGS, /* bundled set of settings */
	    PRO_PRSTRING /* Print string and exit */
	      };

/* profile specials for booleans */
enum on_or_off {OFF,ON};

/* Explicit flags for each option.  */
static int exp_T = 0;
static int exp_bacc = 0;
static int exp_badp = 0;
static int exp_bad = 0;
static int exp_bap = 0;
static int exp_bbb = 0;
static int exp_bc = 0;
static int exp_bli = 0;
static int exp_bl = 0;
static int exp_bs = 0;
static int exp_cdb = 0;
static int exp_cd = 0;
static int exp_ce = 0;
static int exp_ci = 0;
static int exp_cli = 0;
static int exp_cp = 0;
static int exp_c = 0;
static int exp_di = 0;
static int exp_dj = 0;
static int exp_d = 0;
static int exp_eei = 0;
static int exp_ei = 0;
static int exp_fbc = 0;
static int exp_fbx = 0;
static int exp_fb = 0;
static int exp_fc1 = 0;
static int exp_fca = 0;
static int exp_fc = 0;
static int exp_fk = 0;
static int exp_fs = 0;
static int exp_gnu = 0;
static int exp_ip = 0;
static int exp_i = 0;
static int exp_lc = 0;
static int exp_lp = 0;
static int exp_l = 0;
static int exp_pcs = 0;
static int exp_psl = 0;
static int exp_pro = 0;
static int exp_ps = 0;
static int exp_kr = 0;
static int exp_sc = 0;
static int exp_sob = 0;
static int exp_ss = 0;
static int exp_st = 0;
static int exp_troff = 0;
static int exp_v = 0;
static int exp_version = 0;
static int exp_lpc = 0;

/* The following variables are controlled by command line parameters and
   their meaning is explained in indent_globs.h.  */
int leave_comma;
int decl_com_ind;
int case_indent;
int com_ind;
int decl_indent;
int ljust_decl;
int unindent_displace;
int else_if;
int indent_parameters;
int ind_size;
int blanklines_after_procs;
int lpc;
int noarrowspace;

/*
 * N.B.: because of the way the table here is scanned, options whose names are
 * substrings of other options must occur later; that is, with -lp vs -l, -lp
 * must be first.  Also, while (most) booleans occur more than once, the last
 * default value is the one actually assigned.
 */
struct pro {
    char       *p_name;		/* name, eg -bl, -cli */
    enum profile p_type;
    int         p_default;	/* the default value (if int) */

    /* If p_type == PRO_BOOL, ON or OFF to tell how this switch affects
         the variable.
       Not used for other p_type's.  */
    enum on_or_off p_special;

    /* if p_type == PRO_SETTINGS, a (char *) pointing to a list of the
       switches to set, separated by NULs, terminated by 2 NULs.
       if p_type == PRO_BOOL, PRO_INT, or PRO_FONT, address of the
       variable that gets set by the option.
       if p_type == PRO_PRSTRING, a (char *) pointing to the string.  */
    int *p_obj;

    /* Points to a nonzero value (allocated statically for all options)
       if the option has been specified explicitly.  This is necessary
       because for boolean options, the options to set and reset the
       variable must share the explicit flag.  */
    int* p_explicit;
};

struct pro pro[] = {

    {"T", PRO_KEY, 0, 0, 0, &exp_T},
    {"bacc", PRO_BOOL, false, ON,
      &blanklines_around_conditional_compilation, &exp_bacc},
    {"badp", PRO_BOOL, false, ON,
       &blanklines_after_declarations_at_proctop, &exp_badp},
    {"bad", PRO_BOOL, false, ON, &blanklines_after_declarations, &exp_bad},
    {"bap", PRO_BOOL, false, ON, &blanklines_after_procs, &exp_bap},
    {"bbb", PRO_BOOL, false, ON, &blanklines_before_blockcomments, &exp_bbb},
    {"bc", PRO_BOOL, true, OFF, &leave_comma, &exp_bc},
    {"bli", PRO_INT, 0, 0, &brace_indent, &exp_bli},
    {"bl", PRO_BOOL, true, OFF, &btype_2, &exp_bl},
    {"br", PRO_BOOL, true, ON, &btype_2, &exp_bl},
    {"bs", PRO_BOOL, false, ON, &Bill_Shannon, &exp_bs},
    {"cdb", PRO_BOOL, true, ON, &comment_delimiter_on_blankline, &exp_cdb},
    {"cd", PRO_INT, 0, 0, &decl_com_ind, &exp_cd},
    {"ce", PRO_BOOL, true, ON, &cuddle_else, &exp_ce},
    {"ci", PRO_INT, 0, 0, &continuation_indent, &exp_ci},
    {"cli", PRO_INT, 0, 0, &case_indent, &exp_cli},
    {"cp", PRO_INT, 33, 0, &else_endif_col, &exp_cp},
    {"c", PRO_INT, 33, 0, &com_ind, &exp_c},
    {"di", PRO_INT, 16, 0, &decl_indent, &exp_di},
    {"dj", PRO_BOOL, false, ON, &ljust_decl, &exp_dj},
    {"d", PRO_INT, 0, 0, &unindent_displace, &exp_d},
    {"eei", PRO_BOOL, false, ON, &extra_expression_indent, &exp_eei},
    {"ei", PRO_BOOL, true, ON, &else_if, &exp_ei},
    {"fbc", PRO_FONT, 0, 0, (int *) &blkcomf, &exp_fbc},
    {"fbx", PRO_FONT, 0, 0, (int *) &boxcomf, &exp_fbx},
    {"fb", PRO_FONT, 0, 0, (int *) &bodyf, &exp_fb},
    {"fc1", PRO_BOOL, true, ON, &format_col1_comments, &exp_fc1},
    {"fca", PRO_BOOL, true, ON, &format_comments, &exp_fca},
    {"fc", PRO_FONT, 0, 0, (int *) &scomf, &exp_fc},
    {"fk", PRO_FONT, 0, 0, (int *) &keywordf, &exp_fk},
    {"fs", PRO_FONT, 0, 0, (int *) &stringf, &exp_fs},
    {"gnu", PRO_SETTINGS, 0, 0,
      (int *)"-nbad\0-bap\0-nbbb\0-nbc\0-bl\0-ncdb\0-nce\0-di0\0-ndj\0\
-ei\0-nfc1\0-i2\0-ip5\0-lp\0-pcs\0-nps\0-psl\0-nsc\0-nsob\0-bli2\0-ss\0\
-cp1\0-nfca\0", &exp_gnu},
    {"ip", PRO_INT, 4, ON, &indent_parameters, &exp_ip},
    {"i", PRO_INT, 4, 0, &ind_size, &exp_i},
    {"kr", PRO_SETTINGS, 0, 0,
       (int *)"-nbad\0-bap\0-nbbb\0-nbc\0-br\0-c33\0-cd33\0-ncdb\0-ce\0\
-ci4\0-cli0\0-d0\0-di1\0-nfc1\0-i4\0-ip0\0-l75\0-lp\0-npcs\0-npsl\0\
-nsc\0-nsc\0-nsob\0-nfca\0-cp33\0-nss\0", &exp_kr},
    {"lpc", PRO_BOOL, false, ON, &lpc, &exp_lpc},
    {"lc", PRO_INT, 0, 0, &block_comment_max_col, &exp_lc},
    {"lp", PRO_BOOL, true, ON, &lineup_to_parens, &exp_lp},
    {"l", PRO_INT, 78, 0, &max_col, &exp_l},
    {"nbacc", PRO_BOOL, false, ON,
       &blanklines_around_conditional_compilation, &exp_bacc},
    {"nbadp", PRO_BOOL, false, OFF,
       &blanklines_after_declarations_at_proctop, &exp_badp},
    {"nbad", PRO_BOOL, false, OFF, &blanklines_after_declarations, &exp_bad},
    {"nbap", PRO_BOOL, false, OFF, &blanklines_after_procs, &exp_bap},
    {"nbbb", PRO_BOOL, false, OFF, &blanklines_before_blockcomments, &exp_bbb},
    {"nbc", PRO_BOOL, true, ON, &leave_comma, &exp_bc},
    {"nbs", PRO_BOOL, false, OFF, &Bill_Shannon, &exp_bs},
    {"ncdb", PRO_BOOL, true, OFF, &comment_delimiter_on_blankline, &exp_cdb},
    {"nce", PRO_BOOL, true, OFF, &cuddle_else, &exp_ce},
    {"ndj", PRO_BOOL, false, OFF, &ljust_decl, &exp_dj},
    {"neei", PRO_BOOL, false, OFF, &extra_expression_indent, &exp_eei},
    {"nei", PRO_BOOL, true, OFF, &else_if, &exp_ei},
    {"nfc1", PRO_BOOL, true, OFF, &format_col1_comments, &exp_fc1},
    {"nfca", PRO_BOOL, true, OFF, &format_comments, &exp_fca},
    {"nlp", PRO_BOOL, true, OFF, &lineup_to_parens, &exp_lp},
    {"npcs", PRO_BOOL, false, OFF, &proc_calls_space, &exp_pcs},
    {"npro", PRO_IGN, 0, 0, 0, &exp_pro},
    {"npsl", PRO_BOOL, true, OFF, &procnames_start_line, &exp_psl},
    {"nps", PRO_BOOL, false, OFF, &pointer_as_binop, &exp_ps},
    {"nsc", PRO_BOOL, true, OFF, &star_comment_cont, &exp_sc},
    {"nsob", PRO_BOOL, false, OFF, &swallow_optional_blanklines, &exp_sob},
    {"nss", PRO_BOOL, false, OFF, &space_sp_semicolon, &exp_ss},
    {"nv", PRO_BOOL, false, OFF, &verbose, &exp_v},
    {"pcs", PRO_BOOL, false, ON, &proc_calls_space, &exp_pcs},
    {"psl", PRO_BOOL, true, ON, &procnames_start_line, &exp_psl},
    {"ps", PRO_BOOL, false, ON, &pointer_as_binop, &exp_ps},
    {"sc", PRO_BOOL, true, ON, &star_comment_cont, &exp_sc},
    {"sob", PRO_BOOL, false, ON, &swallow_optional_blanklines, &exp_sob},
    {"ss", PRO_BOOL, false, ON, &space_sp_semicolon, &exp_ss},
    {"st", PRO_STDIN, 0, 0, 0, &exp_st},
    {"troff", PRO_BOOL, false, ON, &troff, &exp_troff},
    {"version", PRO_PRSTRING, 0, 0, (int *)VERSION_STRING, &exp_version},
    {"v", PRO_BOOL, false, ON, &verbose, &exp_v},

    /* Signify end of structure.  */
    {0, 0, 0, 0, 0, 0}
};

/*
 * set_profile reads $HOME/.indent.pro and ./.indent.pro and handles arguments
 * given in these files.
 */
set_profile()
{
    register FILE *f;
    char *fname;
    static char prof[] = ".indent.pro";

    char *homedir;

    homedir = getenv("HOME");
    fname = xmalloc(strlen(homedir) + 10 + sizeof prof);
    sprintf(fname, "%s/%s", homedir, prof);
    if ((f = fopen(prof, "r")) != NULL) {
	scan_profile(f);
	(void) fclose(f);
    }
    if ((f = fopen(fname, "r")) != NULL) {
	scan_profile(f);
	(void) fclose(f);
    }
    free (fname);
}

scan_profile(f)
    register FILE *f;
{
    register int i;
    register char *p;
    char        buf[BUFSIZ];

    while (1) {
	for (p = buf; (i = getc(f)) != EOF && (*p = i) > ' '; ++p);
	if (p != buf) {
	    *p++ = 0;
	    if (verbose)
		printf("profile: %s\n", buf);
	    set_option(buf, 1);
	}
	else if (i == EOF)
	    return;
    }
}

/* S1 should be a string.  S2 should be a string, perhaps followed by
   an argument.  Compare the two, returning true if they are equal,
   and if they are equal set *START_PARAM to point to the argument
   in S2.  */
eqin(s1, s2, start_param)
     register char *s1;
     register char *s2;
     char **start_param;
{
    while (*s1) {
	if (*s1++ != *s2++)
	    return (false);
    }
    *start_param = s2;
    return (true);
}

/*
 * Set the defaults.
 */
set_defaults()
{
    register struct pro *p;

    for (p = pro; p->p_name; p++)
	if (p->p_type == PRO_BOOL || p->p_type == PRO_INT)
	    *p->p_obj = p->p_default;
}

/* Process an option ARG (e.g. "-l60").
   EXPLICIT should be nonzero iff the argument is being explicitly
   specified (as opposed to being taken from a PRO_SETTINGS group of
   settings).  */

set_option (arg, explicit)
     char *arg;
     int explicit;
{
    struct pro *p;
    char       *param_start;

    arg++;			/* ignore leading "-" */
    for (p = pro; p->p_name; p++)
	if (*p->p_name == *arg && eqin(p->p_name, arg, &param_start))
	    goto found;
    fprintf(stderr, "indent: unknown parameter \"%s\"\n", arg - 1);
    exit(1);
found:
    /* If the parameter has been explicitly specified, we don't */
    /* want a group of bundled settings to override the explicit */
    /* setting.  */
    if (explicit || !*(p->p_explicit))
      {
	if (explicit)
	  *(p->p_explicit) = 1;
	
	switch (p->p_type) {
	
        case PRO_PRSTRING:
	  puts ((char *)p->p_obj);
	  exit (0);

	case PRO_SETTINGS:
	  {
	    char *t;  /* current position */
	
	    t = (char *)p->p_obj;
	    do
	      {
		set_option(t, 0);
		/* advance to character following next NUL */
		while (*t++) ;
	      }
	    while (*t);
	  }
	
	case PRO_IGN:
	  break;
	
	case PRO_STDIN:
	  if (in_name == 0)
	    {
	      read_stdin();
	      /* Let it be known that we have an input file.  */
	      in_name = "Standard Input";
	    }
	  if (output == 0)
	    output = stdout;
	  break;
	
	case PRO_KEY:
	  if (*param_start == 0)
	    goto need_param;
	  {
	    register char *str = (char *) xmalloc(strlen(param_start) + 1);
	    strcpy(str, param_start);
	    addkey(str, 4);
	  }
	  break;
	
	case PRO_BOOL:
	  if (p->p_special == OFF)
	    *p->p_obj = false;
	  else
	    *p->p_obj = true;
	  break;

	case PRO_INT:
	  if (*param_start == 0) {
	  need_param:
	    fprintf(stderr, "indent: ``%s'' requires a parameter\n",
		    arg - 1);
	    exit(1);
	  }
	  *p->p_obj = atoi(param_start);
	  break;
	
	case PRO_FONT:
	  parsefont((struct fstate *) p->p_obj, param_start);
	  break;
	
	default:
	  fprintf(stderr, "indent: set_option: internal error: p_type %d\n",
		  p->p_type);
	  exit(1);
	}
      }
}
