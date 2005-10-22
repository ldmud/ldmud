
#include <ctype.h>
#include <stdio.h>

#include "bat.h"
#include "main.h"
#include "object.h"
#include "simulate.h"
#include "interpret.h"
#include "xalloc.h"
#include "svalue.h"
#include "array.h"
#include "swap.h"
#include "typedefs.h"
#include "comm.h"
#include "mstrings.h"
#include "mapping.h"
#include "stdstrings.h"
#include "profil2.h"

#define TYPE_TESTV1(arg1,type1) \
  if ((arg1)->type != type1) \
      efun_gen_arg_error(1, type1, arg1);

#define TYPE_TESTV2(arg1,type1) \
  if ((arg1)->type != type1) \
      efun_gen_arg_error(2, type1, arg1);

#define TYPE_TESTV3(arg1,type1) \
  if ((arg1)->type != type1) \
      efun_gen_arg_error(3, type1, arg1);

#define TYPE_TESTV4(arg1,type1) \
  if ((arg1)->type != type1) \
      efun_gen_arg_error(4, type1, arg1);

static int same_path(const char* p1, const char* p2, int len);
static int count_objects(const char* name, int len);

char catch_write_buf[CATCH_WRITE_BUF_LEN+1];
int catching_write   = 0;
int catch_write_len  = 0;


void begin_catch_write()
{
    if(catching_write)
    {
	catching_write = 0;
	catch_write_len = 0;
	catch_write_buf[0] = '\0';
	
	error("Double catch_write() detected.\n");

	return;
    }

    catching_write  = 1;
    catch_write_len = 0;
    catch_write_buf[0] = '\0';
}

char* end_catch_write()
{
    char* tmp = 0;
    if(catching_write == 0)
	return 0;
    
    catch_write_buf[CATCH_WRITE_BUF_LEN-1] = '\0';
    catch_write_buf[catch_write_len] = '\0';
    tmp = string_copy(catch_write_buf);

    catch_write_buf[0] = '\0';
    catching_write = 0;

    return tmp;
}

vector_t *all_objects(const char* name, int len)
{
    vector_t *v;
    object_t *ob;
    int count;
    int i;
    char *p, *cp;

    if (len < 0)
	return 0;
    cp = string_copy(name);
    if ((p = strrchr(cp, '#')) != 0)
	*p = '\0';
    p = cp;
    if (*p == '/') {
	p++;
	if (len > 0)
	    len--;
    }

    count = count_objects(name, len);
    
    if (count == 0)
    {
	xfree(cp);
	return allocate_array(0);
    }
    if (count >= MAX_ARRAY_SIZE)
	count = MAX_ARRAY_SIZE;
    
    v = allocate_uninit_array(count);
    
    for (i = 0, ob = obj_list; ob && i < count; ob = ob->next_all) {
	if ((ob->flags & O_DESTRUCTED) || !same_path(p, get_txt(ob->name), len))
	    continue;
	if (ob->flags & O_SWAPPED)
	    load_ob_from_swap(ob);

	v->item[i].type = T_OBJECT;
	v->item[i].u.ob = ob;
	put_ref_object(&v->item[i], ob, "all_objects");

	i++;
    }
    
    xfree(cp);
    return v;
}

svalue_t *f_all_objects(svalue_t * sp, int n)
{
    svalue_t* arg;
    char *str;
    vector_t* v;
    
    arg = sp - n + 1;
    
    switch(arg[0].type) 
    {
    case T_OBJECT: str = get_txt(arg[0].u.ob->name); break;
    case T_STRING: str = get_txt(arg[0].u.str); break;
    default: TYPE_TESTV1(&arg[0], T_STRING); break;
    }
    
    if (n == 1)
	v = all_objects(str, 0);
    else {
	TYPE_TESTV2(arg+1, T_NUMBER);
	v = all_objects(str, arg[1].u.number);
    }

    sp = pop_n_elems(n, sp) + 1;
    
    if (v)
	put_array(sp, v);
    else
	put_number(sp, 0);
    
    return sp;
}

static object_t* first_object(const char *name, int len)
{
    object_t *ob;
    char *p;
    char *cp;

    if (len < 0)
	return 0;
    cp = string_copy(name);
    if ((p = strrchr(cp, '#')) != 0)
	*p = '\0';
    p = cp;
    if (*p == '/') {
	p++;
	if (len > 0)
	    len--;
    }
   
    for (ob = obj_list; ob; ob = ob->next_all) {
	if (!(ob->flags & O_DESTRUCTED) && same_path(p, get_txt(ob->name), len)) {
	    if (ob->flags & O_SWAPPED)
		load_ob_from_swap(ob);
      
	    xfree(cp);
	    return ob;
	}
    }
  
    xfree(cp);
    return 0;
}

svalue_t *f_first_object(svalue_t * sp, int num_arg)
{
    object_t *ob;
    svalue_t *arg;
    char *str;

    arg = sp - num_arg + 1;

    switch(arg[0].type) 
    {
    case T_OBJECT: str = get_txt(arg[0].u.ob->name); break;
    case T_STRING: str = get_txt(arg[0].u.str); break;
    default: TYPE_TESTV1(&arg[0], T_STRING); break;
    }
  
    if (num_arg == 1)
	ob = first_object(str, 0);
    else {
	TYPE_TESTV2(&arg[1], T_NUMBER);
	ob = first_object(str, arg[1].u.number);
    }

    sp = pop_n_elems(num_arg, sp) + 1;
  
    if (ob)
	put_ref_object(sp, ob, "first_object");
    else
	put_number(sp, 0);
  
    return sp;
}

static object_t* next_object(object_t* ob, int len)
{
    char *p, *cp;

    if (len < 0)
	return 0;
    cp = string_copy(get_txt(ob->name));
    if ((p = strrchr(cp, '#')) != 0)
	*p = '\0';
    p = cp;
    if (*p == '/') {
	p++;
	if (len > 0)
	    len--;
    }


    for(ob = ob->next_all; ob; ob = ob->next_all) {
	if (!(ob->flags & O_DESTRUCTED) &&
	    same_path(p, get_txt(ob->name), len)) {
	    if (ob->flags & O_SWAPPED)
		load_ob_from_swap(ob);
	    xfree(cp);
	    return ob;
	}
    }
    
    xfree(cp);
    return 0;
}

svalue_t *f_next_object(svalue_t * sp, int n)
{
    object_t *ob;
    svalue_t *arg;
    
    arg = sp - n + 1;

    TYPE_TESTV1(arg, T_OBJECT);
    if (n == 1)
	ob = next_object(arg[0].u.ob, 0);
    else {
	TYPE_TESTV2(arg+1, T_NUMBER);
	ob = next_object(arg[0].u.ob, arg[1].u.number);
    }
    
    sp = pop_n_elems(n, sp) + 1;

    if (ob)
	put_ref_object(sp, ob, "next_object");
    else
	put_number(sp, 0);

    return sp;
}

svalue_t *f_all_shadow(svalue_t * sp)
{
    object_t *ob;
    vector_t *v;
    svalue_t *svp;
    int i;

    TYPE_TESTV1(sp, T_OBJECT);

    ob = sp->u.ob;

    if( !(ob->flags & O_SHADOW) )
    {
	i = 0;
    }
    else
    {
	while (O_GET_SHADOW(ob)->shadowed_by)
	    ob = O_GET_SHADOW(ob)->shadowed_by;
  
	for (i = 1; O_GET_SHADOW(ob)->shadowing; i++)
	    ob = O_GET_SHADOW(ob)->shadowing;
    }
  
    v = allocate_uninit_array(i);
    svp = v->item;
  
    while (i--)
    {
	put_ref_object(svp, ob, "all_shadow");
	ob = O_GET_SHADOW(ob)->shadowed_by;
	svp++;
    }

    free_svalue(sp);
    put_array(sp, v);

    return sp;
}

svalue_t *f_first_shadow(svalue_t * sp)
{
    object_t* ob;

    TYPE_TESTV1(sp, T_OBJECT);
    ob = sp->u.ob;

    if( !(ob->flags & O_SHADOW) )
    {
	free_svalue(sp);
	put_number(sp, 0);
	return sp;
    }
    
    while (O_GET_SHADOW(ob)->shadowed_by)
    {
	ob = O_GET_SHADOW(ob)->shadowed_by;
	/*	
		fprintf(stderr, "FShadow: %s\n", get_txt(ob->name));
	*/
    }

    free_svalue(sp);
    put_ref_object(sp, ob, "first_shadow");

    return sp;
}

svalue_t *f_last_shadow(svalue_t * sp)
{
    object_t* ob;

    TYPE_TESTV1(sp, T_OBJECT);
    ob = sp->u.ob;

    if( !(ob->flags & O_SHADOW) )
    {
	free_svalue(sp);
	put_number(sp, 0);
	return sp;
    }
    
    while (O_GET_SHADOW(ob)->shadowing)
    {
	ob = O_GET_SHADOW(ob)->shadowing;
	/*
	  fprintf(stderr, "LShadow: %s\n", get_txt(ob->name));
	*/
    }

    free_svalue(sp);
    put_ref_object(sp, ob, "last_shadow");

    return sp;
}

svalue_t *f_next_shadow(svalue_t * sp)
{
    object_t* ob;
    
    TYPE_TESTV1(sp, T_OBJECT);
    ob = sp->u.ob;

    if( !(ob->flags & O_SHADOW) )
    {
	free_svalue(sp);
	put_number(sp, 0);
	return sp;
    }
    
    if(O_GET_SHADOW(ob)->shadowing)
    {
	ob = O_GET_SHADOW(ob)->shadowing;
    }
    else
    {
	free_svalue(sp);
	put_number(sp, 0);
	return sp;
    }

    free_svalue(sp);
    put_ref_object(sp, ob, "next_shadow");

    return sp;
}

static int same_path(const char* p1, const char* p2, int len)
{
    int len1, len2;
  
    if (*p1 != *p2)
	return 0;
    len1 = strlen(p1);
    len2 = strlen(p2);
    if (len) {
	if (len2 < len || len > len1)
	    return 0;
	len1 = len;
    } else {
	if (len1 > len2)
	    return 0;
    }
    if (strncmp(p1, p2, len1))
	return 0;
    if (!len && p2[len1] != '#' && p2[len1] != '\0')
	return 0;
    return 1;
}

static int count_objects(const char* name, int len)
{
    int i;
    object_t* ob;
    char *p, *cp;
  
    if (len < 0)
	return 0;
    cp = string_copy(name);
    if ((p = strrchr(cp, '#')) != 0)
	*p = '\0';
    p = cp;
    if (*p == '/') {
	p++;
	if (len > 0)
	    len--;
    }
  
    for (i = 0, ob = obj_list; ob; ob = ob->next_all)
	if (!(ob->flags & O_DESTRUCTED) && same_path(p, get_txt(ob->name), len))
	    i++;
    
    xfree(cp);
    return i;
}


svalue_t *f_count_objects(svalue_t * sp, int n)
{
    svalue_t* arg;
    char *str;
    int ret;

    arg = sp - n + 1;
    
    switch(arg[0].type) 
    {
    case T_OBJECT: str = get_txt(arg[0].u.ob->name); break;
    case T_STRING: str = get_txt(arg[0].u.str); break;
    default: TYPE_TESTV1(&arg[0], T_STRING); break;
    }
    
    if (n == 1)
	ret = count_objects(str, 0);
    else {
	TYPE_TESTV2(&arg[1], T_NUMBER);
	ret = count_objects(str, arg[1].u.number);
    }
    
    sp = pop_n_elems(n, sp) + 1;
    put_number(sp, ret);
    
    return sp;
}

#define MAX_LINE_LEN    1024

static int count_lines(string_t *file)
{
    FILE *f;
    int ret;
    static char buff[MAX_LINE_LEN];
  
    file = check_valid_path(file, current_object, STR_COUNT_LINES, MY_FALSE);
    if (file == 0)
	return 0;
    f = fopen(get_txt(file), "r");
    if (f == 0)
	return -1;
    ret = 0;
    while (fgets(buff, MAX_LINE_LEN, f) != 0)
	ret++;
    fclose(f);
    return ret;
}

svalue_t *f_count_lines(svalue_t *sp)
{
    int i;
  
    assign_eval_cost();
    TYPE_TESTV1(sp, T_STRING);
    i = count_lines(sp->u.str);

    free_svalue(sp);
   
    put_number(sp, i);
    
    return sp;
}



#define MAX_SEND_LEN 4000

static string_t *line_wrap_string(line, prompt, startline, linelen)
    char *line, *prompt, *startline;
    int linelen;
{
    char *tmp, buff[MAX_SEND_LEN+2];
    char *srcpos;
    enum {S_prompt, S_line} curstr;
    int startlen, curlen, totlen, lastspace, wordlen;
    int startpos;			/* tmp for startline-add */
    string_t *new;

    /* clear the first byte because it may be read in the end */
    buff[0] = 0;

    if (linelen < 21)
	linelen = 21;
    wordlen = linelen/4;
    startlen = strlen(startline);
    totlen = 0;
    curlen = 0;
    lastspace = -1;
    tmp = buff+1;			/* space for lastspace */
    srcpos = prompt;
    curstr = S_prompt;

    while(totlen < (MAX_SEND_LEN-4))
    {
	if(*srcpos == '\0')
	{
	    if(curstr == S_prompt)
	    {
		curstr = S_line;
		srcpos = line;
		continue;
	    }
	    else
		break;		/* out from while */
	}
      
	tmp[totlen] = *srcpos;
	if (*srcpos == ' ') {
	    lastspace = totlen;
	} else if (*srcpos == '\n') { /* reset to line start */
	    lastspace = totlen++;
	    srcpos++;
	    curlen = 0;
	    startpos =totlen;
	    goto startline_add;	/* jump, jump,... */
	}
	totlen++;
	curlen++;
	srcpos++;
      
	if(curlen >= linelen)	/* time to check wrap */
	{
	    if(totlen-lastspace < wordlen) /* space in asseptable len */
	    {
		tmp[lastspace] = '\n';
		curlen = totlen - lastspace - 1;
		startpos = lastspace + 1;
	    }
	    else			/* force wrap */
		if(totlen < (MAX_SEND_LEN-4)) /* check there is space to insert */
		{
		    tmp[totlen] = '\n';
		    lastspace = totlen; /* reset it to line end (\n) (safe place)*/
		    totlen++;
		    startpos = totlen;
		    curlen = 0;
		}
		else
		    break;		/* there wasn't space. out of while */

	    if(curstr == S_prompt || startlen == 0)
		continue;
	  
	    /*
	     * Insert startline
	     */
	startline_add:
	    if(totlen+startlen < (MAX_SEND_LEN-4))
	    {
		int i;
		for(i = totlen-1 ; i>=startpos ; i--) /*move old to right place*/
		    tmp[i+startlen] = tmp[i];
		for(i = 0 ; i<startlen ; i++, startpos++)	/* copy startline */
		    tmp[startpos] = startline[i];
		curlen += startlen;
		totlen += startlen;
	    }
	    else
		break;		/* there wasn't space. out of while */
	} /* if wrap */
    } /* while totlen */

    /*
     * Insert possible last linefeed
     */
    if(tmp[totlen-1]!='\n')
    {
	tmp[totlen]='\n';
	totlen++;
    }
    tmp[totlen]='\0';
    totlen++;

    /*
     * Time to copy & return
     */
    
    new = new_mstring(tmp);
    return new;
}

#define DEFAULT_WRAP_LEN	79

svalue_t *f_line_wrap (svalue_t *sp, int num_arg)
{
    svalue_t * args;
    char *str, *first = "", *rest = "";
    int len = DEFAULT_WRAP_LEN;
    string_t *new;
  
    args = sp - num_arg + 1;

    TYPE_TESTV1(args, T_STRING);
    str = get_txt(args[0].u.str);

    if(num_arg > 1 && !(args[1].type == T_NUMBER && args[1].u.number == 0))
    {
	TYPE_TESTV2(args+1, T_STRING);
	first = get_txt(args[1].u.str);
    }

    if(num_arg > 2 && !(args[2].type == T_NUMBER && args[2].u.number == 0))
    {
	TYPE_TESTV3(args+2, T_STRING);
	rest = get_txt(args[2].u.str);
    }

    if(num_arg > 3)
    {
	TYPE_TESTV4(args+3, T_NUMBER);
	len = args[3].u.number;
    }

    new = line_wrap_string(str, first, rest, len);
  
    sp = pop_n_elems(num_arg, sp) + 1;

    if (new)
    {
	put_string(sp, new);
    } 
    else
    {
	put_number(sp, 0);
    }
    
    return sp;
}


svalue_t *f_nuke_controls (svalue_t *sp, int num_arg)
{
    char *s, *d;
    unsigned char c;
    string_t *new;
    int len, i;

    TYPE_TESTV1(sp, T_STRING);

    len = 0;
    s = get_txt(sp->u.str);
    /*
      printf("ORIG '%s', size %d, strlen %d\n", 
      sp->u.str->str->txt,
      sp->u.str->str->size,
      strlen(sp->u.str->str->txt)
      );
    */
    for(i = 0; i < mstrsize(sp->u.str); i++)
    {
	c = s[i];

	if (isprint(c) || c == 229 || c == 228 || c == 246 
	    || c == 197 || c == 196 || c == 214)
	    len++;
    }

    //    printf("nuked len %d\n", len);

    new = alloc_mstring(len);
    d = get_txt(new);

    for(i = 0; i < mstrsize(sp->u.str); i++)
    {
	c = s[i];

	if (isprint(c) || c == 229 || c == 228 || c == 246 
	    || c == 197 || c == 196 || c == 214)
	{
	    //	    printf("Copying '%c'\n", c);
	    *d = c;
	    d++;
	}
    }
    /*
      printf("DEST '%s', size %d, strlen %d\n", 
      new->str->txt,
      new->str->size,
      strlen(new->str->txt)
      );
    */

    free_mstring(sp->u.str);
    sp->u.str = new;

    return sp;
}

svalue_t *f_query_input_to(svalue_t * sp, int num_arg)
{
    object_t *ob;
    interactive_t *ip;
  
    TYPE_TESTV1(sp, T_OBJECT);
  
    ob = sp->u.ob;
    if (O_SET_INTERACTIVE(ip, ob) && 
	ip->input_to)
    {
	if(ip->input_to->fun.is_lambda)
	{
	    put_number(sp, 0);
	}
	else
	{
	    sp->type = T_STRING;
	    sp->u.str = dup_mstring(ip->input_to->fun.function.named.name);
	    //	  put_malloced_string(sp, string_copy(ip->input_to->fun.function.named.name));
	}
    }
    else
    {
	put_number(sp, 0);
    }
  
    deref_object(ob, "query_input_to");
  
    return sp;
}



svalue_t *
f_profil_enable (svalue_t *sp)
{
    /* int profil_enable (int enabled); */

    fprintf (stderr, "ENABLE %ld\n", sp->u.number);

    profil_enable (sp->u.number);
    free_svalue (sp);

    put_number (sp, 0);
    return sp;
}

svalue_t *
f_profil_reset (svalue_t *sp)
{
    /* int profil_reset (); */

    profil_reset ();

    push_number (sp, 0);
    return sp;
}

svalue_t *
f_profil_query_results (svalue_t *sp)
{
    /* string profil_query_results (int sort, int max_entries); */

    unsigned int len;
    string_t *string;

    unsigned int sort, max_entries;

    max_entries= sp->u.number;
    free_svalue (sp);
    sp--;

    sort= sp->u.number;
    free_svalue (sp);

    len= profil_query_results (NULL, 0, sort, max_entries);
    memsafe (string= mstring_alloc_string (len MTRACE_PASS), len,
	     "profile_query_results(): return info");
    string->str->size= profil_query_results (string->str->txt, len,
					     sort, max_entries)-1;
  

    put_string(sp, string);
    return sp;
}
