#include "config.h"
#include "lint.h"
#include "interpret.h"
#include "object.h"
#include "wiz_list.h"
#include "regexp.h"
#include "exec.h"
#include "lang.h"
#include "instrs.h"
#include "stralloc.h"

/*
 * This file contains functions used to manipulate arrays.
 * Some of them are connected to efuns, and some are only used internally
 * by the game driver.
 */

extern int d_flag;

int num_arrays;

/*
 * Make an empty vector for everyone to use, never to be deallocated.
 * It is cheaper to reuse it, than to use malloc() and allocate.
 */
struct null_vector_aggregate_struct null_vector_aggregate = {
VEC_INIT(0 /* size */, 1 /* ref */, T_INVALID)
};

void (*allocate_array_error_handler)PROT(()) = (void(*)PROT(()))error;
/*
 * Allocate an array of size 'n'.
 */
#ifndef allocate_array
struct vector *allocate_array(n)
    mp_int n;
#else
struct vector *_allocate_array(n, file, line)
    mp_int n; int line; char *file;
#endif
{
    extern struct svalue const0;
    mp_int i;
    struct vector *p;
    struct svalue *svp;

    if (n < 0 || n > MAX_ARRAY_SIZE)
	error("Illegal array size: %ld.\n", n);
    if (n == 0) {
        p = &null_vector;
	p->ref++;
	return p;
    }
    num_arrays++;
    p = ALLOC_VECTOR(n, file, line);
    if (!p) {
	(*allocate_array_error_handler)("Out of memory\n");
	return 0;
    }
    p->ref = 1;
#ifndef MALLOC_smalloc
    p->size = n;
#endif
    (p->user = current_object->user)->size_array += n;
    svp = p->item;
    for (i = n; --i >= 0; )
	*svp++ = const0;
    return p;
}

#ifndef allocate_uninit_array
struct vector *allocate_uninit_array(n)
    mp_int n;
#else
struct vector *_allocate_uninit_array(n, file, line)
    mp_int n; int line; char *file;
#endif
{
    struct vector *p;

    if (n < 0 || n > MAX_ARRAY_SIZE)
	error("Illegal array size: %ld.\n", n);
    if (n == 0) {
        p = &null_vector;
	p->ref++;
	return p;
    }
    num_arrays++;
    p = ALLOC_VECTOR(n, file, line);
    if (!p) {
	(*allocate_array_error_handler)("Out of memory\n");
	return 0;
    }
    p->ref = 1;
#ifndef MALLOC_smalloc
    p->size = n;
#endif
    (p->user = current_object->user)->size_array += n;
    return p;
}

void free_vector(p)
    struct vector *p;
{
    mp_int i;
    struct svalue *svp;

    p->ref--;
    if (p->ref > 0)
	return;
#ifdef DEBUG
    if (p == &null_vector)
	fatal("Tried to free the zero-size shared vector.\n");
#endif
    i = VEC_SIZE(p);
    num_arrays--;
    p->user->size_array -= i;
    svp = p->item;
    do {
	free_svalue(svp++);
    } while (--i);
    xfree((char *)p);
}

void free_empty_vector(p)
    struct vector *p;
{
    mp_int i;

    i = VEC_SIZE(p);
    p->user->size_array -= i;
    num_arrays--;
    xfree((char *)p);
}

struct vector *shrink_array(p, n)
    struct vector *p;
    int n;
{
    struct vector *res;

    res = slice_array(p, 0, n-1);
    free_vector(p);
    return res;
}

struct vector *explode_string(str, del)
    char *str, *del;
{
    char *p, *beg;
    int num, len;
    struct vector *ret;
    char *buff;

    len = strlen(del);
    /*
     * Take care of the case where the delimiter is an
     * empty string. Then, return an array with only one element,
     * which is the original string.
     */
    if (len == 0) {
	ret = allocate_array(1);
	ret->item[0].type = T_STRING;
	ret->item[0].x.string_type = STRING_MALLOC;
	ret->item[0].u.string = string_copy(str);
	return ret;
    }
    /*
     * Skip leading 'del' strings, if any.
     */
    while(strncmp(str, del, len) == 0) {
	str += len;
	if (str[0] == '\0')
	    return allocate_array(0);
    }
    /*
     * Find number of occurences of the delimiter 'del'.
     */
    /*
     * Compute number of array items. It is either number of delimiters,
     * or, one more.
     * Amylaar: if we are actually going to do this braindamaged stripping
     * of the last empty string, we should at least keep in mind that
     * explode("###","##") == ("","#"), thus, it needs an array of size 2.
     */
    for (p=str, num=1; *p;) {
	if (strncmp(p, del, len) == 0) {
	    p += len;
	    if (*p)
	        num++;
	} else
	    p += 1;
    }
    buff = xalloc(strlen(str) + 1);
    ret = allocate_array(num);
    for (p=str, beg = str, num=0; *p; ) {
	if (strncmp(p, del, len) == 0) {
	    strncpy(buff, beg, p - beg);
	    buff[p-beg] = '\0';
#if defined(DEBUG) || 0
	    if (num >= VEC_SIZE(ret))
		fatal("Too big index in explode !\n");
#endif
	    /* free_svalue(&ret->item[num]); Not needed for new array */
	    ret->item[num].type = T_STRING;
	    ret->item[num].x.string_type = STRING_MALLOC;
	    ret->item[num].u.string = string_copy(buff);
	    num++;
	    beg = p + len;
	    p = beg;
	} else {
	    p += 1;
	}
    }
    /* Copy last occurence, if there was not a 'del' at the end. */
    if (*beg != '\0') {
#if defined(DEBUG) || 1
	if (num >= VEC_SIZE(ret))
	    fatal("Too big index in explode !\n");
#endif
	/* free_svalue(&ret->item[num]); Not needed */
	ret->item[num].type = T_STRING;
	ret->item[num].x.string_type = STRING_MALLOC;
	ret->item[num].u.string = string_copy(beg);
    }
    xfree(buff);
    return ret;
}

struct vector *new_explode_string(str, del)
    char *str, *del;
{
    char *p, *beg;
    int num, len;
    struct vector *ret;
    char *buff;

    len = strlen(del);
    /*
     * Take care of the case where the delimiter is an
     * empty string. Then, return an array whitch holds all characters as
     * single-character strings.
     */
    if (len <= 1) {
	if (len < 1) {
	    struct svalue *svp;

	    len = strlen(str);
	    ret = allocate_array(len);
	    for( svp = ret->item; --len >= 0; svp++, str++ ) {
		buff = xalloc(2);
		if (!buff) {
		    free_vector(ret);
		    error("Out of memory\n");
		}
		buff[0] = *str;
		buff[1] = '\0';
		svp->type = T_STRING;
		svp->x.string_type = STRING_MALLOC;
		svp->u.string = buff;
	    }
	    return ret;
	} else {
	    char c;
	    struct svalue *svp;

	    c = *del;
	    for (num = 1, p = str; p = strchr(p, c); p++, num++);
	    ret = allocate_array(num);
	    for (svp = ret->item; p = strchr(str, c); str = p + 1, svp++) {
		len = p - str;
		buff = xalloc(len + 1);
		if (!buff) {
		    free_vector(ret);
		    error("Out of memory\n");
		}
		memcpy(buff, str, len);
		buff[len] = '\0';
		svp->type = T_STRING;
		svp->x.string_type = STRING_MALLOC;
		svp->u.string = buff;
	    }
	    svp->type = T_STRING;
	    svp->x.string_type = STRING_MALLOC;
	    if ( !(svp->u.string = string_copy(str)) ) {
		free_vector(ret);
		error("Out of memory\n");
	    }
	    return ret;
	}
    }
    /*
     * Find number of occurences of the delimiter 'del'.
     */
    /*
     * Compute number of array items. It is one more than the number of
     * delimiters.
     */
    for (p=str, num=1; *p;) {
	if (strncmp(p, del, len) == 0) {
	    p += len;
	    num++;
	} else
	    p += 1;
    }
    ret = allocate_array(num);
    for (p=str, beg = str, num=0; *p; ) {
	if (strncmp(p, del, len) == 0) {
#if 1
	    int bufflen;

	    bufflen = p - beg;
	    buff = xalloc(bufflen + 1);
	    if (!buff) {
		free_vector(ret);
		error("Out of memory\n");
	    }
	    memcpy(buff, beg, bufflen);
	    buff[bufflen] = '\0';
#else
	    *p = '\0';
	    buff = make_shared_string(beg);
	    *p = *del;
#endif
	    ret->item[num].type = T_STRING;
	    ret->item[num].x.string_type = STRING_MALLOC;
	    ret->item[num].u.string = buff;
	    num++;
	    beg = p + len;
	    p = beg;
	} else {
	    p += 1;
	}
    }
    /* Copy last occurence */
    if ( !(ret->item[num].u.string = string_copy(beg)) ) {
	free_vector(ret);
	error("Out of memory\n");
    }
    ret->item[num].type = T_STRING;
    ret->item[num].x.string_type = STRING_MALLOC;
    return ret;
}

#ifndef implode_string
char *implode_string(arr, del)
    struct vector *arr;
    char *del;
#else
char *_implode_string(arr, del, file, line)
    struct vector *arr;
    char *del;
    char *file; int line;
#endif
{
    mp_int size, i, del_len, arr_size;
    char *p, *q;
    struct svalue *svp;

    del_len = strlen(del);
    size = -del_len;
    for (i = (arr_size = VEC_SIZE(arr)), svp = arr->item; --i >= 0; svp++)
    {
	if (svp->type == T_STRING) {
	    size += del_len + strlen(svp->u.string);
	} else if (svp->type == T_OBJECT && svp->u.ob->flags & O_DESTRUCTED) {
	    extern void zero_object_svalue PROT((struct svalue *));

	    zero_object_svalue(svp);
	}
    }
#ifndef implode_string
    if (size <= 0)
	return string_copy("");
    p = xalloc(size + 1);
#else
    if (size <= 0)
	return _string_copy("", file, line);
    p = smalloc(size + 1, file, line);
#endif
    if (!p) {
	return 0;
    }
    q = p;
    svp = arr->item;
    for (i = arr_size; svp->type != T_STRING; ) {
	--i;
	svp++;
    }
    strcpy(p, svp->u.string);
    p += strlen(svp->u.string);
    while (--i > 0) {
	svp++;
	if (svp->type == T_STRING) {
	    strcpy(p, del);
	    p += del_len;
	    strcpy(p, svp->u.string);
	    p += strlen(svp->u.string);
	}
    }
    return q;
}

#if 0
/*
 * Check that an assignment to an array item is not cyclic.
 */
static void check_for_recursion(vec, v)
    struct vector *vec, *v;
{
    register int i;
    extern int eval_cost;

    if (vec->user)
	vec->user->cost++;
    eval_cost++;
    if (v == vec)
	error("Recursive asignment of vectors.\n");
    for (i=0; i<v->size; i++) {
	if (v->item[i].type == T_POINTER)
	    check_for_recursion(vec, v->item[i].u.vec);
    }
}
#endif

/*
 * Slice of an array.
 */
struct vector *slice_array(p,from,to)
    struct vector *p;
    int from;
    int to;
{
    struct vector *d;
    int cnt;
    
    if (from < 0)
    	from = 0;
#if 0 /* this test is superflous... */
    if (from >= p->size)
	return allocate_array(0); /* Slice starts above array */
#endif
#if 0 /* moved to caller */
    if (to >= p->size)
	to = p->size-1;
#endif
    if (to < from)
	return allocate_array(0); 
    
    d = allocate_array(to-from+1);
    for (cnt=from;cnt<=to;cnt++) 
	assign_svalue_no_free (&d->item[cnt-from], &p->item[cnt]);
    
    return d;
}

/* EFUN: filter_array
   
   Runs all elements of an array through ob->func()
   and returns an array holding those elements that ob->func
   returned nonzero for.
   */
struct svalue *filter (sp, num_arg)
    struct svalue *sp;
    int num_arg;
{
    extern void zero_object_svalue PROT((struct svalue *));

    extern struct svalue *inter_sp;
    extern struct svalue const0;

    struct svalue *arg;
    struct vector *p;
    char *func;
    struct object *ob;
    int num_extra;
    struct svalue *extra;
    struct vector *vec;
    struct svalue *v, *w;
    char *flags;
    int cnt,res;
    mp_int p_size;
    
    res=0;
    arg = sp - num_arg + 1;
    if (arg->type != T_POINTER)
	bad_efun_vararg(1, sp);
    p = arg->u.vec;
    p_size = VEC_SIZE(p);
    flags = alloca(p_size+1); 
#ifdef MAPPINGS
    if (arg[1].type == T_MAPPING) {
	struct svalue *get_map_lvalue
				PROT((struct mapping*, struct svalue*, int));

	struct mapping *m;

	if (num_arg > 2) {
	    inter_sp = sp;
	    error("Too many arguments to filter_array()\n");
	}
	m = arg[1].u.map;
	for (w = p->item, cnt = p_size; --cnt >= 0; ) {
	    if (w->type == T_OBJECT && w->u.ob->flags & O_DESTRUCTED)
		zero_object_svalue(w);
	    if (get_map_lvalue(m, w++, 0) == &const0) {
		flags[cnt] = 0;
		continue;
	    }
	    flags[cnt] = 1;
	    res++;
	}
    } else
#endif
    {
	assign_eval_cost();
	inter_sp = sp;
	if (arg[1].type == T_CLOSURE) {
	    ob = 0;
	    func = (char *)(arg+1);
	    num_extra = num_arg - 2;
	    extra = arg + 2;
	} else if (arg[1].type == T_STRING) {
	    if (num_arg > 2) {
		num_extra = num_arg - 3;
		if (arg[2].type == T_OBJECT)
		    ob = arg[2].u.ob;
		else if (arg[2].type == T_STRING &&
		    ( ob = find_object(arg[2].u.string) ));
		else bad_efun_vararg(3, sp);
		extra = arg + 3;
	    } else {
		ob = current_object;
		num_extra = 0; /* thus extra needs no initialization */
	    }
	    func = arg[1].u.string;
	} else {
	    bad_efun_vararg(2, sp);
	}
	for (w = p->item, cnt = p_size; --cnt >= 0; ) {
	    flags[cnt] = 0;
	    if (current_object->flags & O_DESTRUCTED)
		continue;
	    if (w->type == T_OBJECT && w->u.ob->flags & O_DESTRUCTED)
		zero_object_svalue(w);
	    push_svalue(w++);
	    push_svalue_block(num_extra, extra);
	    if (ob) {
		if (ob->flags & O_DESTRUCTED)
		    error("object used by filter_array destructed"); /* amylaar */
		v = sapply (func, ob, 1 + num_extra);
		if (!v || v->type == T_NUMBER && !v->u.number)
		    continue;
	    } else {
		call_lambda((struct svalue *)func, 1 + num_extra);
		v = inter_sp--;
		if (v->type == T_NUMBER) {
		    if (!v->u.number)
			continue;
		} else {
		    free_svalue(v);
		}
	    }
	    flags[cnt]=1;
	    res++;
	}
    }
    vec = allocate_array(res);
    if (res) {
	for(v = p->item, w = vec->item, flags = &flags[p_size]; ; v++) {
	    if (*--flags) {
		assign_svalue_no_free (w++, v);
		if (--res <= 0) break;
	    }
	}
    }
    free_vector(p);
    arg->u.vec = vec;
    while(--num_arg)
	free_svalue(sp--);
    return sp;
}

/* Unique maker
   
   These routines takes an array of objects and calls the function 'func'
   in them. The return values are used to decide which of the objects are
   unique. Then an array on the below form are returned:
   
   ({
   ({Same1:1, Same1:2, Same1:3, .... Same1:N }),
   ({Same2:1, Same2:2, Same2:3, .... Same2:N }),
   ({Same3:1, Same3:2, Same3:3, .... Same3:N }),
   ....
   ....
   ({SameM:1, SameM:2, SameM:3, .... SameM:N }),
   })
   i.e an array of arrays consisting of lists of objectpointers
   to all the nonunique objects for each unique set of objects.
   
   The basic purpose of this routine is to speed up the preparing of the
   array used for describing.
   
   */

struct unique
{
    int count;
    struct svalue *val;
    struct svalue mark;
    struct unique *same;
    struct unique *next;
};

static int sameval(arg1,arg2)
    struct svalue *arg1;
    struct svalue *arg2;
{
    if (!arg1 || !arg2) return 0;
    if (arg1->type == T_NUMBER && arg2->type == T_NUMBER) {
	return arg1->u.number == arg2->u.number;
    } else if (arg1->type == T_POINTER && arg2->type == T_POINTER) {
	return arg1->u.vec == arg2->u.vec;
    } else if (arg1->type == T_STRING && arg2->type == T_STRING) {
	return !strcmp(arg1->u.string, arg2->u.string);
    } else if (arg1->type == T_OBJECT && arg2->type == T_OBJECT) {
	return arg1->u.ob == arg2->u.ob;
    } else
	return 0;
}


static int put_in(ulist,marker,elem)
    struct unique **ulist;
    struct svalue *marker;
    struct svalue *elem;
{
    struct unique *llink,*slink,*tlink;
    int cnt,fixed;
    
    llink= *ulist;
    cnt=0; fixed=0;
    while (llink) {
	if ((!fixed) && (sameval(marker,&(llink->mark)))) {
	    for (tlink=llink;tlink->same;tlink=tlink->same) (tlink->count)++;
	    (tlink->count)++;
	    slink = (struct unique *) xalloc(sizeof(struct unique));
	    slink->count = 1;
	    assign_svalue_no_free(&slink->mark,marker);
	    slink->val = elem;
	    slink->same = 0;
	    slink->next = 0;
	    tlink->same = slink;
	    fixed=1; /* We want the size of the list so do not break here */
	}
	llink=llink->next; cnt++;
    }
    if (fixed) return cnt;
    llink = (struct unique *) xalloc(sizeof(struct unique));
    llink->count = 1;
    assign_svalue_no_free(&llink->mark,marker);
    llink->val = elem;
    llink->same = 0;
    llink->next = *ulist;
    *ulist = llink;
    return cnt+1;
}


struct vector *
make_unique(arr,func,skipnum)
    struct vector *arr;
    char *func;
    struct svalue *skipnum;
{
    struct svalue *v;
    struct vector *res,*ret;
    struct unique *head,*nxt,*nxt2;
    
    mp_int cnt,ant,cnt2, arr_size;
    
    head = 0; ant=0; arr->ref++;
    arr_size = VEC_SIZE(arr);
    for(cnt=0;cnt<arr_size;cnt++) if (arr->item[cnt].type == T_OBJECT) {
	v = apply(func,arr->item[cnt].u.ob,0);
	if ((!v) || (v->type!=skipnum->type) || !sameval(v,skipnum)) 
	    if (v) ant=put_in(&head,v,&(arr->item[cnt]));
    }
    arr->ref--;
    ret = allocate_array(ant);
    
    for (cnt=ant-1;cnt>=0;cnt--) { /* Reverse to compensate put_in */
	ret->item[cnt].type = T_POINTER;
	ret->item[cnt].u.vec = res = allocate_array(head->count);
	nxt2 = head;
	head = head->next;
	cnt2 = 0;
	while (nxt2) {
	    assign_svalue_no_free (&res->item[cnt2++], nxt2->val);
	    free_svalue(&nxt2->mark);
	    nxt = nxt2->same;
	    xfree((char *) nxt2);
	    nxt2 = nxt;
	}
	if (!head) 
	    break; /* It shouldn't but, to avoid skydive just in case */
    }
    return ret;
}

/*
 * End of Unique maker
 *************************
 */

/* Concatenation of two arrays into one
 */
struct vector *add_array(p,q)
    struct vector *p, *q;
{
    mp_int cnt;
    struct svalue *s, *d;
    mp_int q_size;
    
    s = p->item;
    p = allocate_array((cnt = VEC_SIZE(p)) + (q_size = VEC_SIZE(q)));
    d = p->item;
    for ( ; --cnt >= 0; ) {
	assign_svalue_no_free (d++, s++);
    }
    s = q->item;
    for (cnt = q_size; --cnt >= 0; ) {
	assign_svalue_no_free (d++, s++);
    }
    return p;
}

struct vector *subtract_array_tmp_vec;

struct vector *subtract_array(minuend, subtrahend)
    struct vector *minuend, *subtrahend;
{
    struct vector *vtmpp;
    static struct svalue ltmp = { T_POINTER };
    struct vector *difference;
    struct svalue *source,*dest;
    mp_int i, minuend_size;

    ltmp.u.vec = subtrahend;
    vtmpp = order_alist(&ltmp, 1, 1);
    free_vector(subtrahend);
    subtrahend = vtmpp->item[0].u.vec;
    difference = allocate_array(minuend_size = VEC_SIZE(minuend));
    for (source = minuend->item, dest = difference->item, i = minuend_size;
      i--; source++) {
        extern struct svalue const0;

        int assoc PROT((struct svalue *key, struct vector *));
	if (source->type == T_OBJECT && source->u.ob->flags & O_DESTRUCTED)
	    assign_svalue(source, &const0);
	if ( assoc(source, subtrahend) < 0 )
	    assign_svalue_no_free(dest++, source);
    }
    subtract_array_tmp_vec = vtmpp;
    return shrink_array(difference, dest-difference->item);
}


/* Returns an array of all objects contained in 'ob'
 */
struct vector *all_inventory(ob)
    struct object *ob;
{
    struct vector *d;
    struct object *cur;
    int cnt,res;
    
    cnt=0;
    for (cur=ob->contains; cur; cur = cur->next_inv)
	cnt++;
    
    if (!cnt)
	return allocate_array(0);

    d = allocate_array(cnt);
    cur=ob->contains;
    
    for (res=0;res<cnt;res++) {
	d->item[res].type=T_OBJECT;
	d->item[res].u.ob = cur;
	add_ref(cur,"all_inventory");
	cur=cur->next_inv;
    }
    return d;
}


/* Runs all elements of an array through ob::func
   and replaces each value in arr by the value returned by ob::func
   */
void map_array (arr, func, ob, num_extra, extra)
    struct vector *arr;
    char *func;
    struct object *ob;
    int num_extra;
    struct svalue *extra;
{
    extern struct svalue *inter_sp;

    struct vector *r;
    struct svalue *v, *w, *x;
    mp_int cnt;
    
    r = allocate_array(cnt = VEC_SIZE(arr));
    if (!r)
	error("Out of memory\n");
    push_referenced_vector(r);
    
    w = arr->item;
    x = r->item;
    for (; --cnt >= 0; w++, x++) {
	if (current_object->flags & O_DESTRUCTED)
	    continue;
	push_svalue(w);
	push_svalue_block(num_extra, extra);
	if (ob) {
	    if (ob->flags & O_DESTRUCTED)
		error("object used by map_array destructed"); /* amylaar */
	    v = sapply (func, ob, 1 + num_extra);
	    if (v) {
		transfer_svalue_no_free (x, v);
		v->type = T_INVALID;
	    }
	} else {
	    call_lambda((struct svalue *)func, 1 + num_extra);
	    transfer_svalue_no_free (x, inter_sp--);
	}
    }
}

static INLINE int sort_array_cmp(func, ob, p1, p2)
    char *func;
    struct object *ob;
    struct svalue *p1,*p2;
{
    struct svalue *d;

    if (ob->flags & O_DESTRUCTED)
        error("object used by sort_array destructed");
    push_svalue(p1);
    push_svalue(p2);
    d = sapply(func, ob, 2);
    if (!d) return 0;
    if (d->type != T_NUMBER) {
	/* value will be freed at next call of apply() */
	return 1;
    }
    return d->u.number > 0;
}

static INLINE int sort_array_lambda_cmp(func, p1, p2)
    char *func;
    struct svalue *p1,*p2;
{
    extern struct svalue *inter_sp;

    struct svalue *d;

    push_svalue(p1);
    push_svalue(p2);
    call_lambda((struct svalue *)func, 2);
    d = inter_sp--;
    if (d->type != T_NUMBER) {
	free_svalue(d);
	return 1;
    }
    return d->u.number > 0;
}


struct vector *sort_array(data, func, ob)
    struct vector *data;
    char *func;
    struct object *ob;
{
  int step,halfstep,size;
  int i,j,index1,index2,end1,end2;
  struct svalue *source,*dest,*temp;
  size = VEC_SIZE(data);
  temp = data -> item;
  for (i=0;i<size;i++)
  {
    extern struct svalue const0;
    if (temp[i].type == T_OBJECT && temp[i].u.ob->flags & O_DESTRUCTED)
      assign_svalue(&temp[i],&const0);
  }
  if (size<=1)
    return data;
  /* In order to provide clean error recovery, data must always hold
     exactly one copy of each original content svalue when an error is
     possible. Thus, it would be not a good idea to use it as scrap
     space.
   */
  source = (struct svalue *)alloca(size*sizeof(struct svalue));
  dest = (struct svalue *)alloca(size*sizeof(struct svalue));
  push_referenced_vector(data);
  for (i=0;i<size;i++)
    source[i]=temp[i];
  step = 2;
  halfstep = 1;
  while (halfstep<size)
  {
    for (i=j=0;i<size;i+=step)
    {
      index1 = i;
      index2 = i + halfstep;
      end1 = index2;
      if (end1 > size)
	end1 = size;
      end2 = i + step;
      if (end2 > size)
        end2 = size;
      if (ob) {
        while (index1<end1 && index2<end2)
        {
          if (sort_array_cmp(func,ob,source+index1,source+index2))
            dest[j++]=source[index2++];
          else
            dest[j++]=source[index1++];
        }
      } else {
        while (index1<end1 && index2<end2)
        {
          if (sort_array_lambda_cmp(func,source+index1,source+index2))
            dest[j++]=source[index2++];
          else
            dest[j++]=source[index1++];
        }
      }
      if (index1==end1)
      {
        while (index2<end2)
          dest[j++]=source[index2++];
      }
      else
      {
        while (index1<end1)
          dest[j++]=source[index1++];
      }
    }
    halfstep = step;
    step += step;
    temp = source;
    source = dest;
    dest = temp;
  }
  temp = data->item;
  for (i=size; --i >= 0; )
    temp[i]=source[i];
  drop_stack();
  return data;
}

#if 0 /* Not used anywhere... */
/* Turns a structured array of elements into a flat array of elements.
   */
static int num_elems(arr)
    struct vector *arr;
{
    int cnt,il;

    cnt = arr->size;

    for (il=0;il<arr->size;il++) 
	if (arr->item[il].type == T_POINTER) 
	    cnt += num_elems(arr->item[il].u.vec) - 1;
    return cnt;
}

struct vector *flatten_array(arr)
    struct vector *arr;
{
    int max, cnt, il, il2;
    struct vector *res, *dres;
    
    if (arr->size < 1) 
	return allocate_array(0);

    max = num_elems(arr);

    if (arr->size == max)
	return arr;

    if (max == 0) 	    /* In the case arr is an array of empty arrays */
	return allocate_array(0);

    res = allocate_array(max); 

    for (cnt = 0 , il = 0; il < arr->size; il++)
	if (arr->item[il].type != T_POINTER) 
	    assign_svalue(&res->item[cnt++],&arr->item[il]);
	else {
	    dres = flatten_array(arr->item[il].u.vec);
	    for (il2=0;il2<dres->size;il2++)
		assign_svalue(&res->item[cnt++],&dres->item[il2]);
	    free_vector(dres);
	}
    
    return res;
}
#endif

/*
 * deep_inventory()
 *
 * This function returns the recursive inventory of an object. The returned 
 * array of objects is flat, ie there is no structure reflecting the 
 * internal containment relations.
 *
 */
static int deep_inventory_size(ob)
    struct object	*ob;
{
    int n;

    n = 0;
    do {
	if (ob->contains)
	    n += deep_inventory_size(ob->contains);
	n++;
    } while (ob = ob->next_inv);
    return n;
}

static struct svalue *write_deep_inventory(first, svp)
    struct object *first;
    struct svalue *svp;
{
    struct object *ob;

    ob = first;
    do {
	svp->type = T_OBJECT;
	add_ref( (svp->u.ob = ob), "deep_inventory");
	svp++;
    } while (ob = ob->next_inv);
    ob = first;
    do {
	if (ob->contains)
	    svp = write_deep_inventory(ob->contains, svp);
    } while (ob = ob->next_inv);
    return svp;
}

struct vector *deep_inventory(ob, take_top)
    struct object	*ob;
    int			take_top; /* only 0 and 1 are valid arguments */
{
    struct vector *dinv;
    struct svalue *svp;
    int n;

    n = take_top;
    if (ob->contains) {
	n += deep_inventory_size(ob->contains);
    }
    dinv = allocate_array(n);
    svp = dinv->item;
    if (take_top) {
	svp->type = T_OBJECT;
	add_ref( (svp->u.ob = ob), "deep_inventory");
	svp++;
    }
    if (ob->contains) {
	write_deep_inventory(ob->contains, svp);
    }
    return dinv;
}

#if 0 /* Not used anywhere... */
/* sum_array, processes each element in the array together with the
              results from the previous element. Starting value is 0.
	      Note! This routine allocates a struct svalue which it returns.
	      This value should be pushed to the stack and then freed.
   */
struct svalue *sum_array (arr, func, ob, extra)
    struct vector *arr;
    char *func;
    struct object *ob;
    struct svalue *extra;
{
    struct svalue *ret, v;

    int cnt;

    v.type = T_NUMBER;
    v.u.number = 0;

    for (cnt=0;cnt<arr->size;cnt++) {
	push_svalue(&arr->item[cnt]);
	push_svalue(&v);
	if (extra) {
	    push_svalue (extra);
	    ret = apply(func, ob, 3);
	} else {
	    ret = apply(func,ob,2);
	}
	if (ret)
	    v = *ret;
    }

    ret = (struct svalue *) xalloc(sizeof(struct svalue));
    *ret = v;

    return ret;
}
#endif


static INLINE int alist_cmp(p1, p2)
    struct svalue *p1,*p2;
{
    register int d;

    if (d = (p1->u.number >> 1) - (p2->u.number >> 1)) return d;
    if (d = p1->u.number - p2->u.number) return d;
    if (d = p1->type - p2->type) return d;
    switch (p1->type) {
      case T_FLOAT:
      case T_CLOSURE:
      case T_SYMBOL:
      case T_QUOTED_ARRAY:
	if (d = p1->x.generic - p2->x.generic) return d;
    }
    return 0;
}

#if 0
struct vector *order_alist(inlist)
    struct vector *inlist;
{
    struct vector *outlist;
    struct svalue *inlists, *outlists, *root, *inpnt, *insval;
    int listnum, keynum, i, j;

    listnum = inlist->size;
    inlists = inlist->item;
    keynum = inlists[0].u.vec->size;
    root = inlists[0].u.vec->item;
    /* transform inlists[i].u.vec->item[j] into a heap, starting at the top */
    insval = (struct svalue*)xalloc(sizeof(struct svalue[1])*listnum);
    for(j=0,inpnt=root; j<keynum; j++,inpnt++) {
	int curix, parix;

	/* make sure that strings can be compared by their pointer */
	if (inpnt->type == T_STRING && inpnt->x.string_type != STRING_SHARED) {
	    char *str = make_shared_string(inpnt->u.string);
	    free_svalue(inpnt);
	    inpnt->type = T_STRING;
	    inpnt->x.string_type = STRING_SHARED;
	    inpnt->u.string = str;
	}
	/* propagate the new element up in the heap as much as necessary */
	for (i=0; i<listnum; i++) {
	    insval[i] = inlists[i].u.vec->item[j];
	    /* but first ensure that it contains no destructed object */
	    if (insval[i].type == T_OBJECT
	      && insval[i].u.ob->flags & O_DESTRUCTED) {
                extern struct svalue const0;

		free_object(insval[i].u.ob, "order_alist");
	        inlists[i].u.vec->item[j] = insval[i] = const0;
	    }
	}
	for(curix = j; curix; curix=parix) {
	    parix = (curix-1)>>1;
	    if ( alist_cmp(&root[parix], &root[curix]) > 0 ) {
		for (i=0; i<listnum; i++) {
		    inlists[i].u.vec->item[curix] =
		      inlists[i].u.vec->item[parix];
		    inlists[i].u.vec->item[parix] = insval[i];
		}
	    }
	}
    }
    xfree((char*)insval);
    outlist = allocate_array(listnum);
    outlists = outlist->item;
    for (i=0; i<listnum; i++) {
	outlists[i].type  = T_POINTER;
	outlists[i].u.vec = allocate_array(keynum);
    }
    for(j=0; j<keynum; j++) {
	int curix;

	for (i=0;  i<listnum; i++) {
	    outlists[i].u.vec->item[j] = inlists[i].u.vec->item[0];
	}
	for (curix=0; ; ) {
	    int child, child2;

	    child = curix+curix+1;
	    child2 = child+1;
	    if (child2<keynum && root[child2].type != T_INVALID
	      && (root[child].type == T_INVALID ||
		alist_cmp(&root[child], &root[child2]) > 0))
	    {
		child = child2;
	    }
	    if (child<keynum && root[child].type != T_INVALID) {
		for (i=0; i<listnum; i++) {
		    inlists[i].u.vec->item[curix] =
		      inlists[i].u.vec->item[child];
		}
		curix = child;
	    } else break;
	}
	for (i=0; i<listnum; i++) {
	    inlists[i].u.vec->item[curix].type = T_INVALID;
	}
    }
    return outlist;
}
#endif

struct vector *order_alist(inlists, listnum, reuse)
    struct svalue *inlists;
    int listnum;
    int reuse;
{
    extern void free_object_svalue PROT((struct svalue *));

    struct vector *outlist, *v;
    struct svalue *outlists, **root, **root2, *inpnt;
    int keynum, i, j;

    keynum = VEC_SIZE(inlists[0].u.vec);
    root = (struct svalue **)alloca(keynum * sizeof(struct svalue *[2]));
    root--;
    /* transform inlists[i].u.vec->item[j] into a heap, starting at the top */
    for(j=1,inpnt=inlists->u.vec->item; j<=keynum; j++,inpnt++) {
	int curix, parix;

	/* make sure that strings can be compared by their pointer */
	if (inpnt->type == T_STRING) {
	    if (inpnt->x.string_type != STRING_SHARED) {
		char *str = make_shared_string(inpnt->u.string);
		free_string_svalue(inpnt);
		inpnt->x.string_type = STRING_SHARED;
		inpnt->u.string = str;
	    }
	} else if (inpnt->type == T_OBJECT) {
	    if (inpnt->u.ob->flags & O_DESTRUCTED) {
		free_object_svalue(inpnt);
		inpnt->type = T_NUMBER;
		inpnt->u.number = 0;
	    }
	}
	/* propagate the new element up in the heap as much as necessary */
	for(curix = j; parix = curix>>1; ) {
	    if ( alist_cmp(root[parix], inpnt) > 0 ) {
		root[curix] = root[parix];
		curix = parix;
	    } else {
		break;
	    }
	}
	root[curix] = inpnt;
    }
    root++;
    root2 = &root[keynum];
    for(j = keynum; --j >= 0; ) {
	int curix;

	*root2++ = *root;
	for (curix=0; ; ) {
	    int child, child2;

	    child = curix+curix+1;
	    child2 = child+1;
	    if (child2 >= keynum) {
		if (child2 == keynum && root[child]) {
		    root[curix] = root[child];
		    curix = child;
		}
		break;
	    }
	    if (root[child2]) {
		if (!root[child] || alist_cmp(root[child], root[child2]) > 0)
		{
		    root[curix] = root[child2];
		    curix = child2;
		    continue;
		}
	    } else if (!root[child]) {
		break;
	    }
	    root[curix] = root[child];
	    curix = child;
	}
	root[curix] = 0;
    }
    root = &root[keynum];
    outlist = allocate_array(listnum);
    outlists = outlist->item;
    v = allocate_array(keynum);
    for (i = listnum; --i >= 0; ) {
	int offs;
	struct svalue *outpnt;

	outlists[i].type  = T_POINTER;
	outpnt = ( outlists[i].u.vec = v )->item;
	v = inlists[i].u.vec;
	offs = (char *)v - (char *)inlists[0].u.vec;
	if (inlists[i].u.vec->ref == reuse) {
	    if (i)
		inlists[i].type = T_INVALID;
	    for (root2 = root, j = keynum; --j >= 0; ) {
		inpnt = (struct svalue *)((char *)*root2++ + offs);
		if (inpnt->type == T_OBJECT &&
		    inpnt->u.ob->flags & O_DESTRUCTED)
		{
		    free_object_svalue(inpnt);
		    outpnt->type = T_NUMBER;
		    outpnt->u.number = 0;
		    outpnt++;
		} else {
		    *outpnt++ = *inpnt;
		}
		inpnt->type = T_INVALID;
	    }
	} else {
	    if (i)
		v = allocate_array(keynum);
	    for (root2 = root, j = keynum; --j >= 0; ) {
		inpnt = (struct svalue *)((char *)*root2++ + offs);
		if (inpnt->type == T_OBJECT &&
		    inpnt->u.ob->flags & O_DESTRUCTED)
		{
		    outpnt->type = T_NUMBER;
		    outpnt->u.number = 0;
		    outpnt++;
		} else {
		    assign_svalue_no_free(outpnt++, inpnt);
		}
	    }
	}
    }
    return outlist;
}

int search_alist(key, keylist)
    struct svalue *key;
    struct vector *keylist;
{
    mp_int i, o, d, keynum;

    if ( !(keynum = VEC_SIZE(keylist)) ) return 0;
    i = keynum >> 1;
    o = (i+2) >> 1;
    for (;;) {
	d = alist_cmp(key, &keylist->item[i]);
	if (d<0) {
	    i -= o;
	    if (i<0) {
		i = 0;
	    }
	} else if (d>0) {
	    i += o;
	    if (i >= keynum) {
	        i = keynum-1;
	    }
	} else {
	    return i;
	}
	if (o<=1) {
	    if (alist_cmp(key, &keylist->item[i]) > 0) return i+1;
	    return i;
	}
	o = (o+1) >> 1;
    }
}


char *last_insert_alist_shared_string = 0;
	/* gcollect.c needs access to this */

struct svalue *insert_alist(key, key_data, list)
    struct svalue *key, *key_data;
    struct vector *list;
{
    static struct svalue stmp;
    mp_int i,j,ix;
    mp_int keynum, list_size;
    int new_member;

    if (key->type == T_STRING && key->x.string_type != STRING_SHARED) {
	char *tmpstr;

	if (last_insert_alist_shared_string)
	    free_string(last_insert_alist_shared_string);
	tmpstr = make_shared_string(key->u.string);
	if (key->x.string_type == STRING_MALLOC)
	    xfree(key->u.string);
	key->x.string_type = STRING_SHARED;
	key->u.string = tmpstr;
	increment_string_ref(tmpstr);
	last_insert_alist_shared_string = tmpstr;
    }
    keynum = VEC_SIZE(list->item[0].u.vec);
    ix = search_alist(key, list->item[0].u.vec);
    if (key_data == 0) {
	 stmp.type = T_NUMBER;
	 stmp.u.number = ix;
         return &stmp;
    }
    stmp.type = T_POINTER;
    stmp.u.vec = allocate_array(list_size = VEC_SIZE(list));
    new_member = ix == keynum || alist_cmp(key, &list->item[0].u.vec->item[ix]);
    for (i=0; i < list_size; i++) {
	struct vector *vtmp;

        if (new_member) {
            struct svalue *pstmp = list->item[i].u.vec->item;
    
	    vtmp = allocate_array(keynum+1);
            for (j=0; j < ix; j++) {
               assign_svalue_no_free(&vtmp->item[j], pstmp++);
            }
            assign_svalue_no_free(&vtmp->item[ix], i ? &key_data[i] : key );
            for (j=ix+1; j <= keynum; j++) {
               assign_svalue_no_free(&vtmp->item[j], pstmp++);
            }
	} else {
	    vtmp = slice_array(list->item[i].u.vec, 0, keynum-1);
	    if (i)
	        assign_svalue(&vtmp->item[ix], &key_data[i]);
	}
	stmp.u.vec->item[i].type=T_POINTER;
	stmp.u.vec->item[i].u.vec=vtmp;
    }
    return &stmp;
}


struct svalue assoc_shared_string_key;

int assoc(key, list)
    struct svalue *key;
    struct vector *list;
{
    int i;
    extern char* findstring PROT((char*));

    if (key->type == T_STRING && key->x.string_type != STRING_SHARED) {

	if ( !(assoc_shared_string_key.u.string = findstring(key->u.string)) )
	    return -1;
	key = &assoc_shared_string_key;
    }
    i = search_alist(key, list);
    if (i == VEC_SIZE(list) || alist_cmp(key, &list->item[i]))
        i = -1;
    return i;
}

struct vector *intersect_alist(a1, a2)
    struct vector *a1,*a2;
{
    struct vector *a3;
    int d, l, i1, i2, a1s, a2s;

    a1s = VEC_SIZE(a1);
    a2s = VEC_SIZE(a2);
    a3 = allocate_array( a1s < a2s ? a1s : a2s);
    for (i1=i2=l=0; i1 < a1s && i2 < a2s; ) {
	d = alist_cmp(&a1->item[i1], &a2->item[i2]);
	if (d<0)
	    i1++;
	else if (d>0)
	    i2++;
	else {
	    assign_svalue_no_free(&a3->item[l++], &a2->item[(i1++,i2++)] );
	}
    }
    return shrink_array(a3, l);
}

struct vector *symmetric_difference_alist(a1, a2)
    struct vector *a1,*a2;
{
    struct vector *a3;
    int d, l, i1, i2, a1s, a2s;

    a1s = VEC_SIZE(a1);
    a2s = VEC_SIZE(a2);
    a3 = allocate_array( a1s + a2s );
    for (i1=i2=l=0; i1 < a1s && i2 < a2s; ) {
	d = alist_cmp(&a1->item[i1], &a2->item[i2]);
	if (d<0)
	    assign_svalue_no_free(&a3->item[l++], &a1->item[i1++]);
	else if (d>0)
	    assign_svalue_no_free(&a3->item[l++], &a2->item[i2++]);
	else {
	    i1++;
	    i2++;
	}
    }
    while (i1 < a1s )
	    assign_svalue_no_free(&a3->item[l++], &a1->item[i1++]);
    while (i2 < a2s )
	    assign_svalue_no_free(&a3->item[l++], &a2->item[i2++]);
    return shrink_array(a3, l);
}

/* Determine if an array satisfies the conditions for being an alist.
 * Note that an ordinary array can do this by chance.
 * Alists must not have their members swapped, because the order of the
 * pointers is unreproducible.
 */
int is_alist(v)
    struct vector *v;
{
    struct svalue *svp;
    mp_int i;

    for (svp = v->item, i = VEC_SIZE(v); --i > 0; svp++) {
	if (svp->type == T_STRING && svp->x.string_type != STRING_SHARED)
	    return 0;
	if (alist_cmp(svp, svp+1) > 0)
	    return 0;
    }
    if (svp->type == T_STRING && svp->x.string_type != STRING_SHARED)
	return 0;
    return 1;
}

struct vector *intersect_array(a1, a2)
    struct vector *a1,*a2;
{
    struct vector *vtmpp1,*vtmpp2,*vtmpp3;
    static struct svalue ltmp = { T_POINTER };

    ltmp.u.vec = a1;
    vtmpp1 = order_alist(&ltmp, 1, 1);
    free_vector(ltmp.u.vec);
    ltmp.u.vec = a2;
    vtmpp2 = order_alist(&ltmp, 1, 1);
    free_vector(ltmp.u.vec);
    vtmpp3 = intersect_alist(vtmpp1->item[0].u.vec, vtmpp2->item[0].u.vec);
    free_vector(vtmpp1);
    free_vector(vtmpp2);
    return vtmpp3;
}

struct vector *match_regexp(v, pattern)
    struct vector *v;
    char *pattern;
{
    struct regexp *reg;
    char *res;
    mp_int i, num_match, v_size;
    struct vector *ret;
    extern int eval_cost;

    if ((v_size = VEC_SIZE(v)) == 0)
	return allocate_array(0);
    reg = regcomp(pattern, 0);
    if (reg == 0)
	return 0;
    res = (char *)alloca(v_size);
    for (num_match=i=0; i < v_size; i++) {
	char *line;

	res[i] = 0;
	if (v->item[i].type != T_STRING)
	    continue;
	eval_cost++;
	line = v->item[i].u.string;
	if (regexec(reg, line, line) == 0)
	    continue;
	res[i] = 1;
	num_match++;
    }
    ret = allocate_array(num_match);
    for (num_match=i=0; i < v_size; i++) {
	if (res[i] == 0)
	    continue;
	assign_svalue_no_free(&ret->item[num_match], &v->item[i]);
	num_match++;
    }
    xfree((char *)reg);
    return ret;
}

struct svalue *f_regexplode(sp)
    struct svalue *sp;
{
    extern int eval_cost;
    extern struct svalue *inter_sp;

    struct regexplode_match {
	char *start, *end;
	struct regexplode_match *next;
    };

    char *pattern;
    struct regexp *reg;
    struct regexplode_match *matches, **matchp, *match;
    char *text, *str;
    struct svalue *svp;
    int num_match;
    struct vector *ret;

    if (sp[-1].type != T_STRING)
	bad_xefun_arg(1, sp);
    if (sp->type != T_STRING)
	bad_xefun_arg(2, sp);
    text = sp[-1].u.string;
    pattern = sp->u.string;
    reg = regcomp(pattern, 0);
    if (reg == 0) {
	inter_sp = sp;
	error("Unrecognized search pattern");
    }
    num_match = 0;
    matchp = &matches;
    str = text;
    while (regexec(reg, str, text)) {
	eval_cost++;
	match = (struct regexplode_match *)alloca(sizeof *match);
	match->start = reg->startp[0];
	match->end = str = reg->endp[0];
	*matchp = match;
	matchp = &match->next;
	num_match++;
	if (!*str || (match->start == str && !*++str) )
	    break;
    }
    *matchp = 0;
    if (num_match > (MAX_ARRAY_SIZE-1 >> 1) ) {
	xfree((char *)reg);
	inter_sp = sp;
	error("Illegal array size");
    }
    ret = allocate_array((num_match << 1) + 1);
    svp = ret->item;
    match = matches;
    while (match) {
	mp_int len;

	len = match->start - text;
	str = xalloc(len + 1);
	strncpy(str, text, len);
	str[len] = 0;
	text += len;
	svp->type = T_STRING;
	svp->x.string_type = STRING_MALLOC;
	svp->u.string = str;
	svp++;
	len = match->end - text;
	str = xalloc(len + 1);
	strncpy(str, text, len);
	str[len] = 0;
	text += len;
	svp->type = T_STRING;
	svp->x.string_type = STRING_MALLOC;
	svp->u.string = str;
	svp++;
	match = match->next;
    }
    xfree((char *)reg);
    svp->type = T_STRING;
    svp->x.string_type = STRING_MALLOC;
    svp->u.string = string_copy(text);
    free_string_svalue(sp);
    sp--;
    free_string_svalue(sp);
    sp->type = T_POINTER;
    sp->u.vec = ret;
    return sp;
}

#ifdef F_INHERIT_LIST
/*
 * Returns a list of all inherited files.
 *
 * Must be fixed so that any number of files can be returned, now max 256
 * (Sounds like a contradiction to me /Lars).
 */
struct svalue *f_inherit_list(sp)
    struct svalue *sp;
{
    extern void free_object_svalue PROT((struct svalue *));

    extern struct svalue *inter_sp;

    struct object *ob;
    struct vector *vec;
    struct program *pr, **prp, *plist[256];
    int next, cur, cnt;
    struct svalue *svp;

    if (sp->type != T_OBJECT)
	bad_xefun_arg(1, sp);
    ob = sp->u.ob;
    inter_sp = sp; /* three possibilities for out of memory */
    if (O_PROG_SWAPPED(ob))
	if (load_ob_from_swap(ob) < 0)
	    error("Out of memory\n");
    plist[0] = ob->prog;
    next = 1;
    for (cur = 0; cur < next; cur++)
    {
	pr = plist[cur];
	cnt = pr->num_inherited;
	if (next + cnt > sizeof plist/sizeof *plist)
	    break;
	prp = &pr->inherit[0].prog;
	while(--cnt >= 0) {
	    plist[next++] = *prp;
	    prp = (struct program **)((char *)prp + sizeof(struct inherit));
	}
    }
	    
    vec = allocate_array(next);

    for (svp = vec->item, prp = plist; --next >= 0; svp++) {
	char *str;

	pr = *prp++;
	if ( !(str = make_shared_string(pr->name)) ) {
	    free_vector(vec);
	    error("Out of memory\n");
	}
	svp->type = T_STRING;
	svp->x.string_type = STRING_SHARED;
	svp->u.string = str;
    }
    free_object_svalue(sp);
    sp->type = T_POINTER;
    sp->u.vec = vec;
    return sp;
}
#endif /* F_INHERIT_LIST */

/* EFUN: filter_objects
   
   Runs func in all objects in an array and returns an array holding
   those elements that func returned non-zero for.
   */
struct svalue *f_filter_objects (sp, num_arg)
    struct svalue *sp;
    int num_arg;
{
    extern struct svalue *inter_sp;

    struct vector *p;
    char *func;
    struct svalue *arguments;
    struct vector *w;
    struct svalue *v;
    char *flags;
    int cnt,res;
    struct object *ob;
    mp_int p_size;
    
    assign_eval_cost();
    inter_sp = sp; /* needed for errors in allocate_array(), apply() */
    arguments = sp-num_arg+3;
    if (arguments[-2].type != T_POINTER)
	bad_xefun_vararg(1, sp);
    if (arguments[-1].type != T_STRING)
	bad_xefun_vararg(2, sp);
    p = arguments[-2].u.vec;
    func = arguments[-1].u.string;
    num_arg -= 2;
    p_size = VEC_SIZE(p);
    res=0;
    switch(arguments[-1].x.string_type) {
      default:
	if ( !(func = findstring(func)) )
	    break;
      case STRING_SHARED:
	flags=alloca(p_size+1); 
	for (cnt=0; cnt < p_size; cnt++) {
	    flags[cnt]=0;
	    v = &p->item[cnt];
	    if (v->type != T_OBJECT) {
		if (v->type != T_STRING)
		    continue;
		if ( !(ob = find_object(v->u.string)) )
		    continue;
	    } else {
		extern struct svalue const0;

		ob = v->u.ob;
		if (ob->flags & O_DESTRUCTED) {
		    assign_svalue(v, &const0);
		    continue;
		}
	    }
	    if (current_object->flags & O_DESTRUCTED)
		continue;
	    push_svalue_block(num_arg, arguments);
	    v = sapply (func, ob, num_arg);
	    if ((v) && (v->type!=T_NUMBER || v->u.number) ) {
		flags[cnt]=1;
		res++;
	    }
	}
    }
    w = allocate_array(res);
    if (res) {
	v = &w->item[res];
	for (;;) {
	    if (flags[--cnt]) {
		struct svalue sv;

		*--v = sv = p->item[cnt];
		if (sv.type == T_STRING) {
		    if (sv.x.string_type == STRING_MALLOC) {
			if ( !(v->u.string = string_copy(sv.u.string)) ) {
			    v->type = T_INVALID;
			    free_vector(w);
			    error("Out of memory\n");
			}
		    } else {
			increment_string_ref(sv.u.string);
		    }
		} else {
		    add_ref(sv.u.ob, "filter");
		}
		if (v == w->item)
		    break;
	    }
	}
    }
    free_vector(p);
    do {
	free_svalue(sp--);
    } while(--num_arg >= 0);
    sp->u.vec = w;
    return sp;
}

/* EFUN: map_objects
   
   Runs func in all objects in an array and returns an array holding
   the results.
   */
struct svalue *f_map_objects (sp, num_arg)
    struct svalue *sp;
    int num_arg;
{
    extern struct svalue *inter_sp;
    struct vector *p;
    char *func;
    struct svalue *arguments;
    struct vector *r;
    struct svalue *v, *w, *x;
    int cnt;
    struct object *ob;
    int size;
    
    assign_eval_cost();
    inter_sp = sp;
    arguments = sp-num_arg+3;
    if (arguments[-2].type != T_POINTER)
	bad_xefun_vararg(1, sp);
    if (arguments[-1].type != T_STRING)
	bad_xefun_vararg(2, sp);
    p = arguments[-2].u.vec;
    func = arguments[-1].u.string;
    num_arg -= 2;
    r = allocate_array(size = VEC_SIZE(p));
    arguments[-2].u.vec = r;
    push_referenced_vector(p);
    switch(arguments[-1].x.string_type) {
      default:
	if ( !(func = findstring(func)) )
	    break;
      case STRING_SHARED:
	for (cnt = size, v = p->item, x = r->item; --cnt >= 0; v++, x++) {
	    if (v->type != T_OBJECT) {
		if (v->type != T_STRING)
		    continue;
		if ( !(ob = find_object(v->u.string)) )
		    continue;
	    } else {
		ob = v->u.ob;
		if (ob->flags & O_DESTRUCTED) {
		    extern struct svalue const0;
    
		    assign_svalue(v, &const0);
		    continue;
		}
	    }
	    if (current_object->flags & O_DESTRUCTED)
		continue;
	    push_svalue_block(num_arg, arguments);
	    w = sapply (func, ob, num_arg);
	    if (w) {
		*x = *w;
		w->type = T_INVALID;
	    }
	}
    }
    do {
	free_svalue(sp--);
    } while(--num_arg >= 0);
    free_vector(p);
    return sp;
}

struct svalue *f_functionlist(sp)
    struct svalue *sp;
{
    extern struct svalue *inter_sp;

    struct object *ob;
    struct program *prog, *defprog;
    int i, j;
    unsigned short *ixp;
    char *vis_tags;
    struct vector *list;
    uint32 *fun, flags, active_flags;
    struct svalue *svp;
    mp_int mode_flags;
    mp_int num_functions;

    inter_sp = sp;
    if (sp[-1].type != T_OBJECT) {
	if (sp[-1].type != T_STRING || !(ob = find_object2(sp[-1].u.string)))
	    bad_xefun_arg(1, sp);
    } else
	ob = sp[-1].u.ob;
    if (sp->type != T_NUMBER)
	bad_xefun_arg(2, sp);
    mode_flags = sp->u.number;
    if (O_PROG_SWAPPED(ob))
	if (load_ob_from_swap(ob) < 0)
	    error("Out of memory\n");
    prog = ob->prog;
    num_functions = prog->num_functions;
    vis_tags = alloca(num_functions);
    memset(
      vis_tags,
      mode_flags &
      (NAME_HIDDEN|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|
       NAME_INHERITED) ?
	'\0' :
	'\2'  ,
      num_functions
    );
    flags = mode_flags &
	(TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|NAME_INHERITED);
    fun = prog->functions;
    num_functions = 0;
    j = prog->num_function_names;
    for (ixp = prog->function_names + j; --j >= 0; ) {
	i = *--ixp;
	if ( !(fun[i] & flags) ) {
	    vis_tags[i] = '\1';
	    num_functions++;
	}
    }
    if ( !(mode_flags &
	   (NAME_HIDDEN|TYPE_MOD_PRIVATE|TYPE_MOD_STATIC|TYPE_MOD_PROTECTED|
	    NAME_INHERITED) ) )
    {
	num_functions = prog->num_functions;
    }
    for (i = mode_flags & 0xf, j = 0; i; i >>= 1) {
	if (i & 1)
	    j += num_functions;
    }
    list = allocate_array(j);
    svp = list->item + j;
    for(i = prog->num_functions, fun += i; --i >= 0; ) {
	unsigned char *funstart;

	fun--;
	if (!vis_tags[i]) continue;
	flags = *fun;
	active_flags = (flags & ~INHERIT_MASK);
	if (vis_tags[i] & 2)
	    active_flags |= NAME_HIDDEN;
	defprog = prog;
	if ( !~(flags | ~(NAME_INHERITED|NAME_CROSS_DEFINED) ) ) {
	    active_flags |= NAME_CROSS_DEFINED;
	    j = (flags & INHERIT_MASK) - (INHERIT_MASK + 1 >> 1);
	    flags = fun[j];
	    j += i;
	} else {
	    j = i;
	}
	while (flags & NAME_INHERITED) {
	    struct inherit *ip = &defprog->inherit[flags & INHERIT_MASK];

	    defprog = ip->prog;
	    j -= ip->function_index_offset;
	    flags = defprog->functions[j];
	}
	funstart = defprog->program + (flags & FUNSTART_MASK);
	if (mode_flags & 8) {
	    svp--;
	    svp->u.number = (funstart[0] & 0x7f); /* number of arguments */
	}
	if (mode_flags & 4) {
	    svp--;
	    svp->u.number = funstart[-1]; /* return type */
	}
	if (mode_flags & 2) {
	    if (funstart[2] == F_ESCAPE-F_OFFSET &&
		funstart[3] == F_UNDEF-F_OFFSET-0x100)
	    {
		active_flags |= NAME_UNDEFINED;
	    }
	    svp--;
	    svp->u.number = active_flags;
	}
	if (mode_flags & 1) {
	    svp--;
	    svp->type = T_STRING;
	    svp->x.string_type = STRING_SHARED;
	    memcpy(
	      (char *)&svp->u.string,
	      funstart - 1 - sizeof svp->u.string,
	      sizeof svp->u.string
	    );
	    increment_string_ref(svp->u.string);
	}
    }
    free_svalue(sp);
    sp--;
    free_svalue(sp);
    sp->type = T_POINTER;
    sp->u.vec = list;
    return sp;
}

void set_vector_user(p, owner)
    struct vector *p;
    struct object *owner;
{
    struct svalue *svp;
    mp_int i;

    i = VEC_SIZE(p);
    if (p->user)
	p->user->size_array -= i;
    if (p->user = owner->user)
	p->user->size_array += i;
    svp = p->item;
    for (; --i >= 0; svp++) {
	set_svalue_user(svp, owner);
    }
}

long total_array_size()
{
    extern struct wiz_list *all_wiz, default_wizlist_entry;

    struct wiz_list *wl;
    long total;

    total = default_wizlist_entry.size_array;
    for (wl = all_wiz; wl; wl = wl->next)
	total += wl->size_array;
    total *= sizeof(struct svalue);
    total += num_arrays * (sizeof(struct vector) - sizeof(struct svalue));
    return total;
}
